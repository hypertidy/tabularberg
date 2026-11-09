//! Storage constructors and credential / container specs.
//!
//! Credentials and virtual chunk containers arrive from R as small JSON
//! strings (built by R/credentials.R), so the FFI surface stays narrow.

use std::collections::HashMap;
use std::sync::Arc;

use extendr_api::prelude::*;
use icechunk::config::{Credentials, HttpConfig};
use icechunk::storage::{S3Credentials, S3Options, S3StaticCredentials, Storage};
use icechunk::virtual_chunks::VirtualChunkContainer;
use icechunk::ObjectStoreConfig;
use serde_json::Value;

use crate::err::{arg, ic, R};
use crate::runtime::block_on;

pub struct RStorage {
    pub(crate) inner: Arc<dyn Storage + Send + Sync>,
    pub(crate) desc: String,
}

fn wrap(inner: Arc<dyn Storage + Send + Sync>, desc: String) -> ExternalPtr<RStorage> {
    ExternalPtr::new(RStorage { inner, desc })
}

fn s(v: &Value, k: &str) -> Option<String> {
    v.get(k).and_then(|x| x.as_str()).map(|x| x.to_string())
}
fn b(v: &Value, k: &str) -> bool {
    v.get(k).and_then(|x| x.as_bool()).unwrap_or(false)
}
fn parse(json: &str) -> R<Value> {
    if json.is_empty() {
        return Ok(Value::Null);
    }
    serde_json::from_str(json).map_err(|e| arg(format!("bad JSON spec: {e}")))
}

pub(crate) fn s3_options(v: &Value) -> S3Options {
    let mut o = S3Options::default();
    o.region = s(v, "region");
    o.endpoint_url = s(v, "endpoint_url");
    o.anonymous = b(v, "anonymous");
    o.allow_http = b(v, "allow_http");
    o.force_path_style = b(v, "force_path_style");
    o.requester_pays = b(v, "requester_pays");
    o
}

pub(crate) fn s3_credentials(v: &Value) -> R<Option<S3Credentials>> {
    match s(v, "kind").as_deref() {
        None => Ok(None),
        Some("from_env") => Ok(Some(S3Credentials::FromEnv)),
        Some("anonymous") => Ok(Some(S3Credentials::Anonymous)),
        Some("static") => Ok(Some(S3Credentials::Static(S3StaticCredentials {
            access_key_id: s(v, "access_key_id").ok_or_else(|| arg("access_key_id missing"))?,
            secret_access_key: s(v, "secret_access_key")
                .ok_or_else(|| arg("secret_access_key missing"))?,
            session_token: s(v, "session_token"),
            expires_after: None,
        }))),
        Some(k) => Err(arg(format!("unknown s3 credential kind '{k}'"))),
    }
}

/// One credential spec -> icechunk Credentials (for virtual chunk authorisation).
pub(crate) fn credentials(v: &Value) -> R<Option<Credentials>> {
    if v.is_null() {
        return Ok(None);
    }
    match s(v, "type").as_deref() {
        Some("s3") => Ok(s3_credentials(v)?.map(Credentials::S3)),
        Some("http") => Ok(Some(Credentials::HttpAccess)),
        Some("local") => Ok(Some(Credentials::LocalFileSystemAccess)),
        #[cfg(feature = "cloud")]
        Some("gcs") => {
            use icechunk::storage::GcsCredentials;
            Ok(Some(Credentials::Gcs(match s(v, "kind").as_deref() {
                Some("anonymous") => GcsCredentials::Anonymous,
                _ => GcsCredentials::FromEnv,
            })))
        }
        #[cfg(feature = "cloud")]
        Some("azure") => {
            use icechunk::storage::AzureCredentials;
            Ok(Some(Credentials::Azure(match s(v, "kind").as_deref() {
                Some("anonymous") => AzureCredentials::Anonymous,
                _ => AzureCredentials::FromEnv,
            })))
        }
        Some(t) => Err(arg(format!("credential type '{t}' unknown or not compiled in"))),
        None => Err(arg("credential spec has no 'type'")),
    }
}

/// JSON object {prefix: spec|null, ...} -> authorisation map.
pub(crate) fn authorize_map(json: &str) -> R<HashMap<String, Option<Credentials>>> {
    let v = parse(json)?;
    let mut out = HashMap::new();
    if let Value::Object(m) = v {
        for (k, spec) in m.iter() {
            out.insert(k.clone(), credentials(spec)?);
        }
    }
    Ok(out)
}

/// JSON array of {url_prefix, store: {type, ...}} -> containers.
pub(crate) fn containers(json: &str) -> R<Vec<VirtualChunkContainer>> {
    let v = parse(json)?;
    let mut out = Vec::new();
    if let Value::Array(items) = v {
        for it in items {
            let prefix = s(&it, "url_prefix").ok_or_else(|| arg("container url_prefix missing"))?;
            let st = it.get("store").cloned().unwrap_or(Value::Null);
            let cfg = match s(&st, "type").as_deref() {
                Some("s3") => ObjectStoreConfig::S3(s3_options(&st)),
                Some("s3_compatible") => ObjectStoreConfig::S3Compatible(s3_options(&st)),
                Some("http") => ObjectStoreConfig::Http(HttpConfig::default()),
                Some("local") => ObjectStoreConfig::LocalFileSystem(
                    s(&st, "path").unwrap_or_default().into(),
                ),
                Some("in_memory") => ObjectStoreConfig::InMemory,
                #[cfg(feature = "cloud")]
                Some("gcs") => ObjectStoreConfig::Gcs(HashMap::new()),
                Some(t) => return Err(arg(format!("container store type '{t}' unknown"))),
                None => return Err(arg("container store has no 'type'")),
            };
            out.push(VirtualChunkContainer::new(prefix, cfg).map_err(arg)?);
        }
    }
    Ok(out)
}

/// @noRd
#[extendr]
fn ic_storage_local_(path: &str) -> R<ExternalPtr<RStorage>> {
    let st = block_on(icechunk::storage::new_local_filesystem_storage(std::path::Path::new(path)))
        .map_err(ic)?;
    Ok(wrap(st, format!("local: {path}")))
}

/// @noRd
#[extendr]
fn ic_storage_memory_() -> R<ExternalPtr<RStorage>> {
    let st = block_on(icechunk::storage::new_in_memory_storage()).map_err(ic)?;
    Ok(wrap(st, "in-memory".into()))
}

/// @noRd
#[extendr]
fn ic_storage_http_(base_url: &str) -> R<ExternalPtr<RStorage>> {
    let st = icechunk::storage::new_http_storage(base_url, None, None).map_err(ic)?;
    Ok(wrap(st, format!("http: {base_url}")))
}

/// opts_json: S3Options fields plus credential spec under "credentials".
/// @noRd
#[extendr]
fn ic_storage_s3_(bucket: &str, prefix: &str, opts_json: &str, backend: &str) -> R<ExternalPtr<RStorage>> {
    let v = parse(opts_json)?;
    let opts = s3_options(&v);
    let creds = s3_credentials(v.get("credentials").unwrap_or(&Value::Null))?;
    let prefix = if prefix.is_empty() { None } else { Some(prefix.to_string()) };
    let desc = format!(
        "s3: {bucket}/{} ({backend}{})",
        prefix.clone().unwrap_or_default(),
        opts.endpoint_url.as_ref().map(|e| format!(", {e}")).unwrap_or_default()
    );
    let st: Arc<dyn Storage + Send + Sync> = match backend {
        #[cfg(feature = "cloud")]
        "object_store" => block_on(icechunk::storage::new_s3_object_store_storage(
            opts, bucket.to_string(), prefix, creds, vec![], vec![],
        ))
        .map_err(ic)?,
        #[cfg(feature = "full")]
        "aws" => icechunk::storage::new_s3_storage(
            opts, bucket.to_string(), prefix, creds, vec![], vec![], None,
        )
        .map_err(ic)?,
        _ => return Err(arg(format!("s3 backend '{backend}' not compiled in (see ic_compiled_features())"))),
    };
    Ok(wrap(st, desc))
}

/// @noRd
#[extendr]
fn ic_storage_gcs_(bucket: &str, prefix: &str, anonymous: bool) -> R<ExternalPtr<RStorage>> {
    #[cfg(feature = "cloud")]
    {
        use icechunk::storage::GcsCredentials;
        let prefix = if prefix.is_empty() { None } else { Some(prefix.to_string()) };
        let creds = if anonymous { GcsCredentials::Anonymous } else { GcsCredentials::FromEnv };
        let st = icechunk::storage::new_gcs_storage(
            bucket.to_string(), prefix.clone(), Some(creds), None, vec![], vec![],
        )
        .map_err(ic)?;
        return Ok(wrap(st, format!("gcs: {bucket}/{}", prefix.unwrap_or_default())));
    }
    #[allow(unreachable_code)]
    {
        let _ = (bucket, prefix, anonymous);
        Err(arg("gcs not compiled in (see ic_compiled_features())"))
    }
}

/// @noRd
#[extendr]
fn ic_storage_azure_(account: &str, container: &str, prefix: &str) -> R<ExternalPtr<RStorage>> {
    #[cfg(feature = "cloud")]
    {
        use icechunk::storage::AzureCredentials;
        let prefix = if prefix.is_empty() { None } else { Some(prefix.to_string()) };
        let st = block_on(icechunk::storage::new_azure_blob_storage(
            account.to_string(), container.to_string(), prefix.clone(),
            Some(AzureCredentials::FromEnv), None,
        ))
        .map_err(ic)?;
        return Ok(wrap(st, format!("azure: {account}/{container}/{}", prefix.unwrap_or_default())));
    }
    #[allow(unreachable_code)]
    {
        let _ = (account, container, prefix);
        Err(arg("azure not compiled in (see ic_compiled_features())"))
    }
}

/// @noRd
#[extendr]
fn ic_storage_desc_(st: ExternalPtr<RStorage>) -> String {
    st.desc.clone()
}

extendr_module! {
    mod storage;
    fn ic_storage_local_;
    fn ic_storage_memory_;
    fn ic_storage_http_;
    fn ic_storage_s3_;
    fn ic_storage_gcs_;
    fn ic_storage_azure_;
    fn ic_storage_desc_;
}
