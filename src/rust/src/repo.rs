//! Repository: open/create, refs, ancestry, diff, sessions.

use std::sync::Arc;

use extendr_api::prelude::*;
use futures::TryStreamExt;
use icechunk::diff::Diff;
use icechunk::format::SnapshotId;
use icechunk::format::format_constants::SpecVersionBin;
use icechunk::repository::{Repository, VersionInfo};
use icechunk::RepositoryConfig;
use icechunk::config::Credentials;
use icechunk::storage::S3Credentials;
use icechunk::ObjectStoreConfig;
use std::collections::HashMap;

use crate::err::{arg, ic, R};
use crate::runtime::block_on;
use crate::session::{wrap_session, RSession};
use crate::storage::{authorize_map, containers, RStorage};

pub struct RRepo {
    pub(crate) inner: Arc<Repository>,
}

fn snap(id: &str) -> R<SnapshotId> {
    SnapshotId::try_from(id).map_err(|e| arg(format!("bad snapshot id '{id}': {e}")))
}

/// Exactly one of branch / tag / snapshot non-empty ("" means unset).
pub(crate) fn version(branch: &str, tag: &str, snapshot: &str) -> R<VersionInfo> {
    let n = [branch, tag, snapshot].iter().filter(|s| !s.is_empty()).count();
    if n > 1 {
        return Err(arg("give only one of branch, tag, snapshot"));
    }
    Ok(if !snapshot.is_empty() {
        VersionInfo::SnapshotId(snap(snapshot)?)
    } else if !tag.is_empty() {
        VersionInfo::TagRef(tag.to_string())
    } else if !branch.is_empty() {
        VersionInfo::BranchTipRef(branch.to_string())
    } else {
        VersionInfo::BranchTipRef("main".to_string())
    })
}

fn config_with(containers_json: &str) -> R<Option<RepositoryConfig>> {
    config_with_split(containers_json, "")
}

/// Manifest splitting rules from R: a JSON array of
/// `{"path": <regex or null>, "dims": [[<axis index or dimension name or "*">, <chunks per manifest>], ...]}`.
/// Rules are tried in order; `path: null` matches any array.
fn split_config(split_json: &str) -> R<Option<icechunk::config::ManifestSplittingConfig>> {
    use icechunk::config::{ManifestSplitCondition, ManifestSplitDim, ManifestSplitDimCondition, ManifestSplittingConfig};
    if split_json.is_empty() {
        return Ok(None);
    }
    let v: serde_json::Value = serde_json::from_str(split_json).map_err(|e| arg(format!("bad split rules: {e}")))?;
    let rules = v.as_array().ok_or_else(|| arg("split rules must be a list"))?;
    let mut out = Vec::new();
    for r in rules {
        let cond = match r.get("path").and_then(|p| p.as_str()) {
            Some(re) => ManifestSplitCondition::PathMatches { regex: re.to_string() },
            None => ManifestSplitCondition::AnyArray,
        };
        let mut dims = Vec::new();
        for d in r.get("dims").and_then(|d| d.as_array()).ok_or_else(|| arg("each split rule needs dims"))? {
            let pair = d.as_array().filter(|a| a.len() == 2).ok_or_else(|| arg("split dims are [dimension, chunks] pairs"))?;
            let key = pair[0].as_str().ok_or_else(|| arg("split dimension must be a string"))?;
            let n = pair[1].as_u64().ok_or_else(|| arg("split size must be a positive integer"))? as u32;
            let condition = if key == "*" {
                ManifestSplitDimCondition::Any
            } else if let Ok(ax) = key.parse::<usize>() {
                ManifestSplitDimCondition::Axis(ax)
            } else {
                ManifestSplitDimCondition::DimensionName(key.to_string())
            };
            dims.push(ManifestSplitDim { condition, num_chunks: n });
        }
        out.push((cond, dims));
    }
    Ok(Some(ManifestSplittingConfig { split_sizes: Some(out) }))
}

fn config_with_split(containers_json: &str, split_json: &str) -> R<Option<RepositoryConfig>> {
    let conts = containers(containers_json)?;
    let split = split_config(split_json)?;
    if conts.is_empty() && split.is_none() {
        return Ok(None);
    }
    let mut cfg = RepositoryConfig::default();
    for c in conts {
        cfg.set_virtual_chunk_container(c).map_err(arg)?;
    }
    if let Some(sp) = split {
        let mut m = cfg.manifest.clone().unwrap_or_default();
        m.splitting = Some(sp);
        cfg.manifest = Some(m);
    }
    Ok(Some(cfg))
}

/// @noRd
#[extendr]
fn ic_repo_open_(st: ExternalPtr<RStorage>, containers_json: &str, authorize_json: &str) -> R<ExternalPtr<RRepo>> {
    let cfg = config_with(containers_json)?;
    let auth = authorize_map(authorize_json)?;
    let mut b = Repository::open(Arc::clone(&st.inner)).authorize_virtual_chunk_access(auth);
    if let Some(c) = cfg {
        b = b.config(c);
    }
    let repo = block_on(b.execute()).map_err(ic)?;
    Ok(ExternalPtr::new(RRepo { inner: Arc::new(repo) }))
}

/// @noRd
#[extendr]
fn ic_repo_create_(
    st: ExternalPtr<RStorage>,
    containers_json: &str,
    authorize_json: &str,
    spec_version: i32,
    split_json: &str,
) -> R<ExternalPtr<RRepo>> {
    let cfg = config_with_split(containers_json, split_json)?;
    let auth = authorize_map(authorize_json)?;
    let mut b = Repository::create(Arc::clone(&st.inner)).authorize_virtual_chunk_access(auth);
    if let Some(c) = cfg {
        b = b.config(c);
    }
    if spec_version > 0 {
        let v = SpecVersionBin::try_from(spec_version as u8).map_err(arg)?;
        b = b.spec_version(v);
    }
    let repo = block_on(b.execute()).map_err(ic)?;
    Ok(ExternalPtr::new(RRepo { inner: Arc::new(repo) }))
}

/// URL prefixes of the virtual chunk containers the repository declares
/// (persisted config plus any passed at open), and those it authorises.
/// @noRd
#[extendr]
fn ic_repo_containers_(repo: ExternalPtr<RRepo>) -> List {
    let mut declared: Vec<String> = repo
        .inner
        .config()
        .virtual_chunk_containers
        .as_ref()
        .map(|m| m.keys().cloned().collect())
        .unwrap_or_default();
    declared.sort();
    let mut authorized: Vec<String> = repo.inner.authorized_virtual_container_prefixes().into_iter().collect();
    authorized.sort();
    list!(declared = declared, authorized = authorized)
}

/// The same repository with a new virtual chunk authorisation map (no
/// re-read of the repository; config unchanged).
/// @noRd
#[extendr]
fn ic_repo_reauthorize_(repo: ExternalPtr<RRepo>, authorize_json: &str) -> R<ExternalPtr<RRepo>> {
    let auth = authorize_map(authorize_json)?;
    let config = anonymous_containers(&repo.inner, &auth);
    let r = block_on(repo.inner.reopen(config, Some(auth))).map_err(ic)?;
    Ok(ExternalPtr::new(RRepo { inner: Arc::new(r) }))
}

/// Mark S3 containers anonymous when they are authorised with anonymous
/// credentials. icechunk 2.3.1's object_store S3 fetcher (the `cloud` tier)
/// applies an anonymous credential and then overwrites it with the
/// container's own `anonymous` flag, so the credential alone still signs
/// requests and looks for credentials in instance metadata. The AWS SDK
/// fetcher (`full` tier, icechunk-python) honours the credential; this makes
/// both behave the same. Returns the full container map (reopen replaces it)
/// or None when nothing changes.
fn anonymous_containers(
    repo: &Repository,
    auth: &HashMap<String, Option<Credentials>>,
) -> Option<RepositoryConfig> {
    let mut conts = repo.config().virtual_chunk_containers.clone()?;
    let mut changed = false;
    for (prefix, cont) in conts.iter_mut() {
        if !matches!(auth.get(prefix), Some(Some(Credentials::S3(S3Credentials::Anonymous)))) {
            continue;
        }
        match &mut cont.store {
            ObjectStoreConfig::S3(o) | ObjectStoreConfig::S3Compatible(o) | ObjectStoreConfig::Tigris(o)
                if !o.anonymous =>
            {
                o.anonymous = true;
                changed = true;
            }
            _ => {}
        }
    }
    changed.then(|| RepositoryConfig { virtual_chunk_containers: Some(conts), ..Default::default() })
}

/// @noRd
#[extendr]
fn ic_repo_exists_(st: ExternalPtr<RStorage>) -> R<bool> {
    block_on(Repository::exists(Arc::clone(&st.inner), None)).map_err(ic)
}

/// @noRd
#[extendr]
fn ic_repo_spec_version_(repo: ExternalPtr<RRepo>) -> i32 {
    repo.inner.spec_version() as i32
}

/// @noRd
#[extendr]
fn ic_repo_config_json_(repo: ExternalPtr<RRepo>) -> R<String> {
    serde_json::to_string(repo.inner.config()).map_err(arg)
}

/// @noRd
#[extendr]
fn ic_repo_save_config_(repo: ExternalPtr<RRepo>) -> R<()> {
    block_on(repo.inner.save_config()).map_err(ic)?;
    Ok(())
}

/// @noRd
#[extendr]
fn ic_branches_(repo: ExternalPtr<RRepo>) -> R<List> {
    let r = &repo.inner;
    block_on(async {
        let names: Vec<String> = r.list_branches().await.map_err(ic)?.into_iter().collect();
        let mut ids = Vec::with_capacity(names.len());
        for n in &names {
            ids.push(r.lookup_branch(n).await.map_err(ic)?.to_string());
        }
        Ok(list!(name = names, snapshot_id = ids))
    })
}

/// @noRd
#[extendr]
fn ic_tags_(repo: ExternalPtr<RRepo>) -> R<List> {
    let r = &repo.inner;
    block_on(async {
        let names: Vec<String> = r.list_tags().await.map_err(ic)?.into_iter().collect();
        let mut ids = Vec::with_capacity(names.len());
        for n in &names {
            ids.push(r.lookup_tag(n).await.map_err(ic)?.to_string());
        }
        Ok(list!(name = names, snapshot_id = ids))
    })
}

/// @noRd
#[extendr]
fn ic_ancestry_(repo: ExternalPtr<RRepo>, branch: &str, tag: &str, snapshot: &str) -> R<List> {
    let v = version(branch, tag, snapshot)?;
    let r = &repo.inner;
    block_on(async {
        let infos: Vec<_> = r.ancestry(&v).await.map_err(ic)?.try_collect().await.map_err(ic)?;
        let mut id = Vec::new();
        let mut parent = Vec::new();
        let mut at = Vec::new();
        let mut msg = Vec::new();
        let mut meta = Vec::new();
        for i in infos {
            id.push(i.id.to_string());
            parent.push(i.parent_id.map(|p| p.to_string()).unwrap_or_default());
            at.push(i.flushed_at.to_rfc3339());
            msg.push(i.message);
            meta.push(serde_json::to_string(&i.metadata).unwrap_or_default());
        }
        Ok(list!(snapshot_id = id, parent_id = parent, written_at = at, message = msg, metadata = meta))
    })
}

/// @noRd
#[extendr]
fn ic_branch_create_(repo: ExternalPtr<RRepo>, name: &str, snapshot_id: &str) -> R<()> {
    let id = snap(snapshot_id)?;
    block_on(repo.inner.create_branch(name, &id)).map_err(ic)
}

/// @noRd
#[extendr]
fn ic_branch_reset_(repo: ExternalPtr<RRepo>, name: &str, to: &str, from: &str) -> R<()> {
    let to = snap(to)?;
    let from = if from.is_empty() { None } else { Some(snap(from)?) };
    block_on(repo.inner.reset_branch(name, &to, from.as_ref())).map_err(ic)
}

/// @noRd
#[extendr]
fn ic_branch_delete_(repo: ExternalPtr<RRepo>, name: &str) -> R<()> {
    block_on(repo.inner.delete_branch(name)).map_err(ic)
}

/// @noRd
#[extendr]
fn ic_tag_create_(repo: ExternalPtr<RRepo>, name: &str, snapshot_id: &str) -> R<()> {
    let id = snap(snapshot_id)?;
    block_on(repo.inner.create_tag(name, &id)).map_err(ic)
}

/// @noRd
#[extendr]
fn ic_tag_delete_(repo: ExternalPtr<RRepo>, name: &str) -> R<()> {
    block_on(repo.inner.delete_tag(name)).map_err(ic)
}

/// @noRd
#[extendr]
fn ic_lookup_(repo: ExternalPtr<RRepo>, branch: &str, tag: &str) -> R<String> {
    let r = &repo.inner;
    block_on(async {
        if !tag.is_empty() {
            Ok(r.lookup_tag(tag).await.map_err(ic)?.to_string())
        } else {
            Ok(r.lookup_branch(branch).await.map_err(ic)?.to_string())
        }
    })
}

pub(crate) fn diff_list(d: Diff) -> List {
    let p = |s: std::collections::BTreeSet<icechunk::format::Path>| -> Vec<String> {
        s.into_iter().map(|x| x.to_string()).collect()
    };
    let mut chunk_path = Vec::new();
    let mut chunk_coord = Vec::new();
    for (path, set) in d.updated_chunks {
        for c in set {
            chunk_path.push(path.to_string());
            chunk_coord.push(c.0.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(","));
        }
    }
    let moved_from: Vec<String> = d.moved_nodes.iter().map(|m| m.from.to_string()).collect();
    let moved_to: Vec<String> = d.moved_nodes.iter().map(|m| m.to.to_string()).collect();
    list!(
        new_groups = p(d.new_groups),
        new_arrays = p(d.new_arrays),
        deleted_groups = p(d.deleted_groups),
        deleted_arrays = p(d.deleted_arrays),
        updated_groups = p(d.updated_groups),
        updated_arrays = p(d.updated_arrays),
        chunk_path = chunk_path,
        chunk_coord = chunk_coord,
        moved_from = moved_from,
        moved_to = moved_to
    )
}

/// @noRd
#[extendr]
fn ic_diff_(
    repo: ExternalPtr<RRepo>,
    from_branch: &str, from_tag: &str, from_snapshot: &str,
    to_branch: &str, to_tag: &str, to_snapshot: &str,
) -> R<List> {
    let from = version(from_branch, from_tag, from_snapshot)?;
    let to = version(to_branch, to_tag, to_snapshot)?;
    let d = block_on(repo.inner.diff(&from, &to)).map_err(ic)?;
    Ok(diff_list(d))
}

/// @noRd
#[extendr]
fn ic_session_readonly_(repo: ExternalPtr<RRepo>, branch: &str, tag: &str, snapshot: &str) -> R<ExternalPtr<RSession>> {
    let v = version(branch, tag, snapshot)?;
    let s = block_on(repo.inner.readonly_session(&v)).map_err(ic)?;
    Ok(wrap_session(s))
}

/// @noRd
#[extendr]
fn ic_session_writable_(repo: ExternalPtr<RRepo>, branch: &str) -> R<ExternalPtr<RSession>> {
    let s = block_on(repo.inner.writable_session(branch)).map_err(ic)?;
    Ok(wrap_session(s))
}

extendr_module! {
    mod repo;
    fn ic_repo_open_;
    fn ic_repo_create_;
    fn ic_repo_exists_;
    fn ic_repo_containers_;
    fn ic_repo_reauthorize_;
    fn ic_repo_spec_version_;
    fn ic_repo_config_json_;
    fn ic_repo_save_config_;
    fn ic_branches_;
    fn ic_tags_;
    fn ic_ancestry_;
    fn ic_branch_create_;
    fn ic_branch_reset_;
    fn ic_branch_delete_;
    fn ic_tag_create_;
    fn ic_tag_delete_;
    fn ic_lookup_;
    fn ic_diff_;
    fn ic_session_readonly_;
    fn ic_session_writable_;
}
