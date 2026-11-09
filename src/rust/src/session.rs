//! Session, zarr-key store primitives, commit family, chunk reference
//! table, and session-bytes handoff.

use std::sync::Arc;

use bytes::Bytes;
use extendr_api::prelude::*;
use futures::{StreamExt, TryStreamExt};
use icechunk::conflicts::basic_solver::{BasicConflictSolver, VersionSelection};
use icechunk::conflicts::detector::ConflictDetector;
use icechunk::format::manifest::{Checksum, ChunkPayload, SecondsSinceEpoch, VirtualChunkLocation, VirtualChunkRef};
use icechunk::format::{ByteRange, ChunkIndices, Path};
use icechunk::session::{Session, SessionMode};
use icechunk::store::{KeyNotFoundError, StoreErrorKind};
use icechunk::Store;
use tokio::sync::RwLock;

use crate::err::{arg, ic, tagged, R};
use crate::runtime::block_on;

pub struct RSession {
    pub(crate) session: Arc<RwLock<Session>>,
    pub(crate) store: Store,
}

pub(crate) fn wrap_session(s: Session) -> ExternalPtr<RSession> {
    let session = Arc::new(RwLock::new(s));
    let store = Store::from_session_and_config(Arc::clone(&session), 64);
    ExternalPtr::new(RSession { session, store })
}

fn key(k: &str) -> String {
    // icechunk keys are relative ("arr/c/0/0", "zarr.json"); tolerate a leading "/".
    k.trim_start_matches('/').to_string()
}

fn path(p: &str) -> R<Path> {
    let p = if p.starts_with('/') { p.to_string() } else { format!("/{p}") };
    Path::try_from(p.as_str()).map_err(|e| arg(format!("bad path '{p}': {e}")))
}

// ---- info ---------------------------------------------------------------

#[extendr]
fn ic_session_info_(s: ExternalPtr<RSession>) -> List {
    let g = s.session.blocking_read();
    let mode = match g.mode() {
        SessionMode::Readonly => "readonly",
        SessionMode::Writable => "writable",
        SessionMode::Rearrange => "rearrange",
    };
    list!(
        snapshot_id = g.snapshot_id().to_string(),
        branch = g.branch().unwrap_or("").to_string(),
        mode = mode,
        read_only = g.read_only(),
        is_fork = g.is_fork(),
        has_uncommitted_changes = g.has_uncommitted_changes()
    )
}

#[extendr]
fn ic_status_(s: ExternalPtr<RSession>) -> R<List> {
    let d = block_on(async { s.session.read().await.status().await }).map_err(ic)?;
    Ok(crate::repo::diff_list(d))
}

// ---- store primitives ----------------------------------------------------

fn is_missing(e: &icechunk::store::StoreError) -> bool {
    matches!(e.kind, StoreErrorKind::NotFound(_))
}

/// Whole value, or a byte range when offset >= 0 (length < 0: to end).
#[extendr]
fn ic_get_(s: ExternalPtr<RSession>, k: &str, offset: f64, length: f64) -> R<Robj> {
    let br = if offset < 0.0 {
        ByteRange::ALL
    } else if length < 0.0 {
        ByteRange::From(offset as u64)
    } else {
        ByteRange::Bounded(offset as u64..(offset + length) as u64)
    };
    match block_on(s.store.get(&key(k), &br)) {
        Ok(b) => Ok(Raw::from_bytes(&b).into()),
        Err(e) if is_missing(&e) => Ok(().into()),
        Err(e) => Err(ic(e)),
    }
}

/// Concurrent gets; list of raw (NULL where the key is absent).
#[extendr]
fn ic_get_many_(s: ExternalPtr<RSession>, keys: Strings) -> R<List> {
    let ks: Vec<String> = keys.iter().map(|k| key(k.as_str())).collect();
    let res: Vec<std::result::Result<Bytes, icechunk::store::StoreError>> = block_on(async {
        futures::stream::iter(ks.iter().map(|k| s.store.get(k, &ByteRange::ALL)))
            .buffered(64)
            .collect()
            .await
    });
    let mut out: Vec<Robj> = Vec::with_capacity(res.len());
    for r in res {
        match r {
            Ok(b) => out.push(Raw::from_bytes(&b).into()),
            Err(e) if is_missing(&e) => out.push(().into()),
            Err(e) => return Err(ic(e)),
        }
    }
    Ok(List::from_values(out))
}

#[extendr]
fn ic_set_(s: ExternalPtr<RSession>, k: &str, value: Raw) -> R<()> {
    let b = Bytes::copy_from_slice(value.as_slice());
    block_on(s.store.set(&key(k), b)).map_err(ic)
}

#[extendr]
fn ic_exists_(s: ExternalPtr<RSession>, k: &str) -> R<bool> {
    block_on(s.store.exists(&key(k))).map_err(ic)
}

#[extendr]
fn ic_delete_(s: ExternalPtr<RSession>, k: &str) -> R<()> {
    block_on(s.store.delete(&key(k))).map_err(ic)
}

/// mode "prefix": all keys under prefix (recursive); "dir": one level.
#[extendr]
fn ic_list_(s: ExternalPtr<RSession>, prefix: &str, mode: &str) -> R<Vec<String>> {
    let p = prefix.trim_start_matches('/');
    block_on(async {
        let v: Vec<String> = if mode == "dir" {
            s.store.list_dir(p).await.map_err(ic)?.try_collect().await.map_err(ic)?
        } else {
            s.store.list_prefix(p).await.map_err(ic)?.try_collect().await.map_err(ic)?
        };
        Ok(v)
    })
}

/// etag ("" none) or last_modified (seconds since epoch, < 0 none) checksum.
#[extendr]
fn ic_set_virtual_ref_(
    s: ExternalPtr<RSession>,
    k: &str,
    location: &str,
    offset: f64,
    length: f64,
    etag: &str,
    last_modified: f64,
    validate_container: bool,
) -> R<()> {
    let checksum = if !etag.is_empty() {
        Some(Checksum::ETag(icechunk::storage::ETag(etag.to_string())))
    } else if last_modified >= 0.0 {
        Some(Checksum::LastModified(SecondsSinceEpoch(last_modified as u32)))
    } else {
        None
    };
    let loc = VirtualChunkLocation::from_url(location).map_err(|e| arg(format!("bad location: {e}")))?;
    let vref = VirtualChunkRef { location: loc, offset: offset as u64, length: length as u64, checksum };
    block_on(s.store.set_virtual_ref(&key(k), vref, validate_container)).map_err(ic)
}

/// Vectorised virtual refs for one array: coords is an n x ndim integer
/// matrix of 0-based chunk indices; etag (""/NA none) takes precedence
/// over last_modified (NA none) as the checksum.
#[extendr]
fn ic_set_virtual_refs_(
    s: ExternalPtr<RSession>,
    array_path: &str,
    coords: Integers,
    ndim: i32,
    locations: Strings,
    offsets: Doubles,
    lengths: Doubles,
    etags: Strings,
    last_modified: Doubles,
    validate_container: bool,
) -> R<i32> {
    // coords: n x ndim integer matrix, column-major (R order)
    let n = locations.len();
    let nd = ndim as usize;
    if coords.len() != n * nd || offsets.len() != n || lengths.len() != n
        || etags.len() != n || last_modified.len() != n
    {
        return Err(arg("coords (n x ndim), locations, offsets, lengths, etags, last_modified must agree"));
    }
    let cv: Vec<i32> = coords.iter().map(|x| x.inner()).collect();
    let mut refs = Vec::with_capacity(n);
    for i in 0..n {
        let mut c = Vec::with_capacity(nd);
        for d in 0..nd {
            let v = cv[d * n + i];
            if v < 0 {
                return Err(arg(format!("row {}: negative or NA chunk coordinate", i + 1)));
            }
            c.push(v as u32);
        }
        let loc = VirtualChunkLocation::from_url(locations.elt(i).as_str())
            .map_err(|e| arg(format!("row {}: bad location: {e}", i + 1)))?;
        let e = etags.elt(i);
        let lm = last_modified.elt(i);
        let checksum = if !e.is_na() && !e.as_str().is_empty() {
            Some(Checksum::ETag(icechunk::storage::ETag(e.as_str().to_string())))
        } else if !lm.is_na() && lm.inner() >= 0.0 {
            Some(Checksum::LastModified(SecondsSinceEpoch(lm.inner() as u32)))
        } else {
            None
        };
        refs.push((
            ChunkIndices(c),
            VirtualChunkRef {
                location: loc,
                offset: offsets.elt(i).inner() as u64,
                length: lengths.elt(i).inner() as u64,
                checksum,
            },
        ));
    }
    let p = path(array_path)?;
    let res = block_on(s.store.set_virtual_refs(&p, validate_container, refs)).map_err(ic)?;
    Ok(match res {
        icechunk::store::SetVirtualRefsResult::Done => 0,
        icechunk::store::SetVirtualRefsResult::FailedRefs(v) => v.len() as i32,
    })
}

// ---- commit family -------------------------------------------------------

fn props(json: &str) -> R<icechunk::format::snapshot::SnapshotProperties> {
    if json.is_empty() {
        return Ok(Default::default());
    }
    serde_json::from_str(json).map_err(|e| arg(format!("metadata must be a JSON object: {e}")))
}

/// how: "commit" | "amend" | "flush" (anonymous snapshot)
#[extendr]
fn ic_commit_(s: ExternalPtr<RSession>, message: &str, metadata_json: &str, how: &str, allow_empty: bool) -> R<String> {
    let p = props(metadata_json)?;
    block_on(async {
        let mut g = s.session.write().await;
        let mut b = g.commit(message).properties(p).allow_empty(allow_empty);
        b = match how {
            "amend" => b.amend(),
            "flush" => b.anonymous(),
            _ => b,
        };
        b.execute().await.map(|id| id.to_string()).map_err(|e| {
            let conflict = matches!(e.kind, icechunk::session::SessionErrorKind::Conflict { .. });
            if conflict { tagged("conflict", e) } else { ic(e) }
        })
    })
}

#[extendr]
fn ic_discard_(s: ExternalPtr<RSession>) -> R<()> {
    block_on(async { s.session.write().await.discard_changes() }).map_err(ic)
}

/// solver "detect" (fail on any conflict) or "basic" with on_chunk
/// "ours" | "theirs" | "fail".
#[extendr]
fn ic_rebase_(s: ExternalPtr<RSession>, solver: &str, on_chunk: &str) -> R<()> {
    block_on(async {
        let mut g = s.session.write().await;
        let r = if solver == "basic" {
            let sel = match on_chunk {
                "theirs" => VersionSelection::UseTheirs,
                "fail" => VersionSelection::Fail,
                _ => VersionSelection::UseOurs,
            };
            let sv = BasicConflictSolver { on_chunk_conflict: sel, ..Default::default() };
            g.rebase(&sv).await
        } else {
            g.rebase(&ConflictDetector).await
        };
        r.map_err(|e| tagged("conflict", e))
    })
}

// ---- chunk refs as columns (byterefs layout) -------------------------------

/// One array's chunks as columns: `dims` (list of integer vectors, one per
/// dimension, 0-based chunk indices), `kind`, `location`, `offset`,
/// `length`, `etag`, `last_modified`, `key`, and `data` (inline bytes, NULL
/// otherwise). Built without per-row strings for the coordinates.
#[extendr]
fn ic_refs_columns_(s: ExternalPtr<RSession>, array_path: &str) -> R<List> {
    let mut dims: Vec<Vec<i32>> = Vec::new();
    let mut a_key: Vec<Rstr> = Vec::new();
    let empty = Rstr::from("");
    let mut a_kind: Vec<&'static str> = Vec::new();
    let mut a_loc: Vec<Rstr> = Vec::new();
    let mut interned: std::collections::HashMap<String, Rstr> = std::collections::HashMap::new();
    let mut a_off: Vec<f64> = Vec::new();
    let mut a_len: Vec<f64> = Vec::new();
    let mut a_etag: Vec<Option<String>> = Vec::new();
    let mut a_lm: Vec<f64> = Vec::new();
    let mut a_data: Vec<Robj> = Vec::new();
    let p = path(array_path)?;
    block_on(async {
        let g = s.session.read().await;
        let node = g.get_array(&p).await.map_err(ic)?;
        let mut st = g.all_node_chunks(&node).await.boxed();
        while let Some(item) = st.next().await {
            let (_p, info) = item.map_err(ic)?;
            let c = &info.coord.0;
            if dims.is_empty() {
                dims = vec![Vec::new(); c.len()];
            }
            for (d, v) in c.iter().enumerate() {
                dims[d].push(*v as i32);
            }
            // keys only where the caller needs to fetch bytes (native/inline)
            if matches!(info.payload, ChunkPayload::Virtual(_)) {
                a_key.push(empty.clone());
            } else {
                a_key.push(Rstr::from(chunk_key(&p, &info.coord).as_str()));
            }
            match info.payload {
                ChunkPayload::Ref(r) => {
                    a_kind.push("native");
                    a_loc.push(Rstr::from(format!("chunks/{}", r.id).as_str()));
                    a_off.push(r.offset as f64);
                    a_len.push(r.length as f64);
                    a_etag.push(None);
                    a_lm.push(f64::NAN);
                    a_data.push(().into());
                }
                ChunkPayload::Virtual(v) => {
                    let (e, l) = match v.checksum {
                        Some(Checksum::ETag(e)) => (Some(e.0), f64::NAN),
                        Some(Checksum::LastModified(t)) => (None, t.0 as f64),
                        None => (None, f64::NAN),
                    };
                    a_kind.push("virtual");
                    let u = v.location.url();
                    let r = interned.entry(u.to_string()).or_insert_with(|| Rstr::from(u)).clone();
                    a_loc.push(r);
                    a_off.push(v.offset as f64);
                    a_len.push(v.length as f64);
                    a_etag.push(e);
                    a_lm.push(l);
                    a_data.push(().into());
                }
                ChunkPayload::Inline(b) => {
                    a_kind.push("inline");
                    a_loc.push(Rstr::from(""));
                    a_off.push(f64::NAN);
                    a_len.push(b.len() as f64);
                    a_etag.push(None);
                    a_lm.push(f64::NAN);
                    a_data.push(Raw::from_bytes(&b).into());
                }
                _ => {
                    a_kind.push("unknown");
                    a_loc.push(Rstr::from(""));
                    a_off.push(f64::NAN);
                    a_len.push(f64::NAN);
                    a_etag.push(None);
                    a_lm.push(f64::NAN);
                    a_data.push(().into());
                }
            }
        }
        Ok::<(), Error>(())
    })?;
    let etag: Strings = a_etag.iter().map(|e| match e { Some(v) => Rstr::from(v.as_str()), None => Rstr::na() }).collect();
    let dims_r: Vec<Robj> = dims.into_iter().map(|v| Integers::from_values(v).into()).collect();
    Ok(list!(
        dims = List::from_values(dims_r),
        key = a_key.into_iter().collect::<Strings>(),
        kind = a_kind,
        location = a_loc.into_iter().collect::<Strings>(),
        offset = a_off,
        length = a_len,
        etag = etag,
        last_modified = a_lm,
        data = List::from_values(a_data)
    ))
}

// ---- chunk reference table ------------------------------------------------

fn coord_str(c: &ChunkIndices) -> String {
    c.0.iter().map(|i| i.to_string()).collect::<Vec<_>>().join(",")
}

fn chunk_key(p: &Path, c: &ChunkIndices) -> String {
    let base = p.to_string();
    let base = base.trim_start_matches('/');
    let idx = c.0.iter().map(|i| i.to_string()).collect::<Vec<_>>().join("/");
    if c.0.is_empty() {
        if base.is_empty() { "c".into() } else { format!("{base}/c") }
    } else if base.is_empty() {
        format!("c/{idx}")
    } else {
        format!("{base}/c/{idx}")
    }
}

/// One row per chunk: array path, chunk coord, zarr key, kind
/// (native | virtual | inline), location (virtual URL, or repo-relative
/// "chunks/<id>" for native), offset, length, checksum, and (optionally)
/// the inline bytes.
#[extendr]
fn ic_chunk_refs_(s: ExternalPtr<RSession>, array_path: &str, include_inline: bool) -> R<List> {
    let mut a_path = Vec::new();
    let mut a_coord = Vec::new();
    let mut a_key = Vec::new();
    let mut a_kind = Vec::new();
    let mut a_loc = Vec::new();
    let mut a_off: Vec<f64> = Vec::new();
    let mut a_len: Vec<f64> = Vec::new();
    let mut a_etag = Vec::new();
    let mut a_lm: Vec<f64> = Vec::new();
    let mut a_inline: Vec<Robj> = Vec::new();
    block_on(async {
        let g = s.session.read().await;
        let mut push = |p: Path, info: icechunk::format::manifest::ChunkInfo| {
            a_coord.push(coord_str(&info.coord));
            a_key.push(chunk_key(&p, &info.coord));
            a_path.push(p.to_string());
            let (kind, loc, off, len, etag, lm, inl): (&str, String, f64, f64, String, f64, Robj) =
                match info.payload {
                    ChunkPayload::Ref(r) => ("native", format!("chunks/{}", r.id), r.offset as f64, r.length as f64, String::new(), f64::NAN, ().into()),
                    ChunkPayload::Virtual(v) => {
                        let (e, l) = match v.checksum {
                            Some(Checksum::ETag(e)) => (e.0, f64::NAN),
                            Some(Checksum::LastModified(t)) => (String::new(), t.0 as f64),
                            None => (String::new(), f64::NAN),
                        };
                        ("virtual", v.location.url().to_string(), v.offset as f64, v.length as f64, e, l, ().into())
                    }
                    ChunkPayload::Inline(b) => {
                        let n = b.len() as f64;
                        let r: Robj = if include_inline { Raw::from_bytes(&b).into() } else { ().into() };
                        ("inline", String::new(), 0.0, n, String::new(), f64::NAN, r)
                    }
                    _ => ("unknown", String::new(), f64::NAN, f64::NAN, String::new(), f64::NAN, ().into()),
                };
            a_kind.push(kind.to_string());
            a_loc.push(loc);
            a_off.push(off);
            a_len.push(len);
            a_etag.push(etag);
            a_lm.push(lm);
            a_inline.push(inl);
        };
        if array_path.is_empty() {
            let mut st = g.all_chunks().await.map_err(ic)?.boxed();
            while let Some(item) = st.next().await {
                let (p, info) = item.map_err(ic)?;
                push(p, info);
            }
        } else {
            let node = g.get_array(&path(array_path)?).await.map_err(ic)?;
            let mut st = g.all_node_chunks(&node).await.boxed();
            while let Some(item) = st.next().await {
                let (p, info) = item.map_err(ic)?;
                push(p, info);
            }
        }
        Ok::<(), Error>(())
    })?;
    Ok(list!(
        path = a_path,
        coord = a_coord,
        key = a_key,
        kind = a_kind,
        location = a_loc,
        offset = a_off,
        length = a_len,
        etag = a_etag,
        last_modified = a_lm,
        inline = List::from_values(a_inline)
    ))
}

/// Nodes (groups and arrays) with shape and chunk counts.
#[extendr]
fn ic_nodes_(s: ExternalPtr<RSession>) -> R<List> {
    let mut p = Vec::new();
    let mut t = Vec::new();
    let mut shape = Vec::new();
    let mut nchunks = Vec::new();
    let mut dims = Vec::new();
    block_on(async {
        let g = s.session.read().await;
        let root = Path::root();
        for n in g.list_nodes(&root).await.map_err(ic)? {
            let n = n.map_err(ic)?;
            p.push(n.path.to_string());
            match &n.node_data {
                icechunk::format::snapshot::NodeData::Array { shape: sh, dimension_names, .. } => {
                    t.push("array".to_string());
                    let mut a = Vec::new();
                    let mut c = Vec::new();
                    for i in 0..sh.len() {
                        if let Some(d) = sh.get(i) {
                            a.push(d.array_length().to_string());
                            c.push(d.num_chunks().to_string());
                        }
                    }
                    shape.push(a.join(","));
                    nchunks.push(c.join(","));
                    dims.push(
                        dimension_names
                            .as_ref()
                            .map(|v| v.iter().map(|d| {
                                let o: Option<String> = d.clone().into();
                                o.unwrap_or_default()
                            }).collect::<Vec<_>>().join(","))
                            .unwrap_or_default(),
                    );
                }
                _ => {
                    t.push("group".to_string());
                    shape.push(String::new());
                    nchunks.push(String::new());
                    dims.push(String::new());
                }
            }
        }
        Ok::<(), Error>(())
    })?;
    Ok(list!(path = p, node_type = t, shape = shape, num_chunks = nchunks, dimension_names = dims))
}

#[extendr]
fn ic_virtual_locations_(s: ExternalPtr<RSession>) -> R<Vec<String>> {
    block_on(async {
        let g = s.session.read().await;
        let v: Vec<String> = g.all_virtual_chunk_locations().await.map_err(ic)?.try_collect().await.map_err(ic)?;
        Ok(v)
    })
}

// ---- handoff ----------------------------------------------------------------
//
// Header (DESIGN.md 2.1): b"ICR1" + u16 LE N + N bytes ASCII icechunk crate
// version + rmp payload from Session::as_bytes().

pub const ICECHUNK_CRATE_VERSION: &str = env!("ICECHUNK_CRATE_VERSION");
const MAGIC: &[u8; 4] = b"ICR1";

pub fn encode_handoff(payload: &[u8]) -> Vec<u8> {
    let v = ICECHUNK_CRATE_VERSION.as_bytes();
    let mut out = Vec::with_capacity(6 + v.len() + payload.len());
    out.extend_from_slice(MAGIC);
    out.extend_from_slice(&(v.len() as u16).to_le_bytes());
    out.extend_from_slice(v);
    out.extend_from_slice(payload);
    out
}

pub fn decode_handoff(b: &[u8]) -> std::result::Result<&[u8], String> {
    if b.len() < 6 || &b[0..4] != MAGIC {
        return Err("not icechunk session bytes (missing ICR1 header)".into());
    }
    let n = u16::from_le_bytes([b[4], b[5]]) as usize;
    if b.len() < 6 + n {
        return Err("truncated icechunk session header".into());
    }
    let v = std::str::from_utf8(&b[6..6 + n]).map_err(|_| "bad version in header".to_string())?;
    if v != ICECHUNK_CRATE_VERSION {
        return Err(format!(
            "session bytes were written by icechunk {v} but this build has icechunk {ICECHUNK_CRATE_VERSION}"
        ));
    }
    Ok(&b[6 + n..])
}

#[extendr]
fn ic_session_bytes_(s: ExternalPtr<RSession>) -> R<Raw> {
    let g = s.session.blocking_read();
    let payload = g.as_bytes().map_err(ic)?;
    Ok(Raw::from_bytes(&encode_handoff(&payload)))
}

#[extendr]
fn ic_session_from_bytes_(b: Raw) -> R<ExternalPtr<RSession>> {
    let payload = decode_handoff(b.as_slice()).map_err(|e| tagged("version", e))?;
    let s = Session::from_bytes(payload).map_err(ic)?;
    Ok(wrap_session(s))
}

#[extendr]
fn ic_session_fork_(s: ExternalPtr<RSession>) -> R<ExternalPtr<RSession>> {
    let f = block_on(async { s.session.read().await.fork().await }).map_err(ic)?;
    Ok(wrap_session(f))
}

#[extendr]
fn ic_session_merge_(s: ExternalPtr<RSession>, b: Raw) -> R<()> {
    let payload = decode_handoff(b.as_slice()).map_err(|e| tagged("version", e))?;
    let other = Session::from_bytes(payload).map_err(ic)?;
    block_on(async { s.session.write().await.merge(other).await }).map_err(ic)
}

#[extendr]
fn ic_handoff_version_() -> &'static str {
    ICECHUNK_CRATE_VERSION
}

#[allow(dead_code)]
fn _unused(_: KeyNotFoundError) {}

extendr_module! {
    mod session;
    fn ic_session_info_;
    fn ic_status_;
    fn ic_get_;
    fn ic_get_many_;
    fn ic_set_;
    fn ic_exists_;
    fn ic_delete_;
    fn ic_list_;
    fn ic_set_virtual_ref_;
    fn ic_set_virtual_refs_;
    fn ic_commit_;
    fn ic_discard_;
    fn ic_rebase_;
    fn ic_chunk_refs_;
    fn ic_refs_columns_;
    fn ic_nodes_;
    fn ic_virtual_locations_;
    fn ic_session_bytes_;
    fn ic_session_from_bytes_;
    fn ic_session_fork_;
    fn ic_session_merge_;
    fn ic_handoff_version_;
}
