"""Build the icechunkr test fixture corpus.

With uv (what CI runs), from the repository root:
  uv run --no-project --python 3.12 --with "icechunk==2.3.*" --with "zarr>=3" --with numpy --with h5py python -I fixtures/make_fixtures.py
  uv run --no-project --python 3.12 --with "icechunk<2" --with "zarr>=3" --with numpy python -I fixtures/make_fixtures.py --v1

Every fixture gets expected/<fixture>/<array>.npy and an entry in
manifest.json (snapshot ids, refs, shapes, dtypes, chunking). Content is
deterministic; snapshot ids are not, so they are recorded per generation.
"""

import argparse
import json
import os
import shutil
import sys
from pathlib import Path

import icechunk as ic
import numpy as np
import zarr

HERE = Path(__file__).resolve().parent
OUT = HERE / "repos"
EXP = HERE / "expected"


def fresh(name):
    p = OUT / name
    if p.exists():
        shutil.rmtree(p)
    p.parent.mkdir(parents=True, exist_ok=True)
    return p


def save_expected(fixture, arrays):
    d = EXP / fixture
    if d.exists():
        shutil.rmtree(d)
    d.mkdir(parents=True)
    for name, a in arrays.items():
        np.save(d / f"{name.strip('/').replace('/', '__')}.npy", a)


def describe(group):
    out = {}
    for name, arr in group.arrays():
        out[name] = {
            "shape": list(arr.shape),
            "chunks": list(arr.chunks),
            "shards": list(arr.shards) if arr.shards else None,
            "dtype": str(arr.dtype),
            "fill_value": str(arr.fill_value),
        }
    return out


def refs(repo):
    return {
        "branches": {b: repo.lookup_branch(b) for b in sorted(repo.list_branches())},
        "tags": {t: repo.lookup_tag(t) for t in sorted(repo.list_tags())},
        "ancestry_main": [s.id for s in repo.ancestry(branch="main")],
    }


def rng(seed=42):
    return np.random.default_rng(seed)


# ---------------------------------------------------------------- native

BENCH_FORMULA = "np.arange(4096*4096, dtype=float32) % 1000, reshaped (4096, 4096)"


def bench_array():
    return (np.arange(4096 * 4096, dtype=np.float32) % 1000).reshape(4096, 4096)


def native(name="native_v2", spec_version=None, bench=True):
    p = fresh(name)
    kw = {} if spec_version is None else {"spec_version": spec_version}
    repo = ic.Repository.create(ic.local_filesystem_storage(str(p)), **kw)
    s = repo.writable_session("main")
    root = zarr.group(store=s.store, zarr_format=3)
    root.attrs["title"] = "icechunk-r native fixture"
    r = rng()
    expected = {}

    if bench:
        # The pizzarr#196 benchmark case: 64 one-MiB float32 chunks, no codec.
        # Expected values are BENCH_FORMULA (not saved as .npy: 64 MiB).
        a = bench_array()
        z = root.create_array("bench_f32", shape=a.shape, chunks=(512, 512), dtype="float32",
                              compressors=None, fill_value=np.nan, dimension_names=["y", "x"])
        z[:] = a
        z = root.create_array("bench_f32_zstd", shape=a.shape, chunks=(512, 512), dtype="float32",
                              compressors=zarr.codecs.ZstdCodec(level=3), fill_value=np.nan,
                              dimension_names=["y", "x"])
        z[:] = a
    else:
        a = (np.arange(256 * 256, dtype=np.float32) % 1000).reshape(256, 256)
        z = root.create_array("f32", shape=a.shape, chunks=(64, 64), dtype="float32",
                              compressors=None, fill_value=np.nan, dimension_names=["y", "x"])
        z[:] = a
        expected["f32"] = a

    # int16 with fill value; only some chunks written (the rest are absent).
    b = np.full((100, 120), -999, dtype=np.int16)
    b[:50, :60] = r.integers(-500, 500, size=(50, 60), dtype=np.int16)
    z = root.create_array("int16_fill", shape=b.shape, chunks=(50, 60), dtype="int16",
                          fill_value=-999, dimension_names=["y", "x"])
    z[:50, :60] = b[:50, :60]
    expected["int16_fill"] = b

    # uint8 2D, ragged edge chunks.
    c = r.integers(0, 255, size=(37, 53), dtype=np.uint8)
    z = root.create_array("uint8_2d", shape=c.shape, chunks=(16, 16), dtype="uint8", fill_value=0)
    z[:] = c
    expected["uint8_2d"] = c

    # 3D sharded.
    d = r.standard_normal((8, 64, 64)).astype("float64")
    z = root.create_array("sharded_3d", shape=d.shape, chunks=(1, 16, 16), shards=(4, 32, 32),
                          dtype="float64", fill_value=0.0, dimension_names=["t", "y", "x"])
    z[:] = d
    expected["sharded_3d"] = d

    # nested group
    g = root.create_group("nested")
    e = np.arange(10, dtype="int32")
    z = g.create_array("vec", shape=e.shape, chunks=(4,), dtype="int32", fill_value=0)
    z[:] = e
    expected["nested/vec"] = e

    # variable-length strings
    try:
        f = np.array(["a", "bb", "ccc", "", "eeeee"], dtype=object)
        z = root.create_array("strings", shape=f.shape, chunks=(2,), dtype=str)
        z[:] = f
        expected["strings"] = f.astype(str)
    except Exception as ex:  # pragma: no cover
        print("strings array skipped:", ex, file=sys.stderr)

    s.commit("native fixture")
    save_expected(name, expected)
    out = {"path": f"repos/{name}", "arrays": describe(root), **refs(repo),
           "spec_version": spec_version or "default"}
    if bench:
        out["bench_expected"] = BENCH_FORMULA
    return out


# ---------------------------------------------------------------- inline

def inline():
    name = "inline_v2"
    p = fresh(name)
    cfg = ic.RepositoryConfig.default()
    cfg.inline_chunk_threshold_bytes = 4096
    repo = ic.Repository.create(ic.local_filesystem_storage(str(p)), config=cfg)
    s = repo.writable_session("main")
    root = zarr.group(store=s.store, zarr_format=3)
    a = np.arange(200, dtype="float32").reshape(10, 20)
    z = root.create_array("small", shape=a.shape, chunks=(5, 10), dtype="float32",
                          compressors=None, fill_value=0.0)
    z[:] = a
    s.commit("inline fixture")
    save_expected(name, {"small": a})
    return {"path": f"repos/{name}", "arrays": describe(root), **refs(repo),
            "inline_chunk_threshold_bytes": 4096}


# ---------------------------------------------------------------- virtual (local)

def virtual_local():
    """Virtual refs into local files: a raw little-endian binary and a
    contiguous NetCDF4 variable (offset found with h5py)."""
    name = "virtual_v2"
    p = fresh(name)
    src = OUT / "virtual_sources"
    if src.exists():
        shutil.rmtree(src)
    src.mkdir(parents=True)

    a = (np.arange(64 * 48, dtype="<f4") / 10).reshape(64, 48)
    raw = src / "grid.bin"
    # four row-blocks of 16 rows each, laid out one after another
    raw.write_bytes(a.tobytes())

    import h5py
    nc = src / "grid.nc"
    with h5py.File(nc, "w") as f:
        dset = f.create_dataset("v", data=a, chunks=None)  # contiguous
    with h5py.File(nc, "r") as f:
        nc_off = int(f["v"].id.get_offset())
        nc_len = int(f["v"].id.get_storage_size())

    prefix = f"file://{src}/"
    cfg = ic.RepositoryConfig.default()
    cfg.set_virtual_chunk_container(
        ic.VirtualChunkContainer(prefix, ic.local_filesystem_store(str(src))))
    repo = ic.Repository.create(
        ic.local_filesystem_storage(str(p)), config=cfg,
        authorize_virtual_chunk_access={prefix: ic.credentials.LocalFileSystemAccess})
    s = repo.writable_session("main")
    root = zarr.group(store=s.store, zarr_format=3)
    z = root.create_array("from_bin", shape=a.shape, chunks=(16, 48), dtype="float32",
                          compressors=None, fill_value=np.nan, dimension_names=["y", "x"])
    rowbytes = 48 * 4
    for i in range(4):
        s.store.set_virtual_ref(f"from_bin/c/{i}/0", f"{prefix}grid.bin",
                                offset=i * 16 * rowbytes, length=16 * rowbytes)
    z = root.create_array("from_nc", shape=a.shape, chunks=a.shape, dtype="float32",
                          compressors=None, fill_value=np.nan, dimension_names=["y", "x"])
    s.store.set_virtual_ref("from_nc/c/0/0", f"{prefix}grid.nc", offset=nc_off, length=nc_len)
    s.commit("virtual fixture (local)")
    save_expected(name, {"from_bin": a, "from_nc": a})
    return {"path": f"repos/{name}", "arrays": describe(root), **refs(repo),
            "virtual_prefix": prefix, "sources": {"grid.bin": raw.stat().st_size,
                                                   "grid.nc": [nc_off, nc_len]}}


# ---------------------------------------------------------------- history

def history():
    name = "history_v2"
    p = fresh(name)
    repo = ic.Repository.create(ic.local_filesystem_storage(str(p)))
    s = repo.writable_session("main")
    root = zarr.group(store=s.store, zarr_format=3)
    z = root.create_array("x", shape=(4,), chunks=(2,), dtype="int32", fill_value=0)
    z[:] = [1, 1, 1, 1]
    c1 = s.commit("c1: ones")

    s = repo.writable_session("main")
    zarr.open_array(s.store, path="x")[:2] = [2, 2]
    c2 = s.commit("c2: first half twos")
    repo.create_tag("v1", c2)

    s = repo.writable_session("main")
    zarr.open_array(s.store, path="x")[2:] = [3, 3]
    c3 = s.commit("c3: second half threes (to be amended)")

    s = repo.writable_session("main")
    zarr.open_array(s.store, path="x")[2:] = [4, 4]
    c3a = s.amend("c3 amended: second half fours")

    repo.create_branch("dev", c2)
    s = repo.writable_session("dev")
    zarr.open_array(s.store, path="x")[:] = [9, 9, 9, 9]
    zarr.open_group(s.store).attrs["branch"] = "dev"
    d1 = s.commit("dev: nines")

    exp = {"x_main": np.array([2, 2, 4, 4], "int32"), "x_v1": np.array([2, 2, 1, 1], "int32"),
           "x_dev": np.array([9, 9, 9, 9], "int32"), "x_c1": np.array([1, 1, 1, 1], "int32")}
    save_expected(name, exp)
    return {"path": f"repos/{name}", **refs(repo),
            "snapshots": {"c1": c1, "c2": c2, "c3_before_amend": c3, "c3_amended": c3a, "dev1": d1},
            "dev_ancestry": [s.id for s in repo.ancestry(branch="dev")]}


def key_digests(path, **open_kw):
    """sha256 + length of every zarr key, as read through icechunk's store,
    for each branch and tag: the bytes-level ground truth for other readers."""
    import hashlib
    from zarr.core.buffer import default_buffer_prototype
    from zarr.core.sync import sync
    repo = ic.Repository.open(ic.local_filesystem_storage(str(path)), **open_kw)
    out = {}
    versions = [("branch", b) for b in sorted(repo.list_branches())] + \
               [("tag", t) for t in sorted(repo.list_tags())]
    for kind, name in versions:
        s = repo.readonly_session(**{kind: name})
        st = s.store

        async def collect():
            keys = [k async for k in st.list()]
            res = {}
            for k in sorted(keys):
                b = await st.get(k, prototype=default_buffer_prototype())
                if b is not None:
                    raw = b.to_bytes()
                    res[k] = [hashlib.sha256(raw).hexdigest(), len(raw)]
            return res
        out[f"{kind}:{name}"] = sync(collect())
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--v1", action="store_true", help="write native_v1 only (icechunk 1.x)")
    args = ap.parse_args()
    mpath = HERE / "manifest.json"
    manifest = json.loads(mpath.read_text()) if mpath.exists() else {}
    manifest.setdefault("fixtures", {})
    ver = ic.__version__
    if args.v1:
        assert ver.startswith("1."), f"--v1 needs icechunk 1.x, have {ver}"
        manifest["fixtures"]["native_v1"] = native("native_v1", bench=False) | {"written_by": ver}
    else:
        assert ver.startswith("2."), f"needs icechunk 2.x, have {ver}"
        for k, fn in [("native_v2", native), ("inline_v2", inline),
                      ("virtual_v2", virtual_local), ("history_v2", history)]:
            manifest["fixtures"][k] = fn() | {"written_by": ver}
        # spec v1 written by icechunk 2 (distinct from a repo written by 1.x)
        manifest["fixtures"]["native_spec1_by_v2"] = native("native_spec1_by_v2", spec_version=1, bench=False) | {"written_by": ver}
    for k, fx in manifest["fixtures"].items():
        if args.v1 != (k == "native_v1"):
            continue
        kw = {}
        if "virtual_prefix" in fx:
            kw["authorize_virtual_chunk_access"] = {fx["virtual_prefix"]: ic.credentials.LocalFileSystemAccess} \
                if hasattr(ic.credentials, "LocalFileSystemAccess") else ic.containers_credentials({fx["virtual_prefix"]: None})
        d = EXP / k
        d.mkdir(parents=True, exist_ok=True)
        (d / "keys.json").write_text(json.dumps(key_digests(HERE / fx["path"], **kw), indent=1) + "\n")
    manifest["versions"] = manifest.get("versions", {}) | {
        ("icechunk_v1" if args.v1 else "icechunk_v2"): ver, "zarr": zarr.__version__,
        "numpy": np.__version__}
    mpath.write_text(json.dumps(manifest, indent=2, default=str, allow_nan=False) + "\n")
    print("wrote", mpath)


if __name__ == "__main__":
    main()
