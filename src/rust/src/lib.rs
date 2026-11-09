use extendr_api::prelude::*;

mod err;
mod repo;
mod runtime;
mod session;
mod storage;

/// Version of the icechunk crate compiled into this package.
/// @noRd
#[extendr]
fn ic_icechunk_version_() -> &'static str {
    session::ICECHUNK_CRATE_VERSION
}

/// Cargo features compiled in (the pizzarr#185 lesson: check the binary).
/// @noRd
#[extendr]
fn ic_compiled_features_() -> Vec<String> {
    let mut f = Vec::new();
    if cfg!(feature = "minimal") { f.push("minimal".to_string()); f.push("fs".into()); f.push("http".into()); }
    if cfg!(feature = "cloud") { f.push("cloud".into()); f.push("s3_object_store".into()); f.push("gcs".into()); f.push("azure".into()); }
    if cfg!(feature = "full") { f.push("full".into()); f.push("s3_aws".into()); f.push("redirect".into()); }
    f
}

extendr_module! {
    mod icechunkr;
    use storage;
    use repo;
    use session;
    fn ic_icechunk_version_;
    fn ic_compiled_features_;
}
