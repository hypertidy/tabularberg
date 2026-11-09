//! Process-global tokio runtime. Every exported function blocks on it.

use std::sync::OnceLock;
use tokio::runtime::Runtime;

static RT: OnceLock<Runtime> = OnceLock::new();

pub(crate) fn rt() -> &'static Runtime {
    RT.get_or_init(|| {
        let threads = std::env::var("ICECHUNK_R_THREADS")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .filter(|&n| n > 0)
            .unwrap_or(4);
        tokio::runtime::Builder::new_multi_thread()
            .worker_threads(threads)
            .enable_all()
            .build()
            .expect("icechunk: failed to create tokio runtime")
    })
}

pub(crate) fn block_on<F: std::future::Future>(f: F) -> F::Output {
    rt().block_on(f)
}
