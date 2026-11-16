//! Process-global tokio runtime. Every exported function blocks on it.
//!
//! It is created on first use and can be shut down (`shutdown()`, called from
//! an R exit finalizer and from `.onUnload()`): on Windows, a process that
//! exits with the runtime still live (worker threads, open HTTP/TLS
//! connections) crashes in ExitProcess, which kills the workers and then runs
//! thread-local and DLL teardown on the main thread. Use after shutdown
//! starts a new runtime.

use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio::runtime::Runtime;

static RT: Mutex<Option<Arc<Runtime>>> = Mutex::new(None);

fn build() -> Runtime {
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
}

pub(crate) fn rt() -> Arc<Runtime> {
    let mut g = RT.lock().unwrap_or_else(|e| e.into_inner());
    g.get_or_insert_with(|| Arc::new(build())).clone()
}

pub(crate) fn block_on<F: std::future::Future>(f: F) -> F::Output {
    rt().block_on(f)
}

/// Stop the worker threads and drop every task (connection pools included).
/// Returns false if there was no runtime, or it is in use elsewhere (then it
/// is left alone).
pub(crate) fn shutdown() -> bool {
    let taken = RT.lock().unwrap_or_else(|e| e.into_inner()).take();
    match taken.map(Arc::try_unwrap) {
        Some(Ok(rt)) => {
            rt.shutdown_timeout(Duration::from_secs(2));
            true
        }
        Some(Err(shared)) => {
            *RT.lock().unwrap_or_else(|e| e.into_inner()) = Some(shared);
            false
        }
        None => false,
    }
}
