//! Errors cross into R as messages prefixed with "[icechunk:<class>] ".
//! The R side (R/errors.R) turns the prefix into a condition subclass.

use extendr_api::Error;
use std::fmt::Display;

use std::sync::Once;

static HOOK: Once = Once::new();

/// extendr turns an Err into an R error by panicking and catching; keep
/// the default hook for real panics but silence our tagged errors.
fn quiet_hook() {
    HOOK.call_once(|| {
        let prev = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            let p = info.payload();
            let msg = p.downcast_ref::<String>().map(|s| s.as_str())
                .or_else(|| p.downcast_ref::<&str>().copied())
                .unwrap_or("");
            if !msg.starts_with("[icechunk:") {
                prev(info);
            }
        }));
    });
}

pub(crate) fn tagged(class: &str, e: impl Display) -> Error {
    quiet_hook();
    let mut msg = e.to_string();
    if let Some(i) = msg.find("\n\ncontext:") {
        msg.truncate(i);
    }
    Error::Other(format!("[icechunk:{class}] {}", msg.trim_end()))
}

pub(crate) fn ic<E: Display>(e: E) -> Error {
    let msg = e.to_string();
    let lower = msg.to_lowercase();
    let class = if lower.contains("conflict") || lower.contains("rebase") {
        "conflict"
    } else if lower.contains("not found") || lower.contains("does not exist")
        || lower.contains("cannot find") || lower.contains("no such")
    {
        "not_found"
    } else if lower.contains("credential") || lower.contains("unauthori")
        || lower.contains("forbidden") || lower.contains("access denied")
        || lower.contains("not authorized")
    {
        "auth"
    } else {
        "error"
    };
    tagged(class, msg)
}

pub(crate) fn arg(msg: impl Display) -> Error {
    tagged("argument", msg)
}

pub(crate) type R<T> = std::result::Result<T, Error>;
