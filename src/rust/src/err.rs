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

/// Like [ic], but appends the `source()` chain. Some icechunk errors (e.g.
/// "error fetching virtual reference") only say what failed in their source.
pub(crate) fn ic_chain<E: std::error::Error + std::fmt::Debug>(e: &E) -> Error {
    let mut msg = e.to_string();
    if let Some(i) = msg.find("\n\ncontext:") {
        msg.truncate(i);
    }
    let mut src = e.source();
    let mut extended = false;
    while let Some(c) = src {
        let mut part = c.to_string();
        if let Some(i) = part.find("\n\ncontext:") {
            part.truncate(i);
        }
        if !msg.contains(&part) {
            msg.push_str(": ");
            msg.push_str(&part);
            extended = true;
        }
        src = c.source();
    }
    if !extended {
        // The Display chain said nothing more; the Debug form of the kind
        // usually carries the underlying cause (e.g. an SDK dispatch failure).
        let mut dbg = format!("{e:?}");
        if let Some(i) = dbg.find(", context:") {
            dbg.truncate(i);
            dbg.push_str(" }");
        }
        if dbg.len() > 800 {
            // truncate() panics off a char boundary; back up to one.
            let mut i = 800;
            while !dbg.is_char_boundary(i) {
                i -= 1;
            }
            dbg.truncate(i);
        }
        // Skip it when it only repeats the message, e.g.
        // ObjectModified("<the path already in the message>").
        let lits = debug_literals(&dbg);
        if lits.is_empty() || lits.iter().any(|l| !msg.contains(l.as_str())) {
            msg.push_str(&format!(" [cause: {dbg}]"));
        }
    }
    ic(msg)
}

/// The double-quoted string literals in a Debug rendering. Escapes are
/// taken literally (`\n` -> `n`), so an escaped literal will not match the
/// message and the Debug is kept: the safe side.
fn debug_literals(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut buf = String::new();
    let (mut in_str, mut esc) = (false, false);
    for ch in s.chars() {
        if !in_str {
            if ch == '"' {
                in_str = true;
                buf.clear();
            }
        } else if esc {
            buf.push(ch);
            esc = false;
        } else if ch == '\\' {
            esc = true;
        } else if ch == '"' {
            out.push(buf.clone());
            in_str = false;
        } else {
            buf.push(ch);
        }
    }
    out
}

pub(crate) fn arg(msg: impl Display) -> Error {
    tagged("argument", msg)
}

pub(crate) type R<T> = std::result::Result<T, Error>;
