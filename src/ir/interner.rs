use std::sync::LazyLock;

use lasso::Spur;
use lasso::ThreadedRodeo;

/// An interner using lasso: https://crates.io/crates/lasso
/// Not actually used in any threaded context (that all ust operates on the Spurs)
/// but the ThreadedRodeo lets us use LazyLock and avoid passing the rodeo around.
static INTERNER: LazyLock<ThreadedRodeo> = LazyLock::new(ThreadedRodeo::default);

/// Intern a string and return the Spur key.
pub fn intern(s: &str) -> Spur {
    INTERNER.get_or_intern(s)
}

/// Get the interned string for a Spur.
pub fn intern_resolve(spur: Spur) -> &'static str {
    INTERNER.resolve(&spur)
}
