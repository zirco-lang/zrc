//! List of lints provided by zircop

mod empty_struct_used;
mod underscore_variable_used;
mod unused_variables;

use crate::pass::PassList;

/// Returns the default set of lints provided by Zircop
#[must_use]
pub fn get_default_lints() -> PassList {
    PassList::new(vec![
        empty_struct_used::EmptyStructUsedLint::init(),
        underscore_variable_used::UnderscoreVariableUsedLint::init(),
        unused_variables::UnusedVariablesLint::init(),
    ])
}
