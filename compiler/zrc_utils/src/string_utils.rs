//! String utility functions for the Zirco compiler
//!
//! This module provides common string manipulation utilities used throughout
//! the compiler pipeline.

/// Removes all underscore separators from a numeric string literal.
///
/// This is commonly used when parsing number literals that may contain
/// underscores for readability (e.g., `1_000_000` becomes `1000000`).
///
/// # Arguments
/// * `input` - The input string containing potential underscore separators
///
/// # Returns
/// A new string with all underscores removed
///
/// # Examples
/// ```
/// use zrc_utils::string_utils::remove_underscores;
///
/// assert_eq!(remove_underscores("1_000_000"), "1000000");
/// assert_eq!(remove_underscores("0xFF_AA_BB"), "0xFFAABB");
/// assert_eq!(remove_underscores("no_underscores"), "nounderscores");
/// ```
#[must_use]
pub fn remove_underscores(input: &str) -> String {
    input.replace('_', "")
}

/// Checks if a string represents a valid identifier in Zirco.
///
/// A valid identifier must:
/// - Start with a letter (a-z, A-Z) or underscore (_)
/// - Contain only letters, digits (0-9), or underscores
/// - Not be empty
///
/// # Arguments
/// * `input` - The string to validate as an identifier
///
/// # Returns
/// `true` if the string is a valid identifier, `false` otherwise
///
/// # Panics
/// This function should not panic as it checks for empty input before accessing characters.
///
/// # Examples
/// ```
/// use zrc_utils::string_utils::is_valid_identifier;
///
/// assert!(is_valid_identifier("valid_name"));
/// assert!(is_valid_identifier("_private"));
/// assert!(is_valid_identifier("name123"));
/// assert!(!is_valid_identifier("123invalid"));
/// assert!(!is_valid_identifier(""));
/// assert!(!is_valid_identifier("invalid-name"));
/// ```
#[must_use]
pub fn is_valid_identifier(input: &str) -> bool {
    if input.is_empty() {
        return false;
    }

    let mut chars = input.chars();
    let first = chars.next().expect("input is not empty");

    // First character must be letter or underscore
    if !first.is_ascii_alphabetic() && first != '_' {
        return false;
    }

    // Remaining characters must be alphanumeric or underscore
    chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_remove_underscores() {
        assert_eq!(remove_underscores("1_000_000"), "1000000");
        assert_eq!(remove_underscores("0xFF_AA_BB"), "0xFFAABB");
        assert_eq!(remove_underscores("no_change"), "nochange");
        assert_eq!(remove_underscores(""), "");
        assert_eq!(remove_underscores("_only_underscores_"), "onlyunderscores");
    }

    #[test]
    fn test_is_valid_identifier() {
        // Valid identifiers
        assert!(is_valid_identifier("valid_name"));
        assert!(is_valid_identifier("_private"));
        assert!(is_valid_identifier("name123"));
        assert!(is_valid_identifier("a"));
        assert!(is_valid_identifier("_"));
        assert!(is_valid_identifier("CamelCase"));
        assert!(is_valid_identifier("snake_case_123"));

        // Invalid identifiers
        assert!(!is_valid_identifier("123invalid"));
        assert!(!is_valid_identifier(""));
        assert!(!is_valid_identifier("invalid-name"));
        assert!(!is_valid_identifier("invalid.name"));
        assert!(!is_valid_identifier("invalid name"));
        assert!(!is_valid_identifier("invalid@name"));
    }
}
