use std::sync::OnceLock;

/// C to Zirco translator module using regex-based pattern matching
use regex::Regex;

/// Get the function declaration regex
fn function_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| {
        // Matches: return_type function_name(params);
        // This is a simplified regex that captures most common function declarations
        Regex::new(
            r"(?m)^\s*([a-zA-Z_][a-zA-Z0-9_\s\*]*)\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(([^)]*)\)\s*;",
        )
        .expect("Invalid regex")
    })
}

/// Get the struct declaration regex
fn struct_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| {
        Regex::new(r"(?ms)struct\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\{([^}]*)\}\s*;")
            .expect("Invalid regex")
    })
}

/// Get the typedef struct regex
fn typedef_struct_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| {
        // Matches typedef struct declarations
        let pattern = concat!(
            r"(?ms)typedef\s+struct\s+",
            r"([a-zA-Z_][a-zA-Z0-9_]*)??\s*",
            r"\{([^}]*)\}\s*",
            r"([a-zA-Z_][a-zA-Z0-9_]*)\s*;"
        );
        Regex::new(pattern).expect("Invalid regex")
    })
}

/// Translate C code to Zirco
pub fn translate(input: &str) -> String {
    let mut output = String::new();
    output.push_str("#pragma once\n\n");

    // Remove preprocessor directives and comments
    let cleaned = preprocess_input(input);

    // Translate typedef structs first
    for caps in typedef_struct_regex().captures_iter(&cleaned) {
        let name = caps.get(3).map_or("", |cap| cap.as_str());
        let body = caps.get(2).map_or("", |cap| cap.as_str());

        if !name.is_empty() {
            output.push_str(&translate_struct(name, body));
            output.push('\n');
        }
    }

    // Translate regular structs
    for caps in struct_regex().captures_iter(&cleaned) {
        let name = caps.get(1).map_or("", |cap| cap.as_str());
        let body = caps.get(2).map_or("", |cap| cap.as_str());

        output.push_str(&translate_struct(name, body));
        output.push('\n');
    }

    // Translate function declarations
    for caps in function_regex().captures_iter(&cleaned) {
        let return_type = caps.get(1).map_or("", |cap| cap.as_str()).trim();
        let name = caps.get(2).map_or("", |cap| cap.as_str());
        let params = caps.get(3).map_or("", |cap| cap.as_str());

        output.push_str(&translate_function(name, return_type, params));
        output.push('\n');
    }

    output
}

/// Remove comments and preprocessor directives
fn preprocess_input(input: &str) -> String {
    let mut result = String::new();

    for line in input.lines() {
        let trimmed = line.trim();

        // Skip preprocessor directives except #pragma once
        if trimmed.starts_with('#') {
            if trimmed.starts_with("#pragma once") {
                // Already handled
            }
            continue;
        }

        // Remove single-line comments
        if let Some(pos) = line.find("//") {
            result.push_str(&line[..pos]);
        } else {
            result.push_str(line);
        }
        result.push('\n');
    }

    // Remove block comments
    let mut output = String::new();
    let mut in_comment = false;
    let mut chars = result.chars().peekable();

    while let Some(current_char) = chars.next() {
        if !in_comment && current_char == '/' && chars.peek() == Some(&'*') {
            in_comment = true;
            chars.next(); // consume '*'
        } else if in_comment && current_char == '*' && chars.peek() == Some(&'/') {
            in_comment = false;
            chars.next(); // consume '/'
        } else if !in_comment {
            output.push(current_char);
        } else {
            // In comment, skip
        }
    }

    output
}

/// Translate a struct declaration
fn translate_struct(name: &str, body: &str) -> String {
    let mut output = format!("struct {name} {{\n");

    for line in body.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        // Parse field: type name;
        if let Some((field_type, field_name)) = parse_field(trimmed) {
            output.push_str("    ");
            output.push_str(&field_name);
            output.push_str(": ");
            output.push_str(&field_type);
            output.push_str(",\n");
        }
    }

    output.push_str("}\n");
    output
}

/// Parse a struct field
fn parse_field(line: &str) -> Option<(String, String)> {
    let line = line.trim_end_matches(';').trim();

    // Split by last space to get type and name
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() < 2 {
        return None;
    }

    let field_name = parts[parts.len() - 1];
    let field_type = parts[..parts.len() - 1].join(" ");

    let translated_type = translate_type(&field_type);
    Some((translated_type, field_name.to_string()))
}

/// Translate a function declaration
fn translate_function(name: &str, return_type: &str, params: &str) -> String {
    let ret_type = translate_type(return_type);
    let param_list = translate_params(params);

    format!("fn {name}({param_list}) -> {ret_type};\n")
}

/// Translate function parameters
fn translate_params(params: &str) -> String {
    let params = params.trim();

    // Handle void or empty
    if params.is_empty() || params == "void" {
        return String::new();
    }

    // Check for variadic
    if params.contains("...") {
        let temp = params.replace("...", "");
        let non_variadic = temp.trim_end_matches(',').trim();
        if non_variadic.is_empty() {
            return "...".to_string();
        }
        let translated = translate_param_list(non_variadic);
        return format!("{translated}, ...");
    }

    translate_param_list(params)
}

/// Translate a list of parameters
fn translate_param_list(params: &str) -> String {
    params
        .split(',')
        .filter_map(|param| {
            let param = param.trim();
            if param.is_empty() {
                return None;
            }

            // Find the identifier and separate it from pointers
            // In C: "void *ptr" or "void* ptr" or "void * ptr"
            // We need to identify "ptr" as the name and "void *" as the type
            let parts: Vec<&str> = param.split_whitespace().collect();
            if parts.is_empty() {
                return None;
            }

            // Find the last identifier that's not just asterisks
            let mut last_ident_idx = None;
            for (idx, part) in parts.iter().enumerate().rev() {
                // Strip leading asterisks to see if there's an identifier
                let stripped = part.trim_start_matches('*');
                if !stripped.is_empty()
                    && stripped
                        .chars()
                        .next()
                        .is_some_and(|ch| ch.is_alphabetic() || ch == '_')
                {
                    last_ident_idx = Some(idx);
                    break;
                }
            }

            last_ident_idx.map_or_else(
                || {
                    // No identifier found, just a type
                    let translated_type = translate_type(param);
                    Some(translated_type)
                },
                |idx| {
                    // Extract the name (removing leading asterisks)
                    let param_name = parts[idx].trim_start_matches('*');

                    // Everything before the name (and any asterisks in the name) is the type
                    let mut type_str = parts[..idx].join(" ");

                    // Count asterisks in the name part and add them to the type
                    let asterisk_count = parts[idx].chars().take_while(|&ch| ch == '*').count();
                    for _ in 0..asterisk_count {
                        type_str.push('*');
                    }

                    let translated_type = translate_type(&type_str);
                    Some(format!("{param_name}: {translated_type}"))
                },
            )
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// Translate C types to Zirco types
fn translate_type(c_type: &str) -> String {
    let c_type = c_type.trim();

    // Remove const, volatile, static, extern, inline keywords
    let mut ty = c_type
        .replace("const", "")
        .replace("volatile", "")
        .replace("static", "")
        .replace("extern", "")
        .replace("inline", "")
        .replace("  ", " ")
        .trim()
        .to_string();

    // Count and remove pointers
    let mut pointer_count = 0;
    while ty.ends_with('*') {
        pointer_count += 1;
        ty.pop();
        ty = ty.trim().to_string();
    }

    // Handle const after type
    ty = ty
        .replace("const", "")
        .replace("  ", " ")
        .trim()
        .to_string();

    // Translate base types
    let base_type = match ty.as_str() {
        "void" => "void",
        "char" => "i8",
        "unsigned char" => "u8",
        "short" | "short int" => "i16",
        "unsigned short" | "unsigned short int" => "u16",
        "int" => "i32",
        "unsigned" | "unsigned int" => "u32",
        "long" | "long int" | "long long" | "long long int" => "i64",
        "unsigned long" | "unsigned long int" | "unsigned long long" | "unsigned long long int" => {
            "u64"
        }
        "float" => "f32",
        "double" => "f64",
        "size_t" | "ssize_t" => "usize",
        "bool" | "_Bool" => "bool",
        // Custom types pass through
        other => other,
    };

    // Apply pointers
    let mut result = base_type.to_string();
    for _ in 0..pointer_count {
        result = format!("*{result}");
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_translate_type() {
        assert_eq!(translate_type("int"), "i32");
        assert_eq!(translate_type("unsigned int"), "u32");
        assert_eq!(translate_type("char *"), "*i8");
        assert_eq!(translate_type("const char *"), "*i8");
        assert_eq!(translate_type("void"), "void");
        assert_eq!(translate_type("int **"), "**i32");
    }

    #[test]
    fn test_simple_function() {
        let input = "int add(int a, int b);";
        let result = translate(input);
        assert!(result.contains("fn add(a: i32, b: i32) -> i32;"));
    }

    #[test]
    fn test_void_function() {
        let input = "void hello(void);";
        let result = translate(input);
        assert!(result.contains("fn hello() -> void;"));
    }

    #[test]
    fn test_pointer_types() {
        let input = "char* strcpy(char* dest, const char* src);";
        let result = translate(input);
        assert!(result.contains("fn strcpy(dest: *i8, src: *i8) -> *i8;"));
    }

    #[test]
    fn test_variadic_function() {
        let input = "int printf(const char* format, ...);";
        let result = translate(input);
        assert!(result.contains("fn printf(format: *i8, ...) -> i32;"));
    }

    #[test]
    fn test_struct_translation() {
        let input = r"
        struct Point {
            int x;
            int y;
        };
        ";
        let result = translate(input);
        assert!(result.contains("struct Point {"));
        assert!(result.contains("x: i32,"));
        assert!(result.contains("y: i32,"));
    }
}
