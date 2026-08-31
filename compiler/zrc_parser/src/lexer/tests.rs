use zrc_diagnostics::{HelpKind, LabelKind, Severity};
use zrc_utils::{span::Spannable, spanned_test};

use crate::lexer::{
	self,
	token::{Token, TokenKind::*},
};

macro_rules! token {
	($kind:ident,$start:expr,$literal:expr,$end:expr) => {
		Token::from_kind_and_literal($kind, spanned_test!($start, $literal, $end))
	};
}

macro_rules! assert_tokens {
	($input:expr, $expected:expr) => {
		assert_eq!(
			lexer::ZircoLexer::new($input, "<test>").collect_all_tokens(),
			Ok($expected)
		);
	};
}

#[test]
fn punctuation() {
	let input = ":;,.()[]{}?";
	assert_tokens!(
		input,
		vec![
			token!(Colon, 0, ":", 1),
			token!(Semicolon, 1, ";", 2),
			token!(Comma, 2, ",", 3),
			token!(Dot, 3, ".", 4),
			token!(LeftParen, 4, "(", 5),
			token!(RightParen, 5, ")", 6),
			token!(LeftBracket, 6, "[", 7),
			token!(RightBracket, 7, "]", 8),
			token!(LeftBrace, 8, "{", 9),
			token!(RightBrace, 9, "}", 10),
			token!(QuestionMark, 10, "?", 11)
		]
	);
}

#[test]
fn single_char_operators() {
	let input = "=+-*/%&|^~><!";
	assert_tokens!(
		input,
		vec![
			token!(Assign, 0, "=", 1),
			token!(Plus, 1, "+", 2),
			token!(Minus, 2, "-", 3),
			token!(Star, 3, "*", 4),
			token!(Slash, 4, "/", 5),
			token!(Percent, 5, "%", 6),
			token!(BitwiseAnd, 6, "&", 7),
			token!(BitwiseOr, 7, "|", 8),
			token!(BitwiseXor, 8, "^", 9),
			token!(BitwiseNot, 9, "~", 10),
			token!(Greater, 10, ">", 11),
			token!(Less, 11, "<", 12),
			token!(LogicalNot, 12, "!", 13)
		]
	);
}

#[test]
fn double_char_operators() {
	let input = "++--==!=&&||>=<= += -= *= /= %= &= |= ^= :: -> <- => ...";
	assert_tokens!(
		input,
		vec![
			token!(PlusPlus, 0, "++", 2),
			token!(MinusMinus, 2, "--", 4),
			token!(EqEq, 4, "==", 6),
			token!(NotEq, 6, "!=", 8),
			token!(LogicalAnd, 8, "&&", 10),
			token!(LogicalOr, 10, "||", 12),
			token!(GreaterEq, 12, ">=", 14),
			token!(LessEq, 14, "<=", 16),
			token!(PlusAssign, 17, "+=", 19),
			token!(MinusAssign, 20, "-=", 22),
			token!(StarAssign, 23, "*=", 25),
			token!(SlashAssign, 26, "/=", 28),
			token!(PercentAssign, 29, "%=", 31),
			token!(BitwiseAndAssign, 32, "&=", 34),
			token!(BitwiseOrAssign, 35, "|=", 37),
			token!(BitwiseXorAssign, 38, "^=", 40),
			token!(ColonColon, 41, "::", 43),
			token!(SmallArrow, 44, "->", 46),
			token!(SmallArrowBack, 47, "<-", 49),
			token!(FatArrow, 50, "=>", 52),
			token!(Ellipsis, 53, "...", 56)
		]
	);
}

#[test]
fn keywords() {
	let input = concat!(
		"true false if else while do for break continue return let const fn as struct union",
		" enum match sizeof type switch default new unreachable packed"
	);
	assert_tokens!(
		input,
		vec![
			token!(True, 0, "true", 4),
			token!(False, 5, "false", 10),
			token!(If, 11, "if", 13),
			token!(Else, 14, "else", 18),
			token!(While, 19, "while", 24),
			token!(Do, 25, "do", 27),
			token!(For, 28, "for", 31),
			token!(Break, 32, "break", 37),
			token!(Continue, 38, "continue", 46),
			token!(Return, 47, "return", 53),
			token!(Let, 54, "let", 57),
			token!(Const, 58, "const", 63),
			token!(Fn, 64, "fn", 66),
			token!(As, 67, "as", 69),
			token!(Struct, 70, "struct", 76),
			token!(Union, 77, "union", 82),
			token!(Enum, 83, "enum", 87),
			token!(Match, 88, "match", 93),
			token!(SizeOf, 94, "sizeof", 100),
			token!(Type, 101, "type", 105),
			token!(Switch, 106, "switch", 112),
			token!(Default, 113, "default", 120),
			token!(New, 121, "new", 124),
			token!(Unreachable, 125, "unreachable", 136),
			token!(Packed, 137, "packed", 143)
		]
	);
}

#[test]
fn nonsensical_symbol() {
	let test = "#";
	let diag = lexer::ZircoLexer::new(test, "<test>")
		.collect_all_tokens()
		.expect_err("lexing should fail");

	assert_eq!(diag.severity, Severity::Error);

	assert_eq!(diag.labels.len(), 1);
	let label = &diag.labels[0];
	assert_eq!(
		label.kind,
		spanned_test!(0, LabelKind::UnknownToken("#".to_string()), 1)
	);
}

#[test]
fn simple_identifiers() {
	let input = "foo bar baz qux";
	assert_tokens!(
		input,
		vec![
			token!(Identifier, 0, "foo", 3),
			token!(Identifier, 4, "bar", 7),
			token!(Identifier, 8, "baz", 11),
			token!(Identifier, 12, "qux", 15)
		]
	);
}

#[test]
fn complex_identifiers() {
	let input = "foo_bar baz123 qux_456";
	assert_tokens!(
		input,
		vec![
			token!(Identifier, 0, "foo_bar", 7),
			token!(Identifier, 8, "baz123", 14),
			token!(Identifier, 15, "qux_456", 22),
		]
	);
}

#[test]
fn suspiciously_keywordlike_identifiers() {
	let input = "iff trueish lett";
	assert_tokens!(
		input,
		vec![
			token!(Identifier, 0, "iff", 3),
			token!(Identifier, 4, "trueish", 11),
			token!(Identifier, 12, "lett", 16)
		]
	);
}

#[test]
fn javascript_user_detection() {
	let input = "===";
	let diag = lexer::ZircoLexer::new(input, "<test>")
		.collect_all_tokens()
		.expect_err("lexing should fail");

	assert_eq!(diag.severity, Severity::Error);

	assert_eq!(diag.labels.len(), 1);
	let label = &diag.labels[0];
	assert_eq!(
		label.kind,
		spanned_test!(0, LabelKind::JavascriptUserDetected, 3)
	);

	assert_eq!(diag.helps.len(), 1);
	let help = &diag.helps[0];
	assert_eq!(help, &HelpKind::JavascriptUserDetected("=="));
}

#[test]
fn whitespace_skipping() {
	let input = " \n\tfoo\r\nbaz  qux";
	assert_tokens!(
		input,
		vec![
			token!(Identifier, 3, "foo", 6),
			token!(Identifier, 8, "baz", 11),
			token!(Identifier, 13, "qux", 16)
		]
	);
}

#[test]
fn number_literals() {
	let input =
		"0 1 12345 123_456.789 0xdeadbeef 0xdeAd_bEef 0b101010 0b1010_1010 1.foo 1..2 400i32";

	assert_tokens!(
		input,
		vec![
			token!(NumberLiteral, 0, "0", 1),
			token!(NumberLiteral, 2, "1", 3),
			token!(NumberLiteral, 4, "12345", 9),
			token!(NumberLiteral, 10, "123_456.789", 21),
			token!(NumberLiteral, 22, "0xdeadbeef", 32),
			token!(NumberLiteral, 33, "0xdeAd_bEef", 44),
			token!(NumberLiteral, 45, "0b101010", 53),
			token!(NumberLiteral, 54, "0b1010_1010", 65),
			token!(NumberLiteral, 66, "1", 67),
			token!(Dot, 67, ".", 68),
			token!(Identifier, 68, "foo", 71),
			token!(NumberLiteral, 72, "1", 73),
			token!(Dot, 73, ".", 74),
			token!(Dot, 74, ".", 75),
			token!(NumberLiteral, 75, "2", 76),
			token!(NumberLiteral, 77, "400", 80),
			token!(Identifier, 80, "i32", 83),
		]
	);
}

#[test]
fn string_literals() {
	let input =
		r#""hello" "world" "foo\nbar" "baz\tqux" "escaped quote: \"" "\0" "\xFF" "a\u{1234}""#;
	assert_tokens!(
		input,
		vec![
			token!(StringLiteral, 0, r#""hello""#, 7),
			token!(StringLiteral, 8, r#""world""#, 15),
			token!(StringLiteral, 16, r#""foo\nbar""#, 26),
			token!(StringLiteral, 27, r#""baz\tqux""#, 37),
			token!(StringLiteral, 38, r#""escaped quote: \"""#, 57),
			token!(StringLiteral, 58, r#""\0""#, 62),
			token!(StringLiteral, 63, r#""\xFF""#, 69),
			token!(StringLiteral, 70, r#""a\u{1234}""#, 81)
		]
	);
}

#[test]
fn invalid_string_literal() {
	let input = r#""unterminated"#;
	let diag = lexer::ZircoLexer::new(input, "<test>")
		.collect_all_tokens()
		.expect_err("lexing should fail");

	assert_eq!(diag.severity, Severity::Error);

	assert_eq!(diag.labels.len(), 1);
	let label = &diag.labels[0];
	assert_eq!(
		label.kind,
		spanned_test!(0, LabelKind::UnterminatedStringLiteral, 13)
	);
}

#[test]
fn char_literals() {
	let input = r"'a' '\n' '\t' '\'' '\0' '\xFF' '\u{1234}'";

	assert_tokens!(
		input,
		vec![
			token!(CharLiteral, 0, r"'a'", 3),
			token!(CharLiteral, 4, r"'\n'", 8),
			token!(CharLiteral, 9, r"'\t'", 13),
			token!(CharLiteral, 14, r"'\''", 18),
			token!(CharLiteral, 19, r"'\0'", 23),
			token!(CharLiteral, 24, r"'\xFF'", 30),
			token!(CharLiteral, 31, r"'\u{1234}'", 41),
		]
	);
}
#[test]
fn invalid_char_literal() {
	let input = r"'u";
	let diag = lexer::ZircoLexer::new(input, "<test>")
		.collect_all_tokens()
		.expect_err("lexing should fail");

	assert_eq!(diag.severity, Severity::Error);

	assert_eq!(diag.labels.len(), 1);
	let label = &diag.labels[0];
	assert_eq!(
		label.kind,
		spanned_test!(0, LabelKind::UnterminatedStringLiteral, 2)
	);
}

#[test]
fn simple_comments() {
	let input = "foo /* test */ bar // This is a comment\nbaz";
	assert_tokens!(
		input,
		vec![
			token!(Identifier, 0, "foo", 3),
			token!(Identifier, 15, "bar", 18),
			token!(Identifier, 40, "baz", 43)
		]
	);
}

#[test]
fn nested_block_comments() {
	let input = "foo /* This is a /* nested */ comment */ bar";
	assert_tokens!(
		input,
		vec![
			token!(Identifier, 0, "foo", 3),
			token!(Identifier, 41, "bar", 44)
		]
	);
}
