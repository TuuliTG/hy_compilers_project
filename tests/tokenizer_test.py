from compiler.tokenizer import tokenize
from compiler.domain import Token, SourceLocation, L, TokenType
import pytest


def test_tokenizer_basics() -> None:
    assert tokenize("if  3\nwhile") == [
        Token(loc=SourceLocation("dummy", 1, 0),
              type=TokenType.IDENTIFIER, text="if"),
        Token(loc=SourceLocation("dummy", 1, 4),
              type=TokenType.INT_LITERAL, text="3"),
        Token(loc=SourceLocation("dummy", 2, 0),
              type=TokenType.IDENTIFIER, text="while")
    ]


def test_longer_integers() -> None:
    assert tokenize("hello 123 1001 1") == [
        Token(loc=SourceLocation("dummy", 1, 0),
              type=TokenType.IDENTIFIER, text="hello"),
        Token(loc=SourceLocation("dummy", 1, 6),
              type=TokenType.INT_LITERAL, text="123"),
        Token(loc=SourceLocation("dummy", 1, 10),
              type=TokenType.INT_LITERAL, text="1001"),
        Token(loc=SourceLocation("dummy", 1, 15),
              type=TokenType.INT_LITERAL, text="1")
    ]


def test_negative_integers() -> None:
    assert tokenize("-123 -2 3") == [
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="123"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
    ]


def test_identifier_cannot_start_with_integer() -> None:
    with pytest.raises(ValueError) as e:
        tokenize("1hello cat")
    assert "Invalid character or token" in str(e.value)


def test_identifier_can_start_with_underscore() -> None:
    assert tokenize("_hello cat") == [
        Token(loc=L, type=TokenType.IDENTIFIER, text="_hello"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="cat")
    ]


def test_numbers_are_separated_by_operators() -> None:
    assert tokenize("1+2*3 = 4 5<=6/7==8 >= 9 - 10") == [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="*"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
        Token(loc=L, type=TokenType.OPERATOR, text="="),
        Token(loc=L, type=TokenType.INT_LITERAL, text="4"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="5"),
        Token(loc=L, type=TokenType.OPERATOR, text="<="),
        Token(loc=L, type=TokenType.INT_LITERAL, text="6"),
        Token(loc=L, type=TokenType.OPERATOR, text="/"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="7"),
        Token(loc=L, type=TokenType.OPERATOR, text="=="),
        Token(loc=L, type=TokenType.INT_LITERAL, text="8"),
        Token(loc=L, type=TokenType.OPERATOR, text=">="),
        Token(loc=L, type=TokenType.INT_LITERAL, text="9"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="10"),
    ]


def test_identifier_may_start_with_capital_letter() -> None:
    assert tokenize("Test variable") == [
        Token(loc=L, type=TokenType.IDENTIFIER, text="Test"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="variable")
    ]


def test_source_location_is_defined_correctly() -> None:
    assert tokenize("hello 123") == [
        Token(loc=SourceLocation("dummy", 1, 0),
              type=TokenType.IDENTIFIER, text="hello"),
        Token(loc=SourceLocation("dummy", 1, 6),
              type=TokenType.INT_LITERAL, text="123"),
    ]


def test_tabs_are_handled_correctly() -> None:
    assert tokenize("hello \n\t a = b") == [
        Token(loc=SourceLocation("dummy", 1, 0),
              type=TokenType.IDENTIFIER, text="hello"),
        Token(loc=SourceLocation("dummy", 2, 2),
              type=TokenType.IDENTIFIER, text="a"),
        Token(loc=SourceLocation("dummy", 2, 4),
              type=TokenType.OPERATOR, text="="),
        Token(loc=SourceLocation("dummy", 2, 6),
              type=TokenType.IDENTIFIER, text="b"),
    ]


def test_new_lines_are_handled_correctly() -> None:
    assert tokenize("a\nb\n1\n23") == [
        Token(loc=SourceLocation("dummy", 1, 0),
              type=TokenType.IDENTIFIER, text="a"),
        Token(loc=SourceLocation("dummy", 2, 0),
              type=TokenType.IDENTIFIER, text="b"),
        Token(loc=SourceLocation("dummy", 3, 0),
              type=TokenType.INT_LITERAL, text="1"),
        Token(loc=SourceLocation("dummy", 4, 0),
              type=TokenType.INT_LITERAL, text="23"),
    ]


def test_punctuation() -> None:
    assert tokenize("a(b) {12} 1,2 ;") == [
        Token(loc=L, type=TokenType.IDENTIFIER, text="a"),
        Token(loc=L, type=TokenType.PUNCTUATION, text="("),
        Token(loc=L, type=TokenType.IDENTIFIER, text="b"),
        Token(loc=L, type=TokenType.PUNCTUATION, text=")"),
        Token(loc=L, type=TokenType.PUNCTUATION, text="{"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="12"),
        Token(loc=L, type=TokenType.PUNCTUATION, text="}"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.PUNCTUATION, text=","),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.PUNCTUATION, text=";"),

    ]


def test_one_line_comments() -> None:
    assert tokenize("// comment 1\n a = 1 \n#comment 2\nb=2") == [
        Token(loc=L, type=TokenType.IDENTIFIER, text="a"),
        Token(loc=L, type=TokenType.OPERATOR, text="="),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="b"),
        Token(loc=L, type=TokenType.OPERATOR, text="="),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2")
    ]


def test_one_line_comments_on_same_line_with_code() -> None:
    assert tokenize("// comment 1\n a = 1 # comment 2\nb=2") == [
        Token(loc=L, type=TokenType.IDENTIFIER, text="a"),
        Token(loc=L, type=TokenType.OPERATOR, text="="),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="b"),
        Token(loc=L, type=TokenType.OPERATOR, text="="),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2")
    ]

# def test_identifier_cant_have_punctuation_marks() -> None:
#    assert tokenize("hello. cat! .a ")


def test_handle_multiline_comment() -> None:
    assert tokenize(
        "/* here is a multiline comment \n second line \n third line */\na = 1") == [
            Token(loc=SourceLocation(file="dummy", row=4, column=0),
                  type=TokenType.IDENTIFIER, text="a"),
            Token(loc=L, type=TokenType.OPERATOR, text="="),
            Token(loc=L, type=TokenType.INT_LITERAL, text="1")
    ]


def test_handle_multiline_comment_not_ending_correctly() -> None:
    with pytest.raises(ValueError) as e:
        tokenize("/* here is a faulty multiline comment. ")
    assert "The multiline comment is not closed" in str(e.value)


def test_comment_cannot_start_with_end_tag() -> None:
    with pytest.raises(ValueError) as e:
        tokenize("*/ here is a faulty multiline comment. ")
    assert "Invalid character or token" in str(e.value)
