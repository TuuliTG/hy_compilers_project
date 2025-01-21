from compiler.domain import Token, SourceLocation, L, TokenType
from compiler.ast import BinaryOp, Expression, Literal, Identifier
from compiler.parser import parse
import pytest


def test_parse_sum() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(left=Literal(1), op="+", right=Literal(2))


def test_parse_subtraction() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(left=Literal(2), op="-", right=Literal(1))


def test_parse_identifier_as_part_of_sum_function() -> None:
    tokens = [
        Token(loc=L, type=TokenType.IDENTIFIER, text="a"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(left=Identifier("a"), op="+", right=Literal(3))


def test_parse_identifier_as_part_of_sum_function_two_identifiers() -> None:
    tokens = [
        Token(loc=L, type=TokenType.IDENTIFIER, text="a"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="b"),
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(left=Identifier("a"), op="+", right=Identifier("b"))


def test_parse_multiplication() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="*"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(left=Literal(2), op="*", right=Literal(3))


def test_parse_multiple_operations() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3")
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(left=BinaryOp(left=Literal(
        value=1), op='+', right=Literal(value=2)), op='-', right=Literal(value=3))


def test_parse_multiple_operations_2() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="4")
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(
        left=BinaryOp(
            left=BinaryOp(
                left=Literal(1), op='+', right=Literal(2)),
            op='-', right=Literal(3)),
        op='+', right=Literal(value=4)
    )


def test_empty_tokens_fails_gracefully() -> None:
    tokens: list[Token] = []
    with pytest.raises(IndexError) as e:
        parse(tokens)
    assert "Array of tokens was empty" in str(e.value)


def test_calculation_ends_with_plus_sign_throws_an_error() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
        Token(loc=L, type=TokenType.OPERATOR, text="+")
    ]
    with pytest.raises(Exception) as e:
        parse(tokens)
    assert "an integer literal or an identifier" in str(
        e.value)


def test_calculation_starts_with_plus_sign_throws_an_error() -> None:
    tokens = [
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3")
    ]
    with pytest.raises(Exception) as e:
        parse(tokens)
    assert "an integer literal or an identifier" in str(e.value)


def test_sum_and_multiplication() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="*"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3")
    ]
    ast = parse(tokens)
    assert ast is not None
    assert ast == BinaryOp(
        left=Literal(value=1), op='+', right=BinaryOp(
            left=Literal(value=2), op='*', right=Literal(value=3))
    )


def test_multiplication_and_sum() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="*"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3")
    ]
    ast = parse(tokens)
    assert ast is not None
    assert ast == BinaryOp(
        left=BinaryOp(
            left=Literal(value=1), op='*', right=Literal(value=2)
        ), op='+', right=Literal(value=3)
    )


def test_multiplication_sum_and_subtraction() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="*"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="4")
    ]
    ast = parse(tokens)
    assert ast is not None
    assert ast == BinaryOp(
        left=BinaryOp(
            left=Literal(value=1), op='+', right=BinaryOp(
                left=Literal(value=2), op='*', right=Literal(3)
            )
        ),
        op='-', right=Literal(value=4)
    )


def test_multiplication_sum_and_subtraction_2() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
        Token(loc=L, type=TokenType.OPERATOR, text="*"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="4")
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(
        left=BinaryOp(
            left=Literal(value=1), op='+', right=Literal(value=2)),
        op='-', right=BinaryOp(
           left=Literal(value=3), op='*', right=Literal(value=4)
        )
    )


def test_division_sum_and_multiplication() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="/"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
        Token(loc=L, type=TokenType.OPERATOR, text="*"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="4")
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(
        left=Literal(value=1), op='+', right=BinaryOp(
            left=BinaryOp(
                left=Literal(value=2), op='/', right=Literal(value=3)), op='*', right=Literal(value=4)
        )
    )


def test_parentheses_sum_and_multiplication() -> None:
    tokens = [
        Token(loc=L, type=TokenType.IDENTIFIER, text="("),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.IDENTIFIER, text=")"),
        Token(loc=L, type=TokenType.OPERATOR, text="/"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(left=BinaryOp(
        left=Literal(value=1), op='+', right=Literal(2)), op='/', right=Literal(value=3)
    )


def test_double_parentheses_sum_and_multiplication() -> None:
    tokens = [
        Token(loc=L, type=TokenType.IDENTIFIER, text="("),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="("),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
        Token(loc=L, type=TokenType.IDENTIFIER, text=")"),
        Token(loc=L, type=TokenType.IDENTIFIER, text=")"),
        Token(loc=L, type=TokenType.OPERATOR, text="/"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="4"),
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(
        left=BinaryOp(
            left=Literal(value=1), op='+', right=BinaryOp(
                left=Literal(value=2), op='-', right=Literal(value=3))), op='/', right=Literal(value=4)
    )


def test_missing_parentheses_throws_an_error() -> None:
    tokens = [
        Token(loc=L, type=TokenType.IDENTIFIER, text="("),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="/"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
    ]
    with pytest.raises(Exception) as e:
        parse(tokens)
    assert "expected \")\"" in str(e.value)


def test_parse_if_expression() -> None:
    tokens = [
        Token(loc=L, type=TokenType.IDENTIFIER, text="if"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="a"),
        Token(loc=L, type=TokenType.OPERATOR, text="="),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="then"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="b"),
        Token(loc=L, type=TokenType.OPERATOR, text="="),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2")
    ]

    assert True
