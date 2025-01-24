from compiler.domain import Token, SourceLocation, L, TokenType
from compiler.ast import BinaryOp, Expression, FunctionExpression, IfExpression, Literal, Identifier, UnaryExpression
from compiler.parser import parse
import pytest
from compiler.tokenizer import tokenize


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
        value=1), op='+', right=Literal(value=2)), op='-', right=Literal(value=3)
    )


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
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="then"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="b"),
        Token(loc=L, type=TokenType.OPERATOR, text="*"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2")
    ]
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=BinaryOp(
            left=Identifier(name='a'), op='+', right=Literal(value=1)), then_branch=BinaryOp(
            left=Identifier(name='b'), op='*', right=Literal(value=2)), else_branch=None
    )


def test_parse_if_expression_with_else_branch() -> None:
    tokens = [
        Token(loc=L, type=TokenType.IDENTIFIER, text="if"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="a"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="then"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="b"),
        Token(loc=L, type=TokenType.OPERATOR, text="*"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="else"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="c"),
        Token(loc=L, type=TokenType.OPERATOR, text="/"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3")
    ]
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=BinaryOp(
            left=Identifier(name='a'), op='+', right=Literal(value=1)),
        then_branch=BinaryOp(
            left=Identifier(name='b'), op='*', right=Literal(value=2)), else_branch=BinaryOp(
            left=Identifier(name="c"), op="/", right=Literal(3)
        )
    )


def test_if_statement_as_part_of_expression() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="if"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="then"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="else"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3")
    ]
    ast = parse(tokens)
    assert ast == BinaryOp(
        left=Literal(value=1), op='+', right=IfExpression(
            condition_branch=Literal(value=2),
            then_branch=Literal(value=2), else_branch=Literal(value=3))
    )


def test_nested_if_expressions() -> None:
    tokens = tokenize("if a then if b then b + 1 else c + 2")
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=Identifier(name='a'),
        then_branch=IfExpression(
            condition_branch=Identifier(name='b'),
            then_branch=BinaryOp(left=Identifier(name='b'),
                                 op='+', right=Literal(value=1)),
            else_branch=BinaryOp(left=Identifier(name='c'), op='+', right=Literal(value=2))),
        else_branch=None)


def test_function_call() -> None:
    tokens = tokenize("f(a,b,c)")
    ast = parse(tokens)

    assert ast == FunctionExpression(
        function_name="f", args=[
            Identifier(name="a"), Identifier(name="b"), Identifier(name="c")]
    )


def test_function_call_with_binary_operation() -> None:
    tokens = tokenize("f(a,b+c)")
    ast = parse(tokens)

    assert ast == FunctionExpression(
        function_name="f", args=[
            Identifier(name="a"), BinaryOp(
                left=Identifier(name="b"), op='+', right=Identifier(name='c')
            )
        ]
    )


def test_function_call_in_the_middle_of_other_expression() -> None:
    tokens = tokenize("a + b(x,y) * 2")
    ast = parse(tokens)

    assert ast == BinaryOp(
        left=Identifier(name='a'), op='+', right=BinaryOp(
            left=FunctionExpression(
                function_name='b', args=[Identifier(name='x'), Identifier(name='y')]), op='*', right=Literal(value=2)
        )
    )


def test_remainder_operator() -> None:
    tokens = tokenize("a%b")
    ast = parse(tokens)
    assert ast == BinaryOp(left=Identifier(name="a"),
                           op="%", right=Identifier(name="b"))


def test_remainder_operator_precedence() -> None:
    tokens = tokenize("1+a%b")
    ast = parse(tokens)
    assert ast == BinaryOp(
        left=Literal(1), op="+", right=BinaryOp(
            left=Identifier(name="a"), op="%",
            right=Identifier(name="b")
        )
    )


def test_if_expressions_2() -> None:
    tokens = tokenize("if a==b then c")
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=BinaryOp(
            left=Identifier(name='a'), op='==', right=Identifier('b')),
        then_branch=Identifier(name="c"),
        else_branch=None)


def test_if_expressions_3() -> None:
    tokens = tokenize("if a<=b then c")
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=BinaryOp(
            left=Identifier(name='a'), op='<=', right=Identifier('b')),
        then_branch=Identifier(name="c"),
        else_branch=None)


def test_if_expressions_4() -> None:
    tokens = tokenize("if a>=b then c")
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=BinaryOp(
            left=Identifier(name='a'), op='>=', right=Identifier('b')),
        then_branch=Identifier(name="c"),
        else_branch=None)


def test_if_expressions_5() -> None:
    tokens = tokenize("if a!=b then c")
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=BinaryOp(
            left=Identifier(name='a'), op='!=', right=Identifier('b')),
        then_branch=Identifier(name="c"),
        else_branch=None)


def test_if_expressions_6() -> None:
    tokens = tokenize("if a>b then c")
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=BinaryOp(
            left=Identifier(name='a'), op='>', right=Identifier('b')),
        then_branch=Identifier(name="c"),
        else_branch=None)


def test_and_expression() -> None:
    tokens = tokenize("if a>b and b == 2 then c")
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=BinaryOp(
            left=BinaryOp(left=Identifier(name='a'), op='>',
                          right=Identifier(name='b')),
            op='and', right=BinaryOp(
                left=Identifier(name='b'), op='==', right=Literal(value=2))),
        then_branch=Identifier(name='c'), else_branch=None)


def test_and_or_expressions() -> None:
    tokens = tokenize("if a>b and b == 2 or b == 3 then c")
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=BinaryOp(
            left=BinaryOp(
                left=BinaryOp(
                    left=Identifier(name='a'), op='>', right=Identifier(name='b')), op='and', right=BinaryOp(
                    left=Identifier(name='b'), op='==', right=Literal(value=2))), op='or', right=BinaryOp(
                left=Identifier(name='b'), op='==', right=Literal(value=3))),
        then_branch=Identifier(name='c'), else_branch=None
    )


def test_assignment_operator() -> None:
    tokens = tokenize("a=b*5")
    ast = parse(tokens)
    assert ast == BinaryOp(
        left=Identifier(name="a"), op="=", right=BinaryOp(
            left=Identifier(name="b"), op="*", right=Literal(5)
        )
    )


def test_chaining_of_not() -> None:
    tokens = tokenize("if not not (a==b) then a=b")
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=UnaryExpression(
            operator="not", operand=UnaryExpression(
                operator="not", operand=BinaryOp(
                    left=Identifier(name="a"), op="==", right=Identifier(name="b")
                )
            )
        ),
        then_branch=BinaryOp(
            left=Identifier(
                name="a"
            ), op="=", right=Identifier(name="b")
        ),
        else_branch=None
    )


def test_different_operand_levels_mixed() -> None:
    tokens = tokenize(
        "if a + b == 5 * 3 then c = f(a,b) else b = 5 / 8 + 3 - 5")
    ast = parse(tokens)
    assert ast == IfExpression(
        condition_branch=BinaryOp(
            left=BinaryOp(
                left=Identifier(name='a'), op='+', right=Identifier(name='b')), op='==', right=BinaryOp(
                left=Literal(value=5), op='*', right=Literal(value=3))),
        then_branch=BinaryOp(
            left=Identifier(name='c'), op='=', right=FunctionExpression(
                function_name='f', args=[Identifier(name='a'), Identifier(name='b')])),
        else_branch=BinaryOp(
            left=Identifier(name='b'), op='=', right=BinaryOp(
                left=BinaryOp(
                    left=BinaryOp(left=Literal(value=5), op='/', right=Literal(value=8)), op='+', right=Literal(value=3)
                ), op='-', right=Literal(value=5)
            )
        )
    )
