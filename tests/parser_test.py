from compiler.domain import Token, SourceLocation, L, TokenType
from compiler.ast import BinaryOp, BlockExpression, BooleanLiteral, Expression, FunctionExpression, IfExpression, Literal, Identifier, UnaryExpression, VariableDeclaration, WhileLoop
from compiler.parser import parse, parse
import pytest
from compiler.tokenizer import tokenize


def test_parse_sum() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
    ]
    ast = parse(tokens)
    assert ast == BlockExpression(location=L,
                                  expressions=[
                                      BinaryOp(location=L, left=Literal(
                                          location=L, value=1), op="+", right=Literal(location=L, value=2))
                                  ])


def test_parse_subtraction() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
    ]
    ast = parse(tokens)
    assert ast == BlockExpression(location=L,
                                  expressions=[BinaryOp(
                                      location=L, left=Literal(location=L, value=2), op="-", right=Literal(location=L, value=1))])


def test_parse_identifier_as_part_of_sum_function() -> None:
    tokens = [
        Token(loc=L, type=TokenType.IDENTIFIER, text="a"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
    ]
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        BinaryOp(location=L, left=Identifier(
            location=L, name="a"), op="+", right=Literal(location=L, value=3))
    ]
    )


def test_parse_identifier_as_part_of_sum_function_two_identifiers() -> None:
    tokens = [
        Token(loc=L, type=TokenType.IDENTIFIER, text="a"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.IDENTIFIER, text="b"),
    ]
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[BinaryOp(location=L, left=Identifier(location=L, name="a"), op="+", right=Identifier(location=L, name="b"))
                                                           ])


def test_parse_multiplication() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="*"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3"),
    ]
    ast = parse(tokens)
    assert ast == BlockExpression(location=L,
                                  expressions=[BinaryOp(location=L, left=Literal(location=L, value=2), op="*", right=Literal(location=L, value=3))])


def test_parse_multiple_operations() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="-"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3")
    ]
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[BinaryOp(location=L, left=BinaryOp(location=L, left=Literal(
        location=L, value=1), op='+', right=Literal(location=L, value=2)), op='-', right=Literal(location=L, value=3)
    )])


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
    assert ast == BlockExpression(location=L, expressions=[BinaryOp(
        location=L,
        left=BinaryOp(
            location=L,
            left=BinaryOp(
                location=L,
                left=Literal(location=L, value=1), op='+', right=Literal(location=L, value=2)),
            op='-', right=Literal(location=L, value=3)),
        op='+', right=Literal(location=L, value=4)
    )])


def test_empty_tokens_fails_gracefully() -> None:
    tokens: list[Token] = []
    ast = parse(tokens)
    assert ast == None


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
    assert ast == BlockExpression(location=L, expressions=[BinaryOp(
        location=L, left=Literal(location=L, value=1), op='+', right=BinaryOp(
            location=L, left=Literal(location=L, value=2), op='*', right=Literal(location=L, value=3))
    )])


def test_multiplication_and_sum() -> None:
    tokens = [
        Token(loc=L, type=TokenType.INT_LITERAL, text="1"),
        Token(loc=L, type=TokenType.OPERATOR, text="*"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="2"),
        Token(loc=L, type=TokenType.OPERATOR, text="+"),
        Token(loc=L, type=TokenType.INT_LITERAL, text="3")
    ]
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        BinaryOp(
            location=L,
            left=BinaryOp(
                location=L,
                left=Literal(location=L, value=1), op='*', right=Literal(location=L, value=2)
            ), op='+', right=Literal(location=L, value=3)
        )
    ]
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
    assert ast == BlockExpression(location=L, expressions=[
        BinaryOp(
            location=L,
            left=BinaryOp(
                location=L,
                left=Literal(location=L, value=1), op='+', right=BinaryOp(
                    location=L, left=Literal(location=L, value=2), op='*', right=Literal(location=L, value=3)
                )
            ),
            op='-', right=Literal(location=L, value=4)
        )])


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
    assert ast == BlockExpression(location=L, expressions=[
        BinaryOp(
            location=L,
            left=BinaryOp(
                location=L,
                left=Literal(location=L, value=1), op='+', right=Literal(location=L, value=2)),
            op='-', right=BinaryOp(
                location=L,
                left=Literal(location=L, value=3), op='*', right=Literal(location=L, value=4)
            )
        )])


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
    assert ast == BlockExpression(location=L, expressions=[BinaryOp(
        location=L,
        left=Literal(location=L, value=1), op='+', right=BinaryOp(
            location=L,
            left=BinaryOp(
                location=L,
                left=Literal(location=L, value=2), op='/', right=Literal(location=L, value=3)), op='*', right=Literal(location=L, value=4)
        )
    )])


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
    assert ast == BlockExpression(location=L, expressions=[
        BinaryOp(location=L, left=BinaryOp(
            location=L,
            left=Literal(location=L, value=1), op='+',
            right=Literal(location=L, value=2)), op='/', right=Literal(location=L, value=3)
        )])


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
    assert ast == BlockExpression(location=L, expressions=[
        BinaryOp(
            location=L,
            left=BinaryOp(
                location=L,
                left=Literal(location=L, value=1), op='+', right=BinaryOp(
                    location=L,
                    left=Literal(location=L, value=2),
                    op='-', right=Literal(location=L, value=3))), op='/',
            right=Literal(location=L, value=4)
        )])


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
    assert ast == BlockExpression(location=L, expressions=[
        IfExpression(
            location=L,
            condition_branch=BinaryOp(
                location=L,
                left=Identifier(location=L, name='a'), op='+', right=Literal(location=L, value=1)), then_branch=BinaryOp(
                location=L,
                left=Identifier(location=L, name='b'), op='*', right=Literal(location=L, value=2)), else_branch=None
        )])


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
    assert ast == BlockExpression(
        location=L,
        expressions=[
            IfExpression(
                location=L,
                condition_branch=BinaryOp(
                    location=L,
                    left=Identifier(location=L, name='a'), op='+', right=Literal(location=L, value=1)),
                then_branch=BinaryOp(
                    location=L,
                    left=Identifier(location=L, name='b'), op='*', right=Literal(location=L, value=2)),
                else_branch=BinaryOp(
                    location=L, left=Identifier(location=L, name="c"), op="/", right=Literal(location=L, value=3)
                )
            )
        ])


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
    assert ast == BlockExpression(location=L, expressions=[BinaryOp(
        location=L,
        left=Literal(location=L, value=1), op='+', right=IfExpression(
            location=L,
            condition_branch=Literal(location=L, value=2),
            then_branch=Literal(location=L, value=2), else_branch=Literal(location=L, value=3))
    )
    ])


def test_nested_if_expressions() -> None:
    tokens = tokenize("if a then if b then b + 1 else c + 2")
    ast = parse(tokens)
    assert ast == BlockExpression(location=SourceLocation(file='dummy', row=1, column=0),
                                  expressions=[
        IfExpression(location=SourceLocation(file='dummy', row=1, column=0),
                     condition_branch=Identifier(
            location=SourceLocation(file='dummy', row=1, column=3), name='a'),
            then_branch=IfExpression(location=SourceLocation(file='dummy', row=1, column=10),
                                     condition_branch=Identifier(
                location=SourceLocation(file='dummy', row=1, column=13), name='b'),
            then_branch=BinaryOp(location=SourceLocation(file='dummy', row=1, column=20), left=Identifier(
                location=SourceLocation(file='dummy', row=1, column=20), name='b'), op='+', right=Literal(
                location=SourceLocation(file='dummy', row=1, column=24), value=1)),
            else_branch=BinaryOp(
                location=SourceLocation(
                    file='dummy', row=1, column=31),
                left=Identifier(location=SourceLocation(
                    file='dummy', row=1, column=31), name='c'),
                op='+',
                right=Literal(location=SourceLocation(file='dummy', row=1, column=35), value=2))),
            else_branch=None
        )
    ])


def test_function_call() -> None:
    tokens = tokenize("f(a,b,c)")
    ast = parse(tokens)

    assert ast == BlockExpression(location=SourceLocation(file='dummy', row=1, column=0), expressions=[FunctionExpression(
        location=SourceLocation(file='dummy', row=1, column=0),
        function_name="f", args=[
            Identifier(location=SourceLocation(
                file='dummy', row=1, column=2), name="a"),
            Identifier(location=SourceLocation(
                file='dummy', row=1, column=4), name="b"),
            Identifier(location=SourceLocation(file='dummy', row=1, column=6), name="c")]
    )])


def test_function_call_with_binary_operation() -> None:
    tokens = tokenize("f(a,b+c)")
    ast = parse(tokens)

    assert ast == BlockExpression(location=SourceLocation(file='dummy', row=1, column=0), expressions=[
        FunctionExpression(
            location=SourceLocation(file='dummy', row=1, column=0),
            function_name="f", args=[
                Identifier(location=SourceLocation(
                    file='dummy', row=1, column=2), name="a"),
                BinaryOp(
                    location=SourceLocation(file='dummy', row=1, column=4),
                    left=Identifier(location=SourceLocation(
                        file='dummy', row=1, column=4), name="b"),
                    op='+',
                    right=Identifier(location=SourceLocation(
                        file='dummy', row=1, column=6), name='c')
                )
            ]
        )])


def test_function_call_in_the_middle_of_other_expression() -> None:
    tokens = tokenize("a + b(x,y) * 2")
    ast = parse(tokens)

    assert ast == BlockExpression(location=SourceLocation(file='dummy', row=1, column=0),
                                  expressions=[
        BinaryOp(
            location=SourceLocation(file='dummy', row=1, column=0),
            left=Identifier(location=SourceLocation(
                file='dummy', row=1, column=0), name='a'),
            op='+',
            right=BinaryOp(
                location=SourceLocation(file='dummy', row=1, column=4),
                left=FunctionExpression(
                    location=SourceLocation(file='dummy', row=1, column=4),
                    function_name='b',
                    args=[
                        Identifier(location=SourceLocation(
                            file='dummy', row=1, column=6), name='x'),
                        Identifier(location=SourceLocation(file='dummy', row=1, column=8), name='y')]),
                op='*',
                right=Literal(location=SourceLocation(
                    file='dummy', row=1, column=13), value=2)
            )
        )
    ])


def test_remainder_operator() -> None:
    tokens = tokenize("a%b")
    ast = parse(tokens)
    assert ast == BlockExpression(
        location=SourceLocation(file='dummy', row=1, column=0),
        expressions=[BinaryOp(
            location=SourceLocation(file='dummy', row=1, column=0),
            left=Identifier(
                location=SourceLocation(file='dummy', row=1, column=0), name="a"),
            op="%",
            right=Identifier(location=SourceLocation(file='dummy', row=1, column=2), name="b"))
        ])


def test_remainder_operator_precedence() -> None:
    tokens = tokenize("1+a%b")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[BinaryOp(
        location=L, left=Literal(location=L, value=1), op="+", right=BinaryOp(
            location=L, left=Identifier(location=L, name="a"), op="%",
            right=Identifier(location=L, name="b")
        )
    )])


def test_if_expressions_2() -> None:
    tokens = tokenize("if a==b then c")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[IfExpression(
        location=L, condition_branch=BinaryOp(
            location=L, left=Identifier(location=L, name='a'), op='==', right=Identifier(location=L, name='b')),
        then_branch=Identifier(location=L, name="c"),
        else_branch=None)
    ])


def test_if_expressions_3() -> None:
    tokens = tokenize("if a<=b then c")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[IfExpression(
        location=L, condition_branch=BinaryOp(
            location=L, left=Identifier(location=L, name='a'), op='<=', right=Identifier(location=L, name='b')),
        then_branch=Identifier(location=L, name="c"),
        else_branch=None)
    ])


def test_if_expressions_4() -> None:
    tokens = tokenize("if a>=b then c")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[IfExpression(
        location=L, condition_branch=BinaryOp(
            location=L, left=Identifier(location=L, name='a'), op='>=', right=Identifier(location=L, name='b')),
        then_branch=Identifier(location=L, name="c"),
        else_branch=None)
    ])


def test_if_expressions_5() -> None:
    tokens = tokenize("if a!=b then c")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[IfExpression(
        location=L,
        condition_branch=BinaryOp(
            location=L, left=Identifier(location=L, name='a'), op='!=', right=Identifier(location=L, name='b')),
        then_branch=Identifier(location=L, name="c"),
        else_branch=None)
    ])


def test_if_expressions_6() -> None:
    tokens = tokenize("if a>b then c")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[IfExpression(
        location=L, condition_branch=BinaryOp(
            location=L, left=Identifier(location=L, name='a'), op='>', right=Identifier(location=L, name='b')),
        then_branch=Identifier(location=L, name="c"),
        else_branch=None)
    ])


def test_and_expression() -> None:
    tokens = tokenize("if a>b and b == 2 then c")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        IfExpression(
            location=L,
            condition_branch=BinaryOp(
                location=L, left=BinaryOp(location=L, left=Identifier(location=L, name='a'), op='>',
                                          right=Identifier(location=L, name='b')),
                op='and', right=BinaryOp(
                    location=L, left=Identifier(location=L, name='b'), op='==', right=Literal(location=L, value=2))),
            then_branch=Identifier(location=L, name='c'), else_branch=None)
    ])


def test_and_or_expressions() -> None:
    tokens = tokenize("if a>b and b == 2 or b == 3 then c")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        IfExpression(
            location=L, condition_branch=BinaryOp(
                location=L, left=BinaryOp(
                    location=L, left=BinaryOp(
                        location=L, left=Identifier(location=L, name='a'),
                        op='>',
                        right=Identifier(location=L, name='b')),
                    op='and',
                    right=BinaryOp(
                        location=L, left=Identifier(location=L, name='b'),
                        op='==',
                        right=Literal(location=L, value=2))),
                op='or',
                right=BinaryOp(
                    location=L,
                    left=Identifier(location=L, name='b'),
                    op='==',
                    right=Literal(location=L, value=3))),
            then_branch=Identifier(location=L, name='c'), else_branch=None
        )])


def test_assignment_operator() -> None:
    tokens = tokenize("a=b*5")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[BinaryOp(
        location=L, left=Identifier(location=L, name="a"), op="=", right=BinaryOp(
            location=L, left=Identifier(location=L, name="b"), op="*", right=Literal(location=L, value=5)
        )
    )])


def test_chaining_of_not() -> None:
    tokens = tokenize("if not not (a==b) then a=b")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        IfExpression(
            location=L,
            condition_branch=UnaryExpression(
                location=L,
                operator="not", operand=UnaryExpression(
                    location=L,
                    operator="not", operand=BinaryOp(
                        location=L, left=Identifier(location=L, name="a"), op="==", right=Identifier(location=L, name="b")
                    )
                )
            ),
            then_branch=BinaryOp(
                location=L,
                left=Identifier(
                    location=L, name="a"
                ), op="=", right=Identifier(location=L, name="b")
            ),
            else_branch=None
        )])


def test_different_operand_levels_mixed() -> None:
    tokens = tokenize(
        "if a + b == 5 * 3 then c = f(a,b) else b = 5 / 8 + 3 - 5")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        IfExpression(
            location=L, condition_branch=BinaryOp(
                location=L,
                left=BinaryOp(
                    location=L,
                    left=Identifier(location=L, name='a'),
                    op='+',
                    right=Identifier(location=L, name='b')),
                op='==',
                right=BinaryOp(
                    location=L,
                    left=Literal(location=L, value=5),
                    op='*',
                    right=Literal(location=L, value=3))),
            then_branch=BinaryOp(
                location=L,
                left=Identifier(location=L, name='c'),
                op='=',
                right=FunctionExpression(
                    location=L, function_name='f', args=[
                        Identifier(location=L, name='a'), Identifier(
                            location=L, name='b')
                    ])),
            else_branch=BinaryOp(
                location=L,
                left=Identifier(location=L, name='b'),
                op='=',
                right=BinaryOp(
                    location=L,
                    left=BinaryOp(
                        location=L, left=BinaryOp(
                            location=L,
                            left=Literal(location=L, value=5),
                            op='/', right=Literal(
                                location=L, value=8)),
                        op='+',
                        right=Literal(location=L, value=3)
                    ),
                    op='-',
                    right=Literal(location=L, value=5)
                )
            )
        )])


def test_chained_assignment_is_parsed_correctly() -> None:
    tokens = tokenize("a = b = c")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[BinaryOp(
        location=L, left=Identifier(location=L, name="a"), op="=", right=BinaryOp(
            location=L, left=Identifier(location=L, name="b"), op="=", right=Identifier(location=L, name="c")
        )
    )])


def test_chaining_of_not_with_both_not_and_minus_sign() -> None:
    tokens = tokenize("if not  -(a==b) then a=b")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        IfExpression(
            location=L,
            condition_branch=UnaryExpression(
                location=L,
                operator="not", operand=UnaryExpression(
                    location=L,
                    operator="-", operand=BinaryOp(
                        location=L,
                        left=Identifier(location=L, name="a"),
                        op="==",
                        right=Identifier(location=L, name="b")
                    )
                )
            ),
            then_branch=BinaryOp(
                location=L,
                left=Identifier(location=L, name="a"),
                op="=",
                right=Identifier(location=L, name="b")
            ),
            else_branch=None
        )])


def test_simple_block() -> None:
    tokens = tokenize("{f(a);\nx = y;\nf(x)}")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        FunctionExpression(location=L, function_name="f", args=[
                           Identifier(location=L, name="a")]),
        BinaryOp(
            location=L,
            left=Identifier(location=L, name="x"),
            op="=",
            right=Identifier(location=L, name="y")),
        FunctionExpression(location=L, function_name="f", args=[
                           Identifier(location=L, name="x")])]
    )


def test_simple_block_ends_with_semicolumn() -> None:
    tokens = tokenize("{f(a);\nx = y;\nf(x);}")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        FunctionExpression(location=L, function_name="f", args=[
                           Identifier(location=L, name="a")]),
        BinaryOp(location=L, left=Identifier(location=L, name="x"), op="=",
                 right=Identifier(location=L, name="y")),
        FunctionExpression(location=L, function_name="f", args=[
                           Identifier(location=L, name="x")]),
        Literal(location=L, value=None)]
    )


def test_two_expressions() -> None:
    tokens = tokenize("f(a);\nx = y;")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        FunctionExpression(location=L, function_name="f", args=[
                           Identifier(location=L, name="a")]),
        BinaryOp(location=L, left=Identifier(location=L, name="x"), op="=",
                 right=Identifier(location=L, name="y")),
        Literal(location=L, value=None)
    ]
    )


def test_expression_after_block() -> None:
    tokens = tokenize("{f(a);\nx = y;\nf(x)};\nif a==b then c")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        BlockExpression(location=L, expressions=[
            FunctionExpression(location=L, function_name="f", args=[
                               Identifier(location=L, name="a")]),
            BinaryOp(location=L, left=Identifier(location=L, name="x"), op="=",
                     right=Identifier(location=L, name="y")),
            FunctionExpression(location=L, function_name="f", args=[
                               Identifier(location=L, name="x")])
        ]
        ),
        IfExpression(
            location=L, condition_branch=BinaryOp(
                location=L,
                left=Identifier(location=L, name="a"),
                op="==",
                right=Identifier(location=L, name="b")),
            then_branch=Identifier(location=L, name="c"), else_branch=None)
    ]
    )


def test_block_without_curly_braces() -> None:
    tokens = tokenize("f(a);\nx = y;\nf(x);\nif a==b then c")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        FunctionExpression(location=L, function_name="f", args=[
                           Identifier(location=L, name="a")]),
        BinaryOp(location=L, left=Identifier(location=L, name="x"), op="=",
                 right=Identifier(location=L, name="y")),
        FunctionExpression(location=L, function_name="f",
                           args=[Identifier(location=L, name="x")]),
        IfExpression(
            location=L,
            condition_branch=BinaryOp(
                location=L,
                left=Identifier(location=L, name="a"),
                op="==",
                right=Identifier(location=L, name="b")),
            then_branch=Identifier(location=L, name="c"), else_branch=None)
    ]
    )


def test_block_without_curly_braces_ends_with_semicolumn() -> None:
    tokens = tokenize("f(a);\nx = y;\nf(x);\nif a==b then c;")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        FunctionExpression(location=L, function_name="f", args=[
                           Identifier(location=L, name="a")]),
        BinaryOp(location=L, left=Identifier(location=L, name="x"), op="=",
                 right=Identifier(location=L, name="y")),
        FunctionExpression(location=L, function_name="f", args=[
                           Identifier(location=L, name="x")]),
        IfExpression(
            location=L,
            condition_branch=BinaryOp(
                location=L,
                left=Identifier(location=L, name="a"), op="==", right=Identifier(location=L, name="b")
            ), then_branch=Identifier(location=L, name="c"), else_branch=None),
        Literal(location=L, value=None)
    ]
    )


def test_missing_semicolumn_raises_error() -> None:
    tokens = tokenize("a = b \n x = 1")
    with pytest.raises(Exception) as e:
        parse(tokens)
    assert "Expected ';', but found 'b'" in str(
        e.value)


def test_missing_semicolumn_in_last_expression_does_not_raise_error() -> None:
    tokens = tokenize("a = b; \n x = 1")

    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        BinaryOp(location=L, left=Identifier(location=L, name="a"), op="=",
                 right=Identifier(location=L, name="b")),
        BinaryOp(location=L, left=Identifier(location=L, name="x"),
                 op="=", right=Literal(location=L, value=1))
    ])


def test_semicolumn_can_exist_after_curly_braces() -> None:
    tokens = tokenize("{a = b}; \n x = 1")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        BlockExpression(location=L, expressions=[BinaryOp(
            location=L,
            left=Identifier(location=L, name="a"),
            op="=",
            right=Identifier(location=L, name="b"))]),
        BinaryOp(
            location=L,
            left=Identifier(location=L, name="x"),
            op="=", right=Literal(location=L, value=1))
    ])


def test_block_1() -> None:
    tokens = tokenize("{ { a } { b } }")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        BlockExpression(location=L, expressions=[
                        Identifier(location=L, name="a")]),
        BlockExpression(location=L, expressions=[
                        Identifier(location=L, name="b")])
    ])


def test_block_2() -> None:
    tokens = tokenize("{ a b }")
    with pytest.raises(Exception) as e:
        parse(tokens)
    assert "Expected ';', but found 'b'" in str(
        e.value)


def test_block_3() -> None:
    tokens = tokenize("{ if true then { a } b }")
    ast = parse(tokens)
    assert ast == BlockExpression(
        location=L,
        expressions=[
            IfExpression(
                location=L,
                condition_branch=BooleanLiteral(location=L, value=True),
                then_branch=BlockExpression(
                    location=L,
                    expressions=[Identifier(location=L, name="a")]
                ), else_branch=None),
            Identifier(location=L, name="b")
        ]
    )


def test_block_does_not_end_with_curly_braces() -> None:
    tokens = tokenize("{ a; b ")
    with pytest.raises(Exception) as e:
        parse(tokens)
    assert "Expected }" in str(
        e.value)


def test_block_does_not_end_with_curly_braces_when_block_is_in_the_middle_of_the_code() -> None:
    tokens = tokenize("x = 1; { a; b; {y = 2} f(a);")
    with pytest.raises(Exception) as e:
        parse(tokens)
    assert "Expected }" in str(
        e.value)


def test_block_4() -> None:
    tokens = tokenize("{ if true then { a }; b }")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        IfExpression(
            location=L,
            condition_branch=BooleanLiteral(location=L, value=True),
            then_branch=BlockExpression(
                location=L, expressions=[Identifier(location=L, name="a")]),
            else_branch=None),
        Identifier(location=L, name="b")
    ])


def test_block_5() -> None:
    tokens = tokenize("{ if true then { a } b c }")
    with pytest.raises(Exception) as e:
        parse(tokens)
    assert "Expected ';'" in str(
        e.value)


def test_block_6() -> None:
    tokens = tokenize("{ if false then { a } b; c }")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        IfExpression(
            location=L,
            condition_branch=BooleanLiteral(location=L, value=False),
            then_branch=BlockExpression(location=L, expressions=[
                                        Identifier(location=L, name="a")]),
            else_branch=None),
        Identifier(location=L, name="b"),
        Identifier(location=L, name="c")
    ])


def test_block_7() -> None:
    tokens = tokenize("{ if true then { a } else { b } 3 }")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        IfExpression(
            location=L,
            condition_branch=BooleanLiteral(location=L, value=True),
            then_branch=BlockExpression(location=L, expressions=[
                                        Identifier(location=L, name="a")]),
            else_branch=BlockExpression(location=L, expressions=[Identifier(location=L, name="b")])),
        Literal(location=L, value=3)
    ])


def test_block_8() -> None:
    tokens = tokenize("x = { { f(a) } { b } }")
    ast = parse(tokens)
    assert ast == BlockExpression(
        location=L,
        expressions=[BinaryOp(
            location=L,
            left=Identifier(location=L, name="x"),
            op="=",
            right=BlockExpression(
                location=L,
                expressions=[
                    BlockExpression(location=L,
                                    expressions=[FunctionExpression(
                                        location=L,
                                        function_name="f", args=[Identifier(location=L, name="a")])]),
                    BlockExpression(location=L, expressions=[
                        Identifier(location=L, name="b")])
                ]
            )
        )])


def test_simple_variable_declaration() -> None:
    tokens = tokenize("var x = 1")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        VariableDeclaration(location=L, variable_name="x",
                            initializer=Literal(location=L, value=1))
    ])


def test_variable_declaration() -> None:
    tokens = tokenize("var x = f(a)")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        VariableDeclaration(location=L, variable_name="x", initializer=FunctionExpression(
            location=L, function_name="f", args=[Identifier(location=L, name="a")]))
    ])


def test_dont_allow_variable_declaration_inside_if_statement() -> None:
    tokens = tokenize("if a == 2 then var x = 1")
    with pytest.raises(Exception) as e:
        parse(tokens)
    assert "Variable declaration is only allowed in top level" in str(
        e.value)


def test_simple_variable_declaration_inside_block() -> None:
    tokens = tokenize("{var x = 1}")
    ast = parse(tokens)
    assert ast == BlockExpression(location=L, expressions=[
        VariableDeclaration(location=SourceLocation("dummy", row=1, column=1), variable_name="x",
                            initializer=Literal(location=L, value=1))
    ])


def test_if_block_with_boolean_check() -> None:
    tokens = tokenize("{ if a==b == false then x = x + 2 else x = x + 3 }")
    ast = parse(tokens)
    assert ast == BlockExpression(
        location=SourceLocation(file='dummy', row=1, column=0),
        expressions=[
            IfExpression(
                location=SourceLocation(file='dummy', row=1, column=2),
                condition_branch=BinaryOp(
                    location=SourceLocation(file='dummy', row=1, column=5),
                    left=BinaryOp(
                        location=SourceLocation(file='dummy', row=1, column=5),
                        left=Identifier(location=SourceLocation(
                            file='dummy', row=1, column=5), name='a'),
                        op='==',
                        right=Identifier(location=SourceLocation(file='dummy', row=1, column=8), name='b')),
                    op='==',
                    right=BooleanLiteral(location=SourceLocation(file='dummy', row=1, column=13), value=False)),
                then_branch=BinaryOp(
                    location=SourceLocation(file='dummy', row=1, column=24),
                    left=Identifier(location=SourceLocation(
                        file='dummy', row=1, column=24), name='x'),
                    op='=',
                    right=BinaryOp(
                        location=SourceLocation(
                            file='dummy', row=1, column=28),
                        left=Identifier(location=SourceLocation(
                            file='dummy', row=1, column=28), name='x'),
                        op='+',
                        right=Literal(location=SourceLocation(file='dummy', row=1, column=32), value=2))),
                else_branch=BinaryOp(
                    location=SourceLocation(file='dummy', row=1, column=39),
                    left=Identifier(location=SourceLocation(
                        file='dummy', row=1, column=39), name='x'),
                    op='=',
                    right=BinaryOp(
                        location=SourceLocation(
                            file='dummy', row=1, column=43),
                        left=Identifier(location=SourceLocation(
                            file='dummy', row=1, column=43), name='x'),
                        op='+',
                        right=Literal(location=SourceLocation(
                            file='dummy', row=1, column=47), value=3)
                    )
                )
            )
        ]
    )


def test_parse_while_loop() -> None:
    tokens = tokenize("while a > 0 do a = a - 1")
    ast = parse(tokens)
    assert ast == BlockExpression(
        location=L, expressions=[
            WhileLoop(location=L,
                      while_condition=BinaryOp(
                          location=L,
                          left=Identifier(location=L, name="a"),
                          op=">",
                          right=Literal(location=L, value=0)
                      ),
                      do_expression=BinaryOp(
                          location=L,
                          left=Identifier(location=L, name="a"),
                          op="=",
                          right=BinaryOp(
                              location=L,
                              left=Identifier(L, "a"),
                              op="-",
                              right=Literal(L, 1)
                          )
                      ))
        ]
    )
