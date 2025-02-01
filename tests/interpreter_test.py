from compiler.domain import Token, SourceLocation, L, TokenType
from compiler.ast import BinaryOp, BlockExpression, BooleanLiteral, Expression, FunctionExpression, IfExpression, Literal, Identifier, Operator, UnaryExpression, VariableDeclaration, WhileLoop
from compiler.parser import parse, parse
import pytest
from compiler.tokenizer import tokenize
from compiler.interpreter import interpret


def test_interpret_basic() -> None:
    tokens = tokenize("1 + 2")
    ast = parse(tokens)
    result = interpret(ast)
    assert result == 3
