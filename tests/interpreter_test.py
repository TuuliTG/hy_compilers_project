from dataclasses import dataclass
from compiler.domain import Token, SourceLocation, L, TokenType
from compiler.ast import BinaryOp, BlockExpression, BooleanLiteral, Expression, FunctionExpression, IfExpression, Literal, Identifier, Operator, UnaryExpression, VariableDeclaration, WhileLoop
from compiler.parser import parse, parse
import pytest
from compiler.tokenizer import tokenize
from compiler.interpreter import SymTab, interpret


def test_interpret_basic_arithmetics() -> None:
    assert tokenize_parse_and_interpret("1 + 2") == 3
    assert tokenize_parse_and_interpret("3-2") == 1
    assert tokenize_parse_and_interpret("2*3") == 6
    assert tokenize_parse_and_interpret("1+2*4") == 9


def test_interpret_block() -> None:
    res = tokenize_parse_and_interpret("1+2; {2+2; 2-1}")
    assert res == 1


def test_parse_block_which_ends_in_semicolon() -> None:
    res = tokenize_parse_and_interpret("1+2; {2+2; 2-1;}")
    assert res == None


def test_varible_declaration() -> None:
    res = tokenize_parse_and_interpret("var x = 1; var y = 2; x + y")
    assert res == 3


def test_varible_declaration_in_different_blocks() -> None:
    res = tokenize_parse_and_interpret(
        "var x = 1; var y = 2;  var z = {var x = 3; var y = 5; x + y}; x + y + z"
    )
    assert res == 11


def test_varible_declaration_in_different_blocks_fetches_variable_from_outer_scope() -> None:
    res = tokenize_parse_and_interpret(
        "var x = 1; var y = 2;  var z = {x + y}; x + y + z"
    )
    assert res == 6


def test_varible_declaration_in_different_blocks_fails_if_variable_is_not_found() -> None:
    with pytest.raises(Exception) as e:
        tokenize_parse_and_interpret(
            "var x = 1; var y = 2;  var z = {x + a}; x + y + z"
        )
    assert "Unkown variable 'a'" in str(
        e.value)


def test_unary_operations() -> None:
    assert tokenize_parse_and_interpret("var x = 1; -x") == -1
    assert tokenize_parse_and_interpret("var x = 2; not x") == -2


def test_assignment() -> None:
    assert tokenize_parse_and_interpret("var x=1; x=2; x") == 2


def test_assignment_fails_if_variable_is_not_declared() -> None:
    with pytest.raises(Exception) as e:
        tokenize_parse_and_interpret(
            "var x=1; y=2; x"
        )
    assert "Unkown variable 'y'" in str(
        e.value)


def tokenize_parse_and_interpret(code: str):
    return interpret(parse(tokenize(code)), symTab=SymTab(locals=dict(), parent=None))
