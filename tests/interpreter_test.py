from dataclasses import dataclass
from typing import Any, Callable, Generator
from compiler.domain import Token, SourceLocation, L, TokenType
from compiler.ast import BinaryOp, BlockExpression, BooleanLiteral, Expression, FunctionExpression, IfExpression, Literal, Identifier, Operator, UnaryExpression, VariableDeclaration, WhileLoop
from compiler.parser import parse, parse
import pytest
from compiler.tokenizer import tokenize
from compiler.interpreter import SymTab, Value, interpret


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


def test_and_operator() -> None:
    assert tokenize_parse_and_interpret(
        "var x=1; var y=2; if x==1 and y==2 then x+y else 0") == 3


def test_and_operator_2() -> None:
    assert tokenize_parse_and_interpret(
        "var x=1; var y=2; if x==1 and y==3 then x+y else 0") == 0


def test_and_operator_right_hand_side_not_evaluated() -> None:
    assert tokenize_parse_and_interpret(
        """
    var evaluated_right_hand_side = false;
    false and { evaluated_right_hand_side = true; true };
    evaluated_right_hand_side 
    """
    ) == False


def test_or_operator_right_hand_side_not_evaluated() -> None:
    assert tokenize_parse_and_interpret(
        """
    var evaluated_right_hand_side = false;
    true or { evaluated_right_hand_side = true; true };
    evaluated_right_hand_side 
    """
    ) == False


def test_or_operator() -> None:
    assert tokenize_parse_and_interpret(
        "var x = 1; var y = 2; if x == 2 or y == 2 then 2 else 0") == 2


def test_while_loop() -> None:
    assert tokenize_parse_and_interpret(
        "var x = 10; while x > 0 do x = x - 1; x") == 0


def test_function_call(capsys: Any) -> None:
    assert tokenize_parse_and_interpret("print_int(1)") == None
    captured = capsys.readouterr()
    assert captured.out == "1\n"


def test_function_call_inside_expression(capsys: Any) -> None:
    assert tokenize_parse_and_interpret(
        "var x = 2; if x == 2 then print_int(x+1) else print_int(x)") == None
    captured = capsys.readouterr()
    assert captured.out == "3\n"


def test_if_expression_without_else_branch() -> None:
    assert tokenize_parse_and_interpret("if 1 < 2 then 3") == None


def test_if_expression_without_else_branch_2() -> None:
    assert tokenize_parse_and_interpret(
        "var x =1; if 1 < 2 then x=2; x") == 2


def tokenize_parse_and_interpret(code: str) -> Value:
    nodes = parse(tokenize(code))
    if nodes is not None:
        return interpret(nodes, symTab=SymTab(locals=dict(), parent=None))
    else:
        raise Exception("No nodes")
