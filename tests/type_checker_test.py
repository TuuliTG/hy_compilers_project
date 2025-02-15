from compiler.interpreter import SymTab
from compiler.parser import parse
from compiler.tokenizer import tokenize
from compiler.type_checker import get_type, typecheck
from compiler.types import Bool, FunType, Int, Type, Unit
import pytest


def test_type_checker_basic() -> None:
    assert _tokenize_parse_type_check("1+2") == Int
    assert _tokenize_parse_type_check("1*2") == Int
    assert _tokenize_parse_type_check("1<2") == Bool
    _assert_raises_exception(
        "true < 2", "Operator < expects two BasicType(name='Int')"
    )


def test_equals_and_not_equals() -> None:
    assert _tokenize_parse_type_check("1==2") == Bool
    assert _tokenize_parse_type_check("1 != 2") == Bool
    assert _tokenize_parse_type_check("true != false") == Bool
    _assert_raises_exception(
        "true == 1", "Expressions should be of the same type"
    )
    _assert_raises_exception(
        "true != 1", "Expressions should be of the same type"
    )


def test_and_or() -> None:
    assert _tokenize_parse_type_check("1<2 or 1>3") == Bool
    assert _tokenize_parse_type_check("1<2 and 1>3") == Bool
    _assert_raises_exception(
        "1 or 1>3", "Operator or expects two BasicType(name='Bool')")


def test_return_unit() -> None:
    assert _tokenize_parse_type_check("1+2;") == Unit
    assert _tokenize_parse_type_check("{}") == Unit


def test_unary_op() -> None:
    assert _tokenize_parse_type_check("-1") == Int
    _assert_raises_exception("- true", "Expected an integer with '-' operator")
    assert _tokenize_parse_type_check("not true") == Bool
    assert _tokenize_parse_type_check("not (1>2)") == Bool
    assert _tokenize_parse_type_check("-(1+2)") == Int
    _assert_raises_exception("not 1", "Expected a boolean with 'not' operator")


def test_function() -> None:
    assert _tokenize_parse_type_check(
        "print_int(1)") == FunType(name='funtype', args=[Int], return_type=Unit)


def test_variable_declaration() -> None:
    assert _tokenize_parse_type_check("var a = 1;") == Unit
    _assert_raises_exception("var a = 1; var a=2",
                             "Variable 'a' already exists")


def test_typed_variable_declaration() -> None:
    assert _tokenize_parse_type_check("var x: Int = 1") == Unit
    _assert_raises_exception(
        "var x: Int = true", "Expected the variable to be of type Int but found Bool"
    )
    assert _tokenize_parse_type_check("var x: Bool = true") == Unit


def test_assignment() -> None:
    assert _tokenize_parse_type_check("var a = 1; a = 2") == Int
    assert _tokenize_parse_type_check("var a = 1; {var b=2; a = 3}") == Int
    _assert_raises_exception(
        "var a = 1; b = 2", "Variable 'b' has not been declared"
    )
    _assert_raises_exception(
        "var a = 1; {var b=2; a = true}", "Variable 'a' should be of the type 'Int'")


def test_while_loop() -> None:
    assert _tokenize_parse_type_check("while true do 1+2") == Unit
    _assert_raises_exception(
        "while 1 do 2+2", "While loop condition should be of boolean type, got 'BasicType(name='Int')'"
    )


def test_if_condition_with_else_branch_return_correct_type() -> None:
    assert _tokenize_parse_type_check("if 1 < 2 then 3 else 4") == Int
    assert _tokenize_parse_type_check("if 1 < 2 then true else false") == Bool


def test_if_condition_without_else_branch_returns_unit_type() -> None:
    assert _tokenize_parse_type_check("if 1 < 2 then 3") == Unit


def test_if_clause_fails_if_types_are_different() -> None:
    _assert_raises_exception("if 1<2 then 1 else true",
                             "Then branch and else branch should have the same return type")


def _tokenize_parse_type_check(code: str) -> Type:
    return get_type(parse(tokenize(code)), symTab=SymTab(locals=dict(), parent=None))


def _assert_raises_exception(code: str, error_msg: str) -> None:
    with pytest.raises(Exception) as e:
        _tokenize_parse_type_check(code)
    assert error_msg in str(
        e.value)
