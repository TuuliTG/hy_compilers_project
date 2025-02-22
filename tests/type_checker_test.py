from compiler.interpreter import SymTab
from compiler.parser import parse
from compiler.tokenizer import tokenize
from compiler.type_checker import get_type, typecheck
from compiler.types import Bool, FunType, Int, Unit, Type
import pytest


def test_type_checker_basic() -> None:
    assert _tokenize_parse_type_check("1+2") == Int
    assert _tokenize_parse_type_check("1*2") == Int
    assert _tokenize_parse_type_check("1<2") == Bool
    _assert_raises_exception(
        "true < 2", "Operator < expects two Int"
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
        "1 or 1>3", "Operator or expects two Bool")


def test_return_unit() -> None:
    assert _tokenize_parse_type_check("1+2;") == Unit
    assert _tokenize_parse_type_check("{}") == Unit


def test_unary_op() -> None:
    assert _tokenize_parse_type_check("-1") == Int
    _assert_raises_exception("- true", "Expected an integer with '-' operator")
    assert _tokenize_parse_type_check("not true") == Bool
    assert _tokenize_parse_type_check("not (1>2)") == Bool
    assert _tokenize_parse_type_check("-(1+2)") == Int
    assert _tokenize_parse_type_check("var x = 1; - x") == Int
    _assert_raises_exception("not 1", "Expected a boolean with 'not' operator")


def test_unary_op_nested() -> None:
    assert _tokenize_parse_type_check("var x = 1; if -x > 2 then 3") == Unit


def test_function() -> None:
    assert _tokenize_parse_type_check(
        "print_int(1)") == Unit


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
        "while 1 do 2+2", "While loop condition should be of boolean type, got 'Int'"
    )


def test_if_condition_with_else_branch_return_correct_type() -> None:
    assert _tokenize_parse_type_check("if 1 < 2 then 3 else 4") == Int
    assert _tokenize_parse_type_check("if 1 < 2 then true else false") == Bool


def test_if_condition_without_else_branch_returns_unit_type() -> None:
    assert _tokenize_parse_type_check("if 1 < 2 then 3") == Unit


def test_if_clause_fails_if_types_are_different() -> None:
    _assert_raises_exception("if 1<2 then 1 else true",
                             "Then branch and else branch should have the same return type")


def test_wrong_type_in_function_params() -> None:
    _assert_raises_exception("print_int(true)", "")
    _assert_raises_exception("print_bool(1+2*3)", "")


def test_bigger_program() -> None:
    _tokenize_parse_type_check("""
        var n = read_int();
        print_int(n);
        while n > 1 do {
            if n % 2 == 0 then {
                n = n / 2;
            } else {
                n = 3 * n + 1;
            }
            print_int(n);
        }
    """)

    assert True


def _tokenize_parse_type_check(code: str) -> Type:
    nodes = parse(tokenize(code))
    if nodes is not None:
        return get_type(nodes, symTab=SymTab(locals=dict(), parent=None))
    else:
        raise Exception("No nodes")


def _assert_raises_exception(code: str, error_msg: str) -> None:
    with pytest.raises(Exception) as e:
        _tokenize_parse_type_check(code)
    assert error_msg in str(
        e.value)
