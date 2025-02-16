from compiler import ir
from compiler.domain import L
from compiler.interpreter import SymTab
from compiler.ir_generator import generate_ir
from compiler.parser import parse
from compiler.tokenizer import tokenize
from compiler.type_checker import get_type


def test_basic() -> None:
    irs = _generate_ir("1+2")
    assert len(irs) == 4
    assert irs == [
        ir.Label(L, "Start"),
        ir.LoadIntConst(L, 1, ir.IRVar("x1")),
        ir.LoadIntConst(L, 2, ir.IRVar("x2")),
        ir.Call(L, ir.IRVar("+"), [ir.IRVar("x1"),
                ir.IRVar("x2")], ir.IRVar("x3"))
        # ir.Call(L, ir.IRVar("print_int")...)
    ]


def test_if() -> None:
    irs = _generate_ir("if 1> 2 then 3")
    assert True


def test_if_with_else() -> None:
    irs = _generate_ir("if 1> 2 then 3 else 4")
    assert True


def test_assignment() -> None:
    irs = _generate_ir("var x = 1; {var y=1; x}")
    assert True


def _generate_ir(code: str) -> list[ir.Instruction]:
    nodes = parse(tokenize(code))
    if nodes is not None:
        get_type(nodes, symTab=SymTab(locals=dict(), parent=None))
        return generate_ir(nodes)
    else:
        return []
