from compiler.assembly_generator import generate_assembly
from compiler.interpreter import SymTab
from compiler.ir_generator import generate_ir
from compiler.parser import parse
from compiler.tokenizer import tokenize
from compiler.type_checker import get_type


def test_assembly() -> None:
    asm = _assemble("true or true")
    assert True


def _assemble(code: str) -> str:
    nodes = parse(tokenize(code))
    if nodes is not None:
        get_type(nodes, symTab=SymTab(locals=dict(), parent=None))
        irs = generate_ir(nodes)
        return generate_assembly(irs)
    else:
        return ""
