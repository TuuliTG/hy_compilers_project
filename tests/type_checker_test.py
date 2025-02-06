from compiler.parser import parse
from compiler.tokenizer import tokenize
from compiler.type_checker import typecheck
from compiler.types import Int, Type
import pytest


def test_type_checker_basic() -> None:
    assert _tokenize_parse_type_check("1+2") == Int


def _tokenize_parse_type_check(code: str) -> Type:
    return typecheck(parse(tokenize(code)))
