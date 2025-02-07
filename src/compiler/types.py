from dataclasses import dataclass


@dataclass(frozen=True)
class Type:
    """Base class for types"""


@dataclass(frozen=True)
class BasicType(Type):
    name: str


@dataclass(frozen=True)
class FunType(BasicType):
    args: list[Type]
    return_type: Type


Int = BasicType('Int')
Bool = BasicType('Bool')
Unit = BasicType('Unit')
