from dataclasses import dataclass


@dataclass(frozen=True)
class Type():
    name: str

    def __str__(self) -> str:
        return f'{self.name}'


@dataclass(frozen=True)
class FunType(Type):
    args: list[Type]
    return_type: Type


Int = Type('Int')
Bool = Type('Bool')
Unit = Type('Unit')
