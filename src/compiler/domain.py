from dataclasses import dataclass
from enum import Enum
from typing import Any


class TokenType(Enum):
    INT_LITERAL = "int_literal"
    OPERATOR = "operator"
    IDENTIFIER = "identifier"
    PUNCTUATION = "punctuation"
    END = "end"


@dataclass
class SourceLocation:
    file: str
    row: int
    column: int

    def __eq__(self, other: Any) -> bool:
        if self is L or other is L:
            return True
        if not isinstance(other, SourceLocation):
            return False
        return (self.file == other.file and
                self.row == other.row and
                self.column == other.column)


L = SourceLocation(file="", row=0, column=0)


@dataclass
class Token:
    loc: SourceLocation
    type: TokenType
    text: str
