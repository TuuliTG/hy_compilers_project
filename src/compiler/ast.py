from dataclasses import dataclass


@dataclass
class Expression:
    """Base class for AST nodes representing expressions."""


@dataclass
class Literal(Expression):
    value: int | bool | None
    # (value=None is used when parsing the keyword `unit`)


@dataclass
class Identifier(Expression):
    name: str


@dataclass
class IfExpression(Expression):
    condition_branch: Expression
    then_branch: Expression
    else_branch: Expression | None


@dataclass
class FunctionExpression(Expression):
    function_name: str
    args: list[Expression]


@dataclass
class UnaryExpression(Expression):
    operator: str
    operand: Expression


@dataclass
class BinaryOp(Expression):
    """AST node for a binary operation like `A + B`"""
    left: Expression
    op: str
    right: Expression


@dataclass
class BlockExpression(Expression):
    expressions: list[Expression]
