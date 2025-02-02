from dataclasses import dataclass

from compiler.domain import SourceLocation


@dataclass
class Expression:
    """Base class for AST nodes representing expressions."""
    location: SourceLocation


@dataclass
class Literal(Expression):
    value: int | bool | None
    # (value=None is used when parsing the keyword `unit`)


@dataclass
class BooleanLiteral(Expression):
    value: bool


@dataclass
class WhileLoop(Expression):
    while_condition: Expression
    do_expression: Expression


@dataclass
class Identifier(Expression):
    name: str


@dataclass
class Operator(Expression):
    token: str


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
    operator: Operator
    operand: Expression


@dataclass
class BinaryOp(Expression):
    """AST node for a binary operation like `A + B`"""
    left: Expression
    op: Operator
    right: Expression


@dataclass
class BlockExpression(Expression):
    expressions: list[Expression]


@dataclass
class VariableDeclaration(Expression):
    variable_name: str
    initializer: Expression


@dataclass
class Assignment(Expression):
    variable_name: Identifier
    initializer: Expression
