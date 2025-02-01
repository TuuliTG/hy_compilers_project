from typing import Any
from compiler import ast

type Value = int | bool | None


def interpret(node: ast.Expression) -> Value:
    match node:
        case ast.BlockExpression():
            return interpret(node.expressions[0])

        case ast.Literal():
            return node.value

        case ast.BinaryOp():
            a: Any = interpret(node.left)
            b: Any = interpret(node.right)
            if node.op.token == '+':
                return a + b
            elif node.op.token == '<':
                return a < b
            else:
                raise ...

        case ast.IfExpression():
            if interpret(node.condition_branch):
                return interpret(node.then_branch)
            else:
                return interpret(node.else_branch)
