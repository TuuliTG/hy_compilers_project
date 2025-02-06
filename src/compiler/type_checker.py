from dataclasses import dataclass
from typing import Optional
import compiler.ast as ast
from compiler.types import Bool, FunType, Int, Type, Unit


@dataclass
class TypeTab:
    locals: dict
    parent: Optional["TypeTab"]


root_table = TypeTab(locals={}, parent=None)
root_table.locals.update({

})


def typecheck(node: ast.Expression) -> Type:
    match node:
        case ast.BinaryOp():
            t1 = typecheck(node.left)
            t2 = typecheck(node.right)
            if node.op.token in ['+', '-', '*', '/', '%']:
                if t1 is not Int or t2 is not Int:
                    raise Exception(
                        f"Operator {node.op.token} expects two integers, found {t1} and {t2}")
                return Int
            elif node.op in ['>', '<', '>=', '<=', '==', '!=']:
                if t1 is not Int or t2 is not Int:
                    raise Exception(
                        f"Operator {node.op.token} expects two integers, found {t1} and {t2}")
                return Bool

        case ast.IfExpression():
            t1 = typecheck(node.condition_branch)
            if t1 is not Bool:
                raise Exception("Expected boolean value")
            t2 = typecheck(node.then_branch)
            if node.else_branch is not None:
                t3 = typecheck(node.else_branch)
                if t2 != t3:
                    raise Exception(
                        "Then branch and else branch should have the same return type")
            return t2

        case ast.Literal():
            if isinstance(node.value, int):
                return Int
            elif node.value is None:
                return Unit
            else:
                raise Exception(f"Expected an integer, but got {node.value}")

        case ast.BooleanLiteral():
            if isinstance(node.value, bool):
                return Bool
            else:
                raise Exception(f"Expected a boolean, but got {node.value}")

        case _:
            raise Exception(f"Unsupported AST node {node}")
