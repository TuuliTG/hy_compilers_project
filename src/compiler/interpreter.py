from dataclasses import dataclass
from typing import Any
from compiler import ast

type Value = int | bool | None

built_ins = {

}


@dataclass
class SymTab():
    locals: dict


@dataclass
class SymTabChild(SymTab):
    parent: SymTab | None


def interpret(node: ast.Expression, symTab: SymTab) -> Value:
    def find_variable(variable_name: str, symTab: SymTabChild) -> Value:
        if variable_name in symTab.locals.keys():
            return symTab.locals[variable_name]
        elif isinstance(symTab.parent, SymTabChild):
            return find_variable(variable_name=variable_name, symTab=symTab.parent)
        else:
            raise Exception(f"Unkown variable '{variable_name}'")

    match node:
        case ast.BlockExpression():
            index = 0
            sym_tab_child = SymTabChild(parent=symTab, locals=dict())
            while index < len(node.expressions)-1:
                interpret(node.expressions[index], sym_tab_child)
                index = index + 1
            return interpret(node.expressions[-1], sym_tab_child)

        case ast.Literal():
            return node.value

        case ast.BinaryOp():
            a: Any = interpret(node.left, symTab)
            b: Any = interpret(node.right, symTab)
            if node.op.token == '+':
                return a + b
            elif node.op.token == '-':
                return a - b
            elif node.op.token == '<':
                return a < b
            elif node.op.token == '*':
                return a * b
            else:
                raise Exception(f"Unsupported operator {node.op.token}")

        case ast.IfExpression():
            if interpret(node.condition_branch):
                return interpret(node.then_branch)
            else:
                return interpret(node.else_branch)

        case ast.Identifier():
            return find_variable(node.name, symTab)

        case ast.VariableDeclaration():
            variable_name = node.variable_name
            initializer = interpret(node.initializer, symTab)
            symTab.locals[variable_name] = initializer
            return None

        case _:
            raise Exception(f"Unsupported AST node {node}")
