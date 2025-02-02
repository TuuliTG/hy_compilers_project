from dataclasses import dataclass
from typing import Any, Callable, Optional
from compiler import ast

type Value = int | bool | None | Callable[[Value, Value], Value]


@dataclass
class SymTab:
    locals: dict
    parent: Optional["SymTab"]


root_table = SymTab(locals={}, parent=None)


def add(a: Value, b: Value) -> Value:
    return a + b


def subtract(a: Value, b: Value) -> Value:
    return a - b


def multiply(a: Value, b: Value) -> Value:
    return a * b


def divide(a: Value, b: Value) -> Value:
    return a // b


def less_than(a: Value, b: Value) -> Value:
    return a < b


def more_than(a: Value, b: Value) -> Value:
    return a > b


def less_than_or_equal(a: Value, b: Value) -> Value:
    return a <= b


def more_than_or_equal(a: Value, b: Value) -> Value:
    return a >= b


def equals(a: Value, b: Value) -> Value:
    return a == b


def modulus(a: Value, b: Value) -> Value:
    return a % b


def not_equals(a: Value, b: Value) -> Value:
    return a != b


def unary_negate(a: Value) -> Value:
    return -a


root_table.locals.update({
    '+': add,
    '-': subtract,
    '*': multiply,
    '/': divide,
    '<': less_than,
    '>': more_than,
    '<=': less_than_or_equal,
    '>=': more_than_or_equal,
    '==': equals,
    '!=': not_equals,
    '%': modulus,
    'unary_-': unary_negate,
    'unary_not': unary_negate,
    'true': True,
    'false': False
})


def interpret(node: ast.Expression, symTab: SymTab) -> Value:
    def find_variable(variable_name: str, symTab: SymTab) -> Value:
        if variable_name in symTab.locals.keys():
            return symTab.locals[variable_name]
        elif symTab.parent is not None:
            return find_variable(variable_name=variable_name, symTab=symTab.parent)
        else:
            raise Exception(f"Unkown variable '{variable_name}'")

    match node:
        case ast.BlockExpression():
            index = 0
            sym_tab = SymTab(parent=symTab, locals=dict())
            while index < len(node.expressions)-1:
                interpret(node.expressions[index], sym_tab)
                index = index + 1
            return interpret(node.expressions[-1], sym_tab)

        case ast.Literal():
            return node.value

        case ast.BinaryOp():
            a: Any = interpret(node.left, symTab)
            b: Any = interpret(node.right, symTab)
            if node.op.token == 'and':
                if a == False:
                    return False
                if a == True and b == True:
                    return True
            elif node.op.token == 'or':
                if a == True:
                    return True
                elif b == True:
                    return True
                else:
                    return False
            else:

                operator_function = find_variable(node.op.token, root_table)

                if not callable(operator_function):
                    raise Exception(
                        f"Operator {node.op.token} is not a function")

                return operator_function(a, b)

        case ast.UnaryExpression():
            operand = interpret(node.operand, symTab)
            operator_function = find_variable(
                f"unary_{node.operator.token}", root_table)

            if not callable(operator_function):
                raise Exception(f"Operator {node.op.token} is not a function")

            return operator_function(operand)

        case ast.IfExpression():
            if interpret(node.condition_branch, symTab):
                return interpret(node.then_branch, symTab)
            else:
                return interpret(node.else_branch, symTab)

        case ast.Identifier():
            return find_variable(node.name, symTab)

        case ast.VariableDeclaration():
            variable_name = node.variable_name
            initializer = interpret(node.initializer, symTab)
            symTab.locals[variable_name] = initializer
            return None

        case ast.Assignment():
            variable_name = node.variable_name.name
            # Check if variable exists
            find_variable(variable_name, symTab)

            value = interpret(node.initializer, symTab)
            symTab.locals[variable_name] = value
            return value

        case ast.BooleanLiteral():
            return node.value

        case _:
            raise Exception(f"Unsupported AST node {node}")
