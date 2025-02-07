from dataclasses import dataclass
from typing import Optional
import compiler.ast as ast
from compiler.interpreter import SymTab
from compiler.types import BasicType, Bool, FunType, Int, Type, Unit


root_table = SymTab(locals={}, parent=None)
root_table.locals.update({
    '+': FunType(name='funtype', args=[Int, Int], return_type=Int),
    '-': FunType(name='funtype', args=[Int, Int], return_type=Int),
    '*': FunType(name='funtype', args=[Int, Int], return_type=Int),
    '/': FunType(name='funtype', args=[Int, Int], return_type=Int),
    '<': FunType(name='funtype', args=[Int, Int], return_type=Bool),
    '>': FunType(name='funtype', args=[Int, Int], return_type=Bool),
    '<=': FunType(name='funtype', args=[Int, Int], return_type=Bool),
    '>=': FunType(name='funtype', args=[Int, Int], return_type=Bool),
    '==': FunType(name='funtype', args=[Int, Int], return_type=Bool),
    '!=': FunType(name='funtype', args=[Int, Int], return_type=Bool),
    '%': FunType(name='funtype', args=[Int, Int], return_type=Int),
    'or': FunType(name='funtype', args=[Bool, Bool], return_type=Bool),
    'and': FunType(name='funtype', args=[Bool, Bool], return_type=Bool),
    'print_int': FunType(name='funtype', args=[Int], return_type=Unit),
})


def typecheck(node: ast.Expression, symTab: SymTab) -> Type:
    if symTab.parent is None:
        symTab.parent = root_table

    def find_type(variable_name: str, symTab: SymTab) -> tuple[BasicType, SymTab]:
        if variable_name in symTab.locals.keys():
            return symTab.locals[variable_name], symTab
        elif symTab.parent is not None:
            return find_type(variable_name=variable_name, symTab=symTab.parent)
        else:
            raise Exception(f"Could not find type for node '{variable_name}'")

    match node:
        case ast.BinaryOp():
            t1 = typecheck(node.left, symTab)
            t2 = typecheck(node.right, symTab)

            expected_type, _ = find_type(node.op.token, symTab)
            if isinstance(expected_type, FunType):
                if t1 is not expected_type.args[0] or t2 is not expected_type.args[0]:
                    raise Exception(
                        f"Operator {node.op.token} expects two {expected_type.args[0]}s, found {t1} and {t2}")
                return expected_type.return_type
            else:
                raise Exception(f"Unknown binary operation {node.op.token}")

        case ast.UnaryExpression():
            operator = node.operator
            token_type = typecheck(node.operand, symTab)
            if operator.token == '-':
                if token_type.name != 'Int':
                    raise Exception(
                        "Expected an integer with '-' operator")
            elif operator.token == 'not':
                if token_type.name != 'Bool':
                    raise Exception("Expected a boolean with 'not' operator")
            return token_type

        case ast.VariableDeclaration():
            variable_name = node.variable_name
            if variable_name in symTab.locals.keys():
                raise Exception(
                    f"Variable '{node.variable_name}' already exists"
                )

            type = typecheck(node.initializer, symTab)
            symTab.locals[variable_name] = type
            return Unit

        case ast.Assignment():
            variable_name = node.variable_name
            if not isinstance(variable_name, ast.Identifier):
                raise Exception(
                    f"""
                    Variable name should be a proper identifier, got
                    '{variable_name.name}'.
                        """
                )
            try:
                _, sym_tab_where_variable_was_found = find_type(
                    variable_name.name, symTab)
            except:
                raise Exception(
                    f"Variable '{variable_name.name}' has not been declared")

            new_type = typecheck(node.initializer, symTab)
            sym_tab_where_variable_was_found.locals[variable_name.name] = new_type
            return new_type

        case ast.IfExpression():
            t1 = typecheck(node.condition_branch, symTab)
            if t1 is not Bool:
                raise Exception("Expected boolean value")
            t2 = typecheck(node.then_branch, symTab)
            if node.else_branch is not None:
                t3 = typecheck(node.else_branch, symTab)
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

        case ast.Identifier():
            type, _ = find_type(node.name, symTab)
            return type

        case ast.BooleanLiteral():
            if isinstance(node.value, bool):
                return Bool
            else:
                raise Exception(f"Expected a boolean, but got {node.value}")

        case ast.BlockExpression():
            index = 0
            sym_tab = SymTab(parent=symTab, locals=dict())
            while index < len(node.expressions)-1:
                typecheck(node.expressions[index], sym_tab)
                index = index + 1
            return typecheck(node.expressions[-1], sym_tab)

        case ast.FunctionExpression():
            type, _ = find_type(node.function_name, symTab)
            return type

        case _:
            raise Exception(f"Unsupported AST node {node}")
