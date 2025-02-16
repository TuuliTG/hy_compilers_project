from dataclasses import dataclass
from typing import Optional
import compiler.ast as ast
from compiler.interpreter import SymTab
from compiler.types import Bool, FunType, Int, Type, Unit


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
    '%': FunType(name='funtype', args=[Int, Int], return_type=Int),
    'or': FunType(name='funtype', args=[Bool, Bool], return_type=Bool),
    'and': FunType(name='funtype', args=[Bool, Bool], return_type=Bool),
    'print_int': FunType(name='funtype', args=[Int], return_type=Unit),
    'print_bool': FunType(name='funtype', args=[Bool], return_type=Unit),
    'read_int': FunType(name='funtype', args=[Int], return_type=Unit),
})


def get_type(node: ast.Expression, symTab: SymTab) -> Type:
    type = typecheck(node, symTab)
    node.type = type
    return type


def typecheck(node: ast.Expression, symTab: SymTab) -> Type:
    if symTab.parent is None:
        symTab.parent = root_table

    def find_type(variable_name: str, symTab: SymTab) -> tuple[Type, SymTab]:
        if variable_name in symTab.locals.keys():
            return symTab.locals[variable_name], symTab
        elif symTab.parent is not None:
            return find_type(variable_name=variable_name, symTab=symTab.parent)
        else:
            raise Exception(f"Could not find type for node '{variable_name}'")

    match node:
        case ast.BinaryOp():
            t1 = get_type(node.left, symTab)
            t2 = get_type(node.right, symTab)

            if node.op.token == '==' or node.op.token == '!=':
                if t1 != t2:
                    raise Exception('Expressions should be of the same type. ')
                else:
                    return Bool

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
            token_type = get_type(node.operand, symTab)
            if operator.token == '-':
                if token_type != Int:
                    raise Exception(
                        "Expected an integer with '-' operator")
            elif operator.token == 'not':
                if token_type != Bool:
                    raise Exception("Expected a boolean with 'not' operator")
            return token_type

        case ast.VariableDeclaration():
            variable_name = node.variable_name
            if variable_name in symTab.locals.keys():
                raise Exception(
                    f"Variable '{node.variable_name}' already exists"
                )

            expected_type_of_variable = node.type

            type = get_type(node.initializer, symTab)
            if expected_type_of_variable is not None and expected_type_of_variable is not Unit:
                if type != expected_type_of_variable:
                    raise Exception(
                        f"""
                    Expected the variable to be of type {expected_type_of_variable} but found {type}
                                    """
                    )
            symTab.locals[variable_name] = type
            return Unit

        case ast.Assignment():
            variable = node.variable_name
            if not isinstance(variable, ast.Identifier):
                raise Exception(
                    f"""
                    Variable name should be a proper identifier, got
                    '{variable.name}'.
                        """
                )
            try:
                old_type, sym_tab_where_variable_was_found = find_type(
                    variable.name, symTab)
            except:
                raise Exception(
                    f"Variable '{variable.name}' has not been declared")

            new_type = get_type(node.initializer, symTab)
            if new_type != old_type:
                raise Exception(
                    f"""
                    Variable '{variable.name}' should be of the type '{old_type.name}'
                    """
                )

            sym_tab_where_variable_was_found.locals[variable.name] = new_type
            return new_type

        case ast.IfExpression():
            t1 = get_type(node.condition_branch, symTab)
            if t1 is not Bool:
                raise Exception("Expected boolean value")
            t2 = get_type(node.then_branch, symTab)
            if node.else_branch is not None:
                t3 = get_type(node.else_branch, symTab)
                if t2 != t3:
                    raise Exception(
                        "Then branch and else branch should have the same return type")
                else:
                    return t2
            return Unit

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
                get_type(node.expressions[index], sym_tab)
                index = index + 1
            return get_type(node.expressions[-1], sym_tab)

        case ast.FunctionExpression():
            type, _ = find_type(node.function_name, symTab)
            return type

        case ast.WhileLoop():
            condition_type = get_type(node.while_condition, symTab)
            if condition_type != Bool:
                raise Exception(f"While loop condition should be of boolean type, got '{condition_type}'"
                                )
            else:
                get_type(node.do_expression, symTab)
            return Unit
        case _:
            raise Exception(f"Unsupported AST node {node}")
