from compiler import ast, ir
from compiler.domain import L
from compiler.interpreter import SymTab
from compiler.ir import IRVar, Instruction, Label
from compiler.types import BasicType, Bool, Int, Type, Unit


def get_root_types() -> dict[IRVar, Type]:
    root_types = {
        IRVar('print_int'): Unit,
        IRVar('print_bool'): Unit
    }
    return root_types


def generate_ir(root_node: ast.Expression) -> list[ir.Instruction]:
    root_types = get_root_types()
    var_types: dict[IRVar, Type] = root_types.copy()

    var_unit = IRVar('unit')
    var_types[var_unit] = Unit

    next_var_number = 1
    next_label_number = 1

    def new_var(t: Type) -> IRVar:
        nonlocal next_var_number
        var = IRVar(f'x{next_var_number}')
        var_types[var] = t
        next_var_number += 1
        return var

    def new_label(name: str = None) -> Label:
        nonlocal next_label_number
        if name is None:
            label = Label(L, f'L{next_label_number}')
            next_label_number += 1
        else:
            label = Label(L, name)
        return label

    def generate_if_expression_without_else_branch(node: ast.Expression, symTab) -> IRVar:
        l_then = new_label()
        var_cond = visit(node.condition_branch, symTab)
        l_end = new_label()
        instructions.append(ir.CondJump(
            L, var_cond, l_then, l_end)
        )
        instructions.append(l_then)
        visit(node.then_branch, symTab)
        instructions.append(l_end)
        return var_unit

    def find_ir_var(variable_name: str, symTab: SymTab) -> IRVar:
        if variable_name in symTab.locals.keys():
            return symTab.locals[variable_name]
        elif symTab.parent is not None:
            return find_ir_var(variable_name=variable_name, symTab=symTab.parent)
        else:
            raise Exception(f"Could not find type for node '{variable_name}'")

    instructions: list[Instruction] = []
    instructions.append(Label(L, "Start"))

    def visit(node: ast.Expression, symTab: SymTab) -> IRVar:
        match node:
            case ast.Literal():
                if isinstance(node.value, int):
                    var = new_var(Int)
                    instructions.append(ir.LoadIntConst(
                        node.location, node.value, var))
                    return var
                elif node.value is None:
                    return var_unit
                else:
                    raise Exception(
                        f"""Other types of literals not implemented yet, found {
                            node.type}"""
                    )
            case ast.BooleanLiteral():
                var = new_var()
                instructions.append(ir.LoadBoolConst(
                    node.location, node.value, var
                ))
                return var
            case ast.BinaryOp():
                var_left = visit(node.left, symTab)
                var_right = visit(node.right, symTab)
                var_result = new_var(node.type)
                instructions.append(ir.Call(
                    location=node.location,
                    fun=IRVar(node.op.token),
                    args=[var_left, var_right],
                    dest=var_result
                ))
                return var_result

            case ast.UnaryExpression():
                var_operand = visit(node.operand, symTab)
                var_result = new_var(node.type)
                var_name = ""
                if node.operator.token == 'not':
                    var_name = "unary_not"
                else:
                    var_name = "unary_-"
                instructions.append(ir.Call(
                    location=node.location,
                    fun=IRVar(var_name),
                    args=[var_operand],
                    dest=var_result
                ))

                return var_result

            case ast.VariableDeclaration():
                variable_name = node.variable_name
                value_var = visit(node.initializer, symTab)
                variable_var = new_var(node.initializer.type)
                symTab.locals[variable_name] = variable_var
                root_types[variable_var] = node.initializer.type
                instructions.append(
                    ir.Copy(node.location, source=value_var, dest=variable_var))
                return value_var

            case ast.Assignment():
                variable_name = node.variable_name.name
                assignement = visit(node.initializer, symTab)
                var = find_ir_var(variable_name, symTab)
                instructions.append(
                    ir.Copy(node.location, source=assignement, dest=var))
                return var

            case ast.Identifier():
                return find_ir_var(node.name, symTab)

            case ast.FunctionExpression():
                function_var = find_ir_var(node.function_name, symTab)
                args_vars = []
                for arg in node.args:
                    args_vars.append(visit(arg, symTab))
                dest_var = new_var(node.type)
                instructions.append(
                    ir.Call(node.location, function_var,
                            args=args_vars, dest=dest_var)
                )
                return dest_var
            case ast.WhileLoop():
                l_start = new_label("while_start")
                l_body = new_label("while_body")
                l_end = new_label("while_end")

                instructions.append(l_start)
                var_cond = visit(node.while_condition, symTab)
                instructions.append(ir.CondJump(
                    node.location, var_cond, l_body, l_end))
                instructions.append(l_body)
                visit(node.do_expression, symTab)
                instructions.append(
                    ir.Jump(node.do_expression.location, l_start)
                )
                instructions.append(l_end)

            case ast.IfExpression():
                if node.else_branch is None:
                    return generate_if_expression_without_else_branch(node, symTab)
                else:
                    l_then = new_label()
                    var_cond = visit(node.condition_branch, symTab)
                    l_else = new_label()
                    l_end = new_label()
                    instructions.append(ir.CondJump(
                        L, var_cond, l_then, l_else)
                    )
                    instructions.append(l_then)
                    copy_var = new_var(node.then_branch.type)
                    var_result = visit(node.then_branch, symTab)
                    instructions.append(
                        ir.Copy(L, var_result, copy_var)
                    )
                    instructions.append(ir.Jump(L, l_end))
                    instructions.append(l_else)
                    var_else_result = visit(node.else_branch, symTab)
                    instructions.append(
                        ir.Copy(L, var_else_result, copy_var)
                    )
                    instructions.append(l_end)
                    return copy_var

            case ast.BlockExpression():
                index = 0
                sym_tab_child = SymTab(parent=symTab, locals=dict())
                while index < len(node.expressions):
                    var_result = visit(node.expressions[index], sym_tab_child)
                    index = index + 1
                return var_result
            case _:
                raise Exception(f"Unsupported AST node: {node}")

    root_symtab = SymTab(locals={}, parent=None)
    for v in root_types.keys():
        root_symtab.locals[v.name] = v

    # Start visiting the AST from the root.
    var_final_result = visit(root_node, root_symtab)

    if var_types[var_final_result] == Int:
        instructions.append(
            ir.Call(L, IRVar("print_int"), [var_final_result], new_var(Unit))
        )
    elif var_types[var_final_result] == Bool:
        instructions.append(
            ir.Call(L, IRVar("print_bool"), [var_final_result], new_var(Unit))
        )

    return instructions
