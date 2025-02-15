from compiler import ast, ir
from compiler.domain import L
from compiler.ir import IRVar, Instruction, Label
from compiler.types import Int


def generate_ir(root_node: ast.Expression) -> list[ir.Instruction]:
    next_var_number = 1
    next_label_number = 1

    def new_var() -> IRVar:
        nonlocal next_var_number
        var = IRVar(f'x{next_var_number}')
        next_var_number += 1
        return var

    def new_label() -> Label:
        nonlocal next_label_number
        label = Label(L, f'L{next_label_number}')
        next_label_number += 1
        return label

    instructions: list[Instruction] = []
    instructions.append(Label(L, "Start"))

    def visit(node: ast.Expression) -> IRVar:
        match node:
            case ast.Literal():
                var = new_var()
                if isinstance(node.value, int):
                    instructions.append(ir.LoadIntConst(
                        node.location, node.value, var))
                    return var
                else:
                    raise Exception(
                        "Other types of literals not implemented yet")
            case ast.BinaryOp():
                var_left = visit(node.left)
                var_right = visit(node.right)
                var_result = new_var()
                instructions.append(ir.Call(
                    location=node.location,
                    fun=IRVar(node.op.token),
                    args=[var_left, var_right],
                    dest=var_result
                ))
                return var_result

            case ast.IfExpression():
                l_then = new_label()

                var_cond = visit(node.condition_branch)
                if node.else_branch is not None:
                    l_else = new_label()
                    l_end = new_label()
                    instructions.append(ir.CondJump(
                        L, var_cond, l_then, l_else))

                else:
                    l_end = new_label()
                    instructions.append(ir.CondJump(
                        L, var_cond, l_then, l_end))

                instructions.append(l_then)
                if node.else_branch is not None:
                    copy_var = new_var()

                var_result = visit(node.then_branch)

                if node.else_branch is not None:

                    instructions.append(
                        ir.Copy(L, var_result, copy_var)
                    )
                    instructions.append(ir.Jump(L, l_end))
                    instructions.append(l_else)
                    var_else_result = visit(node.else_branch)
                    instructions.append(
                        ir.Copy(L, var_else_result, copy_var)
                    )
                    instructions.append(l_end)
                    return copy_var

                instructions.append(l_end)
                return var_result

            case ast.BlockExpression():
                index = 0
                # sym_tab = SymTab(parent=symTab, locals=dict())
                while index < len(node.expressions):
                    var_result = visit(node.expressions[index])
                    index = index + 1
                return var_result
            case _:
                raise Exception(f"Unsupported AST node: {node}")

    var_result = visit(root_node)
    if root_node.type == Int:
        instructions.append(
            ir.Call(L, IRVar("print_int"), [var_result], new_var())
        )

    # Todo if not int result (boolean or unit) do something else

    return instructions
