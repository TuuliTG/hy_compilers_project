from compiler import ast, ir
from compiler.ir import IRVar, Instruction


def generate_ir(root_node: ast.Expression) -> list[ir.Instruction]:
    next_var_number = 1

    def new_var() -> IRVar:
        nonlocal next_var_number
        var = IRVar(f'x{next_var_number}')
        next_var_number += 1
        return var

    instructions: list[Instruction] = []

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
            case _:
                raise Exception(f"Unsupported AST node: {node}")

    visit(root_node)
    return instructions
