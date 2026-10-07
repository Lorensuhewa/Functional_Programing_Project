class Expr:
    def __init__(self, kind, value=None, left=None, right=None):
        self.kind = kind
        self.value = value
        self.left = left
        self.right = right


def eval_expr(env, expr):

    if expr.kind == "lit":
        return expr.value

    elif expr.kind == "var":
        if expr.value in env:
            return env[expr.value]
        else:
            raise ValueError("Undefined variable: " + expr.value)

    elif expr.kind == "add":
        left = eval_expr(env, expr.left)
        right = eval_expr(env, expr.right)
        return left + right

    elif expr.kind == "sub":
        left = eval_expr(env, expr.left)
        right = eval_expr(env, expr.right)
        return left - right

    elif expr.kind == "mul":
        left = eval_expr(env, expr.left)
        right = eval_expr(env, expr.right)
        return left * right

    elif expr.kind == "div":
        left = eval_expr(env, expr.left)
        right = eval_expr(env, expr.right)

        if right == 0:
            raise ZeroDivisionError("Division by zero")

        return left / right

    else:
        raise ValueError("Unknown expression")