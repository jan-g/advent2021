import pathlib
import sys
import z3


def main(fn):
    with open(fn) as f:
        prog = [l.strip() for l in f]

    print(prog)

    s = z3.Optimize()

    BITLEN = 64
    def I(name):
        return z3.BitVec(name, BITLEN)
        # return z3.Int(name)

    def V(value):
        return z3.BitVecVal(value, BITLEN)
        # return int(value)

    s.set(priority='lex')
    digits = [I(x) for x in "abcdefghijklmn"]
    value = I("answer")
    one = V(1)
    zero = V(0)

    a = 0
    for d in digits:
        s.add(1 <= d)
        s.add(d <= 9)
        a = a * 10 + d
    s.add(value == a)

    vars = {}                   # index of singly-updated variables
    i = list(digits)            # input values

    def var(name):
        current, index = vars.get(name, (zero, 0))
        next = I(f"{name}{index}")
        vars[name] = (next, index + 1)
        return current, next

    def val(name):
        if name in vars:
            current, _ = vars[name]
            return current
        return V(int(name))

    for reg in "wxyz":
        s.add(var(reg)[1] == 0)

    for line in prog:
        ws = line.split()
        if ws[0] == "inp":
            d = i.pop(0)
            _, reg = var(ws[1])
            s.add(reg == d)
            continue

        # optimise for bit-shifting rather than multiplication and division by 26
        if ws[2] == "25":
            ws[2] = "31"
        elif ws[2] == "26":
            ws[2] = "32"

        if ws[0] == "mul":
            if ws[2] == "0":
                _, reg = var(ws[1])
                s.add(reg == zero)
            else:
                v = val(ws[2])
                preg, reg = var(ws[1])
                s.add(reg == preg * v)
        elif ws[0] == "add":
            v = val(ws[2])
            preg, reg = var(ws[1])
            s.add(reg == preg + v)
        elif ws[0] == "div":
            if ws[2] == "1":
                pass
            else:
                v = val(ws[2])
                preg, reg = var(ws[1])
                s.add(v != 0)
                s.add(reg == preg / v)
        elif ws[0] == "mod":
            v = val(ws[2])
            preg, reg = var(ws[1])
            s.add(v > 0)
            s.add(reg == preg % v)
        elif ws[0] == "eql":
            v = val(ws[2])
            preg, reg = var(ws[1])
            s.add(reg == z3.If(preg == v, one, zero))
        else:
            print("unknown line:", ws)

    s.add(val("z") == 0)
    print(s)

    print("maximising")
    s.push()
    s.maximize(value)
    while s.check() == z3.sat:
        m = s.model()
        print(value, "=", m.eval(value))
        print(" ".join(f"{name} = {m.eval(vars[name][0])}" for name in vars))

        if input("another?") != "":
            # Constrain against this solution, look for others.
            ans = []
            for d in digits:
                ans.append(d == m.eval(d))
            s.add(z3.Not(z3.And(ans)))
        else:
            break

    print("minimising")
    s.pop()
    s.minimize(value)
    while s.check() == z3.sat:
        m = s.model()
        print(value, "=", m.eval(value))
        print(" ".join(f"{name} = {m.eval(vars[name][0])}" for name in vars))

        if input("another?") != "":
            # Constrain against this solution, look for others.
            ans = []
            for d in digits:
                ans.append(d == m.eval(d))
            s.add(z3.Not(z3.And(ans)))
        else:
            break



if __name__ == "__main__":
    fn = pathlib.Path(sys.argv[0])
    main(fn.parent.parent / "data" / "day24" / "input")