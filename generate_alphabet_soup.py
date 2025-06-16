from random import randint,choice
from string import ascii_lowercase

num_vars = 3
num_loops = 7
loop_vars = 1

def op():
    return choice("+-*")

target = ascii_lowercase[:num_vars]

def var():
    return choice(target)

def comp_op():
    return choice([">=", "<=", ">", "<"])

def reassign_all():
    for x in target:
        print(" ", x, "=", end=" ")
        for y in target[:-1]:
            print(y, op(), end=" ")
        print(target[-1], end=";\n")

print("int main() {")
for xx in target:
    val = randint(10,100)
    print("  int", xx, "=", val, end=";\n");
    globals()[xx] = val

for l in range(num_loops):
    print("while(", end="")
    while True:
        cond = ""
        for lv in range(loop_vars):
            if lv > 0:
                cond += op()
            cond += var()
        cond += comp_op()
        cond += str(randint(-2**10, 2**10))
        if eval(cond.replace("/", "//")):
            break
    print(cond, end="")
    print(') {printf("%s\\n");' % l)

# reassign_all()

for _l in range(num_loops):
    reassign_all()
    print("}")
print('printf("done\\n");')
print("  return", target[-1], ";")
print("}")
