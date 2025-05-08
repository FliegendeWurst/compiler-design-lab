from random import randint,choice
from string import ascii_lowercase

target = ascii_lowercase[:5]

print("int main() {")

for x in target:
    print("  int", x, "=", randint(10,100), end=";\n");

for x in target:
    print(" ", x, "=", end=" ")
    for y in target[:-1]:
        print(y, choice("+-/*%"), end=" ")
    print(target[-1], end=";\n")


print("  return", target[-1], ";")
print("}")
