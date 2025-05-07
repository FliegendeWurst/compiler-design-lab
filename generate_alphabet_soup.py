from random import randint,choice
from string import ascii_lowercase

print("int main() {")

for x in ascii_lowercase:
    print("  int", x, "=", randint(10,100), end=";\n");

for x in ascii_lowercase:
    print(" ", x, "=", end=" ")
    for y in ascii_lowercase[:-1]:
        print(y, choice("+-/*%"), end=" ")
    print(ascii_lowercase[-1], end=";\n")


print("  return z;")
print("}")
