.intel_syntax noprefix

# I sure hope to have fun with an executable stack one day ... but not yet
.section .note.GNU-stack,"",@progbits

.global main
.global _main
.text
main:
call _main
# move the return value into the first argument for the syscall
mov rdi, rax
# move the exit syscall number into rax
mov rax,0x3C
syscall
