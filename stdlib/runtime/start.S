default rel
section .text
extern main
global _start
_start:
    call    main wrt ..plt
    mov rax, 231
    mov rdi, 0
    syscall

