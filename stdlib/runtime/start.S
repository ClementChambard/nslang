default rel
section .data
extern __ns__argc
extern __ns__argv
extern __ns__env
section .text
extern main
global _start
_start:
    pop rsi
    mov QWORD[__ns__argc], rsi
    mov rdx, rsp
    mov QWORD[__ns__argv], rdx
    and rsp, 0xfffffffffffffff0
    ; env
    ; global init stuff...
    call    main wrt ..plt
    mov rax, 231
    mov rdi, 0
    syscall

