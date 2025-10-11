default rel
section .text

global exit
exit: ; ( rdi: code )
  mov rax, 231
  syscall

global read
read: ; ( rdi: fd, rsi: buf, rdx: count )
  mov rax, 0
  syscall
  ret

global write
write: ; ( rdi: fd, rsi: buf, rdx: count )
  mov rax, 1
  syscall
  ret

global open
open: ; ( rdi: filename, rsi: flags, rdx: mode )
  mov rax, 2
  syscall
  ret

global close
close: ; ( rdi: fd )
  mov rax, 3
  syscall
  ret

global lseek
lseek: ; ( rdi: fd, rsi: offset, rdx: whence )
  mov rax, 8
  syscall
  ret

global brk
brk: ; ( rdi: addr )
  mov rax, 12
  syscall
  ret

global socket
socket: ; ( rdi: domain, rsi: ty, rdx: protocol )
  mov rax, 41
  syscall
  ret

global connect
connect: ; ( rdi: sockfd, rsi: addr, rdx: addrlen )
  mov rax, 42
  syscall
  ret

global accept
accept: ; ( rdi: sockfd, rsi: addr, rdx: addrlen )
  mov rax, 43
  syscall
  ret

global send
send: ; ( rdi: sockfd, rsi: buf, rdx: size, rcx: flags )
  mov rax, 44
  mov r10, rcx
  syscall
  ret

global recv
recv: ; ( rdi: sockfd, rsi: buf, rdx: size, rcx: flags )
  mov rax, 45
  mov r10, rcx
  syscall
  ret

global shutdown
shutdown: ; ( rdi: sockfd, rsi: how )
  mov rax, 48
  syscall
  ret

global bind
bind: ; ( rdi: sockfd, rsi: addr, rdx: addrlen )
  mov rax, 49
  syscall
  ret

global listen
listen: ; ( rdi: sockfd, rsi: backlog )
  mov rax, 50
  syscall
  ret

