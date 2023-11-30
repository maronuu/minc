.globl f
f:
  pushq %rbp
  movq %rsp, %rbp
  pushq %rbx
  pushq %r12
  pushq %r13
  pushq %r14
  pushq %r15
  movq %rdi, -8(%rbp)
    movq    $1, %rsi
    movq    -8(%rbp), %r15
    addq    %rsi, %r15
    movq    %r15, %rcx
    movq    %rcx, -16(%rbp)

    movq    $3, %r8
    movq    -16(%rbp), %r15
    imulq    %r8, %r15
    movq    %r15, %r9
    movq    %r9, %rax
    jmp     .Lendf
.Lendf:
  popq %r15
  popq %r14
  popq %r13
  popq %r12
  popq %rbx
  movq %rbp, %rsp
  popq %rbp
  retq
