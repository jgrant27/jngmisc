;;   pi_x64.asm - calculates Pi using the Leibniz formula.
;;                Each iteration prints a closer approximation to 50 digits.
;;                This is not an optimal implementation and it runs forever.
;;
;;  x86-64/SSE3 with nasm for Linux, Intel, gcc
;;
;;  assemble: nasm -felf64 pi_x64.asm
;;  link:     gcc -o pi_x64 pi_x64.o
;;  run:      ./pi_x64
;;  output: 3.14159264858204423376264458056539297103881835937500
;;          3.14159265108366625440794450696557760238647460937500
;;          3.14159265191852199450295302085578441619873046875000
;;          3.14159265233600137889879988506436347961425781250000
;;          .... and on forever ...
        
global main
extern printf

section .code
align 64
main:
        push  rbp
        mov rbp,rsp

        movdqa  xmm2, oword[numer]
        movdqa  xmm6, oword[denom]
        movdqa  xmm3, oword[add4]
        movdqa  xmm4, xmm2
        movdqa  xmm5, oword[zero]
        mov r12, 100000000

align 64
loop:
        divpd  xmm2, xmm6
        addpd  xmm5, xmm2
        movdqa xmm2, xmm4
        addpd  xmm6, xmm3

        sub r12, 1
        jnz loop

        mov     r12, 100000000
        movdqa xmm0, xmm5
        movdqa xmm1, xmm6
        haddpd xmm0, xmm0

        mov eax, 1
        mov edi, msg
        call printf

        movdqa xmm3, oword[add4]
        
        jmp loop

section .data
align 16
denom:  dq  1.0, 3.0
numer:  dq  4.0, -4.0
add4:   dq  4.0, 4.0
zero:   dq  0.0, 0.0
msg:    db  "%1.50f", 0xA, 0
;; msg:    db  "xmm0: %1.20f  xmm1: %f  xmm2: %f  xmm3: %f  xmm4: %f  xmm5: %f  xmm6: %f", 0xA, 0        
