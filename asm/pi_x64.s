//
// Copyright (c) 2010, Justin Grant <justin at imagine27 dot com>
// All rights reserved.

// Redistribution and use in source and binary forms, with or without modification,
// are permitted provided that the following conditions are met:

// Redistributions of source code must retain the above copyright notice, this list
// of conditions and the following disclaimer.
// Redistributions in binary form must reproduce the above copyright notice, this
// list of conditions and the following disclaimer in the documentation and/or
// other materials provided with the distribution.
// Neither the name of the <ORGANIZATION> nor the names of its contributors may be
// used to endorse or promote products derived from this software without specific
// prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

//   pi_x64.s - calculates Pi using the Leibniz formula.
//              Each iteration prints a closer approximation to 50 digits.
//              This is not an optimal implementation and it runs forever.
//
//  x86-64/SSE3 with for Linux, Intel, gnu assembler, gcc
//
//  assemble: as pi_x64.s -o pi_x64.o
//  link:     gcc -o pi_x64 pi_x64.o
//  run:      ./pi_x64
//  output: 3.14159264858204423376264458056539297103881835937500
//          3.14159265108366625440794450696557760238647460937500
//          3.14159265191852199450295302085578441619873046875000
//          3.14159265233600137889879988506436347961425781250000
//          .... and on forever ...




.section .data
        .align 16
denom:  .double  1.0, 3.0
numer:  .double  4.0, -4.0
add4:   .double  4.0, 4.0
zero:   .double  0.0, 0.0

//pi:   .float 0f+3.1415926535897932384626433832795028841971693993751

msg:    .string  "%1.50f\n"

.section .text
        .globl main
        .type main, @function
        .align 64
main:
	      pushq	%rbp
	      movq	%rsp, %rbp

        movdqa  (numer), %xmm2
        movdqa  (denom), %xmm6
        movdqa  (add4), %xmm3
        movdqa  %xmm2, %xmm4
        movdqa  (zero), %xmm5
        movq    $100000000, %r12

loop:
        divpd  %xmm6, %xmm2
        addpd  %xmm2, %xmm5
        movdqa %xmm4, %xmm2
        addpd  %xmm3, %xmm6

        subq $1, %r12
        jnz loop

        movq   $100000000, %r12
        movdqa %xmm5, %xmm0
        movdqa %xmm6, %xmm1
        haddpd %xmm0, %xmm0
        //cvtss2sd dec2, %xmm0

        movq  $1, %rax
        movq $msg, %rdi
        call printf

        movdqa (add4), %xmm3

        jmp loop

        movq $0, %rax
        popq %rbp
        ret

