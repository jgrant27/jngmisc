\**\
\* Copyright (c) 2012, Justin Grant <justin at imagine27 dot com> *\
\* All rights reserved. *\
\**\
\* Redistribution and use in source and binary forms, with or without modification,  *\
\* are permitted provided that the following conditions are met: *\
\**\
\* Redistributions of source code must retain the above copyright notice, this list  *\
\* of conditions and the following disclaimer. *\
\* Redistributions in binary form must reproduce the above copyright notice, this  *\
\* list of conditions and the following disclaimer in the documentation and/or  *\
\* other materials provided with the distribution. *\
\* Neither the name of the <ORGANIZATION> nor the names of its contributors may be  *\
\* used to endorse or promote products derived from this software without specific  *\
\* prior written permission. *\
\**\
\* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND  *\
\* ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED *\
\* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE  *\
\* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR  *\
\* ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES  *\
\* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; *\
\* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  *\
\* THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING  *\
\* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,  *\
\* EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. *\
\**\


(tc +)

\*
Depends on the maths library -->
    http://www.shenlanguage.org/library.html
*\

\* Complex numbers *\

(datatype complex
  Real : number; Imag : number;
  =============================
  [Real Imag] : complex;)

(define complex-mult
  {complex --> complex --> complex}
  [R1 I1] [R2 I2] -> [(- (* R1 R2) (* I1 I2))
                      (+ (* R1 I2) (* I1 R2))])

(define complex-add
  {complex --> complex --> complex}
  [R1 I1] [R2 I2] -> [(+ R1 R2) (+ I1 I2)])

(define complex-diff
  {complex --> complex --> complex}
  [R1 I1] [R2 I2] -> [(- R1 R2) (- I1 I2)])


\* Fast Fourier Transform *\

(define butterfly-list
  {((list complex) * ((list complex) * (list complex))) -->
   ((list complex) * ((list complex) * (list complex)))}
  (@p X (@p X1 X2)) -> (if (empty? X)
                           (@p X (@p (reverse X1) (reverse X2)))
                           (butterfly-list
                            (@p (tail (tail X))
                                (@p (cons (head X) X1)
                                    (cons (head (tail X)) X2))))))

\*
(butterfly-list (@p [[0 0] [1 0] [0 0] [1 0] [0 0] [1 0] [0 0] [1 0]
                     [0 0] [1 0] [0 0] [1 0] [0 0] [1 0] [0 0] [1 0]] (@p [] [])))
(@p [] (@p [[0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0] [0 0]]
           [[1 0] [1 0] [1 0] [1 0] [1 0] [1 0] [1 0] [1 0]]))
*\

(define calc-results
  {(((list complex) * (list (list complex))) * ((list complex) * (list complex))) -->
   (((list complex) * (list (list complex))) * ((list complex) * (list complex)))}
  (@p (@p [W WN] [YA YB]) (@p Y1 Y2)) ->
  (if (and (empty? Y1) (empty? Y2))
      (@p (@p [W WN] [(reverse YA) (reverse YB)]) (@p Y1 Y2))
      (calc-results
       (@p (@p [(complex-mult W WN) WN]
               [(cons (complex-add  (head Y1) (complex-mult W (head Y2))) YA)
                (cons (complex-diff (head Y1) (complex-mult W (head Y2))) YB)])
           (@p (tail Y1) (tail Y2))))))

\*
(calc-results (@p (@p [[1 0] [1 0]] [[] []]) (@p [[1 1] [3 3]] [[2 2] [4 4]])))
*\

(define fft
  {number --> complex --> (list complex) --> (list complex) --> (list complex)}
  1 WN X Y -> [(head X)]
  2 WN X Y -> [(complex-add  (head X) (head (tail X)))
               (complex-diff (head X) (head (tail X)))]
  N WN X Y -> (let M   (round (/ N 2))
                   Inp (butterfly-list (@p X (@p [] [])))
                   X1  (fst (snd Inp))
                   X2  (snd (snd Inp))
                   Y1  (fft M (complex-mult WN WN) X1 [])
                   Y2  (fft M (complex-mult WN WN) X2 [])
                   W   [1 0]
                   Res (calc-results (@p (@p [W WN] [[] []]) (@p Y1 Y2)))
                (append (head (snd (fst Res)))
                        (head (tail (snd (fst Res)))))))

\*
(fft 16 [0.923880 -0.382683]
     [[0 0] [1 0] [0 0] [1 0] [0 0] [1 0] [0 0] [1 0]
      [0 0] [1 0] [0 0] [1 0] [0 0] [1 0] [0 0] [1 0]] [])
*\

(define dotimes-fft
  {number --> number --> complex --> (list complex) --> (list complex)
          --> (list complex)}
  Iterations Size W Input Res ->
  (if (<= Iterations 0)
      Res
      (dotimes-fft (- Iterations 1) Size W Input (fft Size W Input []))))

(define run-fft
  {number --> number --> (list complex) --> (list complex)}
  Iterations Size Input -> (let Pi    (* 4 (atan 1))
                                Theta (* 2 (/ Pi Size))
                                W     [(cos Theta) (* -1 (sin Theta))]
                             (dotimes-fft Iterations Size W Input [])))

\* Square wave test *\
\*
(26-) (time (run-fft 1000 16 [[0 0] [1 0] [0 0] [1 0] [0 0] [1 0] [0 0] [1 0]
                              [0 0] [1 0] [0 0] [1 0] [0 0] [1 0] [0 0] [1 0]]))

Evaluation took:
2.999 seconds of real time
2.942718 seconds of total run time (2.798716 user, 0.144002 system)
[ Run times consist of 0.371 seconds GC time, and 2.572 seconds non-GC time. ]
98.13% CPU
6,282,874,678 processor cycles
1,641,619,888 bytes consed

[[8 0] [0.0 0.0] [0.0 0.0] [0.0 0.0] [0.0 0.0] [0.0 0.0] [0.0 0.0] [0.0 0.0]
 [-8 0] [0.0 0.0] [0.0 0.0] [0.0 0.0] [0.0 0.0] [0.0 0.0] [0.0 0.0] [0.0 0.0]] : (list complex)

(27-)
*\

\* Saw-tooth wave *\
\*
(21-) (time (run-fft 1000 16 [[1 0] [2 0] [3 0] [0 0] [1 0] [2 0] [3 0] [0 0]
                              [1 0] [2 0] [3 0] [0 0] [1 0] [2 0] [3 0] [0 0]]))

Evaluation took:
0.360 seconds of real time
0.349369 seconds of total run time (0.323640 user, 0.025729 system)
[ Run times consist of 0.036 seconds GC time, and 0.314 seconds non-GC time. ]
96.94% CPU
752,531,787 processor cycles
164,151,984 bytes consed

[[24 0] [0.0 0.0] [0.0 0.0] [0.0 0.0] [-8.000001 -8.0] [0.0 0.0] [0.0 0.0]
 [0.0 0.0] [8 0] [0.0 0.0] [0.0 0.0] [0.0 0.0] [-7.999999 8.0] [0.0 0.0]
 [0.0 0.0] [0.0 0.0]] : (list complex)
*\
