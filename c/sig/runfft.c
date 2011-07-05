
//
// Copyright (c) 2010, Justin Grant <justin at imagine27 dot com>
// All rights reserved.

// Redistribution and use in source and binary forms, with or without modification
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
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE
// EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//


#include "complex.h"
#include <math.h>
#include <time.h>

#define MAX_SIZE 1024 * 1024

complex X[MAX_SIZE], Y[MAX_SIZE];


main()

{

  double start, end;
  double time_ms;

  int i,t,n,iterations;
  complex w;
  double pi,theta;


  pi = atan(1.0) * 4.0;

  printf("Enter size:  ");
  scanf("%d", &t);
  n = 1 << t;

  printf("Enter number of iterations:  ");
  scanf("%d", &iterations);
  theta = 2 * pi/n;
  w.r = cos(theta);
  w.i = -sin(theta);

  printf("w_%d = ", n);
  compwrite(w);  printf("\n");

  printf("generating input ...   ");
  for (i = 0; i < n; i++)
    {
      // square wave
      X[i].r = (double)(i % 2);
      //X[i].r = (sin (2 * pi) * i)); // fix amplitude
      printf("%.2f\n", X[i].r);
      //X[i].r = 1.0;
      X[i].i = 0.0;
    }
  printf("done.\n");
  fflush(stdout);

  printf("running fft ...        ");
  fflush(stdout);
  start = clock();
  for (i = 0; i < iterations; i++)
    {
      cfft(n,w,X,Y);
    }
  end = clock();
  time_ms = (((double)(end - start)) / CLOCKS_PER_SEC * 1000) / iterations;
  printf("done.\n");

  printf("time for F_%d :  %.3f(ms) \n", n, time_ms);
  //printf("Y[0] = ");
  //compwrite(Y[0]);
  //printf("\n");

  for (i = 0;i < n; i++)
   {
     printf("Y[%d] = ", i);
     compwrite(Y[i]);  printf("\n");
   }

}
