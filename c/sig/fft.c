
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
#include <stdlib.h>

void cfft(n,wn,X,Y)
/************************************************************************
  Fast fourier transform.
  n = 2^k
  Y = F_n X
  wn is a primitive nth root of unity.
************************************************************************/

int n;
complex wn;
complex X[],Y[];

{
  int m,j;
  complex w;
  complex *X1, *X2, *Y1, *Y2;
  
  
  /* Base case.  */
  
  if (n == 1)
    {
      Y[0] = X[0];
      return;
    }
  
  if (n == 2)
    {
      Y[0] = compadd(X[0],X[1]);
      Y[1] = compdif(X[0],X[1]);
      return;
    }
  
  /* X1,X2 = L^{n}_2 X; */
  
  m = n/2;
  
  X1 = (complex *) malloc(m*sizeof(complex));
  X2 = (complex *) malloc(m*sizeof(complex));
  Y1 = (complex *) malloc(m*sizeof(complex));
  Y2 = (complex *) malloc(m*sizeof(complex));
  
  for (j = 0; j < m; j++)
    {
      X1[j] = X[2*j];
      X2[j] = X[2*j+1];
    }

  cfft(m,compmult(wn,wn),X1,Y1);
  cfft(m,compmult(wn,wn),X2,Y2);
  
  w.r = 1.0;
  w.i = 0.0;
  
  for (j=0;j<m;j++)
    {
      Y[j] = compadd(Y1[j],compmult(w,Y2[j]));    /*  Y[j] = Y1[j] + w^i Y2[j] */
      Y[j+m] = compdif(Y1[j],compmult(w,Y2[j]));  /*  Y[j+m] = Y1[j] - w^i Y2[j] */
      w = compmult(w,wn);
  }
  free(X1);
  free(X2);
  free(Y1);
  free(Y2);
}
