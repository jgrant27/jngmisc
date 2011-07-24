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

complex compmult(a, b)
complex a, b;
{
  complex c;
  c.r = a.r * b.r - a.i * b.i;
  c.i = a.r * b.i + a.i * b.r;
  return c;
}

complex compmulti(complex a)
{
  complex c;
  c.r = -a.i;
  c.i = a.r;
  return c;
}


complex compadd(a,b)
complex a, b;
{
  complex c;
  c.r = a.r + b.r;
  c.i = a.i + b.i;
  return c;
}

complex compdif(a,b)
complex a, b;
{
  complex c;
  c.r = a.r - b.r;
  c.i = a.i - b.i;
  return c;
}

complex compread()
{
  complex c;
  scanf("%lf",&c.r);
  scanf("%lf",&c.i);
  return c;
}

void compwrite(c)
complex c;
{
  printf("%f + i%f", c.r, c.i);
}
