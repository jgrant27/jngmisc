//
// Copyright (c) 2009, Justin Grant <justin at imagine27 dot com>
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


import java.math.BigDecimal;
import static java.math.BigDecimal.*;

class Pi {
  private static final BigDecimal TWO = new BigDecimal(2);
  private static final BigDecimal FOUR = new BigDecimal(4);
  private static int ROUND_MODE = ROUND_DOWN;

  public static void main(String[] args) {
    long start = System.nanoTime();
    System.out.println(pi(Integer.parseInt(args[0])));
    System.out.println("Elapsed time: " + 
                       ((System.nanoTime() - start) / 1E6) + " msecs");
  }
    
  // Salamin-Brent Algorithm
  public static BigDecimal pi(final int digits) {
    final int SCALE = 10 + digits;
    BigDecimal a = ONE;
    BigDecimal b = ONE.divide(sqrt(TWO, SCALE), SCALE, ROUND_MODE);
    BigDecimal t = new BigDecimal(0.25);
    BigDecimal x = ONE;
    BigDecimal y;
        
    while (!a.equals(b)) {
      y = a;
      a = a.add(b).divide(TWO, SCALE, ROUND_MODE);
      b = sqrt(b.multiply(y), SCALE);
      t = t.subtract(x.multiply(y.subtract(a).multiply(y.subtract(a))));
      x = x.multiply(TWO);
    }
        
    return a.add(b)
      .multiply(a.add(b))
      .divide(t.multiply(FOUR), SCALE, ROUND_MODE)
      .setScale(digits, ROUND_MODE);
  }
    
  // square root method (Newton's)
  public static BigDecimal sqrt(BigDecimal A, final int SCALE) {
    BigDecimal x0 = new BigDecimal("0");
    BigDecimal x1 = new BigDecimal(Math.sqrt(A.doubleValue()));
        
    while (!x0.equals(x1)) {
      x0 = x1;
      x1 = A.divide(x0, SCALE, ROUND_MODE);
      x1 = x1.add(x0);
      x1 = x1.divide(TWO, SCALE, ROUND_MODE);
    }
        
    return x1;
  }
}
