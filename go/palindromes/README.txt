
Finds the longest palindrome in a string. Compares a naive vs. fast implementation.


Example session :

jgrant@socrates:~/jngmisc/go/palindromes$ ./main_x86_64 100000
Running tests to find longest palindromes (100000 words) ...

Sanity tests ...
Asserting >>>eat a banana bob !<<< in >>>anana ...<<< (Fast)    longest : 'anana ...' with length 5   PASS
Asserting >>>lol<<< in >>>lol ...<<< (Fast)    longest : 'lol ...' with length 3   PASS
Asserting >>><<< in >>> ...<<< (Fast)    longest : ' ...' with length 0   PASS
Asserting >>>A876BC110115438776E0FC16<<< in >>>11011 ...<<< (Fast)    longest : '11011 ...' with length 5   PASS
Asserting >>>AATTCTTTGATTGATAATTTTTTC<<< in >>>TTTTTT ...<<< (Fast)    longest : 'TTTTTT ...' with length 6   PASS
Asserting >>>TTTTTT<<< in >>>TTTTTT ...<<< (Fast)    longest : 'TTTTTT ...' with length 6   PASS
Asserting >>>So my mom and dad said t<<< in >>>tattarrattattattarrattat ...<<< (Fast)    longest : 'tattarrattattattarrattat ...' with length 24   PASS

Big tests (100000) ...
Asserting >>> amanaplanacanalpanama  <<< in >>> amanaplanacanalpanama  amanaplanacanalpanama  ama ...<<< (Slow)    longest : ' amanaplanacanalpanama  amanaplanacanalpanama  ama ...' with length 2300000   PASS
29m26.850507105s elapsed
Asserting >>> amanaplanacanalpanama  <<< in >>> amanaplanacanalpanama  amanaplanacanalpanama  ama ...<<< (Fast)    longest : ' amanaplanacanalpanama  amanaplanacanalpanama  ama ...' with length 2300000   PASS
206.839538ms elapsed
