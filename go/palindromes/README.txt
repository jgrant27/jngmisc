
Finds the longest palindrome in a string. Compares a naive vs. fast implementation.


Example session :

jgrant@jgrant-XPS-8500:~/jngmisc/go/palindromes$ go build main.go
jgrant@jgrant-XPS-8500:~/jngmisc/go/palindromes$ ls
main  main.go  palindromes  test1E3.txt  test1E4.txt  test1E5.txt  utils
jgrant@jgrant-XPS-8500:~/jngmisc/go/palindromes$ cat test1E3.txt | ./main
longest : ' amanaplanacanalpanama ...' with length 23000 [0:23000] (Naive)
26.500793ms elapsed (Naive)
longest : ' amanaplanacanalpanama ...' with length 23000 [0:23000] (Fast)
972.16us elapsed (Fast)
jgrant@jgrant-XPS-8500:~/jngmisc/go/palindromes$ cat test1E4.txt | ./main
longest : ' amanaplanacanalpanama ...' with length 230000 [0:230000] (Naive)
1.988520904s elapsed (Naive)
longest : ' amanaplanacanalpanama ...' with length 230000 [0:230000] (Fast)
7.004079ms elapsed (Fast)
jgrant@jgrant-XPS-8500:~/jngmisc/go/palindromes$ cat test1E5.txt | ./main
longest : ' amanaplanacanalpanama ...' with length 2300000 [0:2300000] (Naive)
3m17.894771162s elapsed (Naive)
longest : ' amanaplanacanalpanama ...' with length 2300000 [0:2300000] (Fast)
69.369469ms elapsed (Fast)
