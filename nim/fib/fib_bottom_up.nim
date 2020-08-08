# O(n) time complexity
# O(1) space complexity

func fib(n: uint64): uint64 =
  var (pfib, cfib) = (0'u64, 1'u64)
  if 0 == n:
    pfib
  else:
    for i in 2..n: (pfib, cfib) = (cfib, pfib + cfib)
    cfib

let results = [(0'u64, 0'u64),
               (1'u64, 1'u64),
               (1'u64, 2'u64),
               (2'u64, 3'u64),
               (3'u64, 4'u64),
               (5'u64, 5'u64),
               (233'u64, 13'u64),
               (6765'u64, 20'u64),
               (832040'u64, 30'u64),
               (102334155'u64, 40'u64),
               (12586269025'u64, 50'u64),
               # largest fib num that will fit into a uint64
               (7540113804746346429'u64, 92'u64)]

for pair in results:
  echo pair[1], " ", pair[0]
  assert fib(pair[1]) == pair[0]
