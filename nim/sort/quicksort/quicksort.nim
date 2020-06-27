import os, random, sequtils, strutils


proc quickSort*[T](a: var openarray[T], inl = 0, inr = -1) =
  var r = if inr >= 0: inr else: a.high
  var l = inl
  let n = r - l + 1
  if n < 2: return
  let p = a[l + 3 * n div 4]
  while l <= r:
    if a[l] < p:
      inc l
      continue
    if a[r] > p:
      dec r
      continue
    if l <= r:
      swap a[l], a[r]
      inc l
      dec r
  quickSort(a, inl, r)
  quickSort(a, l, inr)


let params = commandLineParams()
let last = if params.len > 0:
             parseUInt(params[0])
           else:
             100

var a = (1'u..last).mapIt(rand(int(it))).toSeq()
echo a

quickSort a
echo a
