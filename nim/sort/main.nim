import os, random, sequtils, strutils
import quicksort

let params = commandLineParams()
let last = parseUInt(params[0])

var a = toSeq(1'u..last)
a.shuffle()
echo a

quickSort a
echo a
