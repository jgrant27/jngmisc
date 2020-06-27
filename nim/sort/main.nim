import os, random, sequtils, strutils
import quicksort

let params = commandLineParams()
let last = parseUInt(params[0])

var a = (1'u..last).mapIt(rand(int(it))).toSeq()
echo a

quickSort a
echo a
