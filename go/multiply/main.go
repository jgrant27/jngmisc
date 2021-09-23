package main
import (
  "fmt"
  "math"
  "os"
)

func main() {
  n1 := os.Args[1]
  n2 := os.Args[2]

  fmt.Println(myToInt(n1) * myToInt(n2))
}

func myToInt(ns string) int {
  ni := 0
  for i:=0; i<len(ns); i++ {
	n := int(math.Pow(10, float64(len(ns)-i-1))) * int(ns[i]-48)
	ni += n
  }
  return ni
}
