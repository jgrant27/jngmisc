package main

import (
	"constraints"
	"fmt"
)

func Max[T constraints.Ordered](a, b T) T {
	if a > b {
		return a
	} else {
		return b
	}
}

func main() {
	fmt.Println("Generic Max\n")
	fmt.Printf("Int %d\n", Max(1, 2))
	fmt.Printf("Float %.2f\n", Max(1.0, 2.0))
	fmt.Printf("Binary 0b%b\n", Max(0b1, 0b10))
	fmt.Printf("Hex 0x%X\n", Max(0xA, 0xB))
}
