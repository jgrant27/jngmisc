package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"sort"
	"strings"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Usage: ./triplet_finder <filename>")
		return
	}

	file, err := os.Open(os.Args[1])
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return
	}
	defer file.Close()

	// Regex to keep only alphanumeric characters and spaces
	reg, _ := regexp.Compile("[^a-zA-Z0-9 ]+")
	counts := make(map[string]int)
	var words []string

	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanWords)

	for scanner.Scan() {
		// Ignore case and clean punctuation
		word := strings.ToLower(reg.ReplaceAllString(scanner.Text(), ""))
		if word == "" {
			continue
		}
		words = append(words, word)

		// Sliding window for 3-word triplets
		if len(words) >= 3 {
			triplet := strings.Join(words[len(words)-3:], " ")
			counts[triplet]++
		}
	}

	// Find the longest triplet (by character count)
	var longest string
	maxLen := 0
	for triplet := range counts {
		if len(triplet) > maxLen {
			maxLen = len(triplet)
			longest = triplet
		}
	}

	// Sort by frequency for the "Top 5"
	type kv struct {
		Key   string
		Value int
	}
	var sortedCounts []kv
	for k, v := range counts {
		sortedCounts = append(sortedCounts, kv{k, v})
	}
	sort.Slice(sortedCounts, func(i, j int) bool {
		return sortedCounts[i].Value > sortedCounts[j].Value
	})

	fmt.Printf("Longest Triplet: \"%s\" (%d chars)\n\n", longest, maxLen)
	fmt.Println("Top 5 Most Frequent Triplets:")
	for i := 0; i < 5 && i < len(sortedCounts); i++ {
		fmt.Printf("%d. \"%s\": %d occurrences\n", i+1, sortedCounts[i].Key, sortedCounts[i].Value)
	}
}
