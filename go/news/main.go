package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"
	"sync"
)

const STORIES_BASE_URL = "https://hacker-news.firebaseio.com/v0"
const BUFFER_SIZE = 1000

var ch chan Story = make(chan Story, BUFFER_SIZE)
var storiesText []string = make([]string, BUFFER_SIZE)
var wg sync.WaitGroup

type Story struct {
	Index int
	ID    int    `json:"id"`
	Title string `json:"title"`
	URL   string `json:"url"`
}

func getURL(url string) []byte {
	if resp, err := http.Get(url); err != nil {
		panic(err)
	} else {
		defer resp.Body.Close()
		body, _ := ioutil.ReadAll(resp.Body)
		return body
	}
}

func getStory(n, id int) {
	body := getURL(STORIES_BASE_URL + fmt.Sprintf("/item/%d.json", id))
	var story Story
	json.Unmarshal(body, &story)
	story.Index = n
	ch <- story
}

func putStory() {
	defer wg.Done()
	story := <-ch
	storiesText[story.Index-1] = fmt.Sprintf("%v - %v  %v  %v\n", story.Index, story.ID, story.Title, story.URL)
}

func main() {
	body := getURL(STORIES_BASE_URL + "/topstories.json")
	var storyIds []int
	json.Unmarshal(body, &storyIds)
	fmt.Printf("Retrieving %d stories with ids %d ...\n\n", len(storyIds), storyIds)
	for i, id := range storyIds {
		wg.Add(1)
		go getStory(i+1, id)
		go putStory()
	}
	wg.Wait()
	fmt.Println(strings.Join(storiesText[:], ""))
}
