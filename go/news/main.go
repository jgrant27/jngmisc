package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
)

const STORIES_BASE_URL = "https://hacker-news.firebaseio.com/v0"

var ch = make(chan Story)
var stories []Story

type Story struct {
	Index int
	ID    int    `json:"id"`
	Title string `json:"title"`
	URL   string `json:"url"`
}

func handleError(err error) {
	if err != nil {
		panic(err)
	}
}

func getURL(url string) []byte {
	resp, err := http.Get(url)
	handleError(err)
	defer resp.Body.Close()
	body, err := ioutil.ReadAll(resp.Body)
	handleError(err)
	return body
}

func getStory(n, id int) {
	body := getURL(STORIES_BASE_URL + fmt.Sprintf("/item/%d.json", id))
	var story Story
	json.Unmarshal(body, &story)
	story.Index = n
	ch <- story
}

func main() {
	body := getURL(STORIES_BASE_URL + "/topstories.json")
	var storyIds []int
	json.Unmarshal(body, &storyIds)
	fmt.Printf("Retrieving %d stories with ids %d ...\n\n", len(storyIds), storyIds)
	stories = make([]Story, len(storyIds))
	for i, id := range storyIds {
		go getStory(i+1, id)
	}
	for i := 0; i < len(storyIds); i++ {
		story := <-ch
		stories[story.Index-1] = story
	}
	for _, story := range stories {
		fmt.Printf("%d - %d %s %s\n", story.Index, story.ID, story.Title, story.URL)
	}
}
