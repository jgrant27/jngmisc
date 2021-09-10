package main

import (
	"fmt"
	"net/http"
	"encoding/json"
	"io/ioutil"
	"sort"
	"sync"
)

const STORIES_BASE_URL = "https://hacker-news.firebaseio.com/v0"

var mutex sync.Mutex = sync.Mutex{}
var stories = make([]Story, 0)
var wg sync.WaitGroup


func main() {
	resp, _ := http.Get(STORIES_BASE_URL + "/topstories.json")
	body, _ := ioutil.ReadAll(resp.Body)
	var storyIds []int ; json.Unmarshal([]byte(body), &storyIds)

	fmt.Printf("Retrieving %d stories with ids %d ...", len(storyIds), storyIds)

	for i,id := range storyIds {
		wg.Add(1)
		go getStory(&wg, i+1, id)
	}

	wg.Wait()

	sort.Slice(stories, func(i, j int) bool {
		return stories[i].Index < stories[j].Index
	})

	for _,story := range stories {
		fmt.Printf("%d - %d %s %s\n", story.Index, story.ID, story.Title, story.URL)
	}
}

type Story struct {
	Index int
	ID int `json:"id"`
	Title string `json:"title"`
	URL string `json:"url"`
}

func getStory(wg *sync.WaitGroup, n, id int) {
	defer wg.Done()

	resp, _ := http.Get(STORIES_BASE_URL + fmt.Sprintf("/item/%d.json", id))
	body, _ := ioutil.ReadAll(resp.Body)
	var story Story ; json.Unmarshal([]byte(body), &story)
	story.Index = n
	mutex.Lock()
	stories = append(stories, story)
	mutex.Unlock()
}
