extern crate serde;

use serde::Deserialize;
use std::sync::{Arc, Mutex};

const STORIES_URL: &str = "https://hacker-news.firebaseio.com/v0/topstories.json";
const ITEM_URL_BASE: &str = "https://hacker-news.firebaseio.com/v0/item";
const THREAD_CNT: usize = 64;

#[derive(Deserialize)]
struct Story {
    title: String,
}

fn main() {
    let story_ids: Arc<Vec<u64>> =
        Arc::new(reqwest::get(STORIES_URL).unwrap().json().unwrap());
    println!("{} stories to get - {:?}", story_ids.len(), story_ids);
    let cursor = Arc::new(Mutex::new(0));
    let mut handles = Vec::new();
    for _ in 0..THREAD_CNT {
        let cursor = cursor.clone();
        let story_ids = story_ids.clone();
        handles.push(std::thread::spawn(move || loop {
            let index = {
                let mut cursor_guard = cursor.lock().unwrap();
                if *cursor_guard >= story_ids.len() {
                    return;
                }
                *cursor_guard += 1;
                *cursor_guard - 1
            };
            let story_url = format!("{}/{}.json", ITEM_URL_BASE, story_ids[index]);
            let story: Story = reqwest::get(&story_url).unwrap().json().unwrap();
            println!("{} - {}", story_ids[index], story.title.trim());
        }));
    }
    for handle in handles {
        handle.join().unwrap();
    }
}