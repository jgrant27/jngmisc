extern crate serde;

use serde::Deserialize;
use std::sync::{Arc, Mutex};

const STORIES_URL: &str = "https://hacker-news.firebaseio.com/v0/topstories.json";
const ITEM_URL_BASE: &str = "https://hacker-news.firebaseio.com/v0/item";
const THREAD_CNT: usize = 64;
const DEFAULT_STORY_CNT: usize = 100;

#[derive(Deserialize)]
#[allow(dead_code)]
struct Story {
    by: String,
    score: usize,
    time: usize,
    title: String,
    url: String,
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let story_cnt = match args.len() {
        1 => DEFAULT_STORY_CNT,
        _ => match args[1].parse() {
            Ok(n) => n,
            _ => DEFAULT_STORY_CNT,
        },
    };
    let story_ids: Arc<Vec<u64>> = Arc::new(reqwest::get(STORIES_URL).unwrap().json().unwrap());
    let story_ids = story_ids[0..story_cnt].to_vec();
    //story_ids.sort();
    //story_ids.reverse();
    println!("{} stories to get - {:?}\n", story_ids.len(), story_ids);
    let cursor = Arc::new(Mutex::new(0));
    let mut stories_vec: Vec<String> = Vec::new();
    stories_vec.resize(story_cnt, String::new());
    let stories = Arc::new(Mutex::new(stories_vec));
    let mut handles = Vec::new();
    for _ in 0..THREAD_CNT {
        let cursor = cursor.clone();
        let story_ids = story_ids.clone();
        let stories = stories.clone();
        handles.push(std::thread::spawn(move || loop {
            let index = {
                let mut cursor_guard = cursor.lock().unwrap();
                if *cursor_guard >= story_ids.len() {
                    return;
                }
                *cursor_guard += 1;
                *cursor_guard - 1
            };
            let story_url = format!("{}/{}.json", ITEM_URL_BASE, &story_ids[index]);
            let story_res = reqwest::get(&story_url).unwrap().json();
            if story_res.is_ok() {
                let story: Story = story_res.unwrap();
                stories.lock().unwrap()[index] = format!(
                    "{} - {} - {}",
                    &story_ids[index],
                    story.title.trim(),
                    story.url.trim()
                );
            } else {
                stories.lock().unwrap()[index] =
                    format!("{} - Could not deserialize story.", &story_ids[index]);
            }
        }));
    }
    for handle in handles {
        let res = handle.join();
        if res.is_ok() {
            res.unwrap();
        }
    }
    for story in stories.lock().unwrap().iter() {
        print!("{}\n", story);
    }
}
