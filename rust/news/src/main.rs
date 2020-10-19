use threadpool::ThreadPool;
use std::sync::mpsc::channel;
use serde::{Deserialize, Serialize};

const STORIES_URL: &str = "https://hacker-news.firebaseio.com/v0/topstories.json";
const ITEM_URL_BASE: &str = "https://hacker-news.firebaseio.com/v0/item";
const DEFAULT_THREAD_CNT: usize = 64;
const DEFAULT_STORY_CNT: usize = 1000;

fn no_title() -> String { "NO TITLE PROVIDED".to_string() }
fn no_url() -> String { "NO URL PROVIDED".to_string() }

#[derive(Debug, Serialize, Deserialize)]
struct Story {
    id: u64,
    #[serde(default = "no_title")]
    title: String,
    #[serde(default = "no_url")]
    url: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    let mut story_cnt = match args.len() {
        1 => DEFAULT_STORY_CNT,
        _ => args[1].parse()?
    };
    let story_ids: Vec<u64> = reqwest::blocking::get(STORIES_URL)?.json::<Vec<u64>>()?;
    story_cnt = std::cmp::min(story_cnt, story_ids.len());
    let story_ids = story_ids[0..story_cnt].to_vec();
    let mut stories = vec![String::new(); story_cnt];

    println!("{} stories to get - {:?}\n", story_ids.len(), story_ids);

    let pool = ThreadPool::new(DEFAULT_THREAD_CNT);
    let (tx, rx) = channel();

    for (i, id) in story_ids.into_iter().enumerate() {
        let tx = tx.clone();
        pool.execute(move || {
            let story_url = format!("{}/{}.json", ITEM_URL_BASE, &id);
            let story: Story = reqwest::blocking::get(&story_url).unwrap().json::<Story>().unwrap();
            let story_str = format!("{} - {} - {} - {}", i+1, id, story.title, story.url);
            tx.send((i, story_str)).expect("channel should be waiting!");
        });
    }
    for (i, story) in rx.iter().take(story_cnt) {
        stories[i] = story;
    }
    for story in stories {
        print!("{}\n", story);
    }
    println!();

    Ok(())
}
