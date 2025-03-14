use futures::{stream, StreamExt};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::str;
use tokio;

const STORIES_URL: &str = "https://hacker-news.firebaseio.com/v0/topstories.json";
const ITEM_URL_BASE: &str = "https://hacker-news.firebaseio.com/v0/item";
const DEFAULT_STORY_CNT: usize = 1000;

fn no_n() -> u64 {
    0
}

fn no_title() -> String {
    "NO TITLE PROVIDED".to_string()
}
fn no_url() -> String {
    "NO URL PROVIDED".to_string()
}

#[derive(Debug, Serialize, Deserialize)]
struct Story {
    #[serde(default = "no_n")]
    n: u64,
    id: u64,
    #[serde(default = "no_title")]
    title: String,
    #[serde(default = "no_url")]
    url: String,
}

#[tokio::main]
async fn main() {
    let client = Client::new();
    let story_ids: Vec<u64> = client
        .get(STORIES_URL)
        .send()
        .await
        .unwrap()
        .json::<Vec<u64>>()
        .await
        .unwrap();
    let story_cnt = std::cmp::min(DEFAULT_STORY_CNT, story_ids.len());
    let story_ids = story_ids[0..story_cnt].to_vec();

    println!("{} stories to get - {:?}\n", story_ids.len(), story_ids);
    let mut n = 0;
    let stories = stream::iter(story_ids)
        .map(|id| {
            let client = &client;
            n += 1;
            async move {
                let surl = format!("{}/{}.json", ITEM_URL_BASE, &id);
                let story_url = surl.as_str();
                let resp = client.get(story_url).send().await.unwrap();
                let b = resp.bytes().await;
                let sl = b.unwrap().slice(..);
                let mut story: Story = serde_json::from_slice(&sl).unwrap();
                story.n = n;
                story
            }
        })
        .buffered(DEFAULT_STORY_CNT);

    stories
        .for_each(|story| async move {
            let story_str = format!(
                "{} - {} - {} - {}",
                story.n, story.id, story.title, story.url
            );
            println!("{}", story_str);
        })
        .await;

    println!();
}
