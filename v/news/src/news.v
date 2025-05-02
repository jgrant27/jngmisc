import net.http
import json

const stories_url = 'https://hacker-news.firebaseio.com/v0/topstories.json'
const item_base_url = 'https://hacker-news.firebaseio.com/v0/item'

struct Story {
  id    int
  title string
  url   string
}

struct Cursor {
mut:
  pos int
}

fn main() {
  resp := http.get(stories_url)!
  mut ids := json.decode([]int, resp.body)!
  ids.sort()
  shared cursor := Cursor{}
  mut threads := []thread{}
  shared stories := []Story{}

  println(ids)

  for _ in 0 .. 500 {
    threads << go fn (ids []int, shared cursor Cursor, shared stories []Story) {
      for {
        id := lock cursor {
          if cursor.pos >= ids.len {
            break
          }
          cursor.pos++
          ids[cursor.pos - 1]
        }
        resp := http.get('${item_base_url}/${id}.json') or { panic(err) }
        story := json.decode(Story, resp.body) or { panic(err) }
        stories << story
      }
    }(ids, shared cursor, shared stories)
  }
  threads.wait()

  stories.sort(a.id < b.id)
  mut n := 1
  for story in stories {
    println('$n.str() - ${story.id.str()} - ${story.title} - ${story.url}')
    n++
  }
}
