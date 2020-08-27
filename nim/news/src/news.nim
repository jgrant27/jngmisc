import os, httpClient, json, strformat, strutils, threadpool

const
  STORIES_URL = "https://hacker-news.firebaseio.com/v0/topstories.json"
  ITEM_URL_BASE = "https://hacker-news.firebaseio.com/v0/item"
  DEFAULT_THREAD_CNT = 64
  DEFAULT_STORY_CNT = 1000

type
  Ids = seq[int]
  Stories = seq[string]

var
  ids: Ids
  stories: Stories
  chan: Channel[(int, string)]

setMinPoolSize(DEFAULT_THREAD_CNT)
chan.open()

proc getStory(i, id: int) =

  func get_or_default(node: JsonNode, key: string): string =
    if node.hasKey(key):
      node[key].getStr()
    else:
       &"NO {key} PROVIDED"

  let res_json = newHttpClient().getContent(&"{ITEM_URL_BASE}/{id}.json")
  let story = parseJson(res_json)
  let title = get_or_default(story, "title")
  let url = get_or_default(story, "url")
  chan.send((i, &"{i+1} - {id} - {title} - {url}"))

when isMainModule:
  let args = commandLineParams()
  let story_cnt = if 1 == len(args):
                    parseInt(args[0])
                  else:
                    DEFAULT_STORY_CNT

  let res_json = newHttpClient().getContent(STORIES_URL)
  let all_ids = parseJson(res_json).to(seq[int])
  let story_len = min(len(all_ids), story_cnt)
  stories = newSeq[string](story_len)
  ids = all_ids[all_ids.low..story_len-1]

  echo(&"{len(ids)} stories to get - {ids}\n")

  for i in ids.low..ids.high:
    spawn getStory(i, ids[i])
  sync()

  while true:
    let data = chan.tryRecv()
    if data.dataAvailable:
      stories[data.msg[0]] = data.msg[1]
    else:
      break

  for story in stories:
    echo(story)

  chan.close()
