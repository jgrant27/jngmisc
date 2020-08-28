import os, httpClient, json, strformat, strutils, weave

const
  STORIES_URL = "https://hacker-news.firebaseio.com/v0/topstories.json"
  ITEM_URL_BASE = "https://hacker-news.firebaseio.com/v0/item"
  DEFAULT_THREAD_CNT = 64
  DEFAULT_STORY_CNT = 1000

type
  Ids = seq[int]
  Stories = seq[Flowvar[string]]

var
  ids: Ids
  stories: Stories

putEnv("WEAVE_NUM_THREADS", &"{DEFAULT_THREAD_CNT}")


proc getStory(id: int): string =

  func get_or_default(node: JsonNode, key: string): string =
    if node.hasKey(key):
      node[key].getStr()
    else:
       &"NO {key} FOR STORY"

  let res_json = newHttpClient().getContent(&"{ITEM_URL_BASE}/{id}.json")
  let story = parseJson(res_json)
  let title = get_or_default(story, "title")
  let url = get_or_default(story, "url")
  return  &"{id} - {title} - {url}"


when isMainModule:
  let args = commandLineParams()
  let story_cnt = if 0 == len(args):
                    DEFAULT_STORY_CNT
                  else:
                    parseInt(args[0])
  let res_json = newHttpClient().getContent(STORIES_URL)
  let all_ids = parseJson(res_json).to(seq[int])
  let story_len = min(len(all_ids), story_cnt)
  stories = newSeq[Flowvar[string]](story_len)
  ids = all_ids[low(all_ids)..story_len-1]
  echo(&"Getting {len(ids)} stories ... {ids}\n")

  init(Weave)

  for i in low(ids)..high(ids):
    stories[i] = spawn getStory(ids[i])
  syncRoot(Weave)
  for story in stories:
    echo(sync(story))

  exit(Weave)
