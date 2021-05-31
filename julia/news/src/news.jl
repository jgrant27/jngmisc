module news

import HTTP, JSON

const STORIES_BASE_URL = "https://hacker-news.firebaseio.com/v0"

function tasks()
    create_task(i, id) = @async (i, get_json("$(STORIES_BASE_URL)/item/$(id).json"))
    get_json(url) = JSON.parse(String(HTTP.request("GET", url, readtimeout=5, retry=true).body))

    story_ids = get_json("$(STORIES_BASE_URL)/topstories.json")
    println("Retrieving $(length(story_ids)) stories with ids $(story_ids) ...")
    results = [create_task(i, id) for (i, id) in enumerate(story_ids)]
    stories = sort(map(t -> fetch(t), results), by=first)
    print_stories(stories)
end

function channels()
    input = Channel(1000)
    output = Channel(1000)

    get_story(i, url) = begin
        json = JSON.parse(String(HTTP.request("GET", url, readtimeout=5, retry=true).body))
        @async put!(output, (i, json))
    end

    do_work(story_ids) = begin
        for (i, id) in enumerate(story_ids)
            @async put!(input, get_story(i, "$(STORIES_BASE_URL)/item/$(id).json"))
        end
    end

    all_stories_url = "$(STORIES_BASE_URL)/topstories.json"
    story_ids = JSON.parse(String(HTTP.request("GET", all_stories_url, readtimeout=5, retry=true).body))
    @async do_work(story_ids)
    println("Retrieving $(length(story_ids)) stories with ids $(story_ids) ...")
    stories = sort([take!(output) for _ in story_ids], by=first)
    print_stories(stories)
end

function print_stories(stories)
    res = ""
    for (i, s) in stories
        res = res *  "$(i) - $(s["id"]) $(s["title"]) $(get(s, "url", "NONE"))\n"
    end
    println(res)
    println()
end

end
