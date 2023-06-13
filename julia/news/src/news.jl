module news

import HTTP, JSON3
using PrecompileMacro

const STORIES_BASE_URL = "https://hacker-news.firebaseio.com/v0"

@precompile function tasks()
    create_task(i, id) = @async (i, get_json("$(STORIES_BASE_URL)/item/$(id).json"))
    get_json(url) = JSON3.read(String(HTTP.request("GET", url, readtimeout=5, retry=true).body))

    story_ids = get_json("$(STORIES_BASE_URL)/topstories.json")
    println("Retrieving $(length(story_ids)) stories with ids $(story_ids) ...")
    results = [create_task(i, id) for (i, id) in enumerate(story_ids)]
    stories = sort(map(t -> fetch(t), results), by=first)
    print_stories(stories)
end

@precompile function channels_tasks()
    ich = Channel(1000)
    och = Channel(1000)

    function get_story(i, url)
        json = JSON3.read(String(HTTP.request("GET", url, readtimeout=5, retry=true).body))
        put!(och, (i, json))
    end

    @precompile function put_work(story_ids)
        @async for (i, id) in enumerate(story_ids)
            put!(ich, (i, "$(STORIES_BASE_URL)/item/$(id).json"))
        end
    end

    @precompile function do_work()
        @async for (i, url) in ich
            @async get_story(i, url)
        end
    end

    @precompile function get_results()
        res = []
        for _ in story_ids
            push!(res, take!(och))
        end
        sort(res, by=first)
    end

    all_stories_url = "$(STORIES_BASE_URL)/topstories.json"
    story_ids = JSON3.read(String(HTTP.request("GET", all_stories_url, readtimeout=5, retry=true).body))
    println("Putting work for $(length(story_ids)) stories into input channel ...")
    put_work(story_ids)
    println("Doing the work in the input channel and putting results into the output channel ...")
    do_work()
    println("Retrieving $(length(story_ids)) stories with ids $(story_ids) ...")
    stories = get_results()
    #stories = sort([take!(och) for _ in story_ids], by=first)
    print_stories(stories)
end

@precompile function print_stories(stories)
    res = ""
    for (i, s) in stories
        res = res *  "$(i) - $(s["id"]) $(s["title"]) $(get(s, "url", "NONE"))\n"
    end
    println(res)
    println()
end

end
