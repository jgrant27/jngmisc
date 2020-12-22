package com.imagine27.jngmisc.news

import com.google.gson.*
import io.ktor.client.*; import io.ktor.client.request.*; import io.ktor.http.*
import kotlinx.coroutines.*

val STORIES_BASE_URL = "https://hacker-news.firebaseio.com/v0"

fun main() = runBlocking {
    val (client, gson) = Pair(HttpClient(), Gson())
    val json = client.get<String>("$STORIES_BASE_URL/topstories.json")
    val ids = gson.fromJson(json, JsonArray::class.java)
    val coroutines = ids.mapIndexed { ind, id -> async { getStory(1 + ind, client, gson, id.getAsInt()) } }
    coroutines.forEach { val story = it.await()
                         println("${getStr(story, "ind")} - ${getStr(story, "id")} - ${getStr(story, "title")} - ${getStr(story, "url")}") }
}

suspend fun getStory(ind: Int, client: HttpClient, gson: Gson, id: Int): JsonObject {
    val resp = client.get<String>("$STORIES_BASE_URL/item/$id.json")
    val json = gson.fromJson(resp, JsonObject::class.java)
    json.addProperty("ind", ind.toString())
    return json
}

fun getStr(json: JsonObject, key: String): String {
    return if (null == json[key]) { "NO ${key.toUpperCase()} FOUND" } else { json[key].getAsString() }
}
