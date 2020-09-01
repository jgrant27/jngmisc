import java.net.http.{HttpClient, HttpResponse, HttpRequest}
import scala.collection.JavaConverters._
import scala.language.implicitConversions
import scala.concurrent.{duration, ExecutionContext, Await, Future}

final val STORIES_BASE_URL = "https://hacker-news.firebaseio.com/v0"

def getURL(url: String) =
  val request = HttpRequest.newBuilder().uri(java.net.URI.create(url)).build()
  HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString())

def getStory(i: Int, id: Int) =
  val response = getURL(s"${STORIES_BASE_URL}/item/${id}.json")
  val story_map = org.json.JSONObject(response.body)
  (i, id, story_map.optString("title", "NO TITLE FOUND"),
          story_map.optString("url", "NO URL FOUND"))

@main
def main(args: Int*) =
  val response = getURL(s"${STORIES_BASE_URL}/topstories.json")
  val all_ids = org.json.JSONArray(response.body)
    .asScala.toList.asInstanceOf[List[Int]]
  val story_cnt =
    all_ids.length.min(args.length match { case 1 => args(0) case _ => 1000 })
  println(s"${story_cnt} stories to get ... ${all_ids.slice(0, story_cnt)}\n")
  implicit val ec = ExecutionContext.global
  val futures = all_ids.slice(0, story_cnt).zipWithIndex.map {
    case (id, i) => Future { getStory(i, id) } }
  for fut <- futures do
    val story = Await.result(fut, duration.Duration(10, "seconds"))
    println(s"${story(0) + 1} - ${story(1)} - ${story(2)} - ${story(3)}")
