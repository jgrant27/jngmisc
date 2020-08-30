import io.circe.parser, io.circe.generic.semiauto.deriveDecoder

import java.util.concurrent.Executors

import sttp.client._, sttp.client.circe._, sttp.model._

import scala.concurrent.Channel


object Main {

  final val STORIES_URL = uri"https://hacker-news.firebaseio.com/v0/topstories.json"
  final val ITEM_URL_BASE = "https://hacker-news.firebaseio.com/v0/item"
  final val DEFAULT_THREAD_CNT = 64
  final val DEFAULT_STORY_CNT = 1000

  val pool = java.util.concurrent.Executors.newFixedThreadPool(DEFAULT_THREAD_CNT)
  val channel = new Channel[(Int, Story)]()

  case class Story(id: Int, title: String, url: String)
  implicit val storyDecoder = deriveDecoder[Story]
  implicit val backend = HttpURLConnectionBackend()
  //implicit val backend = CurlBackend()

  def getStory(id: Int): Story = {
    val body = basicRequest.get(uri"$ITEM_URL_BASE/$id.json").response(asJson[Story]).send().body
    body match {
      case Right(json) => json
      case _ => Story(-1, "NO TITLE", "NO URL")
    }
  }

  def main(args: Array[String]): Unit = {

    val body = basicRequest.get(STORIES_URL).response(asJson[List[Int]]).send().body
    var all_ids: List[Int] = body match {
      case Right(lst) => lst
      case _ => List()
    }

    val story_cnt = all_ids.length.min(
      args.length match {
        case 0 => DEFAULT_STORY_CNT
        case n => args(0).toInt
      })

    println(s"${story_cnt} stories to get ... ${all_ids.slice(0, story_cnt)}")

    0 to story_cnt-1 foreach { i =>
      pool.execute(
        new Runnable {
          def run {
            val story = getStory(all_ids(i))
            channel.write((i, story))
          }
        }
      )
    }
    pool.shutdown()

    var stories = new Array[Story](story_cnt)
    0 to story_cnt-1 foreach { i =>
      val (i, story) = channel.read
      stories(i) = story
    }

    0 to story_cnt-1 foreach { i =>
      val story = stories(i)
      println(s"${i + 1} - ${story.id} - ${story.title} - ${story.url}")
    }

  }
}
