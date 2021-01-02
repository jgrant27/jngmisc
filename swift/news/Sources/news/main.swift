import Foundation
import AsyncHTTPClient

let STORIES_BASE_URL = "https://hacker-news.firebaseio.com/v0"
let TOP_STORIES_URL = "\(STORIES_BASE_URL)/topstories.json"

func jsonFromResponse(_ response: HTTPClient.Response) -> Any {
    let bytes = try! response.body.flatMap { $0.getData(at: 0, length: $0.readableBytes) }
    let jsonString = String(bytes: bytes!, encoding: .utf8)!
    return try! JSONSerialization.jsonObject(with: jsonString.data(using: .utf8)!)
}

let client = HTTPClient(eventLoopGroupProvider: .createNew)

let ids = jsonFromResponse(try! client.get(url: TOP_STORIES_URL).wait()) as! [UInt]
print("Loading \(ids.count) news articles ... \n\(ids)")

ids.enumerated().map { (ind, id) in
    return (ind + 1, client.get(url: "\(STORIES_BASE_URL)/item/\(id).json"))
}.forEach { (ind, response) in
    let response = try! response.wait()
    let story = jsonFromResponse(response) as! [String: Any]
    print("\(ind) - \(story["id"]!) - \(story["title"] ?? "NO TITLE") - \(story["url"] ?? "NO URL")")
}
