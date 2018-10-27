package fintech.homework05

import com.sun.net.httpserver.Authenticator.Success
import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {
  val storage = new MapStorage()
  val app = new TweetApi(storage)
  val request = CreateTweetRequest("Hello #hello", "nastya")
  val created: Tweet = app.createTweet(request).value

  it should "create Tweet correctly" in {
    created should be(Tweet(created.id, "nastya", "Hello #hello", Seq("#hello"), created.createdAt, 0))
  }

  it should "return correct Tweet" in {
    val r = GetTweetRequest(created.id)
    app.getTweet(r).value should be(created)
  }

  it should "return correct Error if there is less than 1 symbols in the Tweet text" in {
    val r = CreateTweetRequest("", "nastya")
    val c = app.createTweet(r)
    c should be(Error("Length of your Tweet should be more than 0"))
  }

  it should "increase number of likes" in {
    app.likeTweet(LikeRequest(created.id))
    storage.getTweet(created.id).value.likes should be(1)
  }

  it should "return correct Error if there is no tweet with this id to like" in {
    val r = LikeRequest("0")
    val c = app.likeTweet(r)
    c should be(Error("There's no tweet with this id"))
  }

  it should "return correct Error if there is no tweet with this id" in {
    val r = GetTweetRequest("0")
    app.getTweet(r) should be(Error("There's no tweet with this id"))
  }
}
