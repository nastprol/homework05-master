package fintech.homework05

import com.sun.net.httpserver.Authenticator.Success
import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {
  val storage = new MapStorage()
  val app = new TweetApi(storage)
  val request = CreateTweetRequest("Hello #hello", "nastya")
  val created = app.createTweet(request) match {
    case Success(t: Tweet) => t
    case Error(msg) => Error(msg)
  }
  val createdTweet: Tweet = created.asInstanceOf[Tweet]

  it should "create Tweet correctly" in {
    createdTweet should be(Tweet(createdTweet.id, "nastya", "Hello #hello", Seq("#hello"), createdTweet.createdAt, 0))
  }

  it should "return correct Tweet" in {
    val r = GetTweetRequest(createdTweet.id)

    app.getTweet(r) should be(Success(createdTweet))
  }

  it should "return correct Error if there is less than 1 symbols in the Tweet text" in {
    val r = CreateTweetRequest("", "nastya")
    val c = app.createTweet(r)
    c should be(Error("Length of your Tweet should be more than 0"))
  }

  it should "increase number of likes" in {
    val r = CreateTweetRequest("Hello #hello", "nastya")
    val c = app.createTweet(r) match {
      case Success(t: Tweet) => t
      case Error(msg) => Error(msg)
    }
    val ct: Tweet = c.asInstanceOf[Tweet]

    app.likeTweet(LikeRequest(ct.id))
    val liked = storage.getTweet(ct.id) match {
      case Success(t: Tweet) => t
      case Error(msg) => Error(msg)
    }

    val likedTweet = liked.asInstanceOf[Tweet]
    likedTweet.likes should be(1)
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
