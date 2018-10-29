package fintech.homework05

import java.time.Instant

import com.sun.net.httpserver.Authenticator.Success
import org.scalatest.{FlatSpec, Matchers}

class TweetStorageSpec extends FlatSpec with Matchers {
  val storage = new MapStorage()
  val tweet = Tweet("0",
    "nastya",
    "Hello #hello",
    Seq("#hello"),
    Some(Instant.now),
    0)

  it should "save Tweet correctly" in {
    storage.saveTweet(tweet) should be(Success(tweet))
  }

  it should "get Tweet with id correctly" in {
    storage.getTweet(tweet.id) should be(Success(tweet))
  }

  it should "return correct Error if there is no Tweet with this ID" in {
    storage.getTweet("1") should be(Error("There's no tweet with this id"))
  }

  it should "return correct Error if there is already Tweet with this ID" in {
    storage.saveTweet(tweet) should be(Error("Tweet with this id already exists"))
  }

  it should "like Tweet correctly" in {
    storage.updateTweet(tweet.copy(likes = tweet.likes + 1)) should be(Success(tweet.copy(likes = tweet.likes + 1)))
  }

  it should "return correct Error if there is no Tweet with this ID to like" in {
    val otherTweet = Tweet("1",
      "nastya",
      "Hello #hello",
      Seq("#hello"),
      Some(Instant.now),
      0)
    storage.updateTweet(otherTweet) should be(Error("There's no tweet with this id"))
  }
}
