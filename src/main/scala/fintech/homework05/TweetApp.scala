package fintech.homework05

import java.time.Instant
import java.util.UUID

import com.sun.net.httpserver.Authenticator.Success

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Result[T]
  * в котором может лежать либо текст ошибки, либо результат выполнение
  */

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int)

case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)

trait TweetStorage {

  def saveTweet(tweet: Tweet): Result[Tweet]

  def getTweet(id: String): Result[Tweet]

  def likeTweet(tweet: Tweet): Result[Int]
}

class MapStorage(private var storage: Map[String, Tweet] = Map.empty) extends TweetStorage {

    def saveTweet(tweet: Tweet): Result[Tweet] = storage.get(tweet.id) match {
      case Some(_) => Error("Tweet with this id already exists")
      case None =>
        storage = storage.updated(tweet.id, tweet)
        Success(tweet)
    }

    def getTweet(id: String): Result[Tweet] = storage.get(id) match {
      case Some(t) => Success(t)
      case None => Error("There's no tweet with this id")
    }

    private def deleteTweet(id: String): Result[Tweet] = {
      val tweet = storage.get(id)
      tweet match{
        case Some(t) =>
          storage = storage.filter(p => p._1 != id)
          Success(t)
        case None => Error("There's no tweet with this id")
      }
    }

    def likeTweet(tweet: Tweet): Result[Int] = {
      val t = Tweet(tweet.id,
        tweet.user,
        tweet.text,
        tweet.hashTags,
        tweet.createdAt,
        tweet.likes + 1)
      deleteTweet(tweet.id) match {
        case Error(message) => Error(message)
        case Success(_) =>
          saveTweet(t)
          Success(tweet.likes + 1)
      }
    }
}

  class TweetApi(storage: TweetStorage) {
    private def getHashTags(text: String): Seq[String] =
      text.split(" ").filter(p => p.length > 0 && p.charAt(0) == '#')

    def createTweet(request: CreateTweetRequest): Result[Tweet] = request match {
      case CreateTweetRequest(text, user) if text.length < 1 => Error("Length of your Tweet should be more than 0")
      case CreateTweetRequest(text, user) => storage.saveTweet(Tweet(UUID.randomUUID.toString,
        user,
        text,
        getHashTags(text),
        Some(Instant.now()),
        0))
    }

    def getTweet(request: GetTweetRequest): Result[Tweet] = storage.getTweet(request.id)

    def likeTweet(request: LikeRequest): Result[Int] = storage.getTweet(request.id) match {
      case Error(message) => Error(message)
      case Success(tweet: Tweet) => storage.likeTweet(tweet)
    }
  }

  sealed trait Result[+T]{
    def value: T = this match {
      case Success(v) => v
    }
  }
  final case class Success[T](v: T) extends Result[T]
  final case class Error[T](v: String) extends Result[T]


object TweetApiExample extends App {

  val storage: TweetStorage = new MapStorage()
  val app = new TweetApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val response = app.createTweet(request)
  response match {
    case Success(value) => println(s"Created tweet with id: ${value.id}")
    case Error(message) => println(s"Failed to create tweet: $message")
  }

}
