import sttp.client3._
import sttp.client3.circe._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import java.net.{CookieHandler, CookieManager, CookiePolicy}
import scala.io.Source
import scala.util.Using
import scala.io.StdIn.readLine

object ScalaWordleBot:

  case class RegisterRequest(mode: String = "wordle", name: String) 
  case class RegisterResponse(id: String)
  case class CreateRequest(id: String, overwrite: Boolean = true)
  case class CreateResponse(message: String)
  case class GuessRequest(id: String, guess: String)
  case class GuessResponse(feedback: String, message: String, answer: Option[String])

  val cookieManager = new CookieManager()
  cookieManager.setCookiePolicy(CookiePolicy.ACCEPT_ALL)
  CookieHandler.setDefault(cookieManager)

  val backend = HttpClientSyncBackend.usingClient(
  java.net.http.HttpClient.newBuilder()
    .cookieHandler(cookieManager)
    .build()
  )
  val BaseUrl = uri"https://wordle.we4shakthi.in/game"

  def register(name: String): Option[String] =
    val req = basicRequest
      .post(BaseUrl.addPath("register"))
      .body(RegisterRequest(name = name))
      .contentType("application/json")
      .response(asJson[RegisterResponse])
    req.send(backend).body.toOption.map(_.id)

  def createGame(id: String): Boolean =
    val req = basicRequest
      .post(BaseUrl.addPath("create"))
      .body(CreateRequest(id))
      .contentType("application/json")
      .response(asJson[CreateResponse])
    req.send(backend).body match
      case Right(res) =>
        println(s"Game created: ${res.message}")
        true
      case Left(err) =>
        println(s"Game creation failed: $err")
        false

  def guessAPI(id: String, guess: String): (String, String, Option[String]) =
    val req = basicRequest
      .post(BaseUrl.addPath("guess"))
      .body(GuessRequest(id, guess))
      .contentType("application/json")
      .response(asJson[GuessResponse])
    req.send(backend).body match
      case Right(res) => (res.feedback, res.message, res.answer)
      case Left(_)    => ("EXCEEDED", "You have exceeded the number of allowed guesses.", None)

  def readWords(): List[String] =
    Using(Source.fromFile("medium.txt"))(_.getLines().toList.map(_.trim.toLowerCase).filter(_.length == 5)).getOrElse(Nil)

  def filterWords(words: List[String], guess: String, feedback: String): List[String] =
    val greens = guess.zip(feedback).zipWithIndex.collect {
      case ((c, 'G'), i) => i -> c
    }.toMap
    val yellows = guess.zip(feedback).collect { case (c, 'Y') => c }
    val reds = guess.zip(feedback).collect { case (c, 'R') => c }

    words.filter: word =>
      greens.forall((i, c) => word(i) == c) &&
      yellows.forall(c => word.contains(c)) &&
      reds.diff(yellows).forall(c => !word.contains(c))

  def playGame(playerId: String, words: List[String]): Unit =
    val maxAttempts = 6
    var possible = words
    var attempts = 0
    var won = false

    while attempts < maxAttempts && !won && possible.nonEmpty do
      val guess = possible.head
      println(s"Attempt ${attempts + 1}: $guess")
      val (fb, msg, answer) = guessAPI(playerId, guess)
      println(s"Feedback: $fb")

      if fb == "GGGGG" then
        println(s"\u2705 Correct word guessed: $guess in ${attempts + 1} attempts.")
        won = true
      else if msg.toLowerCase.contains("exceeded") then
        println("\u274C Sorry, you lost.")
        answer.foreach(a => println(s"The correct word was: $a"))
        return
      else
        possible = filterWords(possible.tail, guess, fb)
        attempts += 1

    if !won then println("\u274C Sorry, the bot failed to guess the word.")

@main def runBot(): Unit =
  val words = ScalaWordleBot.readWords()
  if words.isEmpty then
    println("Word list is empty or missing. Please check 'medium.txt'.")
    return

  ScalaWordleBot.register("keerthana") match
    case Some(playerId) =>
      def loop(): Unit =
        if ScalaWordleBot.createGame(playerId) then
          ScalaWordleBot.playGame(playerId, words)
          println("Do you want to play again? (y/n): ")
          if readLine().trim.toLowerCase == "y" then loop()
      loop()
    case None =>
      println("\u274C Registration failed.")
