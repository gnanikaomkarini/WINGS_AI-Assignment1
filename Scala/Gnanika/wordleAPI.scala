//> using scala "3.5.0"
//> using dep "com.lihaoyi::requests:0.9.0"
//> using dep "com.lihaoyi::ujson:4.2.1"

import requests.*
import ujson.*

class APICalls:
  val name: String = "gnanika"
  val mode: String = "wordle"
  val BASE_URL     = "https://wordle.we4shakthi.in/game/"
  val REGISTER_URL = BASE_URL + "register"
  val CREATE_URL   = BASE_URL + "create"
  val GUESS_URL    = BASE_URL + "guess"
  val HEADERS      = Map("Content-Type" -> "application/json")
  val sess         = requests.Session()
  var id: String   = ""

  def register(): String =
    val res = sess.post(
      url     = REGISTER_URL,
      data    = Obj("mode" -> mode, "name" -> name),
      headers = HEADERS
    )
    println(ujson.read(res.text())("message").str)
    id = ujson.read(res.text())("id").str
    id

  def create(): Boolean =
    val res = sess.post(
      url     = CREATE_URL,
      data    = Obj("id" -> id, "overwrite" -> true),
      headers = HEADERS
    )
    println(ujson.read(res.text())("message").str)
    res.statusCode == 201

  def guess(guess: String): (String, Option[String]) =
    try
      val res = sess.post(
        url     = GUESS_URL,
        data    = Obj("id" -> id, "guess" -> guess),
        headers = HEADERS,
        check   = false  // ✅ disables automatic throwing of exceptions
      )

      val json = ujson.read(res.text())

      res.statusCode match
        case 200 =>
          val feedback = json("feedback").str
          val answerOpt = json.obj.get("answer").map(_.str)
          if feedback == "GGGGG" then
            ("WON", answerOpt)
          else
            (feedback, None)

        case 422 =>
          val answer = json.obj.get("answer").map(_.str).getOrElse("unknown")
          ("LOST", Some(answer))

        case _ =>
          println(s"Unexpected response (${res.statusCode}): ${json.obj.get("message").map(_.str).getOrElse("Unknown error")}")
          ("ERROR", None)
    catch
      case e: Exception =>
        println(s"Exception during guess(): ${e.getMessage}")
        ("ERROR", None)
/*
@main def run(): Unit =
  val api = APICalls()
  api.register()
  if api.create() then
    var result = ""
    var ansOpt: Option[String] = None

    while result != "WON" && result != "LOST" && result != "ERROR" do
      val guess = scala.io.StdIn.readLine("Enter your guess: ")
      val (res, ans) = api.guess(guess)
      println(s"Result: $res" + ans.map(a => s", Answer: $a").getOrElse(""))
      result = res
      ansOpt = ans

    result match
      case "WON" =>
        println("🎉 Congratulations! You've won the game!")
      case "LOST" =>
        println(s"😢 You lost! The answer was: ${ansOpt.getOrElse("unknown")}")
      case "ERROR" =>
        println("⚠️  Something went wrong during the game.")
*/