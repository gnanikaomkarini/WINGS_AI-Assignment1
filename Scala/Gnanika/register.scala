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
    val res = sess.post(
      url     = GUESS_URL,
      data    = Obj("id" -> id, "guess" -> guess),
      headers = HEADERS
    )
    val feedback = ujson.read(res.text())("feedback").str
    if feedback == "GGGGG" then
      ("WON", None)
    else if res.statusCode == 200 then
      (feedback, None)
    else if res.statusCode == 422 then
      val answer = ujson.read(res.text())("answer").str
      ("LOST", Some(answer))
    else
      ("ERROR", None)

@main def run(): Unit =
  val api = APICalls()
  api.register()
  if api.create() then
    var result = ""
    var ansOpt: Option[String] = None
    while result != "WON" && result != "LOST" do
      val guess = scala.io.StdIn.readLine("Enter your guess: ")
      val (res, ans) = api.guess(guess)
      result = res
      ansOpt = ans
      println(s"Result: $result" + ansOpt.map(a => s", Answer: $a").getOrElse(""))
    if result == "WON" then
      println("Congratulations! You've won the game!")
    else
      println(s"You lost! The answer was: ${ansOpt.getOrElse("unknown")}")
