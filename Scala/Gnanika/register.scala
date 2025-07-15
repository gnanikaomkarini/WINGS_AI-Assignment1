//> using scala "3.5.0"
//> using dep "com.lihaoyi::requests:0.8.0"
//> using dep "com.lihaoyi::ujson:3.3.1"

import requests.*
import ujson.*
val BASE_URL     = "https://wordle.we4shakthi.in/game/"
val MODE         = "wordle"
val NAME         = "gnanika"
val REGISTER_URL = BASE_URL + "register"
val CREATE_URL   = BASE_URL + "create"
val GUESS_URL    = BASE_URL + "guess"
val HEADERS = Map("Content-Type" -> "application/json")
object WordleClient:

  def main(args: Array[String]): Unit = 
    

    val sess = requests.Session()

    val res = sess.post(
      url     = REGISTER_URL,
      data    = Obj("mode" -> MODE, "name" -> NAME),
      headers = HEADERS
    )

    println(s"Status: ${res.statusCode}")
    println("Response: " + res.text())
    val gameId = ujson.read(res.text())("id").str
    