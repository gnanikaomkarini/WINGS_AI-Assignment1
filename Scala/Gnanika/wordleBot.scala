//> using scala "3.5.0"
//> using dep "com.lihaoyi::requests:0.9.0"
//> using dep "com.lihaoyi::ujson:4.2.1"

import scala.io.Source
import scala.util.Random

class WordleBot() {
  val instructions = "The computer will guess the word. Feedback is fetched from the API."
  var words: List[String] = Nil
  var possibleWords: List[String] = Nil
  var guess: String = ""
  var attempts: Int = 0
  var status: String = "PLAY"
  var response: String = ""
  val maxAttempts: Int = 6
  val wordSize: Int = 5
  val api = new APICalls()

  def loadWords(filePath: String): Unit = {
    words = Source.fromFile(filePath).getLines().map(_.trim.toLowerCase).filter(_.length == wordSize).toList
    possibleWords = Random.shuffle(words)
    guess = possibleWords.headOption.getOrElse("")
  }

  def removeImpossibleWords(): Unit = {
    val lastGuess = guess
    val lastFeedback = response
    val wordsTail = possibleWords.drop(1)

    val greens = lastGuess.zip(lastFeedback).map { case (c, f) => if (f == 'G') Some(c) else None }
    val ambers = lastGuess.zip(lastFeedback).collect { case (c, f) if f == 'Y' => c }
    val greys  = lastGuess.zip(lastFeedback).collect { case (c, f) if f == 'R' => c }

    def matchGreens(w: String): Boolean =
      w.zip(greens).forall { case (wc, g) => g.forall(_ == wc) || g.isEmpty }

    def matchAmbers(w: String): Boolean =
      ambers.forall(a => w.contains(a)) &&
      lastFeedback.zipWithIndex.forall { case (f, i) => !(f == 'Y' && w(i) == lastGuess(i)) }

    def matchGreys(w: String): Boolean =
      greys.distinct.filterNot(ambers.contains).forall(g => !w.contains(g))

    possibleWords = wordsTail.filter(w => matchGreens(w) && matchAmbers(w) && matchGreys(w))
  }

  def play(): Unit = {
    println(guess)
    val (result, answerOpt) = api.guess(guess)
    response = result
    if (result == "WON") {
      println(s"Wowee! The computer guessed your word in ${attempts + 1} attempts!")
      status = "WON"
    }
    else if (result == "LOST") {
      println("Game over! You have exceeded the number of allowed guesses.")
      println(s"The answer was: ${answerOpt.getOrElse("unknown")}")
      status = "LOST"
    }
    else if (result.length != wordSize || !result.forall("GYR".contains(_))) {
      println("Invalid feedback from API.")
      status = "ERROR"
    }
    else {
      response = result
    }
  }

  def game(): Unit = {
    println(instructions)
    while (status == "PLAY" && possibleWords.nonEmpty && attempts < maxAttempts) {
      play()
      if (status == "WON" || status == "LOST" || status == "ERROR") return
      removeImpossibleWords()
      if (possibleWords.isEmpty) {
        println("No more possible words. Game over.")
        return
      }
      guess = possibleWords.head
      attempts += 1
    }
    if (status != "WON") {
      println("The computer couldn't guess your word in 6 attempts.")
    }
  }
  def wordle(): Unit = {
    api.register()
    var playing = true
    loadWords("medium.txt")
    while (playing) {
      api.create()
      game()
      println("Do you want to play again? (yes/no)")
      val input = scala.io.StdIn.readLine().trim.toLowerCase
      if (input != "yes") {
        playing = false
        println("Thanks for playing!")
      }
      possibleWords = Random.shuffle(words)
      attempts = 0
      status = "PLAY"
      response = ""
    }
  }
}

// --- main method must be outside the class ---
@main def runBot(): Unit = {
  val bot = new WordleBot()
  bot.wordle()
}

