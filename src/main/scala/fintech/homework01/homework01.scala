package fintech.homework01


// Используя функции io.readLine и io.printLine напишите игру "Виселица"
// Пример ввода и тест можно найти в файле src/test/scala/fintech/homework01/HangmanTest.scala
// Тест можно запустить через в IDE или через sbt (написав в консоли sbt test)

// Правила игры "Виселица"
// 1) Загадывается слово
// 2) Игрок угадывает букву
// 3) Если такая буква есть в слове - они открывается
// 4) Если нет - рисуется следующий элемент висельника
// 5) Последней рисуется "веревка". Это означает что игрок проиграл
// 6) Если игрок все еще жив - перейти к пункту 2

// Пример игры:

// Word: _____
// Guess a letter:
// a
// Word: __a_a
// Guess a letter:
// b
// +----
// |
// |
// |
// |
// |

// и т.д.

class Hangman(io: IODevice) {
  private def getHiddenWord(word: String, guessedLetters: Set[Char]): String ={
    for {letter <- word} yield {
      if (!guessedLetters.contains(letter)) '_' else letter}
  }

  private def printGallows(countMistakes: Int): Unit ={
    if (countMistakes > 0)
      io.printLine(stages(countMistakes - 1))
    if (countMistakes == stages.length)
      io.printLine("You are dead")
  }

  def play(word: String): Unit = {
    var countMistakes: Int = 0
    var guessedLetters: Set[Char] = Set()
    var userWin = false
    while (countMistakes != stages.length && !userWin){
      val hiddenWord = getHiddenWord(word, guessedLetters)
      io.printLine("Word: " + hiddenWord)
      if (hiddenWord == word){
        io.printLine("You WIN!!!")
        userWin = true
      }
      else {
        io.printLine("Guess a letter:")
        val input = io.readLine()
        if (input.length == 1) {
          val guessLetter = input(0).toLower
          if (!guessedLetters.contains(guessLetter) && !word.contains(guessLetter))
            countMistakes += 1
          guessedLetters += guessLetter
          printGallows(countMistakes)
        }
        else io.printLine("Incorrect input")
      }
    }
  }

  val stages = List(
    """+----
      ||
      ||
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  /
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||   |
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin
  )
}

trait IODevice {
  def printLine(text: String): Unit
  def readLine(): String
}
