import scala.io.StdIn._

var currentTest = 0
var currentAssn = 0
var currentQuiz = 0

def average(input: List[Double]): Double = if (input.isEmpty) 0 else input.sum / input.length

def fullAve(tests: Double, assignments: Double, quizzes: Double): Double = (tests * 0.4) + (assignments * 0.4) + (quizzes * 0.2)

def courseAverage(tests: List[Double], assns: List[Double], quizzes: List[Double]): Double = {
  val aveTest = average(tests)
  val aveAssn = average(assns)
  val aveQuiz = average(quizzes)
  fullAve(aveTest, aveAssn, aveQuiz)
}

def stdv(tests: List[Double], assignments: List[Double], quizzes: List[Double]): Double = {
  val varGrade = varTest(tests) + varAssn(assignments) + varQuiz(quizzes)
  return math.sqrt(varGrade)
}

def square (item: Double): Double = item * item

def varTest (tests: List[Double]): Double = {
  if (tests.isEmpty) {
    0
  } else {
    tests.foreach(square(_ - average(tests))) / tests.length
  }
}

def varAssn (assns: List[Double]): Double = {
  if (assns.isEmpty) {
    0
  } else {
    assns.foreach(square(_ - average(assns))) / assns.length
  }
}

def varQuiz (quizzes: List[Double]): Double = {
  if (quizzes.isEmpty) {
    0
  } else {
    quizzes.foreach(square(_ - average(quizzes))) / quizzes.length
  }
}

def printMenu: Unit = {
  println("Select one of the following options:")
  println("1. Add a test grade.")
  println("2. Add a quiz grade.")
  println("3. Add an assignment grade.")
  println("4. Calculate average.")
  println("5. Calculate standard deviation of all the grades.")
  println("6. Quit")
}

var tests: List[Double] = List()
var quizzes: List[Double] = List()
var assignments: List[Double] = List()

def mainGrades(tests: List[Double], assignments: List[Double], quizzes: List[Double]): Unit = {
  printMenu
  readInt() match {
    case 1 =>
      println("Add a test grade.")
      mainGrades(readDouble() :: tests, assignments, quizzes)
    case 2 =>
      println("Add a quiz grade.")
      mainGrades(tests, readDouble() :: assignments, quizzes)
    case 3 =>
      println("Add an assignment grade.")
      mainGrades(tests, assignments, readDouble() :: quizzes)
    case 4 =>
      println(s"The averege is ${courseAverage(tests, assignments, quizzes)}")
      mainGrades(tests, assignments, quizzes)
    case 5 =>
      println(s"The standard deviation is ${stdv(tests, assignments, quizzes)}")
      mainGrades(tests, assignments, quizzes)
    case 6 =>

    case _ =>
    // Print quit.
    // return the mainGrades function value for the input
  }
}

mainGrades(tests, assignments, quizzes)
