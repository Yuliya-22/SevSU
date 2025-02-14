
object Lab4 {

  // Определение case-класса для передачи аргументов в фунцию деления
  private case class NumPair(numerator: Int, denominator: Int)
  // Определение case-классов для узлов дерева вычислений
  private sealed trait Expr
  private case class Number(value: Double) extends Expr
  private case class Add(left: Expr, right: Expr) extends Expr
  private case class Subtract(left: Expr, right: Expr) extends Expr
  private case class Multiply(left: Expr, right: Expr) extends Expr
  private case class Divide(left: Expr, right: Expr) extends Expr

  private def dividePairs(pairs: List[(Int, Int)]): List[Option[Double]] = {
    pairs.map {
      case (numerator, denominator) =>
        denominator match {
          case 0 => None
          case _ => Some(numerator.toDouble / denominator)
        }
    }
  }

  private def formatResults(results: List[Option[Double]]): List[String] = {
    results.map {
      case Some(value) => s"Результат деления = $value"
      case None => "Деление на ноль невозможно"
    }
  }

  private def divideNumPairs(pairs: List[NumPair]): List[Option[Double]] = {
    pairs.map {
      case NumPair(numerator, denominator) =>
        denominator match {
          case 0 => None
          case _ => Some(numerator.toDouble / denominator)
        }
    }
  }

  // Функция для преобразования дерева в строку обратной польской нотации
  private def treeToStr(expr: Expr): String = {
    expr match {
      case Number(value) => value.toString
      case Add(left, right) => s"${treeToStr(left)} ${treeToStr(right)} +"
      case Subtract(left, right) => s"${treeToStr(left)} ${treeToStr(right)} -"
      case Multiply(left, right) => s"${treeToStr(left)} ${treeToStr(right)} *"
      case Divide(left, right) => s"${treeToStr(left)} ${treeToStr(right)} /"
    }
  }

  def work(): Unit = {
    println("\n\n--------------------------------------------------------------Лабораторная 4\n")
    val testList = List((4, 2), (3, 0), (10, 5))
    println(s"Исходный список: $testList")
    println("")

    val result = dividePairs(testList)
    println(s"Результаты деления первого числа пары (списка) на второе: $result")
    val formattedResults = formatResults(result)
    println(s"Форматированный вывод: $formattedResults")
    val result2 = divideNumPairs(List(NumPair(4, 2), NumPair(3, 0), NumPair(10, 5)))
    println(s"Результаты деления первого числа пары (класса) на второе: $result2")
    println("")

    val expression = Add(Number(3), Multiply(Number(4), Number(5)))
    val rpn = treeToStr(expression)
    println(s"Дерево, преобразованное в строку: $rpn")
  }
}
