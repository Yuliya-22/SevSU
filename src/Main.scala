
object Main {
  // Обычная рекурсия
  def cumulativeSumRecursion(list: List[Int]): List[Int] = {
    def helper(remaining: List[Int], acc: Int): List[Int] = remaining match {
      case Nil => Nil
      case head :: tail =>
        val newAcc = acc + head
        newAcc :: helper(tail, newAcc)
    }
    if (list.isEmpty) List()
    else list.head :: helper(list.tail, list.head)
  }

  // Хвостовая рекурсия
  def cumulativeSumTailRecursion(list: List[Int]): List[Int] = {
    @annotation.tailrec
    def helper(remaining: List[Int], acc: Int, result: List[Int]): List[Int] = remaining match {
      case Nil => result.reverse
      case head :: tail =>
        val newAcc = acc + head
        helper(tail, newAcc, newAcc :: result)
    }
    if (list.isEmpty) List()
    else helper(list.tail, list.head, List(list.head))
  }

  // Поиск аргумента ф-ии cumulativeSumRecursion, достаточного для переполнения стека
  def findStackOverflowLength(start: Int): Int = {
    def testLength(length: Int): Int = {
      try {
        val testList = List.fill(length)(1)  // Генерируем список нужной длины
        cumulativeSumRecursion(testList)     // Вызываем проверяемую функцию
        testLength(length + start)           // Увеличиваем длину
      } catch {
        case _: StackOverflowError => length // Возвращаем длину, при которой произошла ошибка
      }
    }
    testLength(start)
  }

  def main(args: Array[String]): Unit = {
    val testList = List(1, 4, 2)
    println(s"Исходный список: $testList")

    val resultRecursion = cumulativeSumRecursion(testList)
    println(s"Список, обработанный с помощью обычной рекурсии: $resultRecursion")

    val resultTailRecursion = cumulativeSumTailRecursion(testList)
    println(s"Список, обработанный с помощью хвостовой рекурсии: $resultTailRecursion")

    val overflowLength = findStackOverflowLength(1000)
    println(s"\nДлина списка, вызывающая переполнение стека: $overflowLength")

    val longList = List.fill(overflowLength)(1)
    try {
      val resultRecursion2 = cumulativeSumRecursion(longList)
      println(s"Обычная рекурсия длинного списка выполнена успешно")
    }
    catch {
      case _: StackOverflowError => println(s"Обычная рекурсия длинного списка вызвала ошибку StackOverflowError")
    }
    try {
      val resultTailRecursion2 = cumulativeSumTailRecursion(longList)
      println(s"Хвостовая рекурсия длинного списка выполнена успешно")
    }
    catch {
      case _: StackOverflowError => println(s"Хвостовая рекурсия длинного списка вызвала ошибку StackOverflowError")
    }
  }
}