import scala.util.Random

object Lab5 {

  // Функция, возвращающая количество собственных вызовов
  private def countSelfCalls(n: Int): Int = {
    if (n <= 0) 0
    else 1 + countSelfCalls(n - 1)
  }

  // Функция перевода десятичного числа в n-ичное представление
  private def decimalToNBase(num: Int, base: Int): String = {
    if (num == 0) "0"
    else {
      @scala.annotation.tailrec
      def convert(n: Int, acc: String): String = {
        if (n == 0) acc
        else convert(n / base, (n % base).toString + acc)
      }
      // Проверка на случаи, когда основание больше числа
      if (base > num) {
        if (num < 10) num.toString
        else ('A' + (num - 10)).toChar.toString // Преобразуем в букву
      }
      else { convert(num, "") }
    }
  }

  // Каррированная версия для двоичной системы
  private def decimalToBinary: Int => String = decimalToNBase(_, 2)

  // Функция, возвращающая случайный список
  private def randomListGenerator(Z: Int): Int => List[Int] = {
    val random = new Random()
    length => {
      (1 to length).toList.map {_ =>
        val randomNum = Z + random.nextInt(11) - 5  // число от (Z-5) до (Z+5)
        randomNum
      }
    }
  }

  def work(): Unit = {
    println("\n\n--------------------------------------------------------------Лабораторная 5\n")
    val calls = countSelfCalls(5)
    println(s"Количество собственных вызовов: $calls")
    println("")

    val number = 18
    val base = 16
    val result = decimalToNBase(number, base)
    println(s"$base-ичное представление числа dec($number): $result")
    val result2 = decimalToBinary(number)
    println(s"Двоичное представление числа dec($number): $result2")
    println("")

    val length = 4
    val generateList = randomListGenerator(number)
    val randomList = generateList(length)
    println(s"Случайный список чисел $number +-5 с количеством элементов $length: $randomList")
  }
}
