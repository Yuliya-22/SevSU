
object Lab1 {
  // Обычная рекурсия
  private def cumulativeSumRecursion(list: List[Int]): List[Int] = {
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
  private def cumulativeSumTailRecursion(list: List[Int]): List[Int] = {
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
  private def findStackOverflowLength(start: Int): Int = {
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

  def work(): Unit = {
    println("\n--------------------------------------------------------------Лабораторная 1\n")
    val testList = List(1, 4, 2)
    println(s"Исходный список: $testList")
    val resultRecursion = cumulativeSumRecursion(testList)
    println(s"Список, обработанный с помощью обычной рекурсии: $resultRecursion")
    val resultTailRecursion = cumulativeSumTailRecursion(testList)
    println(s"Список, обработанный с помощью хвостовой рекурсии: $resultTailRecursion")
    println("")

    val overflowLength = findStackOverflowLength(1000)
    println(s"Длина списка, вызывающая переполнение стека: $overflowLength")
    val longList = List.fill(overflowLength)(1)
    try {
      val resultRecursion2 = cumulativeSumRecursion(longList)
      println(s"Обычная рекурсия длинного списка выполнена успешно")
    }
    catch { case _: StackOverflowError => println(s"Обычная рекурсия длинного списка вызвала ошибку StackOverflowError") }
    try {
      val resultTailRecursion2 = cumulativeSumTailRecursion(longList)
      println(s"Хвостовая рекурсия длинного списка выполнена успешно")
    }
    catch { case _: StackOverflowError => println(s"Хвостовая рекурсия длинного списка вызвала ошибку StackOverflowError") }
  }
}

object Application {

  private type Student = (
    Int,    // ID
      String, // Имя
      Int,    // Год рождения
      String, // Факультет
      Char,   // Пол
      Int,    // Курс
      Boolean // Проживает ли в общежитии
    )

  private type Room = (
    Int,      // Номер комнаты
    Int,      // Вместимость комнаты
    List[Int] // ID студентов, проживающих в комнате
  )

  private val students: List[Student] = List(
    (0, "Алёна", 2006, "FIL", 'F', 1, true),
    (1, "Гриша", 2005, "AVT", 'M', 2, true),
    (2, "Настя", 2004, "MTS", 'F', 3, false),
    (3, "Коля", 2002, "MTS", 'M', 1, false),
    (4, "Миша", 2002, "AVT", 'M', 3, true),
    (5, "Оля", 2000, "FIL", 'F', 3, false),
    (6, "Маша", 2001, "AVT", 'F', 5, true),
    (7, "Таня", 2004, "FIL", 'M', 4, true),
    (8, "Женя", 2000, "FIL", 'F', 4, true),
    (9, "Света", 2007, "AVT", 'F', 3, true),
    (10, "Аня", 2003, "MTS", 'F', 4, false),
    (11, "Лена", 2003, "AVT", 'F', 2, true),
    (12, "Сергей", 2005, "FIL", 'M', 3, false),
    (13, "Влад", 2004, "FIL", 'M', 5, false),
    (14, "Гена", 2003, "MTS", 'M', 1, true),
    (15, "Дима", 2006, "AVT", 'M', 5, false),
    (16, "Катя", 2001, "FIL", 'F', 4, false),
    (17, "Артём", 2005, "MTS", 'M', 3, true),
    (18, "Диана", 2006, "FIL", 'M', 4, false),
    (19, "Дмитрий", 1992, "FIL", 'M', 5, true),
    (20, "Лжедмитрий", 1992, "FIL", 'M', 5, true)
  )

  private val rooms: List[Room] = List(
    (37, 3, List(0, 7, 8)),
    (42, 2, List(1, 4)),
    (43, 3, List(6, 9, 11)),
    (54, 2, List(14, 17)),
    (56, 2, List(19, 20))
  )

  def philologyStudentsOlderThan93(students: List[Student] = students): List[(String, Int)] = {
    students
      .filter { case (_, _, birthYear, faculty, _, course, _) =>
        faculty == "FIL" && birthYear < 1993
      }
      .map { case (_, name, _, _, _, course, _) =>
        (name, course)
      }
  }

  def philologyStudentsInSameRoom(students: List[Student] = students, rooms: List[Room] = rooms): List[(String, Int, Int)] = {
    val filStudentsInDorm = students.filter { case (_, _, _, faculty, _, _, inDorm) =>
      faculty == "FIL" && inDorm
    }
    val roomMap: Map[Int, Int] = rooms.flatMap { case (roomNumber, _, studentIds) =>
      studentIds.map(id => (id, roomNumber))
    }.toMap
    filStudentsInDorm.map { case (id, name, _, _, _, _, _) =>
      (name, id, roomMap(id))
    }
  }

}
object Lab2 {

  private def processList(lst: List[Int], func: Int => Int): List[Int] = {
    @annotation.tailrec
    def helper(remaining: List[Int], acc: List[Int]): List[Int] = {
      remaining match {
        case Nil => acc.reverse
        case head :: tail => helper(tail, func(head) :: acc)
      }
    }

    helper(lst, Nil)
  }

  private def transformList(lst: List[Int]): List[String] = {
    lst.zipWithIndex.map { case (value, index) =>
      s"Элемент под номером $index равен $value"
    }
  }

  def work(): Unit = {
    println("\n\n--------------------------------------------------------------Лабораторная 2\n")
    val testList = List(21, 3, 7, 87)
    println(s"Исходный список: $testList")
    val result1 = processList(testList, _ -4)
    println(s"Обработанный список: $result1")
    val result2 = transformList(testList)
    println(s"Изменённый список: $result2")
    println("")

    val philologyStudents = Application.philologyStudentsOlderThan93()
    println(s"Имя и курс всех студентов факультета филологии, старше 93 года: $philologyStudents")
    val studentsInSameRoom = Application.philologyStudentsInSameRoom()
    println(s"Имя, ID и номер комнаты студентов факультета филологии, проживающих в одной комнате: $studentsInSameRoom")
  }
}

object Main {

  def main(args: Array[String]): Unit = {

    Lab1.work()
    Lab2.work()

  }
}