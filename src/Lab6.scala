
object Lab6 {

  private class Box[T] {
    private var value: Option[T] = None

    def save(v: T): Unit = { value = Some(v) }

    def get: Option[T] = value
  }

  def work(): Unit = {
    println("\n\n--------------------------------------------------------------Лабораторная 5\n")
    val intBox = new Box[Int]()
    intBox.save(42)
    println(intBox.get)
    val stringBox = new Box[String]()
    stringBox.save("Hello")
    println(stringBox.get)
    val emptyBox = new Box[Double]()
    println(emptyBox.get)
    println("")
  }
}
