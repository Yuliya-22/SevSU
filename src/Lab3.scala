import scala.annotation.tailrec

object Lab3 {

  trait Tree {
    def getLeftSubtree: Tree
    def getRightSubtree: Tree
    def getNodeData: Int
  }

  private class Node(val data: Int, left: Tree, right: Tree) extends Tree {
    override def getLeftSubtree: Tree = left
    override def getRightSubtree: Tree = right
    override def getNodeData: Int = data
  }

  private class Leaf extends Tree {
    override def getLeftSubtree: Tree = this
    override def getRightSubtree: Tree = this
    override def getNodeData: Int = throw new NoSuchElementException("Leaf has no data")
  }

  // Функция для печати дерева
  private def printTree(tree: Tree): Unit = {
    tree match {
      case node: Node =>
        printTree(node.getLeftSubtree)
        println(node.getNodeData)
        printTree(node.getRightSubtree)
      case _: Leaf =>
        print("\uD83C\uDF43 ")
    }
  }

  // Функция для вставки элемента
  private def insert(value: Int, tree: Tree): Tree = {
    tree match {
      case node: Node =>
        if (value < node.getNodeData) {
          new Node(node.getNodeData, insert(value, node.getLeftSubtree), node.getRightSubtree)
        } else {
          new Node(node.getNodeData, node.getLeftSubtree, insert(value, node.getRightSubtree))
        }
      case _: Leaf => new Node(value, new Leaf, new Leaf)
    }
  }

  // Функция для проверки наличия элемента
  @tailrec
  private def contains(value: Int, tree: Tree): Boolean = {
    tree match {
      case node: Node =>
        if (value == node.getNodeData) {
          true }
        else if (value < node.getNodeData) {
          contains(value, node.getLeftSubtree) }
        else {
          contains(value, node.getRightSubtree) }
      case _: Leaf => false
    }
  }

  // Функция для вычисления суммы элементов
  private def sum(tree: Tree): Int = {
    tree match {
      case node: Node => node.getNodeData + sum(node.getLeftSubtree) + sum(node.getRightSubtree)
      case _: Leaf => 0
    }
  }

  def work(): Unit = {
    println("\n\n--------------------------------------------------------------Лабораторная 3\n")
    var tree: Tree = new Leaf
    tree = insert(5, tree)
    tree = insert(2, tree)
    tree = insert(7, tree)
    tree = insert(1, tree)
    tree = insert(8, tree)
    tree = insert(-1, tree)
    println(s"Дерево в порядке возрастания: ")
    printTree(tree)
    println("")

    println(s"Содержит ли дерево 1? Ответ: ${contains(1, tree)}")
    println(s"Содержит ли дерево 4? Ответ: ${contains(4, tree)}")
    println(s"Содержит ли дерево 8? Ответ: ${contains(8, tree)}")
    println(s"Сумма всех элементов: ${sum(tree)}")
  }
}
