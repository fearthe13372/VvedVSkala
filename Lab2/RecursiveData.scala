

// Напишите свои решения в виде функций.

sealed trait List[A]
case class Cons[A](head: A, tail: List[A]) extends List[A]
case class Nil[A]() extends List[A]

object RecursiveData {
    // a) Реализуйте функцию, определяющую является ли пустым `List[Int]`.

    def ListIntEmpty(list: List[Int]): Boolean = list match {
      case Cons(_,_)=>false
      case  Nil() => true
    }

    // Используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
    def testListIntEmpty(list: List[Int]): Boolean = ListIntEmpty(list)


    // b) Реализуйте функцию, которая получает head `List[Int]`или возвращает -1 в случае если он пустой.
    def ListIntHead(list: List[Int]): Int = list match{
      case Cons(head,_)=>head
      case Nil() => -1
    }

    // Используйте функцию из пункта (b) здесь, не изменяйте сигнатуру
    def testListIntHead(list: List[Int]): Int = ListIntHead(list)

    // c) Можно ли изменить `List[A]` так чтобы гарантировать что он не является пустым?
    // Ответ: сделать так, чтобы Nil содержал какое-нибудь значение (например, head)
    //

    /* d) Реализуйте универсальное дерево (Tree) которое хранит значения в виде листьев и состоит из:
     *      node - левое и правое дерево (Tree)
     *      leaf - переменная типа A
     */

    sealed trait Tree[A]
    case class Node[A](leftNode: Tree[A], rightNode: Tree[A]) extends Tree[A]
    case class Leaf[A](value: A) extends Tree[A]
     
     // Точка входа в программу
     def main(args: Array[String]) = {
        println(testListIntEmpty(Cons(1,Nil())))
       println(testListIntHead(Cons(1,Nil())))
     }
}
