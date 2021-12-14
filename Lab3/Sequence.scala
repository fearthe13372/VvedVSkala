package Lab3
import scala.annotation.tailrec


// Примечание: напишите функции с хвостовой рекурсией
object Sequence {
    // a) Найдите последний элемент Seq.

    def testLastElement[A](seq: Seq[A]): Option[A] = {
        @tailrec
        def loop(rem:Seq[A],agg:Seq[A]):Option[A]= rem match{
            case head +: Seq() =>Some(head)
            case head +: tail => loop(tail,agg)
            case Seq() => None
        }
        loop(seq,Seq())
    }

    // b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) -
    // если Seq длиннее игнорируйте оставшиеся элементы.

    def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = Seq((a(0),b(0)),(a(1),b(1)))

    // c) Проверьте, выполняется ли условие для всех элементов в Seq.

    def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = seq.forall(cond)

    // d) Проверьте, является ли Seq палиндромом

    def testPalindrom[A](seq: Seq[A]): Boolean = seq ==seq.reverse

    // e) Реализуйте flatMap используя foldLeft.

    def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft(Seq[B]())((agg,value)=>agg++(f(value)))
    
    // Точка входа в программу
    def main(args: Array[String]) = {

        println(testLastElement(Seq(1,2,3,4,5,6)))

        println(testZip(Seq(1,2),Seq(3,4,5,6)))

        println(testForAll(Seq(1,2,3,4,5,6))(_ >=3))

        println(testPalindrom(Seq(1,2,3,2,1)))

        println(testFlatMap(Seq(1,2,3,4,5,6))(value=>Seq(value*2)))
    }
}
