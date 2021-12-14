package Lab3
import scala.util.{Try, Failure, Success}


object Adts {
    // a) Дан List[Int], верните элемент с индексом n


    def getNth(list: List[Int], n: Int): Option[Int] = {
        list.zip(Range(0,list.length -1)).find(tuple=>tuple._2==n).map(_._1)
    }

    // Примените функцию из пункта (a) здесь, не изменяйте сигнатуру 
    def testGetNth(list: List[Int], n: Int): Option[Int] = getNth(list, n)


    // b) Напишите функцию, увеличивающую число в два раза.
    def doubleNum(n: Option[Int]): Option[Int] = {
        n.map(_ * 2)
    }

    // Примените функцию из пункта (b) здесь, не изменяйте сигнатуру
    def testDouble(n: Option[Int]): Option[Int] = doubleNum(n)

    // c) Напишите функцию, проверяющую является ли число типа Int четным. Если так, верните Right.
    // В противном случае, верните Left("Нечетное число.").

    def isEven(n: Int): Either[String, Int] = {
        Either.cond(n % 2 == 0, n, "Нечетное число")
    }

    // Примените функцию из пункта (c) здесь, не изменяйте сигнатуру
    def testIsEven(n: Int): Either[String, Int] = isEven(n)

    // d) Напишите функцию, реализующую безопасное деление целых чисел.
    // Верните Right с результатом или Left("Вы не можете делить на ноль.").

    def safeDivide(a: Int, b: Int): Either[String, Int] = {
        Either.cond(b != 0, a / b, "Вы не можете делить на ноль")
    }

    // Примените функцию из пункта (d) здесь, не изменяйте сигнатуру
    def testSafeDivide(a: Int, b: Int): Either[String, Int] = safeDivide(a, b)

    // e) Обработайте исключения функции с побочным эффектом вернув 0.

    def goodOldJava(impure: String => Int, str: String): Try[Int] = Try(impure(str)) match {
            case Success(num) => Try(num)
            case Failure(_) => Try(0)
        }


    // Примените функцию из пункта (e) здесь, не изменяйте сигнатуру
    def testGoodOldJava(impure: String => Int, str: String): Try[Int] = goodOldJava(impure, str)
    
    // Точка входа в программу
    def main(args: Array[String]) = {

        println(testGetNth(List (1,2,3,4,5,6,7,8,9,10),5))

        println(testDouble(Some(5)))

        println(testIsEven(9))

        println(testSafeDivide(10,0))

        println(testGoodOldJava(_.toInt,"123"))
    }
}
