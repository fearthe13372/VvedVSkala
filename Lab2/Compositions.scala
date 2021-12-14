

/** Option представляет собой контейнер, который хранит какое-то значение
 * или не хранит ничего совсем, указывает, вернула ли операция результат или нет. 
 * Это часто используется при поиске значений или когда операции могут потерпеть неудачу, 
 * и вам не важна причина.
 *
 * Комбинаторы называются так потому, что они созданы, чтобы объединять результаты. 
 * Результат одной функции часто используется в качестве входных данных для другой.
 *
 * Наиболее распространенным способом, является использование их со стандартными структурами данных.
 * Функциональные комбинаторы `map` и` flatMap` являются контекстно-зависимыми. 
 * map - применяет функцию к каждому элементу из списка, возвращается список с тем же числом элементов.
 * flatMap берет функцию, которая работает с вложенными списками и объединяет результаты.
 */

sealed trait Option[A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
}
case class Some[A](a: A) extends Option[A] {
    def map[B](f: A => B): Option[B] = Some(f(a))
    def flatMap[B](f: A => Option[B]): Option[B] = f(a)
}
case class None[A]()     extends Option[A] {
    def map[B](f: A => B): Option[B] = None()
    def flatMap[B](f: A => Option[B]): Option[B] = None()
}

// Напишите ваши решения в тестовых функциях.
object Compositions {
    // a) Используйте данные функции. Вы можете реализовать свое решение прямо в тестовой функции.
    // Нельзя менять сигнатуры


    def testCompose[A, B, C, D](f: A => B)
                               (g: B => C)
                               (h: C => D): A => D =(a:A) => h(g(f(a)))

    // b) Напишите функции с использованием `map` и `flatMap`.
    // Вы можете реализовать свое решение прямо в тестовой функции.
    // Нельзя менять сигнатуры

    def testMapFlatMap[A, B, C, D](f: A => Option[B])
                                  (g: B => Option[C])
                                  (h: C => D): Option[A] => Option[D] = (a: Option[A]) => a.flatMap(f).flatMap(g).map(h)

    // c) Напишите функцию используя for. Вы можете реализовать свое решение прямо в тестовой функции.
    // Нельзя менять сигнатуры
    def testForComprehension[A, B, C, D](f: A => Option[B])
                                        (g: B => Option[C])
                                        (h: C => D): Option[A] => Option[D] = (a: Option[A]) =>
        for {
            func1<- a
            func2 <- f(func1)
            func3 <- g(func2)
        } yield h(func3)

    
    // Точка входа в программу

    def main(args: Array[String]) :Unit= {
        print("Test compose")

        println(Compositions.testCompose((a:Int)=>a*3)((b:Int)=>b*2)
        ((c:Int)=>Math.pow(c,2)/100)(10))
        print("Test Map Flat Map")

        println(Compositions.testMapFlatMap((a:Int)=> Some(a.toString))
        ((b:String)=>Some(b.isBlank))((c:Boolean)=>c.toString)(Some(0)))
        print("Test For Comprehenssion")

        println(Compositions.testForComprehension((a:Int)=>Some(a.toString))
        ((b:String)=>Some(!b.isBlank))((c:Boolean)=>c.toString)(Some(5)))

    }
}
