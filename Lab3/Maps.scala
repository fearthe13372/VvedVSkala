package Lab3
import scala.annotation.tailrec

/* Напишите вашу реализацию в тестовые функции.
 * https://docs.scala-lang.org/overviews/collections/maps.html
 */
object Maps {
    case class User(name: String, age: Int)

    /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) 
     * и вычислите средний возраст: `name -> averageAge`. Вы можете реализовать 
     * ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
     */

    def testGroupUsers(users: Seq[User]): Map[String, Int] = {
        val averageAge=(users.foldLeft(0)((acc,user)=>acc+user.age)/users.length)
        users.groupBy(user=>user.name).map((name,_)=>name->averageAge)
    }

    /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей,
     * содержащихся в Map, содержат подстроку "Adam"? Вы можете реализовать ваше решение в 
     * теле тестовой функции. Не изменяйте сигнатуру.
     */

    def testNumberFrodos(map: Map[String, User]): Int = map.count(_._2.name==("Adam"))

    /* c) Удалите всех пользователей возраст которых менее 35 лет.
     * Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
     */

    def testUnderaged(map: Map[String, User]): Map[String, User] = map.filter(_._2.age > 35)
    
    // Точка входа в программу
    def main(args: Array[String]) = {

        println(Maps.testGroupUsers(Seq(Maps.User("Adam",36),Maps.User("Andrew",42),Maps.User("Johny",27),Maps.User("Darel",18))))

        println(Maps.testNumberFrodos{Map("First User"->Maps.User("Adam",36),"Second User"->Maps.User("Andrew",42),"Third User"->Maps.User("Johny",27),"Fourth User"->Maps.User("Darel",18))})

        println(Maps.testUnderaged{Map("First User"->Maps.User("Adam",36),"Second User"->Maps.User("Andrew",42),"Third User"->Maps.User("Johny",27),"Fourth User"->Maps.User("Darel",18))})

    }
}
