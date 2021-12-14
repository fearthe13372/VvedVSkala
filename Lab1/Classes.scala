object Classes {
  def main(args: Array[String]): Unit = {
    println("Cat: " + Animal.apply("cat"))
    println("Parrot eats meat: " + Animal.parrot.eats(Meat()))
    println("Cat is known animal: " + Animal.knownAnimal("cat"))
    println("Fish exists: " + Animal.apply("goldfish"))
    println("Meat exists: " + Food.apply("meat"))
  }

  trait Food

  case class Meat() extends Food

  case class Vegetables() extends Food

  case class Plants() extends Food

  /* This task has no tests. It is an exercise for you to write different class structures.
 *
 a) Создать класс Animal, который имеет следующие поля:
 *      - name: String (название)
 *      - species: String (вид)
 *      - food: String
 *
 *    Синтаксис: class MyClass(val publicField: Int, privateField: String) {
 *              // остальные поля и методы
 *            }
*/

  trait Animal {
    val name: String
    val food: Food

    def eats(food: Food): Boolean = this.food.equals(food)
  }

  case class Mammals(name: String, food: Food) extends Animal

  case class Birds(name: String, food: Food) extends Animal

  case class Fish(name: String, food: Food) extends Animal

  /*
 * b) Создайте объект-компаньон для класса Animal и добавьте следующие сущности как поля:
 *      - cat, mammal, meat
 *      - parrot, bird, vegetables
 *      - goldfish, fish, plants
 *
 *    Синтаксис: object MyClass {
 *              // статические поля и методы
 *            }
 */

  object Animal {
    val cat = Mammals("cat", Meat())
    val parrot = Birds("parrot", Vegetables())
    val goldfish = Fish("goldfish", Plants())

    def knownAnimal(name: String): Boolean =
      name.equals(cat.name) || name.equals(parrot.name) || name.equals(goldfish.name)

    def apply(name: String): Option[Animal] = name match {
      case "cat" => Some(cat)
      case "parrot" => Some(parrot)
      case "goldfish" => Some(goldfish)
      case _ => None()
    }
  }

  object Food {
    val meat: Meat = Meat()
    val vegetables: Vegetables = Vegetables()
    val plants: Plants = Plants()

    def apply(food: String): Option[Food] = food match {
      case "meat" => Some(meat)
      case "vegetables" => Some(vegetables)
      case "plants" => Some(plants)
      case _ => None()
    }
  }

  /*
   * c) Добавьте следующие метод в Animals:
   *      def eats(food: String): Boolean
   *
   *     который проверяет ест ли животное определенную пищу
   *
   * d) Переопределите ваш класс Animal как трейт и создайте объекты класса-образца для Mammals, Birds и Fishes.
   *    Вам все еще нужно поле `species`?
   *
   * e) Добавьте следующие функции в объект-компаньон Animal:
   *      def knownAnimal(name: String): Boolean  // true если это имя одного из трех животных из (b)
   *      def apply(name: String): Option[Animal] // возвращает одно из трех животных в соответствии с именем (Some) или ничего (None), см. ниже
   *
   * f) Создайте трейт Food со следующими классами-образцами:
   *      - Meat
   *      - Vegetables
   *      - Plants
   *   и добавьте это в определение Animal. Так же добавьте объект-компаньон с методом apply():
   *      def apply(food: String): Option[Food]
   */

  sealed trait Option[A] {

    def isEmpty: Boolean
  }

  case class Some[A](a: A) extends Option[A] {
    val isEmpty = false
  }

  case class None[A]() extends Option[A] {
    val isEmpty = true
  }
}