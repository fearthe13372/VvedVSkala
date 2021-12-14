package Lab3


object Strings {
    // a) Преобразуйте все символы типа Char в верхний регистр (не используйте заглавные буквы).

    def testUppercase(str: String): String = str.map(_.toUpper)


    // b) Вставьте следующие значения в строку:
    //     Hi my name is <name> and I am <age> years old.

    def testInterpolations(name: String, age: Int): String =s"Hi my name is ${name} and I am ${age} years old."

    /* c) Добавьте два числа в следующую строку:
     *       Hi,
     *       now follows a quite hard calculation. We try to add:
     *         a := <value of a>
     *         b := <value of b>
     *         result is <a + b>
     */

    def testComputation(a: Int, b: Int): String ="Hi,\n"+
      "now follows a quite hard calculation. We try to add:\n"+
      s"a := ${a}\n" +
      s"b := ${b} \n" +
      "\n"+
      s"result is ${a + b}"

    // d) Если длина строки равна 2, верните всю строку, иначе верните первые два символа строки.

    def testTakeTwo(str: String): String = {
        if (str.length > 2) str.take(2)
        else if(str.length == 2) str
        else "Длинна строки меньше 2"
    }
    
    // Точка входа в программу
    def main(args: Array[String]) = {
        println(testUppercase("lol"))
        println(testInterpolations("Egor",20))
        println(testComputation(5,10))
        println(testTakeTwo("l"))
    }
}
