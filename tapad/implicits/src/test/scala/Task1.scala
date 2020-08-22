
package implicits

object Task1 extends App {

  /*
    Task 1a: Find at least three places you can place this implicit val without breaking the method invocation on line 24.
    Task 1b: Move this val to another file, and make this code compile _without_ adding new import statements. You're
      now using the "default implicit search path"
    Task 2a: Implement a summoner method for `Printable` such that the following code compiles
//  Printable[Array[Int]].print(Array(1,2,3))
    Task 2b: Change the method `bold` to use context bounds instead of an implicit parameter list. You may need to use
    the summoner method from the previous task to get hold of an instance.
   */

  implicit val printableInt: Printable[Array[Int]] = new Printable[Array[Int]] {
    override def print(a: Array[Int]): String = a.toList.mkString(",")
  }

  def bold[A](a: A)(implicit p: Printable[A]): String = s"**${p.print(a)}**"

  bold(Array(1,2,3))
}