package implicits

trait Printable[A] {
  def print(a: A): String
}