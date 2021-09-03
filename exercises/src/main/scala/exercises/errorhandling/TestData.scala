package exercises.errorhandling
import scala.{Either => _, Left => _, Option => _, Right => _, _}
//
// 将Error放进一个Seq，可以解决需要返回多个错误的需求
trait Partial[+A,+B]
case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
case class Success[+B](get: B) extends Partial[Nothing,B]

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object TestData {
  def mkName(name: String): Either[String,Name] =
   if (name == "" || name == null) Left("name is empty")
   else Right(new Name(name))

  def mkAge(age: Int): Either[String,Age] =
    if (age <= 0) Left("age is invalid")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_,_))


  def main(args: Array[String]): Unit = {
    println(mkPerson("", -1))
  }
}
