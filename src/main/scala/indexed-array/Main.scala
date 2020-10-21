package sportarray

import shapeless._
import shapeless.ops.hlist._
import Skeleton._
import ArrayDefs._

object Main extends App {
  case class A1[T](data: List[T])
  implicit def a1ev[T: IsElement] = IsArray[A1[T], T](
    self => A1[T](Nil: List[T]),
    (self, n) => {println("HERE"); self.data(n)},
    self => self.data.length,
    (self, o) => A1[T](o :: self.data),
  )
  val t1 = A1[Double](List(1, 2, 3))
  import IsArraySyntax._
  println("Imported Syntax")
  val c = implicitly[A1[Double] => IsArrayOps[A1[Double]]]
  println("Defined implicit conv")
  println(implicitly[IsArray[A1[Double]]].getAtN(t1, 1))
}
