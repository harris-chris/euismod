package euismod

import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.featurespec.{AnyFeatureSpec}

import shapeless._
import shapeless.{HList, HNil, Lazy, :: => #:}
import shapeless.ops.nat.{GT, GTEq, Pred, Diff => NatDiff, ToInt}
import shapeless.nat._
import shapeless.ops.hlist._
import org.scalactic._
import TripleEquals._
import org.scalactic.TolerantNumerics

object Dummy {

  def addEquality[A](f: (A, Any) => Boolean): Equality[A] = new Equality[A] {
    def areEqual(a: A, b: Any): Boolean = f(a, b)
  }

  implicit def arrDoubleEquality[A[_]]( implicit 
    aIsArr: ArrayDefs.IsArray[A, Double],
    fl: ArrayDefs.Flatten[A, Double],
    sh: ArrayDefs.Shape[A[Double]],
  ):Equality[A[Double]] = addEquality[A[Double]]((a, b) => b match {
    case p: A[Double] => {
      implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.1)
      sh(a) == sh(p) &&
      fl(a).zip(fl(p)).forall{ case(aE, pE) => aE === pE }
    }
    case _ => false
  })
  
  object Types {
    case class List1d[T] (
      data: List[T],
    )
    case class List2d[T] ( 
      data: List[List[T]],
    )
    case class List3d[T] ( 
      data: List[List[List[T]]],
    )
    case class List4d[T] ( 
      data: List[List[List[List[T]]]],
    )
  }

  object Values {
    import Types._
    val intsVals3d = List(
      List(
        List(1, 2, 3, 4),
        List(5, 6, 7, 8),
        List(9, 10, 11, 12),
      ),
      List(
        List(13, 14, 15, 16),
        List(17, 18, 19, 20),
        List(21, 22, 23, 24),
      )
    )
    val ints3d = List3d[Int](intsVals3d)

    val dblVals1d = List(0.0, 0.1, 0.2, 0.3, 0.4)
    val dblVals2d = List(
      List(0.00, 0.01, 0.02, 0.03, 0.04),
      List(0.10, 0.11, 0.12, 0.13, 0.14),
      List(0.20, 0.21, 0.22, 0.23, 0.24),
    )
    val dblVals3d = List(
      List(
        List(0.000, 0.001, 0.002, 0.003, 0.004),
        List(0.010, 0.011, 0.012, 0.013, 0.014),
        List(0.020, 0.021, 0.022, 0.023, 0.024),
      ),
      List(
        List(0.100, 0.101, 0.102, 0.103, 0.104),
        List(0.110, 0.111, 0.112, 0.113, 0.114),
        List(0.120, 0.121, 0.122, 0.123, 0.124),
      ),
    )
    val dblVals4d = List(
      List(
        List(
          List(0.0000, 0.0001, 0.0002, 0.0003, 0.0004),
          List(0.0010, 0.0011, 0.0012, 0.0013, 0.0014),
          List(0.0020, 0.0021, 0.0022, 0.0023, 0.0024),
        ),
        List(
          List(0.0100, 0.0101, 0.0102, 0.0103, 0.0104),
          List(0.0110, 0.0111, 0.0112, 0.0113, 0.0114),
          List(0.0120, 0.0121, 0.0122, 0.0123, 0.0124),
        ),
      ),
      List(
        List(
          List(0.1000, 0.1001, 0.1002, 0.1003, 0.1004),
          List(0.1010, 0.1011, 0.1012, 0.1013, 0.1014),
          List(0.1020, 0.1021, 0.1022, 0.1023, 0.1024),
        ),
        List(
          List(0.1100, 0.1101, 0.1102, 0.1103, 0.1104),
          List(0.1110, 0.1111, 0.1112, 0.1113, 0.1114),
          List(0.1120, 0.1121, 0.1122, 0.1123, 0.1124),
        ),
      )
    )
    val dbl1d = List1d[Double](dblVals1d)
    val dbl2d = List2d[Double](dblVals2d)
    val dbl3d = List3d[Double](dblVals3d)
    val dbl4d = List4d[Double](dblVals4d)

    val blVals3d = List(
      List(
        List(true   , false   , false   , false   , false   ),
        List(true   , false   , true    , true    , true    ),
        List(false  , true    , true    , true    , true    ),
      ),
      List(
        List(true   , true    , false   , false   , true    ),
        List(true   , false   , true    , true    , true    ),
        List(false  , false   , false   , true    , true    ),
      ),
    )
    val bl3d = List3d[Boolean](blVals3d)
  }
  object IsArrayImplicits {
    import Types._
    import Values._
    import ArrayDefs._
    import ArrayDefs.IsArraySyntax._
    implicit def list1dIsArray[T] = new IsArray[List1d, T] {
      type S = T
      def getEmpty[_T] = List1d[_T](Nil: List[_T])
      def getAtN(a: List1d[T], n: Int) = a.data(n)
      def length(a: List1d[T]) = a.data.length
      def cons(a: List1d[T], other: S) = List1d(other :: a.data)
    }
    implicit def list2dIsArray[T] = new IsArray[List2d, T] {
      type S = List1d[T]
      def getEmpty[_T]: List2d[_T] = List2d[_T](Nil: List[List[_T]])
      def getAtN(a: List2d[T], n: Int): S = List1d(a.data(n))
      def length(a: List2d[T]): Int = a.data.length
      def cons(a: List2d[T], sub: S): List2d[T] = List2d(sub.data :: a.data)
    }
    implicit def list3dIsArray[T] = new IsArray[List3d, T] {
      type S = List2d[T]
      def getEmpty[_T] = List3d[_T](Nil: List[List[List[_T]]])
      def getAtN(a: List3d[T], n: Int) = List2d(a.data(n))
      def length(a: List3d[T]) = a.data.length
      def cons(a: List3d[T], sub: S): List3d[T] = List3d(sub.data :: a.data)
    }
    implicit def list4dIsArray[T] = new IsArray[List4d, T] {
      type S = List3d[T]
      def getEmpty[_T] = List4d[_T](Nil: List[List[List[List[_T]]]])
      def getAtN(a: List4d[T], n: Int) = List3d(a.data(n))
      def length(a: List4d[T]) = a.data.length
      def cons(a: List4d[T], sub: S): List4d[T] = List4d(sub.data :: a.data)
    }
  }
}
  

class ArraySpec extends AnyFeatureSpec with GivenWhenThen with Matchers {
  import ArrayDefs._
  object Current extends Tag("Current")

  feature("Arraylike objects should be able to implement IsArray") {
    case class A1[T](data: List[T])
    implicit def a1ev[T] = new IsArray[A1, T] {
      type S = T
      def getEmpty[_T] = A1[_T](Nil: List[_T])
      def getAtN(a: A1[T], n: Int) = a.data(n)
      def length(a: A1[T]) = a.data.length
      def cons(a: A1[T], sub: S) = A1[T](sub :: a.data)
    }

    scenario("A 1d type that can implement IsArray, implements IsArray") {
      "implicitly[IsArray[A1, Double]]" should compile
    }

    scenario("A 2d 1dOf1d type that can implement IsArray, implements IsArray") {
      case class A1OfA1[T](data: List[A1[T]])
      implicit def a1ofa1ev[T] = new IsArray[A1OfA1, T] {
        type S = A1[T]
        def getEmpty[_T] = A1OfA1[_T](Nil: List[A1[_T]])
        def getAtN(a: A1OfA1[T], n: Int) = a.data(n)
        def length(a: A1OfA1[T]) = a.data.length
        def cons(a: A1OfA1[T], sub: S) = A1OfA1[T](sub :: a.data)
      }
      "implicitly[IsArray[A1OfA1, Double] { type S = A1[Double] }]" should compile
    }

    scenario("A 2d list-of-list type that can implement IsArray, implements IsArray") {
      case class A2[T](data: List[List[T]])
      implicit def a2ev[T] = new IsArray[A2, T] {
        type S = A1[T]
        def getEmpty[_T] = A2[_T](Nil: List[List[_T]])
        def getAtN(a: A2[T], n: Int) = A1[T](a.data(n))
        def length(a: A2[T]) = a.data.length
        def cons(a: A2[T], sub: S) = A2[T](sub.data :: a.data)
      }
      "the[IsArray[A2, Double] {type S = A1[Double] }]" should compile
    }
  }

  feature("Implicit class conversions and typeclass syntax for IsArray implementations") {
    case class A1[T](data: List[T])
    implicit def a1ev[T] = new IsArray[A1, T] {
      type S = T
      def getEmpty[_T] = A1[_T](Nil: List[_T])
      def getAtN(a: A1[T], n: Int) = a.data(n)
      def length(a: A1[T]) = a.data.length
      def cons(a: A1[T], sub: S) = A1[T](sub :: a.data)
    }
    scenario("The user creates a valid arraylike and implements the typeclass; syntax should be available") {
      val t1 = A1[Double](List(1, 2, 3))
      import IsArraySyntax._
      val c = implicitly[A1[Double] => IsArrayOps[A1, Double, Double]]
      assert(t1.data.zipWithIndex.forall(t => t1.getAtN(t._2) == t._1))
    }
  }

  feature("The Depth typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object DepthTest extends Tag("DepthTest")
    scenario("Depth has type Out of Nat 1 for a 1d array", DepthTest) {
      "implicitly[Depth[List1d[Double]] { type Out = Nat._1 }]" should compile
    }
    scenario("Depth has type Out of Nat 2 for a 2d array", DepthTest) {
      "implicitly[Depth[List2d[Double]] { type Out = Nat._2 }]" should compile
    }
    scenario("Depth has type Out of Nat 3 for a 3d array", DepthTest) {
      "implicitly[Depth[List3d[Double]] { type Out = Nat._3 }]" should compile
    }
  }

  feature("The PrettyPrint typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object PrettyPrintTest extends Tag("PrettyPrintTest")
    scenario("Pretty printing a 1d array produces numpy-like output", PrettyPrintTest) {
      val pp = PrettyPrint[List1d[Double]].apply(dbl1d)
    }
    scenario("Pretty printing a 2d array produces numpy-like output", PrettyPrintTest) {
      val pp = PrettyPrint[List2d[Double]].apply(dbl2d)
    }
    scenario("Pretty printing a 3d array produces numpy-like output", PrettyPrintTest) {
      val pp = PrettyPrint[List3d[Double]].apply(dbl3d)
    }
    scenario("Pretty printing a 4d array produces numpy-like output", PrettyPrintTest) {
      val pp = PrettyPrint[List4d[Double]].apply(dbl4d)
    }
  }

  feature("The TransposeRT typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object TransposeRTTest extends Tag("TransposeRTTest")
    scenario("transposing a 2d array with (0, 1) correctly flips the axes", TransposeRTTest) {
      val act = TransposeAxRT[List2d[Double]].apply(dbl2d, (0, 1))
      val exp = Transpose[List2d[Double], AllSlice].apply(dbl2d)
      assert(act === Some(exp))
    }
    scenario("transposing a 3d array with (0, 1) correctly flips the axes", TransposeRTTest) {
      val act = TransposeAxRT[List3d[Double]].apply(dbl3d, (0, 1))
      val exp = Transpose[List3d[Double], (_0, _1)].apply(dbl3d)
      assert(act === Some(exp))
    }
    scenario("transposing a 3d array with (1, 2) correctly flips the axes", TransposeRTTest) {
      val act = TransposeAxRT[List3d[Double]].apply(dbl3d, (1, 2))
      val exp = Transpose[List3d[Double], (_1, _2)].apply(dbl3d)
      assert(act === Some(exp))
    }
  }

  feature("The TransposeFromListInt typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object TransposeFromListIntTest extends Tag("TransposeFromListIntTest")
    scenario("transposing a 2d array with List(1) returns None", TransposeFromListIntTest) {
      val act = TransposeFromListInt[List2d[Double]].apply(dbl2d, List(1))
      val exp = None
      assert(act === None)
    }
    scenario("transposing a 2d array with List(1, 0) correctly flips the axes", TransposeFromListIntTest) {
      val act = TransposeFromListInt[List2d[Double]].apply(dbl2d, List(1, 0))
      val exp = Transpose[List2d[Double], (_0, _1)].apply(dbl2d)
      assert(act === Some(exp))
    }
    scenario("transposing a 2d array with List(0, 1) does nothing", TransposeFromListIntTest) {
      val act = TransposeFromListInt[List2d[Double]].apply(dbl2d, List(0, 1))
      val exp = dbl2d
      assert(act === Some(exp))
    }
    scenario("transposing a 3d array with List(1, 0, 2) correctly transposes", TransposeFromListIntTest) {
      val act = TransposeFromListInt[List3d[Double]].apply(dbl3d, List(1, 0, 2))
      val exp = Transpose[List3d[Double], (_0, _1)].apply(dbl3d)
      assert(act === Some(exp))
    }
    scenario("transposing a 3d array with List(2, 0, 1) correctly transposes", TransposeFromListIntTest) {
      val act = TransposeFromListInt[List3d[Double]].apply(dbl3d, List(2, 0, 1))
      val a021 = Transpose[List3d[Double], (_0, _1)].apply(dbl3d) 
      val a012 = Transpose[List3d[Double], (_1, _2)].apply(a021) 
      val exp = a012
      assert(act === Some(exp))
    }
    scenario("transposing a 3d array with List(2, 1, 0) correctly transposes", TransposeFromListIntTest) {
      val act = TransposeFromListInt[List3d[Double]].apply(dbl3d, List(2, 1, 0))
      val a120 = Transpose[List3d[Double], (_0, _1)].apply(dbl3d) 
      val a102 = Transpose[List3d[Double], (_1, _2)].apply(a120) 
      val a012 = Transpose[List3d[Double], (_0, _1)].apply(a102)
      val exp = a012
      assert(act === Some(exp))
    }
    scenario("transposing a 4d array with List(2, 1, 3, 0) correctly transposes", TransposeFromListIntTest, Current) {
      val act = TransposeFromListInt[List4d[Double]].apply(dbl4d, List(2, 1, 3, 0))
      val a1230 = Transpose[List4d[Double], (_0, _1)].apply(dbl4d) 
      val a1203 = Transpose[List4d[Double], (_2, _3)].apply(a1230)
      val a1023 = Transpose[List4d[Double], (_1, _2)].apply(a1203)
      val a0123 = Transpose[List4d[Double], (_0, _1)].apply(a1023)
      val exp = a0123
      assert(act === Some(exp))
    }
  }

  feature("The TransposeUsingString typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object TransposeUsingStringTest extends Tag("TransposeUsingStringTest")
    scenario("transposing a 2d array with 'ac' returns None", TransposeUsingStringTest) {
      val act = TransposeUsingString[List2d[Double]].apply(dbl2d, "ac")
      val exp = None
      assert(act === None)
    }
    scenario("transposing a 4d array with List(2, 1, 3, 0) correctly transposes", TransposeUsingStringTest) {
      val act = TransposeUsingString[List4d[Double]].apply(dbl4d, "kjli")
      val a1230 = Transpose[List4d[Double], (_0, _1)].apply(dbl4d) 
      val a1203 = Transpose[List4d[Double], (_2, _3)].apply(a1230)
      val a1023 = Transpose[List4d[Double], (_1, _2)].apply(a1203)
      val a0123 = Transpose[List4d[Double], (_0, _1)].apply(a1023)
      val exp = a0123
      assert(act === Some(exp))
    }
  }

  feature("The Transpose typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object TransposeTest extends Tag("TransposeTest")
    scenario("transposing a 2d array with AllSlice correctly flips the axes", TransposeTest) {
      val act = Transpose[List2d[Double], AllSlice].apply(dbl2d)
      val exp = List2d[Double](
        List(
          List(0.0 , 0.1 , 0.2 ),
          List(0.01, 0.11, 0.21),
          List(0.02, 0.12, 0.22),
          List(0.03, 0.13, 0.23),
          List(0.04, 0.14, 0.24),
        )
      )
      assert(act === exp)
    }
    scenario("transposing a 3d array with AllSlice correctly flips the axes", TransposeTest) {
      val act = Transpose[List3d[Double], AllSlice].apply(dbl3d)
      val exp = List3d[Double](
        List(
          List(
            List(0.0  , 0.1  ),
            List(0.01 , 0.11 ),
            List(0.02 , 0.12 ),
          ),
          List(
            List(0.001, 0.101),
            List(0.011, 0.111),
            List(0.021, 0.121),
          ),
          List(
            List(0.002, 0.102),
            List(0.012, 0.112),
            List(0.022, 0.122),
          ),
          List(
            List(0.003, 0.103),
            List(0.013, 0.113),
            List(0.023, 0.123),
          ),
          List(
            List(0.004, 0.104),
            List(0.014, 0.114),
            List(0.024, 0.124)
          )
        )
      )
      assert(act === exp)
    }
    scenario("transposing a 4d array with AllSlice correctly flips the axes", TransposeTest) {
      val act = Transpose[List4d[Double], AllSlice].apply(dbl4d)
      val exp = List4d[Double](
        List(
          List(
            List(
              List(0.0   , 0.1   ),
              List(0.01  , 0.11  )
            ),
            List(
              List(0.001 , 0.101 ),
              List(0.011 , 0.111 )
            ),
            List(
              List(0.002 , 0.102 ),
              List(0.012 , 0.112 )
            ),
          ),
          List(
            List(
              List(0.0001, 0.1001),
              List(0.0101, 0.1101)
            ),
            List(
              List(0.0011, 0.1011),
              List(0.0111, 0.1111)
            ),
            List(
              List(0.0021, 0.1021),
              List(0.0121, 0.1121)
            ),
          ),
          List(
            List(
              List(0.0002, 0.1002),
              List(0.0102, 0.1102)
            ),
            List(
              List(0.0012, 0.1012),
              List(0.0112, 0.1112)
            ),
            List(
              List(0.0022, 0.1022),
              List(0.0122, 0.1122)
            )
          ),
          List(
            List(
              List(0.0003, 0.1003),
              List(0.0103, 0.1103)
            ),
            List(
              List(0.0013, 0.1013),
              List(0.0113, 0.1113)
            ),
            List(
              List(0.0023, 0.1023),
              List(0.0123, 0.1123)
            )
          ),
          List(
            List(
              List(0.0004, 0.1004),
              List(0.0104, 0.1104)
            ),
            List(
              List(0.0014, 0.1014),
              List(0.0114, 0.1114)
            ),
            List(
              List(0.0024, 0.1024), 
              List(0.0124, 0.1124)
            )
          )
        )
      )
      assert(act === exp)
    }
    scenario("Transposing a 2d array along axes 0/1 matches with numpy", TransposeTest) {
      val act = Transpose[List2d[Double], (Nat._0, Nat._1)].apply(dbl2d)
      val exp = List2d[Double] (
        List(
          List(0.0 , 0.1 , 0.2 ),
          List(0.01, 0.11, 0.21),
          List(0.02, 0.12, 0.22),
          List(0.03, 0.13, 0.23),
          List(0.04, 0.14, 0.24),
        )
      )
      assert(act === exp)
    }
    scenario("Transposing a 3d array along axes 0/1 matches with numpy", TransposeTest) {
      val exp = List3d[Double] (
        List(
          List(
            List(0.0  , 0.001, 0.002, 0.003, 0.004),
            List(0.1  , 0.101, 0.102, 0.103, 0.104)
          ),
          List(
            List(0.01 , 0.011, 0.012, 0.013, 0.014),
            List(0.11 , 0.111, 0.112, 0.113, 0.114)
          ),
          List(
            List(0.02 , 0.021, 0.022, 0.023, 0.024),
            List(0.12 , 0.121, 0.122, 0.123, 0.124)
          ))
      )
      val act = Transpose[List3d[Double], (Nat._0, Nat._1)].apply(dbl3d)
      assert(act === exp)
    }
    scenario("Transposing a 3d array along axes 1/2 matches with numpy", TransposeTest) {
      val exp = List3d[Double] (
        List(
          List(
            List(0.0  , 0.01 , 0.02 ),
            List(0.001, 0.011, 0.021),
            List(0.002, 0.012, 0.022),
            List(0.003, 0.013, 0.023),
            List(0.004, 0.014, 0.024)
          ),
          List(
            List(0.1  , 0.11 , 0.12 ),
            List(0.101, 0.111, 0.121),
            List(0.102, 0.112, 0.122),
            List(0.103, 0.113, 0.123),
            List(0.104, 0.114, 0.124)
          ))
      )
      val act = Transpose[List3d[Double], (Nat._1, Nat._2)].apply(dbl3d)
      assert(act === exp)
    }
    scenario("Transposing a 4d array along axes 2/3 matches with numpy", TransposeTest) {
      //val act = Transpose[List4d[Double], (Nat._2, Nat._3)].apply(dbl4d)
      val act = Transpose[List4d[Double], (Nat._2, Nat._3)].apply(dbl4d)
      val exp = List4d[Double] (
        List(
          List(
            List(
              List(0.0   , 0.001 , 0.002 ),
              List(0.0001, 0.0011, 0.0021),
              List(0.0002, 0.0012, 0.0022),
              List(0.0003, 0.0013, 0.0023),
              List(0.0004, 0.0014, 0.0024),
            ),
            List(
              List(0.01  , 0.011 , 0.012 ),
              List(0.0101, 0.0111, 0.0121),
              List(0.0102, 0.0112, 0.0122),
              List(0.0103, 0.0113, 0.0123),
              List(0.0104, 0.0114, 0.0124),
            )
          ),
          List(
            List(
              List(0.1   , 0.101 , 0.102 ),
              List(0.1001, 0.1011, 0.1021),
              List(0.1002, 0.1012, 0.1022),
              List(0.1003, 0.1013, 0.1023),
              List(0.1004, 0.1014, 0.1024)
            ),
            List(
              List(0.11  , 0.111 , 0.112 ),
              List(0.1101, 0.1111, 0.1121),
              List(0.1102, 0.1112, 0.1122),
              List(0.1103, 0.1113, 0.1123),
              List(0.1104, 0.1114, 0.1124)
            )
          )
        )
      )
      assert(act === exp)
    }
    scenario("Transposing a 3d array along 1/2 then 0/1 is the same as a full transpose", TransposeTest) {
      val st1 = Transpose[List3d[Double], (Nat._0, Nat._1)].apply(dbl3d)
      val st2 = Transpose[List3d[Double], (Nat._1, Nat._2)].apply(st1)
      val act = Transpose[List3d[Double], (Nat._0, Nat._1)].apply(st2)
      val exp = Transpose[List3d[Double], AllSlice].apply(dbl3d)
      assert(act === exp)
    }
    scenario("Transposing a 4d array 0/1, 1/2, 2/3, 0/1, 1/2, 0/1 is the same as a full transpose", TransposeTest) {
      val st1 = Transpose[List4d[Double], (Nat._0, Nat._1)].apply(dbl4d)
      val st2 = Transpose[List4d[Double], (Nat._1, Nat._2)].apply(st1)
      val st3 = Transpose[List4d[Double], (Nat._2, Nat._3)].apply(st2)
      val st4 = Transpose[List4d[Double], (Nat._0, Nat._1)].apply(st3)
      val st5 = Transpose[List4d[Double], (Nat._1, Nat._2)].apply(st4)
      val act = Transpose[List4d[Double], (Nat._0, Nat._1)].apply(st5)
      val exp = Transpose[List4d[Double], AllSlice].apply(dbl4d)
      assert(act === exp)
    }
  }

  feature("The ConcatenateOpt typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object ConcatenateOptTest extends Tag("ConcatenateOptTest")
    scenario("Concatenating a 3d array along dimension 0 returns the correct result", ConcatenateOptTest) {
      val cn = ConcatenateOpt[List3d, List3d, Int]
      val b = ints3d.map(_+1).apply(List(0))
      val conc = cn(ints3d, b, 0).get
      assert(conc.shape == 3 :: 3 :: 4 :: HNil)
      assert(conc.data == 
        List(
          List(
            List(1, 2, 3, 4),
            List(5, 6, 7, 8),
            List(9, 10, 11, 12),
          ),
          List(
            List(13, 14, 15, 16),
            List(17, 18, 19, 20),
            List(21, 22, 23, 24),
          ),
          List(
            List(2, 3, 4, 5),
            List(6, 7, 8, 9),
            List(10, 11, 12, 13),
      )))
    }
    scenario("Concatenating a 3d array along dimension 1 returns the correct result", ConcatenateOptTest) {
      val cn = ConcatenateOpt[List3d, List3d, Int]
      val b = ints3d.map(_+1).apply(List(0, 1) :: List(0) :: List(0, 1, 2, 3) :: HNil)
      val conc =  cn(ints3d, b, 1).get
      assert(conc.shape == 2 :: 4 :: 4 :: HNil)
      assert(conc.data == 
        List(
          List(
            List(1, 2, 3, 4),
            List(5, 6, 7, 8),
            List(9, 10, 11, 12),
            List(2, 3, 4, 5),
          ),
          List(
            List(13, 14, 15, 16),
            List(17, 18, 19, 20),
            List(21, 22, 23, 24),
            List(14, 15, 16, 17),
      )))
    }
    scenario("Concatenating a 3d array along dimension 2 returns the correct result", ConcatenateOptTest) {
      val cn = ConcatenateOpt[List3d, List3d, Int]
      val b = ints3d.map(_+1).apply(List(0, 1) :: List(0, 1, 2) :: List(0) :: HNil)
      val conc = cn(ints3d, b, 2).get
      assert(conc.shape == 2 :: 3 :: 5 :: HNil)
      assert(conc.data == 
        List(
          List(
            List( 1,  2,  3,  4,  2),
            List( 5,  6,  7,  8,  6),
            List( 9, 10, 11, 12, 10),
          ),
          List(
            List( 13, 14, 15, 16, 14),
            List( 17, 18, 19, 20, 18),
            List( 21, 22, 23, 24, 22),
      )))
    }
  }

  feature("The AddOpt typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object AddOptTest extends Tag("AddOptTest")
    scenario("adding a List1d to a List1d produces a combined array", AddOptTest) {
      val l1 = List1d[Int](List(1, 2, 3))
      val l2 = List1d[Int](List(4, 5, 6))
      val exp = List1d[Int](List(1, 2, 3, 4, 5, 6))
      val t: List[Int] = l1.flatten
      assert(AddOpt[List1d, List1d, Int].apply(l1, l2) === Some(exp))
    }
    scenario("adding a List2d to a List2d returns a combined array", AddOptTest) {
      val l1 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6)))
      val l2 = List2d[Int](List(List(7, 8, 9), List(10, 11, 12)))
      val exp = List2d[Int](List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(10, 11, 12)))
      assert(AddOpt[List2d, List2d, Int].apply(l1, l2) === Some(exp))
    }
    scenario("adding a List2d to a List2d with different dim0 size returns a combined array", AddOptTest) {
      val l1 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6)))
      val l2 = List2d[Int](List(List(7, 8, 9)))
      val exp = List2d[Int](List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
      assert(AddOpt[List2d, List2d, Int].apply(l1, l2) === Some(exp))
    }
    scenario("adding a List2d to a List2d with different dim1 length returns None", AddOptTest) {
      val l1 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6)))
      val l2 = List2d[Int](List(List(7, 8), List(10, 11)))
      assert(AddOpt[List2d, List2d, Int].apply(l1, l2) === None)
    }
    scenario("adding a List3d to a List3d with dim1 and dim2 lengths the same returns a combined array", AddOptTest) {
      val l1 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6)))
      val l2 = List2d[Int](List(List(7, 8), List(10, 11)))
      assert(AddOpt[List2d, List2d, Int].apply(l1, l2) === None)
    }
    scenario("adding a List3d to a List3d with different dim1 length returns None", AddOptTest) {
      val l1 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6)))
      val l2 = List2d[Int](List(List(7, 8), List(10, 11)))
      assert(AddOpt[List2d, List2d, Int].apply(l1, l2) === None)
    }
  }

  feature("MaskFromNumSeq typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object MaskFromNumSeqTest extends Tag("MaskFromNumSeqTest")
    scenario("MaskDTFromNumSeq returns a correct mask for a 1d array", MaskFromNumSeqTest) {
      val allFalse = dbl1d.map(_ => false)
      assert(
        MaskFromNumSeq[List1d[Boolean], List[Int] :: HNil].apply(List(1, 2) :: HNil, allFalse) ===
        List1d[Boolean](allFalse.data.updated(1, true).updated(2, true))
      )
    }
    scenario("MaskFromNumSeq returns a correct mask for a 2d array", MaskFromNumSeqTest) {
      val allFalse = dbl2d.map(_ => false)
      val exp = List2d[Boolean](
        List(
          List(false, false, false, false, false),
          List(false, false, false, true, true),
          List(false, false, false, true, true),
        )
      )
      assert(
        MaskFromNumSeq[List2d[Boolean], List[Int] :: List[Int] :: HNil].apply(
          List(1, 2) :: List(3,4) :: HNil, allFalse) === exp
      )
    }
    scenario("MaskFromNumSeq returns a correct mask for a 3d array", MaskFromNumSeqTest) {
      val allFalse = dbl3d.map(_ => false)
      val exp = List3d[Boolean](
        List(
          List(
            List(false, false, true, true, false),
            List(false, false, false, false, false),
            List(false, false, true, true, false),
          ),
          List(
            List(false, false, false, false, false),
            List(false, false, false, false, false),
            List(false, false, false, false, false),
          )
        )
      )
      assert(
        MaskFromNumSeq[List3d[Boolean], List[Int] :: List[Int] :: List[Int] :: HNil].apply(
          List(0) :: List(0,2) :: List(2, 3) :: HNil, allFalse) === exp
      )
    }
  }

  feature("The Where typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object WhereTest extends Tag("WhereTest")
    scenario("Where with a 1d arraylike and a masking array returns a correct result", WhereTest) {
      val mask = MaskFromNumSeq[List1d[Boolean], List[Int] :: HNil].apply(
        List(1, 2) :: HNil, dbl1d.map(_ => false))
      val newValues = dbl1d.map(_ => 3.0)
      val act = Where[List1d, Double].apply(dbl1d, mask, newValues)
      val exp = List1d[Double](dbl1d.data.updated(1, 3.0).updated(2, 3.0))
      assert(act === exp)
    }
    scenario("Where with a 2d arraylike and a masking array returns a correct result", WhereTest) {
      val mask = MaskFromNumSeq[List2d[Boolean], List[Int] :: List[Int] :: HNil].apply(
        List(1, 2) :: List(0, 1) :: HNil, dbl2d.map(_ => false))
      val invMask = mask.map(b => if(b == true) {false} else {true})
      val newValues = dbl2d.map(_ => 3.0)
      val act = Where[List2d, Double].apply(dbl2d, mask, newValues)
      val ai = ApplyIndex[List2d[Double], List2d[Boolean]]
      assert(ai(act, mask).get.forall(_ == 3.0))
      assert(ai(act, invMask) == ai(dbl2d, invMask))
    }
    scenario("Where with a 3d arraylike and a masking array returns a correct result", WhereTest) {
      val mask = MaskFromNumSeq[List3d[Boolean], List[Int] :: List[Int] :: List[Int] :: HNil].apply(
        List(1, 2) :: List(0, 1) :: List(2) :: HNil, dbl3d.map(_ => false))
      val invMask = mask.map(b => if(b == true) {false} else {true})
      val newValues = dbl3d.map(_ => 3.0)
      val act = Where[List3d, Double].apply(dbl3d, mask, newValues)
      val ai = ApplyIndex[List3d[Double], List3d[Boolean]]
      assert(ai(act, mask).get.forall(_ == 3.0))
      assert(ai(act, invMask) == ai(dbl3d, invMask))
    }
    scenario("Where with a 4d arraylike and a masking array returns a correct result", WhereTest) {
      val mask = MaskFromNumSeq[List4d[Boolean], List[Int] :: List[Int] :: List[Int] :: List[Int] :: HNil].apply(
        List(1, 2) :: List(0, 1) :: List(2) :: List(0, 2) :: HNil, dbl4d.map(_ => false))
      val invMask = mask.map(b => if(b == true) {false} else {true})
      val newValues = dbl4d.map(_ => 3.0)
      val act = Where[List4d, Double].apply(dbl4d, mask, newValues)
      val ai = ApplyIndex[List4d[Double], List4d[Boolean]]
      assert(ai(act, mask).get.forall(_ == 3.0))
      assert(ai(act, invMask) == ai(dbl4d, invMask))
    }
  }

  feature("The Reduce typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object ReduceTest extends Tag("ReduceTest")
    scenario("Using Reduce on a 1d arraylike does not compile", ReduceTest) {
      "Reduce[List1d, Double, Nat._0].apply(dbl1d, lst => lst.foldLeft(0.0)(_ + _))" shouldNot compile
    }
    scenario("Reducing a 2d arraylike across dim0 returns a correct 1d arraylike", ReduceTest) {
      val act: List1d[Double] = Reduce[List2d, Double, Nat._0].apply(
        dbl2d, lst => lst.foldLeft(0.0: Double)(_ + _)
      )
      val exp = List1d[Double](dbl2d.data.transpose.map(_.foldLeft(0.0)((a, b) => a + b)))
      assert(act.length === dbl2d.getAtN(0).length)
      assert(act === exp)
    }
    scenario("Reducing a 2d arraylike across dim1 returns a correct 1d arraylike", ReduceTest) {
      val act: List1d[Double] = Reduce[List2d, Double, Nat._1].apply(
        dbl2d, lst => lst.foldLeft(0.0: Double)(_ + _)
      )
      val exp = List1d(dbl2d.data.map(_.foldLeft(0.0)(_ + _)))
      assert(act.length === dbl2d.length)
      assert(act === exp)
    }
    scenario("Reducing a 3d arraylike across dim1 returns a correct 2d arraylike", ReduceTest) {
      val act = Reduce[List3d, Double, Nat._1].apply(
        dbl3d, lst => lst.foldLeft(0.0: Double)(_ + _)
      )
      val exp = List2d[Double](
        List(
          List(0.030, 0.033, 0.036, 0.039, 0.042),
          List(0.330, 0.333, 0.336, 0.339, 0.342),
        )
      )
      assert(act.shape === dbl3d.shape.at(Nat._0) :: dbl3d.shape.at(Nat._2) :: HNil)
      assert(act === exp)
    }
    scenario("Reducing a 3d arraylike across dim2 returns a correct 2d arraylike", ReduceTest) {
      val act = Reduce[List3d, Double, Nat._2].apply(
        dbl3d, lst => lst.foldLeft(0.0: Double)(_ + _)
      )
      val exp = List2d[Double](
        dbl3d.data.map(_.map(lst => lst.foldLeft(0.0)(_ + _)))
      )
      assert(act.shape === dbl3d.shape.at(Nat._0) :: dbl3d.shape.at(Nat._1) :: HNil)
      assert(act === exp)
    }
  }

  feature("Multi-dimensional arrays") {
    import Dummy._
    case class A1[T](data: List[T])
    implicit def a1ev[T] = new IsArray[A1, T] {
      type S = T
      def getEmpty[_T] = A1[_T](Nil: List[_T])
      def getAtN(a: A1[T], n: Int) = a.data(n)
      def length(a: A1[T]) = a.data.length
      def cons(a: A1[T], sub: S) = A1[T](sub :: a.data)
    }
    case class A1OfA1[T](data: List[A1[T]])
    implicit def a1ofa1ev[T] = new IsArray[A1OfA1, T] {
      type S = A1[T]
      def getEmpty[_T] = A1OfA1[_T](Nil: List[A1[_T]])
      def getAtN(a: A1OfA1[T], n: Int) = a.data(n)
      def length(a: A1OfA1[T]) = a.data.length
      def cons(a: A1OfA1[T], sub: S) = A1OfA1[T](sub :: a.data)
    }
    scenario("A value is returned from a 2d-dimensional array using getAtN") {
      Given("A 1-dimensional arraylike, and a 2d list of 1d arraylike 2d array implementation")
      When("getAtN is called on a concrete instance of the 2d arraylike")
      import ArrayDefs.IsArraySyntax._
      val t2 = A1OfA1[Double](A1[Double](List(1.0, 2.0)) :: Nil)
      Then("the returned value should be the 1d arraylike")
      val t1: A1[Double] = t2.getAtN(0)
    }
    scenario("A value is returned from a 3d-dimensional array using getAtN") {
      Given("A 3d arraylike")
      val t3 = Dummy.Types.List3d[Double](Dummy.Values.dblVals3d)
      When("getAtN is called on a concrete instance of the 3d arraylike")
      import Dummy.IsArrayImplicits._
      import ArrayDefs.IsArraySyntax._
      Then("the returned value should be the 1d arraylike")
      val t2: Dummy.Types.List2d[Double] = t3.getAtN(0)
    }
  }

  feature("IsArray.getAtN") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    scenario("getAtN is called on a 1d Array to recover its original elements") {
      import ArrayDefs.IsArraySyntax._
      val dbl1d = List1d[Double](dblVals1d)
      assert(dblVals1d.zipWithIndex.forall({case(x, i) => x == dbl1d.getAtN(i)}))
    }
  }

  feature("IsArray.cons") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    scenario("IsArray objects are constructed from individual values using cons") {
      import ArrayDefs.IsArraySyntax._
      val lst0 = List1d[Double](Nil)
      val lst1 = dblVals1d(4) :: lst0
      val lst2 = dblVals1d(3) :: lst1
      val lst3 = dblVals1d(2) :: lst2
      val lst4 = dblVals1d(1) :: lst3
      val lst5 = dblVals1d(0) :: lst4
      assert(lst5 == List1d[Double](dblVals1d))
    }
  }
  
  feature("IsArray.getEmpty[_T]") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    object GetEmptyTest extends Tag("GetEmptyTest")
    scenario(".getEmpty[_T] is used on a 1d arraylike to create a new empty arraylike", GetEmptyTest) {
      "val l1: List1d[Int] = dbl1d.getEmpty[Int]" should compile
    }
    scenario(".getEmpty[_T] is used  on a 2d arraylike to create a new empty arraylike", GetEmptyTest) {
      "val l2: List2d[Int] = dbl2d.getEmpty[Int]" should compile
    }
  }

  feature("The SubArrays typeclass") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    scenario("SubArrays.Out of a 1d array is an Hlist containing a 1d array") {
      val sa: SubArrays[List1d[Double]] { 
        type Out = List1d[Double] :: HNil 
      } = SubArrays[List1d[Double]]
    }
    scenario("SubArrays.Out of a 2d array is an Hlist containing a 2d and a 1d array") {
      val sa: SubArrays[List2d[Double]] { 
        type Out = List2d[Double] :: List1d[Double] :: HNil 
      } = SubArrays[List2d[Double]]
    }
    scenario("SubArrays.Out of a 3d array is an Hlist containing a 3d, 2d and 1d array") {
      val sa: SubArrays[List3d[Double]] { 
        type Out = List3d[Double] :: List2d[Double] :: List1d[Double] :: HNil 
      } = SubArrays[List3d[Double]]
    }
  }

  feature("The ArraysSort typeclass") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    scenario("Descending should witness a descending list of arrays") {
      type AR = List3d[Int] :: List2d[Int] :: List1d[Int] :: HNil
      "implicit val ev = ArraySort[AR, Descending]" should compile
    }
    scenario("Descending should not witness an ascending list of arrays") {
      type AR = List1d[Int] :: List2d[Int] :: List3d[Int] :: HNil
      "implicit val ev = ArraySort[AR, Descending]" shouldNot compile
    }
    scenario("Ascending should witness an ascending list of arrays") {
      type AR = List1d[Int] :: List2d[Int] :: List3d[Int] :: HNil
      "implicit val ev = ArraySort[AR, Ascending]" should compile
    }
    scenario("Should not witness an disordered list of arrays") {
      type AR = List1d[Int] :: List3d[Int] :: List2d[Int] :: HNil
      "implicit val ev = ArraySort[AR, Ascending]" shouldNot compile
    }
  }

  feature("The FromElemsAndSubArraysOpt typeclass") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    object FromElemsAndSubArraysOptTest extends Tag("FromElemsAndSubArraysOptTest")
    scenario("a List1d can be constructed from a list of Ts", FromElemsAndSubArraysOptTest) {
      val ts3: List[Double] = List(1.5, 2.5, 3.5)
      val newShape = 3 :: HNil
      val ga = SubArrays[List1d[Double]]
      val fe = FromElemsAndSubArraysOpt[ga.Out, Double, Int :: HNil]
      val act = fe(ts3, newShape)
      assert(act match {
        case Some(l1) => {
          l1.flatten.toList === ts3
        }
        case None => false
      })
    }
    scenario("a List2d can be constructed from a list of Ts", FromElemsAndSubArraysOptTest) {
      val ts3: List[Double] = List(0.00, 0.01, 0.02, 0.10, 0.11, 0.12)
      val newShape = 2 :: 3 :: HNil
      val ga = SubArrays[List2d[Double]]
      val fe = FromElemsAndSubArraysOpt[ga.Out, Double, Int :: Int :: HNil]
      val actO = fe(ts3, newShape)
      val exp = List2d[Double](List(
        List(0.00, 0.01, 0.02),
        List(0.10, 0.11, 0.12),
      ))
      assert(actO match {
        case Some(act) => {
          act === exp
        }
        case None => false
      })
    }
    scenario("a List3d can be constructed from a list of Ts", FromElemsAndSubArraysOptTest) {
      val ts3: List[Double] = List(
        0.000, 0.001, 0.002, 0.010, 0.011, 0.012, 0.100, 0.101, 0.102, 0.110, 0.111, 0.112,
      )
      val newShape = 2 :: 2 :: 3 :: HNil
      val ga = SubArrays[List3d[Double]]
      val fe = FromElemsAndSubArraysOpt[ga.Out, Double, Int :: Int :: Int :: HNil]
      val actO = fe(ts3, newShape)
      val exp = List3d[Double](
        List(
          List(
            List(0.000, 0.001, 0.002),
            List(0.010, 0.011, 0.012),
          ),
          List(
            List(0.100, 0.101, 0.102),
            List(0.110, 0.111, 0.112),
      )))
      assert(actO match {
        case Some(act) => {
          act === exp
        }
        case None => false
      })
    }
    scenario("a List1d can be constructed from a 2d array and a list of Ts", FromElemsAndSubArraysOptTest) {
      val ts6: List[Double] = List(1.5, 2.5, 3.5, 2.0, 3.0, 4.0)
      val newShape = 6 :: HNil
      val sa = SubArrays[List2d[Double]]
      val fe = FromElemsAndSubArraysOpt[sa.Out, Double, Int :: HNil]
      val act = fe(ts6, newShape)
      assert(act match {
        case Some(l1) => l1.flatten.toList === ts6
        case None => false
      })
    }
    scenario("a List2d can be constructed from a 2d array and a list of Ts", FromElemsAndSubArraysOptTest) {
      val ts6: List[Double] = List(1.5, 2.5, 3.5, 2.0, 3.0, 4.0)
      val newShape = 3 :: 2 :: HNil
      val sa = SubArrays[List2d[Double]]
      val fe = FromElemsAndSubArraysOpt[sa.Out, Double, Int :: Int :: HNil]
      val act = fe(ts6, newShape)
      assert(act match {
        case Some(l2) => {
          l2.flatten.toList === ts6 && l2.shape === newShape
        }
        case None => false
      })
    }
  }

  //feature("The FromElemsAndSubArraysUsingListOpt typeclass") {
    //import ArrayDefs.IsArraySyntax._
    //import Dummy.Types._
    //import Dummy.Values._
    //import Dummy.IsArrayImplicits._
    //object FromElemsAndSubArraysUsingListOptTest extends Tag("FromElemsAndSubArraysUsingListOptTest")
    //scenario("a List1d can be constructed from a list of Ts", FromElemsAndSubArraysUsingListOptTest) {
      //val ts3: List[Double] = List(1.5, 2.5, 3.5)
      //val newShape = 3 :: HNil
      //val ga = SubArrays[List1d[Double]]
      //val fe = FromElemsAndSubArraysUsingListOpt[ga.Out, Double]
      //val act = fe(ts3, List(3))
      //val exp = FromElemsAndSubArraysOpt[ga.Out, Double, Int :: HNil].apply(ts3, 3 :: HNil)
      //assert(act.get === exp.get)
    //}
  //}

  feature("The FromElemsAndArrayOpt typeclass") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    object FromElemsAndArrayOptTest extends Tag("FromElemsAndArrayOptTest")
    scenario("a List1d can be constructed from a list of Ts", FromElemsAndArrayOptTest) {
      val ts3: List[Double] = List(1.5, 2.5, 3.5)
      val newShape = 3 :: HNil
      val fe = FromElemsAndArrayOpt[List1d, Double, Int :: HNil]
      val act = fe(ts3, newShape)
      assert(act match {
        case Some(l1) => {
          l1.flatten.toList === ts3
        }
        case None => false
      })
    }
    scenario("a List2d can be constructed from a list of Ts", FromElemsAndArrayOptTest) {
      val ts3: List[Double] = List(0.00, 0.01, 0.02, 0.10, 0.11, 0.12)
      val newShape = 2 :: 3 :: HNil
      val fe = FromElemsAndArrayOpt[List2d, Double, Int :: Int :: HNil]
      val actO = fe(ts3, newShape)
      val exp = List2d[Double](List(
        List(0.00, 0.01, 0.02),
        List(0.10, 0.11, 0.12),
      ))
      assert(actO match {
        case Some(act) => {
          act === exp
        }
        case None => false
      })
    }
  }

  feature("IsArray.map") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    object MapTest extends Tag("MapTest")
    scenario(".map on a 1d array returns a mapped 1d array", MapTest) {
      val mapped = dbl1d.map(t => t.toInt)
      assert(mapped.shape === dbl1d.shape)
      assert(mapped.flatten === dbl1d.flatten.map(_.toInt))
    }
    scenario(".map on a 2d array returns a mapped 2d array", MapTest) {
      val mapped = dbl2d.map(t => t.toInt)
      assert(mapped.shape === dbl2d.shape)
      assert(mapped.flatten === dbl2d.flatten.map(_.toInt))
    }
    scenario(".map on a 3d array returns a mapped 3d array", MapTest) {
      val mapped = dbl3d.map(t => t.toInt)
      assert(mapped.shape === dbl3d.shape)
      assert(mapped.flatten === dbl3d.flatten.map(_.toInt))
    }
    scenario(".map double => char on a 3d array returns a mapped 3d array", MapTest) {
      val mapped = dbl3d.map(t => 'c')
      assert(mapped.shape === dbl3d.shape)
      assert(mapped.flatten === dbl3d.flatten.map(_ => 'c'))
    }
  }

  feature("The ApplyIndex typeclass") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    object ApplyIndexTest extends Tag("ApplyIndexTest")

    val mini3 = List1d[Int](List(1, 2, 3))
    val mini33 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val mini233 = List3d[Int](
      List(
        List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)),
        List(List(10, 11, 12), List(13, 14, 15), List(16, 17, 18))
      )
    )
    scenario("a List[Int] is used to return the listed elements from a 1d array", ApplyIndexTest) {
      assert(
        ApplyIndex[List1d[Int], List[Int]].apply(mini3, List(1, 2)) === 
        List1d[Int](List(2, 3))
      ) 
    }
    scenario("a List[Int] :: HNil is used to return the listed elements from a 1d array", ApplyIndexTest) {
      assert(
        ApplyIndex[List2d[Int], List[Int] :: HNil].apply(mini33, List(1, 2) :: HNil) === 
        List2d[Int](List(List(4, 5, 6), List(7, 8, 9)))
      ) 
    }
    scenario("a Int is used to return the listed elements from a 3d array", ApplyIndexTest) {
      assert(
        ApplyIndex[List3d[Int], Int].apply(mini233, 1) === mini233.getAtN(1)
      ) 
    }
    scenario("a Int :: HNil is used to return T from a 1d array", ApplyIndexTest) {
      assert(
        ApplyIndex[List1d[Int], Int :: HNil].apply(mini3, 1 :: HNil) === mini3.getAtN(1)
      ) 
    }
    scenario("a Int :: HNil is used to return a 2d from a 3d array", ApplyIndexTest) {
      val l2 = ApplyIndex[List3d[Int], Int :: HNil].apply(mini233, 1 :: HNil)
      assert(l2.isInstanceOf[List2d[Int]])
      assert(l2 === mini233.getAtN(1)) 
    }
    scenario("a Int :: List[Int] :: HNil is used to return a 2d from a 3d array", ApplyIndexTest) {
      val l2 = ApplyIndex[List3d[Int], Int :: List[Int] :: HNil].apply(mini233, 1 :: List(0, 1) :: HNil)
      assert(l2.isInstanceOf[List2d[Int]])
      assert(l2 === mini233.getAtN(1)(List(0, 1))) 
    }
    scenario("a Int :: Range :: HNil is used to return a 2d from a 3d array", ApplyIndexTest) {
      val l2 = ApplyIndex[List3d[Int], Int :: Range :: HNil].apply(mini233, 1 :: Range(0, 2) :: HNil)
      assert(l2.isInstanceOf[List2d[Int]])
      assert(l2 === mini233.getAtN(1)(List(0, 1))) 
    }
    scenario("a List[Int] :: Int :: HNil is used to return a 2d from a 3d array", ApplyIndexTest) {
      val l2 = ApplyIndex[List3d[Int], List[Int] :: Int :: HNil].apply(mini233, List(0) :: 1 :: HNil)
      assert(l2.isInstanceOf[List2d[Int]])
      assert(l2 === IsArray[List2d, Int, List1d[Int]].fromList(List(mini233.getAtN(0).getAtN(1)))) 
    }
    scenario("an HList of List[Int] is used to return the correct elements from a 3d array", ApplyIndexTest) {
      assert(
        ApplyIndex[List3d[Int], List[Int] :: List[Int] :: List[Int] :: HNil].apply(
          mini233, List(1) :: List(1, 2) :: List(0, 1) :: HNil
        ) === List3d[Int](List(List(List(13, 14), List(16, 17))))
      )
    }
    scenario("an HList of List[Int] :: Int is used to return the correct elements from a 3d array", ApplyIndexTest) {
      assert(
        ApplyIndex[List3d[Int], List[Int] :: Int :: HNil].apply(mini233, List(1) :: 2 :: HNil) ===
        List2d[Int](List(List(16, 17, 18)))
      )
    }
    scenario("apply with an HList of more elements than array dimensions will not compile", ApplyIndexTest) {
      "ApplyIndex[List3d[Int], Int :: Int :: Int :: Int :: HNil].apply(mini233, 1 :: 1 :: 1 :: 1 :: HNil)" shouldNot compile
    }
    scenario("ApplyIndex with a boolean array of the same shape should return an appropriate List[T]", ApplyIndexTest) {
      // exp taken from array_mask_test.ipynb
      val exp = List(
        0.0, 0.01 , 0.012, 0.013, 0.014, 0.021, 0.022, 0.023, 0.024, 0.1  , 0.101, 
        0.104, 0.11 , 0.112, 0.113, 0.114, 0.123, 0.124
      )
      assert(ApplyIndex[List3d[Double], List3d[Boolean]].apply(dbl3d, bl3d) === Some(exp))
    }
  }

  feature("The IsArray.length method returns the length of the top dimension") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    scenario("An 1d array of y elements should return .length of y")
    {
      assert(dbl1d.length == dbl1d.data.length)
    }
    scenario("A 2d array of y elements should return .length of y")
    {
      assert(dbl2d.length == dbl2d.data.length)
    }
    scenario("A 3d array of y elements should return .length of y")
    {
      assert(dbl3d.length == dbl3d.data.length)
    }
  }

  feature("The Shape Typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object ShapeTest extends Tag("ShapeTest")
    scenario("An 1d array of _ elements should return the correct shape", ShapeTest) {
      assert(Shape[List1d[Double]].apply(dbl1d) === dbl1d.length :: HNil)
    }
    scenario("An 2d array of _ elements should return the correct shape", ShapeTest) {
      assert(Shape[List2d[Double]].apply(dbl2d) === dbl2d.length :: dbl2d.getAtN(0).length :: HNil)
    }
    scenario("An 3d array of _ elements should return the correct shape", ShapeTest) {
      val l2d = dbl3d.getAtN(0).getAtN(0).length
      val l1d = dbl3d.getAtN(0).length
      val l0d = dbl3d.length
      assert(Shape[List3d[Double]].apply(dbl3d) === l0d :: l1d :: l2d :: HNil)
    }
  }

  feature("The SetElem typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import ArrayDefs.IsArraySyntax._
    import Dummy.IsArrayImplicits._
    object SetElemTest extends Tag("SetElemTest")
    scenario("setElem on dbl3d returns an updated array", SetElemTest) {
      val exp = List3d[Double](
        dbl3d.data.updated(0, dbl3d.data(0).updated(1, dbl3d.data(0)(1).updated(2, 1)))
      )
      assert(SetElem[List3d, Double, Int :: Int :: Int :: HNil].apply(
        dbl3d, 0 :: 1 :: 2 :: HNil, 1) === exp
      )
    }
  }

  feature("The Flatten typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import ArrayDefs.IsArraySyntax._
    import Dummy.IsArrayImplicits._
    object FlattenTest extends Tag("FlattenTest")
    scenario("Flatten for a 1d array returns the correct List[T]", FlattenTest) {
      assert(Flatten[List1d, Double].apply(dbl1d) === dbl1d.data)
    }
    scenario("Flatten for a 2d array returns the correct List[T]", FlattenTest) {
      // check taken from flatten_test.ipynb
      assert(Flatten[List2d, Double].apply(dbl2d) === List(
        0.0, 0.01, 0.02, 0.03, 0.04, 0.1 , 0.11, 0.12, 0.13, 0.14, 0.2 , 0.21, 0.22, 0.23, 0.24
      ))
    }
    scenario("Flatten for a 3d array returns the correct List[T]", FlattenTest) {
      // check taken from flatten_test.ipynb
      assert(Flatten[List3d, Double].apply(dbl3d) === List(
        0.0 , 0.001, 0.002, 0.003, 0.004, 0.01 , 0.011, 0.012, 0.013,
        0.014, 0.02 , 0.021, 0.022, 0.023, 0.024, 0.1  , 0.101, 0.102,
        0.103, 0.104, 0.11 , 0.111, 0.112, 0.113, 0.114, 0.12 , 0.121,
        0.122, 0.123, 0.124)
      )
    }
  }

  feature("The Operate typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import ArrayDefs.IsArraySyntax._
    import Dummy.IsArrayImplicits._
    object OperateTest extends Tag("OperateTest")
    scenario("Operate for two same-sized 1d arrays returns a correct 1d array", OperateTest) {
      val dblMult2 = dbl1d.map(_ * 2)
      assert(OperateOpt[List1d, List1d, Double, Double].apply(
        dbl1d, dblMult2, (a, b) => a + b,
      ) === Some(dbl1d.map(_ * 3)))
    }
    scenario("Operate for two same-sized 3d arrays returns a correct 3d array", OperateTest) {
      val dblMult2 = dbl3d.map(_ * 2)
      assert(OperateOpt[List3d, List3d, Double, Double].apply(
        dbl3d, dblMult2, (a, b) => a + b,
      ) === Some(dbl3d.map(_ * 3)))
    }
    scenario("Operate for two same-sized but different data 2d arrays returns a correct 2d array", OperateTest) {
      val a1 = List2d[String](List(List("a", "b"), List("c", "d"))) 
      val a2 = List2d[Int](List(List(0, 1), List(2, 3)))
      val exp = List2d[String](List(List("a0", "b1"), List("c2", "d3")))
      assert(OperateOpt[List2d, List2d, String, Int].apply(
        a1, a2, (a, b) => a + b,
      ) === Some(exp))
    }
    scenario("Operate for a multi-elem 1d arr and a single-elem 1d arr returns a correct 1d array", OperateTest) {
      val a1 = dbl1d
      val a2 = List1d[Double](List(2.0))
      assert(OperateOpt[List1d, List1d, Double, Double].apply(
        a1, a2, (a, b) => a * b,
      ) === Some(dbl1d.map(_ * 2.0)))
    }
    scenario("Operate for a multi-elem 2d arr and a single-elem 2d arr returns a correct 2d array", OperateTest) {
      val a1 = dbl2d
      val a2 = List2d[Double](List(List(2.0)))
      assert(OperateOpt[List2d, List2d, Double, Double].apply(
        a1, a2, (a, b) => a * b,
      ) === Some(dbl2d.map(_ * 2.0)))
    }
  }

  feature("The ExpandShapeDims typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import ArrayDefs.IsArraySyntax._
    import Dummy.IsArrayImplicits._
    object ExpandShapeDimsTest extends Tag("ExpandShapeDimsTest")
    
    scenario("ExpandShapeDims is used to convert a 2d shape into a 3d shape", ExpandShapeDimsTest) {
      val act = ExpandShapeDims[Int :: Int :: HNil, _1 :: _0 :: _1 :: _0 :: HNil].apply(
        2 :: 3 :: HNil)
      val exp = 2 :: 1 :: 3 :: 1 :: HNil
      assert(act === exp)
    }
    
    scenario("ExpandShapeDims is used to convert a 1d shape into a 3d shape", ExpandShapeDimsTest) {
      val act = ExpandShapeDims[Int :: HNil, _0 :: _0 :: _1 :: HNil].apply(
        3 :: HNil)
      val exp = 1 :: 1 :: 3 :: HNil
      assert(act === exp)
    }
  }

  feature("The ExpandDims typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import ArrayDefs.IsArraySyntax._
    import Dummy.IsArrayImplicits._
    object ExpandDimsTest extends Tag("ExpandDimsTest")
    scenario("Correctly inserting at dim0 into a 1d array creates a 2d array", ExpandDimsTest) {
      val act = ExpandDims[List1d[Double], List2d[Double], Nat._0].apply(dbl1d)
      val exp = List2d[Double](List(dbl1d.data))
      assert(act.shape === 1 :: dbl1d.shape)
      assert(act === exp)
    }
    scenario("Correctly inserting at dim2 into a 1d array creates a 2d array", ExpandDimsTest) {
      val act = ExpandDims[List1d[Double], List2d[Double], Nat._1].apply(dbl1d)
      val exp = List2d[Double](dbl1d.data.map(List(_)))
      assert(act.shape === dbl1d.length :: 1 :: HNil)
      assert(act === exp)
    }
    scenario("Correctly inserting at dim1 into a 2d array creates a 3d array", ExpandDimsTest) {
      val act = ExpandDims[List2d[Double], List3d[Double], Nat._1].apply(dbl2d)
      val exp = List3d[Double](dbl2d.data.map(List(_)))    
      assert(act.shape === dbl2d.shape.at(_0) :: 1 :: dbl2d.shape.at(_1) :: HNil)
      assert(act === exp)
    }
    scenario("Correctly inserting at dim2 into a 3d array creates a 4d array", ExpandDimsTest) {
      val act = ExpandDims[List3d[Double], List4d[Double], Nat._3].apply(dbl3d)
      val exp = List4d[Double](dbl3d.data.map(_.map(_.map(List(_))))) 
      assert(act.shape === dbl3d.shape ++ (1 :: HNil))
      assert(act === exp)
    }
  }

  feature("The BroadcastShapesOpt typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import ArrayDefs.IsArraySyntax._
    import Dummy.IsArrayImplicits._
    object BroadcastShapesOptTest extends Tag("BroadcastShapesOptTest")
    scenario("Broadcasting shape from smaller to larger correctly", BroadcastShapesOptTest) {
      val act = BroadcastShapesOpt[
        Int :: HNil, Int :: Int :: HNil
      ].apply(3 :: HNil, 2 :: 3 :: HNil)
      val exp = 2 :: 3 :: HNil
      assert(act === Some(exp))
    }
    scenario("Broadcasting from equal to equal correctly", BroadcastShapesOptTest) {
      val act = BroadcastShapesOpt[
        Int :: Int :: HNil, Int :: Int :: HNil
      ].apply(2 :: 3 :: HNil, 2 :: 3 :: HNil)
      val exp = 2 :: 3 :: HNil
      assert(act === Some(exp))
    }
    scenario("Broadcasting from smaller to larger incorrectly", BroadcastShapesOptTest) {
      val act = BroadcastShapesOpt[
        Int :: HNil, Int :: Int :: HNil
      ].apply(4 :: HNil, 2 :: 3 :: HNil)
      val exp = None
      assert(act === exp)
    }
    scenario("Broadcasting from equal to equal incorrectly", BroadcastShapesOptTest) {
      val act = BroadcastShapesOpt[
        Int :: Int :: HNil, Int :: Int :: HNil
      ].apply(3 :: 2 :: HNil, 2 :: 3 :: HNil)
      val exp = None
      assert(act === exp)
    }
  }

  feature("The BroadcastOpt typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import ArrayDefs.IsArraySyntax._
    import Dummy.IsArrayImplicits._
    object BroadcastOptTest extends Tag("BroadcastOptTest")
    scenario("Broadcasting from 1d to 1d (1, L) correctly", BroadcastOptTest) {
      val shape = 1 :: dbl1d.length :: HNil
      val act = BroadcastOpt[
        List1d[Double], Int :: Int :: HNil, List2d[Double]
      ].apply(dbl1d, shape)
      val exp = List2d[Double](List(dbl1d.data))
      assert(Shape[List2d[Double]].apply(act.get) === shape)
      assert(act === Some(exp))
    }
    scenario("Broadcasting from 1d to 1d (L, 1) correctly", BroadcastOptTest) {
      val shape = dbl1d.length :: 1 :: HNil
      val act = BroadcastOpt[
        List1d[Double], Int :: Int :: HNil, List2d[Double]
      ].apply(dbl1d, shape)
      val exp = List2d[Double](dbl1d.data.map(List(_)))
      assert(Shape[List2d[Double]].apply(act.get) === shape)
      assert(act === Some(exp))
    }
    scenario("Broadcasting from 2d to 3d (D0, D1, 1) correctly", BroadcastOptTest) {
      val shape = Shape[List2d[Double]].apply(dbl2d) :+ 1
      val act = BroadcastOpt[
        List2d[Double], Int :: Int :: Int :: HNil, List3d[Double]
      ].apply(dbl2d, shape)
      val exp = List3d[Double](dbl2d.data.map(_.map(List(_))))
      assert(Shape[List3d[Double]].apply(act.get) === shape)
      assert(act === Some(exp))
    }
  }
}
