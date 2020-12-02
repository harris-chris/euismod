package sportarray

import org.scalatest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.featurespec.{AnyFeatureSpec}
import com.github.nscala_time.time.Imports._

import sportdate.SportDate
import sportdate.{IsSportDateInstances, IsSportDateSyntax}

import Skeleton.{IsBase}
import IndicesObj._

import shapeless._
import shapeless.{HList, HNil, Lazy, :: => #:}
import shapeless.ops.nat.{GT, GTEq, Pred, Diff => NatDiff, ToInt}
//import shapeless.test.{illTyped}
import shapeless.ops.hlist._

object Dummy {
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
        def cons(a: A2[T], sub: S) = A2[T](a1ev.toList(sub) :: a.data)
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

  feature("The DepthCT typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object DepthCTTest extends Tag("DepthCTTest")
    scenario("DepthCT has type Out of Nat 1 for a 1d array", DepthCTTest) {
      "implicitly[DepthCT[List1d[Double]] { type Out = Nat._1 }]" should compile
    }
    scenario("DepthCT has type Out of Nat 2 for a 2d array", DepthCTTest) {
      "implicitly[DepthCT[List2d[Double]] { type Out = Nat._2 }]" should compile
    }
    scenario("DepthCT has type Out of Nat 3 for a 3d array", DepthCTTest) {
      "implicitly[DepthCT[List3d[Double]] { type Out = Nat._3 }]" should compile
    }
  }

  feature("The PrettyPrint typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object PrettyPrintTest extends Tag("PrettyPrintTest")
    scenario("Pretty printing a 1d array produces numpy-like output", PrettyPrintTest) {
      val pp = PrettyPrint[List1d, Double].apply(dbl1d)
    }
    scenario("Pretty printing a 2d array produces numpy-like output", PrettyPrintTest) {
      val pp = PrettyPrint[List2d, Double].apply(dbl2d)
    }
    scenario("Pretty printing a 3d array produces numpy-like output", PrettyPrintTest) {
      val pp = PrettyPrint[List3d, Double].apply(dbl3d)
    }
    scenario("Pretty printing a 4d array produces numpy-like output", PrettyPrintTest) {
      val pp = PrettyPrint[List4d, Double].apply(dbl4d)
    }
  }

  feature("The TransposeDT typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object TransposeDTTest extends Tag("TransposeDTTest")
    scenario("transposing a 2d array with AllSlice correctly flips the axes", TransposeDTTest) {
      val act = TransposeDT[List2d[Double], AllSlice].apply(dbl2d)
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
    scenario("transposing a 3d array with AllSlice correctly flips the axes", TransposeDTTest) {
      val act = TransposeDT[List3d[Double], AllSlice].apply(dbl3d)
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
    scenario("transposing a 4d array with AllSlice correctly flips the axes", TransposeDTTest) {
      val act = TransAllDT[List4d[Double], Nat._0, Nat._3].apply(dbl4d)
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
    scenario("Transposing a 2d array along axes 0/1 matches with numpy", TransposeDTTest) {
      val act = TransposeDT[List2d[Double], (Nat._0, Nat._1)].apply(dbl2d)
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
    scenario("Transposing a 3d array along axes 0/1 matches with numpy", TransposeDTTest) {
      val exp = List3d[Double](
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
      val act = TransposeDT[List3d[Double], (Nat._0, Nat._1)].apply(dbl3d)
      assert(act === exp)
    }
    scenario("Transposing a 3d array along axes 1/2 matches with numpy", TransposeDTTest) {
      val exp = List3d[Double](
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
      val act = TransposeDT[List3d[Double], (Nat._1, Nat._2)].apply(dbl3d)
      assert(act === exp)
    }
    scenario("Transposing a 4d array along axes 2/3 matches with numpy", TransposeDTTest, Current) {
      //val act = TransposeDT[List4d[Double], (Nat._2, Nat._3)].apply(dbl4d)
      val act = TransAxDT[List4d[Double], Nat._2, Nat._3].apply(dbl4d)
      val exp = List4d[Double](
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
    scenario("Transposing a 3d array along 1/2 then 0/1 is the same as a full transpose", TransposeDTTest) {
      val st1 = TransposeDT[List3d[Double], (Nat._0, Nat._1)].apply(dbl3d)
      val st2 = TransposeDT[List3d[Double], (Nat._1, Nat._2)].apply(st1)
      val act = TransposeDT[List3d[Double], (Nat._0, Nat._1)].apply(st2)
      val exp = TransposeDT[List3d[Double], AllSlice].apply(dbl3d)
      assert(act === exp)
    }
    scenario("Transposing a 4d array 0/1, 1/2, 2/3, 0/1, 1/2, 0/1 is the same as a full transpose", TransposeDTTest) {
      val st1 = TransposeDT[List4d[Double], (Nat._0, Nat._1)].apply(dbl4d)
      val st2 = TransposeDT[List4d[Double], (Nat._1, Nat._2)].apply(st1)
      val st3 = TransposeDT[List4d[Double], (Nat._2, Nat._3)].apply(st2)
      val st4 = TransposeDT[List4d[Double], (Nat._0, Nat._1)].apply(st3)
      val st5 = TransposeDT[List4d[Double], (Nat._1, Nat._2)].apply(st4)
      val act = TransposeDT[List4d[Double], (Nat._0, Nat._1)].apply(st5)
      val exp = TransposeDT[List4d[Double], AllSlice].apply(dbl4d)
      assert(act === exp)
    }
  }

  feature("The ConcatenateRT typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object ConcatenateRTTest extends Tag("ConcatenateRTTest")
    scenario("Concatenating a 3d array along dimension 0 returns the correct result", ConcatenateRTTest) {
      val cn = ConcatenateRT[List3d, List3d, Int]
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
    scenario("Concatenating a 3d array along dimension 1 returns the correct result", ConcatenateRTTest) {
      val cn = ConcatenateRT[List3d, List3d, Int]
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
    scenario("Concatenating a 3d array along dimension 2 returns the correct result", ConcatenateRTTest) {
      val cn = ConcatenateRT[List3d, List3d, Int]
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

  feature("The AddRT typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object AddRTTest extends Tag("AddRTTest")
    scenario("adding a List1d to a List1d produces a combined array", AddRTTest) {
      val l1 = List1d[Int](List(1, 2, 3))
      val l2 = List1d[Int](List(4, 5, 6))
      val exp = List1d[Int](List(1, 2, 3, 4, 5, 6))
      val t: List[Int] = l1.flatten
      assert(AddRT[List1d, List1d, Int].apply(l1, l2) === Some(exp))
    }
    scenario("adding a List2d to a List2d returns a combined array", AddRTTest) {
      val l1 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6)))
      val l2 = List2d[Int](List(List(7, 8, 9), List(10, 11, 12)))
      val exp = List2d[Int](List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9), List(10, 11, 12)))
      assert(AddRT[List2d, List2d, Int].apply(l1, l2) === Some(exp))
    }
    scenario("adding a List2d to a List2d with different dim0 size returns a combined array", AddRTTest) {
      val l1 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6)))
      val l2 = List2d[Int](List(List(7, 8, 9)))
      val exp = List2d[Int](List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
      assert(AddRT[List2d, List2d, Int].apply(l1, l2) === Some(exp))
    }
    scenario("adding a List2d to a List2d with different dim1 length returns None", AddRTTest) {
      val l1 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6)))
      val l2 = List2d[Int](List(List(7, 8), List(10, 11)))
      assert(AddRT[List2d, List2d, Int].apply(l1, l2) === None)
    }
    scenario("adding a List3d to a List3d with dim1 and dim2 lengths the same returns a combined array", AddRTTest) {
      val l1 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6)))
      val l2 = List2d[Int](List(List(7, 8), List(10, 11)))
      assert(AddRT[List2d, List2d, Int].apply(l1, l2) === None)
    }
    scenario("adding a List3d to a List3d with different dim1 length returns None", AddRTTest) {
      val l1 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6)))
      val l2 = List2d[Int](List(List(7, 8), List(10, 11)))
      assert(AddRT[List2d, List2d, Int].apply(l1, l2) === None)
    }
  }

  feature("MaskFromNumSeqDT typeclass") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object MaskFromNumSeqDTTest extends Tag("MaskFromNumSeqDTTest")
    scenario("MaskDTFromNumSeq returns a correct mask for a 1d array", MaskFromNumSeqDTTest) {
      val allFalse = dbl1d.map(_ => false)
      assert(
        MaskFromNumSeqDT[List1d[Boolean], List[Int] :: HNil].apply(List(1, 2) :: HNil, allFalse) ===
        List1d[Boolean](allFalse.data.updated(1, true).updated(2, true))
      )
    }
    scenario("MaskFromNumSeqDT returns a correct mask for a 2d array", MaskFromNumSeqDTTest) {
      val allFalse = dbl2d.map(_ => false)
      val exp = List2d[Boolean](
        List(
          List(false, false, false, false, false),
          List(false, false, false, true, true),
          List(false, false, false, true, true),
        )
      )
      assert(
        MaskFromNumSeqDT[List2d[Boolean], List[Int] :: List[Int] :: HNil].apply(
          List(1, 2) :: List(3,4) :: HNil, allFalse) === exp
      )
    }
    scenario("MaskFromNumSeqDT returns a correct mask for a 3d array", MaskFromNumSeqDTTest) {
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
        MaskFromNumSeqDT[List3d[Boolean], List[Int] :: List[Int] :: List[Int] :: HNil].apply(
          List(0) :: List(0,2) :: List(2, 3) :: HNil, allFalse) === exp
      )
    }
  }

  feature("IsXd typeclass") {
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
    scenario("An Is2d arraylike returns a value") {
      Given("An 2-d arraylike which returns a 1-d arraylike")
      When("Implementation of Is2d is attempted without Is1d implemented for the 1-d array")
      Then("It should fail to compile")
      "implicit def a1Of1Is2d[T] = Is2d[A1OfA1, T, A1[T]]" shouldNot typeCheck
      
      When("Implementation of Is2d is attempted with Is1d implemented for the 1-d array")
      Then("It should compile")
      implicit def a1Is1d[T] = Is1d[A1, T] 
      "implicit def a1Of1Is2d[T] = Is2d[A1OfA1, T, A1[T]]" should compile
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
    implicit def a1Is1d[T] = Is1d[A1, T] 
    implicit def a1Of1Is2d[T] = Is2d[A1OfA1, T, A1[T]]
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

  feature("IsArray.getArrays") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    scenario(".getArrays is called") {
      When(".getArrays is called on a 1d array")
      Then("It should return an empty version of itself")
      val l1Arrs0: List1d[Double] :: HNil = dbl1d.getArraysAsc
      val l1Arrs1: List1d[Double] :: HNil = dbl1d.getArraysDesc
      assert(l1Arrs0 == dbl1d.getEmpty :: HNil)
      When(".getArrays is called on a 2d array")
      Then("It should return an empty 1d and 2d array")
      val l2Arrs: List2d[Double] :: List1d[Double] :: HNil = dbl2d.getArraysDesc
      assert(l2Arrs == dbl2d.getEmpty :: dbl1d.getEmpty :: HNil)
      When(".getArrays is called on a 3d array")
      Then("It should return an empty 1d, 2d and 3d array")
      val l3Arrs: List1d[Double] :: List2d[Double] :: List3d[Double] :: HNil = dbl3d.getArraysAsc
      assert(l3Arrs == dbl1d.getEmpty :: dbl2d.getEmpty :: dbl3d.getEmpty :: HNil)
    }
  }

  feature("The FromElemsDT typeclass") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    object FromElemsDTTest extends Tag("FromElemsDTTest")
    scenario("a List1d can be constructed from a list of Ts", FromElemsDTTest) {
      val ts3: List[Double] = List(1.5, 2.5, 3.5)
      val newShape = 3 :: HNil
      val ga = GetArrsAsc[List1d, Double, HNil]
      val fe = FromElemsDT[Double, ga.Out, Int :: HNil, Nat._0]
      val act = fe(ts3, ga(HNil), newShape)
      assert(act match {
        case Some(l1) => {
          l1.flatten.toList === ts3
        }
        case None => false
      })
    }
    scenario("a List2d can be constructed from a list of Ts", FromElemsDTTest) {
      val ts3: List[Double] = List(0.00, 0.01, 0.02, 0.10, 0.11, 0.12)
      val newShape = 2 :: 3 :: HNil
      val ga = GetArrsAsc[List2d, Double, HNil]
      val fe = FromElemsDT[Double, ga.Out, Int :: Int :: HNil, Nat._0]
      val actO = fe(ts3, ga(HNil), newShape)
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
    scenario("a List3d can be constructed from a list of Ts", FromElemsDTTest) {
      val ts3: List[Double] = List(
        0.000, 0.001, 0.002, 0.010, 0.011, 0.012, 0.100, 0.101, 0.102, 0.110, 0.111, 0.112,
      )
      val newShape = 2 :: 2 :: 3 :: HNil
      val ga = GetArrsAsc[List3d, Double, HNil]
      val fe = FromElemsDT[Double, ga.Out, Int :: Int :: Int :: HNil, Nat._0]
      val actO = fe(ts3, ga(HNil), newShape)
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
    scenario("a List1d can be constructed from a 2d array and a list of Ts", FromElemsDTTest) {
      val ts6: List[Double] = List(1.5, 2.5, 3.5, 2.0, 3.0, 4.0)
      val newShape = 6 :: HNil
      val ga = GetArrsAsc[List2d, Double, HNil]
      val fe = FromElemsDT[Double, ga.Out, Int :: HNil, Nat._0]
      val act = fe(ts6, ga(HNil), newShape)
      assert(act match {
        case Some(l1) => l1.flatten.toList === ts6
        case None => false
      })
    }
    scenario("a List2d can be constructed from a 2d array and a list of Ts", FromElemsDTTest) {
      val ts6: List[Double] = List(1.5, 2.5, 3.5, 2.0, 3.0, 4.0)
      val newShape = 3 :: 2 :: HNil
      val ga: GetArrsAsc[List2d, Double, HNil] { type Out = List1d[Double] :: List2d[Double] :: HNil } = 
        GetArrsAsc[List2d, Double, HNil]
      val fe = FromElemsDT[Double, ga.Out, Int :: Int :: HNil, Nat._0]
      val act = fe(ts6, ga(HNil), newShape)
      assert(act match {
        case Some(l2) => {
          l2.flatten.toList === ts6 && l2.shape === newShape
        }
        case None => false
      })
    }
  }

  //feature("IsArray.map") {
    //import ArrayDefs.IsArraySyntax._
    //import Dummy.Types._
    //import Dummy.Values._
    //import Dummy.IsArrayImplicits._
    //object MapTest extends Tag("MapTest")
    //scenario(".map on a 1d array returns a mapped 1d array", MapTest) {
      //val mapped = dbl1d.map(t => t.toInt)
      //assert(mapped.shape === dbl1d.shape)
      //assert(mapped.flatten === dbl1d.flatten.map(_.toInt))
    //}
    //scenario(".map on a 2d array returns a mapped 2d array", MapTest) {
      //val mapped = dbl2d.map(t => t.toInt)
      //assert(mapped.shape === dbl2d.shape)
      //assert(mapped.flatten === dbl2d.flatten.map(_.toInt))
    //}
    //scenario(".map on a 3d array returns a mapped 3d array", MapTest) {
      //val mapped = dbl3d.map(t => t.toInt)
      //assert(mapped.shape === dbl3d.shape)
      //assert(mapped.flatten === dbl3d.flatten.map(_.toInt))
    //}
    //scenario(".map double => char on a 3d array returns a mapped 3d array", MapTest) {
      //val mapped = dbl3d.map(t => 'c')
      //assert(mapped.shape === dbl3d.shape)
      //assert(mapped.flatten === dbl3d.flatten.map(_ => 'c'))
    //}
  //}

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

  feature("The IsArray.shape method returns an HList of the total shape of the array") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object ShapeTest extends Tag("ShapeTest")
    scenario("An 1d array of _ elements should return the correct shape", ShapeTest) {
      assert(dbl1d.shape === dbl1d.length :: HNil)
    }
    scenario("An 2d array of _ elements should return the correct shape", ShapeTest) {
      assert(dbl2d.shape === dbl2d.length :: dbl2d.getAtN(0).length :: HNil)
    }
    scenario("An 3d array of _ elements should return the correct shape", ShapeTest) {
      val l2d = dbl3d.getAtN(0).getAtN(0).length
      val l1d = dbl3d.getAtN(0).length
      val l0d = dbl3d.length
      assert(dbl3d.shape === l0d :: l1d :: l2d :: HNil)
    }
  }

  feature("IsArray.setAtN") {
    import Dummy.Types._
    import Dummy.Values._
    import ArrayDefs.IsArraySyntax._
    import Dummy.IsArrayImplicits._
    object SetAtNTest extends Tag("SetAtNTest")
    scenario("dbl1d.setAtN returns the correct List[T]", SetAtNTest) {
      assert(dbl1d.setAtN(1, 0.01) === List1d(dbl1d.data.updated(1, 0.01)))
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
    scenario("dbl1d.flatten returns the correct List[T]", FlattenTest) {
      assert(Flatten[List1d, Double].apply(dbl1d) === dbl1d.data)
    }
    scenario("dbl2d.flatten returns the correct List[T]", FlattenTest) {
      // check taken from flatten_test.ipynb
      assert(Flatten[List2d, Double].apply(dbl2d) === List(
        0.0, 0.01, 0.02, 0.03, 0.04, 0.1 , 0.11, 0.12, 0.13, 0.14, 0.2 , 0.21, 0.22, 0.23, 0.24
      ))
    }
    scenario("dbl3d.flatten returns the correct List[T]", FlattenTest) {
      // check taken from flatten_test.ipynb
      assert(Flatten[List3d, Double].apply(dbl3d) === List(
        0.0 , 0.001, 0.002, 0.003, 0.004, 0.01 , 0.011, 0.012, 0.013,
        0.014, 0.02 , 0.021, 0.022, 0.023, 0.024, 0.1  , 0.101, 0.102,
        0.103, 0.104, 0.11 , 0.111, 0.112, 0.113, 0.114, 0.12 , 0.121,
        0.122, 0.123, 0.124)
      )
    }
  }

  //feature("IsArray.reshapes") {
    //import Dummy.Types._
    //import Dummy.Values._
    //import ArrayDefs.IsArraySyntax._
    //import Dummy.IsArrayImplicits._
    //object ReshapesTest extends Tag("ReshapesTest")
    
    //trait ShapeList[A, D <: HList] {
      //type Out
      //def shapeList(lst: Option[List[A]], dims: D): Out
    //}
    //object ShapeList {
      //implicit def slIfNoDims[A]: ShapeList[A, HNil] { type Out = Option[A] } = new ShapeList[A, HNil] {
        //type Out = Option[A]
        //def shapeList(lst: Option[List[A]], dims: HNil): Out = lst.map(_(0))
      //}
      //implicit def slIfDims[A, D <: HList](implicit 
        //sl: ShapeList[List[A], D]
      //): ShapeList[A, Int :: D] { type Out = sl.Out } = new ShapeList[A, Int :: D] {
        //type Out = sl.Out
        //def shapeList(lst: Option[List[A]], dims: Int :: D): Out = {
          //val newLvl: Option[List[List[A]]] = addLevel(lst, dims.head, List()) 
          //sl.shapeList(newLvl, dims.tail)
        //}
      //}
    //}
    //def addLevel[A](belowLst: Option[List[A]], length: Int, thisLst: List[List[A]]): Option[List[List[A]]] = 
      //belowLst.flatMap( bLst => 
        //bLst.length match {
          //case 0 => Some(thisLst.reverse)
          //case x if x >= length => {
            //val (ths, rst) = bLst.splitAt(length)
            //addLevel(Some(rst), length, ths :: thisLst)
          //}
          //case _ => None
        //}
      //)
    //def shapeList[A, D <: HList](lst: List[A], dims: D)(implicit ev: ShapeList[A, D]): ev.Out = 
      //ev.shapeList(Some(lst), dims)

    //scenario("dbl1d.reshape(dbl1d.length) returns dbl1d", ReshapesTest) {
      //assert(dbl1d.reshape(dbl1d.data.length :: HNil) == Some(dbl1d))
    //}
    //scenario("dbl2d.reshape(dbl2d.flatten.length) returns a dbl1d", ReshapesTest) {
      //val dbl2dFlat = dbl2d.data.flatten
      //assert(dbl2d.reshape(dbl2dFlat.length :: HNil) == Some(List1d[Double](dbl2dFlat)))
    //}
    //scenario("dbl3d.reshape(dbl3d.flatten.length) returns a dbl1d", ReshapesTest) {
      //val dbl3dFlat = dbl3d.data.flatten.flatten
      //assert(dbl3d.reshape(dbl3dFlat.length :: HNil) == Some(List1d[Double](dbl3dFlat)))
    //}
    //scenario("dbl2d.reshape(2, 8) returns None, because dbl2d has 15 elements", ReshapesTest) {
      //assert(dbl2d.reshape(2 :: 8 :: HNil) === None)
    //}
    //scenario("dbl2d.reshape(5, 3) returns a 5, 3 shaped List2d", ReshapesTest) {
      //val dbl2dFlat = dbl2d.data.flatten
      //val dbl2dReshaped = shapeList(dbl2dFlat, 3 :: 5 :: HNil)
      //assert(dbl2d.reshape(5 :: 3 :: HNil) === dbl2dReshaped.map(List2d[Double](_)))
    //}
    //scenario("dbl3d.reshape(2, 3, 6) returns a correctly shaped List3d", ReshapesTest) {
      //val dbl3dFlat = dbl3d.flatten
      //val dbl3dReshaped = shapeList(dbl3dFlat, 6 :: 3 :: 2 :: HNil)
      //assert(dbl3d.reshape(2 :: 3 :: 6 :: HNil) === dbl3dReshaped.map(List3d[Double](_)))
    //}
    //scenario("dbl3d.reshape(15, 2) returns a correctly shaped List2d", ReshapesTest) {
      //val dbl3dFlat = dbl3d.flatten
      //val dbl3dReshaped = shapeList(dbl3dFlat, 2 :: 15 :: HNil)
      //assert(dbl3d.reshape(15 :: 2 :: HNil) === dbl3dReshaped.map(List2d[Double](_)))
    //}
    //scenario("dbl3d.reshape(1, 1, 1, 1) does not compile", ReshapesTest) {
      //"dbl3d.reshape(1 :: 1 :: 1 :: 1 :: HNil)" shouldNot compile
    //}
  //}
}
