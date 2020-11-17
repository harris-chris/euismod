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
  }
  object Values {
    import Types._
    val values1d = List(0.1, 0.2, 0.3, 0.4, 0.5)
    val values2d = List(values1d, values1d.map(_ + 1), values1d.map(_ + 2))
    val values3d = List(
      List(
        List(0.1, 0.2, 0.3, 0.4, 0.5),
        List(1.1, 1.2, 1.3, 1.4, 1.5),
        List(2.1, 2.2, 2.3, 2.4, 2.5),
      ),
      List(
        List(3.1, 3.2, 3.3, 3.4, 3.5),
        List(4.1, 4.2, 4.3, 4.4, 4.5),
        List(5.1, 5.2, 5.3, 5.4, 5.5),
      ),
    )
    val list1d = List1d[Double](values1d)
    val list2d = List2d[Double](values2d)
    val list3d = List3d[Double](values3d)
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

  feature("IsArray.++") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    object AddTest extends Tag("AddTest")
    scenario("adding a List1d to a List1d produces a combined array", AddTest) {
      val l1 = List1d[Int](List(1, 2, 3))
      val l2 = List1d[Int](List(4, 5, 6))
      val exp = List1d[Int](List(1, 2, 3, 4, 5, 6))
      val t: List[Int] = l1.flatten
      assert(l1 ++ l2 === Some(exp))
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
      val t3 = Dummy.Types.List3d[Double](Dummy.Values.values3d)
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
      val list1d = List1d[Double](values1d)
      assert(values1d.zipWithIndex.forall({case(x, i) => x == list1d.getAtN(i)}))
    }
  }

  feature("IsArray.cons") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    scenario("IsArray objects are constructed from individual values using cons") {
      import ArrayDefs.IsArraySyntax._
      val lst0 = List1d[Double](Nil)
      val lst1 = values1d(4) :: lst0
      val lst2 = values1d(3) :: lst1
      val lst3 = values1d(2) :: lst2
      val lst4 = values1d(1) :: lst3
      val lst5 = values1d(0) :: lst4
      assert(lst5 == List1d[Double](values1d))
    }
  }
  
  feature("IsArray.getEmpty[_T]") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    object GetEmptyTest extends Tag("GetEmptyTest")
    scenario(".getEmpty[_T] is used on a 1d arraylike to create a new empty arraylike", GetEmptyTest) {
      "val l1: List1d[Int] = list1d.getEmpty[Int]" should compile
    }
    scenario(".getEmpty[_T] is used  on a 2d arraylike to create a new empty arraylike", GetEmptyTest) {
      "val l2: List2d[Int] = list2d.getEmpty[Int]" should compile
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
      val l1Arrs0: List1d[Double] :: HNil = list1d.getArraysAsc
      val l1Arrs1: List1d[Double] :: HNil = list1d.getArraysDesc
      assert(l1Arrs0 == list1d.getEmpty :: HNil)
      When(".getArrays is called on a 2d array")
      Then("It should return an empty 1d and 2d array")
      val l2Arrs: List2d[Double] :: List1d[Double] :: HNil = list2d.getArraysDesc
      assert(l2Arrs == list2d.getEmpty :: list1d.getEmpty :: HNil)
      When(".getArrays is called on a 3d array")
      Then("It should return an empty 1d, 2d and 3d array")
      val l3Arrs: List1d[Double] :: List2d[Double] :: List3d[Double] :: HNil = list3d.getArraysAsc
      assert(l3Arrs == list1d.getEmpty :: list2d.getEmpty :: list3d.getEmpty :: HNil)
    }
  }

  feature(".fromList") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    object FromListTest extends Tag("FromListTest")
    scenario("a List1d can be constructed from a list of Ts", FromListTest) {
      val ts3: List[Double] = List(1.5, 2.5, 3.5)
      assert(list1d.empty.fromElems(ts3, 3 :: HNil) match {
        case Some(l1) => l1.flatten.toList === ts3
        case None => false
      })
    }
    scenario("a List1d can be constructed from a 2d array and a list of Ts", FromListTest) {
      val ts6: List[Double] = List(1.5, 2.5, 3.5, 2.0, 3.0, 4.0)
      val l1O = list2d.empty.fromElems(ts6, 6 :: HNil)
      assert(l1O match {
        case Some(l1) => l1.flatten.toList === ts6
        case None => false
      })
    }
    scenario("a List2d can be constructed from a 2d array and a list of Ts", FromListTest) {
      val ts6: List[Double] = List(1.5, 2.5, 3.5, 2.0, 3.0, 4.0)
      val l2O = list2d.empty.fromElems(ts6, 3 :: 2 :: HNil)
      assert(l2O match {
        case Some(l2) => l2.flatten.toList === ts6
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
      val mapped = list1d.map(t => t.toInt)
      assert(mapped.shape === list1d.shape)
      assert(mapped.flatten === list1d.flatten.map(_.toInt))
    }
    scenario(".map on a 2d array returns a mapped 2d array", MapTest) {
      val mapped = list2d.map(t => t.toInt)
      assert(mapped.shape === list2d.shape)
      assert(mapped.flatten === list2d.flatten.map(_.toInt))
    }
    scenario(".map on a 3d array returns a mapped 3d array", MapTest) {
      val mapped = list3d.map(t => t.toInt)
      assert(mapped.shape === list3d.shape)
      assert(mapped.flatten === list3d.flatten.map(_.toInt))
    }
    scenario(".map double => char on a 3d array returns a mapped 3d array", MapTest) {
      val mapped = list3d.map(t => 'c')
      assert(mapped.shape === list3d.shape)
      assert(mapped.flatten === list3d.flatten.map(_ => 'c'))
    }
  }

  feature("IsArray.getILoc") {
    import ArrayDefs.IsArraySyntax._
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    object GetILocTest extends Tag("GetILocTest")
    def checkGetILocWithInt[A[_], T, _S](
      a: A[T], f:(A[T], Int) => A[T],
    ) ( implicit 
      aIsArr: IsArray[A, T] { type S = _S },
    ): Boolean = {
      import ArrayDefs.IsArraySyntax._
      (0 to a.length - 1).forall(n => f(a, n) == a.getAtN(n) :: a.empty)
    }
    def checkGetILocWithListInt[A[_], T, _S](
      a: A[T], f:(A[T], List[Int]) => A[T],
    ) (implicit 
      aIsArr: IsArray[A, T] { type S = _S },
    ): Boolean = {
      import ArrayDefs.IsArraySyntax._
      (0 to a.length - 2).forall(n => f(a, List(n, n + 1)) == { 
        a.getAtN(n) :: a.getAtN(n + 1) :: a.empty
      })
    }
    val mini3 = List1d[Int](List(1, 2, 3))
    val mini33 = List2d[Int](List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val mini233 = List3d[Int](
      List(
        List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)),
        List(List(10, 11, 12), List(13, 14, 15), List(16, 17, 18))
      )
    )
    scenario("a List[Int] is used to return the listed elements from a 1d array", GetILocTest) {
      assert(mini3(List(1, 2)) === List1d[Int](List(2, 3))) 
    }
    scenario("a List[Int] :: HNil is used to return the listed elements from a 1d array", GetILocTest, Current) {
      assert(mini33(List(1, 2) :: HNil) === List2d[Int](List(List(4, 5, 6), List(7, 8, 9)))) 
    }
    scenario("a Int is used to return the listed elements from a 3d array", GetILocTest) {
      assert(mini233(1) === mini233.getAtN(1)) 
    }
    scenario("a Int :: HNil is used to return T from a 1d array", GetILocTest) {
      assert(mini3(1 :: HNil) === mini3.getAtN(1)) 
    }
    scenario("a Int :: HNil is used to return a 2d from a 3d array", GetILocTest) {
      val l2: List2d[Int] = mini233(1 :: HNil)
      assert(l2 === mini233.getAtN(1)) 
    }
    scenario("a Int :: List[Int] :: HNil is used to return a 2d from a 3d array", GetILocTest) {
      val l2: List2d[Int] = mini233(1 :: List(0, 1) :: HNil)
      assert(l2 === mini233.getAtN(1)(List(0, 1))) 
    }
    scenario("a List[Int] :: Int :: HNil is used to return a 2d from a 3d array", GetILocTest) {
      val l2: List2d[Int] = mini233(List(0) :: 1 :: HNil)
      val l2a: List2d[Int] = mini233(0)
      val l1: List1d[Int] = l2a(1)
      assert(l2 === IsArray[List2d, Int, List1d[Int]].fromList(List(mini233.getAtN(0).getAtN(1)))) 
    }
    scenario("an HList of List[Int] is used to return the correct elements from a 3d array", GetILocTest) {
      assert(
        mini233(List(1) :: List(1, 2) :: List(0, 1) :: HNil) ===
        List3d[Int](List(List(List(13, 14), List(16, 17))))
      )
    }
    scenario("an HList of List[Int] :: Int is used to return the correct elements from a 3d array", GetILocTest) {
      assert(
        mini233(List(1) :: 2 :: HNil) ===
        List2d[Int](List(List(16, 17, 18)))
      )
    }
    scenario("apply with an HList of more elements than array dimensions will not compile", GetILocTest) {
      "mini233.apply(1 :: 1 :: 1 :: 1 :: HNil)" shouldNot compile
    }
  }

  feature("The IsArray.length method returns the length of the top dimension") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    scenario("An 1d array of y elements should return .length of y")
    {
      assert(list1d.length == list1d.data.length)
    }
    scenario("A 2d array of y elements should return .length of y")
    {
      assert(list2d.length == list2d.data.length)
    }
    scenario("A 3d array of y elements should return .length of y")
    {
      assert(list3d.length == list3d.data.length)
    }
  }

  feature("The IsArray.shape method returns an HList of the total shape of the array") {
    import Dummy.Types._
    import Dummy.Values._
    import Dummy.IsArrayImplicits._
    import ArrayDefs.IsArraySyntax._
    scenario("An 1d array of _ elements should return the correct shape") {
      assert(list1d.shape === list1d.length :: HNil)
    }
    scenario("An 2d array of _ elements should return the correct shape") {
      assert(list2d.shape === list2d.getAtN(0).length :: list2d.length :: HNil)
    }
    scenario("An 3d array of _ elements should return the correct shape") {
      val l2d = list3d.getAtN(0).getAtN(0).length
      val l1d = list3d.getAtN(0).length
      val l0d = list3d.length
      assert(list3d.shape === l2d :: l1d :: l0d :: HNil)
    }
  }

  feature("IsArray.flatten") {
    import Dummy.Types._
    import Dummy.Values._
    import ArrayDefs.IsArraySyntax._
    import Dummy.IsArrayImplicits._
    object FlattenTest extends Tag("FlattenTest")
    scenario("list1d.flatten returns the correct List[T]", FlattenTest) {
      assert(list1d.flatten === list1d.data)
    }
    scenario("list2d.flatten returns the correct List[T]", FlattenTest) {
      assert(list2d.flatten === list2d.data.flatten)
    }
    scenario("list3d.flatten returns the correct List[T]", FlattenTest) {
      assert(list3d.flatten === list3d.data.flatten.flatten)
    }
  }

  feature("IsArray.reshapes") {
    import Dummy.Types._
    import Dummy.Values._
    import ArrayDefs.IsArraySyntax._
    import Dummy.IsArrayImplicits._
    object ReshapesTest extends Tag("ReshapesTest")
    
    trait ShapeList[A, D <: HList] {
      type Out
      def shapeList(lst: Option[List[A]], dims: D): Out
    }
    object ShapeList {
      implicit def slIfNoDims[A]: ShapeList[A, HNil] { type Out = Option[A] } = new ShapeList[A, HNil] {
        type Out = Option[A]
        def shapeList(lst: Option[List[A]], dims: HNil): Out = lst.map(_(0))
      }
      implicit def slIfDims[A, D <: HList](implicit 
        sl: ShapeList[List[A], D]
      ): ShapeList[A, Int :: D] { type Out = sl.Out } = new ShapeList[A, Int :: D] {
        type Out = sl.Out
        def shapeList(lst: Option[List[A]], dims: Int :: D): Out = {
          val newLvl: Option[List[List[A]]] = addLevel(lst, dims.head, List()) 
          sl.shapeList(newLvl, dims.tail)
        }
      }
    }
    def addLevel[A](belowLst: Option[List[A]], length: Int, thisLst: List[List[A]]): Option[List[List[A]]] = 
      belowLst.flatMap( bLst => 
        bLst.length match {
          case 0 => Some(thisLst.reverse)
          case x if x >= length => {
            val (ths, rst) = bLst.splitAt(length)
            addLevel(Some(rst), length, ths :: thisLst)
          }
          case _ => None
        }
      )
    def shapeList[A, D <: HList](lst: List[A], dims: D)(implicit ev: ShapeList[A, D]): ev.Out = 
      ev.shapeList(Some(lst), dims)

    scenario("list1d.reshape(list1d.length) returns list1d", ReshapesTest) {
      assert(list1d.reshape(list1d.data.length :: HNil) == Some(list1d))
    }
    scenario("list2d.reshape(list2d.flatten.length) returns a list1d", ReshapesTest) {
      val list2dFlat = list2d.data.flatten
      assert(list2d.reshape(list2dFlat.length :: HNil) == Some(List1d[Double](list2dFlat)))
    }
    scenario("list3d.reshape(list3d.flatten.length) returns a list1d", ReshapesTest) {
      val list3dFlat = list3d.data.flatten.flatten
      assert(list3d.reshape(list3dFlat.length :: HNil) == Some(List1d[Double](list3dFlat)))
    }
    scenario("list2d.reshape(2, 8) returns None, because list2d has 15 elements", ReshapesTest) {
      assert(list2d.reshape(2 :: 8 :: HNil) === None)
    }
    scenario("list2d.reshape(5, 3) returns a 5, 3 shaped List2d", ReshapesTest) {
      val list2dFlat = list2d.data.flatten
      val list2dReshaped = shapeList(list2dFlat, 5 :: 3 :: HNil)
      assert(list2d.reshape(5 :: 3 :: HNil) === list2dReshaped.map(List2d[Double](_)))
    }
    scenario("list3d.reshape(2, 3, 6) returns a correctly shaped List3d", ReshapesTest) {
      val list3dFlat = list3d.flatten
      val list3dReshaped = shapeList(list3dFlat, 2 :: 3 :: 6 :: HNil)
      assert(list3d.reshape(2 :: 3 :: 6 :: HNil) === list3dReshaped.map(List3d[Double](_)))
    }
    scenario("list3d.reshape(15, 2) returns a correctly shaped List2d", ReshapesTest) {
      val list3dFlat = list3d.flatten
      val list3dReshaped = shapeList(list3dFlat, 15 :: 2 :: HNil)
      assert(list3d.reshape(15 :: 2 :: HNil) === list3dReshaped.map(List2d[Double](_)))
    }
    scenario("list3d.reshape(1, 1, 1, 1) does not compile", ReshapesTest) {
      "list3d.reshape(1 :: 1 :: 1 :: 1 :: HNil)" shouldNot compile
    }
  }

  //feature("The IsUpdatable Typeclass") { 
    //import Dummy.Types._
    //import Dummy.Values._
    //import ArrayDefs.IsArraySyntax._
    //import ArrayDefs.IsUpdatableSyntax._
    //scenario("An Updatable array can implement IsUpdatable") {
      //{
        //When("An implicit conversion to IsUpdatable is in scope")
        //implicit def list1dIsUpdatable[T: IsElement] = IsUpdatable[List1d, T, List1d[T]] (
          //fgetEmpty = self => List1d[T](List()),
          //fgetAtN = (self, n) => self.data(n),
          //flength = self => self.data.length,
          //fcons = (self, elem) => List1d(elem :: self.data),
        //)
        //Then("Implicit conversion should occur")
        //"implicitly[List1d[Double] => IsUpdatableOps[List1d[Double], Double]]" should compile
      //}
      //{
        //When("An implicit conversion to IsUpdatable is in scope, based on IsArray")
        //import Dummy.IsArrayImplicits._
        //implicit def list1dIsUpdatable[T: IsElement] = IsUpdatable.fromArray[List1d[T], T, List1d]
        //Then("Implicit conversion should occur")
        //"implicitly[List1d[Double] => IsUpdatableOps[List1d[Double], Double]]" should compile
      //}
    //}
    //scenario("IsUpdatable is used with .map for a 1d Array") {
      //import Dummy.IsArrayImplicits._
      //import Dummy.IsUpdatableImplicits._
      //implicit val intIsElement: IsElement[Int] = new IsElement[Int] {}
      //the[IsUpdatable[List1d[Int] { type E = Int }]]
      //ArrMap.mapIfEIsBase[List1d[Double], Double, Int]
      //val list1dInt = list1d.map((d: Double) => d.toInt)
    //}
  //}

  //feature("An Updatable array can be updated with .setAtN") {
    //import Dummy._
    //When(".setAtN with a 1d Array")
    //implicit def list1dIsUpdatable[T: IsElement] = IsUpdatable[List1d[T], T](
      //fsetAtN = (self, n, setTo) => List1d(self.data.updated(n, setTo))
    //)
    //scenario("Using.setAtN with a new valid value creates a new array of the same type") {
      //val t1 = list1d.setAtN(1, 2.2) 
      //assert(t1.data(1) == 2.2)
    //}
    //scenario("Using setAtN with an invalid value does not compile") {
      //"list1d.setAtN(1, 'c')" shouldNot typeCheck
    //}
  //}

  //feature("An Updatable array can be updated with .setILoc") {
    //scenario("A 1d array returns a same-size 1d array if .setILoc is used") {
    //}
  //}
//}


  //"Arr1d" should "have shape(0) == length" in {
    //import ArrayDefs.IsSpArrSyntax._
    //val list1d = List1d[Dim2T, Double](dim2, values1d)
    //assert(list1d.length == values1d.length)
    //assert(list1d.length == dim2.length)
  //}
  //"Arr1d" should "use fMap to apply functions to its elements" in {
    //import ArrayDefs.IsSpArrSyntax._
    //val list1d = List1d[Dim2T, Double](dim2, values1d)
    //val listmapped = list1d.fmap(_: Double => 'a')
    //val c = list1d.fmap(_: String => 'a')
    //assert(listmapped.toList.forall(_ == 'a'))
  //}
  ////"Arr1d" should "return correct Datum with .loc" in {
    ////import ArrayDefs._
    ////import ArrayDefs.IsSpArrSyntax._
    ////val list1d = List1d[PositionsData, Dim2T](dim2, values1d)
    ////assert(
      ////dim2.toList.zip(values1d).forall({case(d, v) => list1d.loc(d) == Some(v)})
    ////)
  ////}
  ////"Arr2d" should "return a 1d array with .iloc using Int on the second dimension" in {
    ////import ArrayDefs._
    ////import ArrayDefs.Is2dSpArrSyntax._
    ////val list2d = List2d[Dim1T, Dim2T, PositionsData]((dim1, dim2), values2d)
    ////assert(
      ////values2d.zipWithIndex.forall({case(x, i) => 
        ////list2d.iloc(i) == List1d[Dim2T, PositionsData](dim2, x)
      ////})
    ////)
  ////}
  ////"Arr2d" should "return a correct Arr1d with .loc" in {
    ////val arr2d = Arr2d[Dim1T, Dim2T, PositionsData]((dim1, dim2), values2d)
    ////assert(
      ////arr2d.loc(dim1.vals(2)) == Some(Arr1d[Dim2T, PositionsData](dim2, values2d(2)))
    ////)
    ////assert(
      ////arr2d.loc(dim2.vals(1)) == Some(Arr1d[Dim1T, PositionsData](dim1, values2d.map(_(1))))
    ////)
  ////}
  ////"Arr3d" should "return a correct Arr2d with .loc" in {
    ////val arr3d = Arr3d[Dim0T, Dim1T, Dim2T, PositionsData]((dim0, dim1, dim2), values3d)
    ////val dim0sliceat0 = values3d(0)
    ////assert(
      ////arr3d.loc(dim0.vals(0)) == Some(Arr2d[Dim1T, Dim2T, PositionsData]((dim1, dim2), dim0sliceat0))
    ////)
    ////val dim1sliceat1 = List(
      ////List(1.1, 1.2, 1.3, 1.4, 1.5),
      ////List(4.1, 4.2, 4.3, 4.4, 4.5),
    ////)
    ////assert(
      ////arr3d.loc(dim1.vals(1)) == Some(Arr2d[Dim0T, Dim2T, PositionsData]((dim0, dim2), dim1sliceat1))
    ////)
    ////val dim2sliceat2 = List(
      ////List(0.3, 1.3, 2.3),
      ////List(3.3, 4.3, 5.3),
    ////)
    ////assert(
      ////arr3d.loc(dim2.vals(2)) == Some(Arr2d[Dim0T, Dim1T, PositionsData]((dim0, dim1), dim2sliceat2))
    ////)
  ////}
}
