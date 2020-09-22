package sportarray

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.nscala_time.time.Imports._

import sportdate.SportDate
import sportdate.{IsSportDateInstances, IsSportDateSyntax}

import Skeleton.{PositionsData, IsIdxElemImplicits, DateType, Element, Composite}
import Skeleton.IsIdxElemImplicits._
import Skeleton.IsIdxElem
import IndicesObj._

import ListOfListsObj._

class ArraySpec extends AnyFlatSpec with Matchers {
  type Dim0T = Composite
  val dim0 = Index(
    Composite("c0"), Composite("c1")
  )
  type Dim1T = Element
  val dim1 = Index(
    Element("e0"), Element("e1"), Element("e2")
  )
  type Dim2T = DateType
  val dim2 = Index(
    SportDate.YMD(2020,8,1), SportDate.YMD(2020,8,2), SportDate.YMD(2020,8,3), SportDate.YMD(2020,8,4), SportDate.YMD(2020,8,5),
  )
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
  //"Datum" should "store ref and value" in {
    //assert(Datum[Dim2T, PositionsData](SportDate.YMD(2020,8,1), 0.1).ref == SportDate.YMD(2020,8,1))
    //assert(Datum[Dim2T, PositionsData](SportDate.YMD(2020,8,1), 0.1).value == 0.1)
  //}
  "List1d" should "implement the Is1dSpArr typeclass" in {
    import ArrayDefs._
    import ArrayDefs.Is1dSpArrSyntax._
    println(list1dIs1dSpArr[List1d[Dim2T, PositionsData], Dim2T, PositionsData])
    println(implicitly[Is1dSpArr[List1d[Dim2T, PositionsData], Dim2T, PositionsData]])
    println(implicitly[List1d[Dim2T, PositionsData] => Is1dSpArrOps[List1d[Dim2T, PositionsData], Dim2T, PositionsData]])
  }
  "Arr1d" should "return correct datum with .getElem" in {
    import ArrayDefs._
    import ArrayDefs.Is1dSpArrSyntax._
    val list1d = List1d[Dim2T, PositionsData](dim2, values1d)
    values1d.zipWithIndex.forall({case(x, i) => x == list1d.getElem(i)})
  }
  "Arr1d" should "be constructable from cons" in {
    import ArrayDefs._
    import ArrayDefs.Is1dSpArrSyntax._
    val lst = List1d[Dim2T, PositionsData](Index[Dim2T](List()), Nil: List[PositionsData#T])
    val lst1 = (dim2(0), values1d(0)) :: lst
    val lst2 = (dim2(1), values1d(1)) :: lst1
    val lst3 = (dim2(2), values1d(2)) :: lst2
    val lst4 = (dim2(3), values1d(3)) :: lst3
    val lst5 = (dim2(4), values1d(4)) :: lst4
    assert(lst5 == List1d[Dim2T, PositionsData](dim2, values1d))
  }
  //"Arr1d" should "decompose using cons" in {
    //import ArrayDefs._
    //import ArrayDefs.Is1dSpArrSyntax._
    //val lst = List1d[Dim2T, PositionsData](dim2, values1d)
    //lst match {
      //case h :: t => {assert(h == lst.head); assert(l == lst.tail)}
      //case _ => assert(false)
    //}
  //}

  "Arr1d" should "return correct datum with .iloc using Int" in {
    import ArrayDefs._
    import ArrayDefs.Is1dSpArrSyntax._
    val list1d = List1d[Dim2T, PositionsData](dim2, values1d)
    values1d.zipWithIndex.forall({case(x, i) => x == list1d.iloc(i)})
  }

  //"Arr1d" should "return correct Datum with .loc" in {
    //val arr1d = Arr1d[Dim2T, PositionsData](dim2, values1d)
    //assert(
      //arr1d.loc(dim2.vals(2)) == Some(Datum[Dim2T, PositionsData](dim2.vals(2), values1d(2)))
    //)
  //}
  //"Arr2d" should "return a correct Arr1d with .loc" in {
    //val arr2d = Arr2d[Dim1T, Dim2T, PositionsData]((dim1, dim2), values2d)
    //assert(
      //arr2d.loc(dim1.vals(2)) == Some(Arr1d[Dim2T, PositionsData](dim2, values2d(2)))
    //)
    //assert(
      //arr2d.loc(dim2.vals(1)) == Some(Arr1d[Dim1T, PositionsData](dim1, values2d.map(_(1))))
    //)
  //}
  //"Arr3d" should "return a correct Arr2d with .loc" in {
    //val arr3d = Arr3d[Dim0T, Dim1T, Dim2T, PositionsData]((dim0, dim1, dim2), values3d)
    //val dim0sliceat0 = values3d(0)
    //assert(
      //arr3d.loc(dim0.vals(0)) == Some(Arr2d[Dim1T, Dim2T, PositionsData]((dim1, dim2), dim0sliceat0))
    //)
    //val dim1sliceat1 = List(
      //List(1.1, 1.2, 1.3, 1.4, 1.5),
      //List(4.1, 4.2, 4.3, 4.4, 4.5),
    //)
    //assert(
      //arr3d.loc(dim1.vals(1)) == Some(Arr2d[Dim0T, Dim2T, PositionsData]((dim0, dim2), dim1sliceat1))
    //)
    //val dim2sliceat2 = List(
      //List(0.3, 1.3, 2.3),
      //List(3.3, 4.3, 5.3),
    //)
    //assert(
      //arr3d.loc(dim2.vals(2)) == Some(Arr2d[Dim0T, Dim1T, PositionsData]((dim0, dim1), dim2sliceat2))
    //)
  //}
}
