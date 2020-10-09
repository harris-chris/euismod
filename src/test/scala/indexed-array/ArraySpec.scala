package sportarray

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.github.nscala_time.time.Imports._

import sportdate.SportDate
import sportdate.{IsSportDateInstances, IsSportDateSyntax}

import Skeleton.{PositionsData, DateType, Element, Composite, DataType}
import IndicesObj._
import shapeless._
import shapeless.ops.hlist._

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
  def checkList1dWithSingle[I, T: DataType](
    idx: Index[I], data: List[T], f:(List1d[I, T], Int) => List1d[I, T],
  ): Boolean = {
    import ArrayDefs.IsSpArrSyntax._
    data.zipWithIndex.forall({case(x, i) => f(List1d[I, T](idx, data),i) == 
      List1d[I, T](Index(idx(i)), List(x))
    })
  }
  def checkList1dWithListInt[I, T: DataType](
    idx: Index[I], data: List[T], f:(List1d[I, T], List[Int]) => List1d[I, T],
  ): Boolean = {
    import ArrayDefs.IsSpArrSyntax._
    val list1d = List1d[I, T](idx, data)
    data.zipWithIndex.forall(
      {
        case(x, i) => i match {
          case i if i < 4 => {
            val actual = f(list1d, List(i, i+1))
            val expected = List1d[I, T](
              Index(List(idx(i), idx(i+1))), List(data(i), data(i+1))
            )
            actual == expected
          }
          case i if i == 4 => true
        }
      }
    )
  }
  //"Datum" should "store ref and value" in {
    //assert(Datum[Dim2T, PositionsData](SportDate.YMD(2020,8,1), 0.1).ref == SportDate.YMD(2020,8,1))
    //assert(Datum[Dim2T, PositionsData](SportDate.YMD(2020,8,1), 0.1).value == 0.1)
  //}
  "List1d" should "implement the Is1dSpArr typeclass" in {
    import ArrayDefs._
    import ArrayDefs.IsSpArrSyntax._
    list1dIsSpArr[List1d[Dim2T, Double], Dim2T, Double]
    //println(implicitly[Is1dSpArr[List1d[PositionsData, Dim2T], PositionsData, Dim2T]])
    //println(implicitly[List1d[PositionsData, Dim2T] => Is1dSpArrOps[List1d[PositionsData, Dim2T], PositionsData, Dim2T]])
  }
  "Arr1d" should "return correct datum with .getElem" in {
    import ArrayDefs.IsSpArrSyntax._
    val list1d = List1d[Dim2T, Double](dim2, values1d)
    assert(values1d.zipWithIndex.forall({case(x, i) => x == list1d.getElem(i)}))
  }
  "Arr1d" should "be constructable from cons" in {
    import ArrayDefs.IsSpArrSyntax._
    val lst0 = List1d[Dim2T, Double](Index[Dim2T](Nil), Nil)
    val lst1 = (dim2(0), values1d(0)) :: lst0
    val lst2 = (dim2(1), values1d(1)) :: lst1
    val lst3 = (dim2(2), values1d(2)) :: lst2
    val lst4 = (dim2(3), values1d(3)) :: lst3
    val lst5 = (dim2(4), values1d(4)) :: lst4
    assert(lst5 == List1d[Dim2T, Double](dim2, values1d))
  }
  ////"Arr1d" should "decompose using cons" in {
    ////import ArrayDefs._
    ////import ArrayDefs.IsSpArrSyntax._
    ////val lst = List1d[Dim2T, PositionsData](dim2, values1d)
    ////lst match {
      ////case h :: t => {assert(h == lst.head); assert(l == lst.tail)}
      ////case _ => assert(false)
    ////}
  ////}
  "Arr1d" should "return correct datum with .iloc using Int" in {
    import ArrayDefs.IsSpArrSyntax._
    assert(checkList1dWithSingle[Dim2T, Double](dim2, values1d, (l, i) => l.iloc(i)))
  }
  "Arr1d" should "return correct 1dSpArr with .iloc using List[Int]" in {
    import ArrayDefs.IsSpArrSyntax._
    assert(checkList1dWithListInt[Dim2T, Double](dim2, values1d, (l, r) => l.iloc(r)))
  }
  "Arr1d" should "return all data with .iloc using null" in {
    import ArrayDefs.IsSpArrSyntax._
    val list1d = List1d[Dim2T, Double](dim2, values1d)
    assert(list1d.iloc(null) == list1d)
  }
  "Arr1d" should "return the appropriate data with .iloc using an HList of Ints" in {
    import ArrayDefs.IsSpArrSyntax._
    //implicitly[IsSpArr[List1d[PositionsData, Dim2T], _, Dim2T]]
    //implicitly[ILoc[Int, List1d[PositionsData, Dim2T], Dim2T]]
    //implicitly[ILoc[HNil, List1d[PositionsData, Dim2T], Dim2T]]
    assert(
      checkList1dWithSingle[Dim2T, Double](dim2, values1d, (l, i) => l.iloc(i :: HNil))
    )
  }
  "Arr1d" should "return the appropriate data with .iloc using an HList of List[Int]" in {
    import ArrayDefs.IsSpArrSyntax._
    assert(
      checkList1dWithListInt[Dim2T, Double](dim2, values1d, (l, i) => l.iloc(i :: HNil))
    )
  }
  "Arr1d" should "have shape(0) == length" in {
    import ArrayDefs.IsSpArrSyntax._
    val list1d = List1d[Dim2T, Double](dim2, values1d)
    assert(list1d.length == values1d.length)
    assert(list1d.length == dim2.length)
  }
  "Arr2d" should "return a 1d array with .getElem" in {
    import ArrayDefs._
    import ArrayDefs.IsSpArrSyntax._
    val list2d = List2d[Dim1T, Dim2T, Double]((dim1, dim2), values2d)
    assert(
      values2d.zipWithIndex.forall({case(x, i) => 
        list2d.getElem(i) == List1d[Dim2T, Double](dim2, x)
      })
    )
  }
  "Arr2d" should "return a 2d array with .iloc using Int on the first dimension" in {
    import ArrayDefs._
    import ArrayDefs.IsSpArrSyntax._
    val list2d = List2d[Dim1T, Dim2T, Double]((dim1, dim2), values2d)
    println(list2d.iloc(0))
    println(List2d[Dim1T, Dim2T, Double]((dim1, dim2), List(values2d(0))))
    //assert(
      //values2d.zipWithIndex.forall({case(x, i) => 
        //list2d.iloc(i) == List2d[PositionsData, Dim1T, Dim2T]((dim1, dim2), List(values2d(i)))
      //})
    //)
  }
  //"Arr2d" should "return a 2d array with .iloc using Int on the first dimension" in {
    //import ArrayDefs._
    //import ArrayDefs.IsSpArrSyntax._
    //val list2d = List2d[PositionsData, Dim1T, Dim2T]((dim1, dim2), values2d)
    //assert(
      //values2d.zipWithIndex.forall({case(x, i) => 
        //list2d.iloc(i) == List2d[PositionsData, Dim1T, Dim2T]((dim1, dim2), List(values2d(i)))
      //})
    //)
  //}
  //"Arr1d" should "return correct Datum with .loc" in {
    //import ArrayDefs._
    //import ArrayDefs.IsSpArrSyntax._
    //val list1d = List1d[PositionsData, Dim2T](dim2, values1d)
    //assert(
      //dim2.toList.zip(values1d).forall({case(d, v) => list1d.loc(d) == Some(v)})
    //)
  //}
  //"Arr2d" should "return a 1d array with .iloc using Int on the second dimension" in {
    //import ArrayDefs._
    //import ArrayDefs.Is2dSpArrSyntax._
    //val list2d = List2d[Dim1T, Dim2T, PositionsData]((dim1, dim2), values2d)
    //assert(
      //values2d.zipWithIndex.forall({case(x, i) => 
        //list2d.iloc(i) == List1d[Dim2T, PositionsData](dim2, x)
      //})
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
