package sportarray

import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.IsIdxElem
import IndicesObj.Index

import java.time.LocalDate

object test {

  sealed trait UnitType {
    type ElemT 
  }
  
  /// HERE 

  
  // the DataType trait is to ensure we do not do operations on non-compatible arrays 
  //import sportarray.UnitType
  //import sportarray._
  trait Numbers extends UnitType { type ElemT = Int }
  trait Grams extends UnitType { type ElemT = Int }

  case class ListOfListsWithIndices[T <: UnitType, Idx0, Idx1](
    values: List[List[T#ElemT]],
    indices: (List[Idx0], List[Idx1]),
  ) 

  val numbersOfFruitIIntendToBuy = ListOfListsWithIndices[Numbers, LocalDate, String](
    values=List(List(0, 1, 0), List(1, 0, 2)),
    indices=(
      List(LocalDate.of(2021,1,1), LocalDate.of(2020,1,2)), 
      List("Apple", "Orange", "Tomato, technically"),
    )
  )

  case class ArrayOfArrayWithIndices[T <: UnitType, Idx0, Idx1](
    values: Array[Array[T#ElemT]],
    indices: (List[Idx0], List[Idx1]),
  )

  val gramsOfPowdersIAlsoWillBuy = ArrayOfArrayWithIndices[Grams, LocalDate, String](
    values=Array(Array(100, 130, 150), Array(210, 0, 50)),
    indices=(
      List(LocalDate.of(2021,1,1), LocalDate.of(2020,1,2)), 
      List("Sugar", "Salt", "Sumac"),
    )
  )

  // consistent setter, getter and other functions are provided automatically
  println(gramsOfPowdersIAlsoWillBuy.loc("2020-01-01", "Salt")) // 130, of course
  val moarOranges = numbersOfFruitIIntendToBuy.setLoc(("2020-01-02", "Orange"), 30)
  // addition is handled automatically...
  val doubleThatFruitOrder = numbersOfFruitIIntendToBuy + numbersOfFruitIIntendToBuy 
  // but in a DataType-aware way; the following will produce a compile error
  val dontMixYourUnits = numbersOfFruitIIntendToBuy + gramsOfPowdersIAlsoWillBuy
  // thanks to the marvels of typing, arithmetic between array types can be handled automatically
  trait Euros extends UnitType { type ElemT = Double }
  case class SingleIndexedList[T <: UnitType, Idx0](
    values: List[T#ElemT],
    indices: List[Idx0],
  )
  val eurosPerGram = SingleIndexedList[Euros, String](
    values=List(0.01, 0.015, 0.02, 2400.0),
    indices=List("Sugar", "Salt", "Sumac", "Radium"),
  )
  val totalCostOfMyPowders = gramsOfPowdersIAlsoWillBuy * eurosPerGram
}

object ArrayDefs extends App {

  abstract class Is1dVec[A, N, I0, T: Numeric] {
    type Self = A
    def name(self: A): N
    def indices(self: A): (List[I0])
    def getElem(self: A, i: Int): T
    def iloc[R](self: A, r: R)(implicit 
      iLocTc: Is1dVec[A, N, I0, T]#ILocTC[R]
    ): iLocTc.Out = iLocTc.iloc(self, r)
    def :+:[B, Out](self: A, other: B)(implicit 
      bIs1d: Is1dVec[B, N, I0, T], 
      outIs2d: Is2dVec[Out, N, I0, T, A]
    ): Out

    abstract class ILocTC[R] {
      type Out
      def iloc(self: A, ref: R): Out 
    }
    object ILocTC {
      implicit def iLocTCForInt[OutT] = new ILocTC[Int] { 
        type Out = T
        def iloc(self: A, ref: Int) = getElem(self, ref)
      }
      implicit def iLocTCForList[OutT] = new ILocTC[List[Int]] { 
        type Out = List[T]
        def iloc(self: A, ref: List[Int]) = ref.map(getElem(self, _))
      }
    }
  }

  trait TC[S, R] {
    def iloc(self: S, ref: R): Int 
  }
  object TC {
    implicit def TCForInt[S] = new TC[S, Int] { 
      def iloc(self: S, ref: Int) = 3 
    }
  }

  implicit def indexedListIs1dVec[I0, N, T: Numeric]: Is1dVec[IndexedList[N, I0, T], N, I0, T] = 
    new Is1dVec[IndexedList[N, I0, T], N, I0, T] {
      def name(self: Self) = self.name
      def indices(self: Self) = self.indices
      def getElem(self: Self, i: Int) = self.data(i)
    }

  implicit class Is1dVecOps[A, N, I0, T](value: A)(implicit tc1d: Is1dVec[A, N, I0, T]) {
    def name: N = tc1d.name(value)
    def indices: List[I0] = tc1d.indices(value)
    def getElem(i: Int) = tc1d.getElem(value, i)
    def iloc[R](r: R)(implicit iLocTc: tc1d.ILocTC[R]): iLocTc.Out
    def :+:[B, Out](other: B)(implicit 
      bIs1d: Is1dVec[B, N, I0, T],
      outIs2d: Is2dVec[Out, N, I0, T, A]
    ): Out = tc1d.:+:(value, other)
  }

  abstract class Is2dVec[A, I0, I1, T: Numeric, M1](implicit tCm1: Is1dVec[M1, I0, I1, T]) {
    type Self = A
    type Minus1 = M1
    def indices(self: Self): (List[I0], List[I1])
    def getElem(self: A, i: Int): M1
    def loc[I](self: A, at: I)(implicit get2dTC: Get2dTC[I]): get2dTC.Out = ???
    def iloc[R](self: A, ref: R)(implicit iloctc: ILocTC[R, M1, I1, T]): iloctc.Out = iloctc.iloc(self, ref)
    //def ++[B, BI0, BI1](self: A, other: B)(implicit bIs2d: Is2dVec[B, BI0, BI1, T, _]): Self

    abstract class ILocTC[R, Out, OutI0, OutT] {
      type Out
      def iloc(self: A, ref: R): Out 
    }
    object ILocTC {
      implicit def iLocTCForInt[Out, OutI0, OutT] = new ILocTC[Int, Out, OutI0, OutT] { 
        type Out = M1
        def iloc(self: A, ref: Int): Out = getElem(self, ref)
      }
      implicit def iLocTCForIntInt[Out, OutI0, OutT] = new ILocTC[(Int, Int), Out, OutI0, OutT] { 
        type Out = A
        def iloc(self: A, ref: (Int, Int)): A = getElem(self, ref._1).:+:(getElem(self, ref._2))
      }
    }

    trait Get2dTC[I] {
      type DM1
      type Out = Get2dTC.MkOut[DM1]
      def apply(i: I): Out
    }
    object Get2dTC {
      type Aux[I, DM1i] = Get2dTC[I] { type DM1 = DM1i }
      type MkOut[DM1] = Option[Is1dVec[A, I, DM1, T]]
      def instance[I, DM1i](f: I => MkOut[DM1i]): Aux[I, DM1i] = new Get2dTC[I] {
        override type DM1 = DM1i
        override def apply(i: I): Out = f(i)
      }
    
      //implicit val I0Type: Aux[I0, I1] = instance(i => i.iloc(i))
      //implicit val I1Type: Aux[I1, I0] = instance(i => getDim1Slice(i))
    }
  }
  //object Is2dVec {
    //type Aux[A, I0, I1, T, M1] = Is2dVec[A, I0, I1, T, M1] { type Out = O }
  //}
  // give this typeclass method syntax
  implicit class Is2dVecOps[A, I0, I1, T, M1](value: A)(implicit 
    tc2d: Is2dVec[A, I0, I1, T, M1], tc1d: Is1dVec[M1, I0, I1, T]) {
      def getElem(i: Int) = tc2d.getElem(value, i)
    }

  case class IndexedList[N, I0, T: Numeric](
    name: N,
    indices: List[I0],
    data: List[T]
  )

  case class IndexedListOfLists[I0, I1, T: Numeric](
    indices: (List[I0], List[I1]),
    data: List[List[T]],
  )

  implicit def listOfListsIs2dVec[I0, I1, T: Numeric] = 
    new Is2dVec[IndexedListOfLists[I0, I1, T], I0, I1, T, IndexedList[I0, I1, T]] {
      def getElem(self: IndexedListOfLists[I0, I1, T], i: Int): IndexedList[I0, I1, T] = 
        IndexedList[I0, I1, T](self.indices._1(i), self.indices._2, self.data(i))
      def indices(self: Self) = self.indices
    }

  val arr1d = IndexedList[Char, Int, Double](
    '0', List(0,1,2), List(0.1, 1.1, 2.1)
  )
  assert(arr1d.getElem(1) == 1.1)
  assert(arr1d.iloc(0) == 0.1)
  println(implicitly[Is1dVec[IndexedList[Char, Int, Double], Char, Int, Double]]) 
  val arr2d = IndexedListOfLists[Char, Int, Double](
    (List('0', '1'), List(0,1,2)), List(List(0.1, 1.1, 2.1), List(0.2, 1.2, 2.2))
  )
  assert(arr2d.getElem(0) == arr1d)
  assert(listOfListsIs2dVec[Int, Int, Double].iloc(arr2d, (0, 2)) == 2.1)
}

