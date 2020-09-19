package sportarray

import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.IsIdxElem
import IndicesObj.Index

import java.time.LocalDate

//object TestAsOne {
  //trait StoresNumeric[A, T] {
    //def getNum(self: A): T
  //}
  //object StoresNumericSyntax {
    //implicit class StoresNumericOps[A, T](value: A) {
      //def getNum(implicit sn: StoresNumeric[A, T]): T = sn.getNum(value)
    //}
  //}
  //case class ANumber[T](
    //num: T
  //)
  //implicit def aNumberStoresNumeric[T] = 
    //new StoresNumeric[ANumber[T], T] {
      //def getNum(self: ANumber[T]): T = self.num
    //}
  //val a = ANumber[Int](3)
  //import StoresNumericSyntax._
  //// 1. Works fine, so explicit conversion possible
  //aNumberStoresNumeric[Int].getNum(a) 
  //// 2. Works fine, so implicit conversion possible
  //implicitly[StoresNumeric[ANumber[Int], Int]].getNum(a) 
  //// 3. Doesn't work, so implicit conversion not working
  //println(implicitly[ANumber[Int] => StoresNumeric[ANumber[Int], Int]]) // no implicit view available...
  //// 4. The holy grail. Doesn't work, for the same reason as above, plus possibly other
  //a.getNum
//}

object StoresNumericObj {
  abstract class StoresNumeric[A, T: Numeric] {
    def getNum(self: A): T
  }
  object StoresNumericSyntax {
    implicit class StoresNumericOps[A](private val self: A) extends AnyVal {
      def getNum[T](implicit sn: StoresNumeric[A, T]): T = sn.getNum(self)
    }
  }
}

object ANumberObj {
  import StoresNumericObj._
  case class ANumber[T: Numeric](
    num: T
  )
  implicit def aNumberStoresNumeric[T: Numeric] = 
    new StoresNumeric[ANumber[T], T] {
      def getNum(self: ANumber[T]): T = self.num
    }
}

object StoresNumericTest {
  import ANumberObj._
  import StoresNumericObj._
  import StoresNumericSyntax._
  val a = ANumber[Int](3)
  // 1. Works fine, so explicit conversion possible
  aNumberStoresNumeric[Int].getNum(a) 
  // 2. Works fine, so implicit conversion possible
  implicitly[StoresNumeric[ANumber[Int], Int]].getNum(a) 
  // 3. Doesn't work, so implicit conversion not working
  //println(implicitly[ANumber[Int] => StoresNumeric[ANumber[Int], Int]]) // no implicit view available...
  // 4. The holy grail. Doesn't work, for the same reason as above, plus possibly other
  a.getNum
}

object ArrayDefs {

  //sealed abstract class SportArray[+A, +T] {
    //def isEmpty: Boolean
    //def head: T
    //def tail: A
  //}

  //case object NoArr extends SportArray[Nothing, Nothing] {
    //def isEmpty = true
    //def head: Nothing = throw new NoSuchElementException("head of empty sportarray")
    //def tail: SportArray[Nothing, Nothing] = throw new NoSuchElementException("tail of empty sportarray")
  //}

  //final case class ::[A, T](head: T, tail: SportArray[A, T]) extends SportArray[A, T] 
  //{
    //def isEmpty = false
  //}

  abstract class Is1dSpArr[A, I0: IsIdxElem, T <: DataType] {
    type Self = A
    //type Im1 <: DataType
    //def name(self: A): Option[Im1]
    //def indices(self: A): Index[I0]
    def getElem(self: A, i: Int): T#T
    def iloc[R](self: A, r: R)(implicit 
      iLocTc: Is1dSpArr[A, I0, T]#ILocTC[R]
    ): iLocTc.Out = iLocTc.iloc(self, r)
    //def ::(self: A, other: T#T): A

    abstract class ILocTC[R] {
      type Out
      def iloc(self: A, ref: R): Out 
    }
    object ILocTC {
      implicit def iLocTCForInt[OutT] = new ILocTC[Int] { 
        type Out = T#T
        def iloc(self: A, ref: Int) = getElem(self, ref)
      }
      implicit def iLocTCForList[OutT] = new ILocTC[List[Int]] { 
        type Out = List[T#T]
        def iloc(self: A, ref: List[Int]) = ref.map(getElem(self, _))
      }
    }
  }

  object Is1dSpArrSyntax {
    implicit class Is1dSpArrOps[A, I0, T <: DataType](private val self: A) {
      //def indices: Index[I0] = tc1d.indices(value)
      def getElem(i: Int)(implicit tc1d: Is1dSpArr[A, I0, T]) = tc1d.getElem(self, i)
      //def iloc[R](r: R)(implicit iLocTc: Is1dSpArr[A, N, I0, T]#ILocTC[R]): iLocTc.Out
      //def iloc[R](r: R)(implicit iLocTc: tc1d.ILocTC[R]): iLocTc.Out = tc1d.iloc(value, r)
      //def ::(other: T#T): A = tc1d.::(value, other)
    }
  }

  //abstract class Is2dVec[A, I0, I1, T: Numeric, M1](implicit tCm1: Is1dSpArr[M1, I0, I1, T]) {
    //type Self = A
    //type Minus1 = M1
    //def indices(self: Self): (List[I0], List[I1])
    //def getElem(self: A, i: Int)(implicit m1Is1dSpArr: Is1dSpArr[M1, I0, I1, T]): M1
    ////def loc[I](self: A, at: I)(implicit get2dTC: Get2dTC[I]): get2dTC.Out = ???
    //def iloc[R](self: A, ref: R)(implicit iLocTc: ILocTC[R, M1, I1, T]): iLocTc.Out = iLocTc.iloc(self, ref)
    //def :+:[B, BI0, BI1](self: A, other: B)(implicit bIs2d: Is2dVec[B, BI0, BI1, T, _]): Self

    //abstract class ILocTC[R, Out, OutI0, OutT] {
      //type Out
      //def iloc(self: A, ref: R): Out 
    //}
    //object ILocTC {
      //implicit def iLocTCForInt[Out, OutI0, OutT] = new ILocTC[Int, Out, OutI0, OutT] { 
        //type Out = M1
        //def iloc(self: A, ref: Int): Out = getElem(self, ref)
      //}
      //implicit def iLocTCForIntInt[Out, OutI0, OutT] = new ILocTC[(Int, Int), Out, OutI0, OutT] { 
        //type Out = A
        //def iloc(self: A, ref: (Int, Int)): Out = 
          //getElem(self, ref._1).:+:(getElem(self, ref._2))
      //}
    //}

    ////trait Get2dTC[I] {
      ////type DM1
      ////type Out = Get2dTC.MkOut[DM1]
      ////def apply(i: I): Out
    ////}
    ////object Get2dTC {
      ////type Aux[I, DM1i] = Get2dTC[I] { type DM1 = DM1i }
      ////type MkOut[DM1] = Option[Is1dSpArr[A, I, DM1, T]]
      ////def instance[I, DM1i](f: I => MkOut[DM1i]): Aux[I, DM1i] = new Get2dTC[I] {
        ////override type DM1 = DM1i
        ////override def apply(i: I): Out = f(i)
      ////}
    
      //////implicit val I0Type: Aux[I0, I1] = instance(i => i.iloc(i))
      //////implicit val I1Type: Aux[I1, I0] = instance(i => getDim1Slice(i))
    ////}
  //}
  ////object Is2dVec {
    ////type Aux[A, I0, I1, T, M1] = Is2dVec[A, I0, I1, T, M1] { type Out = O }
  ////}
  //// give this typeclass method syntax
  //implicit class Is2dVecOps[A, I0, I1, T, M1](value: A)(implicit 
    //tc2d: Is2dVec[A, I0, I1, T, M1], tc1d: Is1dSpArr[M1, I0, I1, T]) {
      //def getElem(i: Int) = tc2d.getElem(value, i)
      //def indices = tc2d.indices(value)
      //def loc[I](at: I) = ???
      //def iloc[R](r: R)(implicit iLoc2d: tc2d.ILocTC[R, M1, I1, T]) = tc2d.iloc(value, r)
    //}

  //case class IndexedList[N, I0, T: Numeric](
    //name: N,
    //indices: List[I0],
    //data: List[T]
  //)

  //case class IndexedListOfLists[I0, I1, T: Numeric](
    //indices: (List[I0], List[I1]),
    //data: List[List[T]],
  //)

  //implicit def listOfListsIs2dVec[I0, I1, T: Numeric] = 
    //new Is2dVec[IndexedListOfLists[I0, I1, T], I0, I1, T, IndexedList[I0, I1, T]] {
      //def getElem(self: IndexedListOfLists[I0, I1, T], i: Int): IndexedList[I0, I1, T] = 
        //IndexedList[I0, I1, T](self.indices._1(i), self.indices._2, self.data(i))
      //def indices(self: Self) = self.indices
    //}

  //val arr1d = IndexedList[Char, Int, Double](
    //'0', List(0,1,2), List(0.1, 1.1, 2.1)
  //)
  //assert(arr1d.getElem(1) == 1.1)
  //assert(arr1d.iloc(0) == 0.1)
  //println(implicitly[Is1dSpArr[IndexedList[Char, Int, Double], Char, Int, Double]]) 
  //val arr2d = IndexedListOfLists[Char, Int, Double](
    //(List('0', '1'), List(0,1,2)), List(List(0.1, 1.1, 2.1), List(0.2, 1.2, 2.2))
  //)
  //assert(arr2d.getElem(0) == arr1d)
  //assert(arr2d.iloc(0, 2) == 2.1)
}

