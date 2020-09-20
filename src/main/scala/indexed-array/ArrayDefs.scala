package sportarray

import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.IsIdxElem
import IndicesObj.Index

import java.time.LocalDate

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

  abstract class Is1dSpArr[A, I0: IsIdxElem, DT <: DataType] {
    type Self = A
    def indices(self: A): Index[I0]
    def getElem(self: A, i: Int): DT#T
    def iloc[R](self: A, r: R)(implicit iLocTc: ILocTC[R]): iLocTc.Out = iLocTc.iloc(self, r)
    def getNil(self: A): A
    def ::(self: A, other: (I0, DT#T)): A
    def head(self: A): (I0, DT#T) = (indices(self)(0), getElem(self, 0))
    def shape(self: A): Int = indices(self).length
    def tail(self: A): A = ILocTC.iLocTCForList.iloc(self, (1 to shape(self)).toList)

    def unapply(self: A): ((I0, DT#T), A) = (head(self), tail(self)) 

    trait ILocTC[R] {
      type Out
      def iloc(self: A, ref: R): Out 
    }
    object ILocTC {
      implicit val iLocTCForInt = new ILocTC[Int] { 
        type Out = DT#T
        def iloc(self: A, ref: Int): Out = getElem(self, ref)
      }
      implicit val iLocTCForList = new ILocTC[List[Int]] { 
        type Out = Self
        def iloc(self: A, ref: List[Int]): Out = {
          val data: List[DT#T] = ref.map(getElem(self, _)).toList
          val idx: Index[I0] = Index(ref.map(indices(self)(_)))
          idx.toList.zip(data).foldLeft(getNil(self))((a, b) => ::(a, (b._1, b._2))) 
        }
      }
    }
    //trait LocTC[R] {
      //type Out
      //def loc(self: A, ref: R): Out
    //}
    //object LocTC {
      //implicit val locTCForI0 = new LocTC[I0] { 
        //type Out = Option[DT#T]
        //def loc(self: A, ref: I0): Out = indices(self).indexOf(ref).map(getElem(self, _))
      //}
      //implicit val locTCForListI0 = new LocTC[List[I0]] { 
        //type Out = Option[List[DT#T]]
        //def loc(self: A, ref: List[I0]): Out = ref.map(indices(self).indexOfgetElem(self, _))
      //}
    //}
  }

  object Is1dSpArrSyntax {
    implicit class Is1dSpArrOps[A, I0, T <: DataType](self: A)(implicit 
      tc1d: Is1dSpArr[A, I0, T]
    ) {
      def indices: Index[I0] = tc1d.indices(self)
      def getElem(i: Int) = tc1d.getElem(self, i)
      //def iloc[R](r: R)(implicit iLocTc: Is1dSpArr[A, I0, T]#ILocTC[R]): iLocTc.Out = tc1d.iloc(self, r)
      def iloc[R](r: R)(implicit iLocTc: tc1d.ILocTC[R]) = tc1d.iloc(self, r)
      def ::(other: (I0, T#T)): A = tc1d.::(self, other)
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
}

