package sportarray

import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.IsIdxElem
import IndicesObj.Index

import java.time.LocalDate

object test {
  abstract class IsArr[A, T] {
    def getSingleElem(self: A, i: Int): T
    def getSlice[S](self: A, sl: S)(implicit slTc: IsSlice[S]): slTc.Out = slTc.getSlice(self, sl) 

    trait IsSlice[S] {
      type Out
      def getSlice(self: A, sl: S): Out
    }
    object IsSlice {
      implicit val intIsSlice = new IsSlice[Int] {
        type Out = T
        def getSlice(self: A, sl: Int): Out = getSingleElem(self, sl)
      }
      implicit val listIsSlice = new IsSlice[List[Int]] {
        type Out = List[T]
        def getSlice(self: A, sl: List[Int]): Out = sl.map(getSingleElem(self, _))
      }
    }
  }

  implicit def listIsArr[A, T] = 
    new IsArr[List[Double], Double] {
      def getSingleElem(self: List[Double], i: Int) = self(i)
    }

  object IsArrSyntax {
    implicit class IsArrOps[A, T](self: A)(implicit tc: IsArr[A, T]) {
      def getSingleElem(i: Int) = tc.getSingleElem(self, i)
      def getSlice[S](sl: S)(implicit slTc: tc.IsSlice[S]) = tc.getSlice(self, sl)
    }
  }

  import IsArrSyntax.IsArrOps

  val l = List(1.0, 2.0, 3.0)
  l.getSlice(List(0,1)) 
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

  abstract class Is1dSpArr[A, I0: IsIdxElem, DT <: DataType] {
    type Self = A
    def indices(self: A): Index[I0]
    def getElem(self: A, i: Int): DT#T
    def iloc[R](self: A, r: R)(implicit iLocTc: ILocTC[R]): iLocTc.Out = iLocTc.iloc(self, r)
    //def ::(self: A, other: T#T): A

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
        type Out = List[DT#T]
        def iloc(self: A, ref: List[Int]): Out = ref.map(getElem(self, _))
      }
    }
  }

  object Is1dSpArrSyntax {
    implicit class Is1dSpArrOps[A, I0, T <: DataType](self: A)(implicit 
      tc1d: Is1dSpArr[A, I0, T]
    ) {
      def indices: Index[I0] = tc1d.indices(self)
      def getElem(i: Int) = tc1d.getElem(self, i)
      //def iloc[R](r: R)(implicit iLocTc: Is1dSpArr[A, I0, T]#ILocTC[R]): iLocTc.Out = tc1d.iloc(self, r)
      def iloc[R](r: R)(implicit iLocTc: tc1d.ILocTC[R]) = tc1d.iloc(self, r)
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
}

