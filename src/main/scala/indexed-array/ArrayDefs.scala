package sportarray

import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.IsIdxElem
import IndicesObj.Index

import java.time.LocalDate
import shapeless._
import shapeless.ops.hlist._

object ArrayDefs {

  abstract class IsSpArr[A, T <: DataType, I0: IsIdxElem] {
    type Self = A
    type M1
    def getIdx(self: A): Index[I0]
    def getElem(self: A, i: Int): M1
    def getNil(self: A): A
    def iloc[R](self: A, r: R)(implicit iLoc: ILoc[R, A, T, I0]): iLoc.Out = iLoc.iloc(self, r)
    def ::(self: A, other: (I0, M1)): A
    def length(self: A): Int
  }

  abstract class Is1dSpArr[A, T <: DataType, I0: IsIdxElem] extends IsSpArr[A, T, I0] {
    type M1 = T#T
    def shape(self: A): (Int) = length(self)
  }

  abstract class Is2dSpArr[A, T <: DataType, I0: IsIdxElem, I1: IsIdxElem, M1C[_ <: DataType, _]](
    implicit tc1d: Is1dSpArr[M1C[T, I1], T, I1]
  ) extends IsSpArr[A, T, I0] {
    type M1 = M1C[T, I1]
    def shape(self: A): (Int, Int) = 
      (length(self), tc1d.length(getElem(self, 0)))
  }

  trait ILoc[R, A, T <: DataType, I0] {
    type Out
    def iloc(self: A, ref: R): Out
  }
  object ILoc {
    type Aux[A0, R0, T0 <: DataType, I0, O0] = ILoc[A0, R0, T0, I0] { type Out = O0 }
    implicit def iLocForInt[A, T <: DataType, I0: IsIdxElem](implicit isArr: IsSpArr[A, T, I0]) = new ILoc[Int, A, T, I0] { 
      type Out = isArr.M1
      def iloc(self: A, ref: Int): Out = isArr.getElem(self, ref)
    }
    implicit def iLocForList[A, T <: DataType, I0: IsIdxElem](implicit isArr: IsSpArr[A, T, I0]) = 
      new ILoc[List[Int], A, T, I0] { 
      type Out = A
      def iloc(self: A, ref: List[Int]): Out = {
        val data: List[isArr.M1] = ref.map(isArr.getElem(self, _)).toList
        val idx0: Index[I0] = isArr.getIdx(self)
        val newIdx: Index[I0] = Index(ref.map(idx0(_)))
        newIdx.toList.zip(data).foldLeft(isArr.getNil(self))((a, b) => isArr.::(a, (b._1, b._2))) 
      }
    }
    implicit def iLocForNull[A, T <: DataType, I0: IsIdxElem](implicit isArr: IsSpArr[A, T, I0]) = 
      new ILoc[Null, A, T, I0] { 
      type Out = A
      def iloc(self: A, ref: Null): Out = self
    }
  }

  object IsSpArrSyntax {
    implicit class Is1dSpArrOps[A, T <: DataType, I0](self: A)(implicit val tc: Is1dSpArr[A, T, I0] {type M1 = T#T}) {
      def getElem(i: Int) = tc.getElem(self, i)
      def iloc[R](r: R)(implicit iLoc: ILoc[R, A, T, I0]) = tc.iloc(self, r)
      def getNil = tc.getNil(self)
      def ::(other: (I0, tc.M1)): A = tc.::(self, other)
      def shape: Int = tc.shape(self)
      def length: Int = tc.length(self)
      //def unapply: Option[((I0, T#T), A)] = tc1d.unapply(self) 
    }
    implicit class Is2dSpArrOps[A, T <: DataType, I0: IsIdxElem, I1: IsIdxElem, M1C[_ <: DataType, _]](self: A)(implicit 
      val tc: Is2dSpArr[A, T, I0, I1, M1C],
    ) {
      def getElem(i: Int) = tc.getElem(self, i)
      def iloc[R](r: R)(implicit iLoc: ILoc[R, A, T, I0]) = tc.iloc(self, r)
      //def loc[R](r: R)(implicit locTc: tc2d.LocTC[R]) = tc2d.loc(self, r)
      def ::(other:(I0, tc.M1)): A = tc.::(self, other)
      def shape: (Int, Int) = tc.shape(self)
      //def unapply: Option[((I0, T#T), A)] = tc2d.unapply(self) 
    }
  }


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

