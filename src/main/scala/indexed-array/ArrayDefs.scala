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
    def getIdxElem(self: A, i: Int): I0 = getIdx(self)(i)
    def getElem(self: A, i: Int): M1
    def getNil(self: A): A
    def iloc[R](self: A, r: R)(implicit iLoc: ILoc[R, A, I0]): A = iLoc.iloc(self, r)
    def ::(self: A, other: (I0, M1)): A
    def length(self: A): Int
    def toList(self: A): List[M1] = (for(i <- 0 to length(self)) yield (getElem(self, i))).toList
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

  trait ILoc[R, A, I0] {
    def iloc(self: A, ref: R): A
  }
  object ILoc {
    def apply[R, A, I0](implicit tc: ILoc[R, A, I0]): ILoc[R, A, I0] = tc
    def instance[R, A, I0](func: (A, R) => A): ILoc[R, A, I0] = new ILoc[R, A, I0] {
      def iloc(self: A, ref: R): A = func(self, ref)
    }
    implicit def iLocForInt[A, I0](implicit isArr: IsSpArr[A, _, I0]): ILoc[Int, A, I0] = instance(
      (self: A, ref: Int) => isArr.::(isArr.getNil(self), (isArr.getIdx(self)(ref), isArr.getElem(self, ref)))
    )
    implicit def iLocForListOfInts[A, I0: IsIdxElem](implicit isArr: IsSpArr[A, _, I0]): ILoc[List[Int], A, I0] = instance(
      (self: A, ref: List[Int]) => {
        val data: List[isArr.M1] = ref.map(isArr.getElem(self, _)).toList
        val idx0: Index[I0] = isArr.getIdx(self)
        val newIdx = Index(ref.map(idx0(_)))
        newIdx.toList.zip(data).foldLeft(isArr.getNil(self))((a, b) => isArr.::(a, (b._1, b._2))) 
      }
    )
    implicit def iLocForNull[A, I0]: ILoc[Null, A, I0] = instance(
      (self: A, ref: Null) => self
    )
    implicit def iLocForHNil[A, I0]: ILoc[HNil, A, I0] = instance(
      (self: A, ref: HNil) => self
    )
    implicit def iLocForHList[A, H, T <: HList, I0](implicit 
      ilocHead: Lazy[ILoc[H, A, I0]],
      ilocTail: ILoc[T, A, I0],
    ): ILoc[H :: T, A, I0] = instance((self: A, ref: H :: T) => ilocHead.value.iloc(self, ref.head)) 
    // ilocH.iloc(self, ref.head).toList.map(iLocOut.iloc(_, ref.tail))// need to map the iloc tail over every element
    // This is going to be easier if iloc doesn't squeeze the array. At least the squeeze operation could be done later
  }

  object IsSpArrSyntax {
    implicit class IsSpArrOps[A, T <: DataType, I0](self: A)(implicit
      val tc: IsSpArr[A, T, I0] {type M1 = T#T},
    ) {
      def getElem(i: Int) = tc.getElem(self, i)
      def iloc[R](r: R)(implicit iLoc: ILoc[R, A, I0]) = tc.iloc(self, r)
      def getNil = tc.getNil(self)
      def ::(other: (I0, tc.M1)): A = tc.::(self, other)
      def length: Int = tc.length(self)
      def toList: List[tc.M1] = tc.toList(self)
    }
    implicit class Is1dSpArrOps[A, T <: DataType, I0](self: A)(implicit 
      val tc: Is1dSpArr[A, T, I0] {type M1 = T#T},
    ) {
      def shape: Int = tc.shape(self)
      //def unapply: Option[((I0, T#T), A)] = tc1d.unapply(self) 
    }
    implicit class Is2dSpArrOps[A, T <: DataType, I0, I1, M1C[_ <: DataType, _]](self: A)(implicit 
      val tc: Is2dSpArr[A, T, I0, I1, M1C],
    ) {
      //def loc[R](r: R)(implicit locTc: tc2d.LocTC[R]) = tc2d.loc(self, r)
      def shape: (Int, Int) = tc.shape(self)
      //def unapply: Option[((I0, T#T), A)] = tc2d.unapply(self) 
    }
  }
}

