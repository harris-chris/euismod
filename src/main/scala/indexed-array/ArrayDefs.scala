package sportarray

//import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsSpBase, DataType}
import IndicesObj.Index

import java.time.LocalDate
import shapeless.{HList, HNil, Lazy, :: => #:}
import shapeless.ops.hlist._

object ArrayDefs {

  abstract class IsSpArr[A[_, _], I0, M1T](implicit 
    val m1IsSpBase: IsSpBase[M1T] {type Self = M1T}
  ) extends IsSpBase[A[I0, M1T]] {
    type Self = A[I0, M1T]
    type M1 = m1IsSpBase.Self
    def getIdx(self: Self): Index[I0]
    def getIdxElem(self: Self, i: Int): I0 = getIdx(self)(i)
    def getElem(self: Self, i: Int): M1
    def getNil[I, M1](self: Self)(implicit 
      outIsArr: IsSpArr[A, I, M1],
    ): A[I, M1]
    def iloc[R](self: Self, r: R)(implicit iLoc: ILoc[A, I0, M1T, R]): Self = iLoc.iloc(self, r)
    def ::(self: Self, other: (I0, M1)): Self
    def ++(self: Self, other: Self): Self = {
      val i: List[(I0, M1)] = toListWithIndex(other)
      i match {
        case (idx, elem) :: ts => ::(self, (idx, elem))
        case Nil => self
      }
    }
    def fmap[B, C](self: A[I0, M1], f: B => C)(implicit fMap: FMap[A, I0, M1, B, C]) = fMap.fmap(self, f)
    def length(self: Self): Int
    def toList(self: Self): List[M1] = (for(i <- 0 to length(self)) yield (getElem(self, i))).toList
    def toListWithIndex(self: Self): List[(I0, M1)] = getIdx(self).toList.zip(toList(self)).toList
    def fromList[I, M](self: Self, l: List[(I, M)])(implicit 
      outIsArr: IsSpArr[A, I, M],
    ): A[I, M] = l.foldLeft(getNil(self))((b, a) => outIsArr.::(b, a))
    def mapList[C](self: Self, f: M1 => C)(implicit 
      mIsSpBase: IsSpBase[C],
      outIsArr: IsSpArr[A, I0, C],
    ): A[I0, C] = fromList(self,
        toListWithIndex(self).map((t: (I0, M1)) => (t._1, f(t._2)))
      )
  }

  trait FMap[A[_, _], I, M1, B, C] {
    type Out
    def fmap(self: A[I, M1], f: B => C): Out 
  }

  object FMap {
    implicit def fMapIfM1IsB[A[_, _], I, M1, B, C](implicit 
      isArr: Lazy[IsSpArr[A, I, M1]], 
      cIsSpBase: IsSpBase[C],
      outIsArr: IsSpArr[A, I, C],
      outIsSpBase: IsSpBase[A[I, C]],
    ) = new FMap[A, I, M1, M1, C] {
      type Out = A[I, C]
      def fmap(self: A[I, M1], f: M1 => C): A[I, C] = 
        isArr.value.mapList(self, f)
    }
    implicit def fMapIfM1IsNotB[A[_, _], I0, I1, M1C[_, _], M2, B, C, M1O](implicit 
      isArr: Lazy[IsSpArr[A, I0, M1C[I1, M2]]], 
      m1IsSpBase: IsSpBase[M1C[I1, M2]],
      m1IsArr: IsSpArr[M1C, I1, M2],
      fMapForM1: FMap[M1C, I1, M2, B, C] {type Out = M1O},
      outIsArr: IsSpArr[A, I0, M1O],
      m1OutIsSpBase: IsSpBase[M1O],
    ) = new FMap[A, I0, M1C[I1, M2], B, C] {
      type Out = A[I0, M1O]
      def fmap(self: A[I0, M1C[I1, M2]], f: B => C): Out = isArr.value.mapList(
        self, b => fMapForM1.fmap(b, f))
        //val aList: List[(I0O, M1O)] = isArr.value.toListWithIndex(self)
        //val m1List: List[(I0O, M1O)] = aList.map((t: (I0O, M1O)) => (t._1, m1IsArr.fmap(t._2, f)))
        //m1List.foldLeft(isArr.value.getNil(self))((b, a) => isArr.value.::(b, a))
    }
  }

  //abstract class Is1dSpArr[A, T <: DataType, I0] extends IsSpArr[A, T, I0] {
    //override type M1 = T#T
    //def shape(self: A): Tuple1[Int] = Tuple1(length(self))
  //}

  //abstract class Is2dSpArr[A, T <: DataType, I0, I1, M1C[_ <: DataType, _]](
    //implicit tc1d: Is1dSpArr[M1C[T, I1], T, I1]
  //) extends IsSpArr[A, T, I0] {
    //override type M1 = M1C[T, I1]
    //def shape(self: A): (Int, Int) = 
      //(length(self), tc1d.length(getElem(self, 0)))
  //}

  trait ILoc[A[_, _], I, M1, R] {
    def iloc(self: A[I, M1], ref: R): A[I, M1]
  }
  object ILoc {
    def apply[A[_, _], I, M1, R](implicit tc: ILoc[A, I, M1, R]): ILoc[A, I, M1, R] = tc
    implicit def iLocForInt[A[_, _], I0, M1T](implicit 
      m1tIsSpBase: IsSpBase[M1T], 
      isArr: IsSpArr[A, I0, M1T], 
    ) = new ILoc[A, I0, M1T, Int] {
      override def iloc(self: A[I0, M1T], ref: Int) = isArr.::(
        isArr.getNil(self), 
        (isArr.getIdx(self)(ref), isArr.getElem(self, ref))
      )
    }
    implicit def iLocForListOfInts[A[_, _], I0, M1T](implicit 
      m1tIsDataType: DataType[M1T], 
      isArr: IsSpArr[A, I0, M1T],
    ) = new ILoc[A, I0, M1T, List[Int]] {
      def iloc(self: A[I0, M1T], ref: List[Int]) = {
        val data: List[isArr.M1] = ref.map(isArr.getElem(self, _)).toList
        val idx0: Index[I0] = isArr.getIdx(self)
        val newIdx = Index(ref.map(idx0(_)))
        newIdx.toList.zip(data).foldLeft(isArr.getNil(self))((a, b) => isArr.::(a, (b._1, b._2))) 
      }
    }
    implicit def iLocForNull[A[_, _], I, M] = new ILoc[A, I, M, Null] {
      def iloc(self: A[I, M], ref: Null) = self
    }
    implicit def iLocForHNil[A[_, _], I, M] = new ILoc[A, I, M, HNil] {
      def iloc(self: A[I, M], ref: HNil) = self
    }
    implicit def iLocForHList[A[_, _], I, M, H, T <: HList](implicit 
      isArr: IsSpArr[A, I, M],
      ilocHead: Lazy[ILoc[A, I, M, H]],
      ilocTail: ILoc[A, I, M, T],
    ) = new ILoc[A, I, M, H #: T] {
      def iloc(self: A[I, M], ref: H #: T) = ilocHead.value.iloc(self, ref.head)
    // ilocH.iloc(self, ref.head).toList.map(iLocOut.iloc(_, ref.tail))// need to map the iloc tail over every element
    // This is going to be easier if iloc doesn't squeeze the array. At least the squeeze operation could be done later
    }
  }

  object IsSpArrSyntax {
    implicit class IsSpArrOps[A[_, _], I0, M1T](self: A[I0, M1T])(implicit 
      val tc: IsSpArr[A, I0, M1T],
      m1tIsSpBase: IsSpBase[M1T],
    ) {
      def getNil = tc.getNil(self)
      def getElem(i: Int) = tc.getElem(self, i)
      def iloc[R](r: R)(implicit iLoc: ILoc[A, I0, M1T, R]) = tc.iloc(self, r)
      def ::(other: (I0, tc.M1)): A[I0, M1T] = tc.::(self, other)
      def length: Int = tc.length(self)
      def toList: List[tc.M1] = tc.toList(self)
      def toListWithIndex = tc.toListWithIndex(self)
      def fmap[B, C](f: B => C)(implicit fMap: FMap[A, I0, M1T, B, C]) = fMap.fmap(self, f)
    }
  }
    //implicit class Is2dSpArrOps[A, T <: DataType, I0, I1, M1C[_ <: DataType, _]](self: A)(implicit 
      //val tc: Is2dSpArr[A, T, I0, I1, M1C],
    //) {
      //def getNil = tc.getNil(self)
      //def getElem(i: Int) = tc.getElem(self, i)
      //def iloc[R](r: R)(implicit iLoc: ILoc[A, R]) = tc.iloc(self, r)
      //def shape: (Int, Int) = tc.shape(self)
      //def ::(other: (I0, tc.M1)): A = tc.::(self, other)
      //def length: Int = tc.length(self)
      //def toList: List[tc.M1] = tc.toList(self)
      //def toListWithIndex = tc.toListWithIndex(self)
      ////def unapply: Option[((I0, T#T), A)] = tc2d.unapply(self) 
    //}
  //}
}

