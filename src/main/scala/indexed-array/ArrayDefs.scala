package sportarray

//import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsSpBase, DataType}
import IndicesObj.Index

import java.time.LocalDate
import shapeless.{HList, HNil, Lazy, :: => #:}
import shapeless.ops.hlist._

object ArrayDefs {

  abstract class IsSpArr[A, I0, M1T](implicit 
    val m1IsArr: IsSpBase[M1T] {type Self = M1T}
  ) extends IsSpBase[A] {
    type Self = A
    type M1 = m1IsArr.Self
    def getIdx(self: A): Index[I0]
    def getIdxElem(self: A, i: Int): I0 = getIdx(self)(i)
    def getElem(self: A, i: Int): M1
    def getNil[I, M](self: A)(implicit 
      mIsDataType: DataType[M],
      outIsArr: IsSpArr[A, I, M],
    ): A
    def iloc[R](self: A, r: R)(implicit iLoc: ILoc[A, R]): A = iLoc.iloc(self, r)
    def ::(self: A, other: (I0, M1)): A
    def ++(self: A, other: A): A = {
      val i: List[(I0, M1)] = toListWithIndex(other)
      i match {
        case (idx, elem) :: ts => ::(self, (idx, elem))
        case Nil => self
      }
    }
    def fmap[B, C](self: A, f: B => C)(implicit fMap: FMap[A, B, C]) = fMap.fmap(self, f)
    def length(self: A): Int
    def toList(self: A): List[M1] = (for(i <- 0 to length(self)) yield (getElem(self, i))).toList
    def toListWithIndex(self: A): List[(I0, M1)] = getIdx(self).toList.zip(toList(self)).toList
    def fromList[I, M](self: A, l: List[(I, M)])(implicit 
      mIsDataType: DataType[M],
      outIsArr: IsSpArr[A, I, M],
    ): A = l.foldLeft(getNil(self))((b, a) => outIsArr.::(b, a))
    def mapList[M2](self: A, f: M1 => M2)(implicit 
      mIsDataType: DataType[M2],
      outIsArr: IsSpArr[A, I0, M2],
    ): A = 
      fromList(self,
        toListWithIndex(self).map((t: (I0, M1)) => (t._1, f(t._2)))
      )
  }

  trait FMap[A, B, C] {
    def fmap(self: A, f: B => C): A
  }
  object FMap {
    implicit def fMapIfAIsB[A, I0, M1, B, C](implicit 
      isArr: Lazy[IsSpArr[A, I0, M1]], 
      cIsDataType: DataType[C],
      ev: M1=:=B,
      outIsArr: IsSpArr[A, I0, C],
    ) = new FMap[A, M1, C] {
      def fmap(self: A, f: M1 => C): A = 
        isArr.value.mapList(self, f)
    }
    implicit def fMapIfAIsNotB[A, I0, I1, M1, M2, B, C](implicit 
      isArr: Lazy[IsSpArr[A, I0, M1]], 
      cIsDataType: DataType[C],
      m1IsArr: IsSpArr[M1, I1, M2],
      fMap: FMap[M1, B, C],
      outIsArr: IsSpArr[A, I0, C],
    ) = new FMap[A, M1, C] {
      def fmap(self: A, f: M1 => C): A = isArr.value.mapList(self, f)
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

  trait ILoc[A, R] {
    def iloc(self: A, ref: R): A
  }
  object ILoc {
    def apply[A, R](implicit tc: ILoc[A, R]): ILoc[A, R] = tc
    implicit def iLocForInt[A, I0, M1T](implicit 
      m1tIsDataType: DataType[M1T], 
      isArr: IsSpArr[A, I0, M1T], 
    ) = new ILoc[A, Int] {
      override def iloc(self: A, ref: Int) = isArr.::(
        isArr.getNil(self), 
        (isArr.getIdx(self)(ref), isArr.getElem(self, ref))
      )
    }
    implicit def iLocForListOfInts[A, I0, M1T](implicit 
      m1tIsDataType: DataType[M1T], 
      isArr: IsSpArr[A, I0, M1T],
    ) = new ILoc[A, List[Int]] {
      def iloc(self: A, ref: List[Int]) = {
        val data: List[isArr.M1] = ref.map(isArr.getElem(self, _)).toList
        val idx0: Index[I0] = isArr.getIdx(self)
        val newIdx = Index(ref.map(idx0(_)))
        newIdx.toList.zip(data).foldLeft(isArr.getNil(self))((a, b) => isArr.::(a, (b._1, b._2))) 
      }
    }
    implicit def iLocForNull[A] = new ILoc[A, Null] {
      def iloc(self: A, ref: Null) = self
    }
    implicit def iLocForHNil[A] = new ILoc[A, HNil] {
      def iloc(self: A, ref: HNil) = self
    }
    implicit def iLocForHList[A, H, T <: HList](implicit 
      isArr: IsSpArr[A, _, _],
      ilocHead: Lazy[ILoc[A, H]],
      ilocTail: ILoc[A, T],
    ) = new ILoc[A, H #: T] {
      def iloc(self: A, ref: H #: T) = ilocHead.value.iloc(self, ref.head)
    // ilocH.iloc(self, ref.head).toList.map(iLocOut.iloc(_, ref.tail))// need to map the iloc tail over every element
    // This is going to be easier if iloc doesn't squeeze the array. At least the squeeze operation could be done later
    }
  }

  object IsSpArrSyntax {
    implicit class IsSpArrOps[A, I0, M1T](self: A)(implicit 
      val tc: IsSpArr[A, I0, M1T],
      m1TIsDataType: DataType[M1T],
    ) {
      def getNil = tc.getNil(self)
      def getElem(i: Int) = tc.getElem(self, i)
      def iloc[R](r: R)(implicit iLoc: ILoc[A, R]) = tc.iloc(self, r)
      def ::(other: (I0, tc.M1)): A = tc.::(self, other)
      def length: Int = tc.length(self)
      def toList: List[tc.M1] = tc.toList(self)
      def toListWithIndex = tc.toListWithIndex(self)
      def fmap[B, C](f: B => C)(implicit fMap: FMap[A, B, C]) = fMap.fmap(self, f)
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

