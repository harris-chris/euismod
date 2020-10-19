package sportarray

//import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsSpBase, DataType}
import IndicesObj.Index

import java.time.LocalDate
import shapeless.{HList, HNil, Lazy, :: => #:}
import shapeless.ops.hlist._

object ArrayDefs {

  abstract class IsSpArr[A] extends IsSpBase[A] {
    type I0
    type M1T
    type Self = A
    implicit val m1IsSpBase: IsSpBase[M1T] {type Self = M1T}
    type M1 = m1IsSpBase.Self
    def getIdx(self: Self): Index[I0]
    def getElem(self: Self, i: Int): M1
    def getNil(self: Self): A
    
    def getIdxElem(self: Self, i: Int): I0 = getIdx(self)(i)
    def iloc[R](self: Self, r: R)(implicit iLoc: ILoc[A, R]): Self = iLoc.iloc(self, r)
    def ::(self: Self, other: (I0, M1)): Self
    def ++(self: Self, other: Self): Self = {
      val i: List[(I0, M1)] = toListWithIndex(other)
      i match {
        case (idx, elem) :: ts => ::(self, (idx, elem))
        case Nil => self
      }
    }
    //def fmap[B, C](self: Self, f: B => C)(implicit fMap: FMap[A, I0, M1, B, C]) = fMap.fmap(self, f)
    def length(self: Self): Int
    def toList(self: Self): List[M1] = (for(i <- 0 to length(self)) yield (getElem(self, i))).toList
    def toListWithIndex(self: Self): List[(I0, M1)] = getIdx(self).toList.zip(toList(self)).toList
    def fromList[II, IM](self: Self, l: List[(II, IM)])(implicit 
      outIsArr: IsSpArr[A] {type I0 = II; type M1T = IM},
    ): Self = l.foldLeft(getNil(self))((b, a) => outIsArr.::(b, a))
    def mapList[C](self: Self, f: M1 => C)(implicit 
      mIsSpBase: IsSpBase[C],
      outIsArr: IsSpArr[A] {type I0 = IsSpArr.this.I0; type M1T = C},
    ): Self = fromList(self,
        toListWithIndex(self).map((t: (I0, M1)) => (t._1, f(t._2)))
      )
  }
  object IsSpArr {
    def apply[A, _M1: IsSpBase]: IsSpArr[A] { type M1 = _M1 } = new IsSpArr[A] { type M1 = _M1 } 
  }

  //abstract class Is1dSpArr[A] private extends IsSpArr[A] 
  //object Is1dSpArr {
    //def apply[A, _I0, _M1: DataType]: Is1dSpArr[A] { type I0 = _I0; type M1 = _M1 } = 
      //new Is1dSpArr[A] { type I0 = _I0; type M1 = _M1 } 
  //}

  //object Is1dSpArr {
    //def apply[A, _T: DataType]: Is1dSpArr[A] = new Is1dSpArr[A] { type M1 = _T } 
  //}

  abstract class Is2dSpArr[A] private extends IsSpArr[A]
  object Is2dSpArr {
    def apply[A, _A1: Is1dSpArr]: Is2dSpArr[A] = new Is2dSpArr[A] { type Self = A; type M1 = _A1 } 
  }

  abstract class Is3dSpArr[A] private extends IsSpArr[A]
  object Is3dSpArr {
    def apply[A, _A2: Is2dSpArr]: Is3dSpArr[A] = new Is3dSpArr[A] { type Self = A; type M1 = _A2 }
  }

  //trait FMap[A[_, _], I, M1, B, C] {
    //type Out
    //def fmap(self: A[I, M1], f: B => C): Out 
  //}

  //object FMap {
    //implicit def fMapIfM1IsB[A[_, _], I, M1, B, C](implicit 
      //isArr: Lazy[IsSpArr[A, I, M1]], 
      //cIsSpBase: IsSpBase[C],
      //outIsArr: IsSpArr[A, I, C],
      //outIsSpBase: IsSpBase[A[I, C]],
    //) = new FMap[A, I, M1, M1, C] {
      //type Out = A[I, C]
      //def fmap(self: A[I, M1], f: M1 => C): A[I, C] = 
        //isArr.value.mapList(self, f)
    //}
    //implicit def fMapIfM1IsNotB[A[_, _], I0, I1, M1C[_, _], M2, B, C, M1O](implicit 
      //isArr: Lazy[IsSpArr[A, I0, M1C[I1, M2]]], 
      //m1IsSpBase: IsSpBase[M1C[I1, M2]],
      //m1IsArr: IsSpArr[M1C, I1, M2],
      //fMapForM1: FMap[M1C, I1, M2, B, C] {type Out = M1O},
      //outIsArr: IsSpArr[A, I0, M1O],
      //m1OutIsSpBase: IsSpBase[M1O],
    //) = new FMap[A, I0, M1C[I1, M2], B, C] {
      //type Out = A[I0, M1O]
      //def fmap(self: A[I0, M1C[I1, M2]], f: B => C): Out = isArr.value.mapList(
        //self, b => fMapForM1.fmap(b, f))
        ////val aList: List[(I0O, M1O)] = isArr.value.toListWithIndex(self)
        ////val m1List: List[(I0O, M1O)] = aList.map((t: (I0O, M1O)) => (t._1, m1IsArr.fmap(t._2, f)))
        ////m1List.foldLeft(isArr.value.getNil(self))((b, a) => isArr.value.::(b, a))
    //}
  //}

  trait ILoc[A, R] {
    def iloc(self: A, ref: R): A
  }
  object ILoc {
    def apply[A, R](implicit tc: ILoc[A, R]): ILoc[A, R] = tc
    implicit def iLocForInt[A](implicit 
      isArr: IsSpArr[A], 
    ) = new ILoc[A, Int] {
      override def iloc(self: A, ref: Int) = isArr.::(
        isArr.getNil(self), 
        (isArr.getIdx(self)(ref), isArr.getElem(self, ref))
      )
    }
    implicit def iLocForListOfInts[A](implicit 
      isArr: IsSpArr[A],
    ) = new ILoc[A, List[Int]] {
      def iloc(self: A, ref: List[Int]) = {
        val data: List[isArr.M1] = ref.map(isArr.getElem(self, _)).toList
        val idx0: Index[isArr.I0] = isArr.getIdx(self)
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
      isArr: IsSpArr[A],
      ilocHead: Lazy[ILoc[A, H]],
      ilocTail: ILoc[A, T],
    ) = new ILoc[A, H #: T] {
      def iloc(self: A, ref: H #: T) = ilocHead.value.iloc(self, ref.head)
    // ilocH.iloc(self, ref.head).toList.map(iLocOut.iloc(_, ref.tail))// need to map the iloc tail over every element
    // This is going to be easier if iloc doesn't squeeze the array. At least the squeeze operation could be done later
    }
  }

  object IsSpArrSyntax {
    implicit class IsSpArrOps[A](self: A)(implicit 
      val tc: IsSpArr[A],
    ) {
      def getNil = tc.getNil(self)
      def getElem(i: Int) = tc.getElem(self, i)
      def iloc[R](r: R)(implicit iLoc: ILoc[A, R]) = tc.iloc(self, r)
      def ::(other: (tc.I0, tc.M1)): A = tc.::(self, other)
      def length: Int = tc.length(self)
      def toList: List[tc.M1] = tc.toList(self)
      def toListWithIndex = tc.toListWithIndex(self)
      //def fmap[B, C](f: B => C)(implicit fMap: FMap[A, B, C]) = fMap.fmap(self, f)
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

