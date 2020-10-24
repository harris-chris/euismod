package sportarray

//import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsBase, IsElement, PositionsData}
import IndicesObj.Index

import java.time.LocalDate
import shapeless.{HList, HNil, Lazy, :: => #:}
import shapeless.ops.hlist._

object ArrayDefs {

  abstract class IsArray[A] private extends IsBase[A] {
    type E
    implicit val eIsBase: IsBase[E]

    def getEmpty(self: A): A
    def getAtN(self: A, n: Int): E
    def length(self: A): Int
    def ::(self: A, other: E): A

    def ++(self: A, other: A): A = toList(other) match {
      case e :: es => ::(self, e)
      case Nil => self
    }
    def getILoc[R](self: A, r: R)(implicit getILoc: GetILoc[A, R]): A = getILoc.iloc(self, r)
    def toList(self: A): List[E] = (for(i <- 0 to length(self)) yield (getAtN(self, i))).toList
    def ndims(self: A): Int = ???
    def shape(self: A): Int = ???
  }
  object IsArray {
    def apply[A, _E: IsBase](
      fgetEmpty: A => A,
      fgetAtN: (A, Int) => _E,
      flength: A => Int,
      fcons: (A, _E) => A,
    ) (implicit _eIsBase: IsBase[_E],
    ): IsArray[A] { type E = _E } = new IsArray[A] { 
      type E = _E 
      implicit val eIsBase = _eIsBase
      def getEmpty(self: A): A = fgetEmpty(self)
      def getAtN(self: A, n: Int): E = fgetAtN(self, n)
      def length(self: A): Int = flength(self)
      def ::(self: A, other: _E): A = fcons(self, other)
    }
  }

  abstract class Updatable[A](implicit aIsArray: IsArray[A]) {
    def setAtIndex(self: A, i: Int, setTo: aIsArray.E): A
    def setILoc[R](self: A, r: R)(implicit set: SetILoc[A, R]): A
    def setLoc[R](self: A, r: R) = ???
    //def map[E1](self: A, f: E => E1)(implicit 
      //mIsSpBase: IsBase[E1],
      //outIsArr: IsArray[A] {type E = E1},
    //): A = getEmpty(self).foldLeft( fromList(self,
        //toListWithIndex(self).map((t: (I0, M1)) => (t._1, f(t._2)))
      //)
  }

  abstract class Indexed[A](implicit aIsArray: IsArray[A]) {
    type I
    type E0
    type E = (I, E0)
    def getIdx(self: A): Index[I]
    def getIdxElem(self: A, i: Int): I = getIdx(self)(i)
    def getLoc[R](self: A, ref: R)(implicit get: GetLoc[A, R]): A  
  }

  abstract class Is1d[A] private {}
  object Is1d {
    def apply[A, _E]( implicit 
      aIsArray: IsArray[A] { type E = _E },
      eIsElement: IsElement[_E],
    ) = new Is1d[A] {} 
  }

  abstract class Is2d[A] private {}
  object Is2d {
    def apply[A, _E]( implicit 
      aIsArray: IsArray[A] { type E = _E },
      eIs1d: Is1d[_E],
    ) = new Is2d[A] {} 
  }

  //abstract class Is2dArray[A] private extends IsArray[A]
  //object Is2dSpArr {
    //def apply[A, _A1: Is1dSpArr]: Is2dSpArr[A] = new Is2dSpArr[A] { type Self = A; type M1 = _A1 } 
  //}

  //abstract class Is3dSpArr[A] private extends IsSpArr[A]
  //object Is3dSpArr {
    //def apply[A, _A2: Is2dSpArr]: Is3dSpArr[A] = new Is3dSpArr[A] { type Self = A; type M1 = _A2 }
  //}

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

  trait GetILoc[A, R] {
    def iloc(self: A, ref: R): A
  }
  object GetILoc {
    def apply[A, R](implicit tc: GetILoc[A, R]): GetILoc[A, R] = tc
    implicit def GetILocForInt[A](implicit 
      isArr: IsArray[A], 
    ) = new GetILoc[A, Int] {
      override def iloc(self: A, ref: Int) = isArr.::(
        isArr.getEmpty(self), 
        isArr.getAtN(self, ref)
      )
    }
    implicit def iLocForListOfInts[A](implicit 
      isArr: IsArray[A],
    ) = new GetILoc[A, List[Int]] {
      def iloc(self: A, ref: List[Int]) = {
        val data: List[isArr.E] = ref.map(isArr.getAtN(self, _)).toList
        data.foldLeft(isArr.getEmpty(self))((a, b) => isArr.::(a, b)) 
      }
    }
    implicit def iLocForNull[A] = new GetILoc[A, Null] {
      def iloc(self: A, ref: Null) = self
    }
    implicit def iLocForHNil[A] = new GetILoc[A, HNil] {
      def iloc(self: A, ref: HNil) = self
    }
    implicit def iLocForHList[A, H, T <: HList](implicit 
      isArr: IsArray[A],
      ilocHead: Lazy[GetILoc[A, H]],
      ilocTail: GetILoc[A, T],
    ) = new GetILoc[A, H #: T] {
      def iloc(self: A, ref: H #: T) = ilocHead.value.iloc(self, ref.head)
    }
  }

  object IsArraySyntax {
    implicit class IsArrayOps[A, _E](self: A)(implicit 
      val tc: IsArray[A] { type E = _E },
    ) {
      def getEmpty = tc.getEmpty(self)
      def getAtN(n: Int): _E = tc.getAtN(self, n)
      def getILoc[R](r: R)(implicit getILoc: GetILoc[A, R]) = tc.getILoc(self, r)
      def ::(other: _E): A = tc.::(self, other)
      def length: Int = tc.length(self)
      def toList: List[_E] = tc.toList(self)
      //def fmap[B, C](f: B => C)(implicit fMap: FMap[A, B, C]) = fMap.fmap(self, f)
    }
  }

  abstract class GetLoc[A, R] {
    def loc(self: A, ref: R): A = ???
  }

  abstract class SetILoc[A, R] {
    def loc(self: A, ref: R): A = ???
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

