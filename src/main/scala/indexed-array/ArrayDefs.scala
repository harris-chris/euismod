package sportarray

//import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsBase, IsElement, PositionsData}
import IndicesObj.Index

import java.time.LocalDate
import shapeless.{HList, HNil, Lazy, :: => #:}
import shapeless.ops.hlist._

object ArrayDefs {

  abstract class IsArray[A] extends IsBase[A] {
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

  abstract class IsUpdatable[A] extends IsArray[A] {
    def getNew[_E](self: A)(implicit newIsUpd: IsUpdatable[A] { type E = _E }): A
    def setAtN(self: A, n: Int, setTo: E): A = {
      val newData = toList(self).updated(n, setTo)
      newData.foldLeft(getEmpty(self))((s, o) => ::(s, o)) 
    }
    def map[B, C](self: A, f: B => C)(implicit 
      aMap: ArrMap[A, B, C],
    ): A = aMap.map(self, f)
    def setILoc[R](self: A, r: R)(implicit set: SetILoc[A, R]): A = ???
    def setLoc[R](self: A, r: R): A = ???
    def mapList[C](self: A, f: E => C)(implicit 
      cIsUpd: IsUpdatable[A] { type E = C },
    ): A = {
      val newEmpty = getNew[C](self)
      val newData: List[C] = toList(self).map(f)
      newData.foldLeft(newEmpty)((s, o) => cIsUpd.::(s, o))
    }
  }
  //object IsUpdatable {
    //def fromArray[A, _E, N: IsElement](
      //fgetNew: apply[E] { (A, N) => A }, 
    //) (implicit 
      //_eIsBase: IsBase[_E],
      //isArr: IsArray[A] { type E = _E },
    //): IsUpdatable[A] { type E = _E } = new IsUpdatable[A] { 
      //type E = _E
      //implicit val eIsBase = _eIsBase
      //def getNew[_E]: CN[E] = fgetNew[_E]
      //def getEmpty(self: A): A = isArr.getEmpty(self)
      //def getAtN(self: A, n: Int): E = isArr.getAtN(self, n)
      //def length(self: A): Int = isArr.length(self)
      //def ::(self: A, other: E): A = isArr.::(self, other)
    //}
    //def apply[A, _E, _CN[_]](
      //fgetEmpty: A => A,
      //fgetAtN: (A, Int) => _E,
      //flength: A => Int,
      //fcons: (A, _E) => A,
    //) (implicit 
      //_eIsBase: IsBase[_E],
    //): IsUpdatable[A] { type E = _E } = new IsUpdatable[A] { 
      //type E = _E
      //type CN[_] = _CN[_]
      //implicit val eIsBase = _eIsBase
      //def getEmpty(self: A): A = fgetEmpty(self)
      //def getAtN(self: A, n: Int): E = fgetAtN(self, n)
      //def length(self: A): Int = flength(self)
      //def ::(self: A, other: E): A = fcons(self, other)
    //}
  //}

  object IsUpdatableSyntax {
    implicit class IsUpdatableOps[A, _E](self: A)(implicit 
      val isUpd: IsUpdatable[A] { type E = _E },
    ) {
      def setAtN(n: Int, setTo: _E): A = isUpd.setAtN(self, n, setTo)
      def map[B, C](f: B => C)(implicit aMap: ArrMap[A, B, C]) = aMap.map(self, f)
    }
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

  abstract class Is3d[A] private {}
  object Is3d {
    def apply[A, _E]( implicit 
      aIsArray: IsArray[A] { type E = _E },
      eIs2d: Is2d[_E],
    ) = new Is3d[A] {} 
  }

  abstract class ArrMap[A, B, C] {
    def map(self: A, f: B => C): A 
  }
  object ArrMap {
    implicit def mapIfEIsBase[A, B, C](implicit 
      aIsUpd: IsUpdatable[A] { type E = B }, 
      cIsUpd: IsUpdatable[A] { type E = C },
    ): ArrMap[A, B, C] = new ArrMap[A, B, C] {
      def map(self: A, f: B => C): A = aIsUpd.mapList[C](self, f)
    }
    //implicit def mapIfEIsArr[A, _E, B, C](implicit 
      //isArr: IsUpdatable[A] { type E = _E },
      //eIsArr: IsUpdatable[_E],
      //eArrMap: ArrMap[_E, B, C],
    //): ArrMap[A, B, C] = new ArrMap[A, B, C] {
      //def map(self: A, f: B => C): A = isArr.mapList(
        //self, e => eIsArr.map(e, f)
      //)
    //}
  }

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
        val data: List[isArr.E] = ref.map(isArr.getAtN(self, _)).toList.reverse
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

