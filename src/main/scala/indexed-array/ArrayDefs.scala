package sportarray

//import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsBase, IsElement, PositionsData}
import IndicesObj.Index

import java.time.LocalDate
import shapeless.{HList, HNil, Lazy, :: => #:}
import shapeless.ops.hlist._
import shapeless._

object ArrayDefs {

  abstract class IsArray[A[_]] extends IsBase[A[_]] {
    type T
    type S
    implicit val tIsElem: IsElement[T]
    implicit val sIsBase: IsBase[S]

    def getEmpty(self: A[T]): A[T]
    def getAtN(self: A[T], n: Int): S
    def length(self: A[T]): Int
    def cons(self: A[T], other: S): A[T]
    def ::[R](self: A[T], other: R)(implicit c: Cons[A[_], T, R]): c.Out = c.cons(self, other)  

    def flatten(self: A[T]): List[T] = ???
    def reshape[O](self: A[T], shape: List[Int]): Option[O] = ??? 
    // for ++, we do not want to specify the actual implementation of other; any IsArray with the
    // same shape should be fine.
    def ++[B[_]](self: A[T], other: B[T])(implicit bIsArr: IsArray[B]): A[T] = ???
    def getILoc[R](self: A[T], r: R)(implicit getILoc: GetILoc[A, T, R]): A[T] = getILoc.iloc(self, r)
    def toList(self: A[T]): List[S] = (for(i <- 0 to length(self)) yield (getAtN(self, i))).toList
    def ndims(self: A[T]): Int = ???
    def shape(self: A[T]): List[Int] = ???
  }
  object IsArray {
    def apply[A[_], _T, _S](
      fgetEmpty: A[_T] => A[_T],
      fgetAtN: (A[_T], Int) => _S,
      flength: A[_T] => Int,
      fcons: (A[_T], _S) => A[_T],
    ) (implicit 
      _tIsElem: IsElement[_T],
      _sIsBase: IsBase[_S],
    ): IsArray[A] { type T = _T; type S = _S } = new IsArray[A] { 
      type T = _T
      type S = _S 
      implicit val tIsElem = _tIsElem
      implicit val sIsBase = _sIsBase
      def getEmpty(self: A[T]): A[T] = fgetEmpty(self)
      def getAtN(self: A[T], n: Int): S = fgetAtN(self, n)
      def length(self: A[T]): Int = flength(self)
      def cons(self: A[T], other: S): A[T] = fcons(self, other)
    }
  }

  object IsArraySyntax {
    implicit class IsArrayOps[A[_], _T, _S](self: A[_T])(implicit 
      val tc: IsArray[A] { type T = _T; type S = _S },
    ) {
      def getEmpty = tc.getEmpty(self)
      def getAtN(n: Int): _S = tc.getAtN(self, n)
      def getILoc[R](r: R)(implicit getILoc: GetILoc[A, _T, R]) = tc.getILoc(self, r)
      def ::[R](other: R)(implicit cons: Cons[A, _T, R]) = cons.cons(self, other)
      def length: Int = tc.length(self)
      def toList: List[_S] = tc.toList(self)
      //def fmap[B, C](f: B => C)(implicit fMap: FMap[A, B, C]) = fMap.fmap(self, f)
    }
  }

  //abstract class IsUpdatable[A[_]] extends IsArray[A] {
    //def setAtN(self: A, n: Int, setTo: E): A = {
      //val newData = toList(self).updated(n, setTo)
      //newData.foldLeft(getEmpty(self))((s, o) => ::(s, o)) 
    //}
    //def map[B, C](self: A, f: B => C)(implicit 
      //aMap: ArrMap[A, B, C],
    //): A = aMap.map(self, f)
    //def setILoc[R](self: A, r: R)(implicit set: SetILoc[A, R]): A = ???
    //def setLoc[R](self: A, r: R): A = ???
    //def mapList[C](self: A, f: E => C)(implicit 
      //cIsUpd: IsUpdatable[A] { type E = C },
    //): A = {
      //val newEmpty = getNew[C](self)
      //val newData: List[C] = toList(self).map(f)
      //newData.foldLeft(newEmpty)((s, o) => cIsUpd.::(s, o))
    //}
  //}
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

  //object IsUpdatableSyntax {
    //implicit class IsUpdatableOps[A, _E](self: A)(implicit 
      //val isUpd: IsUpdatable[A] { type E = _E },
    //) {
      //def setAtN(n: Int, setTo: _E): A = isUpd.setAtN(self, n, setTo)
      //def map[B, C](f: B => C)(implicit aMap: ArrMap[A, B, C]) = aMap.map(self, f)
    //}
  //}

  abstract class Is1d[A[_]] private {}
  object Is1d {
    def apply[A[_], _T, _S]( implicit 
      aIsArray: IsArray[A] { type T = _T; type S = _S },
    ) = new Is1d[A] {} 
  }

  abstract class Is2d[A[_]] private {}
  object Is2d {
    def apply[A[_], _T, _S[_]]( implicit 
      aIsArray: IsArray[A] { type T = _T; type S = _S[T] },
      eIs1d: Is1d[_S] { type T = _T },
    ) = new Is2d[A] {} 
  }

  abstract class Is3d[A[_]] private {}
  object Is3d {
    def apply[A[_], _T, _S[_]]( implicit 
      aIsArray: IsArray[A] { type T = _T; type S = _S[T] },
      eIs2d: Is2d[_S] { type T = _T },
    ) = new Is3d[A] {} 
  }

  //abstract class ArrMap[A, B, C] {
    //def map(self: A, f: B => C): A 
  //}
  //object ArrMap {
    //implicit def mapIfEIsBase[A, B, C](implicit 
      //aIsUpd: IsUpdatable[A] { type E = B }, 
      //cIsUpd: IsUpdatable[A] { type E = C },
    //): ArrMap[A, B, C] = new ArrMap[A, B, C] {
      //def map(self: A, f: B => C): A = aIsUpd.mapList[C](self, f)
    //}
    ////implicit def mapIfEIsArr[A, _E, B, C](implicit 
      ////isArr: IsUpdatable[A] { type E = _E },
      ////eIsArr: IsUpdatable[_E],
      ////eArrMap: ArrMap[_E, B, C],
    ////): ArrMap[A, B, C] = new ArrMap[A, B, C] {
      ////def map(self: A, f: B => C): A = isArr.mapList(
        ////self, e => eIsArr.map(e, f)
      ////)
    ////}
  //}

  trait Cons[A[_], T, R] {
    type Out
    def cons(self: A[_], other: R): Out
  }

  trait GetILoc[A[_], T, R] {
    def iloc(self: A[T], ref: R): A[T]
  }
  object GetILoc {
    def instance[A[_], T, R](f: (A[T], R) => A[T]): GetILoc[A, T, R] = new GetILoc[A, T, R] {
      def iloc(self: A[T], ref: R): A[T] = f(self, ref)
    }
    implicit def iLocInt[A[_], _T](implicit 
      isArr: IsArray[A] { type T = _T },
    ): GetILoc[A, _T, Int] = instance(
      (s, r) => isArr.cons(isArr.getEmpty(s), isArr.getAtN(s, r))
    )
    implicit def iLocListInt[A[_], _T](implicit 
      isArr: IsArray[A] { type T = _T },
    ): GetILoc[A, _T, List[Int]] = instance(
      (s, r) => {
        val data: List[isArr.S] = r.map(isArr.getAtN(s, _)).toList.reverse
        data.foldLeft(isArr.getEmpty(s))((a, b) => isArr.cons(a, b)) 
      }
    )
    implicit def iLocNull[A[_], _T](implicit isArr: IsArray[A] { type T = _T },
    ): GetILoc[A, _T, Null] = instance((s, r) => s)
    implicit def iLocHNil[A[_], _T](implicit isArr: IsArray[A] { type T = _T },
    ): GetILoc[A, _T, HNil] = instance((s, r) => s)
    implicit def iLocForHList[A[_], _T, Hd, Tl <: HList](implicit 
      isArr: IsArray[A] { type T = _T },
      ilocHead: Lazy[GetILoc[A, _T, Hd]],
      ilocTail: GetILoc[A, _T, Tl],
    ): GetILoc[A, _T, Hd #: Tl] = instance((s, r) => ilocHead.value.iloc(s, r.head))
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

