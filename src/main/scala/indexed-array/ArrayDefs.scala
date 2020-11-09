package sportarray

//import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsBase, IsElement, PositionsData}
import IndicesObj.Index

import java.time.LocalDate
import shapeless.{HList, HNil, Lazy, :: => #:}
import shapeless.ops.hlist._
import shapeless._
import nat._

object ArrayDefs {

  case class Element[T] (
    get: T
  ) extends IsBase[Element[T]]

  abstract class IsArrBase[A, T] extends IsBase[A] { 
    type S 
    def getEmpty(self: A): A
    def getAtN(self: A, n: Int): S
    def length(self: A): Int
    def cons(self: A, other: S): A
  }

  abstract class IsArray[A[_], T] extends IsArrBase[A[T], T] {
    type S
    implicit val sIsBase: IsBase[S]

    def getEmpty(self: A[T]): A[T]
    def getAtN(self: A[T], n: Int): S
    def length(self: A[T]): Int
    def cons(self: A[T], other: S): A[T]

    def ::(a: A[T], o: S): A[T] = cons(a, o)  
    // for ++, we do not want to specify the actual implementation of other; any IsArray with the
    // same shape should be fine.
    def ++[B[_]](self: A[T], other: B[T])(implicit bIsArr: IsArray[B, T]): A[T] = ???
    def getILoc[R](self: A[T], r: R)(implicit getILoc: GetILoc[A, T, R]): A[T] = getILoc.iloc(self, r)
    def toList(self: A[T]): List[S] = (for(i <- 0 to length(self) - 1) yield (getAtN(self, i))).toList
    def ndims(self: A[T]): Int = ???
    def shape(self: A[T]): List[Int] = ???
    def getArrays(self: A[T])(implicit gs: GetArrs[A[T], T, HNil]): gs.Out = gs.getArrs(self, HNil)
  }
  object IsArray {
    def apply[A[_], T, _S](
      fgetEmpty: A[T] => A[T],
      fgetAtN: (A[T], Int) => _S,
      flength: A[T] => Int,
      fcons: (A[T], _S) => A[T],
    ) (implicit 
      _sIsBase: IsBase[_S],
    ): IsArray[A, T] { type S = _S } = new IsArray[A, T] { 
      type S = _S
      implicit val sIsBase = _sIsBase
      def getEmpty(self: A[T]): A[T] = fgetEmpty(self)
      def getAtN(self: A[T], n: Int): S = fgetAtN(self, n)
      def length(self: A[T]): Int = flength(self)
      def cons(self: A[T], other: S): A[T] = fcons(self, other)
    }
  }

  object IsArraySyntax {
    implicit class IsArrayOps[A[_], T, _S](a: A[T])(implicit 
      val tc: IsArray[A, T] { type S = _S },
    ) {
      def getEmpty = tc.getEmpty(a)
      def getAtN(n: Int): _S = tc.getAtN(a, n)
      def getILoc[R](r: R)(implicit getILoc: GetILoc[A, T, R]) = tc.getILoc(a, r)
      def ::(other: _S) = tc.cons(a, other)
      def length: Int = tc.length(a)
      def toList: List[_S] = tc.toList(a)
      def getArrays(implicit gs: GetArrs[A[T], T, HNil]): gs.Out = tc.getArrays(a)
      //def fmap[B, C](f: B => C)(implicit fMap: FMap[A, B, C]) = fMap.fmap(self, f)
    }
  }

  sealed trait GetArrs[A, T, L <: HList] {self =>
    type Out <: HList
    def getArrs(a: A, l: L): Out
  }
  object GetArrs {
    implicit def getArrsIfSIsEle[A, T, _S, L <: HList](implicit 
      sIsEle: IsElement[_S],
      aIsABs: IsArrBase[A, T] { type S = _S },
    ): GetArrs[A, T, L] { type Out = A :: L } = new GetArrs[A, T, L] {
      type Out = A :: L
      def getArrs(a: A, l: L): Out = aIsABs.getEmpty(a) :: l
    }
    implicit def getArrsIfSIsArr[A, T, _S, L <: HList](implicit 
      aIsArr: IsArrBase[A, T] { type S = _S },
      gaForS: GetArrs[_S, T, A :: L],
    ): GetArrs[A, T, L] { type Out = gaForS.Out } = new GetArrs[A, T, L] {
      type Out = gaForS.Out
      def getArrs(a: A, l: L): gaForS.Out = gaForS.getArrs(
        aIsArr.getAtN(a, 0), 
        aIsArr.getEmpty(a) :: l
      )
    }
  }

  abstract class Reshapes[A[_], T] {self =>
    type S
    implicit val aIsArr: IsArray[A, T] { type S = self.S }
    def flatten(a: A[T])(implicit fl: Flatten[A, T]): List[T] = fl.flatten(a)
    def reshape[GAOut <: HList, SH <: HList](a: A[T], shape: SH)(implicit 
      fl: Flatten[A, T],
      ga: GetArrs[A[T], T, HNil] { type Out = GAOut },
      rs: ArrFromListRT[T, GAOut, SH],
    ): rs.Out = {
      val listT: List[T] = fl.flatten(a)
      val arrHlist: GAOut = ga.getArrs(a, HNil)
      rs.fromList(Some(listT.reverse), arrHlist, shape)
    }
  }
  object Reshapes {
    def apply[A[_], T, _S](implicit _aIsArr: IsArray[A, T] { type S = _S }): Reshapes[A, T] { type S = _S } =
      new Reshapes[A, T] { 
        type S = _S
        implicit val aIsArr = _aIsArr
      }
  }
  object ReshapesSyntax {
    implicit class ReshapesOps[A[_], T, _S](a: A[T])(implicit 
      aIsArr: IsArray[A, T] { type S = _S },
      aRes: Reshapes[A, T] { type S = _S },
    ) { self =>
      def flatten(implicit flatten: Flatten[A, T]): List[T] = aRes.flatten(a)
      def reshape[GAOut <: HList, SH <: HList](shape: SH)(implicit 
        fl: Flatten[A, T],
        ga: GetArrs[A[T], T, HNil] { type Out = GAOut },
        rs: ArrFromListRT[T, GAOut, SH],
      ) = aRes.reshape(a, shape)
      //def map[B, C](f: B => C)(implicit aMap: ArrMap[A, B, C]) = aMap.map(self, f)
    }
  }

  trait ArrFromListRT[_S, Arrs <: HList, SH <: HList] {
    type Out 
    def fromList(lsO: Option[List[_S]], la: Arrs, shape: SH): Out
  }
  object ArrFromListRT {
    implicit def ifSingleElemRemainingInShape[T, H0, H1, H2p <: HList](implicit 
      hIsABs: IsArrBase[H1, T] { type S = H0 },
    ): ArrFromListRT[H0, H1 :: H2p, Int :: HNil] { type Out = Option[H1] } = 
    new ArrFromListRT[H0, H1 :: H2p, Int :: HNil] { 
      type Out = Option[H1] 
      def fromList(lsO: Option[List[H0]], la: H1 :: H2p, sh: Int :: HNil) = lsO.flatMap( 
        ls => createArrs[H1, T, H0](la.head, Nil, ls, sh.head)
      ).flatMap(arrs => if(arrs.length == 1){Some(arrs(0))} else {None})
    }

    implicit def ifMultipleElemsRemainingInShape[T, H0, H1, H2p <: HList, SH2p <: HList](implicit 
      hIsABs: IsArrBase[H1, T] { type S = H0 },
      rsForNxt: ArrFromListRT[H1, H2p, Int :: SH2p],   
    ): ArrFromListRT[H0, H1 :: H2p, Int :: Int :: SH2p] { type Out = rsForNxt.Out } = 
    new ArrFromListRT[H0, H1 :: H2p, Int :: Int :: SH2p] { 
      type Out = rsForNxt.Out 
      def fromList(lsO: Option[List[H0]], la: H1 :: H2p, sh: Int :: Int :: SH2p) = {
        println("RUNNING ifLAIsHList")
        val thisA: H1 = la.head
        val thisArrs: Option[List[H1]] = lsO flatMap { ls => createArrs[H1, T, H0](thisA, Nil, ls, sh.head) }
        rsForNxt.fromList(thisArrs, la.tail, sh.tail)
      }
    }
    def createArrs[A, T, _S](
      aEmpty: A, as: List[A], l: List[_S], width: Int,
    )(implicit aIsABs: IsArrBase[A, T] { type S = _S }): Option[List[A]] = 
      l.length match {
        case 0 => {println(f"RETURNING FROM CREATEARRS ${as} "); Some(as)}
        case x if x >= width => {
          println("SPLITTING")
          val (ths, rst) = l.splitAt(width)
          val thsA: A = ths.foldLeft(aEmpty)((s, o) => aIsABs.cons(s, o))
          println(f"LENGTH REMAINING ${rst.length}")
          createArrs[A, T, _S](aEmpty, thsA :: as, rst, width)
        }
        case _ => {println("CREATEARRS RETURNING NONE"); None}
      }
  }

  trait Flatten[A[_], T] { self =>
    def flatten(a: A[T]): List[T]
  }
  object Flatten {
    implicit def flattenIfSIsT[A[_], T: IsElement](implicit 
      aIsArr: IsArray[A, T] { type S = T },
    ): Flatten[A, T] = new Flatten[A, T] {
      def flatten(a: A[T]): List[T] = aIsArr.toList(a)
    }
    implicit def flattenIfSIsNotT[A[_], T, _S[T]](implicit 
      aIsArr: IsArray[A, T] { type S = _S[T] },
      sRes: Reshapes[_S, T], 
      sFl: Flatten[_S, T],
    ): Flatten[A, T] = new Flatten[A, T] {
      def flatten(a: A[T]): List[T] = aIsArr.toList(a).map(sRes.flatten(_)).flatten 
    }
  }


  //abstract class IsUpdatable[A[_], T](implicit 
    //val aIsArr: IsArray[A, T],
  //) {
    //type S = aIsArr.S
    //def getNew[_T]: A[_T] 

    //def map[_T, _S[_]](a: A[T], f: T => _T): A[_T] = {
      //val newEmpty = getNew[_T]
      //val newData: List[S] = aIsArr.toList(a).map(s => sIsUpd.map(s, f))
      //newData.foldLeft(newEmpty)((s, o) => aIsArr.cons(s, o))
    //}
    //def setAtN(self: A[T], n: Int, setTo: S): A[T] = {
      //val newData = aIsArr.toList(self).updated(n, setTo)
      //newData.foldLeft(aIsArr.getEmpty(self))((s, o) => aIsArr.cons(s, o)) 
    //}
    //def setILoc[R](self: A[T], r: R)(implicit set: SetILoc[A, T, R]): A[T] = ???
    //def setLoc[R](self: A[T], r: R): A[T] = ???
  //}

  //object IsUpdatableSyntax {
    //implicit class IsUpdatableOps[A[_], T, _S](a: A[T])(implicit 
      //isUpd: IsUpdatable[A, T] { type S = _S },
    //) { self =>
      //def setAtN(n: Int, setTo: _S): A[T] = isUpd.setAtN(a, n, setTo)
      ////def map[B, C](f: B => C)(implicit aMap: ArrMap[A, B, C]) = aMap.map(self, f)
    //}
  //}

  abstract class Is1d[A] private {}
  object Is1d {
    def apply[A[_], T](implicit aIsArr: IsArrBase[A[T], T] { type S = T }): Is1d[A[T]] = new Is1d[A[T]] {}
  }

  abstract class Is2d[A] private {}
  object Is2d {
    def apply[A[_], T, _S]( implicit 
      aIsArray: IsArrBase[A[T], T] { type S = _S },
      sIs1d: Is1d[_S],
    ): Is2d[A[T]] = new Is2d[A[T]] {}
  }

  abstract class Is3d[A] private {}
  object Is3d {
    def apply[A[_], T, _S]( implicit 
      aIsArray: IsArrBase[A[T], T] { type S = _S },
      sIs2d: Is2d[_S],
    ): Is3d[A[T]] = new Is3d[A[T]] {}
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

  trait GetILoc[A[_], T, R] {
    def iloc(self: A[T], ref: R): A[T]
  }
  object GetILoc {
    def instance[A[_], T, R](f: (A[T], R) => A[T]): GetILoc[A, T, R] = new GetILoc[A, T, R] {
      def iloc(self: A[T], ref: R): A[T] = f(self, ref)
    }
    implicit def iLocInt[A[_], T](implicit 
      isArr: IsArray[A, T],
    ): GetILoc[A, T, Int] = instance(
      (s, r) => isArr.cons(isArr.getEmpty(s), isArr.getAtN(s, r))
    )
    implicit def iLocListInt[A[_], T](implicit 
      isArr: IsArray[A, T],
    ): GetILoc[A, T, List[Int]] = instance(
      (s, r) => {
        val data: List[isArr.S] = r.map(isArr.getAtN(s, _)).toList.reverse
        data.foldLeft(isArr.getEmpty(s))((a, b) => isArr.cons(a, b)) 
      }
    )
    implicit def iLocNull[A[_], T](implicit isArr: IsArray[A, T],
    ): GetILoc[A, T, Null] = instance((s, r) => s)
    implicit def iLocHNil[A[_], T](implicit isArr: IsArray[A, T],
    ): GetILoc[A, T, HNil] = instance((s, r) => s)
    implicit def iLocForHList[A[_], T, Hd, Tl <: HList](implicit 
      isArr: IsArray[A, T],
      ilocHead: Lazy[GetILoc[A, T, Hd]],
      ilocTail: GetILoc[A, T, Tl],
    ): GetILoc[A, T, Hd #: Tl] = instance((s, r) => ilocHead.value.iloc(s, r.head))
  }

  abstract class GetLoc[A[_], T, R] {
    def loc(self: A[T], ref: R): A[T] = ???
  }

  abstract class SetILoc[A[_], T, R] {
    def loc(self: A[T], ref: R): A[T] = ???
  }
}

