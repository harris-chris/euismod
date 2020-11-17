package sportarray

//import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsBase}
import IndicesObj.Index

import scala.annotation.implicitNotFound
import java.time.LocalDate
import shapeless.{HList, HNil, Lazy, :: => #:}
import shapeless.ops.hlist._
import shapeless._
import nat._
import shapeless.ops.nat._

object ArrayDefs {

  case class Element[T] (
    get: T
  ) extends IsBase[Element[T]]

  abstract class IsArrBase[A, T] extends IsBase[A] { 
    type S 
    def getAtN(self: A, n: Int): S
    def length(self: A): Int
    def cons(self: A, other: S): A
  }

  @implicitNotFound(f"Cannot find IsArray implicit")
  abstract class IsArray[A[_], T] extends IsArrBase[A[T], T] { self =>
    type S
    //implicit val sIsBase: IsBase[S]

    def getEmpty[_T]: A[_T] 
    def getAtN(a: A[T], n: Int): S
    def length(a: A[T]): Int
    def cons(a: A[T], sub: S): A[T]

    def apply[R](a: A[T], r: R)(implicit gi: GetILoc[A[T], R]): gi.Out = gi(a, r)
    def empty: A[T] = getEmpty[T]
    def ::(a: A[T], o: S): A[T] = cons(a, o)  
    // for ++, we do not want to specify the actual implementation of other; any IsArray with the
    // same shape should be fine.
    def ++[B[_]](a: A[T], other: B[T])(implicit bIsArr: IsArray[B, T]): A[T] = ???
    def toList(a: A[T]): List[S] = (for(i <- 0 to length(a) - 1) yield (getAtN(a, i))).toList
    def fromList(listS: List[S]): A[T] = 
      listS.reverse.foldLeft(getEmpty[T])((e, s) => cons(e, s))
    def ndims[GSOut <: HList](a: A[T])(implicit 
      gs: GetShape[A[T], T, HNil] { type Out = GSOut }, 
      tl: ToList[GSOut, Int],
    ): Int = shape(a).toList[Int].length
    def shape(a: A[T])(implicit gs: GetShape[A[T], T, HNil]): gs.Out = gs.getShape(a, HNil)
    def getArraysAsc(a: A[T])(implicit ga: GetArrsAsc[A, T, HNil]): ga.Out = ga(HNil)
    def getArraysDesc(a: A[T])(implicit ga: GetArrsDesc[A, T, HNil]): ga.Out = ga(HNil)
    def flatten(a: A[T])(implicit fl: Flatten[A, T]): List[T] = fl.flatten(a)
    def fromElems[GAOut <: HList, SH <: HList](a: A[T], listT: List[T], shape: SH)(implicit 
      ga: GetArrsAsc[A, T, HNil] { type Out = GAOut },
      fr: FromElemsRT[T, GAOut, SH], 
    ): fr.Out = fr.fromElems(Some(listT.reverse), ga(HNil), shape)
    def fromElems[GAOut <: HList](a: A[T], listT: List[T])(implicit 
      ga: GetArrsAsc[A, T, HNil] { type Out = GAOut },
      fr: FromElemsRT[T, GAOut, Int :: HNil], 
    ): fr.Out = fr.fromElems(Some(listT.reverse), ga(HNil), listT.length :: HNil)
    def reshape[GAOut <: HList, SH <: HList](a: A[T], shape: SH)(implicit 
      fl: Flatten[A, T],
      ga: GetArrsAsc[A, T, HNil] { type Out = GAOut },
      fr: FromElemsRT[T, GAOut, SH],
    ): fr.Out = {
      fromElems(a, fl.flatten(a), shape)
    }
    def map[_T, GAOut <: HList, GSOut <: HList, SH <: HList](a: A[T], f: (T) => _T)(implicit
      fl: Flatten[A, T],
      a_tIsArr: IsArray[A, _T],
      gs: GetShape[A[T], T, HNil] { type Out = GSOut }, 
      ga: GetArrsAsc[A, _T, HNil] { type Out = GAOut },
      fr: FromElemsRT[_T, GAOut, GSOut] { type Out = Option[A[_T]] },
    ): A[_T] = {
      val sh: GSOut = gs.getShape(a, HNil)
      val list_t: List[_T] = flatten(a).map(f)
      val empty_t: A[_T] = getEmpty[_T]
      val arrs: GAOut = ga(HNil)
      fr.fromElems(Some(list_t.reverse), arrs, sh).get 
    }
    def getReducedArraysForApply[R, O](a: A[T], r: R)(implicit
      gr: GetRdcArrs[A, T, R] { type Out = O }
    ): O = gr(a, r)
  }

  object IsArraySyntax {
    implicit class IsArrayOps[A[_], T, _S](a: A[T])(implicit 
      val tc: IsArray[A, T] { type S = _S },
    ) {
      def getEmpty[_T] = tc.getEmpty[_T]
      def empty = tc.getEmpty[T]
      def getAtN(n: Int): _S = tc.getAtN(a, n)
      def apply[R](r: R)(implicit getILoc: GetILoc[A[T], R]) = tc.apply(a, r)
      def ::(other: _S) = tc.cons(a, other)
      def length: Int = tc.length(a)
      def toList: List[_S] = tc.toList(a)
      def fromList(listS: List[_S]): A[T] = tc.fromList(listS)
      def getArraysAsc(implicit ga: GetArrsAsc[A, T, HNil]): ga.Out = tc.getArraysAsc(a)
      def getArraysDesc(implicit ga: GetArrsDesc[A, T, HNil]): ga.Out = tc.getArraysDesc(a)
      def shape(implicit gs: GetShape[A[T], T, HNil]): gs.Out = tc.shape(a)
      def flatten(implicit fl: Flatten[A, T]): List[T] = fl.flatten(a)
      def fromElems[Arrs <: HList, SH <: HList](listT: List[T], shape: SH)(implicit 
        ga: GetArrsAsc[A, T, HNil] { type Out = Arrs },
        fr: FromElemsRT[T, Arrs, SH],
      ): fr.Out = tc.fromElems(a, listT, shape)
      def reshape[GAOut <: HList, SH <: HList](shape: SH)(implicit 
        fl: Flatten[A, T],
        ga: GetArrsAsc[A, T, HNil] { type Out = GAOut },
        rs: FromElemsRT[T, GAOut, SH],
      ) = tc.reshape(a, shape)
      def map[_T, GAOut <: HList, GSOut <: HList, SH <: HList](f: T => _T)(implicit
        a_tIsArr: IsArray[A, _T],
        fl: Flatten[A, T],
        gs: GetShape[A[T], T, HNil] { type Out = GSOut }, 
        ga: GetArrsAsc[A, _T, HNil] { type Out = GAOut },
        fr: FromElemsRT[_T, GAOut, GSOut] { type Out = Option[A[_T]] },
      ): A[_T] = tc.map(a, f)
    }
  }

  sealed trait GetRdcArrs[A[_], T, I] {self =>
    type Out <: HList
    def apply(a: A[T], i: I): Out
  }
  object GetRdcArrs {
    type Aux[A[_], T, I <: HList, O <: HList] = GetRdcArrs[A, T, I] { type Out = O }
    def instance[A[_], T, I <: HList, O <: HList](f: (A[T], I) => O): Aux[A, T, I, O] = new GetRdcArrs[A, T, I] { 
      type Out = O
      def apply(a: A[T], i: I): Out = f(a, i)
    }
    implicit def ifIIsHList[
      A[_], T, Inp <: HList, Arrs <: HList, Filt <: HList, NumInts <: Nat, ArrsR <: HList, RdArrs <: HList,
    ] (implicit 
      ga: GetArrsDesc[A, T, HNil] { type Out = Arrs },
      fl: Filter[Inp, Int] { type Out = Filt },
      lf: Length[Filt] { type Out = NumInts },
      r1: Reverse[Arrs] { type Out = ArrsR },
      dr: Drop[ArrsR, NumInts] { type Out = RdArrs },
      r2: Reverse[RdArrs],
    ): Aux[A, T, Inp, r2.Out] = instance((a: A[T], inp: Inp) => r2(dr(r1(ga(HNil)))))
  }

  sealed trait GetArrsDesc[A[_], T, L <: HList] {self =>
    type Out <: HList
    def apply(l: L): Out
  }
  object GetArrsDesc {
    type Aux[A[_], T, L <: HList, O <: HList] = GetArrsDesc[A, T, L] { type Out = O }
    def instance[A[_], T, L <: HList, O <: HList](f: L => O): Aux[A, T, L, O] = new GetArrsDesc[A, T, L] { 
      type Out = O
      def apply(l: L): Out = f(l)
    }
    implicit def ifSIsEle[A[_], T, _S, L <: HList](implicit 
      aIsArr: IsArray[A, T] { type S = T },
      rv: Reverse[A[T] :: L],
    ): Aux[A, T, L, rv.Out] = instance(l => rv(aIsArr.getEmpty[T] :: l))
    implicit def ifSIsArr[A[_], T, _S[_], _S1, L <: HList](implicit 
      aIsABs: IsArray[A, T] { type S = _S[T] },
      sIsABs: IsArray[_S, T],
      gaForS: GetArrsDesc[_S, T, A[T] :: L],
    ): Aux[A, T, L, gaForS.Out] = instance( l => 
      gaForS( aIsABs.getEmpty[T] :: l)
    )
  }

  sealed trait GetArrsAsc[A[_], T, L <: HList] {self =>
    type Out <: HList
    def apply(l: L): Out
  }
  object GetArrsAsc {
    type Aux[A[_], T, L <: HList, O <: HList] = GetArrsAsc[A, T, L] { type Out = O }
    def instance[A[_], T, L <: HList, O <: HList](f: L => O): Aux[A, T, L, O] = new GetArrsAsc[A, T, L] { 
      type Out = O
      def apply(l: L): Out = f(l)
    }
    implicit def ifLIsHList[A[_], T, L <: HList, Asc <: HList]( implicit 
      ga: GetArrsDesc[A, T, L] { type Out = Asc },
      rv: Reverse[Asc],
    ): Aux[A, T, L, rv.Out] = instance(l => rv(ga(l)))
  }

  sealed trait GetShape[A, T, L <: HList] {self =>
    type Out <: HList
    def getShape(a: A, l: L): Out
  }
  object GetShape {
    implicit def gsIfSIsEle[A, T, _S, L <: HList](implicit 
      aIsABs: IsArrBase[A, T] { type S = T },
    ): GetShape[A, T, L] { type Out = Int :: L } = new GetShape[A, T, L] {
      type Out = Int :: L
      def getShape(a: A, l: L): Out = aIsABs.length(a) :: l
    }
    implicit def gsIfSIsArr[A, T, _S, L <: HList](implicit 
      aIsABs: IsArrBase[A, T] { type S = _S },
      gsForS: GetShape[_S, T, Int :: L],
    ): GetShape[A, T, L] { type Out = gsForS.Out } = new GetShape[A, T, L] {
      type Out = gsForS.Out
      def getShape(a: A, l: L): gsForS.Out = gsForS.getShape(
        aIsABs.getAtN(a, 0), 
        aIsABs.length(a) :: l
      )
    }
  }

  trait FromElemsRT[_S, Arrs <: HList, SH <: HList] {
    type Out 
    def fromElems(lsO: Option[List[_S]], la: Arrs, shape: SH): Out
  }
  object FromElemsRT {
    implicit def ifSingleElemRemainingInShape[T, H0, H1, H2p <: HList](implicit 
      hIsABs: IsArrBase[H1, T] { type S = H0 },
    ): FromElemsRT[H0, H1 :: H2p, Int :: HNil] { type Out = Option[H1] } = 
    new FromElemsRT[H0, H1 :: H2p, Int :: HNil] { 
      type Out = Option[H1] 
      def fromElems(lsO: Option[List[H0]], la: H1 :: H2p, sh: Int :: HNil): Out = {
        lsO.flatMap( 
          ls => createArrs[H1, T, H0](la.head, Nil, ls, sh.head)
        ).flatMap(arrs => if(arrs.length == 1){Some(arrs(0))} else {None})
      }
    }

    implicit def ifMultipleElemsRemainingInShape[T, H0, H1, H2p <: HList, SH2p <: HList](implicit 
      hIsABs: IsArrBase[H1, T] { type S = H0 },
      rsForNxt: FromElemsRT[H1, H2p, Int :: SH2p],   
    ): FromElemsRT[H0, H1 :: H2p, Int :: Int :: SH2p] { type Out = rsForNxt.Out } = 
    new FromElemsRT[H0, H1 :: H2p, Int :: Int :: SH2p] { 
      type Out = rsForNxt.Out 
      def fromElems(lsO: Option[List[H0]], la: H1 :: H2p, sh: Int :: Int :: SH2p) = {
        val thisA: H1 = la.head
        val h1Nil = Nil: List[H1]
        val thisArrs: Option[List[H1]] = lsO flatMap { ls => createArrs[H1, T, H0](thisA, h1Nil, ls, sh.head) }
        rsForNxt.fromElems(thisArrs, la.tail, sh.tail)
      }
    }
    def createArrs[A, T, _S](
      aEmpty: A, as: List[A], l: List[_S], width: Int,
    )(implicit aIsABs: IsArrBase[A, T] { type S = _S }): Option[List[A]] = 
      l.length match {
        case 0 => Some(as.reverse)
        case x if x >= width => {
          val (ths, rst) = l.splitAt(width)
          val thsA: A = ths.foldLeft(aEmpty)((s, o) => aIsABs.cons(s, o))
          createArrs[A, T, _S](aEmpty, thsA :: as, rst, width)
        }
        case _ => None
      }
  }

  trait Flatten[A[_], T] { self =>
    def flatten(a: A[T]): List[T]
  }
  object Flatten {
    implicit def flattenIfSIsT[A[_], T](implicit 
      aIsArr: IsArray[A, T] { type S = T },
    ): Flatten[A, T] = new Flatten[A, T] {
      def flatten(a: A[T]): List[T] = aIsArr.toList(a)
    }
    implicit def flattenIfSIsNotT[A[_], T, _S[T]](implicit 
      aIsArr: IsArray[A, T] { type S = _S[T] },
      sIsArr: IsArray[_S, T], 
      sFl: Flatten[_S, T],
    ): Flatten[A, T] = new Flatten[A, T] {
      def flatten(a: A[T]): List[T] = aIsArr.toList(a).map(sIsArr.flatten(_)).flatten 
    }
  }

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

  trait GetSome[A[_], T, R] {
    type Out
    def iloc(a: A[T], ref: R): Out
  }
  object GetSome {
    implicit def ifListInt[A[_], T]: GetSome[A, T, Double] { type Out = A[T] } = new GetSome[A, T, Double] {
      type Out = A[T]
      def iloc(a: A[T], ref: Double): Out = a
    }
  }

  trait GetILoc[A, R] {
    type Out
    def apply(a: A, ref: R): Out
  }
  object GetILoc {
    type Aux[A, R, O] = GetILoc[A, R] { type Out = O }
    def instance[A, R, O](f: (A, R) => O): Aux[A, R, O] = new GetILoc[A, R] { 
      type Out = O
      def apply(a: A, ref: R): Out = f(a, ref)
    }

    implicit def ifRefIsInt[A[_], T, _S](implicit
      aIsArr: IsArray[A, T] { type S = _S }
    ): Aux[A[T], Int, _S] = instance((a, r) => aIsArr.getAtN(a, r))

    implicit def ifRefIsListInt[A[_], T](implicit
      aIsArr: IsArray[A, T],
    ): Aux[A[T], List[Int], A[T]] = instance((a, rs) => aIsArr.fromList(
      rs.map(aIsArr.getAtN(a, _)))
    )

    implicit def ifRefIsHList[A[_], T, R <: HList, Rd <: HList](implicit 
      rd: GetRdcArrs.Aux[A, T, R, Rd],
      gi: GetILocHList[A[T], R, Rd],
    ): Aux[A[T], R, gi.Out] = instance((a, r) => gi(a, r, rd(a, r))) 

  }

  trait GetILocHList[A, R, Arrs] {
    type Out
    def apply(a: A, ref: R, arrs: Arrs): Out
  }
  object GetILocHList {
    type Aux[A, R, Arrs, O] = GetILocHList[A, R, Arrs] { type Out = O }
    def instance[A, R, Arrs, O](f: (A, R, Arrs) => O): Aux[A, R, Arrs, O] = new GetILocHList[A, R, Arrs] { 
      type Out = O
      def apply(a: A, ref: R, arrs: Arrs): Out = f(a, ref, arrs)
    }
    implicit def ifHeadIsInt[A[_], T, _S, R1p <: HList, Arrs <: HList, O](implicit
      aIsArr: IsArray[A, T] { type S = _S },
      iLocS: GetILocHList[_S, R1p, Arrs], 
    ): Aux[A[T], Int :: R1p, Arrs, iLocS.Out] = instance((a, r, arrs) => 
      iLocS(aIsArr.getAtN(a, r.head), r.tail, arrs)
    )
    implicit def ifHeadIsListInt[A[_], T, _S, R1p <: HList, A0[_], A1p <: HList, SO](implicit
      aIsArr: IsArray[A, T] { type S = _S },
      iLocS: GetILocHList[_S, R1p, A1p] { type Out = SO }, 
      outIsArr: IsArray[A0, T] { type S = SO }
    ): Aux[A[T], List[Int] :: R1p, A0[T] :: A1p, A0[T]] = instance((a, r, arrs) => {
      val origS: List[_S] = r.head.map(aIsArr.getAtN(a, _))
      val locedS: List[SO] = origS.map(iLocS(_, r.tail, arrs.tail))
      outIsArr.fromList(locedS)
    })

    implicit def ifRefIsHNil[A, Arrs <: HList]: Aux[A, HNil, Arrs, A] = instance((a, r, arrs) => a)
  }

  abstract class GetLoc[A[_], T, R] {
    def loc(self: A[T], ref: R): A[T] = ???
  }

  abstract class SetILoc[A[_], T, R] {
    def loc(self: A[T], ref: R): A[T] = ???
  }
}

