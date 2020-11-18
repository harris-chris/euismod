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
import shapeless.ops.nat.{Diff => NatDiff}

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
    def ++[B[_]](a: A[T], b: B[T])(implicit 
      st: Stack[A, B, T],
    ): st.Out = st(a, b, 0)
    def stack[B[_]](a: A[T], b: B[T], dim: Int)(implicit 
      st: Stack[A, B, T],
    ): st.Out = st(a, b, dim)
    def toList(a: A[T]): List[S] = (for(i <- 0 to length(a) - 1) yield (getAtN(a, i))).toList
    def fromList(listS: List[S]): A[T] = 
      listS.reverse.foldLeft(getEmpty[T])((e, s) => cons(e, s))
    def ndims[SH <: HList](a: A[T])(implicit 
      sh: Shape[A, T] { type Out = SH }, 
      tl: ToList[SH, Int],
    ): Int = shape(a).toList[Int].length
    def shape(a: A[T])(implicit sh: Shape[A, T]): sh.Out = sh(a)
    def getArraysAsc(a: A[T])(implicit ga: GetArrsAsc[A, T, HNil]): ga.Out = ga(HNil)
    def getArraysDesc(a: A[T])(implicit ga: GetArrsDesc[A, T, HNil]): ga.Out = ga(HNil)
    def flatten(a: A[T])(implicit fl: Flatten[A, T]): List[T] = fl(a)
    def fromElems[GAOut <: HList, SH <: HList](a: A[T], listT: List[T], shape: SH)(implicit 
      ga: GetArrsAsc[A, T, HNil] { type Out = GAOut },
      fr: FromElems[T, GAOut, SH], 
    ): fr.Out = fr(listT, ga(HNil), shape)
    def fromElems[GAOut <: HList, SH <: HList](a: A[T], listT: List[T])(implicit 
      sh: Shape[A, T] { type Out = SH },
      ga: GetArrsAsc[A, T, HNil] { type Out = GAOut },
      fr: FromElems[T, GAOut, SH], 
    ): fr.Out = fr(listT, ga(HNil), sh(a))
    def reshape[GAOut <: HList, SH <: HList](a: A[T], shape: SH)(implicit 
      fl: Flatten[A, T],
      ga: GetArrsAsc[A, T, HNil] { type Out = GAOut },
      fr: FromElems[T, GAOut, SH],
    ): fr.Out = {
      fromElems(a, fl(a), shape)
    }
    def map[_T, GAOut <: HList, SH <: HList](a: A[T], f: (T) => _T)(implicit
      fl: Flatten[A, T],
      a_tIsArr: IsArray[A, _T],
      sh: Shape[A, T] { type Out = SH }, 
      ga: GetArrsAsc[A, _T, HNil] { type Out = GAOut },
      fr: FromElems[_T, GAOut, SH] { type Out = Option[A[_T]] },
    ): A[_T] = {
      val shape: SH = sh(a)
      val list_t: List[_T] = flatten(a).map(f)
      val empty_t: A[_T] = getEmpty[_T]
      val arrs: GAOut = ga(HNil)
      fr(list_t, arrs, shape).get 
    }
    def getReducedArraysForApply[R, O](a: A[T], r: R)(implicit
      gr: GetRdcArrs[A, T, R] { type Out = O }
    ): O = gr(a, r)
  }

  object IsArray {
    def apply[A[_], T, _S](
      implicit isArr: IsArray[A, T] { type S = _S },
    ): IsArray[A, T] { type S = _S } = isArr
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
      def ++[B[_]](b: B[T])(implicit st: Stack[A, B, T]) = tc.++(a, b)
      def stack[B[_]](b: B[T], dim: Int)(implicit st: Stack[A, B, T]) = tc.stack(a, b, dim)
      def length: Int = tc.length(a)
      def toList: List[_S] = tc.toList(a)
      def fromList(listS: List[_S]): A[T] = tc.fromList(listS)
      def getArraysAsc(implicit ga: GetArrsAsc[A, T, HNil]): ga.Out = tc.getArraysAsc(a)
      def getArraysDesc(implicit ga: GetArrsDesc[A, T, HNil]): ga.Out = tc.getArraysDesc(a)
      def shape(implicit sh: Shape[A, T]): sh.Out = tc.shape(a)
      def flatten(implicit fl: Flatten[A, T]): List[T] = fl(a)
      def fromElems[Arrs <: HList, SH <: HList](listT: List[T], shape: SH)(implicit 
        ga: GetArrsAsc[A, T, HNil] { type Out = Arrs },
        fr: FromElems[T, Arrs, SH],
      ): fr.Out = tc.fromElems(a, listT, shape)
      def fromElems[Arrs <: HList, SH <: HList](listT: List[T])(implicit 
        sh: Shape[A, T] { type Out = SH },
        ga: GetArrsAsc[A, T, HNil] { type Out = Arrs },
        fr: FromElems[T, Arrs, SH], 
      ): fr.Out = tc.fromElems(a, listT)
      def reshape[GAOut <: HList, SH <: HList](shape: SH)(implicit 
        fl: Flatten[A, T],
        ga: GetArrsAsc[A, T, HNil] { type Out = GAOut },
        rs: FromElems[T, GAOut, SH],
      ) = tc.reshape(a, shape)
      def map[_T, GAOut <: HList, SH <: HList](f: T => _T)(implicit
        a_tIsArr: IsArray[A, _T],
        fl: Flatten[A, T],
        sh: Shape[A, T] { type Out = SH }, 
        ga: GetArrsAsc[A, _T, HNil] { type Out = GAOut },
        fr: FromElems[_T, GAOut, SH] { type Out = Option[A[_T]] },
      ): A[_T] = tc.map(a, f)
    }
  }

  sealed trait Stack[A[_], B[_], T] {self =>
    type Out
    def apply(a: A[T], b: B[T], dim: Int): Out
  }
  object Stack {
    type Aux[A[_], B[_], T, O] = Stack[A, B, T] { type Out = O }
    def apply[A[_], B[_], T](implicit st: Stack[A, B, T]): Stack[A, B, T] = st
    def instance[A[_], B[_], T, O](f: (A[T], B[T], Int) => O): Aux[A, B, T, O] = new Stack[A, B, T] {
      type Out = O
      def apply(a: A[T], b: B[T], dim: Int): Out = f(a, b, dim)
    }
    implicit def ifArraysUnsizedDim1p[A[_], B[_], T, Arrs <: HList, SH <: HList]( implicit 
      ga: GetArrsAsc[A, T, HNil] { type Out = Arrs },
      aFl: Flatten[A, T],
      bFl: Flatten[B, T],
      aSh: Shape[A, T] { type Out = SH },
      bSh: Shape[B, T] { type Out = SH },
      cs: CombineShapes[SH, SH] { type Out = Option[SH] },
      fr: FromElems[T, Arrs, SH] { type Out = Option[A[T]] },
    ): Aux[A, B, T, Option[A[T]]] = instance((a, b, dim) => {
      println(s"A SHAPE ${aSh(a)}")
      println(s"B SHAPE ${bSh(b)}")
      val elems: List[T] = aFl(a) ++ bFl(b)
      println(s"ELEMS ${elems}")
      val newShape = cs(aSh(a), bSh(b), dim)
      println(s"SHAPE ${newShape}")
      newShape.flatMap(ns => {
        val out = fr(elems, ga(HNil), ns)
        println(s"OUT ${out}")
        out
      })
    })
  }

  sealed trait CombineShapes[A, B] {
    type Out
    def apply(a: A, b: B, dim: Int): Out
  }
  object CombineShapes {
    type Aux[A, B, O] = CombineShapes[A, B] { type Out = O }
    def instance[A, B, O](f: (A, B, Int) => O): Aux[A, B, O] = new CombineShapes[A, B] {
      type Out = O
      def apply(a: A, b: B, dim: Int): Out = f(a, b, dim)
    }
    def apply[A, B](implicit cs: CombineShapes[A, B]): CombineShapes[A, B] = cs
    implicit def ifMatchingHLists[SH <: HList](implicit 
      csrt: CombineShapesRT[SH]
    ): Aux[SH, SH, Option[SH]] = instance(
      (a, b, dim) => csrt(a, b, dim)
    )
  }
  
  sealed trait CombineShapesRT[SH <: HList] {
    type Out = Option[SH]
    def apply(a: SH, b: SH, dim: Int): Out
  }
  object CombineShapesRT {
    def instance[SH <: HList](f: (SH, SH, Int) => Option[SH]): CombineShapesRT[SH] = new CombineShapesRT[SH] {
      def apply(a: SH, b: SH, dim: Int): Out = f(a, b, dim)
    }
    implicit def ifHeadIsInt[Tl <: HList](implicit 
      cb: CombineShapesRT[Tl],
    ): CombineShapesRT[Int :: Tl] = instance((a, b, dim) => 
      if(dim == 0){
        val t1: Option[HList] = cb(a.tail, b.tail, dim - 1)
        cb(a.tail, b.tail, dim - 1).map(tl => (a.head + b.head) :: tl)
      } else {
        if(a.head == b.head) {cb(a.tail, b.tail, dim - 1).map(tl => a.head :: tl)} else { None }
      }
    )
    implicit val ifHNil: CombineShapesRT[HNil] = instance((a, b, isHead) => Some(HNil))
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
      A[_], T, Inp <: HList, Arrs <: HList, Filt <: HList, IntsN <: Nat, ArrsN <: Nat, TakeN <: Nat, RdArrs <: HList,
    ] (implicit 
      ga: GetArrsAsc[A, T, HNil] { type Out = Arrs },
      fl: Filter[Inp, Int] { type Out = Filt },
      lf: Length[Filt] { type Out = IntsN },
      la: Length[Arrs] { type Out = ArrsN },
      di: NatDiff[ArrsN, IntsN] { type Out = TakeN },
      dr: Take[Arrs, TakeN] { type Out = RdArrs },
      re: Reverse[RdArrs],
    ): Aux[A, T, Inp, re.Out] = instance((a: A[T], inp: Inp) => re(dr(ga(HNil))))
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
    def apply[A[_], T, L <: HList](implicit ga: GetArrsDesc[A, T, L]): GetArrsDesc[A, T, L] = ga
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
    def apply[A[_], T, L <: HList](implicit ga: GetArrsAsc[A, T, L]): GetArrsAsc[A, T, L] = ga
    implicit def ifLIsHList[A[_], T, L <: HList, Asc <: HList]( implicit 
      ga: GetArrsDesc[A, T, L] { type Out = Asc },
      rv: Reverse[Asc],
    ): Aux[A, T, L, rv.Out] = instance(l => rv(ga(l)))
  }

  sealed trait Shape[A[_], T] { self =>
    type Out
    def apply(a: A[T]): Out
  }
  object Shape {
    type Aux[A[_], T, O] = Shape[A, T] { type Out = O }
    def instance[A[_], T, O](f: A[T] => O): Aux[A, T, O] = new Shape[A, T] {
      type Out = O
      def apply(a: A[T]): Out = f(a)
    }
    def apply[A[_], T](implicit sh: Shape[A, T]): Shape[A, T] = sh
    implicit def ifFullyTyped[A[_], T](implicit ft: FullyTyped[A[T]]) = ???
    implicit def ifShapeRT[A[_], T](implicit sh: ShapeRT[A, T, HNil]): Aux[A, T, sh.Out] = 
      instance(a => sh(a, HNil))
  }

  sealed trait ShapeRT[A[_], T, L <: HList] {self =>
    type Out <: HList
    def apply(a: A[T], l: L): Out
  }
  object ShapeRT {
    type Aux[A[_], T, L <: HList, O <: HList] = ShapeRT[A, T, L] { type Out = O }
    def instance[A[_], T, L <: HList, O <: HList](f: (A[T], L) => O): Aux[A, T, L, O] = 
    new ShapeRT[A, T, L] { 
      type Out = O 
      def apply(a: A[T], l: L): Out = f(a, l)
    }
    implicit def gsIfSIsEle[A[_], T, _S, L <: HList, O <: HList](implicit 
      aIsABs: IsArrBase[A[T], T] { type S = T },
      rv: Reverse[Int :: L] { type Out = O },
    ): Aux[A, T, L, O] = instance((a, l) => rv(aIsABs.length(a) :: l))
    implicit def gsIfSIsArr[A[_], T, S0[_], L <: HList](implicit 
      aIsABs: IsArrBase[A[T], T] { type S = S0[T] },
      gsForS: ShapeRT[S0, T, Int :: L],
      ): Aux[A, T, L, gsForS.Out] = instance((a, l) => 
        gsForS(aIsABs.getAtN(a, 0), aIsABs.length(a) :: l)
      )
  }

  trait FromElems[T, Arrs <: HList, SH <: HList] {
    type Out
    def apply(l: List[T], arrs: Arrs, sh: SH): Out
  }
  object FromElems {
    type Aux[T, Arrs <: HList, SH <: HList, O] = FromElems[T, Arrs, SH] { type Out = O }
    def instance[T, Arrs <: HList, SH <: HList, O](f: (List[T], Arrs, SH) => O): Aux[T, Arrs, SH, O] = 
    new FromElems[T, Arrs, SH] {
      type Out = O
      def apply(l: List[T], arrs: Arrs, sh: SH): Out = f(l, arrs, sh)
    }
    implicit def ifListT[T, Arrs <: HList, SH <: HList, RSH <: HList, O]( implicit 
      rv: Reverse[SH] { type Out = RSH },
      fr: FromElemsRT[T, Arrs, RSH],
    ): Aux[T, Arrs, SH, fr.Out] = instance((l, arrs, sh) => fr(Some(l.reverse), arrs, rv(sh)))
  }

  trait FromElemsRT[T, Arrs <: HList, SH <: HList] {
    type Out 
    def apply(lO: Option[List[T]], arrs: Arrs, sh: SH): Out
  }
  object FromElemsRT {
    type Aux[T, Arrs <: HList, SH <: HList, O] = FromElemsRT[T, Arrs, SH] { type Out = O }
    def instance[T, Arrs <: HList, SH <: HList, O](
      f: (Option[List[T]], Arrs, SH) => O
    ): Aux[T, Arrs, SH, O] = new FromElemsRT[T, Arrs, SH] {
      type Out = O
      def apply(lO: Option[List[T]], arrs: Arrs, sh: SH): Out = f(lO, arrs, sh)
    }
    implicit def ifSingleElemRemainingInShape[T, H0, H1, H2p <: HList](implicit 
      hIsABs: IsArrBase[H1, T] { type S = H0 },
    ): Aux[H0, H1 :: H2p, Int :: HNil, Option[H1]] = instance((lO, arrs, sh) => {
      lO.flatMap( 
        ls => createArrs[H1, T, H0](arrs.head, Nil, ls, sh.head)
      ).flatMap(arrs => if(arrs.length == 1){Some(arrs(0))} else {None})
    })
    implicit def ifMultipleElemsRemainingInShape[T, H0, H1, H2p <: HList, SH2p <: HList](implicit 
      hIsABs: IsArrBase[H1, T] { type S = H0 },
      rsForNxt: FromElemsRT[H1, H2p, Int :: SH2p],   
    ): Aux[H0, H1 :: H2p, Int :: Int :: SH2p, rsForNxt.Out] = instance((lO, arrs, sh) => {
      val thisA: H1 = arrs.head
      val h1Nil = Nil: List[H1]
      val thisArrs: Option[List[H1]] = lO flatMap { ls => createArrs[H1, T, H0](thisA, h1Nil, ls, sh.head) }
      rsForNxt(thisArrs, arrs.tail, sh.tail)
    })
    def createArrs[A, T, _S](
      aEmpty: A, as: List[A], l: List[_S], width: Int,
    )(implicit aIsABs: IsArrBase[A, T] { type S = _S }): Option[List[A]] = l.length match {
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
    type Out = List[T]
    def apply(a: A[T]): List[T]
  }
  object Flatten {
    def apply[A[_], T](implicit fl: Flatten[A, T]): Flatten[A, T] = fl
    def instance[A[_], T](f: A[T] => List[T]): Flatten[A, T] { type Out = List[T] } = 
    new Flatten[A, T] {
      override type Out = List[T]
      def apply(a: A[T]): List[T] = f(a)
    }
    implicit def flattenIfSIsT[A[_], T](implicit 
      aIsArr: IsArray[A, T] { type S = T },
    ): Flatten[A, T] = instance(a => aIsArr.toList(a))
    implicit def flattenIfSIsNotT[A[_], T, _S[T]](implicit 
      aIsArr: IsArray[A, T] { type S = _S[T] },
      sIsArr: IsArray[_S, T], 
      sFl: Flatten[_S, T],
    ): Flatten[A, T] = instance(a => aIsArr.toList(a).map(sIsArr.flatten(_)).flatten) 
  }

  abstract class FullyTyped[A] {}

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

