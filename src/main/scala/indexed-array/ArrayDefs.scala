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
import shapeless.ops.nat.{GT, GTEq, Pred, Diff => NatDiff, ToInt}
import scala.util.{Try, Success, Failure}

object ArrayDefs {

  trait Slices
  trait AllSlice extends Slices
  case object allSlice extends AllSlice

  case class Element[T] (
    get: T
  ) extends IsBase[Element[T]]

  @implicitNotFound(f"Cannot find IsArray implicit")
  abstract class IsArray[A[_], T] extends IsBase[A[T]] { self =>
    type S
    type Elem = T

    def getEmpty[_T]: A[_T] 
    def getAtN(a: A[T], n: Int): S
    def length(a: A[T]): Int
    def cons(a: A[T], sub: S): A[T]

    def apply[R](a: A[T], r: R)(implicit ai: ApplyIndex[A[T], R]): ai.Out = ai(a, r)
    def empty: A[T] = getEmpty[T]
    def ::(a: A[T], o: S): A[T] = cons(a, o)  
    def ++[B[_]](a: A[T], b: B[T])(implicit 
      cnCt: Concatenate[A, B, T, Nat._0],
    ): cnCt.Out = cnCt(a, b)
    def toList(a: A[T]): List[S] = (for(i <- 0 to length(a) - 1) yield (getAtN(a, i))).toList
    def fromList(listS: List[S]): A[T] = 
      listS.reverse.foldLeft(getEmpty[T])((e, s) => cons(e, s))
    def ndims[SH <: HList](a: A[T])(implicit 
      sh: Shape[A[T]] { type Out = SH }, 
      tl: ToList[SH, Int],
    ): Int = shape(a).toList[Int].length
    def shape(a: A[T])(implicit sh: Shape[A[T]]): sh.Out = sh(a)
    def flatten(a: A[T])(implicit fl: Flatten[A, T]): List[T] = fl(a)
    def fromElems[AR <: HList, SH <: HList](a: A[T], listT: List[T], shape: SH)(implicit 
      sa: SubArrays.Aux[A[T], AR],
      fe: FromElemsOpt[T, AR, SH], 
    ): fe.Out = fe(listT, shape)
    def fromElems[AR <: HList, SH <: HList](a: A[T], listT: List[T])(implicit 
      sh: Shape.Aux[A[T], SH],
      sa: SubArrays.Aux[A[T], AR],
      fe: FromElemsOpt[T, AR, SH], 
    ): fe.Out = fe(listT, sh(a))
    def reshape[AR <: HList, SH <: HList](a: A[T], shape: SH)(implicit 
      fl: Flatten[A, T],
      sa: SubArrays.Aux[A[T], AR],
      fe: FromElemsOpt[T, AR, SH],
    ): fe.Out = {
      fromElems(a, fl(a), shape)
    }
    def map[_T, AR <: HList, SH <: HList](a: A[T], f: (T) => _T)(implicit
      fl: Flatten[A, T],
      sh: Shape.Aux[A[T], SH], 
      sa: SubArrays.Aux[A[_T], AR],
      fr: FromElemsOpt.Aux[_T, AR, SH, Option[A[_T]]],
    ): A[_T] = {
      val shape: SH = sh(a)
      val list_t: List[_T] = flatten(a).map(f)
      val empty_t: A[_T] = getEmpty[_T]
      fr(list_t, shape).get 
    }
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
      def apply[R](r: R)(implicit ai: ApplyIndex[A[T], R]) = tc.apply(a, r)
      def ::(other: _S) = tc.cons(a, other)
      def ++[B[_]](b: B[T])(implicit cn: Concatenate[A, B, T, Nat._0]) = tc.++(a, b)
      def length: Int = tc.length(a)
      def toList: List[_S] = tc.toList(a)
      def fromList(listS: List[_S]): A[T] = tc.fromList(listS)
      def shape(implicit sh: Shape[A[T]]): sh.Out = tc.shape(a)
      def flatten(implicit fl: Flatten[A, T]): List[T] = fl(a)
      def fromElems[AR <: HList, SH <: HList](listT: List[T], shape: SH)(implicit 
        ga: SubArrays.Aux[A[T], AR],
        fr: FromElemsOpt[T, AR, SH],
      ): fr.Out = tc.fromElems(a, listT, shape)
      def fromElems[AR <: HList, SH <: HList](listT: List[T])(implicit 
        sh: Shape[A[T]] { type Out = SH },
        ga: SubArrays.Aux[A[T], AR],
        fr: FromElemsOpt[T, AR, SH], 
      ): fr.Out = tc.fromElems(a, listT)
      def reshape[AR <: HList, SH <: HList](shape: SH)(implicit 
        fl: Flatten[A, T],
        ga: SubArrays.Aux[A[T], AR],
        rs: FromElemsOpt[T, AR, SH],
      ) = tc.reshape(a, shape)
      def map[_T, AR <: HList, SH <: HList](f: T => _T)(implicit
        ai: IsArray[A, _T],
        fl: Flatten[A, T],
        sh: Shape.Aux[A[T], SH], 
        ga: SubArrays.Aux[A[_T], AR],
        fr: FromElemsOpt.Aux[_T, AR, SH, Option[A[_T]]],
      ): A[_T] = tc.map(a, f)
    }
  }

  trait ExpandDimsFromSubArrays[A, AR <: HList, N <: Nat] {
    type Out
    def apply(a: A): Out
  }
  object ExpandDimsFromSubArrays {
    type Aux[A, AR <: HList, N <: Nat, O] = ExpandDimsFromSubArrays[A, AR, N] { type Out = O }
    def apply[A, AR <: HList, N <: Nat] (implicit 
      ed: ExpandDimsFromSubArrays[A, AR, N],
    ): Aux[A, AR, N, ed.Out] = ed 
    def instance[A, AR <: HList, N <: Nat, O](f: A => O): Aux[A, AR, N, O] = 
    new ExpandDimsFromSubArrays[A, AR, N] {
      type Out = O
      def apply(a: A): Out = f(a)
    }

    implicit def ifDepthMatches[A, A0, A1p <: HList, N <: Nat, D0 <: Nat, D1 <: Nat] (implicit
      in: ExpandDims[A, A0, N]
    ): Aux[A, A0 :: A1p, N, in.Out] = instance(a => in(a))

    implicit def ifDepthDoesntMatch[A, A0, A1p <: HList, N <: Nat, D0 <: Nat, D1 <: Nat] (implicit
      d0: Depth.Aux[A, D0],
      d1: Depth.Aux[A0, D1],
      gt: GT[D1, Succ[D0]],
      ed: ExpandDimsFromSubArrays[A, A1p, N]
    ): Aux[A, A0 :: A1p, N, ed.Out] = instance(a => ed(a))
  }

  trait ExpandDims[A, _A, N] {
    type Out
    def apply(a: A): Out
  }
  object ExpandDims {
    type Aux[A, _A , N, O] = ExpandDims[A, _A, N] { type Out = O }
    def apply[A, _A, N] (implicit 
      ed: ExpandDims[A, _A, N],
    ): Aux[A, _A, N, ed.Out] = ed 
    def instance[A, _A, N, O](f: A => O): Aux[A, _A, N, O] = new ExpandDims[A, _A, N] {
      type Out = O
      def apply(a: A): Out = f(a)
    }

    implicit def ifDepthmatches[
      A[_], _A[_], N <: Nat, T, D0 <: Nat, D1 <: Nat, AR <: HList, SH <: HList, _SH <: HList,
      SHP <: HList, SHS <: HList,
    ] (implicit
      d0: Depth.Aux[A[T], D0],
      d1: Depth.Aux[_A[T], D1],
      e0: Succ[D0] =:= D1,
      sa: SubArrays.Aux[_A[T], AR],
      sh: Shape.Aux[A[T], SH],
      sp: Split.Aux[SH, N, SHP, SHS],
      fl: Flatten[A, T],
      pr: Prepend.Aux[SHP, Int :: SHS, _SH],
      fe: FromElemsOpt.Aux[T, AR, _SH, Option[_A[T]]], 
    ): Aux[A[T], _A[T], N, _A[T]] = instance(a => {
      val origSh = sh(a)
      val (pre, suf) = sp(origSh)
      val newSh = pre ++ (1 :: suf)
      fe(fl(a), newSh).get
    })

    implicit def ifHList[
      A, _A, NS <: HList, SA <: HList, L0 <: Nat, L1 <: Nat, TK <: Nat, ARD <: HList, ARA <: HList
    ] (implicit
      sa: SubArrays.Aux[_A, SA],
      l0: Depth.Aux[A, L0],
      l1: Depth.Aux[_A, L1],
      tk: NatDiff.Aux[L1, L0, TK],
      ad: Take.Aux[SA, TK, ARD], 
      rv: Reverse.Aux[ARD, ARA],
      ed: ExpandDimsHList[A, ARA, NS],
    ): Aux[A, _A, NS, ed.Out] = instance(a => ed(a))

  }

  trait ExpandDimsHList[A, AR <: HList, NS <: HList] {
    type Out
    def apply(a: A): Out
  }
    
  object ExpandDimsHList {
    type Aux[A, AR <: HList, NS <: HList, O] = ExpandDimsHList[A, AR, NS] { type Out = O }
    def apply[A, AR <: HList, NS <: HList] (implicit 
      ed: ExpandDimsHList[A, AR, NS],
    ): Aux[A, AR, NS, ed.Out] = ed 
    def instance[A, AR <: HList, NS <: HList, O](f: A => O): Aux[A, AR, NS, O] = 
    new ExpandDimsHList[A, AR, NS] {
      type Out = O
      def apply(a: A): O = f(a)
    }

    implicit def ifHList[A, A0, A1p <: HList, N0 <: Nat, N1p <: HList, DA <: Nat, DR <: Nat, _A] (implicit
      da: Depth.Aux[A, DA],
      dr: Depth.Aux[A0, DR],
      e0: Succ[DA] =:= DR,
      ed: ExpandDims.Aux[A, A0, N0, _A],
      nx: ExpandDimsHList[_A, A1p, N1p],
    ): Aux[A, A0 :: A1p, N0 :: N1p, nx.Out] = instance(a => nx(ed(a)))
  }

  trait BroadcastShapesOpt[SHA <: HList, SHB <: HList] {
    type Out <: Option[_]
    def apply(a: SHA, b: SHB): Out
  }
  object BroadcastShapesOpt {
    type Aux[SHA <: HList, SHB <: HList, O] = BroadcastShapesOpt[SHA, SHB] { type Out = O }
    def instance[SHA <: HList, SHB <: HList, O <: Option[_]] (
      f: (SHA, SHB) => O,
    ): Aux[SHA, SHB, O] = new BroadcastShapesOpt[SHA, SHB] {
      type Out = O
      def apply(a: SHA, b: SHB): Out = f(a, b)
    }
    def apply[SHA <: HList, SHB <: HList](
      implicit br: BroadcastShapesOpt[SHA, SHB],
    ): Aux[SHA, SHB, br.Out] = br

    implicit def ifSHBGtSHA[SHA <: HList, SHB <: HList, LA <: Nat, LB <: Nat] (implicit
      l0: Length.Aux[SHA, LA],
      l1: Length.Aux[SHB, LB],
      e0: GT[LB, LA],
      nx: BroadcastShapesOpt[SHB, SHA],
    ): Aux[SHA, SHB, nx.Out] = instance((a, b) => nx(b, a))

    implicit def ifSH0GtEqSH0[SHA <: HList, SHB <: HList, LA <: Nat, LB <: Nat] (implicit
      l0: Length.Aux[SHA, LA],
      l1: Length.Aux[SHB, LB],
      e0: GTEq[LA, LB],
      al: ToList[SHA, Int],
      bl: ToList[SHB, Int],
    ): Aux[SHA, SHB, Option[SHA]] = instance((a, b) => {
      def go(a: List[Int], b: List[Int]): Boolean = (a.headOption, b.headOption) match {
        case (Some(ah), Some(bh)) if ah == bh => go(a.tail, b.tail)
        case (Some(ah), Some(bh)) if ah != bh => go(a.tail, b)
        case (Some(ah), None) => true
        case (None, None) => true
        case (None, Some(bh)) => false
      }
      if(go(al(a), bl(b))) { Some(a) } else { None }
    })
  }

  trait OperateOpt[A[_], B[_], AT, BT] {
    type Out = Option[A[AT]]
    def apply(a: A[AT], b: B[BT], op: (AT, BT) => AT): Out 
  }
  object OperateOpt {
    type Aux[A[_], B[_], AT, BT] = OperateOpt[A, B, AT, BT]
    def instance[A[_], B[_], AT, BT] (
      f: (A[AT], B[BT], (AT, BT) => AT) => Option[A[AT]],
    ): Aux[A, B, AT, BT] = new OperateOpt[A, B, AT, BT] {
      def apply(a: A[AT], b: B[BT], op: (AT, BT) => AT): Option[A[AT]] = f(a, b, op)
    }
    def apply[A[_], B[_], AT, BT](implicit oo: OperateOpt[A, B, AT, BT]): Aux[A, B, AT, BT] = oo

    implicit def ifSameDepthArrs[A[_], B[_], AT, BT, SA[_], SB[_], DA <: Nat, DB <: Nat] (implicit
      ar: IsArray[A, AT] { type S = SA[AT] }, br: IsArray[B, BT] { type S = SB[BT] },
      da: Depth.Aux[A[AT], DA],
      db: Depth.Aux[B[BT], DB],
      e0: DA =:= DB,
      nx: OperateOpt.Aux[SA, SB, AT, BT],
    ): Aux[A, B, AT, BT] = instance(
      (a, b, op) => (ar.toList(a), br.toList(b)) match {
        case (lstA, lstB) if lstA.length == lstB.length => {
          val lst: List[Option[SA[AT]]] = for(
            (nxa, nxb) <- lstA.zip(lstB)
          ) yield (nx(nxa, nxb, op))
          val flatLst = lst.flatten
          if(flatLst.length == lstA.length) {Some(ar.fromList(flatLst))} else {None}
        }
        case (lstA, lstB) if lstB.length == 1 => {
          val lst: List[Option[SA[AT]]] = lstA.map(nx(_, lstB(0), op)) 
          val flatLst = lst.flatten
          if(flatLst.length == lstA.length) {Some(ar.fromList(flatLst))} else {None}
        }
        case (lstA, lstB) if lstA.length == 1 => {
          val lst: List[Option[SA[AT]]] = lstB.map(nx(lstA(0), _, op)) 
          val flatLst = lst.flatten
          if(flatLst.length == lstB.length) {Some(ar.fromList(flatLst))} else {None}
        }
        case _ => None
      }
    )

    implicit def ifBothBase[A[_], B[_], AT, BT] (implicit
      ar: IsArray[A, AT] { type S = AT },
      br: IsArray[B, BT] { type S = BT },
    ): Aux[A, B, AT, BT] = instance(
      (a, b, op) => (ar.toList(a), br.toList(b)) match {
        case (lstA, lstB) if lstA.length == lstB.length => {
          val lst: List[AT] = for(
            (at, bt) <- lstA.zip(lstB)
          ) yield (op(at, bt))
          Some(ar.fromList(lst))
        }
        case (lstA, lstB) if lstB.length == 1 => {
          val lst: List[AT] = lstA.map(op(_, lstB(0)))
          Some(ar.fromList(lst))
        }
        case (lstA, lstB) if lstA.length == 1 => {
          val lst: List[AT] = lstB.map(op(lstA(0), _))
          Some(ar.fromList(lst))
        }
        case _ => None
      }
    )
  }

  type ArraySortedBy
  type Ascending <: ArraySortedBy
  type Descending <: ArraySortedBy

  trait ArraySort[AR <: HList] {
    type Out <: ArraySortedBy
  }
  object ArraySort {
    type Aux[AR <: HList, O <: ArraySortedBy] = ArraySort[AR] { type Out = O }
    def apply[AR <: HList, O <: ArraySortedBy](
      implicit dc: ArraySort[AR] { type Out = O }
    ): Aux[AR, O] = dc

    implicit def ifTwoPlusElemsRemainingDesc[A0[_], A1[_], T, A2p <: HList, DE0 <: Nat, DE1 <: Nat] (implicit
      d0: Depth.Aux[A0[T], DE0],
      d1: Depth.Aux[A1[T], DE1],
      gt: GT[DE0, DE1],
      nx: ArraySort.Aux[A1[T] :: A2p, Descending],
    ): Aux[A0[T] :: A1[T] :: A2p, Descending] = 
      new ArraySort[A0[T] :: A1[T] :: A2p] { type Out = Descending }

    implicit def ifTwoElemsRemainingDesc[A0[_], A1[_], T, DE0 <: Nat, DE1 <: Nat] (implicit
      d0: Depth.Aux[A0[T], DE0],
      d1: Depth.Aux[A1[T], DE1],
      gt: GT[DE0, DE1],
    ): Aux[A0[T] :: A1[T] :: HNil, Descending] = 
      new ArraySort[A0[T] :: A1[T] :: HNil] { type Out = Descending }

    implicit def ifTwoPlusElemsRemainingAsc[A0[_], A1[_], T, A2p <: HList, DE0 <: Nat, DE1 <: Nat] (implicit
      d0: Depth.Aux[A0[T], DE0],
      d1: Depth.Aux[A1[T], DE1],
      gt: GT[DE1, DE0],
      nx: ArraySort.Aux[A1[T] :: A2p, Ascending],
    ): Aux[A0[T] :: A1[T] :: A2p, Ascending] = 
      new ArraySort[A0[T] :: A1[T] :: A2p] { type Out = Ascending }

    implicit def ifTwoElemsRemainingAsc[A0[_], A1[_], T, DE0 <: Nat, DE1 <: Nat] (implicit
      d0: Depth.Aux[A0[T], DE0],
      d1: Depth.Aux[A1[T], DE1],
      gt: GT[DE1, DE0],
    ): Aux[A0[T] :: A1[T] :: HNil, Ascending] = 
      new ArraySort[A0[T] :: A1[T] :: HNil] { type Out = Ascending }

    implicit def ifSingleElemList[A0[_], T, O <: ArraySortedBy]: Aux[A0[T] :: HNil, O] = 
      new ArraySort[A0[T] :: HNil] { type Out = O }
    
  }

  trait Reduce[A[_], T, DM <: Nat] {
    type Out
    def apply(a: A[T], combine: List[T] => T): Out
  }
  object Reduce {
    type Aux[A[_], T, DM <: Nat, O] = Reduce[A, T, DM] { type Out = O }
    def instance[A[_], T, DM <: Nat, O](f: (A[T], List[T] => T) => O): Aux[A, T, DM, O] = 
    new Reduce[A, T, DM] {
      type Out = O
      def apply(a: A[T], combine: List[T] => T): Out = f(a, combine)
    }
    def apply[A[_], T, DM <: Nat](implicit se: Reduce[A, T, DM]): Aux[A, T, DM, se.Out] = se

    implicit def ifDeGt2[
      A[_], T, DE <: Nat, DM <: Nat, FSH <: HList, FLF <: HList, LF <: HList, RG <: HList, 
      AR <: HList, SH <: HList, Out
    ] (implicit 
      ar: IsArray[A, T],
      de: Depth.Aux[A[T], DE],
      e0: GT[DE, Nat._1],
      rd: ReduceToList[A, T, DM],
      sh: Shape.Aux[A[T], FSH],
      sp: Split.Aux[FSH, Succ[DM], FLF, RG],
      in: Init.Aux[FLF, LF],
      sa: SubArrays.Aux[A[T], AR],
      pr: Prepend.Aux[LF, RG, SH],
      fe: FromElemsOpt.Aux[T, AR, SH, Option[Out]], 
      dm: ToInt[DM],
    ): Aux[A, T, DM, Out] = instance((a, cmb) => {
      val lst: List[T] = rd(a, cmb)
      println(s"LIST ${lst}")
      val dim = dm()
      val (lf, rg) = sp(sh(a))
      val shape: SH = in(lf) ++ rg
      val arrO = fe(lst, shape)
      arrO.get
    })
  }

  trait ReduceToList[A[_], T, DM <: Nat] {
    type Out = List[T]
    def apply(a: A[T], combine: List[T] => T): Out
  }
  object ReduceToList {
    type Aux[A[_], T, DM <: Nat] = ReduceToList[A, T, DM]
    def instance[A[_], T, DM <: Nat](f: (A[T], List[T] => T) => List[T]): Aux[A, T, DM] = 
    new ReduceToList[A, T, DM] {
      def apply(a: A[T], combine: List[T] => T): Out = f(a, combine)
    }
    def apply[A[_], T, DM <: Nat](implicit se: Aux[A, T, DM]): Aux[A, T, DM] = se

    implicit def ifDMIs0AndSIs2dPlus[A[_], T, _S[_]] (implicit 
      ar: IsArray[A, T] { type S = _S[T] },
      fl: Flatten[_S, T],
    ): Aux[A, T, Nat._0] = instance((a, cmb) => {
      val lst2d: List[List[T]] = ar.toList(a).map(fl(_))
      lst2d.transpose.map(cmb(_))
    })
    
    implicit def ifDMIs0AndSIs1d[A[_], T] (implicit 
      ar: IsArray[A, T] { type S = T },
    ): Aux[A, T, Nat._0] = instance((a, cmb) => {
      List(cmb(ar.toList(a)))
    })

    implicit def ifDMGt0[A[_], T, _S[_], DM <: Nat, DMm1 <: Nat](implicit 
      ar: IsArray[A, T] { type S = _S[T] },
      e1: GT[DM, Nat._0],
      pr: Pred.Aux[DM, DMm1],
      rd: ReduceToList[_S, T, DMm1],
    ): Aux[A, T, DM] = instance((a, cmb) => {
      val lst: List[List[T]] = ar.toList(a).map(rd(_, cmb))
      lst.foldLeft(Nil: List[T])(_ ++ _)
    })
  }

  sealed trait SetElem[A[_], T, R <: HList] {
    type Out = A[T]
    def apply(a: A[T], ref: R, elem: T): Out
  }
  object SetElem {
    type Aux[A[_], T, R <: HList] = SetElem[A, T, R] { type Out = A[T] }
    def instance[A[_], T, R <: HList](f: (A[T], R, T) => A[T]): Aux[A, T, R] = new SetElem[A, T, R] {
      def apply(a: A[T], ref: R, elem: T): A[T] = f(a, ref, elem)
    }
    def apply[A[_], T, R <: HList](implicit se: SetElem[A, T, R]): Aux[A, T, R] = se

    implicit def ifHListIntIsBase[A[_], T](implicit
      ia: IsArray[A, T] { type S = T },
    ): Aux[A, T, Int :: HNil] = instance(
      (a, ref, elem) => ia.fromList(ia.toList(a).updated(ref.head, elem))
    )

    implicit def ifHListIntIsArr[A[_], T, _S[_], R1p <: HList](implicit
      ia: IsArray[A, T] { type S = _S[T] },
      iaS: SetElem[_S, T, R1p],
    ): Aux[A, T, Int :: R1p] = instance((a, ref, elem) => {
      val newS = iaS.apply(ia.getAtN(a, ref.head), ref.tail, elem)
      ia.fromList(ia.toList(a).updated(ref.head, newS))
    })
  }

  trait ApplyIndex[A, IDX] {
    type Out
    def apply(a: A, idx: IDX): Out
  }
  object ApplyIndex {
    type Aux[A, IDX, O] = ApplyIndex[A, IDX] { type Out = O }
    def instance[A, IDX, O](
      f: (A, IDX) => O,
    ): Aux[A, IDX, O] = new ApplyIndex[A, IDX] {
      type Out = O
      def apply(a: A, idx: IDX): O = f(a, idx)
    }
    def apply[A, IDX](implicit ai: ApplyIndex[A, IDX]): Aux[A, IDX, ai.Out] = ai

    implicit def ifIdxIsBooleanArray[A[_], T](implicit
      aSh: Shape[A[T]],
      mSh: Shape[A[Boolean]],
      aFl: Flatten[A, T] { type Out = List[T] },
      mFl: Flatten[A, Boolean] { type Out = List[Boolean] },
    ): Aux[A[T], A[Boolean], Option[List[T]]] = instance((a, r) =>
      if(aSh(a) == mSh(r)) { 
        val lstT: List[T] = aFl(a)
        val lstBl: List[Boolean] = mFl(r)
        Some(lstT.zip(lstBl).flatMap{ case(t, bl) => Option.when(bl)(t) })
      } else {None}
    )
    
    implicit def ifIdxIsInt[A[_], T, _S](implicit
      ar: IsArray[A, T] { type S = _S }
    ): Aux[A[T], Int, _S] = instance((a, r) => ar.getAtN(a, r))

    implicit def ifIdxIsListInt[A[_], T](implicit
      ar: IsArray[A, T],
    ): Aux[A[T], List[Int], A[T]] = instance((a, rs) => ar.fromList(
      rs.map(ar.getAtN(a, _)))
    )

    implicit def ifIdxIsHList[
      A[_], T, ARD <: HList, AllAR <: HList, Idx <: HList, IntsIdx <: HList, IntsN <: Nat, AllARN <: Nat,
      TakeN <: Nat, RdAR <: HList, RevRdAR <: HList,
    ](implicit 
      sa: SubArrays.Aux[A[T], ARD],
      r0: Reverse.Aux[ARD, AllAR],
      fl: Filter.Aux[Idx, Int, IntsIdx],
      lf: Length.Aux[IntsIdx, IntsN],
      la: Length.Aux[AllAR, AllARN],
      di: NatDiff.Aux[AllARN, IntsN, TakeN],
      dr: Take.Aux[AllAR, TakeN, RdAR],
      re: Reverse.Aux[RdAR, RevRdAR],
      gi: ApplyIndexDT[A[T], Idx, RevRdAR],
    ): Aux[A[T], Idx, gi.Out] = instance((a, r) => gi(a, r)) 
  }

  sealed trait PrettyPrint[A] {
    type Out = String
    def apply(a: A, indO: Option[String] = None): Out
  }
  object PrettyPrint {
    def instance[A](
      f: (A, Option[String]) => String,
    ): PrettyPrint[A] = new PrettyPrint[A] {
      def apply(a: A, indO: Option[String]): String = f(a, indO)
    }
    def apply[A](implicit pp: PrettyPrint[A]): PrettyPrint[A] = pp

    def maxWidth[A[_], T](a: A[T])(implicit 
      ar: IsArray[A, T],
      fl: Flatten[A, T],
    ): Int = 
      ar.flatten(a).map(_.toString.length).max

    implicit def ifIs1d[A[_], T](implicit 
      ai: IsArray[A, T],
      de: Depth.Aux[A[T], Nat._1],
      fl: Flatten[A, T],
    ): PrettyPrint[A[T]] = instance((a, indO) => {
      val mW = maxWidth(a)
      "[" ++ ai.toList(a).map(_.toString.padTo(mW, ' ')).mkString(", ") ++ "]"
    })
    implicit def ifIs1dp[A[_], T, _S[_], DE <: Nat](implicit 
      ar: IsArray[A, T] { type S = _S[T] },
      de: Depth.Aux[A[T], DE],
      deIsGt2: GT[DE, Nat._1],
      toInt: ToInt[DE],
      fl: Flatten[A, T],
      pp: PrettyPrint[_S[T]],
    ): PrettyPrint[A[T]] = instance((a, indO) => {
      val ind = indO.getOrElse(" ")
      val nextInd = ind ++ " "
      val lineB = "," ++ "\n" * (toInt()-1) ++ ind
      val ls: List[_S[T]] = ar.toList(a)
      "[" ++ pp(ls.head, Some(nextInd)) ++ lineB ++ ls.tail.map(pp(_, Some(nextInd))).mkString(lineB) ++ "]"
    })
  }
  
  sealed trait CombineShapesOpt[SH <: HList] {
    type Out = Option[SH]
    def apply(a: SH, b: SH, dim: Int = 0): Out
  }
  object CombineShapesOpt {
    type Aux[SH <: HList] = CombineShapesOpt[SH] { type Out = Option[SH] }
    def instance[SH <: HList](f: (SH, SH, Int) => Option[SH]): CombineShapesOpt[SH] = 
    new CombineShapesOpt[SH] {
      def apply(a: SH, b: SH, dim: Int): Out = f(a, b, dim)
    }
    implicit def ifHeadIsInt[Tl <: HList](implicit 
      cb: CombineShapesOpt[Tl],
    ): CombineShapesOpt[Int :: Tl] = instance((a, b, dim) => 
      if(dim == 0){
        val t1: Option[HList] = cb(a.tail, b.tail, dim - 1)
        cb(a.tail, b.tail, dim - 1).map(tl => (a.head + b.head) :: tl)
      } else {
        if(a.head == b.head) {cb(a.tail, b.tail, dim - 1).map(tl => a.head :: tl)} else { None }
      }
    )
    implicit val ifHNil: CombineShapesOpt[HNil] = instance((a, b, isHead) => Some(HNil))
  }

  trait SubArrays[A] {self => 
    type Out <: HList
  }
  object SubArrays {
    type Aux[A, O <: HList] = SubArrays[A] { type Out = O }
    def apply[A](implicit sa: SubArrays[A]): Aux[A, sa.Out] = sa

    implicit def go[A] (implicit
      ga: GetARDesc[A, HNil],
    ): Aux[A, ga.Out] = new SubArrays[A] { type Out = ga.Out }

    trait GetARDesc[A, L] {self =>
      type Out <: HList
    }
    object GetARDesc {
      type Aux[A, L, O <: HList] = GetARDesc[A, L] { type Out = O }
      def apply[A, L](implicit ga: GetARDesc[A, L]): Aux[A, L, ga.Out] = ga

      implicit def ifSIsEle[A[_], T, L <: HList](implicit 
        ar: IsArray[A, T] { type S = T },
        rv: Reverse[A[T] :: L],
      ): Aux[A[T], L, rv.Out] = new GetARDesc[A[T], L] { type Out = rv.Out }

      implicit def ifSIsArr[A[_], T, _S[_], _S1, L <: HList](implicit 
        aIsABs: IsArray[A, T] { type S = _S[T] },
        sIsABs: IsArray[_S, T],
        gaForS: GetARDesc[_S[T], A[T] :: L],
      ): Aux[A[T], L, gaForS.Out] = new GetARDesc[A[T], L] { type Out = gaForS.Out }
    }
  }


  sealed trait Depth[A] { self =>
    type Out <: Nat
  }
  object Depth {
    type Aux[A, O <: Nat] = Depth[A] { type Out = O }
    def apply[A](implicit de: Depth[A]): Aux[A, de.Out] = de
    implicit def ifArr[A[_], T, O <: Nat, AR <: HList](implicit 
      sa: SubArrays.Aux[A[T], AR],
      le: Length.Aux[AR, O],
    ): Aux[A[T], O] = new Depth[A[T]] { type Out = O }
  }

  trait Shape[A] { self =>
    type Out <: HList
    def apply(a: A): Out
    def toList(a: A)(implicit tl: ToList[Out, Int]): List[Int] = tl(apply(a))
  }
  object Shape {
    type Aux[A, O] = Shape[A] { type Out = O }
    def instance[A, O <: HList](f: A => O): Aux[A, O] = new Shape[A] {
      type Out = O
      def apply(a: A): Out = f(a)
    }
    def apply[A](implicit sh: Shape[A]): Aux[A, sh.Out] = sh
    
    implicit def ifHList[A, L <: HList](implicit 
      sr: ShapeRecur[A, HNil],
    ): Aux[A, sr.Out] = instance(a => sr(a, HNil))
    
    trait ShapeRecur[A, L <: HList] {self =>
      type Out <: HList
      def apply(a: A, l: L): Out
    }
    object ShapeRecur {
      type RecurAux[A, L <: HList, O <: HList] = ShapeRecur[A, L] { type Out = O }
      def recur[A, L <: HList, O <: HList](f: (A, L) => O): RecurAux[A, L, O] = 
      new ShapeRecur[A, L] { 
        type Out = O 
        def apply(a: A, l: L): Out = f(a, l)
      }

      implicit def gsIfSIsEle[A[_], T, _S, L <: HList, O <: HList](implicit 
        ai: IsArray[A, T] { type S = T },
        rv: Reverse[Int :: L] { type Out = O },
      ): RecurAux[A[T], L, O] = recur((a, l) => rv(ai.length(a) :: l))

      implicit def gsIfSIsArr[A[_], T, S0[_], L <: HList](implicit 
        ai: IsArray[A, T] { type S = S0[T] },
        gsForS: ShapeRecur[S0[T], Int :: L],
      ): RecurAux[A[T], L, gsForS.Out] = recur((a, l) => 
        gsForS.apply(ai.getAtN(a, 0), ai.length(a) :: l)
      )
    }
  }

  trait FromElemsOpt[X, AR <: HList, SH <: HList] {
    type Out <: Option[Any]
    def apply(l: List[X], sh: SH): Out
  }
  object FromElemsOpt {
    type Aux[X, AR <: HList, SH <: HList, O <: Option[_]] = 
      FromElemsOpt[X, AR, SH] { type Out = O }
    def instance[T, AR <: HList, SH <: HList, INIT <: Nat, O <: Option[_]](
      f: (List[T], SH) => O
    ): Aux[T, AR, SH, O] = new FromElemsOpt[T, AR, SH] {
      type Out = O
      def apply(l: List[T], sh: SH): Out = f(l, sh)
    }
    def apply[T, AR <: HList, SH <: HList](
      implicit fe: FromElemsOpt[T, AR, SH],
    ): FromElemsOpt.Aux[T, AR, SH, fe.Out] = fe

    implicit def ifShapeIsHNil[X, AR <: HList]: Aux[X, AR, HNil, Option[X]] = instance(
      (l, sh) => {
        if(l.length == 1){Some(l(0))} else {None}
      }
    )

    implicit def ifShapeIsNotHNil[_S, A0[_], T, A1p <: HList, SH1p <: HList, NxtO](implicit 
      dc: ArraySort.Aux[A0[T] :: A1p, Ascending],
      ai: IsArray[A0, T] { type S = _S },
      fe: FromElemsOpt.Aux[A0[T], A1p, SH1p, Option[NxtO]],
    ): Aux[_S, A0[T] :: A1p, Int :: SH1p, Option[NxtO]] = instance(
      (l, sh) => {
        val thisA: A0[T] = ai.getEmpty
        val h1Nil = Nil: List[A0[T]]
        val combinedS: Option[List[A0[T]]] = combineS[A0, T, _S](thisA, h1Nil, l, sh.head)
        combinedS.flatMap(
          a1s => fe(a1s, sh.tail)
        )
      }
    )

    implicit def ifArraysDescending[AR <: HList, T, SH <: HList, RSH <: HList, RAR <: HList, NxtO](implicit 
      ds: ArraySort.Aux[AR, Descending],
      r0: Reverse.Aux[SH, RSH],
      r1: Reverse.Aux[AR, RAR],
      fe: FromElemsOpt.Aux[T, RAR, RSH, Option[NxtO]],
    ): Aux[T, AR, SH, Option[NxtO]] = instance(
      (l, sh) => fe(l, r0(sh))
    )

    def combineS[A[_], T, _S](
      aEmpty: A[T], as: List[A[T]], l: List[_S], width: Int,
    )(implicit 
      ar: IsArray[A, T] { type S = _S },
    ): Option[List[A[T]]] = l.length match {
      case 0 => Some(as.reverse)
      case x if x >= width => {
        val (ths, rst) = l.splitAt(width)
        val thsA: A[T] = ths.reverse.foldLeft(aEmpty)((s, o) => ar.cons(s, o))
        combineS[A, T, _S](aEmpty, thsA :: as, rst, width)
      }
      case _ => None
    }
  }

  trait Flatten[A[_], T] { self =>
    type Out = List[T]
    def apply(a: A[T]): List[T]
  }
  object Flatten {
    type Aux[A[_], T] = Flatten[A, T]
    def instance[A[_], T](f: A[T] => List[T]): Aux[A, T] = 
    new Flatten[A, T] {
      override type Out = List[T]
      def apply(a: A[T]): List[T] = f(a)
    }
    def apply[A[_], T](implicit fl: Flatten[A, T]): Aux[A, T] = fl

    implicit def flattenIfSIsT[A[_], T](implicit 
      ar: IsArray[A, T] { type S = T },
    ): Flatten[A, T] = instance(a => ar.toList(a))
    implicit def flattenIfSIsNotT[A[_], T, _S[T]](implicit 
      ar: IsArray[A, T] { type S = _S[T] },
      sIsArr: IsArray[_S, T], 
      sFl: Flatten[_S, T],
    ): Flatten[A, T] = instance(a => ar.toList(a).map(sIsArr.flatten(_)).flatten) 
  }

  trait AddOpt[A[_], B[_], T] { self =>
    type Out = Option[A[T]]
    def apply(a: A[T], b: B[T]): Out
  }
  object AddOpt {
    type Aux[A[_], B[_], T] = AddOpt[A, B, T]
    def apply[A[_], B[_], T](implicit ad: AddOpt[A, B, T]): Aux[A, B, T] = ad
    def instance[A[_], B[_], T](f: (A[T], B[T]) => Option[A[T]]): Aux[A, B, T] = 
    new AddOpt[A, B, T] { 
      def apply(a: A[T], b: B[T]): Option[A[T]] = f(a, b)
    }
    implicit def ifSameType[A[_], T, SH <: HList](implicit 
      isArr: IsArray[A, T],
      sh: Shape.Aux[A[T], SH],
      cs: CombineShapesOpt[SH],
    ): Aux[A, A, T] = instance((a, b) => 
      cs(sh(a), sh(b), 0).map(_ => isArr.fromList(isArr.toList(a) ++ isArr.toList(b)))
    )
    implicit def ifDiffType[A[_], B[_], T, AR <: HList, SH <: HList](implicit
      flA: Flatten[A, T],
      flB: Flatten[B, T],
      sa: SubArrays.Aux[A[T], AR],
      aSh: Shape.Aux[A[T], SH],
      bSh: Shape.Aux[B[T], SH],
      cs: CombineShapesOpt[SH],
      fe: FromElemsOpt.Aux[T, AR, SH, Option[A[T]]],
    ): Aux[A, B, T] = instance((a, b) => 
      cs(aSh(a), bSh(b), 0).flatMap(sh => fe(flA(a) ++ flB(b), sh))
    )
  }

  trait Transpose[A, IN] {
    type Out = A
    def apply(a: A): Out
  }
  object Transpose {
    type Aux[A, IN] = Transpose[A, IN]
    def apply[A, IN](implicit tr: Transpose[A, IN]): Aux[A, IN] = tr
    def instance[A, IN](f: A => A): Aux[A, IN] = new Transpose[A, IN] { 
      def apply(a: A): A = f(a)
    }

    implicit def ifNil[A, DE <: Nat, DEm1 <: Nat](implicit
      de: Depth.Aux[A, DE],
      e1: Pred.Aux[DE, DEm1],
      tr: TransAllDT[A, Nat._0, DEm1],
    ): Aux[A, AllSlice] = instance(a => tr(a))

    implicit def ifTupleNat[A, XA <: Nat, XB <: Nat](implicit
      tr: TransAxDT[A, XA, XB],
    ): Aux[A, (XA, XB)] = instance(a => tr(a))

    trait TransAxDT[A, XA <: Nat, XB <: Nat] {
      type Out = A
      def apply(a: A): Out
    }
    object TransAxDT {
      type Aux[A, XA <: Nat, XB <: Nat] = TransAxDT[A, XA, XB]
      def apply[A, XA <: Nat, XB <: Nat](implicit tr: TransAxDT[A, XA, XB]): Aux[A, XA, XB] = tr
      def instance[A, XA <: Nat, XB <: Nat](f: A => A): Aux[A, XA, XB] = new TransAxDT[A, XA, XB] {
        def apply(a: A): A = f(a)
      }

      implicit def ifCurrDimIsXA[A[_], B[_], T, BS, XA <: Nat, XB <: Nat](implicit
        e1: GT[XB, XA],
        e2: XA =:= Nat._0,
        ar: IsArray[A, T] { type S = B[T] },
        bIsArr: IsArray[B, T] { type S = BS },
      ): Aux[A[T], XA, XB] = instance(a => {
        val lst2d: List[List[BS]] = ar.toList(a).map(bIsArr.toList(_))
        val lstB: List[B[T]] = lst2d.transpose.map(lstBs => bIsArr.fromList(lstBs))
        ar.fromList(lstB)
      })

      implicit def ifCurrDimIsNotXA[A[_], T, _S[_], XA <: Nat, XB <: Nat, _XA <: Nat, _XB <: Nat](implicit
        e1: GT[XB, XA],
        e2: GT[XA, Nat._0],
        _xa: Pred.Aux[XA, _XA],
        _xb: Pred.Aux[XB, _XB],
        ar: IsArray[A, T] { type S = _S[T] },
        trForS: TransAxDT[_S[T], _XA, _XB],
      ): Aux[A[T], XA, XB] = instance(a => ar.fromList(ar.toList(a).map(trForS(_))))
    }

    trait TransAllDT[A, DM <: Nat, PS <: Nat] {
      type Out = A
      def apply(a: A): Out
    }
    object TransAllDT {
      type Aux[A, DM <: Nat, PS <: Nat] = TransAllDT[A, DM, PS]
      def apply[A, DM <: Nat, PS <: Nat](implicit tr: TransAllDT[A, DM, PS]): Aux[A, DM, PS] = tr
      def instance[A, DM <: Nat, PS <: Nat](f: A => A): Aux[A, DM, PS] = new TransAllDT[A, DM, PS] { 
        def apply(a: A): A = f(a)
      }

      implicit def whileDmLessThanPass[A, DM <: Nat, PS <: Nat] (implicit
        e1: GT[PS, DM],
        ta: TransAxDT[A, DM, Succ[DM]],
        nxt: Lazy[TransAllDT[A, Succ[DM], PS]],
      ): Aux[A, DM, PS] = instance(a => nxt.value(ta(a)))

      implicit def dmEqualsPass[A, DM <: Nat, PS <: Nat, PSm1 <: Nat] (implicit
        e1: GT[PS, Nat._0],
        e2: DM =:= PS,
        e3: Pred.Aux[PS, PSm1],
        nxt: Lazy[TransAllDT[A, Nat._0, PSm1]],
      ): Aux[A, DM, PS] = instance(a => nxt.value(a))

      implicit def psEqualsZero[A]: Aux[A, Nat._0, Nat._0] = instance(a => a)
    }
  }
  
  trait Concatenate[A[_], B[_], T, D <: Nat] { self =>
    type Out = Option[A[T]]
    def apply(a: A[T], b: B[T]): Out
  }
  object Concatenate {
    def apply[A[_], B[_], T, D <: Nat](implicit cn: Concatenate[A, B, T, D]): Concatenate[A, B, T, D] = cn
    implicit def ifDim0[A[_], B[_], T](implicit 
      ad: AddOpt.Aux[A, B, T],
    ): Concatenate[A, B, T, Nat._0] = new Concatenate[A, B, T, Nat._0] { 
      def apply(a: A[T], b: B[T]): Out = ad(a, b)
    }
    implicit def ifNotDim0[A[_], B[_], T, D <: Nat, Dm1 <: Nat, _SA[_], _SB[_]](implicit
      a0: IsArray[A, T] { type S = _SA[T] },
      b0: IsArray[B, T] { type S = _SB[T] },
      gt: GT[D, Nat._0],
      dm1: Pred.Aux[D, Dm1],
      cn: Concatenate[_SA, _SB, T, Dm1] { type Out = Option[_SA[T]] },
    ): Concatenate[A, B, T, D] = new Concatenate[A, B, T, D] {
      def apply(a: A[T], b: B[T]): Out = {
        val cs = for((sA, sB) <- a0.toList(a).zip(b0.toList(b))) yield (cn(sA, sB))
        if(cs.forall(_.isDefined)) {Some(a0.fromList(cs.map(_.get)))} else {None}
      }
    }
  }

  trait ConcatenateOpt[A[_], B[_], T] { self =>
    type Out = Option[A[T]]
    def apply(a: A[T], b: B[T], dim: Int): Out
  }
  object ConcatenateOpt {
    type Aux[A[_], B[_], T] = ConcatenateOpt[A, B, T]
    def apply[A[_], B[_], T](implicit cn: ConcatenateOpt[A, B, T]): ConcatenateOpt[A, B, T] = cn
    def instance[A[_], B[_], T](f: (A[T], B[T], Int) => Option[A[T]],
    ): Aux[A, B, T] = new ConcatenateOpt[A, B, T] {
      def apply(a: A[T], b: B[T], dim: Int): Option[A[T]] = f(a, b, dim)
    }
    implicit def ifNoSubConc[A[_], B[_], T](implicit 
      ar: IsArray[A, T] { type S = T },
      br: IsArray[B, T] { type S = T },
      cn: Concatenate[A, B, T, Nat._0],
    ): Aux[A, B, T] = instance((a, b, dim) => 
      if(dim == 0) {
        cn(a, b)
      } else {None}
    )
    implicit def ifSubConc[A[_], B[_], T, _SA[_], _SB[_]](implicit 
      ar: IsArray[A, T] { type S = _SA[T] },
      br: IsArray[B, T] { type S = _SB[T] },
      ad: AddOpt[A, B, T],
      cn: Aux[_SA, _SB, T],
    ): Aux[A, B, T] = instance((a, b, dim) => 
      if(dim == 0) {
        ad(a, b)
      } else {
        val cO: List[Option[_SA[T]]] = for(
          (sA, sB) <- ar.toList(a).zip(br.toList(b))
        ) yield (cn(sA, sB, dim-1))
        if(cO.forall(_.isDefined)){Some(ar.fromList(cO.map(_.get)))} else {None}
      }
    )
  }

  trait Where[A[_], T] {
    type Out = A[T]
    def apply(a: A[T], mask: A[Boolean], to: A[T]): Out
  }
  object Where {
    type Aux[A[_], T] = Where[A, T]
    def instance[A[_], T](f: (A[T], A[Boolean], A[T]) => A[T]): Aux[A,T] = new Where[A, T] {
      def apply(a: A[T], mask: A[Boolean], to: A[T]): Out = f(a, mask, to)
    }
    def apply[A[_], T](implicit wh: Where[A, T]): Aux[A, T] = wh

    implicit def ifArray[A[_], T, AR <: HList, SH <: HList](implicit 
      sh: Shape.Aux[A[T], SH],
      sa: SubArrays.Aux[A[T], AR],
      fe: FromElemsOpt.Aux[T, AR, SH, Option[A[T]]], 
      flA: Flatten[A, T],
      flB: Flatten[A, Boolean],
    ): Aux[A, T] = instance((a, mask, to) => {
      val bs = flB(mask)
      val ns = flA(to)
      val upd = for((t, i) <- flA(a).zipWithIndex) yield (
        if(bs(i)) {ns(i)} else {t}
      )
      fe(upd, sh(a)).get
    })
  }

  trait MaskFromNumSeq[A, R <: HList] {
    type Out = A
    def apply(ref: R, mask: A): Out
  }
  object MaskFromNumSeq {
    type Aux[A, R <: HList] = MaskFromNumSeq[A, R] { type Out = A }
    def instance[A, R <: HList](f: (R, A) => A): Aux[A, R] = new MaskFromNumSeq[A, R] { 
      def apply(ref: R, mask: A): A = f(ref, mask)
    }
    def apply[A, R <: HList](implicit ma: MaskFromNumSeq[A, R]): Aux[A, R] = ma 

    implicit def ifRefIsHNil[A]: Aux[A, HNil] = instance((r, m) => m)

    implicit def ifHeadIsListIntNotBase[A[_], _S, R1p <: HList, DE <: Nat](implicit
      ar: IsArray[A, Boolean] { type S = _S },
      de: Depth.Aux[A[Boolean], DE],
      deGt1: GT[DE, Nat._1],
      maskS: MaskFromNumSeq[_S, R1p], 
    ): Aux[A[Boolean], List[Int] :: R1p] = instance((r, m) => {
      val newS = for((s, i) <- ar.toList(m).zipWithIndex) yield (
        if (r.head.contains(i)) {maskS(r.tail, s)} else {s}
      )
      ar.fromList(newS)
    })

    implicit def ifHeadIsListIntIsBase[A[_]](implicit
      ar: IsArray[A, Boolean] { type S = Boolean },
    ): Aux[A[Boolean], List[Int] :: HNil] = instance((r, m) => {
      val newS = for((s, i) <- ar.toList(m).zipWithIndex) yield (
        if (r.head.contains(i)) {true} else {false}
      )
      ar.fromList(newS)
    })
  }

  trait ApplyIndexDT[A, R, AR] {
    type Out
    def apply(a: A, ref: R): Out
  }
  object ApplyIndexDT {
    type Aux[A, R, AR, O] = ApplyIndexDT[A, R, AR] { type Out = O }
    def instance[A, R, AR, O](f: (A, R) => O): Aux[A, R, AR, O] = new ApplyIndexDT[A, R, AR] { 
      type Out = O
      def apply(a: A, ref: R): Out = f(a, ref)
    }
    def apply[A, R, AR](implicit ai: ApplyIndexDT[A, R, AR]): Aux[A, R, AR, ai.Out] = ai

    implicit def ifHeadIsInt[A[_], T, _S, R1p <: HList, AR <: HList, O](implicit
      ar: IsArray[A, T] { type S = _S },
      ai: ApplyIndexDT[_S, R1p, AR], 
    ): Aux[A[T], Int :: R1p, AR, ai.Out] = instance((a, r) => 
      ai(ar.getAtN(a, r.head), r.tail)
    )
    implicit def ifHeadIsListInt[A[_], T, _S, R1p <: HList, A0[_], A1p <: HList, SO](implicit
      ar: IsArray[A, T] { type S = _S },
      ai: ApplyIndexDT[_S, R1p, A1p] { type Out = SO }, 
      oa: IsArray[A0, T] { type S = SO }
    ): Aux[A[T], List[Int] :: R1p, A0[T] :: A1p, A0[T]] = instance((a, r) => {
      val origS: List[_S] = r.head.map(ar.getAtN(a, _))
      val locedS: List[SO] = origS.map(ai(_, r.tail))
      oa.fromList(locedS)
    })

    implicit def ifRefIsHNil[A, AR <: HList]: Aux[A, HNil, AR, A] = instance((a, r) => a)
  }

  abstract class GetLoc[A[_], T, R] {
    def loc(self: A[T], ref: R): A[T] = ???
  }

  abstract class SetILoc[A[_], T, R] {
    def loc(self: A[T], ref: R): A[T] = ???
  }
}

