package sportarray

//import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsBase}
import IndicesObj.Index

import scala.annotation.implicitNotFound
import java.time.LocalDate
import shapeless.{HList, HNil, Lazy, :: => #:, Nat, Succ, LUBConstraint}
import shapeless.ops.hlist._
//import shapeless._
import shapeless.nat._
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
    def toList(a: A[T])(implicit ls: ListSubs[A[T]]): ls.Out = ls(a) 
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
      fe: FromElemsAndSubArraysOpt[AR, T, SH], 
    ): fe.Out = fe(listT, shape)
    def fromElems[AR <: HList, SH <: HList](a: A[T], listT: List[T])(implicit 
      sh: Shape.Aux[A[T], SH],
      sa: SubArrays.Aux[A[T], AR],
      fe: FromElemsAndSubArraysOpt[AR, T, SH], 
    ): fe.Out = fe(listT, sh(a))
    def reshape[AR <: HList, SH <: HList](a: A[T], shape: SH)(implicit 
      fl: Flatten[A, T],
      sa: SubArrays.Aux[A[T], AR],
      fe: FromElemsAndSubArraysOpt[AR, T, SH],
    ): fe.Out = {
      fromElems(a, fl(a), shape)
    }
    def map[_T, AR <: HList, SH <: HList](a: A[T], f: (T) => _T)(implicit
      fl: Flatten[A, T],
      sh: Shape.Aux[A[T], SH], 
      sa: SubArrays.Aux[A[_T], AR],
      fr: FromElemsAndSubArraysOpt.Aux[AR, _T, SH, Option[A[_T]]],
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
      def toList(implicit ls: ListSubs[A[T]]): ls.Out = tc.toList(a)
      def fromList(listS: List[_S]): A[T] = tc.fromList(listS)
      def shape(implicit sh: Shape[A[T]]): sh.Out = tc.shape(a)
      def flatten(implicit fl: Flatten[A, T]): List[T] = fl(a)
      def fromElems[AR <: HList, SH <: HList](listT: List[T], shape: SH)(implicit 
        ga: SubArrays.Aux[A[T], AR],
        fr: FromElemsAndSubArraysOpt[AR, T, SH],
      ): fr.Out = tc.fromElems(a, listT, shape)
      def fromElems[AR <: HList, SH <: HList](listT: List[T])(implicit 
        sh: Shape[A[T]] { type Out = SH },
        ga: SubArrays.Aux[A[T], AR],
        fr: FromElemsAndSubArraysOpt[AR, T, SH], 
      ): fr.Out = tc.fromElems(a, listT)
      def reshape[AR <: HList, SH <: HList](shape: SH)(implicit 
        fl: Flatten[A, T],
        ga: SubArrays.Aux[A[T], AR],
        rs: FromElemsAndSubArraysOpt[AR, T, SH],
      ) = tc.reshape(a, shape)
      def map[_T, AR <: HList, SH <: HList](f: T => _T)(implicit
        ai: IsArray[A, _T],
        fl: Flatten[A, T],
        sh: Shape.Aux[A[T], SH], 
        ga: SubArrays.Aux[A[_T], AR],
        fr: FromElemsAndSubArraysOpt.Aux[AR, _T, SH, Option[A[_T]]],
      ): A[_T] = tc.map(a, f)
    }
  }

  trait ListSubs[A] {
    type Out <: List[_]
    def apply(a: A): Out
  }
  object ListSubs {
    type Aux[A, O <: List[_]] = ListSubs[A] { type Out = O }
    def apply[A] (implicit tl: ListSubs[A]): Aux[A, tl.Out] = tl
    def instance[A, O <: List[_]](f: A => O): Aux[A, O] = new ListSubs[A] {
      type Out = O
      def apply(a: A): Out = f(a)
    }

    implicit def ifArray[A[_], T, _S] (implicit
      ia: IsArray[A, T] { type S = _S }
    ): Aux[A[T], List[_S]] = instance(a => 
      (for(i <- 0 to ia.length(a) - 1) yield (ia.getAtN(a, i))).toList
    )
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

    implicit def ifHListBool[A[_], _A[_], T, SH <: HList, _SH <: HList, N <: HList] (implicit
      sh: Shape.Aux[A[T], SH],
      es: ExpandShapeDims.Aux[SH, N, _SH],
      fl: Flatten[A, T],
      fe: FromElemsAndArrayOpt.Aux[_A, T, _SH, Option[_A[T]]], 
    ): Aux[A[T], _A[T], N, _A[T]] = instance(a => fe(fl(a), es(sh(a))).get)

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
      pr: Prepend.Aux[SHP, Int #: SHS, _SH],
      fe: FromElemsAndSubArraysOpt.Aux[AR, T, _SH, Option[_A[T]]], 
    ): Aux[A[T], _A[T], N, _A[T]] = instance(a => {
      val origSh = sh(a)
      val (pre, suf) = sp(origSh)
      val newSh = pre ++ (1 :: suf)
      fe(fl(a), newSh).get
    })
  }

  trait ExpandShapeDims[SH <: HList, M <: HList] {
    type Out <: HList
    def apply(sh: SH): Out
  }
  object ExpandShapeDims {
    // use M as output
    type Aux[SH <: HList, M <: HList, O] = ExpandShapeDims[SH, M] { type Out = O }
    def instance[SH <: HList, M <: HList, O <: HList](f: SH => O): Aux[SH, M, O] = new ExpandShapeDims[SH, M] {
      type Out = O
      def apply(sh: SH): Out = f(sh)
    }
    def apply[SH <: HList, M <: HList] (implicit es: ExpandShapeDims[SH, M]): Aux[SH, M, es.Out] = es
    
    implicit def ifHeadIsTrue[SH1p <: HList, M1p <: HList] (implicit
      nx: ExpandShapeDims[SH1p, M1p],
    ): Aux[Int #: SH1p, Nat._1 #: M1p, Int #: nx.Out] = instance(sh => sh.head :: nx(sh.tail))
    
    implicit def ifHeadIsFalse[SH <: HList, M1p <: HList] (implicit
      nx: ExpandShapeDims[SH, M1p],
    ): Aux[SH, Nat._0 #: M1p, Int #: nx.Out] = instance(sh => 1 :: nx(sh))

    implicit def ifHNil: Aux[HNil, HNil, HNil] = instance(sh => HNil)
  }

  trait BroadcastOpt[A, SH <: HList, _A] {
    type Out = Option[_A]
    def apply(a: A, shape: SH): Out
  }
  object BroadcastOpt {
    type Aux[A, SH <: HList, _A] = BroadcastOpt[A, SH, _A]
    def instance[A, SH <: HList, _A] (f: (A, SH) => Option[_A]): Aux[A, SH, _A] = 
    new BroadcastOpt[A, SH, _A] {
      def apply(a: A, shape: SH): Option[_A] = f(a, shape)
    }
    def apply[A, SH <: HList, _A] (implicit br: BroadcastOpt[A, SH, _A]): Aux[A, SH, _A] = br
    
    implicit def ifShapesBroadcast[
      A[_], T, _A[_], SHA <: HList, SH <: HList, LE <: Nat, D_A <: Nat,
    ] (implicit
      sa: Shape.Aux[A[T], SHA],
      le: Length.Aux[SH, LE],
      d_a: Depth.Aux[_A[T], D_A],
      e0: LE =:= D_A,
      bs: BroadcastShapesOpt.Aux[SHA, SH],
      fl: Flatten[A, T],
      fe: FromElemsAndArrayOpt.Aux[_A, T, SH, Option[_A[T]]],
    ): Aux[A[T], SH, _A[T]] = instance((a, shape) => {
      val sO = bs.apply(sa(a), shape)
      sO.flatMap(sh => fe(fl(a), sh))
    })
  }

  trait BroadcastShapesOpt[SHA <: HList, SHB <: HList] {
    type Out <: Option[SHB]
    def apply(a: SHA, b: SHB): Out
  }
  object BroadcastShapesOpt {
    type Aux[SHA <: HList, SHB <: HList] = BroadcastShapesOpt[SHA, SHB]
    def instance[SHA <: HList, SHB <: HList] (
      f: (SHA, SHB) => Option[SHB],
    ): Aux[SHA, SHB] = new BroadcastShapesOpt[SHA, SHB] {
      type Out = Option[SHB]
      def apply(a: SHA, b: SHB): Option[SHB] = f(a, b)
    }
    def apply[SHA <: HList, SHB <: HList](
      implicit br: BroadcastShapesOpt[SHA, SHB],
    ): Aux[SHA, SHB] = br

    implicit def ifSHBGtEqSHA[SHA <: HList, SHB <: HList, LA <: Nat, LB <: Nat] (implicit
      l0: Length.Aux[SHA, LA],
      l1: Length.Aux[SHB, LB],
      e0: GTEq[LB, LA],
      al: ToList[SHA, Int],
      bl: ToList[SHB, Int],
    ): Aux[SHA, SHB] = instance((a, b) => {
      def go(a: List[Int], b: List[Int]): Boolean = (a.headOption, b.headOption) match {
        case (Some(ah), Some(bh)) if ah == bh => go(a.tail, b.tail)
        case (Some(ah), Some(bh)) if ah != bh => go(a, b.tail)
        case (None, Some(bh)) => true
        case (None, None) => true
        case (Some(ah), None) => false
      }
      if(go(al(a), bl(b))) { Some(b) } else { None }
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

  /*
   * Witnesses that an HList of Array objects are sorted in Ascending (from lowest depth to
   * highest) or Descending (from highest depth to lowest) order.
   */

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
      nx: ArraySort.Aux[A1[T] #: A2p, Descending],
    ): Aux[A0[T] #: A1[T] #: A2p, Descending] = 
      new ArraySort[A0[T] #: A1[T] #: A2p] { type Out = Descending }

    implicit def ifTwoElemsRemainingDesc[A0[_], A1[_], T, DE0 <: Nat, DE1 <: Nat] (implicit
      d0: Depth.Aux[A0[T], DE0],
      d1: Depth.Aux[A1[T], DE1],
      gt: GT[DE0, DE1],
    ): Aux[A0[T] #: A1[T] #: HNil, Descending] = 
      new ArraySort[A0[T] #: A1[T] #: HNil] { type Out = Descending }

    implicit def ifTwoPlusElemsRemainingAsc[A0[_], A1[_], T, A2p <: HList, DE0 <: Nat, DE1 <: Nat] (implicit
      d0: Depth.Aux[A0[T], DE0],
      d1: Depth.Aux[A1[T], DE1],
      gt: GT[DE1, DE0],
      nx: ArraySort.Aux[A1[T] #: A2p, Ascending],
    ): Aux[A0[T] #: A1[T] #: A2p, Ascending] = 
      new ArraySort[A0[T] #: A1[T] #: A2p] { type Out = Ascending }

    implicit def ifTwoElemsRemainingAsc[A0[_], A1[_], T, DE0 <: Nat, DE1 <: Nat] (implicit
      d0: Depth.Aux[A0[T], DE0],
      d1: Depth.Aux[A1[T], DE1],
      gt: GT[DE1, DE0],
    ): Aux[A0[T] #: A1[T] #: HNil, Ascending] = 
      new ArraySort[A0[T] #: A1[T] #: HNil] { type Out = Ascending }

    implicit def ifSingleElemList[A0[_], T, O <: ArraySortedBy]: Aux[A0[T] #: HNil, O] = 
      new ArraySort[A0[T] #: HNil] { type Out = O }
    
  }

  /**
   * Typeclass used to remove a specified array dimension, by aggregating (using the combine
   * function) across that dimension.
   */
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
      fe: FromElemsAndSubArraysOpt.Aux[AR, T, SH, Option[Out]], 
      dm: ToInt[DM],
    ): Aux[A, T, DM, Out] = instance((a, cmb) => {
      val lst: List[T] = rd(a, cmb)
      val dim = dm()
      val (lf, rg) = sp(sh(a))
      val shape: SH = in(lf) ++ rg
      val arrO = fe(lst, shape)
      arrO.get
    })

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
    ): Aux[A, T, Int #: HNil] = instance(
      (a, ref, elem) => ia.fromList(ia.toList(a).updated(ref.head, elem))
    )

    implicit def ifHListIntIsArr[A[_], T, _S[_], R1p <: HList](implicit
      ia: IsArray[A, T] { type S = _S[T] },
      iaS: SetElem[_S, T, R1p],
    ): Aux[A, T, Int #: R1p] = instance((a, ref, elem) => {
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

    implicit def ifIdxIsRange[A[_], T](implicit
      ar: IsArray[A, T],
    ): Aux[A[T], Range, A[T]] = instance((a, rs) => ar.fromList(
      rs.toList.map(ar.getAtN(a, _)))
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
      gi: ApplyIndexFromSubArrays[A[T], Idx, RevRdAR],
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
      fl: Flatten[A, T],
    ): Int = 
      fl(a).map(_.toString.length).max

    implicit def ifIs1d[A[_], T, S](implicit 
      ls: ListSubs.Aux[A[T], List[S]],
      de: Depth.Aux[A[T], Nat._1],
      fl: Flatten[A, T],
    ): PrettyPrint[A[T]] = instance((a, indO) => {
      val mW = maxWidth(a)
      "[" ++ ls(a).map(_.toString.padTo(mW, ' ')).mkString(", ") ++ "]"
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
    ): CombineShapesOpt[Int #: Tl] = instance((a, b, dim) => 
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
        rv: Reverse[A[T] #: L],
      ): Aux[A[T], L, rv.Out] = new GetARDesc[A[T], L] { type Out = rv.Out }

      implicit def ifSIsArr[A[_], T, _S[_], _S1, L <: HList](implicit 
        aIsABs: IsArray[A, T] { type S = _S[T] },
        sIsABs: IsArray[_S, T],
        gaForS: GetARDesc[_S[T], A[T] #: L],
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
        rv: Reverse[Int #: L] { type Out = O },
      ): RecurAux[A[T], L, O] = recur((a, l) => rv(ai.length(a) :: l))

      implicit def gsIfSIsArr[A[_], T, S0[_], L <: HList](implicit 
        ai: IsArray[A, T] { type S = S0[T] },
        gsForS: ShapeRecur[S0[T], Int #: L],
      ): RecurAux[A[T], L, gsForS.Out] = recur((a, l) => 
        gsForS.apply(ai.getAtN(a, 0), ai.length(a) :: l)
      )
    }
  }

  //trait FromElemsAndSubArraysUsingListOpt[A0, A1p <: HList, X] {
    //type Out = Option[List[A0]]
    //def apply(l: List[X], sh: List[Int]): Out
  //}
  //object FromElemsAndSubArraysUsingListOpt {
    //type Aux[A0, A1p <: HList, X] = FromElemsAndSubArraysUsingListOpt[A0, A1p, X]
    //def instance[A0, A1p <: HList, X](
      //f: (List[X], List[Int]) => Option[List[A0]]
    //): Aux[A0, A1p, X] = new FromElemsAndSubArraysUsingListOpt[A0, A1p, X] {
      //def apply(l: List[X], sh: List[Int]): Out = f(l, sh)
    //}
    //def apply[A0, A1p <: HList, X](
      //implicit fe: FromElemsAndSubArraysUsingListOpt[A0, A1p, X],
    //): FromElemsAndSubArraysUsingListOpt.Aux[A0, A1p, X] = fe

    //implicit def ifArraysDescending[A0, A1p <: HList, X, SH <: HList, RSH <: HList, RAR <: HList](implicit 
      //ds: ArraySort.Aux[A0 #: A1p, Descending],
      //r1: Reverse.Aux[A0 #: A1p, RAR],
      //fe: FromElemsAndSubArraysUsingListOpt[A0, A1p, X],
    //): Aux[A0, A1p, X] = instance(
      //(l, sh) => fe(l, sh.reverse)
    //)

    //// TODO: what if X is an concrete type that is not T? eg T = Int, X = Double
    //// just have one typeclass for if X = T, one for if X[T]
    //implicit def ifArrIsFinal[A0[_], T, X] (implicit
      //ai: IsArray[A0, T] { type S = X },
    //): Aux[A0[T], HNil, X] = instance(
      //(l, sh) => if(sh.length == 1 && l.length == sh.head) {
        //Some(List(ai.fromList(l)))
      //} else { None }
    //)

    //// TODO: remove ArraySort to wrapper layer.
    //implicit def ifArrIsNotHNil[A0[_], A1[_], A2p <: HList, T, X](implicit 
      //dc: ArraySort.Aux[A0[T] #: A1[T] #: A2p, Ascending],
      //ai: IsArray[A0, T] { type S = X },
      //fe: FromElemsAndSubArraysUsingListOpt[A1[T], A2p, A0[T]] = null,
    //): Aux[A0[T], A1[T] #: A2p, X] = instance(
      //(l, sh) => sh match {
        //case s :: Nil => {
          //if(l.length == s) { Some(List(ai.fromList(l))) } else { None }
        //}
        //case h :: t => {
          //val thisA: A0[T] = ai.getEmpty
          //val h1Nil = Nil: List[A0[T]]
          //val combinedS: Option[List[A0[T]]] = combineS[A0, T, X](thisA, h1Nil, l, sh.head)
          //combinedS.flatMap(
            //a1s => fe(a1s, sh.tail)
          //)
        //}
      //}
    //)

    ////implicit def fromListIntIfBase[A0[_], A1p <: HList, T, NxtO] (implicit
      ////dc: ArraySort.Aux[A0[T] #: A1p, Ascending],
      ////ai: IsArray[A0, T] { type S = T },
      ////nxO: FromElemsAndSubArraysUsingListOpt.Aux[A1p, A0[T], Option[NxtO]],
    ////): Aux[A0[T] #: A1p, T, Option[NxtO]] = instance((l, sh) => {
      ////val thisA: A0[T] = ai.getEmpty
      ////val h1Nil = Nil: List[A0[T]]
      ////val combinedS: Option[List[A0[T]]] = combineS[A0, T, T](thisA, h1Nil, l, sh.head)
      ////Option(nxO) match {
        ////case Some(nx) => {
          ////combinedS.flatMap(
            ////a1s => nx(a1s, sh.tail)
          ////)
        ////}
        ////case None => {

        ////}
      ////}
    ////})
    
    //implicit def fromListInt[A0[_], A1p <: HList, T, _S, NxtO] (implicit
      //dc: ArraySort.Aux[A0[T] #: A1p, Ascending],
      //ai: IsArray[A0, T] { type S = _S },
      //nxO: FromElemsAndSubArraysUsingListOpt[A1p, A0[T], Option[NxtO]] = null,
    //): Aux[A0[T] #: A1p, _S, Option[NxtO]] = instance((l, sh) => Option(nxO).flatMap(
      //nx => {
        //val thisA: A0[T] = ai.getEmpty
        //val h1Nil = Nil: List[A0[T]]
        //val combinedS: Option[List[A0[T]]] = combineS[A0, T, _S](thisA, h1Nil, l, sh.head)
        //combinedS.flatMap(
          //a1s => nx(a1s, sh.tail)
        //)
      //}
    //))
  //}

  trait FromElemsAndSubArraysOpt[AR <: HList, T, SH <: HList] {
    type Out <: Option[_]
    def apply(l: List[T], sh: SH): Out
  }
  object FromElemsAndSubArraysOpt {
    type Aux[AR <: HList, T, SH <: HList, O <: Option[_]] = 
      FromElemsAndSubArraysOpt[AR, T, SH] { type Out = O }
    def instance[AR <: HList, T, SH <: HList, INIT <: Nat, O <: Option[_]](
      f: (List[T], SH) => O
    ): Aux[AR, T, SH, O] = new FromElemsAndSubArraysOpt[AR, T, SH] {
      type Out = O
      def apply(l: List[T], sh: SH): Out = f(l, sh)
    }
    def apply[AR <: HList, T, SH <: HList](
      implicit fe: FromElemsAndSubArraysOpt[AR, T, SH],
    ): FromElemsAndSubArraysOpt.Aux[AR, T, SH, fe.Out] = fe

    implicit def ifShapeIsHNil[AR <: HList, T]: Aux[AR, T, HNil, Option[T]] = instance(
      (l, sh) => {
        if(l.length == 1){Some(l(0))} else {None}
      }
    )

    implicit def ifShapeIsNotHNil[_S, A0[_], T, A1p <: HList, SH1p <: HList, NxtO](implicit 
      dc: ArraySort.Aux[A0[T] #: A1p, Ascending],
      ai: IsArray[A0, T] { type S = _S },
      fe: FromElemsAndSubArraysOpt.Aux[A1p, A0[T], SH1p, Option[NxtO]],
    ): Aux[A0[T] #: A1p, _S, Int #: SH1p, Option[NxtO]] = instance(
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
      fe: FromElemsAndSubArraysOpt.Aux[RAR, T, RSH, Option[NxtO]],
    ): Aux[AR, T, SH, Option[NxtO]] = instance(
      (l, sh) => fe(l, r0(sh))
    )
  }

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

  /**
   * Constructs an Array object from an Array type, a list of elements, and a desired shape
   */

  trait FromElemsAndArrayOpt[_A[_], X, SH <: HList] {
    type Out <: Option[Any]
    def apply(l: List[X], sh: SH): Out
  }
  object FromElemsAndArrayOpt {
    type Aux[_A[_], X, SH <: HList, O <: Option[_]] = 
      FromElemsAndArrayOpt[_A, X, SH] { type Out = O }
    def instance[_A[_], T, SH <: HList, INIT <: Nat, O <: Option[_]](
      f: (List[T], SH) => O
    ): Aux[_A, T, SH, O] = new FromElemsAndArrayOpt[_A, T, SH] {
      type Out = O
      def apply(l: List[T], sh: SH): Out = f(l, sh)
    }
    def apply[_A[_], T, SH <: HList](
      implicit fe: FromElemsAndArrayOpt[_A, T, SH],
    ): FromElemsAndArrayOpt.Aux[_A, T, SH, fe.Out] = fe

    implicit def ifArray[_A[_], T, SH <: HList, SA <: HList](implicit 
      ia: IsArray[_A, T],
      sa: SubArrays.Aux[_A[T], SA],
      fe: FromElemsAndSubArraysOpt[SA, T, SH],
    ): Aux[_A, T, SH, fe.Out] = instance(
      (l, sh) => fe(l, sh)
    )
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
    implicit def ifSameType[A[_], T, _S, SH <: HList](implicit 
      ls: ListSubs.Aux[A[T], List[_S]],
      ia: IsArray[A, T] { type S = _S },
      sh: Shape.Aux[A[T], SH],
      cs: CombineShapesOpt[SH],
    ): Aux[A, A, T] = instance((a, b) => 
      cs(sh(a), sh(b), 0).map(_ => ia.fromList(ls(a) ++ ls(b)))
    )
    implicit def ifDiffType[A[_], B[_], T, AR <: HList, SH <: HList](implicit
      flA: Flatten[A, T],
      flB: Flatten[B, T],
      sa: SubArrays.Aux[A[T], AR],
      aSh: Shape.Aux[A[T], SH],
      bSh: Shape.Aux[B[T], SH],
      cs: CombineShapesOpt[SH],
      fe: FromElemsAndSubArraysOpt.Aux[AR, T, SH, Option[A[T]]],
    ): Aux[A, B, T] = instance((a, b) => 
      cs(aSh(a), bSh(b), 0).flatMap(sh => fe(flA(a) ++ flB(b), sh))
    )
  }

  //trait ReduceUsingList[A] {
    //type Out <: Option[_]
    //def apply(a: A, to: List[Int]): Out
  //}
  //object ReduceUsingList {
    //type Aux[A, O <: Option[_]] = ReduceUsingList[A] { type Out = O }
    //def apply[A](implicit re: ReduceUsingList[A]): Aux[A, re.Out] = re
    //def instance[A, O <: Option[_]](f: (A, List[Int]) => O): Aux[A, O] = new ReduceUsingList[A] { 
      //type Out = O
      //def apply(a: A, to: List[Int]): Out = f(a, to)
    //}

    //implicit def ifListInt[A[_], T, _S] (implicit
      //ia: IsArray[A, T] { type S = _S },
      ////ibO: IsArray[B, T] { type S = BS } = null,
      ////lsO: ListSubs.Aux[B[T], List[BS]] = null,
      ////nxO: TransposeAxRT[B[T]] = null,
    //): Aux[A[T]] = instance((a, in) => ???)
  //}

  //trait ReduceUsingInt[A[_], T] {
    //type Out <: Option[_]
    //def apply(a: A[T], dim: Int, combine: List[T] => T): Out
  //}
  //object ReduceUsingInt {
    //type Aux[A[_], T, O <: Option[_]] = ReduceUsingInt[A, T] { type Out = O }
    //def apply[A[_], T](implicit re: ReduceUsingInt[A, T]): Aux[A, T, re.Out] = re
    //def instance[A[_], T, O <: Option[_]](
      //f: (A[T], Int, List[T] => T) => O
    //): Aux[A, T, O] = new ReduceUsingInt[A, T] { 
      //type Out = O
      //def apply(a: A[T], dim: Int, combine: List[T] => T): Out = f(a, dim, combine)
    //}

    //implicit def ifInt[A[_], T, _S, SH <: HList] (implicit
      //rd: ReduceUsingIntToList[A, T],
      //s0: Shape.Aux[A[T], SH],
      //sh: ToList[SH, Int],
      //fe: FromElemsAndSubArraysUsingListOpt.Aux[AR, T, SH, Option[Out]], 
    //): Aux[A, T, Option[_S]] = instance((a, dim, cmb) => rd(a, dim, cmb).flatMap(
      //lst => {
        //val origShape = sh(s0(a)) 
        //val newShape = (origShape take dim) ++ (origShape drop (dim + 1))
        //val arrO = fe(lst, newShape)
        //arrO.get
      //}
    //))

    //trait ReduceUsingIntToList[A[_], T] {
      //type Out = Option[List[T]]
      //def apply(a: A[T], dim: Int, combine: List[T] => T): Out
    //}
    //object ReduceUsingIntToList {
      //type Aux[A[_], T] = ReduceUsingIntToList[A, T]
      //def apply[A[_], T](implicit re: ReduceUsingIntToList[A, T]): Aux[A, T] = re
      //def instance[A[_], T](
        //f: (A[T], Int, List[T] => T) => Option[List[T]]
      //): Aux[A, T] = new ReduceUsingIntToList[A, T] { 
        //def apply(a: A[T], dim: Int, combine: List[T] => T): Option[List[T]] = f(a, dim, combine)
      //}

      //implicit def ifInt[A[_], T, _S[_], DE <: Nat] (implicit
        //d0: Depth.Aux[A[T], DE],
        //de: ToInt[DE],
        //ls: ListSubs.Aux[A[T], List[_S[T]]],
        //nxO: ReduceUsingIntToList[_S, T] = null,
        //flO: Flatten[_S, T] = null,
      //): Aux[A, T] = instance((a, dim, cmb) => Option(nxO).flatMap( 
        //nx => dim match {
          //case d if d > 0 => { 
            //val lstS: List[_S[T]] = ls(a)
            //val lst: List[List[T]] = lstS.flatMap(nx(_, dim - 1, cmb))
            //if(lstS.length == lst.length) { 
              //Some(lst.foldLeft(Nil: List[T])(_ ++ _))
            //} else { None }
          //}
          //case 0 => {
            //Option(flO).flatMap(
              //fl => {
                //val lst2d: List[List[T]] = ls(a).map(fl(_))
                //Some(lst2d.transpose.map(cmb(_)))
              //}
            //)
          //}
        //}
      //))
    //}
  //}

  trait TransposeAxRT[A] {
    type Out = Option[A]
    def apply(a: A, in: (Int, Int)): Out
  }
  object TransposeAxRT {
    type Aux[A] = TransposeAxRT[A]
    def apply[A](implicit re: TransposeAxRT[A]): Aux[A] = re
    def instance[A](f: (A, (Int, Int)) => Option[A]): Aux[A] = new TransposeAxRT[A] { 
      def apply(a: A, in: (Int, Int)): Option[A] = f(a, in)
    }

    implicit def ifTupleInt[A[_], B[_], T, BS] (implicit
      ia: IsArray[A, T] { type S = B[T] },
      la: ListSubs.Aux[A[T], List[B[T]]],
      ibO: IsArray[B, T] { type S = BS } = null,
      lsO: ListSubs.Aux[B[T], List[BS]] = null,
      nxO: TransposeAxRT[B[T]] = null,
    ): Aux[A[T]] = instance((a, in) => 
      Option(ibO).flatMap(
        ib => Option(lsO).flatMap(
          ls => in match {
            case (0, 1) => {
              val lst2d: List[List[BS]] = la(a).map(ls(_))
              val lstB: List[B[T]] = lst2d.transpose.map(lstBs => ib.fromList(lstBs))
              val out: A[T] = ia.fromList(lstB)
              Some(out)
            }
            case (i0, i1) if i1 == i0 + 1 => {
              Option(nxO).flatMap(nx => {
                val lstS: List[B[T]] = la(a)
                val lstB: List[B[T]] = lstS.flatMap(
                  nx(_, (in._1 - 1, in._2 - 1))
                )
                if(lstS.length == lstB.length) { Some(ia.fromList(lstB)) } else { None } 
              }) 
            }
          }
    )))
  }

  trait TransposeFromListInt[A] {
    type Out = Option[A]
    def apply(a: A, seq: List[Int]): Out
  }
  object TransposeFromListInt {
    type Aux[A] = TransposeFromListInt[A]
    def apply[A](implicit re: TransposeFromListInt[A]): Aux[A] = re
    def instance[A](f: (A, List[Int]) => Option[A]): Aux[A] = new TransposeFromListInt[A] { 
      def apply(a: A, seq: (List[Int])): Option[A] = f(a, seq)
    }

    def swapElems[A](l: List[A], i: Int): List[A] = {
      val (l0, l1) = l.splitAt(i)
      val out = l0 ++ (l1(1) :: l1(0) :: l1.drop(2))
      assert(out.length == l.length)
      out
    }
    
    implicit def ifList[A, DE <: Nat] (implicit
      de: Depth.Aux[A, DE],
      tl: ToInt[DE],
      tr: TransposeAxRT[A],
    ): Aux[A] = instance((a, seq) => {
      def go(a: A, l: List[Int], i: Int): Option[A] = i match {
        case i if i + 1 == l.length => Some(a)
        case i if l(i + 1) > l(i) => go(a, l, i + 1)
        case i if l(i + 1) < l(i) => {
          val newL = swapElems(l, i)
          val newA = tr(a, (i, i + 1))
          newA.flatMap(go(_, newL, 0))
        }
        case i if l(i + 1) == l(i) => None //Throw error, bad input list
      }
      if((tl() == seq.length) && (tl() == seq.max - seq.min + 1)) { go(a, seq, 0) } else { None }
    })
  }

  trait TransposeUsingString[A] {
    type Out = Option[A]
    def apply(a: A, seq: String): Out
  }
  object TransposeUsingString {
    type Aux[A] = TransposeUsingString[A]
    def apply[A](implicit re: TransposeUsingString[A]): Aux[A] = re
    def instance[A](f: (A, String) => Option[A]): Aux[A] = new TransposeUsingString[A] { 
      def apply(a: A, seq: String): Option[A] = f(a, seq)
    }
    
    implicit def ifList[A, DE <: Nat] (implicit
      de: Depth.Aux[A, DE],
      tl: ToInt[DE],
      tr: TransposeFromListInt[A],
    ): Aux[A] = instance((a, seq) => tr(a, seq.map(_.toInt).toList))
  }


  /**
   * Transposes an array, either across two specific axes (if passed a Tuple2[Nat, Nat]) or across
   * all axes (if passed an AllSlice object).
   */
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

  /**
   * Concatenates an array, 
   */
  
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
      fe: FromElemsAndSubArraysOpt.Aux[AR, T, SH, Option[A[T]]], 
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
    ): Aux[A[Boolean], List[Int] #: R1p] = instance((r, m) => {
      val newS = for((s, i) <- ar.toList(m).zipWithIndex) yield (
        if (r.head.contains(i)) {maskS(r.tail, s)} else {s}
      )
      ar.fromList(newS)
    })

    implicit def ifHeadIsListIntIsBase[A[_]](implicit
      ar: IsArray[A, Boolean] { type S = Boolean },
    ): Aux[A[Boolean], List[Int] #: HNil] = instance((r, m) => {
      val newS = for((s, i) <- ar.toList(m).zipWithIndex) yield (
        if (r.head.contains(i)) {true} else {false}
      )
      ar.fromList(newS)
    })
  }

  trait ApplyIndexFromSubArrays[A, R, AR] {
    type Out
    def apply(a: A, ref: R): Out
  }
  object ApplyIndexFromSubArrays {
    type Aux[A, R, AR, O] = ApplyIndexFromSubArrays[A, R, AR] { type Out = O }
    def instance[A, R, AR, O](f: (A, R) => O): Aux[A, R, AR, O] = new ApplyIndexFromSubArrays[A, R, AR] { 
      type Out = O
      def apply(a: A, ref: R): Out = f(a, ref)
    }
    def apply[A, R, AR](implicit ai: ApplyIndexFromSubArrays[A, R, AR]): Aux[A, R, AR, ai.Out] = ai

    implicit def ifHeadIsInt[A[_], T, _S, R1p <: HList, AR <: HList, O](implicit
      ar: IsArray[A, T] { type S = _S },
      ai: ApplyIndexFromSubArrays[_S, R1p, AR], 
    ): Aux[A[T], Int #: R1p, AR, ai.Out] = instance((a, r) => 
      ai(ar.getAtN(a, r.head), r.tail)
    )

    implicit def ifHeadIsListInt[A[_], T, _S, R1p <: HList, A0[_], A1p <: HList, SO](implicit
      ar: IsArray[A, T] { type S = _S },
      ai: ApplyIndexFromSubArrays[_S, R1p, A1p] { type Out = SO }, 
      oa: IsArray[A0, T] { type S = SO }
    ): Aux[A[T], List[Int] #: R1p, A0[T] #: A1p, A0[T]] = instance((a, r) => {
      val origS: List[_S] = r.head.map(ar.getAtN(a, _))
      val locedS: List[SO] = origS.map(ai(_, r.tail))
      oa.fromList(locedS)
    })

    implicit def ifHeadIsRange[A[_], T, _S, R1p <: HList, A0[_], A1p <: HList, SO](implicit
      ar: IsArray[A, T] { type S = _S },
      ai: ApplyIndexFromSubArrays[_S, R1p, A1p] { type Out = SO }, 
      oa: IsArray[A0, T] { type S = SO }
    ): Aux[A[T], Range #: R1p, A0[T] #: A1p, A0[T]] = instance((a, r) => {
      val origS: List[_S] = r.head.toList.map(ar.getAtN(a, _))
      val locedS: List[SO] = origS.map(ai(_, r.tail))
      oa.fromList(locedS)
    })

    implicit def ifRefIsHNil[A, AR <: HList]: Aux[A, HNil, AR, A] = instance((a, r) => a)
  }
}

