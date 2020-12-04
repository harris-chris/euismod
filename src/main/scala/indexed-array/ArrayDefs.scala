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

    def setAtN(a: A[T], n: Int, elem: S): A[T] = fromList(toList(a).updated(n, elem))
    def apply[R](a: A[T], r: R)(implicit ai: ApplyIndex[A[T], R]): ai.Out = ai(a, r)
    def empty: A[T] = getEmpty[T]
    def ::(a: A[T], o: S): A[T] = cons(a, o)  
    def ++[B[_]](a: A[T], b: B[T])(implicit 
      cnCt: ConcatenateCT[A, B, T, Nat._0],
    ): cnCt.Out = cnCt(a, b)
    def toList(a: A[T]): List[S] = (for(i <- 0 to length(a) - 1) yield (getAtN(a, i))).toList
    def fromList(listS: List[S]): A[T] = 
      listS.reverse.foldLeft(getEmpty[T])((e, s) => cons(e, s))
    def ndims[SH <: HList](a: A[T])(implicit 
      sh: Shape[A[T]] { type Out = SH }, 
      tl: ToList[SH, Int],
    ): Int = shape(a).toList[Int].length
    def shape(a: A[T])(implicit sh: Shape[A[T]]): sh.Out = sh(a)
    def getArraysAsc(a: A[T])(implicit ga: GetArrsAsc[A, T, HNil]): ga.Out = ga(HNil)
    def getArraysDesc(a: A[T])(implicit ga: GetArrsDesc[A, T, HNil]): ga.Out = ga(HNil)
    def flatten(a: A[T])(implicit fl: Flatten[A, T]): List[T] = fl(a)
    def fromElems[GAOut <: HList, SH <: HList](a: A[T], listT: List[T], shape: SH)(implicit 
      ga: GetArrsAsc[A, T, HNil] { type Out = GAOut },
      fe: FromElems[T, GAOut, SH], 
    ): fe.Out = fe(listT, shape)
    def fromElems[GAOut <: HList, SH <: HList](a: A[T], listT: List[T])(implicit 
      sh: Shape[A[T]] { type Out = SH },
      ga: GetArrsAsc[A, T, HNil] { type Out = GAOut },
      fe: FromElems[T, GAOut, SH], 
    ): fe.Out = fe(listT, sh(a))
    def reshape[GAOut <: HList, SH <: HList](a: A[T], shape: SH)(implicit 
      fl: Flatten[A, T],
      ga: GetArrsAsc[A, T, HNil] { type Out = GAOut },
      fe: FromElems[T, GAOut, SH],
    ): fe.Out = {
      fromElems(a, fl(a), shape)
    }
    def map[_T, GAOut <: HList, SH <: HList](a: A[T], f: (T) => _T)(implicit
      fl: Flatten[A, T],
      a_tIsArr: IsArray[A, _T],
      sh: Shape[A[T]] { type Out = SH }, 
      ga: GetArrsAsc[A, _T, HNil] { type Out = GAOut },
      fr: FromElems[_T, GAOut, SH] { type Out = Option[A[_T]] },
    ): A[_T] = {
      val shape: SH = sh(a)
      val list_t: List[_T] = flatten(a).map(f)
      val empty_t: A[_T] = getEmpty[_T]
      val arrs: GAOut = ga(HNil)
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
      def setAtN(n: Int, elem: _S) = tc.setAtN(a, n, elem)
      def apply[R](r: R)(implicit ai: ApplyIndex[A[T], R]) = tc.apply(a, r)
      def ::(other: _S) = tc.cons(a, other)
      def ++[B[_]](b: B[T])(implicit cn: ConcatenateCT[A, B, T, Nat._0]) = tc.++(a, b)
      def length: Int = tc.length(a)
      def toList: List[_S] = tc.toList(a)
      def fromList(listS: List[_S]): A[T] = tc.fromList(listS)
      def getArraysAsc(implicit ga: GetArrsAsc[A, T, HNil]): ga.Out = tc.getArraysAsc(a)
      def getArraysDesc(implicit ga: GetArrsDesc[A, T, HNil]): ga.Out = tc.getArraysDesc(a)
      def shape(implicit sh: Shape[A[T]]): sh.Out = tc.shape(a)
      def flatten(implicit fl: Flatten[A, T]): List[T] = fl(a)
      def fromElems[Arrs <: HList, SH <: HList](listT: List[T], shape: SH)(implicit 
        ga: GetArrsAsc[A, T, HNil] { type Out = Arrs },
        fr: FromElems[T, Arrs, SH],
      ): fr.Out = tc.fromElems(a, listT, shape)
      def fromElems[Arrs <: HList, SH <: HList](listT: List[T])(implicit 
        sh: Shape[A[T]] { type Out = SH },
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
        sh: Shape[A[T]] { type Out = SH }, 
        ga: GetArrsAsc[A, _T, HNil] { type Out = GAOut },
        fr: FromElems[_T, GAOut, SH] { type Out = Option[A[_T]] },
      ): A[_T] = tc.map(a, f)
    }
  }

  trait ReduceDT[A[_], T, DM <: Nat] {
    type Out
    def apply(a: A[T], combine: List[T] => T): Out
  }
  object ReduceDT {
    type Aux[A[_], T, DM <: Nat, O] = ReduceDT[A, T, DM] { type Out = O }
    def instance[A[_], T, DM <: Nat, O](f: (A[T], List[T] => T) => O): Aux[A, T, DM, O] = 
    new ReduceDT[A, T, DM] {
      type Out = O
      def apply(a: A[T], combine: List[T] => T): Out = f(a, combine)
    }
    def apply[A[_], T, DM <: Nat](implicit se: ReduceDT[A, T, DM]): Aux[A, T, DM, se.Out] = se

    implicit def ifDeGt2[
      A[_], T, DE <: Nat, DM <: Nat, FSH <: HList, FLF <: HList, LF <: HList, RG <: HList, 
      AR <: HList, SH <: HList, Out
    ] (implicit 
      aIsArr: IsArray[A, T],
      de: DepthCT.Aux[A[T], DE],
      e0: GT[DE, Nat._1],
      rd: ReduceToListDT[A, T, DM],
      sh: Shape.Aux[A[T], FSH],
      sp: Split.Aux[FSH, Succ[DM], FLF, RG],
      dr: Drop.Aux[FLF, Nat._1, LF],
      ga: GetArrsAsc.Aux[A, T, HNil, AR],
      pr: Prepend.Aux[LF, RG, SH],
      fe: FromElemsDT.Aux[T, AR, SH, Nat._0, Option[Out]], 
      dm: ToInt[DM],
    ): Aux[A, T, DM, Out] = instance((a, cmb) => {
      val lst: List[T] = rd(a, cmb)
      println(s"LIST ${lst}")
      val dim = dm()
      val (lf, rg) = sp(sh(a))
      val shape: SH = dr(lf) ++ rg
      println(s"SHAPE ${shape}")
      val arrO = fe(lst, shape)
      arrO.get
    })

    //implicit def ifDeLt3[
      //A[_], T, DE <: Nat, DM <: Nat, FSH <: HList, FLF <: HList, LF <: HList, RG <: HList, AR <: HList
    //] (implicit 
      //aIsArr: IsArray[A, T],
      //de: DepthCT.Aux[A[T], DE],
      //e0: GT[Nat._3, DE],
      //rd: ReduceToListDT[A, T, DM],
      //sh: Shape.Aux[A[T], FSH],
      //e1: At
      //ga: GetArrsAsc.Aux[A, T, HNil, AR],
      //fe: FromElemsDT[T, AR, LF :: RG, Nat._0], 
      //dm: ToInt[DM],
    //): Aux[A, T, DM, fe.Out] = instance((a, cmb) => {
      //val lst: List[T] = rd(a, cmb)
      //val dim = dm()
      //val (lf, rg) = sp(sh(a))
      //val shape: LF :: RG = dr(lf) :: rg
      //fe(lst, shape)
    //})
  }

  trait ReduceToListDT[A[_], T, DM <: Nat] {
    type Out = List[T]
    def apply(a: A[T], combine: List[T] => T): Out
  }
  object ReduceToListDT {
    type Aux[A[_], T, DM <: Nat] = ReduceToListDT[A, T, DM]
    def instance[A[_], T, DM <: Nat](f: (A[T], List[T] => T) => List[T]): Aux[A, T, DM] = 
    new ReduceToListDT[A, T, DM] {
      def apply(a: A[T], combine: List[T] => T): Out = f(a, combine)
    }
    def apply[A[_], T, DM <: Nat](implicit se: Aux[A, T, DM]): Aux[A, T, DM] = se

    implicit def ifDMIs0AndSIs2dPlus[A[_], T, _S[_]] (implicit 
      aIsArr: IsArray[A, T] { type S = _S[T] },
      fl: Flatten[_S, T],
    ): Aux[A, T, Nat._0] = instance((a, cmb) => {
      val lst2d: List[List[T]] = aIsArr.toList(a).map(fl(_))
      lst2d.transpose.map(cmb(_))
    })
    
    implicit def ifDMIs0AndSIs1d[A[_], T] (implicit 
      aIsArr: IsArray[A, T] { type S = T },
    ): Aux[A, T, Nat._0] = instance((a, cmb) => {
      List(cmb(aIsArr.toList(a)))
    })

    implicit def ifDMGt0[A[_], T, _S[_], DM <: Nat, DMm1 <: Nat](implicit 
      aIsArr: IsArray[A, T] { type S = _S[T] },
      e1: GT[DM, Nat._0],
      pr: Pred.Aux[DM, DMm1],
      rd: ReduceToListDT[_S, T, DMm1],
    ): Aux[A, T, DM] = instance((a, cmb) => {
      val lst: List[List[T]] = aIsArr.toList(a).map(rd(_, cmb))
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
      (a, ref, elem) => ia.setAtN(a, ref.head, elem)
    )

    implicit def ifHListIntIsArr[A[_], T, _S[_], R1p <: HList](implicit
      ia: IsArray[A, T] { type S = _S[T] },
      iaS: SetElem[_S, T, R1p],
    ): Aux[A, T, Int :: R1p] = instance((a, ref, elem) => {
      val newS = iaS.apply(ia.getAtN(a, ref.head), ref.tail, elem)
      ia.setAtN(a, ref.head, newS)
    })
  }

  sealed trait ApplyIndex[A, IDX] {
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
      aSh: ShapeRT[A[T], HNil],
      mSh: ShapeRT[A[Boolean], HNil],
      aFl: Flatten[A, T] { type Out = List[T] },
      mFl: Flatten[A, Boolean] { type Out = List[Boolean] },
    ): Aux[A[T], A[Boolean], Option[List[T]]] = instance((a, r) =>
      if(aSh(a, HNil) == mSh(r, HNil)) { 
        val lstT: List[T] = aFl(a)
        val lstBl: List[Boolean] = mFl(r)
        Some(lstT.zip(lstBl).flatMap{ case(t, bl) => Option.when(bl)(t) })
      } else {None}
    )
    
    implicit def ifIdxIsInt[A[_], T, _S](implicit
      aIsArr: IsArray[A, T] { type S = _S }
    ): Aux[A[T], Int, _S] = instance((a, r) => aIsArr.getAtN(a, r))

    implicit def ifIdxIsListInt[A[_], T](implicit
      aIsArr: IsArray[A, T],
    ): Aux[A[T], List[Int], A[T]] = instance((a, rs) => aIsArr.fromList(
      rs.map(aIsArr.getAtN(a, _)))
    )

    implicit def ifIdxIsHList[
      A[_], T, AllArrs <: HList, Idx <: HList, IntsIdx <: HList, IntsN <: Nat, AllArrsN <: Nat,
      TakeN <: Nat, RdArrs <: HList, RevRdArrs <: HList,
    ](implicit 
      ga: GetArrsAsc.Aux[A, T, HNil, AllArrs],
      fl: Filter.Aux[Idx, Int, IntsIdx],
      lf: Length.Aux[IntsIdx, IntsN],
      la: Length.Aux[AllArrs, AllArrsN],
      di: NatDiff.Aux[AllArrsN, IntsN, TakeN],
      dr: Take.Aux[AllArrs, TakeN, RdArrs],
      re: Reverse.Aux[RdArrs, RevRdArrs],
      gi: ApplyIndexDT[A[T], Idx, RevRdArrs],
    ): Aux[A[T], Idx, gi.Out] = instance((a, r) => gi(a, r)) 
  }

  sealed trait PrettyPrint[A[_], T] {
    type Out = String
    def apply(a: A[T], indO: Option[String] = None): Out
  }
  object PrettyPrint {
    def instance[A[_], T](
      f: (A[T], Option[String]) => String,
    ): PrettyPrint[A, T] = new PrettyPrint[A, T] {
      def apply(a: A[T], indO: Option[String]): String = f(a, indO)
    }
    def apply[A[_], T](implicit pp: PrettyPrint[A, T]): PrettyPrint[A, T] = pp

    def maxWidth[A[_], T](a: A[T])(implicit 
      aIsArr: IsArray[A, T],
      fl: Flatten[A, T],
    ): Int = 
      aIsArr.flatten(a).map(_.toString.length).max

    implicit def ifIs1d[A[_], T](implicit 
      aIsArr: IsArray[A, T],
      de: DepthCT.Aux[A[T], Nat._1],
      fl: Flatten[A, T],
    ): PrettyPrint[A, T] = instance((a, indO) => {
      val mW = maxWidth(a)
      "[" ++ aIsArr.toList(a).map(_.toString.padTo(mW, ' ')).mkString(", ") ++ "]"
    })
    implicit def ifIs1dp[A[_], T, _S[_], DE <: Nat](implicit 
      aIsArr: IsArray[A, T] { type S = _S[T] },
      de: DepthCT.Aux[A[T], DE],
      deIsGt2: GT[DE, Nat._1],
      toInt: ToInt[DE],
      fl: Flatten[A, T],
      pp: PrettyPrint[_S, T],
    ): PrettyPrint[A, T] = instance((a, indO) => {
      val ind = indO.getOrElse(" ")
      val nextInd = ind ++ " "
      val lineB = "," ++ "\n" * (toInt()-1) ++ ind
      val ls: List[_S[T]] = aIsArr.toList(a)
      "[" ++ pp(ls.head, Some(nextInd)) ++ lineB ++ ls.tail.map(pp(_, Some(nextInd))).mkString(lineB) ++ "]"
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
    def apply[A, B](implicit cs: CombineShapes[A, B]): Aux[A, B, cs.Out] = cs
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
    def apply[A[_], T, L <: HList](implicit ga: GetArrsDesc[A, T, L]): Aux[A, T, L, ga.Out] = ga
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
    def apply[A[_], T, L <: HList](implicit ga: GetArrsAsc[A, T, L]): Aux[A, T, L, ga.Out] = ga
    implicit def ifLIsHList[A[_], T, L <: HList, Asc <: HList]( implicit 
      ga: GetArrsDesc[A, T, L] { type Out = Asc },
      rv: Reverse[Asc],
    ): Aux[A, T, L, rv.Out] = instance(l => rv(ga(l)))
  }

  sealed trait DepthCT[A] { self =>
    type Out <: Nat
  }
  object DepthCT {
    type Aux[A, O <: Nat] = DepthCT[A] { type Out = O }
    def apply[A](implicit de: DepthCT[A]): Aux[A, de.Out] = de
    implicit def ifArr[A[_], T, O <: Nat, Arrs <: HList](implicit 
      ar: GetArrsDesc.Aux[A, T, HNil, Arrs],
      le: Length.Aux[Arrs, O],
    ): Aux[A[T], O] = new DepthCT[A[T]] { type Out = O }
  }

  sealed trait Shape[A] { self =>
    type Out
    def apply(a: A): Out
  }
  object Shape {
    type Aux[A, O] = Shape[A] { type Out = O }
    def instance[A, O](f: A => O): Aux[A, O] = new Shape[A] {
      type Out = O
      def apply(a: A): Out = f(a)
    }
    def apply[A](implicit sh: Shape[A]): Aux[A, sh.Out] = sh
    //implicit def ifFullyTyped[A[_], T](implicit ft: FullyTyped[A[T]]) = ???
    implicit def ifShapeRT[A](implicit sh: ShapeRT[A, HNil]): Aux[A, sh.Out] = 
      instance(a => sh(a, HNil))
  }

  sealed trait ShapeRT[A, L <: HList] {self =>
    type Out <: HList
    def apply(a: A, l: L): Out
  }
  object ShapeRT {
    type Aux[A, L <: HList, O <: HList] = ShapeRT[A, L] { type Out = O }
    def instance[A, L <: HList, O <: HList](f: (A, L) => O): Aux[A, L, O] = 
    new ShapeRT[A, L] { 
      type Out = O 
      def apply(a: A, l: L): Out = f(a, l)
    }
    def apply[A, L <: HList](implicit sh: ShapeRT[A, L]): Aux[A, L, sh.Out] = sh
    implicit def gsIfSIsEle[A[_], T, _S, L <: HList, O <: HList](implicit 
      aIsABs: IsArrBase[A[T], T] { type S = T },
      rv: Reverse[Int :: L] { type Out = O },
    ): Aux[A[T], L, O] = instance((a, l) => rv(aIsABs.length(a) :: l))
    implicit def gsIfSIsArr[A[_], T, S0[_], L <: HList](implicit 
      aIsABs: IsArrBase[A[T], T] { type S = S0[T] },
      gsForS: ShapeRT[S0[T], Int :: L],
      ): Aux[A[T], L, gsForS.Out] = instance((a, l) => 
        gsForS(aIsABs.getAtN(a, 0), aIsABs.length(a) :: l)
      )
  }

  trait FromElems[T, Arrs <: HList, SH <: HList] {
    type Out
    def apply(l: List[T], sh: SH): Out
  }
  object FromElems {
    type Aux[T, Arrs <: HList, SH <: HList, O] = FromElems[T, Arrs, SH] { type Out = O }
    def instance[T, Arrs <: HList, SH <: HList, O](f: (List[T], SH) => O): Aux[T, Arrs, SH, O] = 
    new FromElems[T, Arrs, SH] {
      type Out = O
      def apply(l: List[T], sh: SH): Out = f(l, sh)
    }
    def apply[T, Arrs <: HList, SH <: HList](implicit fe: FromElems[T, Arrs, SH]): Aux[T, Arrs, SH, fe.Out] = fe
    implicit def ifListT[T, Arrs <: HList, SH <: HList, RSH <: HList, O]( implicit 
      fr: FromElemsDT[T, Arrs, SH, Nat._0],
    ): Aux[T, Arrs, SH, fr.Out] = instance((l, sh) => fr(l, sh))
  }

  trait FromElemsDT[X, Arrs <: HList, SH <: HList, INIT <: Nat] {
    type Out <: Option[Any]
    def apply(l: List[X], sh: SH): Out
  }
  object FromElemsDT {
    type Aux[X, Arrs <: HList, SH <: HList, INIT <: Nat, O <: Option[_]] = 
      FromElemsDT[X, Arrs, SH, INIT] { type Out = O }
    def instance[T, Arrs <: HList, SH <: HList, INIT <: Nat, O <: Option[_]](
      f: (List[T], SH) => O
    ): Aux[T, Arrs, SH, INIT, O] = new FromElemsDT[T, Arrs, SH, INIT] {
      type Out = O
      def apply(l: List[T], sh: SH): Out = f(l, sh)
    }
    def apply[T, Arrs <: HList, SH <: HList, INIT <: Nat](
      implicit fe: FromElemsDT[T, Arrs, SH, INIT],
    ): FromElemsDT.Aux[T, Arrs, SH, INIT, fe.Out] = fe

    implicit def ifShapeIsHNil[X, Arrs <: HList, INIT <: Nat]: Aux[X, Arrs, HNil, INIT, Option[X]] = instance(
      (l, sh) => {
        if(l.length == 1){Some(l(0))} else {None}
      }
    )

    implicit def ifShapeIsNotHNil[_S, A0[_], T, A1p <: HList, SH1p <: HList, NxtO](implicit 
      a0IsArr: IsArray[A0, T] { type S = _S },
      feForA1: FromElemsDT.Aux[A0[T], A1p, SH1p, Nat._1, Option[NxtO]],
    ): Aux[_S, A0[T] :: A1p, Int :: SH1p, Nat._1, Option[NxtO]] = instance(
      (l, sh) => {
        val thisA: A0[T] = a0IsArr.getEmpty
        val h1Nil = Nil: List[A0[T]]
        val combinedS: Option[List[A0[T]]] = combineS[A0, T, _S](thisA, h1Nil, l, sh.head)
        combinedS.flatMap(
          a1s => feForA1(a1s, sh.tail)
        )
      }
    )

    implicit def ifInitial[A0[_], T, A1p <: HList, SH <: HList, RSH <: HList, NxtO](implicit 
      a0IsArr: IsArray[A0, T] { type S = T },
      rv: Reverse.Aux[SH, RSH],
      fe: FromElemsDT.Aux[T, A0[T] :: A1p, RSH, Nat._1, Option[NxtO]],
    ): Aux[T, A0[T] :: A1p, SH, Nat._0, Option[NxtO]] = instance(
      (l, sh) => fe(l, rv(sh))
    )

    def combineS[A[_], T, _S](
      aEmpty: A[T], as: List[A[T]], l: List[_S], width: Int,
    )(implicit 
      aIsArr: IsArray[A, T] { type S = _S },
    ): Option[List[A[T]]] = l.length match {
      case 0 => Some(as.reverse)
      case x if x >= width => {
        val (ths, rst) = l.splitAt(width)
        val thsA: A[T] = ths.reverse.foldLeft(aEmpty)((s, o) => aIsArr.cons(s, o))
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
      aIsArr: IsArray[A, T] { type S = T },
    ): Flatten[A, T] = instance(a => aIsArr.toList(a))
    implicit def flattenIfSIsNotT[A[_], T, _S[T]](implicit 
      aIsArr: IsArray[A, T] { type S = _S[T] },
      sIsArr: IsArray[_S, T], 
      sFl: Flatten[_S, T],
    ): Flatten[A, T] = instance(a => aIsArr.toList(a).map(sIsArr.flatten(_)).flatten) 
  }

  trait AddRT[A[_], B[_], T] { self =>
    type Out = Option[A[T]]
    def apply(a: A[T], b: B[T]): Out
  }
  object AddRT {
    type Aux[A[_], B[_], T] = AddRT[A, B, T]
    def apply[A[_], B[_], T](implicit ad: AddRT[A, B, T]): Aux[A, B, T] = ad
    def instance[A[_], B[_], T](f: (A[T], B[T]) => Option[A[T]]): Aux[A, B, T] = 
    new AddRT[A, B, T] { 
      def apply(a: A[T], b: B[T]): Option[A[T]] = f(a, b)
    }
    implicit def ifSameType[A[_], T, SA <: HList, SB <: HList, SH <: HList](implicit 
      isArr: IsArray[A, T],
      aSh: ShapeRT.Aux[A[T], HNil, SA],
      bSh: ShapeRT.Aux[A[T], HNil, SB],
      cs: CombineShapes.Aux[SA, SB, Option[SH]]
    ): Aux[A, A, T] = instance((a, b) => 
      cs(aSh(a, HNil), bSh(b, HNil), 0).map(_ => isArr.fromList(isArr.toList(a) ++ isArr.toList(b)))
    )
    implicit def ifDiffType[A[_], B[_], T, Arrs <: HList, SA <: HList, SB <: HList, SH <: HList](implicit
      flA: Flatten[A, T],
      flB: Flatten[B, T],
      ga: GetArrsAsc.Aux[A, T, HNil, Arrs],
      aSh: Shape.Aux[A[T], SA],
      bSh: Shape.Aux[B[T], SB],
      cs: CombineShapes.Aux[SA, SB, Option[SH]],
      fe: FromElems.Aux[T, Arrs, SH, Option[A[T]]],
    ): Aux[A, B, T] = instance((a, b) => 
      cs(aSh(a), bSh(b), 0).flatMap(sh => fe(flA(a) ++ flB(b), sh))
    )
  }

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
      aIsArr: IsArray[A, T] { type S = B[T] },
      bIsArr: IsArray[B, T] { type S = BS },
    ): Aux[A[T], XA, XB] = instance(a => {
      val lst2d: List[List[BS]] = aIsArr.toList(a).map(bIsArr.toList(_))
      val lstB: List[B[T]] = lst2d.transpose.map(lstBs => bIsArr.fromList(lstBs))
      aIsArr.fromList(lstB)
    })

    implicit def ifCurrDimIsNotXA[A[_], T, _S[_], XA <: Nat, XB <: Nat, _XA <: Nat, _XB <: Nat](implicit
      e1: GT[XB, XA],
      e2: GT[XA, Nat._0],
      _xa: Pred.Aux[XA, _XA],
      _xb: Pred.Aux[XB, _XB],
      aIsArr: IsArray[A, T] { type S = _S[T] },
      trForS: TransAxDT[_S[T], _XA, _XB],
    ): Aux[A[T], XA, XB] = instance(a => aIsArr.fromList(aIsArr.toList(a).map(trForS(_))))
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

  trait TransposeDT[A, IN] {
    type Out = A
    def apply(a: A): Out
  }
  object TransposeDT {
    type Aux[A, IN] = TransposeDT[A, IN]
    def apply[A, IN](implicit tr: TransposeDT[A, IN]): Aux[A, IN] = tr
    def instance[A, IN](f: A => A): Aux[A, IN] = new TransposeDT[A, IN] { 
      def apply(a: A): A = f(a)
    }

    implicit def ifNil[A, DE <: Nat, DEm1 <: Nat](implicit
      de: DepthCT.Aux[A, DE],
      e1: Pred.Aux[DE, DEm1],
      tr: TransAllDT[A, Nat._0, DEm1],
    ): Aux[A, AllSlice] = instance(a => tr(a))

    implicit def ifTupleNat[A, XA <: Nat, XB <: Nat](implicit
      tr: TransAxDT[A, XA, XB],
    ): Aux[A, (XA, XB)] = instance(a => tr(a))
  }
  
  trait ConcatenateCT[A[_], B[_], T, D <: Nat] { self =>
    type Out = Option[A[T]]
    def apply(a: A[T], b: B[T]): Out
  }
  object ConcatenateCT {
    def apply[A[_], B[_], T, D <: Nat](implicit cn: ConcatenateCT[A, B, T, D]): ConcatenateCT[A, B, T, D] = cn
    implicit def ifDim0[A[_], B[_], T](implicit 
      ad: AddRT.Aux[A, B, T],
    ): ConcatenateCT[A, B, T, Nat._0] = new ConcatenateCT[A, B, T, Nat._0] { 
      def apply(a: A[T], b: B[T]): Out = ad(a, b)
    }
    implicit def ifNotDim0[A[_], B[_], T, D <: Nat, Dm1 <: Nat, _SA[_], _SB[_]](implicit
      aIsArr: IsArray[A, T] { type S = _SA[T] },
      bIsArr: IsArray[B, T] { type S = _SB[T] },
      dIsGt0: GT[D, Nat._0],
      sAIsArr: IsArray[_SA, T],
      sBIsArr: IsArray[_SB, T],
      dm1: Pred.Aux[D, Dm1],
      sConc: ConcatenateCT[_SA, _SB, T, Dm1] { type Out = Option[_SA[T]] },
    ): ConcatenateCT[A, B, T, D] = new ConcatenateCT[A, B, T, D] {
      def apply(a: A[T], b: B[T]): Out = {
        val cs = for((sA, sB) <- aIsArr.toList(a).zip(bIsArr.toList(b))) yield (sConc(sA, sB))
        if(cs.forall(_.isDefined)) {Some(aIsArr.fromList(cs.map(_.get)))} else {None}
      }
    }
  }

  trait ConcatenateRT[A[_], B[_], T] { self =>
    type Out = Option[A[T]]
    def apply(a: A[T], b: B[T], dim: Int): Out
  }
  object ConcatenateRT {
    type Aux[A[_], B[_], T] = ConcatenateRT[A, B, T]
    def apply[A[_], B[_], T](implicit cn: ConcatenateRT[A, B, T]): ConcatenateRT[A, B, T] = cn
    def instance[A[_], B[_], T](f: (A[T], B[T], Int) => Option[A[T]],
    ): Aux[A, B, T] = new ConcatenateRT[A, B, T] {
      def apply(a: A[T], b: B[T], dim: Int): Option[A[T]] = f(a, b, dim)
    }
    implicit def ifNoSubConc[A[_], B[_], T](implicit 
      aIsArr: IsArray[A, T] { type S = T },
      bIsArr: IsArray[B, T] { type S = T },
      cnCt: ConcatenateCT[A, B, T, Nat._0],
    ): Aux[A, B, T] = instance((a, b, dim) => 
      if(dim == 0) {
        cnCt(a, b)
      } else {None}
    )
    implicit def ifSubConc[A[_], B[_], T, _SA[_], _SB[_]](implicit 
      aIsArr: IsArray[A, T] { type S = _SA[T] },
      bIsArr: IsArray[B, T] { type S = _SB[T] },
      sAIsArr: IsArray[_SA, T],
      sBIsArr: IsArray[_SB, T],
      ad: AddRT[A, B, T],
      sConc: Aux[_SA, _SB, T],
    ): Aux[A, B, T] = instance((a, b, dim) => 
      if(dim == 0) {
        ad(a, b)
      } else {
        val cO: List[Option[_SA[T]]] = for(
          (sA, sB) <- aIsArr.toList(a).zip(bIsArr.toList(b))
        ) yield (sConc(sA, sB, dim-1))
        if(cO.forall(_.isDefined)){Some(aIsArr.fromList(cO.map(_.get)))} else {None}
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

    implicit def ifArray[A[_], T, Arrs <: HList, SH <: HList](implicit 
      sh: Shape.Aux[A[T], SH],
      ga: GetArrsAsc.Aux[A, T, HNil, Arrs],
      fe: FromElemsDT[T, Arrs, SH, Nat._0] { type Out = Option[A[T]] }, 
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

  trait MaskFromNumSeqDT[A, R <: HList] {
    type Out = A
    def apply(ref: R, mask: A): Out
  }
  object MaskFromNumSeqDT {
    type Aux[A, R <: HList] = MaskFromNumSeqDT[A, R] { type Out = A }
    def instance[A, R <: HList](f: (R, A) => A): Aux[A, R] = new MaskFromNumSeqDT[A, R] { 
      def apply(ref: R, mask: A): A = f(ref, mask)
    }
    def apply[A, R <: HList](implicit ma: MaskFromNumSeqDT[A, R]): Aux[A, R] = ma 

    implicit def ifRefIsHNil[A]: Aux[A, HNil] = instance((r, m) => m)

    implicit def ifHeadIsListIntNotBase[A[_], _S, R1p <: HList, DE <: Nat](implicit
      aIsArr: IsArray[A, Boolean] { type S = _S },
      de: DepthCT.Aux[A[Boolean], DE],
      deGt1: GT[DE, Nat._1],
      maskS: MaskFromNumSeqDT[_S, R1p], 
    ): Aux[A[Boolean], List[Int] :: R1p] = instance((r, m) => {
      val newS = for((s, i) <- aIsArr.toList(m).zipWithIndex) yield (
        if (r.head.contains(i)) {maskS(r.tail, s)} else {s}
      )
      aIsArr.fromList(newS)
    })

    implicit def ifHeadIsListIntIsBase[A[_]](implicit
      aIsArr: IsArray[A, Boolean] { type S = Boolean },
    ): Aux[A[Boolean], List[Int] :: HNil] = instance((r, m) => {
      val newS = for((s, i) <- aIsArr.toList(m).zipWithIndex) yield (
        if (r.head.contains(i)) {true} else {false}
      )
      aIsArr.fromList(newS)
    })
  }

  trait ApplyIndexDT[A, R, Arrs] {
    type Out
    def apply(a: A, ref: R): Out
  }
  object ApplyIndexDT {
    type Aux[A, R, Arrs, O] = ApplyIndexDT[A, R, Arrs] { type Out = O }
    def instance[A, R, Arrs, O](f: (A, R) => O): Aux[A, R, Arrs, O] = new ApplyIndexDT[A, R, Arrs] { 
      type Out = O
      def apply(a: A, ref: R): Out = f(a, ref)
    }
    def apply[A, R, Arrs](implicit ai: ApplyIndexDT[A, R, Arrs]): Aux[A, R, Arrs, ai.Out] = ai

    implicit def ifHeadIsInt[A[_], T, _S, R1p <: HList, Arrs <: HList, O](implicit
      aIsArr: IsArray[A, T] { type S = _S },
      iLocS: ApplyIndexDT[_S, R1p, Arrs], 
    ): Aux[A[T], Int :: R1p, Arrs, iLocS.Out] = instance((a, r) => 
      iLocS(aIsArr.getAtN(a, r.head), r.tail)
    )
    implicit def ifHeadIsListInt[A[_], T, _S, R1p <: HList, A0[_], A1p <: HList, SO](implicit
      aIsArr: IsArray[A, T] { type S = _S },
      iLocS: ApplyIndexDT[_S, R1p, A1p] { type Out = SO }, 
      outIsArr: IsArray[A0, T] { type S = SO }
    ): Aux[A[T], List[Int] :: R1p, A0[T] :: A1p, A0[T]] = instance((a, r) => {
      val origS: List[_S] = r.head.map(aIsArr.getAtN(a, _))
      val locedS: List[SO] = origS.map(iLocS(_, r.tail))
      outIsArr.fromList(locedS)
    })

    implicit def ifRefIsHNil[A, Arrs <: HList]: Aux[A, HNil, Arrs, A] = instance((a, r) => a)
  }

  abstract class GetLoc[A[_], T, R] {
    def loc(self: A[T], ref: R): A[T] = ???
  }

  abstract class SetILoc[A[_], T, R] {
    def loc(self: A[T], ref: R): A[T] = ???
  }
}

