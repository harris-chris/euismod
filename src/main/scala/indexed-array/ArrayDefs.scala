package sportarray

import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.IsIdxElem
import IndicesObj.Index

import java.time.LocalDate
import shapeless._
import shapeless.ops.hlist._

object ArrayDefs {

  abstract class IsSpArr[A, T <: DataType, I0: IsIdxElem] {
    type Self = A
    type M1
    def getIdx(self: A): Index[I0]
    def getElem(self: A, i: Int): M1
    def getNil(self: A): A
    def iloc[R](self: A, r: R)(implicit iLoc: ILoc[R, A, T, I0]): iLoc.Out = iLoc.iloc(self, r)
    def ::(self: A, other: (I0, M1)): A
  }

  abstract class Is1dSpArr[A, T <: DataType, I0: IsIdxElem] extends IsSpArr[A, T, I0] {
    type M1 = T#T
    //def getElem(self: A, i: Int): T#T
    //def iloc[R](self: A, r: R)(implicit iLocTc: ILoc[R, A, T]): iLocTc.Out = iLocTc.iloc(self, r)
    //def loc[R](self: A, r: R)(implicit locTc: Loc[R]): locTc.Out = locTc.loc(self, r)
    //def head(self: A): (I0, T#T) = (indices(self)(0), getElem(self, 0))
    //def shape(self: A): Int = indices(self).length
    //def tail(self: A): A = self match {
      //case s if shape(self) <= 1 => getNil(s)
      //case _ => ILocTC.iLocTCForList.iloc(self, (1 to shape(self)).toList)
    //}

    //def unapply(self: A): Option[((I0, DT#T), A)] = self match {
      //case s if shape(self) == 0 => None
      //case _ => Some(head(self), tail(self)) 
    //}

    //trait ILocTC[R] {
      //type Out
      //def iloc(self: A, ref: R): Out 
    //}
    //object ILocTC {
      //implicit val iLocTCForInt = new ILocTC[Int] { 
        //type Out = DT#T
        //def iloc(self: A, ref: Int): Out = getElem(self, ref)
      //}
      //implicit val iLocTCForList = new ILocTC[List[Int]] { 
        //type Out = Self
        //def iloc(self: A, ref: List[Int]): Out = {
          //val data: List[DT#T] = ref.map(getElem(self, _)).toList
          //val idx: Index[I0] = Index(ref.map(indices(self)(_)))
          //idx.toList.zip(data).foldLeft(getNil(self))((a, b) => ::(a, (b._1, b._2))) 
        //}
      //}
      //implicit val iLocTCForNull = new ILocTC[Null] { 
        //type Out = Self
        //def iloc(self: A, ref: Null): Out = self
      //}
    //}

    //trait LocTC[R] {
      //type Out
      //def loc(self: A, ref: R): Out
    //}
    //object LocTC {
      //type Aux[A0, B0] = ILocTC[A0] { type Out = B0 }
      //implicit val locTCForI0 = new LocTC[I0] { 
        //type Out = Option[DT#T]
        //def loc(self: A, ref: I0): Out = indices(self).indexOf(ref).map(getElem(self, _))
      //}
      //implicit val locTCForListI0 = new LocTC[List[I0]] { 
        //type Out = Option[List[DT#T]]
        //def loc(self: A, ref: List[I0]): Out = ref.flatMap(indices(self).indexOf(_)) match {
          //case is if is.length == ref.length => Some(is.map(getElem(self, _)))
          //case _ => None
        //}
      //}
    //}
  }

  trait ILoc[R, A, T <: DataType, I0] {
    type Out
    def iloc(self: A, ref: R): Out
  }
  object ILoc {
    type Aux[A0, R0, T0 <: DataType, I0, O0] = ILoc[A0, R0, T0, I0] { type Out = O0 }
    implicit def iLocForInt[A, T <: DataType, I0: IsIdxElem](implicit isArr: IsSpArr[A, T, I0]) = new ILoc[Int, A, T, I0] { 
      type Out = isArr.M1
      def iloc(self: A, ref: Int): Out = isArr.getElem(self, ref)
    }
    implicit def iLocForList[A, T <: DataType, I0: IsIdxElem](implicit isArr: IsSpArr[A, T, I0]) = new ILoc[List[Int], A, T, I0] { 
      type Out = A
      def iloc(self: A, ref: List[Int]): Out = {
        val data: List[isArr.M1] = ref.map(isArr.getElem(self, _)).toList
        val idx0: Index[I0] = isArr.getIdx(self)
        val newIdx: Index[I0] = Index(ref.map(idx0(_)))
        idx0.toList.zip(data).foldLeft(isArr.getNil(self))((a, b) => isArr.::(a, (b._1, b._2))) 
      }
    }
    //implicit val iLocForNull = new ILoc[Null] { 
      //type Out = Self
      //def iloc(self: A, ref: Null): Out = self
    //}
  }

  object Is1dSpArrSyntax {
    implicit class Is1dSpArrOps[A, T <: DataType, I0](self: A)(implicit val tc1d: IsSpArr[A, T, I0] {type M1 = T#T}) {
      //def indices = tc1d.indices(self)
      def getElem(i: Int) = tc1d.getElem(self, i)
      def iloc[R](r: R)(implicit iLoc: ILoc[R, A, T, I0]) = tc1d.iloc(self, r)
      //def loc[R](r: R)(implicit locTc: tc1d.LocTC[R]) = tc1d.loc(self, r)
      def getNil = tc1d.getNil(self)
      def ::(other: (I0, tc1d.M1)): A = tc1d.::(self, other)
      //def shape: Int = tc1d.shape(self)
      //def unapply: Option[((I0, T#T), A)] = tc1d.unapply(self) 
    }
  }

  //abstract class Is2dSpArr[A, I0: IsIdxElem, I1: IsIdxElem, DT <: DataType, M1](implicit tc1d: Is1dSpArr[M1, I1, DT]) {
    //type Self = A
    //def indices(self: A): (Index[I0], Index[I1])
    //def getElem(self: A, i: Int): M1
    //def iloc[R, Out](self: A, r: R)(implicit iLocTc: ILocTC.Aux[R, Out], tc1d: Is1dSpArr[Out, I1, DT]): iLocTc.Out = iLocTc.iloc(self, r)
    //def iloc[R0, R1, Out](self: A, r0: R0, r1: R1)(implicit 
      //iLocTc0: ILocTC.Aux[R0, Out], 
      //iLocTc1: Is1dSpArr[Out, I1, DT]#ILocTC[R1]#Aux[R1, Out],
      //tc1d: Is1dSpArr[Out, I1, DT],
    //): Out = {
      //val o1: Out = iLocTc0.iloc(self, r0)
      //tc1d.iloc(o1, r1)(iLocTc1)
      ////implicitly[Is1dSpArr[Out, I1, DT]].iloc(o1, r1)(iLocTc1)
    //}
    ////def loc[R](self: A, r: R)(implicit locTc: LocTC[R]): locTc.Out = locTc.loc(self, r)
    //def getNil(self: A): A
    //def ::(self: A, other: (I0, M1)): A
    ////def head(self: A): (I0, DT#T) = (indices(self)(0), getElem(self, 0))
    //def shape(self: A): (Int, Int) = (indices(self)._1.length, indices(self)._2.length)
    ////def tail(self: A): A = self match {
      ////case s if shape(self) <= 1 => getNil(s)
      ////case _ => ILocTC.iLocTCForList.iloc(self, (1 to shape(self)).toList)
    ////}

    ////def unapply(self: A): Option[((I0, DT#T), A)] = self match {
      ////case s if shape(self) == 0 => None
      ////case _ => Some(head(self), tail(self)) 
    ////}

    //trait ILocTC[R] {
      //type Out
      //def iloc(self: A, r: R)(implicit iLocTc: ILocTC.Aux[R, Out], tc1d: Is1dSpArr[Out, I1, DT]): iLocTc.Out
    //}
    //object ILocTC {
      //type Aux[A0, B0] = ILocTC[A0] { type Out = B0 }
      //implicit val iLocTCForInt = new ILocTC[Int] { 
        //type Out = M1
        //def iloc(self: A, r: Int)(implicit iLocTc: ILocTC.Aux[Int, Out], tc1d: Is1dSpArr[Out, I1, DT]): iLocTc.Out = getElem(self, r)
      //}
      //implicit val iLocTCForList = new ILocTC[List[Int]] { 
        //type Out = Self
        //def iloc(self: A, ref: List[Int])(implicit iLocTc: ILocTC.Aux[List[Int], Out], tc1d: Is1dSpArr[Out, I1, DT]): Out = {
          //val data: List[M1] = ref.map(getElem(self, _)).toList
          //val idx: Index[I0] = Index(ref.map(indices(self)._1(_)))
          //idx.toList.zip(data).foldLeft(getNil(self))((a, b) => ::(a, (b._1, b._2))) 
        //}
      //}
      //implicit val iLocTCForNull = new ILocTC[Null] { 
        //type Out = Self
        //def iloc(self: A, r: Null)(implicit iLocTc: ILocTC.Aux[Null, Out], tc1d: Is1dSpArr[Out, I1, DT]): iLocTc.Out = 
          //self
      //}
    //}
  //}

  //object Is2dSpArrSyntax {
    //implicit class Is2dSpArrOps[A, I0, I1, T <: DataType, M1](self: A)(implicit 
      //val tc2d: Is2dSpArr[A, I0, I1, T, M1],
    //) {
      //def indices: (Index[I0], Index[I1]) = tc2d.indices(self)
      //def getElem(i: Int) = tc2d.getElem(self, i)
      //def iloc[R, Out](r: R)(implicit iLocTc: tc2d.ILocTC.Aux[R, Out], tc1d: Is1dSpArr[Out, I1, T]) = tc2d.iloc(self, r)
      ////def loc[R](r: R)(implicit locTc: tc2d.LocTC[R]) = tc2d.loc(self, r)
      //def ::(other:(I0, M1)): A = tc2d.::(self, other)
      //def shape: (Int, Int) = tc2d.shape(self)
      ////def unapply: Option[((I0, T#T), A)] = tc2d.unapply(self) 
    //}
  //}

    ////trait Get2dTC[I] {
      ////type DM1
      ////type Out = Get2dTC.MkOut[DM1]
      ////def apply(i: I): Out
    ////}
    ////object Get2dTC {
      ////type Aux[I, DM1i] = Get2dTC[I] { type DM1 = DM1i }
      ////type MkOut[DM1] = Option[Is1dSpArr[A, I, DM1, T]]
      ////def instance[I, DM1i](f: I => MkOut[DM1i]): Aux[I, DM1i] = new Get2dTC[I] {
        ////override type DM1 = DM1i
        ////override def apply(i: I): Out = f(i)
      ////}
    
      //////implicit val I0Type: Aux[I0, I1] = instance(i => i.iloc(i))
      //////implicit val I1Type: Aux[I1, I0] = instance(i => getDim1Slice(i))
    ////}
  //}
}

