package sportarray

import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.IsIdxElem
import IndicesObj.Index

object ArrayDefs {

  sealed trait IsBaseArr[DataT <: DataType] { }

  abstract class IsDatum[DataT <: DataType] {
    def get: DataT#ElemT
    def set[INew: IsIdxElem](ref: INew): Is1dIndexArr[INew, DataT]
  }

  abstract class Is1dIndexArr[I0: IsIdxElem, DataT <: DataType] extends IsBaseArr[DataT] {
    type Self = Is1dIndexArr[I0, DataT]
    val indices: (Index[I0])
    def get(i: Int): IsDatum[DataT]
    def set[INew: IsIdxElem](ref: INew): Is2dIndexArr[INew, I0, DataT]    
    def length: Int

    def ++(a: Self): Self

    //def :+(ref: I0, datum: DataT#ElemT): Self
    //def addDim[INew: IsIdxElem](ref: INew): Is2dIndexArr[INew, I0, DataT]

    def loc(at: I0): Option[IsDatum[DataT]] = indices.indexOf(at).map(this.get(_))
  }

  abstract class Is2dIndexArr[I0: IsIdxElem, I1: IsIdxElem, DataT <: DataType] extends IsBaseArr[DataT] {
    val indices: (Index[I0], Index[I1])
    def get(i: Int): Is1dIndexArr[I1, DataT]
    //def set(i: Int): Is1dIndexArr[I1, DataT]
    def ++(a: Is2dIndexArr[I0, I1, DataT]): Is2dIndexArr[I0, I1, DataT]
    def :+(ref: I0, data: Is1dIndexArr[I1, DataT]): Is2dIndexArr[I0, I1, DataT]
    def length: Int

    //def map(f: Is1dIndexArr[I1, DataT] => DataT#ElemT): Is1dIndexArr[I0, DataT] = 
      //Range(0, this.length, 1).map(i => f(this(i)))

    def getDim0Slice(loc: I0): Option[Is1dIndexArr[I1, DataT]] = 
      indices._1.indexOf(loc).map(this.get(_))
    def getDim1Slice(loc: I1): Option[Is1dIndexArr[I0, DataT]] = {
      indices._2.indexOf(loc).map(
        iloc => {
          val arrs: Seq[Is1dIndexArr[I0, DataT]] = Range(0, this.length, 1).map(
            i => {
              val d: Is1dIndexArr[I0, DataT] = this.get(i).get(iloc).set(indices._1(i))
              d
            }
          )
          arrs.reduce(_ ++ _)
        }
      )
    }

    def loc[I](i: I)(implicit tc: LocTC2d[I]): tc.Out = tc(i)

    trait LocTC2d[I] {
      type Im1
      type Out = LocTC2d.MkOut[Im1]
      def apply(i: I): Out
    }
    object LocTC2d {
      type MkOut[Im1] = Option[Is1dIndexArr[Im1, DataT]]

      type Aux[I, Im1In] = LocTC2d[I] { type Im1 = Im1In }
      def instance[I, Im1In](f: I => MkOut[Im1In]): Aux[I, Im1In] = new LocTC2d[I] {
        override type Im1 = Im1In
        override def apply(i: I): Out = f(i)
      }
    
      implicit val I0Type: Aux[I0, I1] = instance(i => getDim0Slice(i))
      implicit val I1Type: Aux[I1, I0] = instance(i => getDim1Slice(i))
    }
  }

  abstract class Is3dIndexArr[
    I0: IsIdxElem, I1: IsIdxElem, I2: IsIdxElem, DataT <: DataType
  ] extends IsBaseArr[DataT] {
    type Self = Is3dIndexArr[I0, I1, I2, DataT]
    val indices: (Index[I0], Index[I1], Index[I2])
    def getDim0Slice(loc: I0): Option[Is2dIndexArr[I1, I2, DataT]]
    def getDim1Slice(loc: I1): Option[Is2dIndexArr[I0, I2, DataT]]
    def getDim2Slice(loc: I2): Option[Is2dIndexArr[I0, I1, DataT]]
    def loc[I](i: I)(implicit tc: LocTC3d[I]): tc.Out = tc(i)

    trait LocTC3d[I] {
      type B
      type C
      type Out = LocTC3d.MkOut[B, C]
      def apply(i: I): Out
    }
    object LocTC3d {
      type MkOut[B, C] = Option[Is2dIndexArr[B, C, DataT]]

      type Aux[I, B0, C0] = LocTC3d[I] { type B = B0; type C = C0 }
      def instance[I, B0, C0](f: I => MkOut[B0, C0]): Aux[I, B0, C0] = new LocTC3d[I] {
        override type B = B0
        override type C = C0
        override def apply(i: I): Out = f(i)
      }
    
      implicit val I0Type: Aux[I0, I1, I2] = instance(i => getDim0Slice(i))
      implicit val I1Type: Aux[I1, I0, I2] = instance(i => getDim1Slice(i))
      implicit val I2Type: Aux[I2, I0, I1] = instance(i => getDim2Slice(i))
    }
  }
}

