package sportarray

import ArrayDefs._
import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.{IsIdxElem}
import IndicesObj._

object ListOfListsObj {

  case class Datum[I0: IsIdxElem, DataT <: DataType] (
    ref: I0,
    value: DataT#ElemT,
  )(implicit ev: Numeric[DataT#ElemT]) extends IsDatum[I0, DataT] {
    import ev._ 
  }
  
  case class Arr1d[I0: IsIdxElem, DataT <: DataType] (
    indices: (Index[I0]),
    data: List[DataT#ElemT],
  )(implicit ev: Numeric[DataT#ElemT]) extends Is1dIndexArr[I0, DataT] {
    def addDelta(delta: Self): Self = ??? 
    def addDeltaFrom(delta: Datum[I0, DataT]): Self = ??? 
    def loc(at: I0): Option[Datum[I0, DataT]] = 
      indices.indexOf(at).map(i => Datum(indices.vals(i), data(i)))
  }

  case class Arr2d[I0: IsIdxElem, I1: IsIdxElem, DataT <: DataType] (
    indices: (Index[I0], Index[I1]),
    data: List[List[DataT#ElemT]],
  )(implicit ev: Numeric[DataT#ElemT]) extends Is2dIndexArr[I0, I1, DataT] {
    //def addDelta(delta: SelfMinus1T): Self = ???
    def getDim0Slice(loc: I0): Option[Is1dIndexArr[I1, DataT]] = 
      indices._1.indexOf(loc).map(i => 
        Arr1d[I1, DataT]((indices._2), data(i))
      )
    def getDim1Slice(loc: I1): Option[Is1dIndexArr[I0, DataT]] = 
      indices._2.indexOf(loc).map(i => 
        Arr1d[I0, DataT]((indices._1), data.map(_(i)))
      )
  }

  case class Arr3d[I0: IsIdxElem, I1: IsIdxElem, I2: IsIdxElem, DataT <: DataType] (
    indices: (Index[I0], Index[I1], Index[I2]),
    data: List[List[List[DataT#ElemT]]],
  )(implicit ev: Numeric[DataT#ElemT]) extends Is3dIndexArr[I0, I1, I2, DataT] {
    //def addDelta(delta: SelfMinus1T): Self = ???
    def getDim0Slice(loc: I0): Option[Is2dIndexArr[I1, I2, DataT]] = 
      indices._1.indexOf(loc).map(i => 
        Arr2d[I1, I2, DataT]((indices._2, indices._3), data(i))
      )
    def getDim1Slice(loc: I1): Option[Is2dIndexArr[I0, I2, DataT]] = 
      indices._2.indexOf(loc).map(i => 
        Arr2d[I0, I2, DataT]((indices._1, indices._3), data.map(_(i)))
      )
    def getDim2Slice(loc: I2): Option[Is2dIndexArr[I0, I1, DataT]] = 
      indices._3.indexOf(loc).map(i => 
        Arr2d[I0, I1, DataT]((indices._1, indices._2), data.map(l1 => l1.map(_(i))))
        )
  }
}
