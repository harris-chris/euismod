package sportarray

import scala.collection.mutable.HashMap
import com.github.nscala_time.time.Imports._

import sportdate.{IsSportDateInstances, IsSportDateSyntax}

object Skeleton {

  sealed trait DataType {
    type ElemT 
  }
  trait PositionsData extends DataType { type ElemT = Double }
  trait WeightsData extends DataType { type ElemT = Double }
  trait ValuesData extends DataType { type ElemT = Double }
  trait PricesData extends DataType { type ElemT = Double }

  trait IsIdxElem

  import sportdate.{IsSportDate => IsDate}
  type Date = IsDate[DateTime] with IsIdxElem

  import ArrayDefs.{IsBaseArr}
  import ArrayDefs.{IsDatum, Is1dIndexArr, Is2dIndexArr}
  type PricesTs = Is1dIndexArr[Date, PricesData]
  type HoldingsTs[T <: DataType] = Is2dIndexArr[Date, IsSecurity, T]
  type PositionsTs = HoldingsTs[PositionsData]

  trait SecurityName
  
  trait IsMetadata {
    val name: SecurityName
  }

  trait IsSecurity extends IsIdxElem {
    val metadata: IsMetadata
  }

  trait HasPrice {
    val priceTs: PricesTs 
  }

  trait IsDelta[T] {
    val deltas: HashMap[IsSecurity, T]
  }

  trait IsTsChange[T] {
    def isValidRoll(d: Date): Boolean
    val delta: IsDelta[T]
  }

  trait IsRebal {
    def triggersOn(d: Option[Date]): Option[Date]
    def getEffects(rs: Rebalances): (PositionsTs, List[IsRebal])
  }

  trait Rebalances extends IsSecurity {
    def addRebalance(r: IsRebal): Rebalances
  }

  trait IsComposite {
    val positionsTs: PositionsTs
  }
}
