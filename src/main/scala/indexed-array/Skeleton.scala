package sportarray

import scala.collection.mutable.HashMap
import com.github.nscala_time.time.Imports._

import sportdate.{IsSportDate, IsSportDateInstances, IsSportDateSyntax, SportDate}
import sportdate.IsSportDateInstances._
import sportdate.IsSportDateSyntax._

object Skeleton {

  type DateType = DateTime

  sealed trait DataType {
    type ElemT 
  }
  trait PositionsData extends DataType { type ElemT = Double }
  trait WeightsData extends DataType { type ElemT = Double }
  trait ValuesData extends DataType { type ElemT = Double }
  trait PricesData extends DataType { type ElemT = Double }

  trait IsIdxElem[A]
  object IsIdxElemImplicits {
    implicit val securityIsIdxElem = new IsIdxElem[IsSecurity] {}
    implicit val dateTimeIsIdxElem = new IsIdxElem[DateType] {}
  }

  import ArrayDefs.{IsBaseArr}
  import ArrayDefs.{IsDatum, Is1dIndexArr, Is2dIndexArr}
  type PricesTs = Is1dIndexArr[DateType, PricesData]
  type HoldingsTs[T <: DataType] = Is2dIndexArr[DateType, IsSecurity, T]
  type PositionsTs = HoldingsTs[PositionsData]

  trait SecurityName
  
  trait IsMetadata {
    val name: SecurityName
  }

  trait IsSecurity extends IsIdxElem[IsSecurity] {
    val metadata: IsMetadata
  }

  trait HasPrice {
    val priceTs: PricesTs 
  }

  trait IsDelta[T] {
    val deltas: HashMap[IsSecurity, T]
  }

  trait IsTsChange[T] {
    def isValidRoll(d: DateType): Boolean
    val delta: IsDelta[T]
  }

  trait IsRebal {
    def triggersOn(d: Option[DateType]): Option[DateType]
    def getEffects(rs: Rebalances): (PositionsTs, List[IsRebal])
  }

  trait Rebalances extends IsSecurity {
    def addRebalance(r: IsRebal): Rebalances
  }

  trait IsComposite {
    val positionsTs: PositionsTs
  }
}
