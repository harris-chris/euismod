package sportarray

import scala.collection.mutable.HashMap
import com.github.nscala_time.time.Imports._

import sportdate.{IsSportDate, IsSportDateInstances, IsSportDateSyntax, SportDate}

object Skeleton {

  sealed trait DataType {
    type ElemT 
  }
  trait PositionsData extends DataType { type ElemT = Double }
  trait WeightsData extends DataType { type ElemT = Double }
  trait ValuesData extends DataType { type ElemT = Double }
  trait PricesData extends DataType { type ElemT = Double }

  trait IsIdxElem[A]

  trait IsDate[A] extends IsSportDate[A] with IsIdxElem[A]

  implicit def dateIsIdxElem(a: DateTime) = new IsIdxElem[DateTime] {}
  implicit def securityIsIdxElem(a: IsSecurity) = new IsIdxElem[IsSecurity] {}

  object Date {
    def YMD(y: Int, m: Int, d: Int): DateTime =
      (new DateTime)
        .withYear(y).withMonthOfYear(m).withDayOfMonth(d)
        .withTimeAtStartOfDay() 
  }

  import ArrayDefs.{IsBaseArr}
  import ArrayDefs.{IsDatum, Is1dIndexArr, Is2dIndexArr}
  type PricesTs = Is1dIndexArr[IsDate[_], PricesData]
  type HoldingsTs[T <: DataType] = Is2dIndexArr[IsDate[_], IsSecurity, T]
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
    def isValidRoll(d: DateTime): Boolean
    val delta: IsDelta[T]
  }

  trait IsRebal {
    def triggersOn(d: Option[DateTime]): Option[DateTime]
    def getEffects(rs: Rebalances): (PositionsTs, List[IsRebal])
  }

  trait Rebalances extends IsSecurity {
    def addRebalance(r: IsRebal): Rebalances
  }

  trait IsComposite {
    val positionsTs: PositionsTs
  }
}
