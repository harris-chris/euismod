package sportarray

import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.IsIdxElem
import IndicesObj.Index

import java.time.LocalDate

object test {

  sealed trait UnitType {
    type ElemT 
  }
  
  /// HERE 

  
  // the DataType trait is to ensure we do not do operations on non-compatible arrays 
  //import sportarray.UnitType
  //import sportarray._
  trait Numbers extends UnitType { type ElemT = Int }
  trait Grams extends UnitType { type ElemT = Int }

  case class ListOfListsWithIndices[T <: UnitType, Idx0, Idx1](
    values: List[List[T#ElemT]],
    indices: (List[Idx0], List[Idx1]),
  ) 

  val numbersOfFruitIIntendToBuy = ListOfListsWithIndices[Numbers, LocalDate, String](
    values=List(List(0, 1, 0), List(1, 0, 2)),
    indices=(
      List(LocalDate.of(2021,1,1), LocalDate.of(2020,1,2)), 
      List("Apple", "Orange", "Tomato, technically"),
    )
  )

  case class ArrayOfArrayWithIndices[T <: UnitType, Idx0, Idx1](
    values: Array[Array[T#ElemT]],
    indices: (List[Idx0], List[Idx1]),
  )

  val gramsOfPowdersIAlsoWillBuy = ArrayOfArrayWithIndices[Grams, LocalDate, String](
    values=Array(Array(100, 130, 150), Array(210, 0, 50)),
    indices=(
      List(LocalDate.of(2021,1,1), LocalDate.of(2020,1,2)), 
      List("Sugar", "Salt", "Sumac"),
    )
  )

  // consistent setter, getter and other functions are provided automatically
  println(gramsOfPowdersIAlsoWillBuy.loc("2020-01-01", "Salt")) // 130, of course
  val moarOranges = numbersOfFruitIIntendToBuy.setLoc(("2020-01-02", "Orange"), 30)
  // addition is handled automatically...
  val doubleThatFruitOrder = numbersOfFruitIIntendToBuy + numbersOfFruitIIntendToBuy 
  // but in a DataType-aware way; the following will produce a compile error
  val dontMixYourUnits = numbersOfFruitIIntendToBuy + gramsOfPowdersIAlsoWillBuy
  // thanks to the marvels of typing, arithmetic between array types can be handled automatically
  trait Euros extends UnitType { type ElemT = Double }
  case class SingleIndexedList[T <: UnitType, Idx0](
    values: List[T#ElemT],
    indices: List[Idx0],
  )
  val eurosPerGram = SingleIndexedList[Euros, String](
    values=List(0.01, 0.015, 0.02, 2400.0),
    indices=List("Sugar", "Salt", "Sumac", "Radium"),
  )
  val totalCostOfMyPowders = gramsOfPowdersIAlsoWillBuy * eurosPerGram
}
