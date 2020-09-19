package sportarray

import Skeleton.{DataType, PositionsData, ValuesData, WeightsData, PricesData}
import Skeleton.IsIdxElem
import IndicesObj.Index

import java.time.LocalDate

object test {

  sealed trait UnitType {
    type ElemT 
  }
  
  /// here 

  
  // the datatype trait is to ensure we do not do operations on non-compatible arrays 
  //import sportarray.unittype
  //import sportarray._
  //trait numbers extends unittype { type elemt = int }
  //trait grams extends unittype { type elemt = int }

  //case class listoflistswithindices[t <: unittype, idx0, idx1](
    //values: list[list[t#elemt]],
    //indices: (list[idx0], list[idx1]),
  //) 

  //val numbersoffruitiintendtobuy = listoflistswithindices[numbers, localdate, string](
    //values=list(list(0, 1, 0), list(1, 0, 2)),
    //indices=(
      //list(localdate.of(2021,1,1), localdate.of(2020,1,2)), 
      //list("apple", "orange", "tomato, technically"),
    //)
  //)

  //case class arrayofarraywithindices[t <: unittype, idx0, idx1](
    //values: array[array[t#elemt]],
    //indices: (list[idx0], list[idx1]),
  //)

  //val gramsofpowdersialsowillbuy = arrayofarraywithindices[grams, localdate, string](
    //values=array(array(100, 130, 150), array(210, 0, 50)),
    //indices=(
      //list(localdate.of(2021,1,1), localdate.of(2020,1,2)), 
      //list("sugar", "salt", "sumac"),
    //)
  //)

  // consistent setter, getter and other functions are provided automatically
  //println(gramsofpowdersialsowillbuy.loc("2020-01-01", "salt")) // 130, of course
  //val moaroranges = numbersoffruitiintendtobuy.setloc(("2020-01-02", "orange"), 30)
  //// addition is handled automatically...
  //val doublethatfruitorder = numbersoffruitiintendtobuy + numbersoffruitiintendtobuy 
  //// but in a datatype-aware way; the following will produce a compile error
  //val dontmixyourunits = numbersoffruitiintendtobuy + gramsofpowdersialsowillbuy
  //// thanks to the marvels of typing, arithmetic between array types can be handled automatically
  //trait euros extends unittype { type elemt = double }
  //case class singleindexedlist[t <: unittype, idx0](
    //values: list[t#elemt],
    //indices: list[idx0],
  //)
  //val eurospergram = singleindexedlist[euros, string](
    //values=list(0.01, 0.015, 0.02, 2400.0),
    //indices=list("sugar", "salt", "sumac", "radium"),
  //)
  //val totalcostofmypowders = gramsofpowdersialsowillbuy * eurospergram
}
