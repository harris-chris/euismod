Sport-array is a typeclass wrapper to provide a standardized, indexed, typed interface over any array-like data structure.
_**standardized**_ - a consistent API is provided regardless of the underlying type.  

_**indexed**_ - indices are stored for the array axes, providing a Pandas Dataframe-like interface

_**typed**_ - the data type, index type, and dimensionality of the array is known at all times, allowing us to prevent mistakes at compile-time (eg, adding percentage weights to absolute numbers).

The package's prelude includes typeclasses for array types: `Is1dIndexArr`, `Is2dIndexArr`, up to 5 dimensions.

*2020/09/09*: None of the below is yet true. This project is a work-in-progress.

For example:
```scala
import sportarray._
// The UnitType trait is a way to ensure that we do not mix incompatible unit types
trait Numbers extends UnitType { type ElemT = Int }
trait Grams extends UnitType { type ElemT = Int }

// a list of lists with a couple of indices can be a 2-d array...
// an instance of the sportarray.Is2dIndexArr typeclass is already imported for list of lists
case class ListOfListsWithIndices[T <: UnitType, Idx0, Idx1](
  values: List[List[T#ElemT]],
  indices: (List[Idx0], List[Idx1]),
) 

// and here's an instance of it.
val numbersOfFruitIIntendToBuy = ListOfListsWithIndices[Numbers, LocalDate, String](
  values=List(List(0, 1, 0), List(1, 0, 2)),
  indices=(
    List(LocalDate.of(2021,1,1), LocalDate.of(2020,1,2)), 
    List("Apple", "Orange", "Tomato, technically"),
  )
)

// very similar: an array of arrays can be a 2-d array
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
// but in a UnitType-aware way; the following will produce a compile error
val neverMixYourUnits = numbersOfFruitIIntendToBuy + gramsOfPowdersIAlsoWillBuy
// getting an element will return an array of one lesser dimension
val arr1d = numbersOfFruitIIntendToBuy.loc("Orange") // arr1d implements the Is1dIndexArr typeclass
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
```

How does this help humanity with its current problems? *Glad you asked*
1. Array types can be hot-swapped, or used in a mixed fashion in the code.
2. Strictly typing the arrays reduces the likelihood of error
3. Retaining index and UnitType information for the arrays allows many common array operations (joins, merges, dot products) to be done with a trivially simple API
