module Tests

open System
open Xunit
open LifeRules
//open Expecto

[<Fact>]
let ``Degenerate cases`` () =
    let zeroHeightColony : CellState[,] = array2D []
    Assert.True ((timeStep zeroHeightColony) = zeroHeightColony)

    let zeroWidthColony = Array2D.create 5 0 Dead
    Assert.True ((timeStep zeroWidthColony) = zeroWidthColony)

    // this could be a good property-based test, actually, all of these fit in the same property category
    let emptyColony = Array2D.create<CellState> 5 5 CellState.Dead
    Assert.Equal (emptyColony, (timeStep emptyColony))

[<Fact>]
let OneCell () = 
    let colony = array2D [[Alive]]
    let expectedColony = array2D [[Dead]]
    Assert.Equal (expectedColony, (timeStep colony))

[<Fact>]
let InsufficientNeighborsToLive () = 
    let colony = array2D [[Alive; Alive]]
    let expectedColony = array2D [[Dead; Dead]]
    Assert.Equal (expectedColony, (timeStep colony))

  
[<Fact>]
let SufficientNeighborsToLive () = 
    let colony = array2D [[Alive; Alive; Alive]]
    let expectedColony = array2D [[Dead; Alive; Dead]]
    Assert.Equal (expectedColony, (timeStep colony))

[<Fact>]
let NewCellBorn () = 
    let colony = array2D [[Alive; Alive; Alive]
                          [Dead; Dead; Dead]]
    let expectedColony = array2D [[Dead; Alive; Dead]
                                  [Dead; Alive; Dead]]
    Assert.Equal (expectedColony, (timeStep colony))

[<Fact>]
let TooManyNeighborsToLive () = 
    let colony = array2D [[Dead; Alive; Dead]
                          [Alive; Alive; Alive]
                          [Dead; Alive; Dead]]
    let expectedColony = array2D [[Alive; Alive; Alive]
                                  [Alive; Dead; Alive]
                                  [Alive; Alive; Alive]]
    Assert.Equal (expectedColony, (timeStep colony))


//[<Tests>]
//let meow = 
//    testList "DegenerateTests" [
//        test "named" { Expect.isTrue true "yo" }
//    ]
