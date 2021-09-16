module Tests

open System
open Xunit
open LifeRules
//open Expecto


module Colony = 
    
    let ofStateArray (stateArray:CellState list list) : Colony =  stateArray |> array2D |> Array2D.map (fun state -> { State = state }) |> D2

[<Fact>]
let ``Degenerate cases`` () =
    let zeroHeightColony = Colony.create2D 0 0 { State = Dead }
    Assert.True ((timeStep zeroHeightColony) = zeroHeightColony)

    let zeroWidthColony = Colony.create2D 5 0 { State = Dead }
    Assert.True ((timeStep zeroWidthColony) = zeroWidthColony)

    // this could be a good property-based test, actually, all of these fit in the same property category
    let emptyColony = Colony.create2D 5 5 { State = CellState.Dead }
    Assert.Equal (emptyColony, (timeStep emptyColony))

[<Fact>]
let OneCell () = 
    let colony = Colony.ofStateArray [[Alive]]
    let expectedColony = Colony.ofStateArray [[Dead]]
    Assert.Equal (expectedColony, (timeStep colony))

[<Fact>]
let InsufficientNeighborsToLive () = 
    let colony = Colony.ofStateArray [[Alive; Alive]]
    let expectedColony = Colony.ofStateArray [[Dead; Dead]]
    Assert.Equal (expectedColony, (timeStep colony))

  
[<Fact>]
let SufficientNeighborsToLive () = 
    let colony = Colony.ofStateArray [[Alive; Alive; Alive]]
    let expectedColony = Colony.ofStateArray [[Dead; Alive; Dead]]
    Assert.Equal (expectedColony, (timeStep colony))

[<Fact>]
let NewCellBorn () = 
    let colony = Colony.ofStateArray [[Alive; Alive; Alive]
                                      [Dead; Dead; Dead]]
    let expectedColony = Colony.ofStateArray [[Dead; Alive; Dead]
                                              [Dead; Alive; Dead]]
    Assert.Equal (expectedColony, (timeStep colony))

[<Fact>]
let TooManyNeighborsToLive () = 
    let colony = Colony.ofStateArray
                         [[Dead; Alive; Dead]
                          [Alive; Alive; Alive]
                          [Dead; Alive; Dead]]
    let expectedColony = Colony.ofStateArray
                                [[Alive; Alive; Alive]
                                 [Alive; Dead; Alive]
                                 [Alive; Alive; Alive]]
    Assert.Equal (expectedColony, (timeStep colony))


//[<Tests>]
//let meow = 
//    testList "DegenerateTests" [
//        test "named" { Expect.isTrue true "yo" }
//    ]
