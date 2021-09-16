module LifeRules

//rules <= 1 neighbor, cell dies, >= 4 neighbors dies, 3 neighbors, becomes populated

type CellState =
    | Alive
    | Dead

type CellPosition = | D2 of int * int // what does this buy me?
type Cell = { State: CellState }
type Colony = | D2 of Cell [,]
// should position belong to a cell?
// con: I either have double tracking or
// pro: enables sparse cell collections (could also have the iterator just ignore an "blocked" or "void" state)

module Colony =
    let toArray (array2D: 'T [,]) = array2D |> Seq.cast<'T> |> Seq.toArray

    let mapi (map: (CellPosition -> Cell -> Cell)) (colony: Colony): Colony =
        match colony with 
        | D2 d2Array -> Array2D.mapi (fun i j value -> map (CellPosition.D2(i, j)) value) d2Array |> D2

    let create2D height width value = Array2D.create<Cell> height width value |> D2
// question: is each cell a state monad?... Seems like it. How can I just use map and have

// Rejected coordinate generation comb 2 [-1;0;1;-1;0;1]
//let rec comb n l =
//    match n, l with
//    | 0, _ -> [[]]
//    | _, [] -> []
//    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs


let private isInBounds colony i j =
    let isValidVerticalIndex = 0 <= i && i < Array2D.length1 colony
    let isValidHorizontalIndex = 0 <= j && j < Array2D.length2 colony
    isValidVerticalIndex && isValidHorizontalIndex


let private getNeighbors2D (CellPosition.D2(i,j)) colony =
    let isInColony (i', j') = isInBounds colony i' j'

    let neighborIndexes =
        (Array2D.init 3 3 (fun i' j' -> (i' - 1, j' - 1))
         |> Colony.toArray
         |> Array.filter (fun t ->
             match t with
             | (0, 0) -> false
             | _ -> true)
         |> Array.map (fun (iOffset, jOffset) -> (i + iOffset, j + jOffset))
         |> Array.filter isInColony)

    neighborIndexes
    |> Array.map (fun (i', j') -> colony.[i', j'])
    |> List.ofArray    

let private getNeighbors position (colony: Colony) = 
    match colony with
    | D2 array2d -> getNeighbors2D position array2d


let private stepCell (cell: Cell) (neighbors: Cell list) =
    let livingNeighbors =
        neighbors
        |> Seq.map (fun cell -> cell.State)
        |> Seq.filter ((=) Alive)
        |> Seq.length

    let newState =
        match (livingNeighbors) with
        | n when n < 2 -> Dead
        | 2 -> cell.State
        | 3 -> Alive
        | n when n > 3 -> Dead
        | n -> failwithf "Impossible number of cell neighbors %i" n

    { State = newState }

let timeStep (colony: Colony): Colony =
    // question, should I pass neighbors here instead of the whole colony?
    colony
    |> Colony.mapi (fun position cell -> stepCell cell (getNeighbors position colony))

// how can I better encode my domain constraints with types...
// the biggest outstanding source of issues would be location in a colony

type Neighborhood = Neighbors of CellState list

// I still need to be able to access specific cells... i can give them identifiers or pass some position...
module Neighborhood =
    let Create (colony: Colony) i j = 0
