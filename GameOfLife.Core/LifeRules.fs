module LifeRules

//rules <= 1 neighbor, cell dies, >= 4 neighbors dies, 3 neighbors, becomes populated

type CellState = Alive | Dead
type Colony = CellState[,]

module Colony = 
  let toArray (array2D: 'T [,]) = array2D |> Seq.cast<'T> |> Seq.toArray
  let mapi = Array2D.mapi 

// Rejected coordinate generation comb 2 [-1;0;1;-1;0;1]
//let rec comb n l = 
//    match n, l with
//    | 0, _ -> [[]]
//    | _, [] -> []
//    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let isInBounds colony i j  = 
    let isValidVerticalIndex = 0 <= i && i < Array2D.length1 colony
    let isValidHorizontalIndex =  0 <= j && j < Array2D.length2 colony
    isValidVerticalIndex && isValidHorizontalIndex

let isCellAlive i j (colony: Colony) = 
    colony.[i,j] = Alive

let getNeighbors i j (colony: Colony) = 
    let isInColony (i',j') = isInBounds colony i' j'
    let neighborIndexes = (Array2D.init 3 3 (fun i' j' -> (i'-1, j'-1)) 
        |> Colony.toArray 
        |> Array.filter (fun t ->
            match t with
            | (0,0) -> false
            | _ -> true
        )
        |> Array.map (fun (iOffset,jOffset) -> (i+iOffset, j+jOffset))
        |> Array.filter isInColony
    )
    neighborIndexes |> Array.map (fun (i',j') -> colony.[i', j'])

    
    

let stepCell (currentState: CellState) (neighbors : CellState seq) = 
    let livingNeighbors = neighbors |> Seq.filter ((=) Alive) |> Seq.length
    match (livingNeighbors) with
    | n when n < 2 -> Dead
    | 2 -> currentState
    | 3 -> Alive
    | n when n > 3 -> Dead
    | n -> failwithf "Impossible number of cell neighbors %i" n

let timeStep (colony: Colony) : Colony =
    // question, should I pass neighbors here instead of the whole colony?
    colony |> Colony.mapi (fun i j cellState -> 
        stepCell cellState (getNeighbors i j colony)
        ) 
