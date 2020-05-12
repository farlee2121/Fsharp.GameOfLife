module LifeRules

//rules <= 1 neighbor, cell dies, >= 4 neighbors dies, 3 neighbors, becomes populated

type CellState = Alive | Dead
type Colony = CellState[,]

module Array2D = 
  let toArray (array2D: 'T [,]) = array2D |> Seq.cast<'T> |> Seq.toArray

// Rejected coordinate generation comb 2 [-1;0;1;-1;0;1]
//let rec comb n l = 
//    match n, l with
//    | 0, _ -> [[]]
//    | _, [] -> []
//    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let isInBounds i j colony = 
    let isValidVerticalIndex = 0 <= i && i < Array2D.length1 colony
    let isValidHorizontalIndex =  0 <= j && j < Array2D.length2 colony
    isValidVerticalIndex && isValidHorizontalIndex

let isCellAlive i j (colony: Colony) = 
    colony.[i,j] = Alive

let countNeighbors i j (colony: Colony) = 
    Array2D.init 3 3 (fun i' j' ->
        match (i + (i'-1), j+(j'-1)) with
        | (i'',j'') when i'' = i && j'' = j -> false
        | (iFinal, jFinal) -> (isInBounds iFinal jFinal colony) && (isCellAlive iFinal jFinal colony) // this feels wrong how could I track this a more compositional approach
        )
    |> Array2D.toArray 
    |> Array.filter ((=) true) |> Array.length

    //let neighborIndexes = (Array2D.init 3 3 (fun i' j' -> (i'-1, j'-1)) 
    //    |> Array2D.toArray 
    //    |> Array.filter (fun t -> match t with
    //        | (0,0) -> false
    //        | _ -> true)
    //) // would use mapi from here

    
    

let stepCell i j (colony : Colony) = 
    match (countNeighbors i j colony) with
    | n when n < 2 -> Dead
    | 2 -> colony.[i,j]
    | 3 -> Alive
    | n when n > 3 -> Dead
    | n -> failwithf "Impossible number of cell neighbors %i" n

let timeStep (colony: Colony) : Colony =
    // question, should I pass neighbors here instead of the whole colony?
    Array2D.mapi (fun i j value -> stepCell i j colony) colony

