module TobogganTrajectory =
    open System.IO

    type Slope = int * int
    type Index = int * int
    type TreeMap = (char array) array

    let traverseUsingSlope<'state> (slope: Slope)
                                   (f: 'state -> Index -> char -> 'state)
                                   (initialState: 'state)
                                   (treeMap: TreeMap)
                                   =
        let w = (Array.head >> Array.length) treeMap
        let h = Array.length treeMap

        let rec traverse acc slope f idx (treeMap: TreeMap) =
            let (dx, dy) = slope
            let (col, row) = idx

            let col' = (col + dx) % w
            let row' = row + dy
            let idx' = (col', row')

            if row' >= h then
                acc
            else
                let acc' = f acc idx' treeMap.[row'].[col']
                traverse acc' slope f idx' treeMap

        traverse initialState slope f (0, 0) treeMap


    let howManyTrees slope treeMap =
        let countTrees acc _ = function
            | '#' -> acc + 1u
            | _   -> acc

        traverseUsingSlope slope countTrees 0u treeMap


    let partOne treeMap =
        treeMap
        |> howManyTrees (3, 1)
        |> printfn "Part 1: found %d trees in the path"


    let partTwo slopes treeMap =
        slopes
        |> List.map (fun slope -> howManyTrees slope treeMap)
        |> List.fold (*) 1u
        |> printfn "Part 2: %d"


    let runExample () =
        let path = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
        
        let treeMap = File.ReadAllLines(path) 
                      |> Array.map (fun line -> line.ToCharArray())
        
        partOne treeMap

        let slopes = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)] 
        partTwo slopes treeMap