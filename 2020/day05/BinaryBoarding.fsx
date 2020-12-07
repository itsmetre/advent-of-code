open System
open System.IO

module BinaryBoarding =
    type State = { PossibleRows : int * int; PossibleColumns : int * int }

    let rec findSeat (state: State) (directions: char list) =
        match directions with
        | [] -> state
        
        | 'F' :: rest ->
            let (x, y) = state.PossibleRows
            let size = y - x + 1
            let state' = { state with PossibleRows = (x, y - (size / 2)) }
            findSeat state' rest
        
        | 'B' :: rest ->
            let (x, y) = state.PossibleRows
            let size = y - x + 1
            let state' = { state with PossibleRows = (x + (size / 2), y) }
            findSeat state' rest
        
        | 'L' :: rest ->
            let (x, y) = state.PossibleColumns
            let size = y - x + 1
            let state' = { state with PossibleColumns = (x, y - (size / 2)) }
            findSeat state' rest

        | 'R' :: rest ->
            let (x, y) = state.PossibleColumns
            let size = y - x + 1
            let state' = { state with PossibleColumns = (x + (size /2), y) }
            findSeat state' rest

        | _ :: rest -> findSeat state rest


    let toSeatId state =
        let row = state.PossibleRows |> fst
        let col = state.PossibleColumns |> fst
        8 * row + col


    let toCharList (s: string) = s.ToCharArray() |> Array.toList

    let seatIds state boardingPasses =
        let indexSeatId map boardingPass =
            let seatId = (toCharList >> findSeat state >> toSeatId) boardingPass
            Map.add seatId seatId map

        boardingPasses
        |> Array.fold indexSeatId Map.empty


    let partOne seats =
        seats
        |> Map.fold (fun acc key _ -> max acc key) 0
        |> printfn "Highest seat id: %d"

     
    let partTwo seats =
        let emptySeats = seq {
            for row in 0 .. 127 do
                for col in 0 .. 7 do
                    let seatId = row * 8 + col
                    if not (Map.containsKey seatId seats) then
                        yield seatId
        }

        let mySeat =
            emptySeats
            |> Seq.pairwise
            |> Seq.filter (fun (x, y) -> y <> (x + 1))
            |> Seq.map snd
            |> Seq.head

        printfn "My seat: %O" mySeat


    let runExample() =
        let path = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
        let initialState = { PossibleRows = (0, 127); PossibleColumns = (0, 7) }
        let seats = File.ReadAllLines(path) |> seatIds initialState
        partOne seats
        partTwo seats