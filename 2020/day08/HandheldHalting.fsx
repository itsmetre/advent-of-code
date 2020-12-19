open System
open System.IO

module HandheldHalting =
    type Instruction =
        | Nop of int
        | Acc of int
        | Jmp of int

    type Program = Instruction array

    type Address = int
    
    type History = Map<Address, int>

    type State =
        { PC : int
          Acc : int
          History : History }


    let toInstruction (input: string) : Instruction =
        match input.Split(' ') with
            | [| instruction; arg |] ->
                let n = int arg
                match instruction with
                    | "acc" -> Acc n
                    | "jmp" -> Jmp n
                    | _     -> Nop n

            | _ -> failwith "Invalid input"


    let toProgram (lines: string array) : Program = 
        Array.map toInstruction lines


    let loopDetected (state: State) : bool =
        let { PC = pc; History = history } = state
        Map.exists (fun _ executionCount -> executionCount > 1) history


    let markAsVisited (addr: Address) (history: History) : History =
        match Map.tryFind addr history with
            | None -> Map.add addr 1 history
            | Some count -> Map.add addr (count + 1) history


    let changeInstructionAt (addr: Address) (program: Program) : Program =
        let lhs = Array.take addr program

        let newInstruction = 
            match program.[addr] with
            | Nop n -> Jmp n
            | Jmp n -> Nop n
            | Acc n -> Acc n

        let rhs = Array.skip (addr + 1) program

        Array.concat [| lhs; [| newInstruction |]; rhs |]

        
    let rec run (program: Program) (initialState: State) : State =
        let { PC = pc; Acc = acc; History = history } = initialState
        
        let state = { initialState with History = markAsVisited pc history }
        
        if loopDetected state || pc >= program.Length then
            state
        else
            let instruction = program.[pc]
            match instruction with
                | Nop _ ->
                    let state' = { state with PC = pc + 1 }
                    run program state'
            
                | Acc n ->
                    let pc' = pc + 1
                    let acc' = acc + n
                    let state' = { state with PC = pc'; Acc = acc' }
                    run program state'

                | Jmp n ->
                    let pc' = pc + n
                    let state' = { state with PC = pc' }
                    run program state'
                         

    let partOne (program: Program) =
        run program { PC = 0; Acc = 0; History = Map.empty }
        |> printfn "%O"


    let partTwo (program: Program) =
        let initialState = { PC = 0; Acc = 0; History = Map.empty }

        let isSwappable = function
            | Nop n | Jmp n -> true
            | _             -> false
        
        let swappable =
            program
            |> Array.mapi (fun i x -> (i, x))
            |> Array.filter (snd >> isSwappable)
            |> Array.map fst

        let states = seq {
                for addr in swappable do
                    let program' = changeInstructionAt addr program
                    let state' = run program' initialState
                    if not (loopDetected state') then
                        yield state'
            } 
        
        states 
        |> Seq.head
        |> printfn "%O"


    let runExample () =
        let path = Path.Combine(__SOURCE_DIRECTORY__, "input.txt")
        let lines = File.ReadAllLines(path)
        let program = toProgram lines
        partOne program
        partTwo program

