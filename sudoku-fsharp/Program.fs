open System
open System.IO
open System.Collections.Generic
open FParsec
open FSharpx.Collections

let vecNth vec idx =
    PersistentVector.nth idx vec

type Puzzle = int PersistentVector
let puzzles = 
    let lbl = pstring "Grid" .>> spaces >>. pint32 .>> spaces
    let row = parray 9 digit |>> List.ofArray |>> List.map (int << string) 
    let rows = parray 9 (row .>> spaces) |>> List.concat
    let ppuzzle = lbl >>. rows
    let pfile = many1 ppuzzle .>> eof
    match runParserOnFile pfile () "..\..\sudoku.txt" System.Text.Encoding.ASCII with
    | Success(result, _, _) -> 
        result |> List.map PersistentVector.ofSeq |> PersistentVector.ofSeq
    | Failure(errorMsg, _, _) -> 
        failwith errorMsg    

let puzzle2str (puzzle:Puzzle) =
    for i in 0..puzzle.Length - 1 do
        printf "%d %s" puzzle.[i]  (if i % 9 = 8 then "\n" else "" )
    printf "\n"

// dbg:
// let (~~) (func:'a-> unit) (arg:'a) = (func arg) |> fun () -> arg

let rowValues (puzzle:Puzzle) (idx:int) = 
    let row = idx / 9
    [0..8] |> List.map (fun col -> col + (row * 9))
    //|> ~~ ((printfn) "Idxs: %A")
    |> List.map (vecNth puzzle) // (fun idx -> puzzle.[idx])
    //|> ~~ ((printfn) "Values: %A")
    |> List.filter ((<>) 0)

let colValues (puzzle:Puzzle) (idx:int) = 
    let col = idx % 9
    [0..8] |> List.map (fun row -> col + (row * 9))
    |> List.map (vecNth puzzle)
    |> List.filter ((<>) 0)

let subGridIdxs (idx:int) = 
    let col = idx % 9
    let row = idx / 9
    let scol = col / 3 * 3
    let srow = row / 3 * 3
    seq { for r in 0..2 do 
            for c in 0..2 do
                yield scol + c + (srow + r) * 9} |> List.ofSeq

let subGridValues (puzzle:Puzzle) (idx:int) =
    subGridIdxs(idx) 
    |> List.map (fun idx -> puzzle.[idx]) 
    |> List.filter (fun v -> v <> 0)

let possByIdx (puzzle:Puzzle) (idx:int) =
    match puzzle.[idx] with
    | 0  -> 
        let taken = colValues puzzle idx @ rowValues puzzle idx @ subGridValues puzzle idx
        [1..9] |> List.filter (fun v -> not (List.exists ((=) v) taken))
    | x  -> [x]

let isSolved : Puzzle -> bool = 
    not << Seq.exists ((=) 0)

let hasValue (a: 'a option) = if a.IsSome then true else false

let firstValue (xs : IEnumerable<Option<'t>>) : Option<'t> =
    let xs2 =  Seq.cache <| seq { for x in xs do if x.IsSome then yield x}
    if Seq.isEmpty xs2 then
        None
    else
        Seq.head xs2    

type possWithIdx = { poss: int list; idx: int }
let noPossibilities = {poss = [1..9]; idx = -1}

// Like normal fold, except returns without going through whole collection if goodEnoughF returns true when called with the results of comb
let rec fold2 combf memo goodEnoughF xs = 
    match xs with
    | [] -> memo
    | x::xs -> 
        let memo = combf memo x
        if goodEnoughF memo then memo
        else fold2 combf memo goodEnoughF xs    

let rec solve (puzzle:Puzzle) : Option<Puzzle> = 
    let unfilledIdxs = Seq.filter (vecNth puzzle >> (=) 0) [0..80]
    let allPoss = Seq.map (fun idx -> {poss = possByIdx puzzle idx; idx = idx}) unfilledIdxs |> List.ofSeq
    let best = fold2 
                (fun memo x -> if  x.poss.Length < memo.poss.Length then x else memo) 
                noPossibilities 
                (fun best ->  best.poss.Length <= 1) 
                allPoss
    assert (best.idx <> noPossibilities.idx)

    if best.poss.Length = 0 then
        None
    else    
        let possPuzzles = best.poss |> Seq.map (fun v -> PersistentVector.update best.idx v puzzle)
        match Seq.tryFind isSolved possPuzzles with
        | None -> firstValue <| (Seq.map solve possPuzzles)
        | x -> x

printfn "Parsed %d puzzles!" puzzles.Length
for (p, i) in Seq.zip puzzles (Seq.initInfinite ((+) 1)) do
    printfn "Puzzle %d:" i
    puzzle2str p
    match solve p with
    | None -> do
        printf("Failed to solve!\n\n")
        exit(1)
    | Some(p) -> do
        printf("Solution:\n")
        puzzle2str(p)

printfn "Finished! Press [enter] to exit;"
System.Console.ReadLine() |> ignore
exit(0)