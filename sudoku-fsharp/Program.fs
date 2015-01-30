open System.IO
open System.Collections.Generic

type Puzzle = int list
let puzzles = 
    use sr = new StreamReader ("..\..\sudoku.txt")
    let mutable puzzle = Array.create 81 0
    let puzzles = new System.Collections.Generic.List<Puzzle>()
    let mutable idx = 0
    while not sr.EndOfStream do
        let line = sr.ReadLine()
        if not (line.StartsWith "Grid") then
            for c in line.ToCharArray() do
                puzzle.[idx] <- System.Int32.Parse(c.ToString())
                idx <- idx + 1
                if idx = 81 then
                    let immutablePuzzle = List.ofArray puzzle
                    puzzles.Add(immutablePuzzle)
                    puzzle <- Array.create 81 0 
                    idx <- 0
    puzzles

let puzzle2str (puzzle:Puzzle) =
    for i in 0..puzzle.Length - 1 do
        printf "%d %s" puzzle.[i]  (if i % 9 = 8 then "\n" else "" )
    printf "\n"

let (~~) (func:'a-> unit) (arg:'a) = (func arg) |> fun () -> arg

let rowValues (puzzle:Puzzle) (idx:int) = 
    let row = idx / 9
    [0..8] |> List.map (fun col -> col + (row * 9))
    //|> ~~ ((printfn) "Idxs: %A")
    |> List.map (List.nth puzzle) // (fun idx -> puzzle.[idx])
    //|> ~~ ((printfn) "Values: %A")
    |> List.filter ((<>) 0)

let colValues (puzzle:Puzzle) (idx:int) = 
    let col = idx % 9
    [0..8] |> List.map (fun row -> col + (row * 9))
    |> List.map (List.nth puzzle)
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
    not << List.exists ((=) 0)

// Need cached sequence or disaster. GG FSharp standard library designer. Of course I want to pattern match with head and tail on my sequences.
let (|SeqEmpty|SeqCons|) (xs: 'a seq) = 
  if Seq.isEmpty xs then SeqEmpty
  else SeqCons(Seq.head xs, Seq.skip 1 xs)

let hasValue (a: 'a option) = if a.IsSome then true else false

let rec firstValue (seq : IEnumerable<Option<'t>>) : Option<'t> =
    match seq with 
    | SeqEmpty -> None
    | SeqCons(x, xs) -> match x with
        | None -> firstValue xs
        | Some(xx) -> Some(xx)       

type possWithIdx = { poss: int list; idx: int }
let noPossibilities = {poss = [1..9]; idx = -1}

let rec solve (puzzle:Puzzle) : Option<Puzzle> = 
    let unfilledIdxs = Seq.filter (List.nth puzzle >> (=) 0) [0..80]
    let allPoss = Seq.map (fun idx -> {poss = possByIdx puzzle idx; idx = idx}) unfilledIdxs |> List.ofSeq

    let best = Seq.fold (fun memo x -> if  x.poss.Length < memo.poss.Length then x else memo) noPossibilities allPoss
    // TODO: make a version of fold that can short circuit. (Want to short circuit on 0 or 1 possibilities here.)
    assert (best.idx <> noPossibilities.idx)

    if best.poss.Length = 0 then
        None
    else    
        let possPuzzles = Seq.cache (Seq.map (fun v -> 
            let mutable tmp = Array.ofList puzzle
            tmp.[best.idx] <- v
            List.ofArray(tmp) : Puzzle
        ) best.poss)

        match Seq.tryFind isSolved possPuzzles with
        | None -> firstValue <| (Seq.cache <| Seq.map solve possPuzzles)
        | x -> x

printfn "we now have %d puzzles!" puzzles.Count
for (p, i) in Seq.zip puzzles [1..70]  do
    printfn "Puzzle %d:" i
    puzzle2str p
    match solve p with
    | None -> do
        printf("Failed to solve!\n\n")
        exit(1)
    | Some(p) -> do
        printf("Solution:\n")
        puzzle2str(p)