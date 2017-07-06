module TicTacToe

[<AutoOpen>]
module Domain =
    type Player = X=0 | O=1
    let opposite (p:Player) = match p with Player.X -> Player.O | _ -> Player.X

    type Board = private { arr : Player option array; count : int }
    with
        
        member board.Add x y player  =
            if x < 3 && y<3 && x >= 0 && y >= 0 then 
                let arr = Array.copy board.arr
                arr.[x + y * 3] <- Some player
                { arr = arr; count = board.count + if board.IsTaken x y then 0 else 1 }
            else failwithf "x: %i < 3 && y: %i<3" x y
        
        member board.Count = board.count

        member board.IsTaken x y = Option.isSome board.arr.[x+y*3]

        member board.Item with get (x,y) = board.arr.[x+y*3]
        member board.Item with get (struct(x,y)) = board.arr.[x+y*3]

    let add x y player (board:Board) = board.Add x y player
    
    let zero = { 
        arr = Array.zeroCreate 9
        count = 0 }

    type GameState = New | Game of gameBoard: Board * next : Player | Finished of board: Board * winner : Player option

let availableMoves (board:Board) = 
    [| for x in 0..2 do
            for y in 0..2 do
                if not <| board.IsTaken x y then 
                    yield struct(x, y) |]
let gameAvailableMoves = function
    | New -> availableMoves zero
    | Game (board, _)  -> availableMoves board
    | _ -> [||]

let winnigCombinations = [|
      for x in 0 .. 2 -> [ for y in 0 .. 2 -> struct(x, y) ]
      for y in 0 .. 2 -> [ for x in 0 .. 2 -> struct(x, y) ]
      yield [0,0; 1,1; 2,2] |> List.map (fun (x, y) -> struct(x, y))
      yield [0,2; 1,1; 2,0] |> List.map (fun (x, y) -> struct(x, y)) |]

let isWinning (board:Board) = 
    let (|M|_|) x = board.[x]
    let isWinning' = function
    | a::b::[c] -> 
        match a, b,c with
        | M a, M b, M c -> a = b && b = c 
        | _ -> false
    | x -> failwithf "winnigCombinations contains wrong setup (%A)" x
    Array.tryFind isWinning' winnigCombinations |> Option.isSome

let nextMove (struct (x,y)) player = function
    | New -> 
        let board = zero |> add x y player 
        Game (board, opposite player)
    | Game (board, player') 
        when player' = player
            && not <| board.IsTaken x y ->
            let board  = board |> add x y player
            if isWinning board then Finished (board, Some player)
            elif board.Count = 9 then Finished (board, None)
            else Game (board, opposite player)
    | x -> x

let minmax activePlayer game =
    let dict = System.Collections.Generic.Dictionary<GameState, int> ()
    
    let rec score state = 
        match dict.TryGetValue state with
        | true, v -> v
        | false, _ ->
            let v =
                match state with  
                | Finished (board, Some player') ->
                        if player' = activePlayer then 10 - board.Count
                        else board.Count - 10
                | Finished _ -> 0
                | Game (_, player) as game ->
                    let scores = gameAvailableMoves game |> Array.map (fun move -> nextMove move player game |> score)
                    if player = activePlayer then scores |> Array.sortDescending    
                    else scores |> Array.sort
                    |> Array.tryHead |> Option.defaultValue 0 
                | New -> 0
            dict.[state] <- v
            v
    
    gameAvailableMoves game 
    |> Array.map (fun move -> nextMove move activePlayer game |> score, move)
    |> Array.sortByDescending fst
    |> Array.map snd
    |> Array.tryHead
    
 