module ticTacToe

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open TicTacToe
open TicTacToe.Domain

let buttons = seq {
    let doc = Browser.document
    let board = doc.getElementById "board"
    let buttons = board.getElementsByTagName_button()
    for i in 0..(int buttons.length - 1) -> buttons.Item i } |> Seq.map (fun x->x.id, x) |> Map.ofSeq

let draw = function
    | New ->
        buttons |> Seq.iter (fun kvp -> kvp.Value.innerHTML <- "&nbsp;")
    | Game (board, _)
    | Finished (board, _) ->
        for i = 0 to 2 do
            for j = 0 to 2 do
                buttons.[sprintf"b%i%i" i j].innerHTML <- board.[i,j] 
                                        |> Option.map (function Player.X -> "X" | _ -> "O") 
                                        |> Option.defaultValue "&nbsp;"
        

let actor = MailboxProcessor.Start(fun inbox -> 
    let rec loop state = async {
        try
            draw state
            let! (msg :string) = inbox.Receive ()
            let x = System.Int32.Parse (msg.[1].ToString())
            let y = System.Int32.Parse (msg.[2].ToString())
            let state = nextMove (struct(x,y)) Player.O state
            
            let state = 
                match minmax Player.X state with
                | Some x-> nextMove x Player.X state
                | None -> state
            return! loop state
        with e -> 
            printfn "ex = %O" e
            return! loop state }
    loop New)

buttons |> Seq.iter (fun kvp -> kvp.Value.addEventListener_click(fun _ -> actor.Post kvp.Key ;null))

