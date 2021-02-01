open Printf

type letter = | X | O

type value =
  | Unspecified
  | Letter of letter

type oneThroughThree = | One | Two | Three

type row = value * value * value

type board = row * row * row

let emptyBoard =
  (Unspecified, Unspecified, Unspecified),
  (Unspecified, Unspecified, Unspecified),
  (Unspecified, Unspecified, Unspecified)

type position = { column: oneThroughThree; row: oneThroughThree }

type move = { at: position; place: letter}

let select (board: board) (position: position) = 
  match board, position with
  | ((x, _, _), _, _), { column = One; row = One} -> x
  | ((_, x, _), _, _), { column = Two; row = One} -> x
  | ((_, _, x), _, _), { column = Three; row = One} -> x
  | (_, (x, _, _), _), { column = One; row = Two} -> x
  | (_, (_, x, _), _), { column = Two; row = Two} -> x
  | (_, (_, _, x), _), { column = Three; row = Two} -> x
  | (_, _, (x, _, _)), { column = One; row = Three} -> x
  | (_, _, (_, x, _)), { column = Two; row = Three} -> x
  | (_, _, (_, _, x)), { column = Three; row = Three} -> x

let set value (board: board) (position: position) = 
  match board, position with
  | ((_, v2, v3), r2, r3), { column = One; row = One} -> (value, v2, v3), r2, r3
  | ((v1, _, v3), r2, r3), { column = Two; row = One} -> (v1, value, v3), r2, r3
  | ((v1, v2, _), r2, r3), { column = Three; row = One} -> (v1, v2, value), r2, r3
  | (r1, (_, v2, v3), r3), { column = One; row = Two} -> r1, (value, v2, v3), r3
  | (r1, (v1, _, v3), r3), { column = Two; row = Two} -> r1, (v1, value, v3), r3
  | (r1, (v1, v2, _), r3), { column = Three; row = Two} -> r1, (v1, v2, value), r3
  | (r1, r2, (_, v2, v3)), { column = One; row = Three} -> r1, r2, (value, v2, v3)
  | (r1, r2, (v1, _, v3)), { column = Two; row = Three} -> r1, r2, (v1, value, v3)
  | (r1, r2, (v1, v2, _)), { column = Three; row = Three} -> r1, r2, (v1, v2, value)

let modify f (board: board) (position :position) =
  set (f (select board position)) board position

let placePieceIfCan piece = modify (function Unspecified -> Letter piece | x -> x)

let makeMove (board: board) (move: move) =
  if select board move.at = Unspecified
  then Some (placePieceIfCan move.place board move.at)
  else None

let waysToWin =
  [
    { row=One; column=One }, { row=One; column=Two }, {row=One; column=Three };
    { row=Two; column=One }, { row=Two; column=Two }, {row=Two; column=Three };
    { row=Three; column=One }, { row=Three; column=Two }, {row=Three; column=Three };
    { row=One; column=One }, { row=Two; column=One }, {row=Three; column=One };
    { row=One; column=Two }, { row=Two; column=Two }, {row=Three; column=Two };
    { row=One; column=Three }, { row=Two; column=Three }, {row=Three; column=Three };
    { row=One; column=One }, { row=Two; column=Two }, {row=Three; column=Three };
    { row=One; column=Three }, { row=Two; column=Two }, {row=Three; column=One };
  ]

let cells =
  List.to_seq [One; Two; Three]
  |> Seq.flat_map (fun row -> (List.to_seq [One; Two; Three] |> Seq.map (fun column -> { row=row; column=column })))
  |> List.of_seq

let map3 f (a, b, c) = f a, f b, f c

let winner (board: board) =
  let winPaths = List.map (map3 (select board)) waysToWin in
  if List.mem (Letter X, Letter X, Letter X) winPaths
  then Some X
  else if List.mem (Letter O, Letter O, Letter O) winPaths
  then Some O
  else None

let slotsRemaining (board: board) =
  List.exists (fun cell -> select board cell = Unspecified) cells

type outcome =
  | NoneYet
  | Winner of letter
  | Draw

let outcome (board: board) =
  match winner board, slotsRemaining board with
  | Some winningLetter, _ -> Winner winningLetter
  | None, false -> Draw
  | _ -> NoneYet

let renderValue = function
  | Unspecified -> " "
  | Letter X -> "X"
  | Letter O -> "O"

let renderLetter = function
  | X -> "X"
  | O -> "O"

let otherPlayer = function
  | X -> O
  | O -> X

let render((a, b, c), (d, e, f), (g, h, i)) =
  sprintf {|%s|%s|%s
------
%s|%s|%s
------
%s|%s|%s
|}
    (renderValue a) (renderValue b) (renderValue c)
    (renderValue d) (renderValue e) (renderValue f)
    (renderValue g) (renderValue h) (renderValue i)

type gameState = { board: board; whoseTurn: letter }

let initialGameState = { board=emptyBoard; whoseTurn=X }

let parseOneThroughThree = function
  | "1" -> Some One
  | "2" -> Some Two
  | "3" -> Some Three
  | _ -> None

let parseMove (raw: string) =
  match String.split_on_char ' ' raw  with
  | [r; c] ->
    begin match parseOneThroughThree r, parseOneThroughThree c with
      | Some row, Some column -> Some { row=row; column=column }
      | _ -> None
    end
  | _ -> None

(* IO Monad *)
(* Type of effects we want to use *)
type effects = {
  readLine: unit -> string;
  printLine: string -> unit;
}

(* Wrapper for results *)
type 'a io  = effects -> 'a

(* Flatmap *)
let (>>=) (io: 'a io) (f: 'a -> 'b io): 'b io =
  fun eff -> f (io eff) eff

(* Unit *)
let pureIo v _ = v

let readLine () eff = eff.readLine ()
let printLine text eff = eff.printLine text

let rec readMoveIo (letter: letter) : effects -> move =
  readLine () >>= fun line ->
  match (parseMove line) with
  | Some position -> pureIo { at=position; place=letter }
  | None ->
    printLine "Bad move! Please input row and column numbers\n" >>= fun () ->
    readMoveIo letter

let rec nextMoveIo (board: board) (letter: letter): effects -> board =
  readMoveIo letter >>= fun move ->
  match makeMove board move with
  | Some newBoard -> pureIo newBoard
  | _ ->
    printLine "Bad move! Position is occupied." >>= fun () ->
    nextMoveIo board letter

let rec playIo { board=board; whoseTurn=currentPlayer }: effects -> unit =
  printLine (sprintf "%s's turn\n" (renderLetter currentPlayer)) >>= fun () ->
  printLine (sprintf "%s\n" (render board)) >>= fun () ->

  nextMoveIo board currentPlayer >>= fun newBoard ->
  printLine "\n" >>= fun () ->

  match outcome newBoard with
  | Winner letter ->
    printLine (sprintf "%s wins!!!\n" (renderLetter letter)) >>= fun () ->
    printLine (sprintf "%s" (render newBoard));
  | Draw ->
    printLine "It's a draw!"
  | NoneYet ->
    playIo { board=newBoard; whoseTurn=otherPlayer currentPlayer }

(* Finally pass the effects around *)
let console = { readLine=read_line; printLine=printf "%s"}
let _ = playIo initialGameState console
