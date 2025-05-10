open Raylib_cs

type Color = White | Black

type PieceType =
    | Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King

type Piece = {
    pieceType: PieceType
    color: Color
    hasMoved: bool
}

type Board = Piece option [,]

type GameState = {
    board: Board
    whiteTurn: bool
    selected: (int * int) option
    legalTargets: (int * int) list
}

let boardSize = 8
let tileSize = 100
let screenWidth = boardSize * tileSize
let screenHeight = boardSize * tileSize

let mutable gameState = {
    board = Array2D.create boardSize boardSize None
    whiteTurn = true
    selected = None
    legalTargets = []
}

let lightColor = Color(0xee, 0xee, 0xd2, 255)
let darkColor = Color(0x76, 0x96, 0x56, 255)
let highlightColor = Color.RED

// 2D board: Some char = a piece, None = empty
let board : char option[,] = Array2D.create boardSize boardSize None

let mkPiece color pieceType = Some { pieceType = pieceType; color = color; hasMoved = false }

// Optional: track selected tile
let mutable selectedX = -1
let mutable selectedY = -1

let isInside (x, y) =
    x >= 0 && x < boardSize && y >= 0 && y < boardSize

let isEnemy (board: Board) (color: Color) (x: int, y: int) =
    match board.[x, y] with
    | Some piece -> piece.color <> color
    | None -> false

let isEmpty (board: Board) (x: int, y: int) =
    board.[x, y].IsNone

let rangeBetween a b =
    if a < b then [a+1 .. b-1]
    else [b+1 .. a-1]

let isClearStraight (board: Board) (fromX, fromY) (toX, toY) =
    if fromX = toX then
        // Vertical move
        rangeBetween fromY toY
        |> List.forall (fun y -> board.[fromX, y] |> Option.isNone)
    elif fromY = toY then
        // Horizontal move
        rangeBetween fromX toX
        |> List.forall (fun x -> board.[x, fromY] |> Option.isNone)
    else
        false

let isLegalKnightMove (board: Board) (piece: Piece) (fromX, fromY) (toX, toY) =
    let dx = abs (toX - fromX)
    let dy = abs (toY - fromY)
    let legalL = (dx = 2 && dy = 1) || (dx = 1 && dy = 2)
    legalL && (isEmpty board (toX, toY) || isEnemy board piece.color (toX, toY))

let isClearDiagonal (board: Board) (fromX, fromY) (toX, toY) =
    let dx = toX - fromX
    let dy = toY - fromY
    if abs dx <> abs dy then false
    else
        let stepX = if dx > 0 then 1 else -1
        let stepY = if dy > 0 then 1 else -1
        let count = abs dx - 1
        Seq.init count (fun i -> (fromX + stepX * (i + 1), fromY + stepY * (i + 1)))
        |> Seq.forall (fun (x, y) -> board.[x, y].IsNone)

let isLegalBishopMove (board: Board) (piece: Piece) (fromX, fromY) (toX, toY) =
    isClearDiagonal board (fromX, fromY) (toX, toY)
    && (isEmpty board (toX, toY) || isEnemy board piece.color (toX, toY))

let isLegalRookMove (board: Board) (piece: Piece) (fromX, fromY) (toX, toY) =
    isClearStraight board (fromX, fromY) (toX, toY)
    && (isEmpty board (toX, toY) || isEnemy board piece.color (toX, toY))

let isLegalQueenMove (board: Board) (piece: Piece) (fromX, fromY) (toX, toY) =
    isLegalRookMove board piece (fromX, fromY) (toX, toY)
    || isLegalBishopMove board piece (fromX, fromY) (toX, toY)

let isLegalKingMove (board: Board) (piece: Piece) (fromX, fromY) (toX, toY) =
    let dx = abs (toX - fromX)
    let dy = abs (toY - fromY)
    (dx <= 1 && dy <= 1)
    && (isEmpty board (toX, toY) || isEnemy board piece.color (toX, toY))

let isLegalPawnMove (board: Board) (piece: Piece) (fromX, fromY) (toX, toY) =
    let direction = match piece.color with White -> -1 | Black -> 1
    let startRank = match piece.color with White -> 6 | Black -> 1

    let dx = toX - fromX
    let dy = toY - fromY

    let target = board.[toX, toY]

    match dx, dy with
    // Forward move by 1 into empty square
    | 0, d when d = direction && target.IsNone -> true
    // Forward move by 2 from start rank, both tiles empty
    | 0, d when d = 2 * direction && fromY = startRank ->
        let intermediateY = fromY + direction
        board.[toX, intermediateY].IsNone && target.IsNone
    // Diagonal capture
    | dx, d when abs dx = 1 && d = direction ->
        match target with
        | Some t when t.color <> piece.color -> true
        | _ -> false
    // Anything else is illegal
    | _ -> false

let isLegalMove (board: Board) (piece: Piece) (fromX, fromY) (toX, toY) =
    if not (isInside (toX, toY)) then false
    elif fromX = toX && fromY = toY then false
    else
        match piece.pieceType with
        | Pawn -> isLegalPawnMove board piece (fromX, fromY) (toX, toY)
        | Knight -> isLegalKnightMove board piece (fromX, fromY) (toX, toY)
        | Rook -> isLegalRookMove board piece (fromX, fromY) (toX, toY)
        | Bishop -> isLegalBishopMove board piece (fromX, fromY) (toX, toY)
        | Queen -> isLegalQueenMove board piece (fromX, fromY) (toX, toY)
        | King -> isLegalKingMove board piece (fromX, fromY) (toX, toY)

let generateLegalMoves (board: Board) (piece: Piece) (x: int, y: int) =
    [ for toX in 0 .. boardSize - 1 do
        for toY in 0 .. boardSize - 1 do
            if isLegalMove board piece (x, y) (toX, toY) then
                yield (toX, toY) ]

let initializeBoard () =
    for i in 0 .. 7 do
        gameState.board.[i, 1] <- mkPiece Black Pawn
        gameState.board.[i, 6] <- mkPiece White Pawn

    let backRow = [ Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook ]
    for i in 0 .. 7 do
        gameState.board.[i, 0] <- mkPiece Black backRow.[i]
        gameState.board.[i, 7] <- mkPiece White backRow.[i]

let drawBoard () =
    for x in 0 .. boardSize - 1 do
        for y in 0 .. boardSize - 1 do
            let isLight = (x + y) % 2 = 0
            let color = if isLight then lightColor else darkColor
            Raylib.DrawRectangle(x * tileSize, y * tileSize, tileSize, tileSize, color)

            match gameState.selected with
            | Some (selX, selY) when selX = x && selY = y ->
                Raylib.DrawRectangleLinesEx(Rectangle(float32 (x * tileSize), float32 (y * tileSize), float32 tileSize, float32 tileSize), 4.0f, highlightColor)
            | _ -> ()
    for (tx, ty) in gameState.legalTargets do
        let cx = tx * tileSize + tileSize / 2
        let cy = ty * tileSize + tileSize / 2
        Raylib.DrawCircle(cx, cy, 12.0f, Raylib.ColorAlpha(highlightColor, 0.3f))

let pieceToChar (piece: Piece) : char =
    let c =
        match piece.pieceType with
        | Pawn -> 'P'
        | Knight -> 'N'
        | Bishop -> 'B'
        | Rook -> 'R'
        | Queen -> 'Q'
        | King -> 'K'
    match piece.color with
    | White -> c
    | Black -> System.Char.ToLowerInvariant(c)

let drawPieces () =
    for x in 0 .. boardSize - 1 do
        for y in 0 .. boardSize - 1 do
            match gameState.board.[x, y] with
            | Some piece ->
                let text = pieceToChar piece |> string
                Raylib.DrawText(text, x * tileSize + 35, y * tileSize + 25, 40, Color.BLACK)
            | None -> ()
let isInsideBoard (x: int) (y: int) =
    x >= 0 && x < boardSize && y >= 0 && y < boardSize

let handleMouseClick () =
    if Raylib.IsMouseButtonPressed(MouseButton.MOUSE_LEFT_BUTTON) |> CBool.op_Implicit then
        let mousePos = Raylib.GetMousePosition()
        let x = int mousePos.X / tileSize
        let y = int mousePos.Y / tileSize

        match gameState.selected with
        | None ->
            match gameState.board.[x, y] with
            | Some piece when piece.color = (if gameState.whiteTurn then White else Black) ->
                let moves = generateLegalMoves gameState.board piece (x, y)
                gameState <- { gameState with selected = Some (x, y); legalTargets = moves }
            | _ -> ()

        | Some (fromX, fromY) ->
            match gameState.board.[fromX, fromY] with
            | Some piece when isLegalMove gameState.board piece (fromX, fromY) (x, y) ->
                gameState.board.[x, y] <- Some { piece with hasMoved = true }
                gameState.board.[fromX, fromY] <- None
                gameState <- {
                    gameState with
                        selected = None
                        legalTargets = []
                        whiteTurn = not gameState.whiteTurn
                }
            | _ ->
                gameState <- { gameState with selected = None; legalTargets = [] }

[<EntryPoint>]
let main _ =
    Raylib.InitWindow(screenWidth, screenHeight, "F# Chess - World State")
    Raylib.SetTargetFPS 60
    initializeBoard()

    while not (Raylib.WindowShouldClose() |> CBool.op_Implicit) do
        handleMouseClick()

        Raylib.BeginDrawing()
        Raylib.ClearBackground(Color.RAYWHITE)

        drawBoard()
        drawPieces()

        Raylib.EndDrawing()

    Raylib.CloseWindow()
    0
