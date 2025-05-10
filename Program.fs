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
}

let boardSize = 8
let tileSize = 100
let screenWidth = boardSize * tileSize
let screenHeight = boardSize * tileSize

let mutable gameState = {
    board = Array2D.create boardSize boardSize None
    whiteTurn = true
    selected = None
}

let lightColor = Color(0xee, 0xee, 0xd2, 255)
let darkColor = Color(0x76, 0x96, 0x56, 255)
let highlightColor = Color.RED

// 2D board: Some char = a piece, None = empty
let board : char option[,] = Array2D.create boardSize boardSize None

let mkPiece color pieceType = Some { pieceType = pieceType; color = color; hasMoved = false }

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


// Optional: track selected tile
let mutable selectedX = -1
let mutable selectedY = -1

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

        if isInsideBoard x y then
            match gameState.selected with
            | None ->
                // Select if there's a piece of correct color
                match gameState.board.[x, y] with
                | Some piece when (piece.color = (if gameState.whiteTurn then White else Black)) ->
                    gameState <- { gameState with selected = Some (x, y) }
                | _ -> ()

            | Some (fromX, fromY) ->
                // Move piece
                let pieceOpt = gameState.board.[fromX, fromY]
                match pieceOpt with
                | Some piece ->
                    gameState.board.[x, y] <- Some { piece with hasMoved = true }
                    gameState.board.[fromX, fromY] <- None
                    gameState <- { gameState with selected = None; whiteTurn = not gameState.whiteTurn }
                | None ->
                    gameState <- { gameState with selected = None }

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
