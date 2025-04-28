open Raylib_cs

let screenWidth = 800
let screenHeight = 800
let tileSize = 100
let boardSize = 8

let lightColor = Color(0xee, 0xee, 0xd2, 255)
let darkColor = Color(0x76, 0x96, 0x56, 255)
let highlightColor = Color.RED

// 2D board: Some char = a piece, None = empty
let board : char option[,] = Array2D.create boardSize boardSize None

// Initialize a few pieces
let initializeBoard () =
    for i in 0 .. 7 do
        board.[i, 1] <- Some '♟' // Black pawns
        board.[i, 6] <- Some '♙' // White pawns
    board.[0, 0] <- Some '♜'; board.[7, 0] <- Some '♜' // Black rooks
    board.[0, 7] <- Some '♖'; board.[7, 7] <- Some '♖' // White rooks
    board.[3, 0] <- Some '♛'
    board.[3, 7] <- Some '♕'

// Optional: track selected tile
let mutable selectedX = -1
let mutable selectedY = -1

let drawBoard () =
    for x in 0 .. boardSize - 1 do
        for y in 0 .. boardSize - 1 do
            let isLight = (x + y) % 2 = 0
            let color = if isLight then lightColor else darkColor
            Raylib.DrawRectangle(x * tileSize, y * tileSize, tileSize, tileSize, color)

            // Highlight selected tile
            if x = selectedX && y = selectedY then
                Raylib.DrawRectangleLinesEx(Rectangle(float32 (x * tileSize), float32 (y * tileSize), float32 tileSize, float32 tileSize), 3.0f, highlightColor)

let drawPieces () =
    for x in 0 .. boardSize - 1 do
        for y in 0 .. boardSize - 1 do
            match board.[x, y] with
            | Some piece ->
                Raylib.DrawText(piece.ToString(), x * tileSize + 35, y * tileSize + 25, 40, Color.BLACK)
            | None -> ()

let checkMouseClick () =
    if CBool.op_Implicit(Raylib.IsMouseButtonPressed MouseButton.MOUSE_LEFT_BUTTON) then
        let mousePos = Raylib.GetMousePosition()
        let x = int mousePos.X / tileSize
        let y = int mousePos.Y / tileSize
        if x >= 0 && x < boardSize && y >= 0 && y < boardSize then
            selectedX <- x
            selectedY <- y
            printfn "Tile selected: %c%d" (char (x + int 'A')) (8 - y)

[<EntryPoint>]
let main _ =
    Raylib.InitWindow(screenWidth, screenHeight, "F# Chessboard with Pieces")
    Raylib.SetTargetFPS 60
    initializeBoard()

    while not (CBool.op_Implicit(Raylib.WindowShouldClose())) do
        checkMouseClick()

        Raylib.BeginDrawing()
        Raylib.ClearBackground Color.RAYWHITE

        drawBoard()
        drawPieces()

        Raylib.EndDrawing()

    Raylib.CloseWindow()
    0
