module Main exposing (main)

import Playground exposing (..)
import Set


windowWidth : Float
windowWidth =
    960


windowHeight : Float
windowHeight =
    600


type alias Memory =
    { ball : Ball
    , playerRight : Player
    , playerLeft : Player
    }


type PlayerSide
    = RightSide
    | LeftSide


type alias Player =
    { score : Int
    , paddleY : Float
    , side : PlayerSide
    }


type alias Ball =
    { x : Float
    , y : Float
    , speedX : Float
    , speedY : Float
    }


ballLength : Float
ballLength =
    20


initialBall : Ball
initialBall =
    { x = 0, y = 0, speedX = 7, speedY = 7 }


initPlayer side =
    { side = side, score = 0, paddleY = 0 }


paddleThick =
    10


paddleLength =
    70


initialMemory : Memory
initialMemory =
    { ball = initialBall
    , playerRight = initPlayer RightSide
    , playerLeft = initPlayer LeftSide
    }



-- main : Program () (Game Memory) Msg


main =
    game view update initialMemory


view : Computer -> Memory -> List Shape
view computer memory =
    [ viewBall memory.ball
    , viewBox
    , viewScore memory.playerLeft
    , viewScore memory.playerRight
    , viewPaddle memory.playerLeft
    , viewPaddle memory.playerRight
    ]


paddleX : PlayerSide -> Float
paddleX side =
    case side of
        RightSide ->
            450

        LeftSide ->
            -450


viewPaddle : { a | side : PlayerSide, paddleY : Float } -> Shape
viewPaddle { side, paddleY } =
    rectangle green paddleThick paddleLength
        |> move (paddleX side) paddleY


viewScore : { player | score : Int, side : PlayerSide } -> Shape
viewScore { side, score } =
    let
        scoreText sideText =
            words black
                ("Score "
                    ++ sideText
                    ++ " : "
                    ++ String.fromInt score
                )
    in
    case side of
        RightSide ->
            scoreText "Right" |> move 0 160

        LeftSide ->
            scoreText "Left" |> move 0 200


viewBall : Ball -> Shape
viewBall ball =
    square blue ballLength
        |> move ball.x ball.y


viewBox : Shape
viewBox =
    [ rectangle red 1 windowHeight
        |> move (windowWidth / 2) 0
    , rectangle red 1 windowHeight
        |> move -(windowWidth / 2) 0
    , rectangle red windowWidth 1
        |> move 0 (windowHeight / 2)
    , rectangle red windowWidth 1
        |> move 0 -(windowHeight / 2)
    ]
        |> group


update : Computer -> Memory -> Memory
update computer memory =
    let
        leftPaddleMovementKeys =
            { upKey = "w", downKey = "s" }

        rightPaddleMovementKeys =
            { upKey = "ArrowUp", downKey = "ArrowDown" }

        { movedBall, touchedSide } =
            moveBall
                memory.playerLeft
                memory.playerRight
                memory.ball
    in
    { memory
        | ball = movedBall
        , playerLeft =
            movePlayerPaddle
                leftPaddleMovementKeys
                computer.keyboard.keys
                memory.playerLeft
        , playerRight =
            movePlayerPaddle
                rightPaddleMovementKeys
                computer.keyboard.keys
                memory.playerRight
    }
        |> scoreAPointIfSideTouched touchedSide


moveBall : Player -> Player -> Ball -> { movedBall : Ball, touchedSide : Maybe PlayerSide }
moveBall playerLeft playerRight =
    let
        halfWidth =
            windowWidth / 2

        halfHeight =
            windowHeight / 2

        halfBallLength =
            ballLength / 2

        bounceOffBox ball =
            let
                updateXSpeed xSpeedUpdate =
                    { ball | speedX = xSpeedUpdate ball.speedX }

                updateYSpeed ySpeedUpdate =
                    { ball | speedY = ySpeedUpdate ball.speedY }
            in
            -- Only possible if missed by paddle
            if ball.x + halfBallLength > halfWidth then
                { movedBall = updateXSpeed (\a -> -(abs a))
                , touchedSide = Just RightSide
                }

            else if ball.x - halfBallLength < -halfWidth then
                { movedBall = updateXSpeed abs
                , touchedSide = Just LeftSide
                }

            else
                { touchedSide = Nothing
                , movedBall =
                    if ball.y + halfBallLength > halfHeight then
                        updateYSpeed (\a -> -(abs a))

                    else if ball.y - halfBallLength < -halfHeight then
                        updateYSpeed abs

                    else
                        ball
                }

        bounceOffPaddels ball =
            let
                clashDistanceX =
                    (paddleThick + ballLength) / 2

                clashDistanceY =
                    (paddleLength + ballLength) / 2

                reverseSpeedX =
                    { ball | speedX = -ball.speedX }

                updateXSpeed xSpeedUpdate =
                    { ball | speedX = xSpeedUpdate ball.speedX }

                updateYSpeed ySpeedUpdate =
                    { ball | speedY = ySpeedUpdate ball.speedY }
            in
            -- Have to check if the Paddle crash with the ball below
            if
                (diff ball.x (paddleX LeftSide) < clashDistanceX)
                    && (diff playerLeft.paddleY ball.y < clashDistanceY)
            then
                updateXSpeed abs

            else if
                (diff ball.x (paddleX RightSide) < clashDistanceX)
                    && (diff playerRight.paddleY ball.y < clashDistanceY)
            then
                updateXSpeed (\a -> -(abs a))

            else
                ball
    in
    moveBallByXYSpeed
        >> bounceOffPaddels
        >> bounceOffBox


moveBallByXYSpeed : Ball -> Ball
moveBallByXYSpeed ball =
    { ball
        | x = ball.x + ball.speedX
        , y = ball.y + ball.speedY
    }


diff : Float -> Float -> Float
diff a b =
    abs (a - b)


scoreAPointIfSideTouched side players =
    let
        scoreAPoint player =
            { player | score = player.score + 1 }
    in
    case side of
        Just LeftSide ->
            { players
                | playerRight =
                    scoreAPoint players.playerRight
            }

        Just RightSide ->
            { players
                | playerLeft =
                    scoreAPoint players.playerLeft
            }

        Nothing ->
            players


movePlayerPaddle paddleMovementKeys keys player =
    { player
        | paddleY =
            updatePaddleY
                paddleMovementKeys
                keys
                player.paddleY
    }


updatePaddleY { upKey, downKey } keys =
    let
        move paddleY =
            if Set.member upKey keys then
                paddleY + 10

            else if Set.member downKey keys then
                paddleY - 10

            else
                paddleY
    in
    move
        >> min (windowHeight / 2 - paddleLength / 2)
        >> max -(windowHeight / 2 - paddleLength / 2)
