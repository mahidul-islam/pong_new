module Main exposing (main, update, view)

import Playground exposing (..)
import Set


constWidth : Float
constWidth =
    960


constHeight : Float
constHeight =
    600


type alias Memory =
    { ball : Ball
    , box : Box
    , playerRight : Player
    , playerLeft : Player
    }


type Player
    = PlayerRight PlayerInfo
    | PlayerLeft PlayerInfo


type alias PlayerInfo =
    { score : Int
    , paddle : Paddle
    }


type alias Paddle =
    { x : Float
    , y : Float
    , length : Float
    , thick : Float
    }


type alias Box =
    { width : Float
    , height : Float
    }


type alias Ball =
    { x : Float
    , y : Float
    , speedX : Float
    , speedY : Float
    , length : Float
    }


initialBox : Box
initialBox =
    { width = constWidth, height = constHeight }


initialBall : Ball
initialBall =
    { x = 0, y = 0, speedX = 5, speedY = 5, length = 20 }


initLeftPlayer : PlayerInfo
initLeftPlayer =
    { score = 0, paddle = initLeftPaddle }


initRightPlayer : PlayerInfo
initRightPlayer =
    { score = 0, paddle = initRightPaddle }


initRightPaddle : Paddle
initRightPaddle =
    { x = 450
    , y = 0
    , thick = 10
    , length = 70
    }


initLeftPaddle : Paddle
initLeftPaddle =
    { x = -450
    , y = 0
    , thick = 10
    , length = 70
    }


initialMemory : Memory
initialMemory =
    { ball = initialBall
    , box = initialBox
    , playerRight = PlayerRight <| initRightPlayer
    , playerLeft = PlayerLeft <| initLeftPlayer
    }



-- main : Program () (Game Memory) Msg


main =
    game view update initialMemory


view : Computer -> Memory -> List Shape
view computer memory1 =
    [ drawBall memory1.ball
    , drawBox memory1.box
    , drawScore memory1.playerLeft
    , drawScore memory1.playerRight
    , drawPaddle memory1.playerLeft
    , drawPaddle memory1.playerRight
    ]


drawPaddle : Player -> Shape
drawPaddle player =
    case player of
        PlayerRight playerInfo ->
            rectangle green playerInfo.paddle.thick playerInfo.paddle.length
                |> move playerInfo.paddle.x playerInfo.paddle.y

        PlayerLeft playerInfo ->
            rectangle green playerInfo.paddle.thick playerInfo.paddle.length
                |> move playerInfo.paddle.x playerInfo.paddle.y


drawScore : Player -> Shape
drawScore player =
    case player of
        PlayerRight playerInfo ->
            (words black <| "Score Right : " ++ String.fromInt playerInfo.score) |> move 0 160

        PlayerLeft playerInfo ->
            (words black <| "Score Left : " ++ String.fromInt playerInfo.score) |> move 0 200


drawBall : Ball -> Shape
drawBall ball4 =
    square blue ball4.length
        |> move ball4.x ball4.y


drawBox : Box -> Shape
drawBox box =
    [ rectangle red 1 box.height
        |> move
            (box.width
                / 2
            )
            0
    , rectangle red 1 box.height
        |> move
            -(box.width
                / 2
             )
            0
    , rectangle red box.width 1
        |> move 0
            (box.height
                / 2
            )
    , rectangle red box.width 1
        |> move 0
            -(box.height
                / 2
             )
    ]
        |> group


update : Computer -> Memory -> Memory
update computer memory =
    { memory
        | ball = moveBall memory.box memory.ball memory.playerLeft memory.playerRight
        , playerLeft = updatePlayer memory.playerLeft memory.box memory.ball computer
        , playerRight = updatePlayer memory.playerRight memory.box memory.ball computer
    }


moveBall : Box -> Ball -> Player -> Player -> Ball
moveBall box ball playerLeft playerRight =
    let
        halfWidth =
            box.width / 2

        halfHeight =
            box.height / 2

        bsX =
            ball.speedX

        bsY =
            ball.speedY

        bx =
            ball.x

        by =
            ball.y
    in
    case playerLeft of
        PlayerLeft playerInfo ->
            case playerRight of
                PlayerRight playerInfo2 ->
                    -- Only possible if missed by paddle
                    if bx > halfWidth && bsX > 0 then
                        { ball
                            | speedX = bsX * -1
                            , speedY = bsY
                            , x = bx + bsX
                            , y = by + bsY
                        }

                    else if by > halfHeight && bsY > 0 then
                        { ball
                            | speedX = bsX
                            , speedY = bsY * -1
                            , x = bx + bsX
                            , y = by + bsY
                        }

                    else if bx < -halfWidth && bsX < 0 then
                        { ball
                            | speedX = bsX * -1
                            , speedY = bsY
                            , x = bx + bsX
                            , y = by + bsY
                        }

                    else if by < -halfHeight && bsY < 0 then
                        { ball
                            | speedX = bsX
                            , speedY = bsY * -1
                            , x = bx + bsX
                            , y = by + bsY
                        }

                    else
                        { ball
                            | x = bx + bsX
                            , y = by + bsY
                            , speedX = bsX
                            , speedY = bsY
                        }

                -- Not Possible
                PlayerLeft playerInfo2 ->
                    { ball
                        | x = bx + bsX
                        , y = by + bsY
                        , speedX = bsX
                        , speedY = bsY
                    }

        -- Not Possible
        PlayerRight playerInfo ->
            { ball
                | x = bx + bsX
                , y = by + bsY
                , speedX = bsX
                , speedY = bsY
            }



-- playerLeft of
--     PlayerRight  playerInfo ->
--         if bx > halfWidth && bsX > 0 then
--             { ball
--                 | speedX = bsX * -1
--                 , speedY = bsY
--                 , x = bx + bsX
--                 , y = by + bsY
--             }
--         else if by > halfHeight && bsY > 0 then
--             { ball
--                 | speedX = bsX
--                 , speedY = bsY * -1
--                 , x = bx + bsX
--                 , y = by + bsY
--             }
--         else if bx < -halfWidth && bsX < 0 then
--             { ball
--                 | speedX = bsX * -1
--                 , speedY = bsY
--                 , x = bx + bsX
--                 , y = by + bsY
--             }
--         else if by < -halfHeight && bsY < 0 then
--             { ball
--                 | speedX = bsX
--                 , speedY = bsY * -1
--                 , x = bx + bsX
--                 , y = by + bsY
--             }
--         else
--             { ball
--                 | x = bx + bsX
--                 , y = by + bsY
--                 , speedX = bsX
--                 , speedY = bsY
--             }


updatePlayer : Player -> Box -> Ball -> Computer -> Player
updatePlayer player box ball computer =
    -- let
    --     _ =
    --         if (box.width / 2) < ball.x then
    --             case player of
    --                 PlayerRight playerInfo ->
    --                     Debug.log "Right Player Score +1" playerInfo.score
    --                 PlayerLeft playerInfo ->
    --                     Debug.log "Nothing" playerInfo.score
    --         else if -(box.width / 2) > ball.x then
    --             case player of
    --                 PlayerRight playerInfo ->
    --                     Debug.log "Nothing" playerInfo.score
    --                 PlayerLeft playerInfo ->
    --                     Debug.log "Left Player Score +1" playerInfo.score
    --         else
    --             Debug.log "Nothing" 0
    -- in
    let
        _ =
            Debug.log "Key pressed" computer.keyboard.keys
    in
    if (box.width / 2) < ball.x then
        case player of
            PlayerRight playerInfo ->
                { playerInfo | score = playerInfo.score + 1 }
                    |> PlayerRight

            PlayerLeft playerInfo ->
                playerInfo
                    |> PlayerLeft

    else if -(box.width / 2) > ball.x then
        case player of
            PlayerRight playerInfo ->
                playerInfo
                    |> PlayerRight

            PlayerLeft playerInfo ->
                { playerInfo | score = playerInfo.score + 1 }
                    |> PlayerLeft

    else
        case player of
            -- PlayerRight playerInfo ->
            --     -- if playerInfo.paddle.y <= 300 && playerInfo.paddle.y >= -300 then
            --     { playerInfo
            --         | paddle =
            --             { initRightPaddle
            --                 | y = playerInfo.paddle.y + (toY computer.keyboard * 10)
            --             }
            --     }
            --         |> PlayerRight
            -- else
            --     playerInfo |> PlayerRight
            PlayerRight playerInfo ->
                if Set.member "ArrowUp" computer.keyboard.keys && playerInfo.paddle.y < 300 then
                    { playerInfo
                        | paddle =
                            { initRightPaddle
                                | y = playerInfo.paddle.y + 10
                            }
                    }
                        |> PlayerRight

                else if Set.member "ArrowDown" computer.keyboard.keys && playerInfo.paddle.y > -300 then
                    { playerInfo
                        | paddle =
                            { initRightPaddle
                                | y = playerInfo.paddle.y - 10
                            }
                    }
                        |> PlayerRight

                else
                    playerInfo |> PlayerRight

            PlayerLeft playerInfo ->
                if Set.member "w" computer.keyboard.keys && playerInfo.paddle.y < 300 then
                    { playerInfo
                        | paddle =
                            { initLeftPaddle
                                | y = playerInfo.paddle.y + 10
                            }
                    }
                        |> PlayerLeft

                else if Set.member "s" computer.keyboard.keys && playerInfo.paddle.y > -300 then
                    { playerInfo
                        | paddle =
                            { initLeftPaddle
                                | y = playerInfo.paddle.y - 10
                            }
                    }
                        |> PlayerLeft

                else
                    playerInfo |> PlayerLeft



-- updateScoreValue : Player -> Player
-- updateScoreValue player =
--     case player of
--         PlayerRight playerInfo ->
--             { playerInfo | score = playerInfo.score + 1 }
--                 |> PlayerRight
--         PlayerLeft playerInfo ->
--             { playerInfo | score = playerInfo.score + 1 }
--                 |> PlayerLeft
