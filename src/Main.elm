module Main exposing (main, update, view)

import Playground exposing (..)


constWidth : Float
constWidth =
    960


constHeight : Float
constHeight =
    600


type alias Memory =
    { ball : Ball
    , box : Box
    }


type alias Player =
    { score : Int, paddle : Paddle }


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


initialMemory : Memory
initialMemory =
    { ball = initialBall
    , box = initialBox
    }



-- main : Program () (Game Memory) Msg


main =
    game view update initialMemory


view : Computer -> Memory -> List Shape
view computer memory1 =
    [ drawBall memory1.ball
    , drawBox memory1.box
    ]


drawBall : Ball -> Shape
drawBall ball4 =
    rectangle blue ball4.length ball4.length
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
    { memory | ball = moveBall memory.box memory.ball }


moveBall : Box -> Ball -> Ball
moveBall box ball =
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
    -- , x = bx + (toX computer.keyboard * memory2.ball.speed)
    -- , y = by + (toY computer.keyboard * memory2.ball.speed)
    -- { memory2 | ball = { ball | x = bx, y = by } }
    --     |>
    if bx >= halfWidth && bsX > 0 then
        { ball
            | speedX = bsX * -1
            , speedY = bsY
            , x = bx + bsX
            , y = by + bsY
        }

    else if by >= halfHeight && bsY > 0 then
        { ball
            | speedX = bsX
            , speedY = bsY * -1
            , x = bx + bsX
            , y = by + bsY
        }

    else if bx <= -halfWidth && bsX < 0 then
        { ball
            | speedX = bsX * -1
            , speedY = bsY
            , x = bx + bsX
            , y = by + bsY
        }

    else if by <= -halfHeight && bsY < 0 then
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
