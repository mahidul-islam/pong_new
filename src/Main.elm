module Main exposing (main, update, view)

import Playground exposing (..)


type alias Memory =
    { ball : Ball
    , box : Box
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


box : Box
box =
    { width = 960, height = 600 }


ball : Ball
ball =
    { x = 0, y = 0, speedX = 5, speedY = 5, length = 20 }


memory : Memory
memory =
    { ball = ball
    , box = box
    }


main : Program () (Game Memory) Msg
main =
    game view update memory


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
drawBox box2 =
    [ rectangle red 1 box2.height
        |> move
            (box2.width
                / 2
            )
            0
    , rectangle red 1 box2.height
        |> move
            -(box2.width
                / 2
             )
            0
    , rectangle red box2.width 1
        |> move 0
            (box2.height
                / 2
            )
    , rectangle red box2.width 1
        |> move 0
            -(box2.height
                / 2
             )
    ]
        |> group


update : Computer -> Memory -> Memory
update computer memory2 =
    let
        halfWidth =
            memory2.box.width / 2

        halfHeight =
            memory2.box.height / 2

        bsX =
            memory2.ball.speedX

        bsY =
            memory2.ball.speedY

        bx =
            memory2.ball.x

        by =
            memory2.ball.y
    in
    -- , x = bx + (toX computer.keyboard * memory2.ball.speed)
    -- , y = by + (toY computer.keyboard * memory2.ball.speed)
    if bx >= halfWidth && bsX > 0 then
        { memory2 | ball = { ball | speedX = bsX * -1, speedY = bsY, x = bx, y = by } }

    else if by >= halfHeight && bsY > 0 then
        { memory2 | ball = { ball | speedX = bsX, speedY = bsY * -1, x = bx, y = by } }

    else if bx <= -halfWidth && bsX < 0 then
        { memory2 | ball = { ball | speedX = bsX * -1, speedY = bsY, x = bx, y = by } }

    else if by <= -halfHeight && bsY < 0 then
        { memory2 | ball = { ball | speedX = bsX, speedY = bsY * -1, x = bx, y = by } }

    else
        { memory2
            | ball = { ball | x = bx + bsX, y = by + bsY, speedX = bsX, speedY = bsY }
        }
