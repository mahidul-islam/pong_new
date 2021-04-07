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
    in
    if memory2.ball.x >= halfWidth && memory2.ball.speedX > 0 then
        { ball =
            { x = memory2.ball.x + memory2.ball.speedX
            , y = memory2.ball.y + memory2.ball.speedY

            -- , x = memory2.ball.x + (toX computer.keyboard * memory2.ball.speed)
            -- , y = memory2.ball.y + (toY computer.keyboard * memory2.ball.speed)
            , speedX = memory2.ball.speedX * -1
            , speedY = memory2.ball.speedY
            , length = memory2.ball.length
            }
        , box = memory2.box
        }

    else if memory2.ball.y >= halfHeight && memory2.ball.speedY > 0 then
        { ball =
            { x = memory2.ball.x + memory2.ball.speedX
            , y = memory2.ball.y + memory2.ball.speedY
            , speedX = memory2.ball.speedX
            , speedY = memory2.ball.speedY * -1
            , length = memory2.ball.length
            }
        , box = memory2.box
        }

    else if memory2.ball.x <= -halfWidth && memory2.ball.speedX < 0 then
        { ball =
            { x = memory2.ball.x + memory2.ball.speedX
            , y = memory2.ball.y + memory2.ball.speedY
            , speedX = memory2.ball.speedX * -1
            , speedY = memory2.ball.speedY
            , length = memory2.ball.length
            }
        , box = memory2.box
        }

    else if memory2.ball.y <= -halfHeight && memory2.ball.speedY < 0 then
        { ball =
            { x = memory2.ball.x + memory2.ball.speedX
            , y = memory2.ball.y + memory2.ball.speedY
            , speedX = memory2.ball.speedX
            , speedY = memory2.ball.speedY * -1
            , length = memory2.ball.length
            }
        , box = memory2.box
        }

    else
        { ball =
            { x = memory2.ball.x + memory2.ball.speedX
            , y = memory2.ball.y + memory2.ball.speedY
            , speedX = memory2.ball.speedX
            , speedY = memory2.ball.speedY
            , length = memory2.ball.length
            }
        , box = memory2.box
        }
