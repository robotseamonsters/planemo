import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window


blueTrans : Color
blueTrans =
  rgba 90 90 180 0.75

blackSpace : Color
blackSpace =
  rgba 10 10 10 1.0

-- MODEL

type alias State =
  { pos : (Float, Float)
  , vel : (Float, Float)
  , acc : (Float, Float)
  , rot : Float
  , col : Color
  }
  
gameState : State
gameState =
  { pos = (0, 0)
  , vel = (0, 0)
  , acc = (0, 0)
  , rot = 0
  , col = blueTrans
  }

type alias InputDir = {x: Int, y: Int }

-- UPDATE
update : (Float, InputDir) -> State -> State
update (dt, keydir) state = 
  state
   |> rotateShip keydir dt
   |> thrustShip keydir
   |> physics dt

integrate2d : Float -> (Float, Float) -> (Float, Float) -> (Float, Float)
integrate2d dt (x, y) (dx, dy) =
  ( x + dt * dx, y + dt * dy )

rotateShip : InputDir -> Float -> State -> State
rotateShip keydir dt state =
  { state |
    rot <- state.rot - dt * toFloat keydir.x * pi * 0.01
  }

thrustShip : InputDir -> State -> State
thrustShip keydir state =
  if keydir.y > 0
    then
      { state |
          acc <- (-(sin state.rot), cos state.rot)
      } 
    else { state | acc <- (0,0) }

mult2d : Float -> (Float, Float) -> (Float, Float)
mult2d val (x, y) =
  (x * val, y * val)


physics : Float -> State -> State
physics dt state =
  { state |
      pos <- integrate2d dt state.pos state.vel,
      vel <- mult2d (0.9995 / dt) <| integrate2d dt state.vel state.acc
  }

-- SIGNALS

main : Signal Element
main =
  Signal.map view (Signal.foldp update gameState input)

input : Signal (Float, InputDir)
input =
  let
    delta = Signal.map(\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,)
      delta Keyboard.wasd)

-- VIEW

view : State -> Element
view state =
  collage 800 800
  [ square 800
    |> filled blackSpace
  , 
    polygon [ (0, 20), (-15, -20), (15, -20) ] 
    |> filled state.col
    |> rotate state.rot
    |> move state.pos
  ]
    

