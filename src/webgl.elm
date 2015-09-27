import Graphics.Element exposing (..)
import Color exposing (..)
import WebGL exposing (..)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Time exposing (..)
import Keyboard
import Window


-- CUSTOM TYPES


type alias Ship =
  { pos : (Float, Float)
  , vel : (Float, Float)
  , acc : (Float, Float)
  , rot : Float
  , col : Color
  , mesh : Renderable
  }


type alias State =
  { ship : Ship }


-- CONSTANTS


blueTrans : Color
blueTrans =
  rgba 90 90 180 0.75


blackSpace : Color
blackSpace =
  rgba 10 10 10 1.0


meshChevronShip : Renderable
meshChevronShip =


-- MODEL


playerShip : Ship
playerShip = 
  { pos = (0,0)
  , vel = (0,0)
  , acc = (0,0)
  , rot = 0
  , col = blueTrans
  , mesh = meshChevronShip
  }


gameState : State
gameState =
  { ship = playerShip }


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
  { state | ship <- shipPhysics dt state.ship }


shipPhysics : Float -> Ship -> Ship
shipPhysics dt ship =
  { ship |
      pos <- integrate2d dt ship.pos ship.vel,
      vel <- mult2d (0.9995 / dt) <| integrate2d dt ship.vel ship.acc
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
  webgl 800 800
  [ render
      vertexShader
      fragmentShader
      state.ship.mesh
      { perspective = viewMatrix }
  ]


viewMatrix : Mat4
viewMatrix =
  mul (makePerspective 45 1 0.01 100)
      (makeLookAt (vec3 10 0 0) (vec3 0 0 0) (vec3 0 1 0))
    

-- SHADERS


vertexShader : Shader { attr | position:Vec3, color:Vec3 } { unif | perspective:Mat4 } { vcolor:Vec3 }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec3 color;
    uniform mat4 perspective;
    varying vec3 vcolor;
    void main () {
      gl_Position = perspective * vec4(position, 1.0);
      vcolor = color; }
  |]


fragmentShader : Shader {} u { vcolor:Vec3 }
fragmentShader =
  [glsl|
    precision mediump float;
    varying vec3 vcolor;
    void main () {
      gl_FragColor = vec4(vcolor, 1.0);
    }
  |]

