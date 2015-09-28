import Graphics.Element exposing (..)
import Window exposing (..)
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
  , mesh : Drawable {color: Vec3, position: Vec3}
  }

type alias Bounds =
  { xmin: Float, ymin: Float, xmax: Float, ymax: Float }

type alias WinDim = (Int, Int)

type alias State =
  { ship : Ship
  , bounds : Bounds
  , dim : WinDim
  }


-- CONSTANTS


blue : Vec3
blue = vec3 0 0 0.75

ltblue : Vec3
ltblue = vec3 0.50 0.50 1.0

chevronShipMesh : Drawable {color: Vec3, position: Vec3} 
chevronShipMesh =
  Triangle
  [ ( {color = ltblue, position = vec3 0 2 0 }
    , {color = blue, position = vec3 0 -1.75 0 }
    , {color = ltblue, position = vec3 1.5 -2 0 }
    )
  ,
    ( {color = ltblue, position = vec3 0 2 0 }
    , {color = blue, position = vec3 0 -1.75 0 }
    , {color = ltblue, position = vec3 -1.5 -2 0 }
    )
  ]


-- MODEL


playerShip : Ship
playerShip = 
  { pos = (0,0)
  , vel = (0,0)
  , acc = (0,0)
  , rot = 0
  , mesh = chevronShipMesh
  }


gameState : State
gameState =
  { ship = playerShip
  , bounds = {xmin = -1, ymin = -1, xmax = 1, ymax = 1}
  , dim = (512, 512)
  }


type alias InputDir = {x: Int, y: Int }


-- UPDATE
update : (Float, InputDir, WinDim) -> State -> State
update (dt, keydir, dim) state = 
  state
   |> inputs keydir dt
   |> physics dt
   |> resize dim


integrate2d : Float -> (Float, Float) -> (Float, Float) -> (Float, Float)
integrate2d dt (x, y) (dx, dy) =
  ( x + dt * dx, y + dt * dy )


rotateShip : InputDir -> Float -> Ship -> Ship
rotateShip keydir dt ship =
  { ship |
    rot <- ship.rot - dt * toFloat keydir.x * pi * 0.001
  }


thrustShip : InputDir -> Ship -> Ship
thrustShip keydir ship =
  if keydir.y > 0
    then
      { ship |
          acc <- (-0.000005 * sin ship.rot, 0.000005 * cos ship.rot)
      } 
    else { ship | acc <- (0,0) }


inputs : InputDir -> Float -> State -> State
inputs keydir dt state =
  { state | ship <- rotateShip keydir dt <| thrustShip keydir state.ship }


resize : WinDim -> State -> State
resize dim state =
  let (w, h) = dim
      fw = toFloat w
      fh = toFloat h
  in
    { state |
      bounds <-
        { xmin = state.bounds.xmin
        , xmax = state.bounds.xmax
        , ymin = -fh / fw
        , ymax = fh / fw
        }
      ,
      dim <- dim
    }
    

mult2d : Float -> (Float, Float) -> (Float, Float)
mult2d val (x, y) =
  (x * val, y * val)


keepBounds : Bounds -> (Float, Float) -> (Float, Float)
keepBounds bounds (x,y) =
  ( x |> min bounds.xmax |> max bounds.xmin
  , y |> min bounds.ymax |> max bounds.ymin )


physics : Float -> State -> State
physics dt state =
  let shipNew = state.ship
  in
    { state | ship <-
      { shipNew | 
        pos <- keepBounds state.bounds (integrate2d dt shipNew.pos shipNew.vel)
      , vel <- mult2d 0.95 <| integrate2d dt shipNew.vel shipNew.acc
      }
    }


-- SIGNALS


main : Signal Element
main =
  Signal.map view (Signal.foldp update gameState input)


input : Signal (Float, InputDir, WinDim)
input =
  let
    delta = (fps 60)
    dim = Window.dimensions
  in
    Signal.sampleOn delta (Signal.map3 (,,)
      delta Keyboard.wasd dim)


-- VIEW


view : State -> Element
view state =
  webgl state.dim
  [ render
      vertexShader
      fragmentShader
      state.ship.mesh
      { mvp = mul (modelMatrix state.ship) viewMatrix |> mul (projMatrix state.dim) }
  ]


projMatrix : (Int, Int) -> Mat4
projMatrix dim =
  let (w,h) = dim
  in
    makeScale3 1 (toFloat w / toFloat h) 1

viewMatrix : Mat4
viewMatrix =
  makeOrtho2D -50 50 -50 50
{-{perspective
  mul (makePerspective 45 1 1.0 200)
      (makeLookAt (vec3 0 0 100) (vec3 0 0 0) (vec3 0 1 0))
-}


modelMatrix : Ship -> Mat4 
modelMatrix ship =
  let (x,y) = ship.pos
  in
    mul (makeTranslate3 x y 0)
        (makeRotate ship.rot (vec3 0 0 1))

-- SHADERS


vertexShader : Shader { attr | position:Vec3, color:Vec3 } { unif | mvp:Mat4 } { vcolor:Vec3 }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec3 color;
    uniform mat4 mvp;
    varying vec3 vcolor;
    void main () {
      gl_Position = mvp * vec4(position, 1.0);
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

