module Main exposing (..)

import Angle
import Axis3d
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Camera3d
import Color
import Direction3d
import Html exposing (Html, div)
import Html.Attributes
import Length exposing (Length, Meters, meters)
import Pixels
import Point3d
import Random
import Scene3d
import Scene3d.Material
import Task
import Viewpoint3d



---- MODEL ----


type alias Model =
    { screenX : Int
    , screenY : Int
    , time : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { screenX = 500
      , screenY = 500
      , time = 0
      }
    , Task.perform
        (\{ viewport } ->
            Resize
                (round viewport.width)
                (round viewport.height)
        )
        Dom.getViewport
    )



---- UPDATE ----


type Msg
    = Resize Int Int
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize x y ->
            ( { model
                | screenX = x
                , screenY = y
              }
            , Cmd.none
            )

        Tick delta ->
            ( { model
                | time = model.time + delta
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        -- how many rows/columns of blocks to draw
        axisCount =
            50

        -- translate the grid of blocks to center it in the viewport
        offset =
            -0.5 + (-1 * axisCount / 2)
    in
    div
        [ Html.Attributes.width model.screenX
        , Html.Attributes.height model.screenY
        , Html.Attributes.style "background-color" "rgb(33, 26, 64)"
        ]
        [ Scene3d.sunny
            { dimensions = ( Pixels.int model.screenX, Pixels.int model.screenY )
            , camera =
                Camera3d.perspective
                    { viewpoint =
                        Viewpoint3d.lookAt
                            { eyePoint = Point3d.meters 0 0 85
                            , focalPoint = Point3d.meters 0 0 0
                            , upDirection = Direction3d.positiveZ
                            }
                    , verticalFieldOfView = Angle.degrees 35
                    }
            , clipDepth = meters 0.1
            , upDirection = Direction3d.positiveZ
            , sunlightDirection = Direction3d.negativeZ
            , shadows = False
            , background = Scene3d.transparentBackground
            , entities =
                List.concatMap
                    (\xi ->
                        List.map
                            (\yi ->
                                blockAtIndex offset model.time xi yi
                            )
                            (List.range 1 axisCount)
                    )
                    (List.range 1 axisCount)
            }
        ]


blockAtIndex : Float -> Float -> Int -> Int -> Scene3d.Entity coordinates
blockAtIndex xyPositionOffset angleOffset xi yi =
    let
        axisOffset n =
            Angle.degrees <| n + (angleOffset / 500)

        -- randomize the axis upon which the cube rotates
        ( rotateDirection, _ ) =
            Random.step
                (Random.andThen
                    (\a ->
                        Random.uniform
                            (Direction3d.xy <| axisOffset a)
                            [ Direction3d.yz <| axisOffset a, Direction3d.zy <| axisOffset a ]
                    )
                    (Random.float 0 10000)
                )
                (Random.initialSeed <|
                    (xi + (yi * 1000))
                )

        block =
            Scene3d.block
                (Scene3d.Material.nonmetal
                    { baseColor = Color.rgb255 91 192 235
                    , roughness = 0.6
                    }
                )
                (Block3d.with
                    { x1 = Length.meters <| xyPositionOffset + toFloat xi
                    , x2 = Length.meters <| xyPositionOffset + toFloat xi + 0.6
                    , y1 = Length.meters <| xyPositionOffset + toFloat yi
                    , y2 = Length.meters <| xyPositionOffset + toFloat yi + 0.6
                    , z1 = Length.meters 0
                    , z2 = Length.meters 0.6
                    }
                )
    in
    Scene3d.rotateAround
        (Axis3d.through
            (Point3d.xyz
                (Length.meters <| toFloat xi + 0.3 + xyPositionOffset)
                (Length.meters <| toFloat yi + 0.3 + xyPositionOffset)
                (Length.meters 0.3)
            )
            rotateDirection
        )
        (Angle.degrees <| (angleOffset / 150) + toFloat xi / 3 + toFloat yi / 3)
        block



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Events.onResize Resize
                    , Events.onAnimationFrameDelta Tick
                    ]
        }
