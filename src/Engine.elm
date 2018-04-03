module Engine exposing (..)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Keyboard exposing (downs)
import Set exposing (Set)
import Time exposing (Time)
import WebGL exposing (..)


type alias Model =
    { time : Float
    , camera : Camera
    , activeKeys : Set Int
    }


type alias Camera =
    { eye : Vec3
    , orientation : Vec3
    , up : Vec3
    }


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


type Msg
    = Tick Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        camera =
            model.camera

        eye =
            model.camera.eye

        orientation =
            model.camera.orientation

        activeKeys =
            model.activeKeys

        leftArrow =
            65

        rightArrow =
            68

        upArrow =
            87

        downArrow =
            83

        qKey =
            81

        eKey =
            69

        leftKeyOn =
            Set.member leftArrow activeKeys

        rightKeyOn =
            Set.member rightArrow activeKeys

        upKeyOn =
            Set.member upArrow activeKeys

        downKeyOn =
            Set.member downArrow activeKeys

        eKeyOn =
            Set.member eKey activeKeys

        qKeyOn =
            Set.member qKey activeKeys

    in
        case msg of
            Tick t ->
                ( { model
                    | camera =
                        camera
                            |> (updateCameraEye
                                    (if (leftKeyOn) then
                                        Vec3.setX ( (Vec3.getX eye) - 0.1) eye
                                    else if (rightKeyOn) then
                                        Vec3.setX ( (Vec3.getX eye) + 0.1) eye
                                    else if (upKeyOn) then
                                        Vec3.setZ ( (Vec3.getZ eye) - 0.1) eye
                                    else if (downKeyOn) then
                                        Vec3.setZ ( (Vec3.getZ eye) + 0.1) eye
                                    else
                                        eye
                                    )
                               )
                            |> (updateCameraOrientation
                                ( if (eKeyOn) then
                                      Vec3.setX ( (Vec3.getX orientation) - 0.1) orientation
                                  else if (qKeyOn) then
                                      Vec3.setX ( (Vec3.getX orientation) + 0.1) orientation
                                  else 
                                    orientation
                                )
                            )
                  }
                , Cmd.none
                )

            KeyDown 65 ->
                ( { model | activeKeys = Set.insert leftArrow activeKeys }, Cmd.none )

            KeyDown 68 ->
                ( { model | activeKeys = Set.insert rightArrow activeKeys }, Cmd.none )

            KeyDown 87 ->
                ( { model | activeKeys = Set.insert upArrow activeKeys }, Cmd.none )

            KeyDown 83 ->
                ( { model | activeKeys = Set.insert downArrow activeKeys }, Cmd.none )

            KeyDown 69 ->
                ( { model | activeKeys = Set.insert eKey activeKeys }, Cmd.none )

            KeyDown 81 ->
                ( { model | activeKeys = Set.insert qKey activeKeys }, Cmd.none )

            KeyDown _ ->
                ( model, Cmd.none )

            KeyUp key ->
                ( { model | activeKeys = Set.remove key activeKeys }, Cmd.none )



updateCameraEye : Vec3 -> Camera -> Camera
updateCameraEye eye cam =
    { cam | eye = eye }

updateCameraOrientation : Vec3 -> Camera -> Camera
updateCameraOrientation orientation cam =
    { cam | orientation = orientation }


-- VIEW


view : Model -> Html msg
view model =
    let
        t =
            model.time

        cam =
            model.camera
    in
        Html.div [] [
        WebGL.toHtml
            [ width 400
            , height 400
            , style [ ( "display", "block" ) ]
            ]
            [ WebGL.entity
                vertexShader
                fragmentShader
                mesh
                { perspective = perspective cam }
            ]
        , Html.p [] [ Html.text (toString cam.eye) ]
        ]


perspective : Camera -> Mat4
perspective cam =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt cam.eye (vec3 (Vec3.getX cam.eye) 0 0) cam.up)


-- Mesh
-- The actual triangle


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]



-- MAIN SUB INIT


subscriptions : Model -> Sub Msg
subscriptions model =
      Sub.batch
        [ AnimationFrame.times Tick
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ] 

init : ( Model, Cmd Msg )
init =
    ( Model
        0
        (Camera (vec3 0 0 4) (vec3 0 0 0) (vec3 0 1 0))
        Set.empty
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
