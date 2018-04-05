module Engine exposing (..)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Input exposing (..) 
import Set exposing (Set)
import Time exposing (Time)
import Color
import WebGL exposing (..)


type alias Model =
    { time : Float
    , camera : Camera
    , keys : Input.Keys
    }


type alias Camera =
    { eye : Vec3
    , orientation : Vec3
    , up : Vec3
    , velocity : Vec3
    }


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type Msg
    = Tick Time
    | KeyDown Int
    | KeyUp Int



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        camera =
            model.camera

        eye =
            model.camera.eye
    in
        case msg of
            Tick t ->
                ( { model
                    | camera =
                        camera
                            |> moveCamera model.keys 
                            |> physics (t / 2000000000000)
                  }
                , Cmd.none
                )
                
            KeyDown code ->
                ( { model | keys = Input.keyFunc True code model.keys}, Cmd.none )

            KeyUp code ->
                ( { model | keys = Input.keyFunc False code model.keys }, Cmd.none )


moveCamera : Input.Keys -> Camera -> Camera
moveCamera { left, right, up, down, space } cam = 
    let
        direction a b =
            if a == b then
                0
            else if a then
                0.1
            else
                -0.1
    in
    { cam 
        | velocity = vec3 (direction left right) (Vec3.getY cam.eye) (direction up down)
    } 


physics : Float -> Camera -> Camera
physics t cam =
    let
        eye =
            Vec3.add cam.eye (Vec3.scale t cam.velocity)
    in
        { cam
            | eye = eye
        }


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
            , style [ ( "display", "block" ), ("border", "1px solid purple") ]
            ]
            [ scene model ]
        , Html.p [] [ Html.text (toString cam.eye) ]
        ]

scene : Model -> Entity
scene model = WebGL.entity
                vertexShader
                fragmentShader
                cubeMesh
                { perspective = perspective model.camera
                , shade = 0.8 }


perspective : Camera -> Mat4
perspective cam =
    Mat4.mul
        (Mat4.makePerspective 45 1 0.01 100)
        (Mat4.makeLookAt cam.eye (Vec3.add cam.eye Vec3.k) cam.up)


-- Meshes

cubeMesh : Mesh Vertex
cubeMesh =
    let
        rft =
            vec3 1 1 1

        lft =
            vec3 -1 1 1

        lbt =
            vec3 -1 -1 1

        rbt =
            vec3 1 -1 1

        rbb =
            vec3 1 -1 -1

        rfb =
            vec3 1 1 -1

        lfb =
            vec3 -1 1 -1

        lbb =
            vec3 -1 -1 -1
    in
        [ face Color.green rft rfb rbb rbt
        , face Color.blue rft rfb lfb lft
        , face Color.yellow rft lft lbt rbt
        , face Color.red rfb lfb lbb rbb
        , face Color.purple lft lfb lbb lbt
        , face Color.orange rbt rbb lbb lbt
        ]
        |> List.concat
        |> WebGL.triangles


face : Color.Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face rawColor a b c d =
    let
        color =
            let
                c =
                    Color.toRgb rawColor
            in
                vec3
                    (toFloat c.red / 255)
                    (toFloat c.green / 255)
                    (toFloat c.blue / 255)

        vertex position =
            Vertex color position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]
-- MAIN SUB INIT


subscriptions : Model -> Sub Msg
subscriptions model =
      Sub.batch
        [ AnimationFrame.times Tick
        , Input.keyDownSub KeyDown
        , Input.keyUpSub KeyUp
        ] 

init : ( Model, Cmd Msg )
init =
    ( Model
        0
        (Camera (vec3 0 0 -2) (vec3 0 0 0) (vec3 0 1 0) (vec3 0.1 0 0))
        Input.init
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
    { perspective : Mat4
    , shade : Float
    }

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
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = shade * vec4(vcolor, 1.0);
        }
    |]
