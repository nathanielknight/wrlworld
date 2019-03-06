module Main exposing (main)

import Array
import Browser
import Dict
import Html exposing (div, pre, span, text)
import Html.Attributes exposing (style)
import List
import Set



----------------------------------------------------------------
-- Elm Infrastructure


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { worldMap = Dict.empty, wind = { speed = 0, direction = N }, vessel = { position = ( 64, 64 ), mode = Boat } }, Cmd.none )


type alias Model =
    { worldMap : CellMap
    , wind : Wind
    , vessel : Vessel
    }


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update _ m =
    ( m, Cmd.none )


view : Model -> Html.Html Msg
view _ =
    div [ style "font-family" "monospace" ] [ renderCells exampleMap ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



----------------------------------------------------------------
-- Game Model


mapSize =
    256



--- It's actually 64, but I zero indexed so ... yeah


viewportSize =
    64


type TileKind
    = Sea
    | Land
    | Mountain


type alias Point =
    ( Int, Int )


type alias CellMap =
    Dict.Dict Point TileKind


type Direction
    = N
    | E
    | S
    | W
    | NE
    | SE
    | SW
    | NW


type alias Wind =
    { speed : Float, direction : Direction }


type VesselMode
    = Boat
    | Wagon
    | Baloon


type alias Vessel =
    { position : Point, mode : VesselMode }


exampleMap : CellMap
exampleMap =
    let
        toPt idx =
            let
                x =
                    modBy mapSize idx

                y =
                    idx // mapSize
            in
            ( x, y )

        pts =
            Array.initialize (mapSize * mapSize) toPt

        ptKind ( x, y ) =
            if (x + y) < 200 then
                Sea

            else if (x + y) < 400 then
                Land

            else
                Mountain
    in
    Array.initialize (mapSize * mapSize) toPt
        |> Array.map (\p -> ( p, ptKind p ))
        |> Array.toList
        |> Dict.fromList



----------------------------------------------------------------
-- View Helpers


type Cell
    = Cell String String String


emptyCell =
    Cell " " "black" "white"


toCell : TileKind -> Cell
toCell t =
    case t of
        Land ->
            Cell "." "darkgreen" "green"

        Sea ->
            Cell "~" "lightgray" "blue"

        Mountain ->
            Cell "^" "lightgray" "gray"


renderCell : Cell -> Html.Html Msg
renderCell (Cell c fg bg) =
    span [ style "color" fg, style "background-color" bg ] [ text c ]



--- Render a CellMap in the Rogue-like style. Expects Points to be in the range 0-63


show x =
    div [] [ text <| Debug.toString x ]


renderCells : CellMap -> Html.Html Msg
renderCells m =
    let
        ys =
            Array.initialize viewportSize (\i -> viewportSize - i - 1)

        -- for 0-indexing
        xs =
            range viewportSize

        cellAt pt =
            Dict.get pt m |> (Maybe.map toCell >> Maybe.withDefault emptyCell)

        row y =
            Array.initialize viewportSize (\x -> cellAt ( x, y ))
                |> Array.map renderCell
                |> Array.toList
                |> span []

        rows =
            Array.map row ys
                |> Array.toList
    in
    pre [] <| List.intersperse (text "\n") rows



----------------------------------------------------------------
-- Range helpers


range : Int -> Array.Array Int
range n =
    Array.initialize n identity


grid : Int -> Array.Array Point
grid n =
    let
        ys =
            range n

        xRow y =
            Array.initialize n (\x -> ( x, y ))

        rows =
            Array.map xRow ys
    in
    Array.foldl Array.append Array.empty rows
