module Main exposing (main)

import Array
import Browser
import Dict
import Html exposing (div, pre, span, text)
import Html.Attributes exposing (style, tabindex)
import Html.Events
import Json.Decode as Json
import List
import Set



----------------------------------------------------------------
-- Elm Infrastructure


main =
    Browser.element
        { init = init
        , update = boringUpdate
        , subscriptions = subscriptions
        , view = view
        }


init : Json.Value -> ( Model, Cmd Msg )
init v =
    let
        map =
            case Json.decodeValue decodeMap v of
                Ok m ->
                    Debug.log "Successfully parsed map" m

                Err e ->
                    Debug.log (Debug.toString e) exampleMap
    in
    ( { worldMap = map
      , wind = N
      , vessel =
            { position = ( mapSize // 2, mapSize // 2 )
            , mode = Wagon
            , charge = 5
            }
      }
    , Cmd.none
    )


type alias Model =
    { worldMap : TileMap
    , wind : Wind
    , vessel : Vessel
    }


type Msg
    = Move Direction
    | Transform VesselMode
    | Pass


update : Msg -> Model -> Model
update msg model =
    case msg of
        Transform md ->
            tryTransform md model

        Move d ->
            tryMove d model

        _ ->
            model


boringUpdate : Msg -> Model -> ( Model, Cmd Msg )
boringUpdate msg model =
    ( update msg model, Cmd.none )


view : Model -> Html.Html Msg
view m =
    let
        worldCells =
            Dict.map (\k v -> toCell v) m.worldMap

        viewCells =
            Dict.insert m.vessel.position (vesselCell m.vessel) worldCells
                |> viewAround m.vessel.position
    in
    div []
        [ div [ style "font-family" "monospace", keyboardControls, tabindex 0 ] [ renderCells viewCells ]
        , div [ style "font-famliy" "cursive" ] [ dashboard m ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



----------------------------------------------------------------
-- Game Model


mapSize =
    256


viewportSize =
    31


{-| Leave one cell in the centre for the player
-}
viewportRange =
    15


type TileKind
    = Sea
    | Land
    | Mountain


type alias Point =
    ( Int, Int )


type alias TileMap =
    Dict.Dict Point TileKind


type alias CellMap =
    Dict.Dict Point Cell


type Direction
    = N
    | E
    | S
    | W


type alias Wind =
    Direction


type VesselMode
    = Boat
    | Wagon
    | Baloon


type alias Vessel =
    { position : Point, mode : VesselMode, charge : Float }


exampleMap : TileMap
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
            if (x + y) < 250 then
                Sea

            else if (x + y) < 262 then
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


{-| Apply a function to a Dictionary's keys
-}
mapKeys : (comparable -> comparable) -> Dict.Dict comparable v -> Dict.Dict comparable v
mapKeys fn frm =
    Dict.foldl (\k v i -> Dict.insert (fn k) v i) Dict.empty frm


{-| Determine if a point is within a given distance (x and y) of another point.
-}
inRangeOf : Int -> Point -> Point -> Bool
inRangeOf rng about pt =
    let
        ( dx, dy ) =
            pointDistances about pt
    in
    (dx < rng) && (dy < rng)


{-| Choose a centrepoint for the screen. Allows the player to wander free near
the edge, but be centred in the middle of the map.
-}
centrePoint : Point -> Point
centrePoint p =
    let
        minCentre =
            viewportRange + 1

        maxCentre =
            mapSize - (viewportRange + 1)

        ( x, y ) =
            p
    in
    ( clamp minCentre maxCentre x, clamp minCentre maxCentre y )


offsets : Point -> Point
offsets ( x, y ) =
    let
        maxOffset =
            mapSize - viewportSize

        minOffset =
            0

        norm =
            clamp minOffset maxOffset
    in
    ( norm <| x - viewportRange, norm <| y - viewportRange )


{-| Extract a 64 x 64 square near the given point, bounded by 0 and the map size.
-}
viewAround : Point -> CellMap -> CellMap
viewAround aroundPt m =
    let
        centre =
            centrePoint aroundPt

        ( dx, dy ) =
            offsets centre

        normCoord ( x, y ) =
            ( x - dx, y - dy )
    in
    m |> Dict.filter (\p c -> inRangeOf viewportRange centre p) |> mapKeys normCoord


{-| Render a TileMap in the Rogue-like style (coords in [0, 64))
-}
renderCells : CellMap -> Html.Html Msg
renderCells m =
    let
        ys =
            Array.initialize viewportSize (\i -> viewportSize - i - 1)

        -- for 0-indexing
        xs =
            range viewportSize

        cellAt pt =
            Dict.get pt m |> Maybe.withDefault emptyCell

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


vesselCell : Vessel -> Cell
vesselCell v =
    case v.mode of
        Wagon ->
            Cell "w" "brown" "green"

        Boat ->
            Cell "b" "brown" "blue"

        Baloon ->
            Cell "B" "red" "skyblue "


vesselStats : Model -> Html.Html Msg
vesselStats m =
    let
        displayCharge =
            floor m.vessel.charge |> String.fromInt

        displayPartial =
            String.concat <| List.repeat (fractional m.vessel.charge) "*"

        fractional f =
            f
                - (toFloat <| floor f)
                |> (*) 4.0
                |> floor
    in
    Html.p [] [ Html.text <| String.concat [ "Engine Charge: ", displayCharge, displayPartial ] ]


windStats : Model -> Html.Html Msg
windStats m =
    let
        indicator =
            case m.wind of
                N ->
                    "▲"

                S ->
                    "▼"

                E ->
                    "▶"

                W ->
                    "◀"
    in
    Html.p [] [ Html.text <| String.concat [ "Wind Direction: ", indicator ] ]


dashboard : Model -> Html.Html Msg
dashboard m =
    div [ Html.Attributes.id "dashboard" ]
        [ vesselStats m
        , windStats m
        ]



----------------------------------------------------------------
-- Input transformer


mapKeyCode : Int -> Msg
mapKeyCode kc =
    case kc of
        -- j, k, and l transform
        74 ->
            Transform Boat

        75 ->
            Transform Baloon

        76 ->
            Transform Wagon

        -- Num-pad map to directions (num-lock off)
        38 ->
            Move N

        39 ->
            Move E

        40 ->
            Move S

        37 ->
            Move W

        -- Also WASD for directions
        87 ->
            Move N

        68 ->
            Move E

        83 ->
            Move S

        65 ->
            Move W

        -- otherwise, do nothing
        _ ->
            Debug.log (Debug.toString kc) Pass


keyboardControls : Html.Attribute Msg
keyboardControls =
    Html.Events.on "keydown" (Json.map mapKeyCode Html.Events.keyCode)



----------------------------------------------------------------
-- Model properties


neighborTilesOf : Point -> Model -> List TileKind
neighborTilesOf pt m =
    let
        neighborPoints =
            List.map (\d -> pointAdd pt d) directionChanges

        maybeTiles =
            List.map (\p -> Dict.get p m.worldMap) neighborPoints

        addMaybeTile mt l =
            case mt of
                Nothing ->
                    l

                Just t ->
                    t :: l
    in
    List.foldl addMaybeTile [] maybeTiles



----------------------------------------------------------------
-- Handle player moves


canBoat : Model -> Bool
canBoat m =
    let
        playerTile =
            Dict.get m.vessel.position m.worldMap
    in
    case playerTile of
        Just Sea ->
            True

        _ ->
            False


canWagon : Model -> Bool
canWagon m =
    let
        nts =
            neighborTilesOf m.vessel.position m
    in
    List.member Land nts


canBaloon : Model -> Bool
canBaloon m =
    let
        charge =
            m.vessel.charge
    in
    charge >= 1.0


canTransform : VesselMode -> Model -> Bool
canTransform mode model =
    case mode of
        Boat ->
            canBoat model

        Wagon ->
            canWagon model

        Baloon ->
            canBaloon model


tryTransform : VesselMode -> Model -> Model
tryTransform toMode model =
    let
        vessel =
            model.vessel

        newVessel =
            if canTransform toMode model then
                { vessel | mode = toMode }

            else
                vessel
    in
    { model | vessel = newVessel }


canMoveTo : Point -> Model -> Bool
canMoveTo pos model =
    let
        vm =
            model.vessel.mode

        targetTile =
            Dict.get pos model.worldMap
    in
    case targetTile of
        Nothing ->
            False

        Just Sea ->
            vm == Boat || vm == Baloon || List.member Land (neighborTilesOf pos model)

        Just Land ->
            vm /= Boat

        Just Mountain ->
            vm == Baloon


moveTo : Point -> Model -> Model
moveTo p m =
    let
        vessel =
            m.vessel

        newVessel =
            { vessel | position = p }
    in
    { m | vessel = newVessel }


moveCost : Direction -> Model -> Float
moveCost d m =
    case m.vessel.mode of
        Baloon ->
            if d == m.wind then
                0.0

            else
                1.0

        Boat ->
            if d == oppositeWind m.wind then
                1.0

            else
                0.0

        Wagon ->
            0.0


deductCharge : Float -> Model -> Model
deductCharge c m =
    let
        vessel =
            m.vessel

        newCharge =
            clamp 0 5 (vessel.charge - c)

        newVessel =
            { vessel | charge = newCharge }
    in
    { m | vessel = newVessel }


moveAndTrackCharge : Direction -> Model -> Model
moveAndTrackCharge d m =
    let
        deltaCharge =
            moveCost d m - 0.3

        newPos =
            targetPos d m
    in
    m |> moveTo newPos |> deductCharge deltaCharge


tryMove : Direction -> Model -> Model
tryMove d m =
    let
        targetCost =
            moveCost d m

        t =
            targetPos d m

        terrainOk =
            canMoveTo t m

        chargeOk =
            m.vessel.charge >= targetCost
    in
    if terrainOk && chargeOk then
        moveAndTrackCharge d m

    else
        m



----------------------------------------------------------------
-- Misc helpers


range : Int -> Array.Array Int
range n =
    Array.initialize n identity


show x =
    div [] [ text <| Debug.toString x ]


pointDistances : Point -> Point -> Point
pointDistances ( x, y ) ( a, b ) =
    ( abs (x - a), abs (y - b) )


pointAdd ( x, y ) ( a, b ) =
    ( a + x, b + y )


directionChange : Direction -> Point
directionChange d =
    case d of
        N ->
            ( 0, 1 )

        E ->
            ( 1, 0 )

        S ->
            ( 0, -1 )

        W ->
            ( -1, 0 )


directionChanges : List Point
directionChanges =
    [ ( 0, 1 )
    , ( 1, 0 )
    , ( 0, -1 )
    , ( -1, 0 )
    ]


targetPos : Direction -> Model -> Point
targetPos d m =
    pointAdd m.vessel.position (directionChange d)


oppositeWind : Wind -> Wind
oppositeWind w =
    case w of
        N ->
            S

        E ->
            W

        W ->
            E

        S ->
            N



----------------------------------------------------------------
-- Parsing maps


decodePoint : Json.Decoder Point
decodePoint =
    let
        x =
            Json.index 0 Json.int

        y =
            Json.index 1 Json.int

        f a b =
            ( a, b )
    in
    Json.map2 f x y


decodeTileKind : Json.Decoder TileKind
decodeTileKind =
    let
        p s =
            case s of
                "sea" ->
                    Sea

                "land" ->
                    Land

                "mountain" ->
                    Mountain

                _ ->
                    Sea

        -- default to Sea to avoid nasty error handling code
    in
    Json.map p Json.string


decodeTile : Json.Decoder ( Point, TileKind )
decodeTile =
    let
        pt =
            Json.index 0 decodePoint

        kind =
            Json.index 1 decodeTileKind

        f a b =
            ( a, b )
    in
    Json.map2 f pt kind


decodeMap : Json.Decoder TileMap
decodeMap =
    let
        pts =
            Json.list decodeTile
    in
    Json.map Dict.fromList pts
