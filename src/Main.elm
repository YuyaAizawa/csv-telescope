module Main exposing (main)

import Browser
import Html exposing (Html, div, select, option, label, textarea, button, table, thead, tbody, tr, th, td, ul, li, text)
import Html.Attributes as HAttr exposing (id, class)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Json
import Svg exposing (Svg, svg)
import Svg.Attributes as SAttr
import Svg.Events

import Csv.Parser
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Locale, usLocale)

main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }



-----------
-- MODEL --
-----------

type alias Model =
  { source : String
  , stars : Stars
  , sight : Sight
  , pointedIdx : Maybe Int
  , mouse : MouseState
  , tab : Tab
  }

type alias Stars =
  { header : List String
  , data : List Star
  }

type alias Star = List String

type alias Sight =
  { origin : Point2D
  , xPerPx : Float
  , yPerPx : Float
  , width : Int
  , height : Int
  , xAxisIdx : Int
  , yAxisIdx : Int
  }

type alias Point2D =
  { x : Float
  , y : Float
  }

init : Model
init =
  { source = "id,x,y,z\nfirst,1,2,3\nsecond,2,3,4"
  , stars =
    { header = []
    , data = []
    }
  , sight =
    { origin = { x = 0.0, y = 0.0 }
    , xPerPx = 2.0
    , yPerPx = 2.0
    , width = 640
    , height = 480
    , xAxisIdx = 0
    , yAxisIdx = 1
    }
  , pointedIdx = Nothing
  , mouse =
    { x = 0
    , y = 0
    , dx = 0
    , dy = 0
    , down = Other
    }
  , tab = TabCsv
  }



------------
-- UPDATE --
------------

type Msg
  = MouseMoved MouseState
  | WheelMoved (Maybe Axis) Float
  | StarPointed (Maybe Int)
  | XAxisSelected Int
  | YAxisSelected Int
  | SourceChanged String
  | ParseRequested
  | TabChanged Tab

type Axis
  = X
  | Y

update : Msg -> Model -> Model
update msg model =
  case msg of
    MouseMoved mouse ->
      case mouse.down of
        Left ->
          let
            sight = model.sight
            x = sight.origin.x - sight.xPerPx * mouse.dx
            y = sight.origin.y - sight.yPerPx * (-mouse.dy)
            sight_ = { sight | origin = Point2D x y }
          in
            { model | sight = sight_, mouse = mouse }
        _ ->
          { model | mouse = mouse }

    WheelMoved axis wheel ->
      let
        sight = model.sight
        ( xPerPx, yPerPx ) =
          case axis of
            Nothing ->
              ( sight.xPerPx * (1 + wheel * (0.001))
              , sight.yPerPx * (1 + wheel * (0.001))
              )
            Just X ->
              ( sight.xPerPx * (1 + wheel * (0.001))
              , sight.yPerPx
              )
            Just Y ->
              ( sight.xPerPx
              , sight.yPerPx * (1 + wheel * (0.001))
              )
        sight_ = { sight | xPerPx = xPerPx, yPerPx = yPerPx }
      in
        { model | sight = sight_ }

    StarPointed idx ->
      { model | pointedIdx = idx, tab = TabSelected }

    XAxisSelected idx ->
      let
        sight = model.sight
        sight_ = { sight | xAxisIdx = idx }
      in
        { model | sight = sight_ }

    YAxisSelected idx ->
      let
        sight = model.sight
        sight_ = { sight | yAxisIdx = idx }
      in
        { model | sight = sight_ }

    SourceChanged source ->
      { model | source = source }

    ParseRequested ->
      let
        stars = parseCsv model.source
      in
        { model
        | stars = stars
        , tab = if List.isEmpty stars.header then TabCsv else TabTable
        }

    TabChanged tab ->
      { model | tab = tab }


parseCsv : String -> Stars
parseCsv source =
  let
    list =
      source
        |> Csv.Parser.parse { fieldSeparator = ',' }
        |> Result.withDefault []
  in
    Maybe.map2 Stars (List.head list) (List.tail list)
      |> Maybe.withDefault { header = [], data = [] }



----------
-- VIEW --
----------

scaleRoom = 60

color =
  { background = "#EAEAF2"
  , scale = "#F6F6F8"
  , text = "#272727"
  , star = "#4C72B0"
  , corona = "#F6F6F8"
  , pointed = "#FFFF00"
  }

type Tab
  = TabSelected
  | TabAxis
  | TabTable
  | TabCsv

view : Model -> Html Msg
view model =
  let
    tabItem name tab =
      li
        [ class <| "tabs-item" ++ if model.tab == tab then " active" else ""
        , onClick <| TabChanged tab
        ]
        [ text name ]
  in
    div [ class "contents" ]
      [ skyView model.mouse model.sight model.pointedIdx model.stars
      , div [ class "controls" ]
          [ ul [ class "tabs" ]
            [ tabItem "Selected" TabSelected
            , tabItem "Axis" TabAxis
            , tabItem "Table" TabTable
            , tabItem "Csv" TabCsv
            ]
          , div [ class "tab-content" ]
            [ case model.tab of
                TabSelected ->
                  starDetailView model.stars model.pointedIdx
                TabAxis ->
                  axisSelectorView model.sight model.stars.header
                TabTable ->
                  starChartView model.sight model.stars
                TabCsv ->
                  div [ HAttr.id "source" ]
                    [ button [ onClick ParseRequested ] [ Html.text "parse" ]
                    , textarea
                      [ HAttr.value model.source
                      , onInput SourceChanged
                      ] []
                    ]
            ]
          ]
      ]

skyView : MouseState -> Sight -> Maybe Int -> Stars -> Html Msg
skyView mouse sight pointed stars =
  let
    bg =
      Svg.rect
        [ SAttr.x <| String.fromInt scaleRoom
        , SAttr.y "0"
        , SAttr.width <| String.fromInt <| sight.width
        , SAttr.height <| String.fromInt <| sight.height
        , SAttr.fill color.background
        , onMouseMove mouse MouseMoved
        ][]

    calcGap : Float -> Float
    calcGap perPx =
      let
        log = logBase 10 perPx
        dec = log - toFloat (floor log)
        coeff =
          if dec > 0.6990 then 10 else
          if dec > 0.3010 then 5 else
          2
      in
        coeff * 10 ^ (floor log + 2) |> toFloat

    xScaleGap = calcGap sight.xPerPx
    yScaleGap = calcGap sight.yPerPx

    xScales = xScalesView xScaleGap sight
    yScales = yScalesView yScaleGap sight

    xHeader =
      stars.header
        |> get sight.xAxisIdx
        |> Maybe.map (\h ->
          Svg.text_
            [ SAttr.x <| String.fromFloat (toFloat sight.width / 2)
            , SAttr.y <| String.fromInt <| sight.height + 30
            , SAttr.fontFamily "sans-serif"
            , SAttr.fontSize "18"
            , SAttr.textAnchor "middle"
            , SAttr.dominantBaseline "text-before-edge"
            , SAttr.fill color.text
            ]
            [ Svg.text <| h ])

    yHeader =
      let
        ox = "0"
        oy = String.fromFloat <| toFloat sight.height / 2
      in
        stars.header
          |> get sight.yAxisIdx
          |> Maybe.map (\h ->
            Svg.text_
              [ SAttr.x ox
              , SAttr.y oy
              , SAttr.fontFamily "sans-serif"
              , SAttr.fontSize "18"
              , SAttr.textAnchor "middle"
              , SAttr.dominantBaseline "text-before-edge"
              , SAttr.transform ("rotate(-90,"++ox++","++oy++")")
              , SAttr.fill color.text
              ]
              [ Svg.text <| h ])

    xWheelArea =
      Svg.rect
        [ SAttr.x <| String.fromInt scaleRoom
        , SAttr.y <| String.fromInt sight.height
        , SAttr.width <| String.fromInt sight.width
        , SAttr.height <| String.fromInt scaleRoom
        , SAttr.fill "transparent"
        , onWheelMove <| WheelMoved (Just X)
        ] []

    yWheelArea =
      Svg.rect
        [ SAttr.x "0"
        , SAttr.y "0"
        , SAttr.width <| String.fromInt scaleRoom
        , SAttr.height <| String.fromInt sight.height
        , SAttr.fill "transparent"
        , onWheelMove <| WheelMoved (Just Y)
        ] []

    stars_ = starsView sight pointed stars.data
  in
    [ bg, xScales, yScales, stars_, xWheelArea, yWheelArea ]
      |> insertIfExist xHeader
      |> insertIfExist yHeader
      |> Svg.svg
        [ SAttr.id "sky"
        , SAttr.viewBox <| "0 0 "
            ++ String.fromInt (sight.width + scaleRoom) ++ " "
            ++ String.fromInt (sight.height + scaleRoom)
        , SAttr.width <| String.fromInt <| sight.width + scaleRoom
        , SAttr.height <| String.fromInt <| sight.height + scaleRoom
        , onWheelMove <| WheelMoved Nothing
        ]
      |> List.singleton
      |> div
        [ SAttr.width <| String.fromInt <| sight.width + scaleRoom
        , SAttr.height <| String.fromInt <| sight.height + scaleRoom
        ]

xScalesView : Float -> Sight -> Svg msg
xScalesView interval sight =
  let
    min = toFloat (ceiling (sight.origin.x / interval)) * interval
    num = toFloat sight.width * sight.xPerPx / interval |> floor
    f = graphToSky sight
    locale = { usLocale | decimals = FormatNumber.Locales.Max 3 }
  in
    List.range 0 num
      |> List.map (\x -> min + toFloat x * interval)
      |> List.map (xScaleView sight locale f)
      |> Svg.g []

xScaleView : Sight -> Locale -> (Point2D -> Point2D) -> Float -> Svg msg
xScaleView sight locale f xOnGraph =
  let
    onSky = f <| Point2D xOnGraph 0.0
    xStr = String.fromFloat <| onSky.x
    line =
      Svg.line
        [ SAttr.x1 xStr
        , SAttr.y1 "0"
        , SAttr.x2 <| String.fromFloat onSky.x
        , SAttr.y2 <| String.fromInt sight.height
        , SAttr.stroke color.scale
        ] []
    text =
      Svg.text_
        [ SAttr.x xStr
        , SAttr.y <| String.fromInt <| sight.height + 10
        , SAttr.fontFamily "sans-serif"
        , SAttr.fontSize "16"
        , SAttr.textAnchor "middle"
        , SAttr.dominantBaseline "text-before-edge"
        , SAttr.fill color.text
        ]
        [ Svg.text <| format locale xOnGraph ]
  in
    Svg.g [] [ line, text ]

yScalesView : Float -> Sight -> Svg msg
yScalesView interval sight =
  let
    min = toFloat (ceiling (sight.origin.y / interval)) * interval
    num = toFloat sight.height * sight.yPerPx / interval |> floor
    f = graphToSky sight
    locale = { usLocale | decimals = FormatNumber.Locales.Max 3 }
  in
    List.range 0 num
      |> List.map (\y -> min + toFloat y * interval)
      |> List.map (yScaleView sight locale f)
      |> Svg.g []

yScaleView : Sight -> Locale -> (Point2D -> Point2D) -> Float -> Svg msg
yScaleView sight locale f yOnGraph =
  let
    onSky = f <| Point2D 0.0 yOnGraph
    yStr = String.fromFloat onSky.y
    line =
      Svg.line
        [ SAttr.x1 <| String.fromInt scaleRoom
        , SAttr.y1 yStr
        , SAttr.x2 <| String.fromInt <| sight.width + scaleRoom
        , SAttr.y2 yStr
        , SAttr.stroke color.scale
        ] []
    text =
      Svg.text_
        [ SAttr.x "50"
        , SAttr.y yStr
        , SAttr.fontFamily "sans-serif"
        , SAttr.fontSize "16"
        , SAttr.textAnchor "end"
        , SAttr.fill color.text
        ]
        [ Svg.text <| format locale yOnGraph ]
  in
    Svg.g [] [ line, text ]

starsView : Sight -> Maybe Int -> List Star -> Svg Msg
starsView sight pointed stars =
  let
    isInSky { x, y } =
      scaleRoom <= x && x <= toFloat (scaleRoom + sight.width)
        && 0 <= y && y <= toFloat sight.height
  in
    stars
      |> List.indexedMap (\idx star ->
        Maybe.map2 (\x y ->
          (idx, Point2D x y |> graphToSky sight))
          (getAsFloat sight.xAxisIdx star)
          (getAsFloat sight.yAxisIdx star))
      |> List.filterMap identity
      |> List.filter(\( _, p ) -> isInSky p)
      |> List.map (\( idx, p ) -> starView pointed idx p)
      |> Svg.g []

starView : Maybe Int -> Int -> Point2D -> Svg Msg
starView pointed idx { x, y } =
  Svg.circle
    [ SAttr.cx <| String.fromFloat x
    , SAttr.cy <| String.fromFloat y
    , SAttr.r "5"
    , SAttr.fill color.star
    , SAttr.stroke <| if pointed == Just idx then color.pointed else color.corona
    , SAttr.strokeWidth <| if pointed == Just idx then "2" else "1"
    , onClick (StarPointed (Just idx))
    ] []

graphToSky : Sight -> Point2D -> Point2D
graphToSky { origin, xPerPx, yPerPx, width, height } { x, y } =
  { x = (x - origin.x) / xPerPx + scaleRoom
  , y = toFloat height + (origin.y - y) / yPerPx
  }

starDetailView : Stars -> Maybe Int -> Html msg
starDetailView { header, data } maybeIdx =
  let
    content =
      case maybeIdx |> Maybe.andThen (\idx -> get idx data) of
        Nothing -> Html.text "no data"
        Just star ->
          let
            tr_ h d =
              tr [] [ th [] [ text h ], td [] [ text d ] ]
          in
            List.map2 tr_ header star
              |> table []
  in
    div
      [ id "star-selected" ]
      [ content ]

axisSelectorView : Sight -> List String -> Html Msg
axisSelectorView sight header =
  let
    toOption selected idx str =
      option
        [ HAttr.value <| String.fromInt idx
        , HAttr.selected <| selected == idx
        ]
        [ Html.text <| Maybe.withDefault "" <| get idx header ]

    xlabel =
      label
        [ HAttr.for "x-axis-select" ]
        [ Html.text <| "x-axis" ]

    xSelector =
      header
        |> List.indexedMap (toOption sight.xAxisIdx)
        |> select
          [ HAttr.id "x-axis-select"
          , onInput (XAxisSelected << Maybe.withDefault 0 << String.toInt)
          , HAttr.value <| String.fromInt sight.xAxisIdx
          ]

    ylabel =
      label
        [ HAttr.for "y-axis-select" ]
        [ Html.text <| "y-axis" ]

    ySelector =
      header
        |> List.indexedMap (toOption sight.yAxisIdx)
        |> select
          [ HAttr.id "y-axis-select"
          , onInput (YAxisSelected << Maybe.withDefault 0 << String.toInt)
          , HAttr.value <| String.fromInt sight.yAxisIdx
          ]
  in
    div [] [ xlabel, xSelector, ylabel, ySelector ]

starChartView : Sight -> Stars -> Html msg
starChartView sight { header, data } =
  let
    rowView : List String -> Html Msg
    rowView star =
      star
        |> List.map (\e -> td [] [ Html.text e ])
        |> tr []

    hd =
      header
        |> List.map (Html.text >> List.singleton >> th [])
        |> tr []
        |> List.singleton
        |> thead []

    bd =
      data
        |> List.map (\star ->
          star
            |> List.map (Html.text >> List.singleton >> td [])
            |> tr [])
        |> tbody []

    tbl = table [] [ hd, bd ]
  in
    div
      [ HAttr.id "star-chart"
      , HAttr.width sight.width
      , HAttr.height 40
      ]
      [ tbl ]



-----------
-- MOUSE --
-----------

type alias MouseState =
  { x : Float
  , y : Float
  , dx : Float
  , dy : Float
  , down : Button
  }

type Button
  = Left
  | Right
  | Middle
  | Other

onClick : msg -> Svg.Attribute msg
onClick msg =
  let
    decoder =
      Json.succeed msg
        |> Json.map stopPropagation
  in
    Svg.Events.custom "click" decoder

onMouseMove : MouseState -> (MouseState -> msg) -> Svg.Attribute msg
onMouseMove state msgMapper =
  mouseStateDecoder state
    |> Json.map msgMapper
    |> Svg.Events.on "mousemove"

mouseStateDecoder : MouseState -> Json.Decoder MouseState
mouseStateDecoder { x, y, dx, dy, down } =
  let
    buttonKind b =
      case b of
        1 -> Left
        2 -> Right
        4 -> Middle
        _ -> Other
  in
    Json.map3
      (\x_ y_ b -> MouseState x_ y_ (x_ - x) (y_ - y) (buttonKind b))
      (Json.field "offsetX" Json.float)
      (Json.field "offsetY" Json.float)
      (Json.field "buttons" Json.int)

onWheelMove : (Float -> msg) -> Svg.Attribute msg
onWheelMove msgMapper =
  let
    decoder =
      Json.field "deltaY" Json.float
        |> Json.map msgMapper
        |> Json.map consumeEvent
  in
    Svg.Events.custom "wheel" decoder


consumeEvent : msg -> CustomDecoder msg
consumeEvent msg =
  { message = msg, stopPropagation = True, preventDefault = True }

stopPropagation : msg -> CustomDecoder msg
stopPropagation msg =
  { message = msg, stopPropagation = True, preventDefault = False }

type alias CustomDecoder msg =
  { message : msg
  , stopPropagation : Bool
  , preventDefault : Bool
  }



----------
-- UTIL --
----------

get : Int -> List a -> Maybe a
get idx list =
  list
    |> List.drop idx
    |> List.head

getAsFloat : Int -> Star -> Maybe Float
getAsFloat idx list =
  list
    |> get idx
    |> Maybe.andThen (String.trim >> String.toFloat)

insertIfExist : Maybe a -> List a -> List a
insertIfExist maybe list =
  case maybe of
    Just e -> e :: list
    Nothing -> list
