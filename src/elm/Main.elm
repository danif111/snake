module Main exposing (..)
import Random exposing (int, generate, list)
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing (Posix, utc, toHour, toMinute, toSecond)
import Setters
import Update
import Json.Decode as Decode

{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }
type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , cherryTime : Int
  , snakeTime : Int
  , time : Int
  , coloredSquare : List Int
  , direction : Key
  , seed : Int
  , cherry : Int
  , score : Int
  , wall : Bool
  , isObstacles : Bool
  , obstacles : List Int
  , gridSize : String
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time time time [500,501,502] ArrowRight -1 -1 0 False False [] "600"
  |> Update.none

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | ToggleWall
  | ToggleObstacle
  | KeyDown Key
  | PlaceSeed Int
  | PlaceCherry Int
  | PlaceObstacles (List Int)
  | SnakeSpeed Posix
  | OnInput String

{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct
 -|   Cmds. -}
moveSnakeUp : List Int -> List Int
moveSnakeUp snake =
    let
       last = Maybe.withDefault 0 (List.head (List.reverse snake))
       new = List.drop 1 snake
       head = (modBy 1600 (last - 40))::[]
    in
    new ++ head

moveSnakeDown : List Int -> List Int
moveSnakeDown snake =
    let
       last = Maybe.withDefault 0 (List.head (List.reverse snake))
       new = List.drop 1 snake
       head = (modBy 1600 (last + 40))::[]
    in
    new ++ head

moveSnakeLeft : List Int -> List Int
moveSnakeLeft snake =
    let
       last = Maybe.withDefault 0 (List.head (List.reverse snake))
       new = List.drop 1 snake
       head = (modBy 1600 (last - 1))::[]
    in
    new ++ head

moveSnakeRight : List Int -> List Int
moveSnakeRight snake =
    let
       last = Maybe.withDefault 0 (List.head (List.reverse snake))
       new = List.drop 1 snake
       head = (modBy 1600 (last + 1))::[]
    in
    new ++ head

updateSquare : Model -> Model
updateSquare ({ coloredSquare, direction } as model) =
        case direction of
        ArrowUp   -> 
            moveSnakeUp coloredSquare
            |> Setters.setColoredSquareIn model
        ArrowDown -> 
            moveSnakeDown coloredSquare
            |> Setters.setColoredSquareIn model
        ArrowLeft -> 
            moveSnakeLeft coloredSquare
            |> Setters.setColoredSquareIn model
        _ -> 
            moveSnakeRight coloredSquare
            |> Setters.setColoredSquareIn model

updateSeed : Model -> ( Model, Cmd Msg )
updateSeed ({ seed } as model) =
  (model, generate PlaceSeed (int 0 1559) )

placeSeed : Int -> Model -> ( Model, Cmd Msg )
placeSeed place model =
  ({model|seed=place},Cmd.none)

updateCherry : Model -> ( Model, Cmd Msg )
updateCherry ({ cherry } as model) =
  (model, generate PlaceCherry (int 0 1559) )

placeCherry : Int -> Model -> ( Model, Cmd Msg )
placeCherry place model =
  ({model|cherry=place},Cmd.none)

updateObstacles : Model -> ( Model, Cmd Msg )
updateObstacles ({ obstacles } as model) =
  (model, generate PlaceObstacles (list 50 (int 0 1559)) )

placeObstacles : List Int -> Model -> ( Model, Cmd Msg )
placeObstacles place model =
  ({model|obstacles=place},Cmd.none)

growSnake : Model -> Model
growSnake ({coloredSquare, direction} as model) =
    let 
       h = Maybe.withDefault 0 (List.head coloredSquare) 
    in
    case direction of
        ArrowUp   -> 
            (modBy 1600 (h+40))::coloredSquare
            |> Setters.setColoredSquareIn model
        ArrowDown -> 
            (modBy 1600 (h-40))::coloredSquare
            |> Setters.setColoredSquareIn model
        ArrowLeft -> 
            (modBy 1600 (h+1))::coloredSquare
            |> Setters.setColoredSquareIn model
        _ -> 
            (modBy 1600 (h-1))::coloredSquare
            |> Setters.setColoredSquareIn model

eatFruit : Model -> (Model, Cmd Msg)
eatFruit ({ coloredSquare, seed, cherry, score} as model) =
  if List.member seed coloredSquare then
    score + 100
    |> Setters.setScoreIn model
    |> growSnake
    |> updateSeed
  else 
    if List.member cherry coloredSquare then
      {model|cherry = -1}
      |> Setters.setScore (score + 100)
      |> Update.none 
    else
      score 
      |> Setters.setScoreIn model
      |> Update.none

gameOver : Model -> Model
gameOver ({coloredSquare, wall, obstacles} as model) =
  let
    snake = List.reverse coloredSquare
    f = Maybe.withDefault 0 (List.head snake)
    s = Maybe.withDefault 0 (List.head (List.drop 1 snake))
    c1 = ((modBy 40 f ) == 0) && ((modBy 40 s)==39) && wall
    c2 = ((modBy 40 s ) == 0) && ((modBy 40 f)==39) && wall
    c3 = (abs(f-s) == 1560) && wall
    c4 = List.member f (List.drop 1 snake)
    c5 = List.member f obstacles
  in
  if c1 || c2 || c3 || c4 || c5 then
      model.time
      |> \time -> Model False time time time time [500,501,502] ArrowRight -1 -1 0 False False [] "600"
  else
      model

toUtcString : Posix -> String
toUtcString time =
  let 
       h = if (toHour utc time) + 2 < 10 then "0" ++ String.fromInt ((toHour utc time) + 2) else String.fromInt ((toHour utc time) + 2)
       m = if (toMinute utc time) < 10 then "0" ++ String.fromInt (toMinute utc time) else String.fromInt (toMinute utc time)
       s = if (toSecond utc time) < 10 then "0" ++ String.fromInt (toSecond utc time) else String.fromInt (toSecond utc time)
 in
 h ++ ":" ++ m ++":"++s

toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
  if gameStarted then
    not gameStarted
    |> Setters.setGameStartedIn model
    |> Update.none
  else
    not gameStarted
    |> Setters.setGameStartedIn model
    |> updateSeed

toggleWall : Model -> ( Model, Cmd Msg )
toggleWall ({ wall, gameStarted } as model) =
  if gameStarted then
    Update.none model
  else
    ({model|wall = not wall}, Cmd.none)

toggleObstacle : Model -> ( Model, Cmd Msg )
toggleObstacle ({ isObstacles, gameStarted } as model) =
  if gameStarted then
    Update.none model
  else
     if isObstacles then
        {model|isObstacles = not isObstacles}
        |> Setters.setObstacles []
        |> Update.none
     else
        {model|isObstacles = not isObstacles}
        |> addObstacles
        

addObstacles : Model -> ( Model, Cmd Msg )
addObstacles ({isObstacles} as model) =
  if isObstacles then
     updateObstacles model
  else
     Update.none model
     

keyDown : Key -> Model -> ( Model, Cmd Msg )
keyDown key model =
  ({model|direction = key}, Cmd.none)

nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
  let 
     time_ = Time.posixToMillis time 
  in
  if time_ - model.lastUpdate >= 1000 then
      if time_ - model.cherryTime >= 10000 then
          if model.cherry > 0 then
            {model|cherry = -1}
            |> Setters.setCherryTime time_
            |> Update.none
          else
            Setters.setCherryTime time_ model
            |> updateCherry 
      else 
        Setters.setTime time_ model
        |> Setters.setLastUpdate time_
        |> Setters.setScore (model.score + 10)
        |> Update.none
  else 
    time_
    |> Setters.setTimeIn model
    |> Update.none

snakeSpeed : Posix -> Model -> ( Model, Cmd Msg )
snakeSpeed time model =
  let 
     time_ = Time.posixToMillis time 
  in
  if time_ - model.snakeTime >= 250 then
    {model|snakeTime = time_}
    |> updateSquare 
    |> gameOver
    |> eatFruit
  else 
    Update.none model

{-| Main update function, mainly used as a router for subfunctions -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleGameLoop -> toggleGameLoop model
    ToggleWall -> toggleWall model
    ToggleObstacle -> toggleObstacle model
    KeyDown key -> keyDown key model
    NextFrame time -> nextFrame time model
    PlaceSeed seed -> placeSeed seed model
    PlaceCherry cherry -> placeCherry cherry model
    PlaceObstacles obstacles -> placeObstacles obstacles model
    SnakeSpeed time -> snakeSpeed time model
    OnInput gridSize -> ({model|gridSize = gridSize}, Cmd.none)

{-| Manage all your view functions here. -}
cell : Int ->List Int -> List Int -> Int -> Int -> Html msg
cell index active obstacles seed cherry =
  let 
     class = if List.member index active then "cell active" else "cell" 
     class1 = if seed == index then ("seed "++ class) else class
     class2 = if List.member index obstacles then ("obstacles "++ class1) else class1
     classfin = if cherry == index then ("cherry "++ class2) else class2
  in
  Html.div [ Attributes.class classfin ] []

numberCells : Int -> List Int -> List Int  -> Int -> Int -> List (Html msg)
numberCells n active obstacles seed cherry =
        case n of 
          0 ->  []
          1 ->  cell 0 active obstacles seed cherry :: []
          x ->  (cell (x-1) active obstacles seed cherry )::(numberCells (x-1) active obstacles seed cherry)


movingSquare : Model -> Html msg
movingSquare { wall, coloredSquare, seed, cherry, obstacles, gridSize } =
  let size =  gridSize ++ "px"  in
  if wall then
     Html.div [ Attributes.class "grid wall", Attributes.style "height" size, Attributes.style "width" size  ](List.reverse (numberCells 1600 coloredSquare obstacles seed cherry))
  else
    Html.div [ Attributes.class "grid", Attributes.style "height" size, Attributes.style "width" size ](List.reverse (numberCells 1600 coloredSquare obstacles seed cherry))

actualTime : Model -> Html msg
actualTime { time } =
  Html.div [ Attributes.class "actual-time" ]
    [ Html.text "Actual time : "
    , time
      |> Time.millisToPosix
      |> toUtcString
      |> Html.text
      |> List.singleton
      |> Html.code []
    ]

actualScore : Model -> Html msg
actualScore ({ score } as model) =
  Html.div [ Attributes.class "actual-time" ]
    [ Html.text "Total Score : "
    , score
      |> String.fromInt
      |> Html.text
      |> List.singleton
      |> Html.code []
    ]

explanations : Model -> Html Msg
explanations ({ gameStarted, wall, isObstacles } as model) =
  let 
     word = if gameStarted then "Stop" else "Start" 
     say = if wall then "Disable" else "Activate"
     write = if isObstacles then "Remove" else "Add"
     classWall = if gameStarted then "btn hide" else "btn"
     size = if gameStarted then "hide" else ""
  in
  Html.div [ Attributes.class "separator" ]
    [ Html.h1 []
      [ Html.text "Welcome to the snake project!" ]
    , actualTime model
    , actualScore model
    , Html.button
      [ Events.onClick ToggleGameLoop, Attributes.class "btn" ]
      [ Html.text (String.join " " [word, "game loop"]) ]
    , Html.button
      [ Events.onClick ToggleWall, Attributes.class classWall]
      [ Html.text (String.join " " [say, "wall"]) ]
    , Html.button
      [ Events.onClick ToggleObstacle, Attributes.class classWall]
      [ Html.text (String.join " " [write, "obstacle"]) ]
    , Html.input 
      [ Events.onInput OnInput, Attributes.value model.gridSize, Attributes.class size] []
    ]

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Html.main_ []
    [ Html.img [ Attributes.src "/logo.svg" ] []
    , explanations model
    , movingSquare model
    ]

{-| Parts for the runtime. Get key presses and subscribe to
 -|   requestAnimationFrame for the game loop. You don't have to bother with
 -|   this. -}
decodeArrow : String -> Decode.Decoder Key
decodeArrow value =
  case value of
    "ArrowUp" -> Decode.succeed ArrowUp
    "ArrowLeft" -> Decode.succeed ArrowLeft
    "ArrowRight" -> Decode.succeed ArrowRight
    "ArrowDown" -> Decode.succeed ArrowDown
    " " -> Decode.succeed Space
    _ -> Decode.fail "Not an arrow"

decodeKey : Decode.Decoder Msg
decodeKey =
  Decode.field "key" Decode.string
  |> Decode.andThen decodeArrow
  |> Decode.map KeyDown

subscriptions : Model -> Sub Msg
subscriptions { gameStarted } =
  let aF = Browser.Events.onAnimationFrame NextFrame
      aG = Browser.Events.onAnimationFrame SnakeSpeed
      base = Browser.Events.onKeyDown decodeKey :: [] in
    Sub.batch (if gameStarted then aF :: aG :: base else base)

{-| Entrypoint of your program -}
main : Program Flags Model Msg
main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    }
