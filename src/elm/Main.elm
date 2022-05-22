module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing (Posix)
import Setters
import Update
import Json.Decode as Decode

{-| Got from JS side, and Model to modify -}

gridSize = Size 40 40
cellSize = Size 20 20
tickFrequency = 100
initialSnakeLength = 20

type alias Flags = { now : Int }
type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , prize : (Maybe Position)
  , score : Int
  , highScore : Int
  , direction : direction
  , snake : NonEmptyList Position
  , coloredSquare : Int
 -- , direction : Key
  }

type alias Snake =
  {
    dir : direction
    , body : List Point
  }
initGame : state -> Int -> (Model, Cmd Msg)
initGame initialState highScore =
  let
     head = computeGridCenter gridSize
     initSnake = NonEmptyList head (List.repeat (initialSnakeLength - 1) head)
  in
  ( { state = initialState
      , gameTicks = 0
      , direction = Update
      , prize = Nothing
      , score = 0
      , snake = initSnake
      , highScore = highScore
      }
      , if (initialState == Active) then placePrize initSnake else Cmd.none
      )

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time 0
  |> Update.none

  -- UPDATE
  type Msg = Tick Time.posix | PlacePrize (Maybe Position) | PointerDownAt (Float, folat )

update : Msg ->Model -> (Model, Cmd Msg)
update msg model =
    if (model.state == Active) then
      case msg of
       PointerDownAt offsetPos ->
         ( { model | direction = pointerOffsetToDirection offsetPos model.direction model.snake.head }
         , Cmd.none
         )

       Tick time ->
          let
             nextHead = adjustPosition model.snake.head model.direction
             atePrize = (Just nextHead) == model.prize
             nextScore = of ate Prize then model.score + 1 else model.score
             nextTail = model.snake.head :: if atePrize then model.snake.tail else stripLast model.snake.tail
             nextSnake = NonEmptyList nextHead nextTail
             nextState = if (isLegalState nextSnake) then Active else Inactive
             nextModel =
               { model 
                | state = nextState
                , snake = nextSnake
                , score = nextScore
                , highScore = Basics.max nextScore model.highScore
                }
            in 
              ( nextModel, if atePrize then placePrize nextSnake else Cmd.none )

          PlacePrize pos ->
            ( { model | prize = pos }, Cmd.none )

        else
          case msg of
           PointerDownAt _ -> if (model.gameTicks >= 0) then initGame Active model.highScore else ( Model, Cmd.none )
           Tick time -> 
             ({ model | gameTicks = model.gameTicks +1}, Cmd.none )
           _-> ( model, Cmd.none )

      isLegalState : NonEmptyList Position -> Bool
      isLegalState snake = (isInGrid gridSize snake.head) && not (List.member snake.head snake.tail)

      placePrize : NonEmptyList Position -> Cmd Msg
      placePrize snake =
        let
           allPoints = compitePointsInGrid gridSize
           snakePoints = NEL.toList snake
           validPoints = List.filter (\p -> not (List.member p snakePoints)) allPoints
        in
        Random.generate PlacePrize (Random.map (\i -> List.head (List.drop i validPoints)) (Random.int 0 (List.length validPoints -1)))

      pointerOffSetToDirection : ( Float, Float ) -> Direction -> Position -> Direction
      pointerOffsetToDirection evenOffSet currentDirection snakeHead = 
        let
           (eventX, eventY) = eventOffset
           dx = eventX - ((toFloat snakeHead.x +0.5) * toFloat cellSize.width)
           dy = eventY -((toFloat snakeHead?y +0.5) * toFloat cellSize.height)

        in
        if (currentDirection == Up || currentDirection == Down) then
          if(dx < 0) then Left else Right
        else
          if (dy < 0) then Up else Down
          


  

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyDown Key

game_trs : Model -> List Html
game_trs model =
    List.map
        ( game_tr model )
        [1..model.size.height]

type Action = Tick Time | KeyPress KeyCode

update : Action -> Model -> ( Model, Effects Action)
update action model = 
  case action of 
    Tick now ->
      ( frame model now, Effects.tick Tick)

    KeyPress key ->
      ( process_key model key, Effects.none )

{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct
 -|   Cmds. -}

updateSquare : Model -> Model
updateSquare ({ coloredSquare } as model) =
  coloredSquare + 1
  |> modBy 1600
  |> Setters.setColoredSquareIn model



toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
  not gameStarted
  |> Setters.setGameStartedIn model
  |> Update.none

arrowUp : Model -> ( Model, Cmd Msg )
arrowUp ({ coloredSquare } as model) =
  coloredSquare - 40
  |> Setters.setColoredSquareIn model
  |> Update.none

arrowDown : Model -> ( Model, Cmd Msg )
arrowDown ({ coloredSquare } as model) =
  coloredSquare + 40
  |> Setters.setColoredSquareIn model
  |> Update.none

arrowLeft : Model -> ( Model, Cmd Msg )
arrowLeft ({ coloredSquare } as model) =
  coloredSquare - 1
  |> Setters.setColoredSquareIn model
  |> Update.none

arrowRight : Model -> ( Model, Cmd Msg )
arrowRight ({ coloredSquare } as model) =
  coloredSquare + 1
  |> Setters.setColoredSquareIn model
  |> Update.none

keyDown : Key -> Model -> ( Model, Cmd Msg )
keyDown key model =
 --({model|direction=key},Cmd.none) 
  case Debug.log "key" key of
    Space   -> toggleGameLoop model
    ArrowUp -> arrowUp model
    ArrowDown  -> arrowDown model
    ArrowLeft  -> arrowLeft model
    ArrowRight -> arrowRight model
  --  _ -> Update.none model

nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
  let time_ = Time.posixToMillis time in
  if time_ - model.lastUpdate >= 1000 then
    updateSquare model
    |> Setters.setTime time_
    |> Setters.setLastUpdate time_
    |> Update.none
  else
    time_
    |> Setters.setTimeIn model
    |> Update.none

{-| Main update function, mainly used as a router for subfunctions -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleGameLoop -> toggleGameLoop model
    KeyDown key -> keyDown key model
    NextFrame time -> nextFrame time model

{-| Manage all your view functions here. -}
cell : Int -> Int -> Html msg
cell index active =
  let class = if active == index then "cell active" else "cell" in
  Html.div [ Attributes.class class ] []

numberCells : Int -> Int -> List (Html msg)
numberCells n active =
        case n of 
          0 ->  []
          1 ->  cell 0 active :: []
          x ->  (cell (x-1) active )::(numberCells (x-1) active)


movingSquare : Model -> Html msg
movingSquare { coloredSquare } =
  Html.div [ Attributes.class "grid" ](List.reverse (numberCells 1600 coloredSquare))
  
  

actualTime : Model -> Html msg
actualTime { time } =
  Html.div [ Attributes.class "actual-time" ]
    [ Html.text "Actual time"
    , time
      |> String.fromInt
      |> Html.text
      |> List.singleton
      |> Html.code []
    ]

explanations : Model -> Html Msg
explanations ({ gameStarted } as model) =
  let word = if gameStarted then "Stop" else "Start" in
  Html.div [ Attributes.class "separator" ]
    [ Html.h1 []
      [ Html.text "Welcome to the snake project!" ]
    , actualTime model
    , Html.button
      [ Events.onClick ToggleGameLoop, Attributes.class "btn" ]
      [ Html.text (String.join " " [word, "game loop"]) ]
    ]

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Html.main_ []
    [ Html.img [ Attributes.src "/logo.svg" ] []
    , explanations model
    , movingSquare model
    , svg [ width "100%"
    , height "auto"
    , viewBox ("0 0 " ++ String.fromInt (gridSize.width * cellSize.width) ++ " " ++ String.fromInt (gridSize.height * cellSize.height))
    , Pointer.onDown (\event -> PointerDownAt event.pointer.offsetPos)
    , Svg.Attributes.style "touch-action: none"
    ]
    (  rect [ width (String.fromInt (gridSize.width * cellSize.width)), height (String.fromInt (gridSize.height * cellSize.height))] []
    :: (maybeToList model.prize |> List.map (\pos -> renderCircle "green" pos))
    ++ List.map (renderCircle "red") model.snake.tail
    ++ [ renderCircle "purple" model.snake.head ]
    ++ [ text_ [ x "5", y "20", Svg.Attributes.style "fill: white"] [ text ("Score: " ++ (String.fromInt model.score))]
       , text_ [ x (String.fromInt ((gridSize.width * cellSize.width) - 5)), y "20", Svg.Attributes.style "fill: white; text-anchor: end"] [ text ("High Score: " ++ (String.fromInt model.highScore))]
       ]
    ++ if (model.state == Inactive && model.gameTicks >= 0) then [ text_ [ x "50%", y "50%", Svg.Attributes.style "dominant-baseline:middle; text-anchor:middle; fill: white; font-size: large"] [ text "Click or touch to begin..." ] ] else []
    )
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

renderCircle : String -> Position -> Html Msg
renderCircle color pos =
  circle [ cx (String.fromInt ((pos.x * cellSize.width) + (cellSize.width // 2)))
         , cy (String.fromInt ((pos.y * cellSize.height) + (cellSize.height // 2)))
         , r (String.fromInt (cellSize.height // 2))
         , fill color
         ] []

decodeKey : Decode.Decoder Msg
decodeKey =
  Decode.field "key" Decode.string
  |> Decode.andThen decodeArrow
  |> Decode.map KeyDown

subscriptions : Model -> Sub Msg
subscriptions { gameStarted } =
  let aF = Browser.Events.onAnimationFrame NextFrame
      base = Browser.Events.onKeyDown decodeKey :: [] in
    Sub.batch (if gameStarted then aF :: base else base)

{-| Entrypoint of your program -}
main : Program Flags Model Msg
main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    }
