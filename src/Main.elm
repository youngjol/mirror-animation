module Main exposing (..)

import Html exposing (..)
-- import Html exposing (Html, Attribute, div, input, text, section, button)

import Html.Events as E
import Browser
import Random
import Svg
import Svg.Attributes as A
import Html.Attributes as A2

--------------------------------------------------------------------------------
{- MAIN -}
--------------------------------------------------------------------------------
main: Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


--------------------------------------------------------------------------------
{- MODEL -}
--------------------------------------------------------------------------------
-- Initial model 
init : () -> (Model, Cmd Msg)
init _ = 
  ( { totalFrames = 1
    , rayAngle = updateRayAngle 1
    , currFrame = defaultFrame
    , reflectCount = 0
    , trajectoryLines = []
    , userAnswer = ""
    , answerCorrect = False
    , message = "Try to find the number of times the light ray was reflected from the object (red) to create the reflection to the right of the viewer (blue)"
    }
  , Cmd.none
  )

type alias Model = 
  { totalFrames : Int 
  , rayAngle : Float
  , currFrame : Int
  , reflectCount : Int
  , trajectoryLines : List Line
  , userAnswer : String
  , answerCorrect : Bool
  , message : String
  }

type alias Line = 
  { initPt : Point
  , angle : Float -- in radians
  , length : Float
  , direction : Direction
  }

type alias Point =
  { x: Float, y: Float }

type Direction 
  = Left | Right 

changeDirection : Direction -> Direction
changeDirection dir =
  case dir of
    Left -> Right
    Right -> Left

minFrames : Int
minFrames = 1

maxFrames : Int
maxFrames = 5

defaultFrame : Int
defaultFrame = 0

frameWidth : Float
frameWidth = 200

frameHeight : Float
frameHeight = 400

objectWidth : Float
objectWidth = 20

objectHeight : Float
objectHeight = 30


--------------------------------------------------------------------------------
{- UPDATE -}
--------------------------------------------------------------------------------
type Msg 
  = Refresh
  | NewTotalFrames Int
  | AddFrame
  | DeleteFrame
  | ReflectLeft
  | Unreflect
  | NewAnswer String

-- Return updated model given a message
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Refresh ->
      ( model, Random.generate NewTotalFrames (Random.int minFrames maxFrames) )
    NewTotalFrames newTotalFrames ->
      ( (updateTotalFrames newTotalFrames model), Cmd.none )
    AddFrame ->
      ( (addFrame model), Cmd.none )
    DeleteFrame -> 
      ( (deleteFrame model), Cmd.none )
    ReflectLeft -> 
      ( (reflectLeft model), Cmd.none )
    Unreflect -> 
      ( (reflectRight model), Cmd.none )
    NewAnswer answer -> 
      ( (updateNewAnswer answer model), Cmd.none )

updateNewAnswer : String -> Model -> Model
updateNewAnswer answer model =
  let 
    intAnswer = String.toInt answer 
    maybeTotalFrames = Just model.totalFrames
  in
    if intAnswer == Nothing then
      { model | message = "Please try an integer"}
    else if intAnswer == maybeTotalFrames then
      { model | answerCorrect = True, message = "Correct!"}
    else -- i.e. intAnswer != maybeTotalFrames
      { model | message = "Please try a different integer" }

reflectLeft : Model -> Model
reflectLeft model =
  if model.reflectCount == (Debug.log "currFrame: " model.currFrame) then -- reflected to the left maximally 
    model 
  else -- i.e. model.reflectCount < model.currFrame 
    let 
      reflectCountUpdatedModel = { model | reflectCount = model.reflectCount + 1 }
    in 
      foldTrajectory reflectCountUpdatedModel

reflectRight : Model -> Model
reflectRight model =
  if model.reflectCount == 0 then -- already reflected to the right maximally 
    model
  else -- i.e. model.reflectCount > 0 
    let 
      reflectCountUpdatedModel = { model | reflectCount = model.reflectCount - 1 }
    in 
      unfoldTrajectory reflectCountUpdatedModel 

updateTotalFrames : Int -> Model -> Model 
updateTotalFrames n model =
  { model | totalFrames = n
                , rayAngle = updateRayAngle n
                , currFrame = defaultFrame
                , reflectCount = 0
                , trajectoryLines = []
  }

addFrame : Model -> Model
addFrame model =
  if model.currFrame == model.totalFrames then model
  else 
    { model | currFrame = model.currFrame + 1, reflectCount = 0
            , trajectoryLines = straightTrajectory model (model.currFrame + 1) } 

deleteFrame : Model -> Model
deleteFrame model = 
  if model.currFrame == 0 then model
  else
     { model | currFrame = model.currFrame - 1, reflectCount = 0
            , trajectoryLines = straightTrajectory model (model.currFrame - 1) } 

straightTrajectory : Model -> Int -> List Line
straightTrajectory model currFrame =
  List.map (frameTrajectoryLine model) (List.range 1 currFrame)

frameTrajectoryLine : Model -> Int -> Line
frameTrajectoryLine model n =
  let
    nTotal = model.totalFrames
    nFloat = toFloat n 
    k = reflectionDownHeight model
    d = 0.8*frameHeight - k 
    startPt = { x = nFloat * frameWidth, y = k - 2 * d * (nFloat-1) }
    len =  frameWidth / (cos model.rayAngle)
  in
    if n < nTotal then -- before correct frame 
      { initPt = startPt, angle = model.rayAngle, length = len, direction = Right }
    else if n == nTotal then -- at correct frame 
      { initPt = startPt, angle = model.rayAngle, length = len/2, direction = Right }
    else -- after correct frame
      { initPt = startPt, angle = model.rayAngle, length = 0, direction = Right }

updateRayAngle : Int -> Float
updateRayAngle n =
  let 
    height = 0.6 * frameHeight
    width =  (toFloat n) * frameWidth
  in 
    atan ( height / width )

foldTrajectory : Model -> Model
foldTrajectory model =
  let
    r = model.reflectCount 
    n = model.currFrame
    lines = model.trajectoryLines
    linesToReflect = List.drop (n-r) lines
    horizAxis = ((toFloat n) - (toFloat r) - 0.5) * frameWidth 
    reflectedLines = List.map reflectLineLeft linesToReflect
    newLines = List.concat [(List.take (n-r) lines), reflectedLines]
  in 
    { model | trajectoryLines = newLines }

unfoldTrajectory : Model -> Model
unfoldTrajectory model =
  let 
    r = model.reflectCount
    n = model.currFrame
    lines = model.trajectoryLines
    linesToReflect = List.drop (n-r-1) lines
    horizAxis = ((toFloat n) - (toFloat r) + 0.5) * frameWidth 
    reflectedLines = List.map reflectLineRight linesToReflect
    newLines =  List.concat [(List.take (n-r-1) lines), reflectedLines] 
  in 
    { model | trajectoryLines = newLines }

reflectLineLeft : Line -> Line
reflectLineLeft line =
  let 
    newAngle = pi - line.angle
  in 
    case line.direction of
      Right ->
        { line | angle = newAngle, direction = changeDirection line.direction }
      Left -> 
        let
          newInitPt = { x = line.initPt.x - 2*frameWidth, y = line.initPt.y }
        in
          { line | initPt = newInitPt, angle = newAngle, direction = changeDirection line.direction }

reflectLineRight : Line -> Line
reflectLineRight line =  
  let 
    newAngle = pi - line.angle
  in 
    case line.direction of
      Right -> 
        let
            newInitPt = { x = line.initPt.x + 2*frameWidth, y = line.initPt.y }
        in 
           { line | initPt = newInitPt, angle = newAngle, direction = changeDirection line.direction }
      Left -> 
        { line | angle = newAngle, direction = changeDirection line.direction }

--------------------------------------------------------------------------------
{- VIEW -}
--------------------------------------------------------------------------------
-- Given model, return HTML page
view : Model -> Html Msg
view model =
  let 
    viewWidth = (String.fromFloat (frameWidth * (toFloat (maxFrames+1))))
    viewHeight = (String.fromFloat frameHeight)
  in 
    div [] [
      section [] 
        [ Svg.svg
          [ A.width viewWidth, A.height viewHeight ]
          (drawAll model)
        ]
    , section []
        [ button [ E.onClick AddFrame ] [ text "Add frame" ]
        , button [ E.onClick DeleteFrame ] [ text "Delete frame"] ]
    , section []
        [ button [ E.onClick ReflectLeft ] [ text "Fold Left" ]
        , button [ E.onClick Unreflect ] [ text "Fold Right"] ]
    , section []
        [ button [E.onClick Refresh ] [ text "Change viewer's direction & reload "]]
    -- , section []
    --     [ input [ A2.type_ "text", A2.placeholder "Number of Reflections", A2.value model.userAnswer, E.onInput NewAnswer ] [] ]
    , section [ A2.style "font-family" "Courier New", A2.style "width" "500px"] 
        [ h1 [ A2.style "font-size" "20px"] [ text "Two Mirror Reflection Exercise" ]
        , div [A2.style "font-size" "14px"] [ text "OBJECTIVE"
                , ul []
                    [ li [] [text "How many times is the light ray (YELLOW) reflected between the two mirrors to reach the viewer from the object?"]
                    , li [] [text "Explore the light trajectory traveling from the object (RED) to the final reflection (GREEN) to the viewer (BLUE)."]]
                ]
        , div [A2.style "font-size" "14px"] 
            [ text "INSTRUCTIONS"
            , ul [] 
                [ li [] [text "'Add frame' or 'Delete frame': add or delete virtual copies of the mirror room"]
                , li [] [text "'Fold Left' or 'Fold Right': reflect the light ray between frames"]
                , li [] [text "'Change viewer's direction & reload': try a different reflection location"]
                ]
            ]
        , div [ A2.style "margin-top" "50px"]
            [ text "Source code: "
            , a [ A2.href "" ] [ text "Github" ]
            ]
        ]
    ]
  
drawAll : Model -> List (Svg.Svg msg)
drawAll model = 
  let 
    d = reflectionDownHeight model
    trajectoryLines = List.map lineToSvgLine model.trajectoryLines 
  in 
    List.concat 
    [ (drawFirstFrame model)
    , (drawFrames d (model.currFrame + 1) "whitesmoke" "lightgrey" "10" ) 
    , trajectoryLines
    ]

lineToSvgLine : Line -> Svg.Svg msg
lineToSvgLine line =
  let 
    x1 = line.initPt.x
    y1 = line.initPt.y 
    finalPt = vectorFinalPt line.initPt line.angle line.length 
    x2 = finalPt.x 
    y2 = finalPt.y 
  in 
    svgLine x1 y1 x2 y2 3 "gold"

reflectionDownHeight : Model -> Float
reflectionDownHeight model = 
  let
    a = (toFloat model.totalFrames) * frameWidth
    b = 0.6 * frameHeight
    c = 0.5 * frameWidth
    d = b * c / a 
  in
    frameHeight - (0.2 * frameHeight + d)

drawFirstFrame : Model -> List (Svg.Svg msg)
drawFirstFrame model = 
  [ (drawFrame "aliceblue" "lightsteelblue" "10" 0)
  , viewerToReflection model
  , drawObject
  , drawViewer
  ]

viewerToReflection : Model  -> Svg.Svg msg
viewerToReflection model =
  let
    startPt = { x = 0.5 * frameWidth, y = 0.8 * frameHeight }
    len = 0.5*frameWidth / (cos model.rayAngle)
  in 
    svgLineFromVector startPt model.rayAngle len 3 "gold"

svgLine : Float -> Float -> Float -> Float -> Float -> String -> Svg.Svg msg
svgLine x1 y1 x2 y2 strokeWidth strokeColor = 
    Svg.line 
    [ A.x1 (String.fromFloat x1)
    , A.y1 (String.fromFloat y1)
    , A.x2 (String.fromFloat x2)
    , A.y2 (String.fromFloat y2)
    , A.stroke strokeColor
    , A.strokeWidth (String.fromFloat strokeWidth)
    ]
    [] 
  
svgLineFromVector : Point -> Float -> Float -> Float -> String -> Svg.Svg msg
svgLineFromVector startPt angle len strokeWidth strokeColor =
  let
    x1 = startPt.x
    y1 = startPt.y
    finalPt = vectorFinalPt startPt angle len
    x2 = finalPt.x
    y2 = finalPt.y
  in 
    svgLine x1 y1 x2 y2 strokeWidth strokeColor

-- Return end point of given vector
vectorFinalPt : Point -> Float -> Float -> Point
vectorFinalPt startPt angle len =
  let
    dx = cos angle
    dy = sin angle 
  in
    {x = toFloat (round (startPt.x + len*dx)), y = toFloat (round (startPt.y - len*dy))}

drawViewer : Svg.Svg msg
drawViewer = 
  Svg.ellipse
  [ A.cx (String.fromFloat (frameWidth / 2))
  , A.cy (String.fromFloat (frameHeight * 0.8))
  , A.rx (String.fromFloat objectWidth)
  , A.ry (String.fromFloat objectHeight)
  , A.fill "cornflowerblue"
  ]
  [] 

drawObject : Svg.Svg msg
drawObject = 
  Svg.ellipse
  [ A.cx (String.fromFloat (frameWidth / 2))
  , A.cy (String.fromFloat (frameHeight * 0.2))
  , A.rx (String.fromFloat objectWidth)
  , A.ry (String.fromFloat objectHeight)
  , A.fill "tomato"
  ]
  [] 

drawFrames : Float -> Int -> String -> String -> String -> List (Svg.Svg msg)
drawFrames refHeight numFrames fillColor lineColor width =
  List.concat
  [ List.map (drawFrame fillColor lineColor width) (List.range 1 (numFrames-1))
  , [drawReflection refHeight]
  , List.map drawVirtualObject (List.range 1 (numFrames-1))
  ]

drawReflection : Float -> Svg.Svg msg
drawReflection refHeight =
  let 
    x1 = frameWidth
    y1 = refHeight - objectHeight/2
    x2 = frameWidth 
    y2 = refHeight + objectHeight/2
  in 
    svgLine x1 y1 x2 y2 10 "limegreen"

drawFrame : String -> String -> String -> Int -> Svg.Svg msg
drawFrame fillColor lineColor width n = 
  let 
    x = String.fromFloat ((toFloat n)*frameWidth)
  in 
    Svg.rect
    [ A.x x
    , A.y "0"
    , A.width (String.fromFloat frameWidth)
    , A.height (String.fromFloat frameHeight)
    , A.stroke lineColor
    , A.strokeWidth width 
    , A.fill fillColor
    ]
    []

drawVirtualObject : Int -> Svg.Svg msg
drawVirtualObject n =
  let 
    x = String.fromFloat ( ((toFloat n) + 1/2)*frameWidth)
  in
    Svg.ellipse
    [ A.cx x
    , A.cy (String.fromFloat (frameHeight * 0.2))
    , A.rx (String.fromFloat objectWidth)
    , A.ry (String.fromFloat objectHeight)
    , A.fill "darkSalmon"
    ]
    [] 

--------------------------------------------------------------------------------
{- SUBSCRIPTIONS -}
--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none