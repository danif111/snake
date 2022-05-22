module Setters exposing (..)

import Functions exposing (flip)

setTime : b -> { a | time : b } -> { a | time : b }
setTime time record = { record | time = time }

setTimeIn : { a | time : b } -> b -> { a | time : b }
setTimeIn = flip setTime

setGameStarted : b -> { a | gameStarted : b } -> { a | gameStarted : b }
setGameStarted gameStarted record = { record | gameStarted = gameStarted }

setGameStartedIn : { a | gameStarted : b } -> b -> { a | gameStarted : b }
setGameStartedIn = flip setGameStarted

setColoredSquare : b -> { a | coloredSquare : b } -> { a | coloredSquare : b }
setColoredSquare coloredSquare record = { record | coloredSquare = coloredSquare }

setColoredSquareIn : { a | coloredSquare : b } -> b -> { a | coloredSquare : b }
setColoredSquareIn = flip setColoredSquare

setScore : b -> { a | score : b } -> { a | score : b }
setScore score record = { record | score = score }

setScoreIn : { a | score : b } -> b -> { a | score : b }
setScoreIn = flip setScore

setDirection : b -> { a | direction : b } -> { a | direction : b }
setDirection direction record = { record | direction = direction }

setDirectionIn : { a | direction : b } -> b -> { a | direction : b }
setDirectionIn = flip setDirection

setLastUpdate : b -> { a | lastUpdate : b } -> { a | lastUpdate : b }
setLastUpdate lastUpdate record = { record | lastUpdate = lastUpdate }

setLastUpdateIn : { a | lastUpdate : b } -> b -> { a | lastUpdate : b }
setLastUpdateIn = flip setLastUpdate

setCherryTime : b -> { a | cherryTime : b } -> { a | cherryTime : b }
setCherryTime cherryTime record = { record | cherryTime = cherryTime }

setObstacles : b -> { a | obstacles : b } -> { a | obstacles : b }
setObstacles obstacles record = { record | obstacles = obstacles }