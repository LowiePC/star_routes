module Game.Logic
    ( module Game.Logic
    ) where

import Types
import Constants
import System.Random (StdGen, randomR)
import Data.List (find)
    
-- =============================================================================
-- ======= Initialisation ======================================================
-- =============================================================================

initGameState :: Galaxy -> Mission -> StdGen -> Maybe GameState
initGameState galaxy mission rng = do
    startPlanet <- find (\p -> planetName p == missionStart mission) (galaxyPlanets galaxy)
    let ship = Ship
            { shipPos = planetPos startPlanet
            , shipFuel = initialFuel
            , shipShield = initialShield
            , shipTime = missionTime mission
            }
    return $ GameState
        { gameGalaxy = galaxy
        , gameShip = ship
        , gameCurrentPlanet = missionStart mission
        , gameTargetPlanet = missionEnd mission
        , gameMissionTime = missionTime mission
        , gameSelectedRoute = Nothing
        , gameTraveling = False
        , gameTravelProgress = 0.0
        , gamePlannedFuelCost = Nothing
        , gameTravelStartFuel = Nothing
        , gameTravelStartTime = Nothing
        , gameHazardsApplied = []
        , gameRng = rng
        , gameStatus = Playing
        }

-- =============================================================================
-- ======= Route Logic =========================================================
-- =============================================================================

getAvailableRoutes :: GameState -> [Route]
getAvailableRoutes gs = filter isAvailable allRoutes
    where
        currentPos = gameCurrentPlanet gs
        allRoutes = galaxyRoutes (gameGalaxy gs)

        isAvailable route = 
            case routeDirection route of 
                Forward -> routeFrom route == currentPos
                Backward -> routeTo route == currentPos
                BiDirectional -> routeFrom route == currentPos || routeTo route == currentPos  

getRouteDestination :: String -> Route -> Maybe String
getRouteDestination current route
    | routeFrom route == current && routeDirection route /= Backward = Just (routeTo route)
    | routeTo route == current && routeDirection route /= Forward = Just (routeFrom route)
    | otherwise = Nothing

canAffordRoute :: Ship -> Route -> Bool
canAffordRoute ship route = 
    shipFuel ship >= fst (routeFuelRange route) && 
    shipTime ship >= routeTime route

-- =============================================================================
-- ======= Travel Logica =======================================================
-- =============================================================================

-- Start het reizen over een route - bepaal fuel cost vooraf
startTravel :: Route -> GameState -> GameState
startTravel route gs =
    let (minFuel, maxFuel) = routeFuelRange route
        (fuelCost, newRng) = randomR (minFuel, maxFuel) (gameRng gs)
        currentShip = gameShip gs
    in gs
        { gameSelectedRoute = Just route
        , gameTraveling = True
        , gameTravelProgress = 0.0
        , gamePlannedFuelCost = Just fuelCost
        , gameTravelStartFuel = Just (shipFuel currentShip)
        , gameTravelStartTime = Just (shipTime currentShip)
        , gameHazardsApplied = []
        , gameRng = newRng
        }

-- Update de reis voortgang (delta is de tijd sinds vorige frame in seconden)
updateTravel :: Float -> GameState -> GameState
updateTravel delta gs
    | not (gameTraveling gs) = gs
    | otherwise = case gameSelectedRoute gs of
        Nothing -> gs
        Just route ->
            let routeTimeF = fromIntegral (routeTime route)
                progressDelta = delta / routeTimeF
                newProgress = min 1.0 (gameTravelProgress gs + progressDelta)
                
                -- Update ship position tijdens reis
                fromPlanet = find (\p -> planetName p == routeFrom route) (galaxyPlanets $ gameGalaxy gs)
                toPlanet = case getRouteDestination (gameCurrentPlanet gs) route of
                    Just dest -> find (\p -> planetName p == dest) (galaxyPlanets $ gameGalaxy gs)
                    Nothing -> Nothing
                
                newShipPos = case (fromPlanet, toPlanet) of
                    (Just from, Just to) -> interpolatePosition (planetPos from) (planetPos to) newProgress
                    _ -> shipPos (gameShip gs)
                
                -- Update fuel en tijd real-time
                ship = gameShip gs
                
                -- Bereken hoeveel fuel en tijd we moeten aftrekken
                Just startFuel = gameTravelStartFuel gs
                Just startTime = gameTravelStartTime gs
                Just plannedFuel = gamePlannedFuelCost gs
                
                targetFuel = startFuel - floor (fromIntegral plannedFuel * newProgress)
                targetTime = startTime - floor (fromIntegral (routeTime route) * newProgress)
                
                ship1 = ship 
                    { shipPos = newShipPos
                    , shipFuel = targetFuel
                    , shipTime = targetTime
                    }
                
                -- Check hazards op huidige positie en pas toe indien nodig
                (ship2, newHazardsApplied, newRng) = 
                    applyHazardsAtProgress gs route ship1 newProgress (gameHazardsApplied gs) (gameRng gs)
                
                -- Update game state
                gs1 = gs 
                    { gameShip = ship2
                    , gameTravelProgress = newProgress
                    , gameHazardsApplied = newHazardsApplied
                    , gameRng = newRng
                    }
                
                -- Check game status tijdens reizen
                gs2 = checkGameStatus gs1
                
            in if newProgress >= 1.0 && gameStatus gs2 == Playing
               then completeTravel route gs2
               else gs2

-- Interpoleer tussen twee posities
interpolatePosition :: Position -> Position -> Float -> Position
interpolatePosition (x1, y1) (x2, y2) t =
    (x1 + t * (x2 - x1), y1 + t * (y2 - y1))

-- Pas hazards toe op bepaalde progress punten
applyHazardsAtProgress :: GameState -> Route -> Ship -> Float -> [String] -> StdGen -> (Ship, [String], StdGen)
applyHazardsAtProgress gs route ship progress appliedHazards rng =
    let fromPlanet = find (\p -> planetName p == routeFrom route) (galaxyPlanets $ gameGalaxy gs)
        toPlanet = case getRouteDestination (gameCurrentPlanet gs) route of
            Just dest -> find (\p -> planetName p == dest) (galaxyPlanets $ gameGalaxy gs)
            Nothing -> Nothing
        
        hazardsOnRoute = case (fromPlanet, toPlanet) of
            (Just from, Just to) -> findHazardsOnRoute (planetPos from) (planetPos to) (galaxyHazards $ gameGalaxy gs)
            _ -> []
        
        -- Vind hazards die we zijn gepasseerd maar nog niet hebben toegepast
        (x1, y1) = case fromPlanet of Just p -> planetPos p; Nothing -> (0, 0)
        (x2, y2) = case toPlanet of Just p -> planetPos p; Nothing -> (0, 0)
        currentPos = interpolatePosition (x1, y1) (x2, y2) progress
        
        -- Check welke hazards we zijn gepasseerd
        hazardsToApply = filter (\h -> 
            not (hazardName h `elem` appliedHazards) &&
            hasPassedHazard (x1, y1) currentPos h) hazardsOnRoute
        
    in foldl (\(s, applied, r) h -> 
            let (newShip, newRng) = applyHazardEffect h s r
                newApplied = hazardName h : applied
            in (newShip, newApplied, newRng)
        ) (ship, appliedHazards, rng) hazardsToApply

-- Check of we een hazard zijn gepasseerd
hasPassedHazard :: Position -> Position -> Hazard -> Bool
hasPassedHazard start current hazard =
    let (hx, hy) = hazardPos hazard
        (sx, sy) = start
        (cx, cy) = current
        -- Check of de huidige positie dichtbij genoeg is bij de hazard
        distToCurrent = sqrt ((cx - hx) * (cx - hx) + (cy - hy) * (cy - hy))
        radius = fromIntegral (hazardRadius hazard)
    in distToCurrent <= radius

-- Voltooi de reis
completeTravel :: Route -> GameState -> GameState
completeTravel route gs = case getRouteDestination (gameCurrentPlanet gs) route of
    Nothing -> gs
    Just destination ->
        let -- Vind de doelplaneet
            destPlanet = find (\p -> planetName p == destination) (galaxyPlanets $ gameGalaxy gs)
            destPos = maybe (shipPos $ gameShip gs) planetPos destPlanet
            ship = (gameShip gs) { shipPos = destPos }
            
            -- Update state met nieuwe positie
            gs1 = gs
                { gameShip = ship
                , gameCurrentPlanet = destination
                , gameTraveling = False
                , gameTravelProgress = 0.0
                , gameSelectedRoute = Nothing
                , gamePlannedFuelCost = Nothing
                , gameTravelStartFuel = Nothing
                , gameTravelStartTime = Nothing
                , gameHazardsApplied = []
                }
            
            -- Pas planet effect toe (indien nog niet bezocht)
            gs2 = applyPlanetEffect destination gs1
            
            -- Check win/lose condities
        in checkGameStatus gs2

-- =============================================================================
-- ======= Hazard Logica =======================================================
-- =============================================================================

-- Vind alle hazards die een route kruisen
findHazardsOnRoute :: Position -> Position -> [Hazard] -> [Hazard]
findHazardsOnRoute from to hazards = filter (intersectsRoute from to) hazards

-- Check of een hazard een route kruist
intersectsRoute :: Position -> Position -> Hazard -> Bool
intersectsRoute (x1, y1) (x2, y2) hazard =
    let (hx, hy) = hazardPos hazard
        radius = fromIntegral (hazardRadius hazard)
        
        -- Bereken de kortste afstand van het hazard centrum tot de lijn
        dx = x2 - x1
        dy = y2 - y1
        lengthSquared = dx * dx + dy * dy
        
        -- Als de lijn eigenlijk een punt is
        distance = if lengthSquared == 0
            then sqrt ((hx - x1) * (hx - x1) + (hy - y1) * (hy - y1))
            else
                let t = max 0 (min 1 (((hx - x1) * dx + (hy - y1) * dy) / lengthSquared))
                    projX = x1 + t * dx
                    projY = y1 + t * dy
                in sqrt ((hx - projX) * (hx - projX) + (hy - projY) * (hy - projY))
    
    in distance <= radius

-- Pas een hazard effect toe op het schip
applyHazardEffect :: Hazard -> Ship -> StdGen -> (Ship, StdGen)
applyHazardEffect hazard ship rng =
    case hazardType hazard of
        Asteroid dmg ->
            (ship { shipShield = shipShield ship - dmg }, rng)
        
        Pirates loss ->
            (ship { shipFuel = shipFuel ship - loss }, rng)
        
        Radiation dmg ->
            (ship { shipShield = shipShield ship - dmg }, rng)
        
        Nebula ->
            let currentFuel = shipFuel ship
                (lossPercent, newRng) = randomR (nebulaMinLoss, nebulaMaxLoss) rng
                loss = (currentFuel * lossPercent) `div` 100
            in (ship { shipFuel = currentFuel - loss }, newRng)

-- =============================================================================
-- ======= Planet Effect Logica ================================================
-- =============================================================================

-- Pas het effect van een planeet toe (indien nog niet bezocht)
applyPlanetEffect :: String -> GameState -> GameState
applyPlanetEffect planetName gs =
    case find (\p -> Types.planetName p == planetName) (galaxyPlanets $ gameGalaxy gs) of
        Nothing -> gs
        Just planet ->
            if planetVisited planet
            then gs
            else
                let effect = planetEffect planet
                    ship = gameShip gs
                    newShip = case effect of
                        Just Fuel -> ship { shipFuel = min 100 (shipFuel ship + fuelRefil) }
                        Just Repair -> ship { shipShield = min 100 (shipShield ship + shieldRefil) }
                        _ -> ship
                    
                    -- Update planet als bezocht
                    updatedPlanets = map (\p ->
                        if Types.planetName p == planetName
                        then p { planetVisited = True }
                        else p) (galaxyPlanets $ gameGalaxy gs)
                    
                    updatedGalaxy = (gameGalaxy gs) { galaxyPlanets = updatedPlanets }
                
                in gs { gameShip = newShip, gameGalaxy = updatedGalaxy }

-- =============================================================================
-- ======= Win/Lose Condities ==================================================
-- =============================================================================

-- Check of de game gewonnen of verloren is
checkGameStatus :: GameState -> GameState
checkGameStatus gs
    | shipFuel (gameShip gs) <= 0 = gs { gameStatus = MissionFailed OutOfFuel }
    | shipShield (gameShip gs) <= 0 = gs { gameStatus = MissionFailed OutOfProtection }
    | shipTime (gameShip gs) <= 0 = gs { gameStatus = MissionFailed OutOfTime }
    | gameCurrentPlanet gs == gameTargetPlanet gs = gs { gameStatus = MissionSuccess }
    | otherwise = gs

-- Check of de game nog aan de gang is
isGameActive :: GameState -> Bool
isGameActive gs = gameStatus gs == Playing

-- =============================================================================
-- ======= Route Selectie ======================================================
-- =============================================================================

-- Selecteer de volgende route in de lijst
selectNextRoute :: GameState -> GameState
selectNextRoute gs =
    let available = getAvailableRoutes gs
        current = gameSelectedRoute gs
    in case (current, available) of
        (_, []) -> gs
        (Nothing, r:_) -> gs { gameSelectedRoute = Just r }
        (Just curr, _) ->
            case dropWhile (/= curr) available of
                [] -> gs { gameSelectedRoute = Just (head available) }
                [_] -> gs { gameSelectedRoute = Just (head available) }
                (_:next:_) -> gs { gameSelectedRoute = Just next }

-- Selecteer de vorige route in de lijst
selectPreviousRoute :: GameState -> GameState
selectPreviousRoute gs =
    let available = getAvailableRoutes gs
        current = gameSelectedRoute gs
    in case (current, available) of
        (_, []) -> gs
        (Nothing, _) -> gs { gameSelectedRoute = Just (last available) }
        (Just curr, _) ->
            case takeWhile (/= curr) available of
                [] -> gs { gameSelectedRoute = Just (last available) }
                prev -> gs { gameSelectedRoute = Just (last prev) }

-- Bevestig de geselecteerde route en start reizen
confirmRoute :: GameState -> GameState
confirmRoute gs = case gameSelectedRoute gs of
    Nothing -> gs
    Just route ->
        if canAffordRoute (gameShip gs) route
        then startTravel route gs
        else gs

-- =============================================================================
-- ======= Utility Functies ====================================================
-- =============================================================================

-- Reset de game state (herstart missie) - reset ook planetVisited flags
resetGameState :: GameState -> Mission -> StdGen -> Maybe GameState
resetGameState gs mission rng = 
    let galaxy = gameGalaxy gs
        -- Reset alle planet visited flags
        resetPlanets = map (\p -> p { planetVisited = False }) (galaxyPlanets galaxy)
        resetGalaxy = galaxy { galaxyPlanets = resetPlanets }
    in initGameState resetGalaxy mission rng

-- Krijg de huidige missie tijd in seconden
getRemainingTime :: GameState -> Int
getRemainingTime = shipTime . gameShip

-- Krijg informatie over de huidige planeet
getCurrentPlanetInfo :: GameState -> Maybe Planet
getCurrentPlanetInfo gs = find (\p -> planetName p == gameCurrentPlanet gs) (galaxyPlanets $ gameGalaxy gs)