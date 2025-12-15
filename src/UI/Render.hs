module UI.Render 
    ( module UI.Render
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Types
import Constants
import Game.Logic
import Data.List (find)

-- =============================================================================
-- ======= Main Render Function ================================================
-- =============================================================================

-- Render de volledige game state
renderGame :: GameState -> Picture
renderGame gs = Pictures
    [ renderMap gs
    , renderStatusPanel gs
    , renderTravelingOverlay gs
    ]

-- =============================================================================
-- ======= Map Rendering =======================================================
-- =============================================================================

-- Render de kaart met planeten, routes en hazards
renderMap :: GameState -> Picture
renderMap gs = Translate (-350) 0 $ Pictures  -- Verschuif de kaart naar links
    [ renderHazards (galaxyHazards $ gameGalaxy gs)
    , renderRoutes gs
    , renderPlanets gs
    , renderShip gs
    ]

-- Render alle hazards met namen
renderHazards :: [Hazard] -> Picture
renderHazards = Pictures . map renderHazard

renderHazard :: Hazard -> Picture
renderHazard hazard =
    let (x, y) = hazardPos hazard
        radius = fromIntegral (hazardRadius hazard)
        col = getHazardColor (hazardType hazard)
        name = hazardName hazard
        
        -- Hazard cirkel
        circle = Color col $ ThickCircle (radius - 2) 4
        
        -- Hazard naam (klein, onder de cirkel)
        nameText = Translate 0 (-(radius + 15)) $
                  Scale (smallTextScale * 0.8) (smallTextScale * 0.8) $
                  Color textColor $
                  Text name
        
    in Translate x y $ Pictures [circle, nameText]

-- Krijg de kleur voor een hazard type
getHazardColor :: HazardType -> Color
getHazardColor (Asteroid _) = asteroidColor
getHazardColor (Pirates _) = piratesColor
getHazardColor Nebula = nebulaColor
getHazardColor (Radiation _) = radiationColor

-- Render alle routes
renderRoutes :: GameState -> Picture
renderRoutes gs = Pictures $ map (renderRoute gs) (galaxyRoutes $ gameGalaxy gs)

renderRoute :: GameState -> Route -> Picture
renderRoute gs route =
    let fromPlanet = find (\p -> planetName p == routeFrom route) (galaxyPlanets $ gameGalaxy gs)
        toPlanet = find (\p -> planetName p == routeTo route) (galaxyPlanets $ gameGalaxy gs)
        isSelected = case gameSelectedRoute gs of
            Just selected -> selected == route
            Nothing -> False
        col = if isSelected then selectedRouteColor else routeColor
    in case (fromPlanet, toPlanet) of
        (Just from, Just to) -> renderRouteLine (planetPos from) (planetPos to) col (routeDirection route)
        _ -> Blank

-- Render een route lijn met richting
renderRouteLine :: Position -> Position -> Color -> Direction -> Picture
renderRouteLine (x1, y1) (x2, y2) col dir =
    let baseLine = Color col $ Line [(x1, y1), (x2, y2)]
        -- Teken pijl indien directed
        arrow = case dir of
            Forward -> renderArrow (x1, y1) (x2, y2) col
            Backward -> renderArrow (x2, y2) (x1, y1) col
            BiDirectional -> renderBiDirectionalArrow (x1, y1) (x2, y2) col
    in Pictures [baseLine, arrow]

-- Render een pijl op het einde van een lijn
renderArrow :: Position -> Position -> Color -> Picture
renderArrow (x1, y1) (x2, y2) col =
    let dx = x2 - x1
        dy = y2 - y1
        len = sqrt (dx * dx + dy * dy)
        -- Normalized direction
        ndx = dx / len
        ndy = dy / len
        -- Perpendicular
        px = -ndy
        py = ndx
        -- Arrow size
        arrowSize = 10
        -- Arrow tip position (50% along the line)
        tipX = x1 + 0.5 * dx
        tipY = y1 + 0.5 * dy
        -- Arrow points
        p1 = (tipX - arrowSize * ndx + arrowSize * 0.5 * px, tipY - arrowSize * ndy + arrowSize * 0.5 * py)
        p2 = (tipX, tipY)
        p3 = (tipX - arrowSize * ndx - arrowSize * 0.5 * px, tipY - arrowSize * ndy - arrowSize * 0.5 * py)
    in Color col $ Line [p1, p2, p3]

-- Render een dubbele pijl (begin en einde)
renderBiDirectionalArrow :: Position -> Position -> Color -> Picture
renderBiDirectionalArrow start end col =
    Pictures
      [ renderArrow start end col                -- pijl aan het einde
      , renderArrow end start col                -- pijl aan het begin (draai lijn om)
      ]

-- Render alle planeten
renderPlanets :: GameState -> Picture
renderPlanets gs = Pictures $ map (renderPlanet gs) (galaxyPlanets $ gameGalaxy gs)

renderPlanet :: GameState -> Planet -> Picture
renderPlanet gs planet =
    let (x, y) = planetPos planet
        name = planetName planet
        isCurrent = name == gameCurrentPlanet gs
        isTarget = name == gameTargetPlanet gs
        radius = if isCurrent then currentPlanetRadius else planetRadius
        col = getPlanetColor planet isCurrent isTarget
        
        -- Planet cirkel
        planetCircle = Color col $ ThickCircle radius 3
        
        -- Inner circle voor visited planets met effect
        innerCircle = if planetVisited planet
            then Color (makeColor 0.5 0.5 0.5 0.5) $ ThickCircle (radius - 4) 2
            else Blank
        
        -- Planet naam
        nameText = Translate 0 planetNameOffset $ 
                   Scale textScale textScale $ 
                   Color textColor $ 
                   Text name
        
    in Translate x y $ Pictures [planetCircle, innerCircle, nameText]

-- Bepaal de kleur van een planeet
getPlanetColor :: Planet -> Bool -> Bool -> Color
getPlanetColor planet isCurrent isTarget
    | isCurrent = currentPlanetColor
    | isTarget = targetPlanetColor
    | otherwise = case planetEffect planet of
        Just Fuel -> fuelPlanetColor
        Just Repair -> repairPlanetColor
        _ -> planetColor

-- Render het schip (kleine indicator op huidige positie)
renderShip :: GameState -> Picture
renderShip gs =
    let (x, y) = shipPos (gameShip gs)
        shipIndicator = Pictures
            [ Color (makeColor 0.2 0.6 1.0 1.0) $ ThickCircle 8 2
            , Color (makeColor 0.4 0.8 1.0 1.0) $ ThickCircle 4 2
            ]
    in Translate x y shipIndicator

-- =============================================================================
-- ======= Status Panel ========================================================
-- =============================================================================

-- Render het status panel aan de rechterkant (verder naar rechts verplaatst)
renderStatusPanel :: GameState -> Picture
renderStatusPanel gs =
    let panelX = 400  -- Was 500, nu 400 voor betere spacing
        panelY = 0
        panelWidth = 360
        panelHeight = 750
        
        panel = Pictures
            [ -- Panel achtergrond
              Translate panelX panelY $ Color panelColor $ 
                rectangleSolid panelWidth panelHeight
            , -- Panel border
              Translate panelX panelY $ Color panelBorderColor $ 
                rectangleWire panelWidth panelHeight
            , -- Status header
              Translate (panelX - 140) (panelY + 340) $ 
                Scale largeTextScale largeTextScale $ 
                Color textColor $ Text "STATUS"
            , -- Fuel bar - binnen panel
              renderProgressBar (panelX) (panelY + 280) 200 20 
                (shipFuel $ gameShip gs) 100 fuelBarColor "FUEL"
            , -- Hull bar - binnen panel
              renderProgressBar (panelX) (panelY + 230) 200 20 
                (shipShield $ gameShip gs) 100 hullBarColor "SHIELD"
            , -- Time bar - binnen panel
              renderProgressBar (panelX) (panelY + 180) 200 20 
                (shipTime $ gameShip gs) (gameMissionTime gs) timeBarColor "TIME"
            , -- Location info
              Translate (panelX - 160) (panelY + 120) $ 
                Scale smallTextScale smallTextScale $ 
                Color textColor $ Text ("Location: " ++ gameCurrentPlanet gs)
            , -- Routes section
              renderRoutesSection gs panelX panelY
            ]
    in panel

-- Render een progress bar
renderProgressBar :: Float -> Float -> Float -> Float -> Int -> Int -> Color -> String -> Picture
renderProgressBar x y width height current maxVal col label =
    let fillWidth = (fromIntegral (max 0 current) / fromIntegral maxVal) * width
        
        background = Translate x y $ Color barBackgroundColor $ rectangleSolid width height
        fill = Translate (x - width/2 + fillWidth/2) y $ 
               Color col $ rectangleSolid fillWidth height
        border = Translate x y $ Color panelBorderColor $ rectangleWire width height
        
        labelText = Translate (x - width/2 - 50) y $ 
                    Scale smallTextScale smallTextScale $ 
                    Color col $ Text label
        
        valueText = Translate (x + width/2 + 10) y $ 
                    Scale smallTextScale smallTextScale $ 
                    Color textColor $ Text (show (max 0 current))
        
    in Pictures [background, fill, border, labelText, valueText]

-- Render de routes sectie
renderRoutesSection :: GameState -> Float -> Float -> Picture
renderRoutesSection gs panelX panelY
    | gameTraveling gs = Blank  -- Toon geen routes tijdens reizen
    | otherwise =
        let routes = getAvailableRoutes gs
            routesHeader = Translate (panelX - 160) (panelY + 70) $ 
                          Scale smallTextScale smallTextScale $ 
                          Color textColor $ Text ("ROUTES [" ++ show (length routes) ++ "]")
            
            routesList = renderRoutesList gs routes panelX (panelY + 10)
            
            controls = Translate (panelX - 160) (panelY - 320) $
                      Scale (smallTextScale * 0.8) (smallTextScale * 0.8) $
                      Color (makeColor 0.6 0.6 0.6 1.0) $ 
                      Text "↕ Select | Enter: Travel"
            
            controls2 = Translate (panelX - 160) (panelY - 345) $
                       Scale (smallTextScale * 0.8) (smallTextScale * 0.8) $
                       Color (makeColor 0.6 0.6 0.6 1.0) $ 
                       Text "R: Restart | Esc: Menu"
            
        in Pictures [routesHeader, routesList, controls, controls2]

-- Render de lijst van routes met vooraf bepaalde fuel cost
renderRoutesList :: GameState -> [Route] -> Float -> Float -> Picture
renderRoutesList gs routes panelX startY =
    let selected = gameSelectedRoute gs
        ship = gameShip gs
        
        renderRouteItem idx route =
            let yPos = startY - fromIntegral idx * 65
                isSelected = case selected of
                    Just s -> s == route
                    Nothing -> False
                
                dest = case getRouteDestination (gameCurrentPlanet gs) route of
                    Just d -> d
                    Nothing -> "?"
                
                -- Als deze route geselecteerd is en we hebben een geplande fuel cost, toon die
                fuelText = if isSelected && gameTraveling gs == False
                    then case gamePlannedFuelCost gs of
                        Just cost -> show cost
                        Nothing -> 
                            let (minFuel, maxFuel) = routeFuelRange route
                            in show minFuel ++ "-" ++ show maxFuel
                    else 
                        let (minFuel, maxFuel) = routeFuelRange route
                        in show minFuel ++ "-" ++ show maxFuel
                
                timeText = show (routeTime route) ++ "s"
                
                canAfford = canAffordRoute ship route
                
                -- Kleuren
                bgCol = if isSelected 
                    then makeColor 0.2 0.3 0.4 0.8 
                    else makeColor 0.15 0.15 0.25 0.6
                borderCol = if isSelected 
                    then selectedRouteTextColor 
                    else panelBorderColor
                textCol = if canAfford 
                    then (if isSelected then selectedRouteTextColor else textColor)
                    else unavailableRouteColor
                
                -- Background box
                box = Translate panelX yPos $ Pictures
                    [ Color bgCol $ rectangleSolid 320 55
                    , Color borderCol $ rectangleWire 320 55
                    ]
                
                -- Route naam
                nameText = Translate (panelX - 150) (yPos + 12) $
                          Scale smallTextScale smallTextScale $
                          Color textCol $ Text dest
                
                -- Fuel info
                fuelInfo = Translate (panelX - 150) (yPos - 10) $
                          Scale (smallTextScale * 0.85) (smallTextScale * 0.85) $
                          Color (if canAfford then availableRouteColor else unavailableRouteColor) $
                          Text ("Fuel: " ++ fuelText)
                
                -- Time info
                timeInfo = Translate (panelX + 30) (yPos - 10) $
                          Scale (smallTextScale * 0.85) (smallTextScale * 0.85) $
                          Color timeBarColor $
                          Text timeText
                
                -- Hazard warning
                hazardWarning = if routeHasHazards gs route
                    then Translate (panelX + 100) (yPos + 12) $
                         Scale (smallTextScale * 0.8) (smallTextScale * 0.8) $
                         Color (makeColor 1.0 0.4 0.2 1.0) $
                         Text "⚠"
                    else Blank
                
            in Pictures [box, nameText, fuelInfo, timeInfo, hazardWarning]
        
    in Pictures $ zipWith renderRouteItem [0..] routes

-- Check of een route hazards heeft
routeHasHazards :: GameState -> Route -> Bool
routeHasHazards gs route =
    let fromPlanet = find (\p -> planetName p == routeFrom route) (galaxyPlanets $ gameGalaxy gs)
        toPlanet = case getRouteDestination (gameCurrentPlanet gs) route of
            Just dest -> find (\p -> planetName p == dest) (galaxyPlanets $ gameGalaxy gs)
            Nothing -> Nothing
        hazards = case (fromPlanet, toPlanet) of
            (Just from, Just to) -> findHazardsOnRoute (planetPos from) (planetPos to) (galaxyHazards $ gameGalaxy gs)
            _ -> []
    in not (null hazards)

-- =============================================================================
-- ======= Traveling Overlay ===================================================
-- =============================================================================

-- Render overlay tijdens het reizen
renderTravelingOverlay :: GameState -> Picture
renderTravelingOverlay gs
    | not (gameTraveling gs) = Blank
    | otherwise =
        let panelX = 400
            panelY = 0
            
            travelingText = Translate (panelX - 160) (panelY + 70) $
                           Scale largeTextScale largeTextScale $
                           Color (makeColor 0.4 0.8 1.0 1.0) $
                           Text "TRAVELING..."
            
            waitText = Translate (panelX - 160) (panelY + 10) $
                      Scale smallTextScale smallTextScale $
                      Color (makeColor 0.6 0.6 0.7 1.0) $
                      Text "Please wait"
            
            progressBar = renderProgressBar panelX (panelY - 50) 250 15
                         (floor (gameTravelProgress gs * 100)) 100
                         (makeColor 0.4 0.8 1.0 1.0) ""
            
        in Pictures [travelingText, waitText, progressBar]

-- =============================================================================
-- ======= Mission Selection Screen ============================================
-- =============================================================================

-- Render het missie selectie scherm
renderMissionSelection :: Galaxy -> Int -> Picture
renderMissionSelection galaxy selectedIdx =
    let missions = galaxyMissions galaxy
        
        title = Translate (-200) 300 $
               Scale (largeTextScale * 1.5) (largeTextScale * 1.5) $
               Color textColor $ Text "SELECT MISSION"
        
        missionList = renderMissionList missions selectedIdx
        
        instructions = Translate (-200) (-300) $
                      Scale smallTextScale smallTextScale $
                      Color (makeColor 0.6 0.6 0.7 1.0) $
                      Text "↕ Select Mission | Enter: Start"
        
    in Pictures [title, missionList, instructions]

-- Render de missie lijst
renderMissionList :: [Mission] -> Int -> Picture
renderMissionList missions selectedIdx =
    let renderMissionItem idx mission =
            let yPos = 150 - fromIntegral idx * 80
                isSelected = idx == selectedIdx
                
                bgCol = if isSelected 
                    then makeColor 0.2 0.3 0.4 0.9 
                    else makeColor 0.15 0.15 0.25 0.7
                borderCol = if isSelected 
                    then selectedRouteTextColor 
                    else panelBorderColor
                
                box = Translate 0 yPos $ Pictures
                    [ Color bgCol $ rectangleSolid 600 70
                    , Color borderCol $ rectangleWire 600 70
                    ]
                
                nameText = Translate (-280) (yPos + 15) $
                          Scale smallTextScale smallTextScale $
                          Color (if isSelected then selectedRouteTextColor else textColor) $
                          Text (missionName mission)
                
                routeText = Translate (-280) (yPos - 10) $
                           Scale (smallTextScale * 0.85) (smallTextScale * 0.85) $
                           Color (makeColor 0.7 0.7 0.8 1.0) $
                           Text (missionStart mission ++ " → " ++ missionEnd mission)
                
                timeText = Translate (150) (yPos + 0) $
                          Scale (smallTextScale * 0.85) (smallTextScale * 0.85) $
                          Color timeBarColor $
                          Text ("Time: " ++ show (missionTime mission) ++ "s")
                
            in Pictures [box, nameText, routeText, timeText]
        
    in Pictures $ zipWith renderMissionItem [0..] missions

-- =============================================================================
-- ======= End Screens =========================================================
-- =============================================================================

-- Render het "Mission Success" scherm
renderMissionSuccess :: Picture
renderMissionSuccess =
    let title = Translate (-250) 100 $
               Scale (largeTextScale * 2) (largeTextScale * 2) $
               Color (makeColor 0.2 1.0 0.3 1.0) $
               Text "MISSION SUCCESS"
        
        instructions = Translate (-150) (-100) $
                      Scale smallTextScale smallTextScale $
                      Color textColor $
                      Text "Press Esc to return to menu"
        
    in Pictures [title, instructions]

-- Render het "Game Over" scherm
renderGameOver :: FailReason -> Picture
renderGameOver reason =
    let title = Translate (-200) 100 $
               Scale (largeTextScale * 2) (largeTextScale * 2) $
               Color (makeColor 1.0 0.2 0.2 1.0) $
               Text "MISSION FAILED"
        
        reasonText = Translate (-150) 0 $
                    Scale smallTextScale smallTextScale $
                    Color textColor $
                    Text (getFailReasonText reason)
        
        instructions = Translate (-150) (-100) $
                      Scale smallTextScale smallTextScale $
                      Color textColor $
                      Text "R: Restart | Esc: Menu"
        
    in Pictures [title, reasonText, instructions]

-- Krijg de tekst voor een fail reason
getFailReasonText :: FailReason -> String
getFailReasonText OutOfFuel = "Out of Fuel"
getFailReasonText OutOfProtection = "Ship Destroyed"
getFailReasonText OutOfTime = "Time Expired"