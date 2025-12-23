module Main (main) where

import Test.HUnit
import Text.Parsec (parse)
import System.Random (mkStdGen)

import Types
import Parser
import Game.Logic
import Constants

-- =============================================================================
-- ======= Parser Tests ========================================================
-- =============================================================================

testParsePlanet :: Test
testParsePlanet = TestList
    [ "parse planet with fuel effect" ~: 
        case parse parsePlanet "" "planet \"Earth\" at (0, 0) type fuel" of
            Right p -> do
                planetName p @?= "Earth"
                planetPos p @?= (0, 0)
                planetEffect p @?= Just Fuel
            Left err -> assertFailure $ "Parse failed: " ++ show err
    
    , "parse planet with repair effect" ~:
        case parse parsePlanet "" "planet \"Mars\" at (100, 50) type repair" of
            Right p -> do
                planetName p @?= "Mars"
                planetPos p @?= (100, 50)
                planetEffect p @?= Just Repair
            Left err -> assertFailure $ "Parse failed: " ++ show err
    
    , "parse planet without effect" ~:
        case parse parsePlanet "" "planet \"Venus\" at (-50, 100)" of
            Right p -> do
                planetName p @?= "Venus"
                planetPos p @?= (-50, 100)
                planetEffect p @?= Just None
            Left err -> assertFailure $ "Parse failed: " ++ show err
    ]

testParseRoute :: Test
testParseRoute = TestList
    [ "parse forward route" ~:
        case parse parseRoute "" "route \"A\" --> \"B\" fuel 10-20 time 5" of
            Right r -> do
                routeFrom r @?= "A"
                routeTo r @?= "B"
                routeDirection r @?= Forward
                routeFuelRange r @?= (10, 20)
                routeTime r @?= 5
            Left err -> assertFailure $ "Parse failed: " ++ show err
    
    , "parse bidirectional route" ~:
        case parse parseRoute "" "route \"X\" <-> \"Y\" fuel 15-25" of
            Right r -> do
                routeDirection r @?= BiDirectional
                routeTime r @?= defaultRouteTime
            Left err -> assertFailure $ "Parse failed: " ++ show err
    
    , "parse backward route" ~:
        case parse parseRoute "" "route \"C\" <-- \"D\" fuel 5-15 time 3" of
            Right r -> do
                routeDirection r @?= Backward
                routeTime r @?= 3
            Left err -> assertFailure $ "Parse failed: " ++ show err
    ]

testParseHazard :: Test
testParseHazard = TestList
    [ "parse asteroid hazard" ~:
        case parse parseHazard "" "hazard asteroid \"Belt\" at (50, 50) radius 30 damage 20" of
            Right h -> do
                hazardName h @?= "Belt"
                hazardPos h @?= (50, 50)
                hazardRadius h @?= 30
                case hazardType h of
                    Asteroid dmg -> dmg @?= 20
                    _ -> assertFailure "Wrong hazard type"
            Left err -> assertFailure $ "Parse failed: " ++ show err
    
    , "parse pirates hazard" ~:
        case parse parseHazard "" "hazard pirates \"Hideout\" at (100, 100) radius 40 fuelLoss 15" of
            Right h -> do
                case hazardType h of
                    Pirates loss -> loss @?= 15
                    _ -> assertFailure "Wrong hazard type"
            Left err -> assertFailure $ "Parse failed: " ++ show err
    
    , "parse nebula hazard" ~:
        case parse parseHazard "" "hazard nebula \"Mist\" at (0, 100) radius 80" of
            Right h -> do
                hazardType h @?= Nebula
            Left err -> assertFailure $ "Parse failed: " ++ show err
    
    , "parse radiation hazard" ~:
        case parse parseHazard "" "hazard radiation \"Zone\" at (-50, 0) radius 60 damage 25" of
            Right h -> do
                case hazardType h of
                    Radiation dmg -> dmg @?= 25
                    _ -> assertFailure "Wrong hazard type"
            Left err -> assertFailure $ "Parse failed: " ++ show err
    ]

testParseMission :: Test
testParseMission = TestList
    [ "parse mission" ~:
        case parse parseMission "" "mission \"Test Mission\" from \"Start\" to \"End\" timeLimit 60" of
            Right m -> do
                missionName m @?= "Test Mission"
                missionStart m @?= "Start"
                missionEnd m @?= "End"
                missionTime m @?= 60
            Left err -> assertFailure $ "Parse failed: " ++ show err
    ]

testParseFullConfig :: Test
testParseFullConfig = "parse complete config file" ~:
    let configText = unlines
            [ "# Test config"
            , "planet \"Earth\" at (0, 0) type fuel"
            , "planet \"Mars\" at (100, 0)"
            , ""
            , "route \"Earth\" --> \"Mars\" fuel 20-30 time 5"
            , ""
            , "hazard asteroid \"Belt\" at (50, 0) radius 40 damage 15"
            , ""
            , "mission \"First Steps\" from \"Earth\" to \"Mars\" timeLimit 30"
            ]
    in case parseConfigFile configText of
        Right galaxy -> do
            length (galaxyPlanets galaxy) @?= 2
            length (galaxyRoutes galaxy) @?= 1
            length (galaxyHazards galaxy) @?= 1
            length (galaxyMissions galaxy) @?= 1
        Left err -> assertFailure $ "Parse failed: " ++ show err

-- =============================================================================
-- ======= Game Logic Tests ====================================================
-- =============================================================================

-- Helper function to create a simple test galaxy
createTestGalaxy :: Galaxy
createTestGalaxy = Galaxy
    { galaxyPlanets = 
        [ Planet "Start" (0, 0) (Just Fuel) False
        , Planet "Mid" (100, 0) (Just Repair) False
        , Planet "End" (200, 0) Nothing False
        ]
    , galaxyRoutes =
        [ Route "Start" "Mid" Forward (10, 15) 5
        , Route "Mid" "End" Forward (20, 25) 5
        ]
    , galaxyHazards =
        [ Hazard (Asteroid 20) "TestAsteroid" (50, 0) 30
        ]
    , galaxyMissions =
        [ Mission "Test Mission" "Start" "End" 60
        ]
    }

testInitGameState :: Test
testInitGameState = "initialize game state" ~:
    let galaxy = createTestGalaxy
        mission = head $ galaxyMissions galaxy
        rng = mkStdGen 42
    in case initGameState galaxy mission rng of
        Just gs -> do
            gameCurrentPlanet gs @?= "Start"
            gameTargetPlanet gs @?= "End"
            shipFuel (gameShip gs) @?= initialFuel
            shipShield (gameShip gs) @?= initialShield
            gameStatus gs @?= Playing
        Nothing -> assertFailure "Failed to initialize game state"

testGetAvailableRoutes :: Test
testGetAvailableRoutes = "get available routes from current planet" ~:
    let galaxy = createTestGalaxy
        mission = head $ galaxyMissions galaxy
        rng = mkStdGen 42
    in case initGameState galaxy mission rng of
        Just gs -> do
            let routes = getAvailableRoutes gs
            length routes @?= 1
            routeFrom (head routes) @?= "Start"
        Nothing -> assertFailure "Failed to initialize game state"

testCanAffordRoute :: Test
testCanAffordRoute = TestList
    [ "ship can afford route" ~:
        let ship = Ship (0, 0) 50 100 30
            route = Route "A" "B" Forward (20, 30) 5
        in canAffordRoute ship route @?= True
    
    , "ship cannot afford route (not enough fuel)" ~:
        let ship = Ship (0, 0) 10 100 30
            route = Route "A" "B" Forward (20, 30) 5
        in canAffordRoute ship route @?= False
    
    , "ship cannot afford route (not enough time)" ~:
        let ship = Ship (0, 0) 50 100 3
            route = Route "A" "B" Forward (20, 30) 5
        in canAffordRoute ship route @?= False
    ]

testHazardIntersection :: Test
testHazardIntersection = TestList
    [ "hazard intersects route" ~:
        let hazard = Hazard (Asteroid 10) "Test" (50, 0) 30
            from = (0, 0)
            to = (100, 0)
        in intersectsRoute from to hazard @?= True
    
    , "hazard does not intersect route" ~:
        let hazard = Hazard (Asteroid 10) "Test" (50, 100) 20
            from = (0, 0)
            to = (100, 0)
        in intersectsRoute from to hazard @?= False
    ]

testApplyHazardEffect :: Test
testApplyHazardEffect = TestList
    [ "asteroid damages shield" ~:
        let ship = Ship (0, 0) 100 100 30
            hazard = Hazard (Asteroid 20) "Test" (0, 0) 10
            rng = mkStdGen 42
            (newShip, _) = applyHazardEffect hazard ship rng
        in shipShield newShip @?= 80
    
    , "pirates steal fuel" ~:
        let ship = Ship (0, 0) 100 100 30
            hazard = Hazard (Pirates 15) "Test" (0, 0) 10
            rng = mkStdGen 42
            (newShip, _) = applyHazardEffect hazard ship rng
        in shipFuel newShip @?= 85
    
    , "radiation damages shield" ~:
        let ship = Ship (0, 0) 100 100 30
            hazard = Hazard (Radiation 25) "Test" (0, 0) 10
            rng = mkStdGen 42
            (newShip, _) = applyHazardEffect hazard ship rng
        in shipShield newShip @?= 75
    
    , "nebula drains fuel randomly" ~:
        let ship = Ship (0, 0) 100 100 30
            hazard = Hazard Nebula "Test" (0, 0) 10
            rng = mkStdGen 42
            (newShip, _) = applyHazardEffect hazard ship rng
            fuelLoss = 100 - shipFuel newShip
        in assertBool "Fuel loss should be between 10 and 50" 
            (fuelLoss >= 10 && fuelLoss <= 50)
    ]

testPlanetEffects :: Test
testPlanetEffects = "apply planet effect once" ~:
    let galaxy = createTestGalaxy
        mission = head $ galaxyMissions galaxy
        rng = mkStdGen 42
    in case initGameState galaxy mission rng of
        Just gs -> do
            -- First visit should apply effect
            let gs1 = applyPlanetEffect "Start" gs
            shipFuel (gameShip gs1) @?= min 100 (initialFuel + fuelRefil)
            
            -- Second visit should not apply effect
            let planet = head $ filter (\p -> planetName p == "Start") (galaxyPlanets $ gameGalaxy gs1)
            planetVisited planet @?= True
            
            let gs2 = applyPlanetEffect "Start" gs1
            shipFuel (gameShip gs2) @?= shipFuel (gameShip gs1)
        Nothing -> assertFailure "Failed to initialize game state"

testWinCondition :: Test
testWinCondition = "win condition when reaching target" ~:
    let galaxy = createTestGalaxy
        mission = head $ galaxyMissions galaxy
        rng = mkStdGen 42
    in case initGameState galaxy mission rng of
        Just gs -> do
            let gs' = gs { gameCurrentPlanet = "End" }
            let gsChecked = checkGameStatus gs'
            gameStatus gsChecked @?= MissionSuccess
        Nothing -> assertFailure "Failed to initialize game state"

testLoseConditions :: Test
testLoseConditions = TestList
    [ "lose when out of fuel" ~:
        let galaxy = createTestGalaxy
            mission = head $ galaxyMissions galaxy
            rng = mkStdGen 42
        in case initGameState galaxy mission rng of
            Just gs -> do
                let ship = (gameShip gs) { shipFuel = 0 }
                let gs' = gs { gameShip = ship }
                let gsChecked = checkGameStatus gs'
                gameStatus gsChecked @?= MissionFailed OutOfFuel
            Nothing -> assertFailure "Failed to initialize game state"
    
    , "lose when shield depleted" ~:
        let galaxy = createTestGalaxy
            mission = head $ galaxyMissions galaxy
            rng = mkStdGen 42
        in case initGameState galaxy mission rng of
            Just gs -> do
                let ship = (gameShip gs) { shipShield = 0 }
                let gs' = gs { gameShip = ship }
                let gsChecked = checkGameStatus gs'
                gameStatus gsChecked @?= MissionFailed OutOfProtection
            Nothing -> assertFailure "Failed to initialize game state"
    
    , "lose when out of time" ~:
        let galaxy = createTestGalaxy
            mission = head $ galaxyMissions galaxy
            rng = mkStdGen 42
        in case initGameState galaxy mission rng of
            Just gs -> do
                let ship = (gameShip gs) { shipTime = 0 }
                let gs' = gs { gameShip = ship }
                let gsChecked = checkGameStatus gs'
                gameStatus gsChecked @?= MissionFailed OutOfTime
            Nothing -> assertFailure "Failed to initialize game state"
    ]

testRouteSelection :: Test
testRouteSelection = TestList
    [ "select next route cycles through available routes" ~:
        let galaxy = Galaxy
                { galaxyPlanets = [Planet "A" (0,0) Nothing False, Planet "B" (100,0) Nothing False, Planet "C" (0,100) Nothing False]
                , galaxyRoutes = [Route "A" "B" Forward (10,20) 5, Route "A" "C" Forward (15,25) 5]
                , galaxyHazards = []
                , galaxyMissions = [Mission "Test" "A" "B" 60]
                }
            mission = head $ galaxyMissions galaxy
            rng = mkStdGen 42
        in case initGameState galaxy mission rng of
            Just gs -> do
                let gs1 = selectNextRoute gs
                case gameSelectedRoute gs1 of
                    Just r -> routeTo r @?= "B"
                    Nothing -> assertFailure "No route selected"
                
                let gs2 = selectNextRoute gs1
                case gameSelectedRoute gs2 of
                    Just r -> routeTo r @?= "C"
                    Nothing -> assertFailure "No route selected"
                
                -- Should cycle back
                let gs3 = selectNextRoute gs2
                case gameSelectedRoute gs3 of
                    Just r -> routeTo r @?= "B"
                    Nothing -> assertFailure "No route selected"
            Nothing -> assertFailure "Failed to initialize game state"
    ]

-- =============================================================================
-- ======= Test Runner =========================================================
-- =============================================================================

parserTests :: Test
parserTests = TestLabel "Parser Tests" $ TestList
    [ testParsePlanet
    , testParseRoute
    , testParseHazard
    , testParseMission
    , testParseFullConfig
    ]

gameLogicTests :: Test
gameLogicTests = TestLabel "Game Logic Tests" $ TestList
    [ testInitGameState
    , testGetAvailableRoutes
    , testCanAffordRoute
    , testHazardIntersection
    , testApplyHazardEffect
    , testPlanetEffects
    , testWinCondition
    , testLoseConditions
    , testRouteSelection
    ]

allTests :: Test
allTests = TestList
    [ parserTests
    , gameLogicTests
    ]

main :: IO ()
main = do
    putStrLn "Running Star Routes Test Suite..."
    putStrLn "=================================="
    counts <- runTestTT allTests
    putStrLn ""
    putStrLn "=================================="
    if errors counts + failures counts == 0
        then putStrLn "✓ All tests passed!"
        else putStrLn "✗ Some tests failed"