module Lib
    ( -- Re-export belangrijke modules
      module Types
    , module Constants
    , module Parser
    , module Game.Logic
    -- Export game functions
    , runGame
    , AppState(..)
    , AppMode(..)
    , initialAppState
    ) where

import Types
import Constants
import Parser
import Game.Logic
import UI.Render
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random (StdGen)

-- =============================================================================
-- ======= App State ===========================================================
-- =============================================================================

-- App state houdt alle game state bij
data AppState = AppState
    { appMode :: AppMode
    , appGalaxy :: Galaxy
    , appGameState :: Maybe GameState
    , appSelectedMission :: Int
    , appRng :: StdGen
    }

data AppMode
    = MissionSelectionMode
    | GamePlayMode
    | MissionSuccessMode
    | GameOverMode FailReason
    deriving (Eq, Show)

-- Initiële app state
initialAppState :: Galaxy -> StdGen -> AppState
initialAppState galaxy rng = AppState
    { appMode = MissionSelectionMode
    , appGalaxy = galaxy
    , appGameState = Nothing
    , appSelectedMission = 0
    , appRng = rng
    }

-- =============================================================================
-- ======= Main Game Function ==================================================
-- =============================================================================

-- Start de game met Gloss
runGame :: Galaxy -> StdGen -> IO ()
runGame galaxy rng = playIO
    (InWindow "Star Routes" (1600, 1000) (100, 100))
    backgroundColor
    60  -- FPS
    (initialAppState galaxy rng)
    renderAppState
    handleInput
    updateAppState

-- =============================================================================
-- ======= Rendering ===========================================================
-- =============================================================================

renderAppState :: AppState -> IO Picture
renderAppState appState = return $ case appMode appState of
    MissionSelectionMode ->
        renderMissionSelection (appGalaxy appState) (appSelectedMission appState)
    
    GamePlayMode -> case appGameState appState of
        Just gs -> renderGame gs
        Nothing -> Blank
    
    MissionSuccessMode ->
        renderMissionSuccess
    
    GameOverMode reason ->
        renderGameOver reason

-- =============================================================================
-- ======= Input Handling ======================================================
-- =============================================================================

handleInput :: Event -> AppState -> IO AppState
handleInput event appState = return $ case appMode appState of
    MissionSelectionMode ->
        handleMissionSelectionInput event appState
    
    GamePlayMode -> case appGameState appState of
        Just gs -> handleGamePlayInput event appState gs
        Nothing -> appState
    
    MissionSuccessMode ->
        handleEndScreenInput event appState
    
    GameOverMode _ ->
        handleEndScreenInput event appState

-- Handle input in mission selection mode
handleMissionSelectionInput :: Event -> AppState -> AppState
handleMissionSelectionInput (EventKey (SpecialKey KeyUp) Down _ _) appState =
    let missions = galaxyMissions (appGalaxy appState)
        newIdx = max 0 (appSelectedMission appState - 1)
    in appState { appSelectedMission = newIdx }

handleMissionSelectionInput (EventKey (SpecialKey KeyDown) Down _ _) appState =
    let missions = galaxyMissions (appGalaxy appState)
        maxIdx = length missions - 1
        newIdx = min maxIdx (appSelectedMission appState + 1)
    in appState { appSelectedMission = newIdx }

handleMissionSelectionInput (EventKey (SpecialKey KeyEnter) Down _ _) appState =
    let missions = galaxyMissions (appGalaxy appState)
        mission = missions !! appSelectedMission appState
    in case initGameState (appGalaxy appState) mission (appRng appState) of
        Just gs -> appState
            { appMode = GamePlayMode
            , appGameState = Just gs
            }
        Nothing -> appState

handleMissionSelectionInput _ appState = appState

-- Handle input in gameplay mode
handleGamePlayInput :: Event -> AppState -> GameState -> AppState
handleGamePlayInput (EventKey (SpecialKey KeyUp) Down _ _) appState gs
    | not (gameTraveling gs) =
        appState { appGameState = Just (selectPreviousRoute gs) }
    | otherwise = appState

handleGamePlayInput (EventKey (SpecialKey KeyDown) Down _ _) appState gs
    | not (gameTraveling gs) =
        appState { appGameState = Just (selectNextRoute gs) }
    | otherwise = appState

handleGamePlayInput (EventKey (SpecialKey KeyEnter) Down _ _) appState gs
    | not (gameTraveling gs) =
        appState { appGameState = Just (confirmRoute gs) }
    | otherwise = appState

handleGamePlayInput (EventKey (Char 'r') Down _ _) appState gs =
    let mission = Mission
            { missionName = ""
            , missionStart = gameCurrentPlanet gs
            , missionEnd = gameTargetPlanet gs
            , missionTime = gameMissionTime gs
            }
        -- Vind de originele missie voor correcte start planet
        originalMission = case filter (\m -> missionEnd m == gameTargetPlanet gs) 
                                       (galaxyMissions $ appGalaxy appState) of
            (m:_) -> m
            [] -> mission
    in case resetGameState gs originalMission (appRng appState) of
        Just newGs -> appState
            { appGameState = Just newGs
            }
        Nothing -> appState

handleGamePlayInput (EventKey (SpecialKey KeyEsc) Down _ _) appState _ =
    appState
        { appMode = MissionSelectionMode
        , appGameState = Nothing
        }

handleGamePlayInput _ appState _ = appState

-- Handle input in end screens
handleEndScreenInput :: Event -> AppState -> AppState
handleEndScreenInput (EventKey (SpecialKey KeyEsc) Down _ _) appState =
    appState
        { appMode = MissionSelectionMode
        , appGameState = Nothing
        }

handleEndScreenInput (EventKey (Char 'r') Down _ _) appState =
    case appGameState appState of
        Just gs ->
            -- Vind de originele missie
            let originalMission = case filter (\m -> missionEnd m == gameTargetPlanet gs) 
                                               (galaxyMissions $ appGalaxy appState) of
                    (m:_) -> m
                    [] -> Mission
                        { missionName = ""
                        , missionStart = gameCurrentPlanet gs
                        , missionEnd = gameTargetPlanet gs
                        , missionTime = gameMissionTime gs
                        }
            in case resetGameState gs originalMission (appRng appState) of
                Just newGs -> appState
                    { appMode = GamePlayMode
                    , appGameState = Just newGs
                    }
                Nothing -> appState
        Nothing -> appState

handleEndScreenInput _ appState = appState

-- =============================================================================
-- ======= Update ==============================================================
-- =============================================================================

updateAppState :: Float -> AppState -> IO AppState
updateAppState deltaTime appState = return $ case appMode appState of
    GamePlayMode -> case appGameState appState of
        Just gs ->
            let updatedGs = updateTravel deltaTime gs
                newStatus = gameStatus updatedGs
            in case newStatus of
                MissionSuccess -> appState
                    { appMode = MissionSuccessMode
                    , appGameState = Just updatedGs
                    }
                MissionFailed reason -> appState
                    { appMode = GameOverMode reason
                    , appGameState = Just updatedGs
                    }
                Playing -> appState { appGameState = Just updatedGs }
        Nothing -> appState
    
    _ -> appState