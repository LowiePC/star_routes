module Types 
    ( module Types
    ) where
--alle datatypes aanmaken

import System.Random (StdGen)

type Vec = (Float, Float)
type Position = Vec

data Ship = Ship
    {   shipPos     :: Position
    ,   shipFuel    :: Int
    ,   shipShield  :: Int 
    ,   shipTime    :: Int
    } deriving (Eq, Show)

-- effect van planeet op het schip
data Effect 
    = Fuel      -- verhoogt brandstof
    | Repair    -- herstelt bescherming van schip
    | None      -- geen effect
    deriving (Eq, Show, Read)

data Planet = Planet
    {   planetName      :: String       -- unieke naam
    ,   planetPos       :: Position
    ,   planetEffect    :: Maybe Effect -- optioneel effect (éénmalig) 
    ,   planetVisited   :: Bool
    } deriving (Eq, Show)

data Direction
    = Forward           -- route kan enkel van -> naar (-->)
    | BiDirectional     -- route kan in beide richtingen (<->)
    | Backward          -- route kan enkel naar <- van (<--)
    deriving (Eq, Show)

data Route = Route
    {   routeFrom       :: String
    ,   routeTo         :: String
    ,   routeDirection  :: Direction
    ,   routeFuelRange  :: (Int, Int)
    ,   routeTime       :: Int
    } deriving (Eq, Show)

data HazardType 
    = Asteroid Int  --asteroïde beschadigt bescherming
    | Pirates Int   -- piraten stelen brandstof
    | Nebula        -- Nebula: verlies tussen 10% - 50% brandstof
    | Radiation Int -- Radiatie beschadigt bescherming
    deriving (Eq, Show)

data Hazard = Hazard
    {   hazardType      :: HazardType
    ,   hazardName      :: String
    ,   hazardPos       :: Position
    ,   hazardRadius    :: Int  
    } deriving (Show, Eq)

data Mission = Mission
    {   missionName     :: String
    ,   missionStart    :: String   -- naam van de planeet waar de missie start
    ,   missionEnd      :: String   -- naam van de doelplaneet
    ,   missionTime     :: Int      -- tijdslimiet voor de missie
    } deriving (Eq, Show)

data Galaxy = Galaxy
    {   galaxyPlanets   :: [Planet]     -- alle planeten in de galaxy
    ,   galaxyRoutes    :: [Route]      -- alle routen tussen de planeten
    ,   galaxyHazards   :: [Hazard]     -- alle hazards in de galaxy
    ,   galaxyMissions  :: [Mission]    -- alle beschikbare missies
    } deriving (Eq, Show)

-- Lege configuratie
emptyGalaxy :: Galaxy
emptyGalaxy = Galaxy [] [] [] []    

data GameState = GameState
    { gameGalaxy        :: Galaxy
    , gameShip          :: Ship
    , gameCurrentPlanet :: String
    , gameTargetPlanet  :: String
    , gameMissionTime   :: Int
    , gameSelectedRoute :: Maybe Route
    , gameTraveling     :: Bool
    , gameTravelProgress :: Float  -- 0.0 tot 1.0
    , gameRng           :: StdGen
    , gameStatus        :: GameStatus
    } deriving (Show)

data GameStatus
    = Playing
    | MissionSuccess
    | MissionFailed FailReason
    deriving (Eq, Show)

data FailReason
    = OutOfFuel
    | OutOfProtection
    | OutOfTime
    deriving (Eq, Show)

-- =============================================================================
-- ======= HELPER FUNCTIES =====================================================
-- =============================================================================

-- Helper om damage van de hazardType te krijgen
getHazardDamage :: HazardType -> Maybe Int
getHazardDamage (Asteroid dmg) = Just dmg
getHazardDamage (Radiation dmg) = Just dmg
getHazardDamage _ = Nothing

-- Helper om fuelDrain van de hazardType te krijgen
getHazardFuelLoss :: HazardType -> Maybe Int
getHazardFuelLoss (Pirates loss) = Just loss
getHazardFuelLoss _ = Nothing