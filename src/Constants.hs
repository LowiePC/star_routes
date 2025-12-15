module Constants 
    ( module Constants
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

-- =========================================================
-- ======= Game Constanten =================================
-- =========================================================

-- initiële brandstofpercentage
initialFuel :: Int
initialFuel = 100

-- initiële beschermingpercentage
initialShield :: Int 
initialShield = 100

-- standaard route tijd
defaultRouteTime :: Int 
defaultRouteTime = 2

-- brandstof verhoogt bij planeet van type fuel
fuelRefil :: Int
fuelRefil = 30

-- bescherming verhoogt bij planeet van type repair
shieldRefil :: Int
shieldRefil = 30

-- minimum van brandstofverlies bij nebula (procent)
nebulaMinLoss :: Int
nebulaMinLoss = 10

-- maximum van brandstofverlies bij nebula (procent)
nebulaMaxLoss :: Int
nebulaMaxLoss = 50

-- =========================================================
-- ======= Rendering Constanten ============================
-- =========================================================

-- radius van een planeet
planetRadius :: Float
planetRadius = 12.0

-- radius van de huidige planeet
currentPlanetRadius :: Float
currentPlanetRadius = 15.0

-- breedte van de route lijnen
routeWidth :: Float
routeWidth = 2.0

-- Scale voor planet naam tekst
textScale :: Float
textScale = 0.12

-- Scale voor kleine tekst (labels, info)
smallTextScale :: Float
smallTextScale = 0.10

-- Scale voor groote tekst (titels)
largeTextScale :: Float
largeTextScale = 0.20

-- Offset van planeet namen onder planeten
planetNameOffset :: Float
planetNameOffset = -25.0

-- Opacity voor hazard circels (0-255)
hazardOpacity :: Int
hazardOpacity = 80

-- Achtergrond kleur (donkerblauw)
backgroundColor :: Color
backgroundColor = makeColor 0.05 0.05 0.15 1.0

-- Planet kleuren
planetColor :: Color
planetColor = makeColor 0.3 0.3 0.3 1.0

currentPlanetColor :: Color
currentPlanetColor = makeColor 0.8 0.8 0.2 1.0  -- Geel voor huidige planeet

targetPlanetColor :: Color
targetPlanetColor = makeColor 0.2 0.8 0.3 1.0   -- Groen voor doel

-- Planet effect kleuren
fuelPlanetColor :: Color
fuelPlanetColor = makeColor 0.9 0.7 0.2 1.0  -- Geel/oranje voor fuel

repairPlanetColor :: Color
repairPlanetColor = makeColor 0.1 0.2 1.0 1.0  -- Blauw voor repair

-- Route kleuren
routeColor :: Color
routeColor = makeColor 0.4 0.4 0.6 1.0

selectedRouteColor :: Color
selectedRouteColor = makeColor 0.2 0.6 1.0 1.0  -- Blauw voor geselecteerde route

-- Hazard kleuren
asteroidColor :: Color
asteroidColor = makeColor 0.6 0.2 0.2 0.5  -- Rood, semi-transparant

piratesColor :: Color
piratesColor = makeColor 0.8 0.4 0.2 0.5  -- Oranje

nebulaColor :: Color
nebulaColor = makeColor 0.4 0.2 0.6 0.5  -- Paars

radiationColor :: Color
radiationColor = makeColor 0.8 0.8 0.2 0.5  -- Geel

-- UI kleuren
textColor :: Color
textColor = white

panelColor :: Color
panelColor = makeColor 0.1 0.1 0.2 0.9

panelBorderColor :: Color
panelBorderColor = makeColor 0.3 0.3 0.5 1.0

-- Progress bar kleuren
fuelBarColor :: Color
fuelBarColor = yellow

hullBarColor :: Color
hullBarColor = green

timeBarColor :: Color
timeBarColor = cyan

barBackgroundColor :: Color
barBackgroundColor = makeColor 0.2 0.2 0.3 1.0

-- Route list kleuren
availableRouteColor :: Color
availableRouteColor = makeColor 0.2 0.8 0.4 1.0  -- Groen

unavailableRouteColor :: Color
unavailableRouteColor = makeColor 0.8 0.2 0.2 1.0  -- Rood

selectedRouteTextColor :: Color
selectedRouteTextColor = makeColor 0.4 0.8 1.0 1.0  -- Cyaan