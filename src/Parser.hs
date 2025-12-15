module Parser
    ( module Parser
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Control.Monad (void)
import Data.Char (isSpace)

import Types
import Constants

-- =========================================================
-- ======= Lexer Setup =====================================
-- =========================================================

-- een lexer voor de configfiles

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
    where
        style = emptyDef
            { Token.commentLine = "#"
            , Token.identStart = letter <|> char '_'
            , Token.identLetter = alphaNum <|> char '_'
            , Token.reservedNames = 
                [ "planet", "at", "type", "fuel", "repair", "none"
                , "route", "time", "hazard", "radius", "damage", "fuelLoss"
                , "mission", "from", "to", "timeLimit"
                , "asteroid", "pirates", "nebula", "radiation"
                ]
            , Token.reservedOpNames = ["-->", "<--", "<->"]
            }

-- Handige lexer functies
reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

integer :: Parser Int
integer = fromIntegral <$> Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer


-- =========================================================
-- ======= Position Parser =================================
-- =========================================================

-- Parse een positie: (x, y)
parsePosition :: Parser Position
parsePosition = parens $ do
    x <- integer
    void $ symbol ","
    y <- integer    
    return (fromIntegral x, fromIntegral y)


-- =========================================================
-- ======= Planet Parser ===================================
-- =========================================================

-- parse een planetEffect
parsePlanetEffect :: Parser Effect
parsePlanetEffect = 
    (reserved "fuel" >> return Fuel) <|>
    (reserved "repair" >> return Repair) <|>
    (reserved "none" >> return None)

-- parse een planet: {string} at ({x}, {y}) [type {effect}]
parsePlanet :: Parser Planet
parsePlanet = do 
    reserved "planet"
    name <- stringLiteral
    reserved "at"
    pos <- parsePosition
    effect <- option None $ do
        reserved "type"
        parsePlanetEffect
    return $ Planet name pos (Just effect) False --Planet verwacht Maybe Effect 


-- =========================================================
-- ======= Route Parser ====================================
-- =========================================================

-- parse de route richting
parseDirection :: Parser Direction
parseDirection =
    (reservedOp "-->" >> return Forward) <|>
    (reservedOp "<--" >> return Backward) <|>
    (reservedOp "<->" >> return BiDirectional)

-- parse brandstof range: {min}-{max}
parseFuelRange :: Parser (Int, Int)
parseFuelRange = do
    minFuel <- integer
    void $ symbol "-"
    maxFuel <- integer
    return (minFuel, maxFuel)


-- parse een route: route {start} {richting} {einde} fuel {min}-{max} [time {tijd}]
parseRoute :: Parser Route
parseRoute = do
    reserved "route"
    from <- stringLiteral
    direction <- parseDirection
    to <- stringLiteral
    reserved "fuel"
    fuelRange <- parseFuelRange
    time <- option defaultRouteTime $ do
        reserved "time"
        integer
    return $ Route from to direction fuelRange time


-- =========================================================
-- ======= Hazard Parser ===================================
-- =========================================================

-- parse hazardType
parseHazardType :: Parser HazardType
parseHazardType = 
    parseAsteroid <|> parsePirates <|> parseNebula <|> parseRadiation
    where
        parseAsteroid = do
            reserved "asteroid"
            return (Asteroid 0)  -- placeholder waarde
        
        parsePirates = do
            reserved "pirates"
            return (Pirates 0)  -- placeholder waarde
        
        parseNebula = do
            reserved "nebula"
            return Nebula
        
        parseRadiation = do
            reserved "radiation"
            return (Radiation 0)  -- placeholder waarde

-- parse een gevaar: hazard {type} {naam} at ({x}, {y}) radius {radius} [damage {schade}] [fuelLoss {brandstofverlies}]

parseHazard :: Parser Hazard
parseHazard = do
    reserved "hazard"
    hazType <- parseHazardType
    name <- stringLiteral
    reserved "at"
    pos <- parsePosition
    reserved "radius"
    radius <- integer
    
    -- Parse type-specifieke velden en update het type
    finalType <- case hazType of 
        Asteroid _ -> do
            reserved "damage"
            dmg <- integer
            return $ Asteroid dmg

        Pirates _ -> do 
            reserved "fuelLoss"
            loss <- integer
            return $ Pirates loss
        
        Radiation _ -> do
            reserved "damage"
            dmg <- integer
            return $ Radiation dmg
        
        Nebula -> return Nebula

    return $ Hazard finalType name pos radius


-- =========================================================
-- ======= Mission Parser ==================================
-- =========================================================

-- parse een missie: mission {string} from {start} to {einde} timeLimit {tijd}
parseMission :: Parser Mission
parseMission = do
    reserved "mission"
    name <- stringLiteral
    reserved "from"
    from <- stringLiteral
    reserved "to"
    to <- stringLiteral
    reserved "timeLimit"
    timeLimit <- integer

    return $ Mission name from to timeLimit

-- =========================================================
-- ======= Main Config Parser ==============================
-- =========================================================

-- Parse één element (planet, route, hazard, of mission)
parseElement :: Parser (Galaxy -> Galaxy)
parseElement = 
    (parsePlanet >>= \p -> return $ \cfg -> cfg { galaxyPlanets = galaxyPlanets cfg ++ [p] }) <|>
    (parseRoute >>= \r -> return $ \cfg -> cfg { galaxyRoutes = galaxyRoutes cfg ++ [r] }) <|>
    (parseHazard >>= \h -> return $ \cfg -> cfg { galaxyHazards = galaxyHazards cfg ++ [h] }) <|>
    (parseMission >>= \m -> return $ \cfg -> cfg { galaxyMissions = galaxyMissions cfg ++ [m] })

-- Parse het volledige configuratie bestand

parseConfig :: Parser Galaxy
parseConfig = do
    whiteSpace
    elements <- many parseElement
    eof
    return $ foldr ($) emptyGalaxy (reverse elements)

parseConfigFile :: String -> Either ParseError Galaxy
parseConfigFile = parse parseConfig ""