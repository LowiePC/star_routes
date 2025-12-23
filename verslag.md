# Star Routes - Projectverslag

**Student:** [Jouw naam]  
**Datum:** December 2024

---

## 1. Introductie & Ontwerpkeuzes

### Overzicht van het spel
Star Routes is een strategisch ruimte-puzzelspel waarbij de speler door een sterrenstelsel navigeert via routes tussen planeten. Het doel is om een doelplaneet te bereiken binnen de tijdslimiet, terwijl je brandstof en bescherming beheert en gevaren ontwijkt.

### Belangrijkste ontwerpkeuzes

#### Modulaire architectuur
De code is opgedeeld in logische modules:
- **Types.hs**: Alle datastructuren (Planet, Route, Hazard, Ship, GameState, etc.)
- **Parser.hs**: Parsec-gebaseerde parser voor configuratiebestanden
- **Constants.hs**: Alle constanten op één plaats (geen magic numbers)
- **Game.Logic.hs**: Alle spellogica (route berekening, hazard detectie, win/lose condities)
- **UI.Render.hs**: Gloss rendering code, gescheiden van logica
- **Lib.hs**: Main game loop en event handling

Deze scheiding zorgt voor:
- Eenvoudig testen van individuele componenten
- Herbruikbaarheid van code
- Duidelijke verantwoordelijkheden per module

#### Real-time travel system
In plaats van instant teleportatie tussen planeten, heb ik gekozen voor een real-time reis systeem:
- Brandstof en tijd worden geleidelijk afgetrokken tijdens het reizen
- Hazards worden gedetecteerd en toegepast op het moment dat je ze passeert
- Visuele feedback door animatie van het schip over de route
- Dit maakt het spel dynamischer en spannender

#### Vooraf bepaalde brandstofkosten
Wanneer een route geselecteerd wordt, wordt direct een willekeurige brandstofkost bepaald binnen het opgegeven bereik. Dit voorkomt:
- Oneerlijke situaties waar je halverwege een reis onverwacht veel brandstof verliest
- De speler kan een geïnformeerde beslissing maken voor het starten van de reis
- Transparantie in de gameplay

---

## 2. Gebruik van Monads

### IO Monad
De `IO` monad wordt gebruikt voor:
- **File reading**: Het inlezen van configuratiebestanden (`readFile`)
- **Command line arguments**: Ophalen van het pad naar het config bestand
- **Gloss game loop**: `playIO` vereist IO voor rendering en event handling

Voorbeeld uit Main.hs:
```haskell
main :: IO ()
main = do
    args <- getArgs
    content <- readFile configFile
    case parseConfigFile content of
        Right galaxy -> do
            rng <- getStdGen
            runGame galaxy rng
```

### Maybe Monad
De `Maybe` monad wordt extensief gebruikt voor foutafhandeling zonder exceptions:
- **Game initialisatie**: `initGameState` retourneert `Maybe GameState` wanneer de startplaneet niet bestaat
- **Route destinatie**: `getRouteDestination` retourneert `Maybe String` wanneer een route niet van de huidige planeet vertrekt
- **Planeet lookup**: `find` functies retourneren `Maybe Planet` bij het zoeken naar planeten

Voorbeeld uit Game.Logic.hs:
```haskell
initGameState :: Galaxy -> Mission -> StdGen -> Maybe GameState
initGameState galaxy mission rng = do
    startPlanet <- find (\p -> planetName p == missionStart mission) (galaxyPlanets galaxy)
    let ship = Ship {...}
    return $ GameState {...}
```

Het voordeel: Als de startplaneet niet bestaat, faalt de hele initialisatie automatisch zonder crashes.

### Parser Monad (Parsec)
Parsec combineert meerdere monadic eigenschappen:
- **State monad**: Houdt parsing positie bij
- **Error monad**: Verzamelt parsing errors
- **Combinators**: `<|>` voor alternatieven, `many` voor herhaling, `option` voor optionele delen

Voorbeeld:
```haskell
parsePlanet :: Parser Planet
parsePlanet = do 
    reserved "planet"
    name <- stringLiteral
    reserved "at"
    pos <- parsePosition
    effect <- option None $ do
        reserved "type"
        parsePlanetEffect
    return $ Planet name pos (Just effect) False
```

De `do`-notatie maakt de sequentiële parsing natuurlijk leesbaar, terwijl Parsec automatisch error handling en backtracking verzorgt.

### Random Monad (StdGen)
Voor randomness gebruik ik pure functies met `StdGen`:
- Geen `unsafePerformIO` nodig
- Deterministisch testbaar
- State threading door `randomR` die een nieuwe generator retourneert

Voorbeeld uit Game.Logic.hs:
```haskell
applyHazardEffect :: Hazard -> Ship -> StdGen -> (Ship, StdGen)
applyHazardEffect hazard ship rng =
    case hazardType hazard of
        Nebula ->
            let (lossPercent, newRng) = randomR (nebulaMinLoss, nebulaMaxLoss) rng
                loss = (currentFuel * lossPercent) `div` 100
            in (ship { shipFuel = currentFuel - loss }, newRng)
```

### Voordelen van Monads in dit project
1. **Expliciete effecten**: Het type systeem maakt duidelijk welke functies side-effects hebben
2. **Composability**: Kleine, testbare functies die gemakkelijk gecombineerd worden
3. **Error handling**: Geen null pointer exceptions, alles wordt afgehandeld via types
4. **Zuiverheid**: Game logica is volledig zuiver, IO beperkt tot randen van het programma

---

## 3. Testing

### Geteste componenten

#### Parser tests (volledig gedekt)
- ✅ Alle constructies: planet, route, hazard, mission
- ✅ Optionele velden (planet effects, route time)
- ✅ Verschillende hazard types met correcte parameters
- ✅ Volledige configuratiebestanden met comments en lege regels
- ✅ Forward, backward en bidirectional routes

#### Game logica tests
- ✅ Initialisatie van game state
- ✅ Route beschikbaarheid en affordability
- ✅ Hazard intersectie berekeningen
- ✅ Hazard effecten (asteroid, pirates, nebula, radiation)
- ✅ Planeet effecten (eenmalig toepassen)
- ✅ Win conditie (doel bereiken)
- ✅ Alle verlies condities (fuel, shield, time)
- ✅ Route selectie en cycling

### Niet-geteste componenten
- **UI Rendering**: Gloss rendering functies zijn visueel en moeilijk te unit testen
- **Travel animation**: Real-time updates vereisen integration testing
- **Mouse hover interactions**: Event handling is handmatig getest

Deze componenten zijn wel manueel getest door het spel te spelen met verschillende configuraties.

### Test uitvoering
```bash
stack test
```

Alle tests slagen en dekken de kernfunctionaliteit van het spel.

---

## 4. Zelfontworpen Level: "The Gauntlet"

### Beschrijving
"The Gauntlet" is een uitdagende missie die alle spelmechanics test. De speler moet strategisch plannen welke planeten te bezoeken voor supplies, welke hazards te vermijden, en hoe tijd optimaal te gebruiken.

### Opzet
```
Start (0, 0)
    ↓ (fuel 15-20, 5s) door Asteroid Field
Fuel Station Alpha (200, 0) [+30 fuel]
    ↓ (fuel 20-30, 6s)
Junction (200, 200)
    ├─→ Route A: (fuel 10-15, 4s) door Pirate Territory
    │   Repair Bay (350, 200) [+30 shield]
    │       ↓ (fuel 25-35, 7s) door Nebula
    │   Goal (500, 200)
    │
    └─→ Route B: (fuel 35-45, 5s) - direct maar duur
        Goal (500, 200)
```

**Hazards:**
- Asteroid Field (100, 0) radius 50, damage 25
- Pirate Territory (275, 200) radius 40, fuelLoss 20
- Mysterious Nebula (425, 200) radius 80, random fuel loss

**Time limit:** 50 seconden

### Uitdagingen
1. **Resource management**: Start met 100 fuel, maar alle routes samen kosten meer
2. **Risk assessment**: Snelle route is duurder in fuel, veilige route kost tijd
3. **Hazard navigation**: Elke route heeft gevaren, geen perfecte optie
4. **Timing pressure**: 50 seconden is krap, geen ruimte voor fouten

### Oplossing
De optimale strategie:
1. Start → Fuel Station Alpha (verbruik ~17 fuel, +30 fuel, netto +13 fuel)
   - Asteroid Field doet 25 shield damage (blijf op 75 shield)
2. Fuel Station Alpha → Junction (verbruik ~25 fuel)
3. Junction → Repair Bay via Route A (verbruik ~12 fuel)
   - Pirates stelen 20 fuel, effectief ~32 fuel verbruikt
   - Shield herstelt naar 100
4. Repair Bay → Goal (verbruik ~30 fuel)
   - Nebula kost random 10-50% van huidige fuel (~2-10 fuel)

**Totaal:**
- Fuel: 100 - 17 + 30 - 25 - 12 - 20 - 30 - ~5 = ~21 fuel remaining
- Shield: 100 - 25 + 30 = 105 (capped at 100)
- Time: ~5 + 6 + 4 + 7 = ~22 seconden, ruim binnen de 50s

**Alternatieve (riskante) route:**
Junction → Goal direct via Route B kost 35-45 fuel en vermijdt alle verdere hazards, maar je hebt niet genoeg fuel na de eerste twee stops.

---

## 5. Reflectie

### Wat ging goed
- **Modulaire structuur**: Door de duidelijke scheiding tussen modules was debuggen en testen eenvoudig
- **Type safety**: Het Haskell type systeem ving veel fouten tijdens compilatie
- **Parsec**: De parser was verrassend eenvoudig te schrijven en robuust
- **Gloss**: Eenvoudige maar effectieve library voor 2D graphics

### Uitdagingen
- **Real-time hazard detection**: Het correct detecteren van hazards tijdens een animatie was complex. Ik moest bijhouden welke hazards al waren toegepast om duplicaten te voorkomen.
- **State management**: Het doorgeven van `StdGen` door alle functies was soms vervelend, maar noodzakelijk voor determinisme.
- **Gloss limitations**: Geen native support voor hover events; ik moest dit zelf implementeren met mouse position tracking.

### Wat ik geleerd heb
- **Monadic thinking**: Het expliciet maken van side-effects via types leidt tot betere code
- **Pure functions**: Scheiding tussen pure logica en IO maakt testen triviaal
- **Parser combinators**: Parsec is krachtig en elegant voor DSL parsing
- **Game architecture**: Hoe je een game loop structureert met event handling en state updates

### Mogelijke verbeteringen
- **Betere UI**: Tooltips, animaties, geluidseffecten
- **Save/Load**: Persistente scores en voortgang
- **Meer hazard types**: Black holes, wormholes, friendly encounters
- **Procedural generation**: Random galaxy generatie voor eindeloze replayability

---

## Conclusie

Dit project was een uitstekende oefening in functioneel programmeren. Het combineren van parsing, game logic, en UI rendering in Haskell heeft me geleerd hoe krachtig type safety en pure functions zijn voor complexe applicaties. Het eindresultaat is een speelbaar, testbaar, en uitbreidbaar spel dat alle projectvereisten vervult.