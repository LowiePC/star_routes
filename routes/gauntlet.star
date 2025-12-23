# The Gauntlet - Strategic Challenge Level
# Tests resource management, risk assessment, and timing

# Starting area
planet "Launch Bay" at (0, 0) type none

# Supply stations (strategic stops)
planet "Fuel Station Alpha" at (200, 0) type fuel
planet "Repair Bay Delta" at (350, 200) type repair

# Key junction
planet "Junction Crossroads" at (200, 200) type none

# Alternative path planets
planet "Safe Harbor" at (150, -150) type repair
planet "Emergency Depot" at (350, -150) type fuel

# Goal
planet "Destination Prime" at (500, 200) type none

# =============================================================================
# Main path routes (the intended gauntlet)
# =============================================================================

# Start to fuel station (through asteroid field)
route "Launch Bay" --> "Fuel Station Alpha" fuel 15-20 time 5

# Fuel station to junction
route "Fuel Station Alpha" --> "Junction Crossroads" fuel 20-30 time 6

# From junction: two choices

# Route A: Through repair bay (safer but longer)
route "Junction Crossroads" --> "Repair Bay Delta" fuel 10-15 time 4
route "Repair Bay Delta" --> "Destination Prime" fuel 25-35 time 7

# Route B: Direct (expensive but fast)
route "Junction Crossroads" --> "Destination Prime" fuel 35-45 time 5

# =============================================================================
# Alternative safe path (more time, less danger)
# =============================================================================

route "Launch Bay" --> "Safe Harbor" fuel 12-18 time 5
route "Safe Harbor" --> "Emergency Depot" fuel 15-25 time 6
route "Emergency Depot" --> "Destination Prime" fuel 30-40 time 8

# =============================================================================
# Hazards on main path
# =============================================================================

# Early hazard: asteroid field blocks direct path to fuel station
hazard asteroid "Asteroid Field Sigma" at (100, 0) radius 50 damage 25

# Mid-game hazard: pirates ambush on route A
hazard pirates "Pirate Ambush Point" at (275, 200) radius 40 fuelLoss 20

# Late game hazard: nebula on final approach via route A
hazard nebula "Mysterious Nebula" at (425, 200) radius 80

# Hazard on direct route B (makes it risky despite being direct)
hazard radiation "Solar Flare Zone" at (350, 200) radius 70 damage 35

# =============================================================================
# Hazards on alternative path (fewer but still present)
# =============================================================================

hazard asteroid "Minor Debris" at (75, -75) radius 35 damage 15
hazard pirates "Patrol Route" at (250, -150) radius 45 fuelLoss 15

# =============================================================================
# Mission
# =============================================================================

mission "The Gauntlet" from "Launch Bay" to "Destination Prime" timeLimit 50

# =============================================================================
# Strategy guide (not in file, just for documentation)
# =============================================================================

# OPTIMAL SOLUTION (Route A - Balanced):
# 1. Launch Bay → Fuel Station Alpha (~17 fuel, -25 shield from asteroid)
#    Resources after: Fuel: 100-17+30=113 (capped at 100), Shield: 75
# 2. Fuel Station Alpha → Junction (~25 fuel)
#    Resources after: Fuel: 75, Shield: 75
# 3. Junction → Repair Bay Delta (~12 fuel, -20 fuel from pirates = 32 total)
#    Resources after: Fuel: 43, Shield: 75
# 4. Repair Bay Delta → Goal (~30 fuel, -10-50% from nebula = ~34-47 fuel)
#    Resources after: Fuel: ~0-10 (TIGHT!), Shield: 100 (repaired at stop 3)
# Time used: ~22 seconds (comfortable)
# Result: SUCCESS (barely)

# RISKY SOLUTION (Route B - Direct):
# 1. Launch Bay → Fuel Station Alpha (~17 fuel, -25 shield)
# 2. Fuel Station Alpha → Junction (~25 fuel)
# 3. Junction → Goal directly (~40 fuel, -35 shield from radiation)
#    Resources after: Fuel: ~18, Shield: 40
# Time used: ~16 seconds (very fast)
# Result: SUCCESS but risky - one mistake earlier and you fail

# SAFE SOLUTION (Alternative path):
# 1. Launch Bay → Safe Harbor (~15 fuel, -15 shield from debris)
# 2. Safe Harbor → Emergency Depot (~20 fuel, -15 fuel from patrol = 35 total)
# 3. Emergency Depot → Goal (~35 fuel)
#    Resources after: Fuel: 100-15+30-20-15-35+30=75, Shield: 100
# Time used: ~19 seconds
# Result: SUCCESS with resources to spare BUT you visit 4 planets vs 3