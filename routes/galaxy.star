# Complex Galaxy - Multiple Missions
# A more complex galaxy with various challenges

# Central hub area
planet "Starport Alpha" at (0, 0) type fuel
planet "Trading Post" at (150, 150) type none
planet "Repair Station Beta" at (-150, 150) type repair

# Outer rim
planet "Mining Colony" at (300, 0) type none
planet "Research Outpost" at (0, 300) type fuel
planet "Frontier Base" at (-300, 0) type repair
planet "Lost Temple" at (0, -300) type none

# Remote destinations
planet "Deep Space Station" at (400, 400) type none
planet "Abandoned Nexus" at (-400, -400) type none

# Main hub connections
route "Starport Alpha" <-> "Trading Post" fuel 10-15 time 3
route "Starport Alpha" <-> "Repair Station Beta" fuel 10-15 time 3
route "Trading Post" <-> "Repair Station Beta" fuel 15-20 time 4

# Hub to outer rim
route "Starport Alpha" --> "Mining Colony" fuel 20-30 time 5
route "Trading Post" --> "Research Outpost" fuel 25-35 time 6
route "Repair Station Beta" --> "Frontier Base" fuel 20-30 time 5
route "Starport Alpha" --> "Lost Temple" fuel 25-35 time 6

# Outer rim connections
route "Mining Colony" <-> "Research Outpost" fuel 30-40 time 7
route "Research Outpost" <-> "Frontier Base" fuel 30-40 time 7
route "Frontier Base" <-> "Lost Temple" fuel 30-40 time 7
route "Lost Temple" <-> "Mining Colony" fuel 30-40 time 7

# Remote destinations
route "Research Outpost" --> "Deep Space Station" fuel 40-50 time 8
route "Lost Temple" --> "Abandoned Nexus" fuel 45-55 time 9

# Return routes from remote
route "Deep Space Station" --> "Mining Colony" fuel 30-40 time 6
route "Abandoned Nexus" --> "Frontier Base" fuel 35-45 time 7

# Hazards near hub
hazard asteroid "Belt Alpha" at (100, 0) radius 40 damage 15
hazard pirates "Raider Hideout" at (-100, 0) radius 50 fuelLoss 20

# Hazards in outer rim
hazard radiation "Solar Storm" at (300, 300) radius 80 damage 25
hazard nebula "Crimson Mist" at (-300, 300) radius 100
hazard asteroid "Debris Field" at (0, -200) radius 60 damage 20

# Remote hazards
hazard pirates "Outlaw Territory" at (350, 350) radius 70 fuelLoss 30
hazard radiation "Dead Zone" at (-350, -350) radius 90 damage 35

# Missions
mission "Milk Run" from "Starport Alpha" to "Trading Post" timeLimit 30
mission "Rescue Mission" from "Starport Alpha" to "Research Outpost" timeLimit 60
mission "Deep Expedition" from "Starport Alpha" to "Deep Space Station" timeLimit 90
mission "Into the Void" from "Starport Alpha" to "Abandoned Nexus" timeLimit 100
mission "Grand Tour" from "Starport Alpha" to "Lost Temple" timeLimit 80
