# Hazard Choice Example
# Choose between safe route with repairs or direct route through hazards

planet "Start" at (0, 0) type none
planet "Safe Haven" at (300, 0) type repair
planet "Fuel Depot" at (150, 150) type fuel
planet "Target" at (300, 300) type none

route "Start" --> "Safe Haven" fuel 20-30 time 5
route "Start" --> "Fuel Depot" fuel 10-20 time 4
route "Fuel Depot" --> "Target" fuel 15-25 time 4
route "Safe Haven" --> "Target" fuel 10-20 time 4

hazard asteroid  "Rocky Belt" at (150, 0) radius 60 damage 30
hazard pirates   "Ambush Sector" at (225, 225) radius 50 fuelLoss 25

mission "Pick Your Poison" from "Start" to "Target" timeLimit 50
