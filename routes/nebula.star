# Nebula Gamble
# Take risky shortcuts through nebulas or play it safe

planet "Home" at (0, 0) type none
planet "Shortcut 1" at (200, 100) type none
planet "Shortcut 2" at (400, 200) type none
planet "Safe Route 1" at (150, -100) type fuel
planet "Safe Route 2" at (350, -200) type repair
planet "Goal" at (600, 0) type none

route "Home" --> "Shortcut 1" fuel 5-10 time 3
route "Shortcut 1" --> "Shortcut 2" fuel 5-10 time 3
route "Shortcut 2" --> "Goal" fuel 5-10 time 3

route "Home" --> "Safe Route 1" fuel 10-20 time 4
route "Safe Route 1" --> "Safe Route 2" fuel 10-20 time 4
route "Safe Route 2" --> "Goal" fuel 10-20 time 4

hazard nebula "Green Veil" at (300, 150) radius 120

mission "Nebula Gamble" from "Home" to "Goal" timeLimit 90
