# Copyright 2024 Bartek thindil Jasicki
#
# This file is part of Steam Sky.
#
# Steam Sky is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Steam Sky is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

import std/[os, tables]
import ../game

type ThemeRecord* = object
  ## Data structure for themes settings
  ##
  ## * name                  - Name of theme
  ## * fileName             - Name of .tcl file of theme
  ## * enemyShipIcon       - Icon used for Enemy Ship event
  ## * attackOnBaseIcon   - Icon used for Attack on Base event
  ## * diseaseIcon          - Icon used for Disease event
  ## * doublePriceIcon      - Icon used for Double Price event
  ## * fullDocksIcon       - Icon used for Full Docks event
  ## * enemyPatrolIcon     - Icon used for Enemy Patrol event
  ## * traderIcon           - Icon used for Trader event
  ## * friendlyShipIcon    - Icon used for Friendly Ship event
  ## * deliverIcon          - Icon used for Deliver Item mission
  ## * destroyIcon          - Icon used for Destroy Ship mission
  ## * patrolIcon           - Icon used for Patrol Area mission
  ## * exploreIcon          - Icon used for Explore Area mission
  ## * passengerIcon        - Icon used for Transport Passenger mission
  ## * pilotIcon            - Icon used for Pilot info
  ## * engineerIcon         - Icon used for Engineer info
  ## * gunnerIcon           - Icon used for Gunners info
  ## * crewTraderIcon      - Icon used for Trader info
  ## * repairIcon           - Icon used for Repairs info when repairs are on
  ## * noRepairIcon        - Icon used for Repairs info when noone is assigned
  ## * repairOrderIcon     - Icon used for giving repair order to all crew members
  ## * upgradeIcon          - Icon used for Upgrade info when upgrade is on
  ## * noUpgradeIcon       - Icon used for Upgrade info when noone is assigned
  ## * cleanIcon            - Icon used for Clean Ship info when cleaning is on
  ## * noCleanIcon         - Icon used for Clean Ship info when noone is assigned
  ## * cleanOrderIcon      - Icon used for giving clean ship order to all crew members
  ## * manufactureIcon      - Icon used for Manufacturing info when someone working on it
  ## * noManufactureIcon   - Icon used for Manufacturing info when noone is assigned
  ## * moveMapUpIcon      - Icon used for move map up button
  ## * moveMapDownIcon    - Icon used for move map down button
  ## * moveMapLeftIcon    - Icon used for move map left button
  ## * moveMapRightIcon   - Icon used for move map right button
  ## * noFuelIcon          - Icon used for show warning about no fuel
  ## * lowFuelIcon         - Icon used for show warning about low level of fuel
  ## * noFoodIcon          - Icon used for show warning about no food
  ## * lowFoodIcon         - Icon used for show warning about low level of food
  ## * noDrinksIcon        - Icon used for show warning about no drinks
  ## * lowDrinksIcon       - Icon used for show warning about low level of drinks
  ## * notVisitedBaseIcon - Icon used for show not visited bases on map
  ## * playerShipIcon      - Icon used for show player ship on map
  ## * emptyMapIcon        - Icon used for empty map fields
  ## * targetIcon           - Icon used for player selected target on map
  ## * storyIcon            - Icon used for show story event location on map
  ## * overloadedIcon       - Icon used for show warning about overloaded ship
  ## * arrowUpIcon         - Icon used for moving map or ship up
  ## * arrowDownIcon       - Icon used for moving map or ship down
  ## * arrowLeftIcon       - Icon used for moving map or ship left
  ## * arrowRightIcon      - Icon used for moving map or ship right
  ## * arrowUpLeftIcon    - Icon used for moving map or ship up and left
  ## * arrowUpRightIcon   - Icon used for moving map or ship up and right
  ## * arrowDownRightIcon - Icon used for moving map or ship down and right
  ## * arrowDownLeftIcon  - Icon used for moving map or ship down and left
  ## * waitIcon             - Icon used for wait one minute
  ## * moveStepIcon        - Icon used for move one step towards destination
  ## * moveToIcon          - Icon used for move ship to destination
  ## * menuIcon             - Icon used for showing menus
  ## * exitIcon             - Icon used for exit button
  ## * randomIcon           - Icon used for setting random value buttons
  ## * maleIcon             - Icon used for male gender
  ## * femaleIcon           - Icon used for female gender
  ## * editIcon             - Icon used for edit button
  ## * showIcon             - Icon used for show button
  ## * cancelIcon           - Icon used for cancel actions
  ## * helpIcon             - Icon used for showing help
  ## * specialHelpColor    - Name of color used to show keys and special names in the help.
  ##                           Can be any value accepted by Tcl.
  ## * underlineHelpColor  - Name of color used for underlined text in the help. Can be any
  ##                           value accepted by Tcl.
  ## * boldHelpColor       - Name of color used for bold text in the help. Can be any value
  ##                           accepted by Tcl.
  ## * italicHelpColor     - Name of color used for italic text in the help. Can be any value
  ##                           accepted by Tcl.
  ## * giveIcon             - Icon used for give items button
  ## * dropIcon             - Icon used for drop items button
  ## * buyIcon              - Icon used for buy items button
  ## * sellIcon             - Icon used for sell items button
  ## * craftIcon            - Icon used for set crafting order button
  ## * studyIcon            - Icon used for set study order button
  ## * deconstructIcon      - Icon used for set deconstruct order button
  ## * negotiateIcon        - Icon used for negotiation button
  ## * cargoIcon            - Icon used for represent the player's ship's cargo
  ## * equipIcon            - Icon used for equip item action
  ## * unequipIcon          - Icon used for unequip item action
  ## * selectAllIcon       - Icon used for select all items on a list button
  ## * unselectAllIcon     - Icon used for unselect all items on a list button
  ## * giveOrderIcon       - Icon used for give order to the crew member button
  ## * noPilotIcon         - Icon used for Pilot info in sentient ships
  ## * noEngineerIcon      - Icon used for Engineer info in sentient ships
  ## * destinationIcon      - Icon used for set the ship destination button
  ## * inventoryIcon        - Icon used for show inventory button
  ## * dismissIcon          - Icon used for dismiss crew member button
  ## * goRestIcon          - Icon used for give order to go rest for the whole crew
  ## * repairPriorityIcon  - Icon used for set the repair priority button
  ## * upgradeButtonIcon   - Icon used for the upgrade button
  ## * powerIcon            - Icon used for the enable or disable engine button
  ## * assignCrewIcon      - Icon used for assign crew members to ship's modules
  ## * assignAmmoIcon      - Icon used for assign ammo to ship's guns
  ## * buyDefaultIcon      - Icon used for buy items button with default color
  ## * sellDefaultIcon     - Icon used for sell items button with default color
  ## * moveIcon             - Icon used for moving items from inventory to cargo
  ## * giveColoredIcon     - Icon used for give items button with green color
  ## * dropColoredIcon     - Icon used for drop items button with green color
  ## * editColoredIcon     - Icon used for edit button with green color
  name*: string
  fileName*: string
  enemyShipIcon*: string
  attackOnBaseIcon*: string
  diseaseIcon*: string
  doublePriceIcon*: string
  fullDocksIcon*: string
  enemyPatrolIcon*: string
  traderIcon*: string
  friendlyShipIcon*: string
  deliverIcon*: string
  destroyIcon*: string
  patrolIcon*: string
  exploreIcon*: string
  passengerIcon*: string
  pilotIcon*: string
  engineerIcon*: string
  gunnerIcon*: string
  crewTraderIcon*: string
  repairIcon*: string
  noRepairIcon*: string
  repairOrderIcon*: string
  upgradeIcon*: string
  noUpgradeIcon*: string
  cleanIcon*: string
  noCleanIcon*: string
  cleanOrderIcon*: string
  manufactureIcon*: string
  noManufactureIcon*: string
  moveMapUpIcon*: string
  moveMapDownIcon*: string
  moveMapLeftIcon*: string
  moveMapRightIcon*: string
  noFuelIcon*: string
  lowFuelIcon*: string
  noFoodIcon*: string
  lowFoodIcon*: string
  noDrinksIcon*: string
  lowDrinksIcon*: string
  notVisitedBaseIcon*: string
  playerShipIcon*: string
  emptyMapIcon*: string
  targetIcon*: string
  storyIcon*: string
  overloadedIcon*: string
  arrowUpIcon*: string
  arrowDownIcon*: string
  arrowLeftIcon*: string
  arrowRightIcon*: string
  arrowUpLeftIcon*: string
  arrowUpRightIcon*: string
  arrowDownRightIcon*: string
  arrowDownLeftIcon*: string
  waitIcon*: string
  moveStepIcon*: string
  moveToIcon*: string
  menuIcon*: string
  exitIcon*: string
  randomIcon*: string
  maleIcon*: string
  femaleIcon*: string
  editIcon*: string
  showIcon*: string
  cancelIcon*: string
  helpIcon*: string
  specialHelpColor*: string
  underlineHelpColor*: string
  boldHelpColor*: string
  italicHelpColor*: string
  giveIcon*: string
  dropIcon*: string
  buyIcon*: string
  sellIcon*: string
  craftIcon*: string
  studyIcon*: string
  deconstructIcon*: string
  negotiateIcon*: string
  cargoIcon*: string
  equipIcon*: string
  unequipIcon*: string
  selectAllIcon*: string
  unselectAllIcon*: string
  giveOrderIcon*: string
  noPilotIcon*: string
  noEngineerIcon*: string
  destinationIcon*: string
  inventoryIcon*: string
  dismissIcon*: string
  goRestIcon*: string
  repairPriorityIcon*: string
  upgradeButtonIcon*: string
  powerIcon*: string
  assignCrewIcon*: string
  assignAmmoIcon*: string
  buyDefaultIcon*: string
  sellDefaultIcon*: string
  moveIcon*: string
  giveColoredIcon*: string
  dropColoredIcon*: string
  editColoredIcon*: string

let
  defaultThemeIconPath = dataDirectory & "ui" & DirSep & "images" & DirSep & "ui" & DirSep
  defaultTheme = ThemeRecord(name: "Default theme", fileName: dataDirectory & DirSep & "ui" & DirSep & "theme.tcl", enemyShipIcon: "\uf51c", attackOnBaseIcon: "\uf543", diseaseIcon: "\uf5a6", doublePriceIcon: "\uf0d6", fullDocksIcon: "\uf057", enemyPatrolIcon: "\uf51b", traderIcon: "\uf197", friendlyShipIcon: "\uf197", deliverIcon: "\uf53b", destroyIcon: "\ufc6a", patrolIcon: "\uf540", exploreIcon: "\uf707", passengerIcon: "\uf183", pilotIcon: defaultThemeIconPath & "pilot.svg", engineerIcon: defaultThemeIconPath & "engineer.svg", gunnerIcon: defaultThemeIconPath & "gunner.svg", crewTraderIcon: defaultThemeIconPath & "crewtrader.svg", repairIcon: defaultThemeIconPath & "repair.svg", noRepairIcon: defaultThemeIconPath & "repair-empty.svg", repairOrderIcon: defaultThemeIconPath & "repair-order.svg", upgradeIcon: defaultThemeIconPath & "upgrade.svg", noUpgradeIcon: defaultThemeIconPath & "upgrade-empty.svg", cleanIcon: defaultThemeIconPath & "clean.svg", noCleanIcon: defaultThemeIconPath & "clean-empty.svg", cleanOrderIcon: defaultThemeIconPath & "clean-order.svg", manufactureIcon: defaultThemeIconPath & "craft.svg", noManufactureIcon: defaultThemeIconPath & "craft-empty.svg", moveMapUpIcon: defaultThemeIconPath & "vertical-flog.svg", moveMapDownIcon: defaultThemeIconPath & "contract.svg", moveMapLeftIcon: defaultThemeIconPath & "horizontal-flip.svg", moveMapRightIcon: defaultThemeIconPath & "flip-right.svg", noFuelIcon: defaultThemeIconPath & "nofuel.svg", lowFuelIcon: defaultThemeIconPath & "lowfuel.svg", noFoodIcon: defaultThemeIconPath & "nofood.svg", lowFoodIcon: defaultThemeIconPath & "lowfood.svg", noDrinksIcon: defaultThemeIconPath & "nodrinks.svg", lowDrinksIcon: defaultThemeIconPath & "lowdrinks.svg", notVisitedBaseIcon: "\u229b", playerShipIcon: "\uf135", emptyMapIcon: "\uf0c8", targetIcon: "\uf05b", storyIcon: "\uf059", overloadedIcon: defaultThemeIconPath & "overloaded.svg", arrowUpIcon: defaultThemeIconPath & "arrow-up.svg", arrowDownIcon: defaultThemeIconPath & "arrow-down.svg", arrowLeftIcon: defaultThemeIconPath & "arrow-left.svg", arrowRightIcon: defaultThemeIconPath & "arrow-right.svg", arrowUpLeftIcon: defaultThemeIconPath & "arrow-up-left.svg", arrowUpRightIcon: defaultThemeIconPath & "arrow-up-right.svg", arrowDownRightIcon: defaultThemeIconPath & "arrow-down-right.svg", arrowDownLeftIcon: defaultThemeIconPath & "arrow-down-left.svg", waitIcon: defaultThemeIconPath & "wait.svg", moveStepIcon: defaultThemeIconPath & "movestep.svg", moveToIcon: defaultThemeIconPath & "moveto.svg", menuIcon: defaultThemeIconPath & "menu.svg", exitIcon: defaultThemeIconPath & "exit.svg", randomIcon: defaultThemeIconPath & "random.svg", maleIcon: defaultThemeIconPath & "male.svg", femaleIcon: defaultThemeIconPath & "female.svg", editIcon: defaultThemeIconPath & "edit.svg", showIcon: defaultThemeIconPath & "show.svg", cancelIcon: defaultThemeIconPath & "cancel.svg", helpIcon: defaultThemeIconPath & "help.svg", specialHelpColor: "yellow", underlineHelpColor: "cadet blue", boldHelpColor: "coral", italicHelpColor: "lime", giveIcon: defaultThemeIconPath & "give.svg", dropIcon: defaultThemeIconPath & "drop.svg", buyIcon: defaultThemeIconPath & "buy.svg", sellIcon: defaultThemeIconPath & "sell.svg", craftIcon: defaultThemeIconPath & "craft-order.svg", studyIcon: defaultThemeIconPath & "study.svg", deconstructIcon: defaultThemeIconPath & "deconstruct.svg", negotiateIcon: defaultThemeIconPath & "negotiate.svg", cargoIcon: defaultThemeIconPath & "cargo.svg", equipIcon: defaultThemeIconPath & "equip.svg", unequipIcon: defaultThemeIconPath & "unequip.svg", selectAllIcon: defaultThemeIconPath & "selectall.svg")

var themesList*: Table[string, ThemeRecord] ## The list of all available themes
