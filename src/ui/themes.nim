# Copyright 2024-2026 Bartek thindil Jasicki
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

## Provides code related to the game's themes' system, like default theme,
## loading them from files, setting images, etc.

import std/[os, parsecfg, streams, strutils, tables, unicode]
import contracts
import ../[config, game, tk]
import coreui, errordialog

type ThemeRecord* = object
  ## Data structure for themes settings
  ##
  ## * name                - Name of theme
  ## * fileName            - Name of .tcl file of theme
  ## * enemyShipIcon       - Icon used for Enemy Ship event
  ## * attackOnBaseIcon    - Icon used for Attack on Base event
  ## * diseaseIcon         - Icon used for Disease event
  ## * doublePriceIcon     - Icon used for Double Price event
  ## * fullDocksIcon       - Icon used for Full Docks event
  ## * enemyPatrolIcon     - Icon used for Enemy Patrol event
  ## * traderIcon          - Icon used for Trader event
  ## * friendlyShipIcon    - Icon used for Friendly Ship event
  ## * deliverIcon         - Icon used for Deliver Item mission
  ## * destroyIcon         - Icon used for Destroy Ship mission
  ## * patrolIcon          - Icon used for Patrol Area mission
  ## * exploreIcon         - Icon used for Explore Area mission
  ## * passengerIcon       - Icon used for Transport Passenger mission
  ## * pilotIcon           - Icon used for Pilot info
  ## * engineerIcon        - Icon used for Engineer info
  ## * gunnerIcon          - Icon used for Gunners info
  ## * crewTraderIcon      - Icon used for Trader info
  ## * repairIcon          - Icon used for Repairs info when repairs are on
  ## * noRepairIcon        - Icon used for Repairs info when noone is assigned
  ## * repairOrderIcon     - Icon used for giving repair order to all crew members
  ## * upgradeIcon         - Icon used for Upgrade info when upgrade is on
  ## * noUpgradeIcon       - Icon used for Upgrade info when noone is assigned
  ## * cleanIcon           - Icon used for Clean Ship info when cleaning is on
  ## * noCleanIcon         - Icon used for Clean Ship info when noone is assigned
  ## * cleanOrderIcon      - Icon used for giving clean ship order to all crew members
  ## * manufactureIcon     - Icon used for Manufacturing info when someone working on it
  ## * noManufactureIcon   - Icon used for Manufacturing info when noone is assigned
  ## * moveMapUpIcon       - Icon used for move map up button
  ## * moveMapDownIcon     - Icon used for move map down button
  ## * moveMapLeftIcon     - Icon used for move map left button
  ## * moveMapRightIcon    - Icon used for move map right button
  ## * noFuelIcon          - Icon used for show warning about no fuel
  ## * lowFuelIcon         - Icon used for show warning about low level of fuel
  ## * noFoodIcon          - Icon used for show warning about no food
  ## * lowFoodIcon         - Icon used for show warning about low level of food
  ## * noDrinksIcon        - Icon used for show warning about no drinks
  ## * lowDrinksIcon       - Icon used for show warning about low level of drinks
  ## * notVisitedBaseIcon  - Icon used for show not visited bases on map
  ## * playerShipIcon      - Icon used for show player ship on map
  ## * emptyMapIcon        - Icon used for empty map fields
  ## * targetIcon          - Icon used for player selected target on map
  ## * storyIcon           - Icon used for show story event location on map
  ## * overloadedIcon      - Icon used for show warning about overloaded ship
  ## * arrowUpIcon         - Icon used for moving map or ship up
  ## * arrowDownIcon       - Icon used for moving map or ship down
  ## * arrowLeftIcon       - Icon used for moving map or ship left
  ## * arrowRightIcon      - Icon used for moving map or ship right
  ## * arrowUpLeftIcon     - Icon used for moving ship up and left
  ## * arrowUpRightIcon    - Icon used for moving map or ship up and right
  ## * arrowDownRightIcon  - Icon used for moving map or ship down and right
  ## * arrowDownLeftIcon   - Icon used for moving map or ship down and left
  ## * waitIcon            - Icon used for wait one minute
  ## * moveStepIcon        - Icon used for move one step towards destination
  ## * moveToIcon          - Icon used for move ship to destination
  ## * menuIcon            - Icon used for showing menus
  ## * exitIcon            - Icon used for exit button
  ## * randomIcon          - Icon used for setting random value buttons
  ## * maleIcon            - Icon used for male gender
  ## * femaleIcon          - Icon used for female gender
  ## * editIcon            - Icon used for edit button
  ## * showIcon            - Icon used for show button
  ## * cancelIcon          - Icon used for cancel actions
  ## * helpIcon            - Icon used for showing help
  ## * specialHelpColor    - Name of color used to show keys and special names in the help.
  ##                           Can be any value accepted by Tcl.
  ## * underlineHelpColor  - Name of color used for underlined text in the help. Can be any
  ##                           value accepted by Tcl.
  ## * boldHelpColor       - Name of color used for bold text in the help. Can be any value
  ##                           accepted by Tcl.
  ## * italicHelpColor     - Name of color used for italic text in the help. Can be any value
  ##                           accepted by Tcl.
  ## * giveIcon            - Icon used for give items button
  ## * dropIcon            - Icon used for drop items button
  ## * buyIcon             - Icon used for buy items button
  ## * sellIcon            - Icon used for sell items button
  ## * craftIcon           - Icon used for set crafting order button
  ## * studyIcon           - Icon used for set study order button
  ## * deconstructIcon     - Icon used for set deconstruct order button
  ## * negotiateIcon       - Icon used for negotiation button
  ## * cargoIcon           - Icon used for represent the player's ship's cargo
  ## * equipIcon           - Icon used for equip item action
  ## * unequipIcon         - Icon used for unequip item action
  ## * selectAllIcon       - Icon used for select all items on a list button
  ## * unselectAllIcon     - Icon used for unselect all items on a list button
  ## * giveOrderIcon       - Icon used for give order to the crew member button
  ## * noPilotIcon         - Icon used for Pilot info in sentient ships
  ## * noEngineerIcon      - Icon used for Engineer info in sentient ships
  ## * destinationIcon     - Icon used for set the ship destination button
  ## * inventoryIcon       - Icon used for show inventory button
  ## * dismissIcon         - Icon used for dismiss crew member button
  ## * goRestIcon          - Icon used for give order to go rest for the whole crew
  ## * repairPriorityIcon  - Icon used for set the repair priority button
  ## * upgradeButtonIcon   - Icon used for the upgrade button
  ## * powerIcon           - Icon used for the enable or disable engine button
  ## * assignCrewIcon      - Icon used for assign crew members to ship's modules
  ## * assignAmmoIcon      - Icon used for assign ammo to ship's guns
  ## * buyDefaultIcon      - Icon used for buy items button with default color
  ## * sellDefaultIcon     - Icon used for sell items button with default color
  ## * moveIcon            - Icon used for moving items from inventory to cargo
  ## * giveColoredIcon     - Icon used for give items button with green color
  ## * dropColoredIcon     - Icon used for drop items button with green color
  ## * editColoredIcon     - Icon used for edit button with green color
  ## * showColoredIcon     - Icon used for show button with green color
  ## * negotiateColoredIcon - Icon used for negotiation button with green color
  ## * craftColoredIcon    - Icon used for set crafting order button with green color
  ## * studyColoredIcon    - Icon used for set study order button with green color
  ## * deconstructColoredIcon - Icon used for set deconstruct order button with green color
  ## * giveOrderColoredIcon - Icon used for give order to the crew member button with green color
  ## * foodIcon            - Icon used for show info about amount of food
  ## * fuelIcon            - Icon used for show info about amount of fuel
  ## * drinksIcon          - Icon used for show info about amount of drinks
  ## * expandIcon          - Icon used for expanding info sections
  ## * contractIcon        - Icon used for contracting info sections
  ## * moreOptionsIcon     - Icon used for show more options in info sections
  ## * mapMenuIcon         - Icon used for showing the map's menu
  ## * mapArrowUpLeftIcon  - Icon used for moving map up and left
  ## * mapArrowUpRightIcon - Icon used for moving map up and right
  ## * mapArrowUpIcon      - Icon used for moving map up
  ## * mapArrowLeftIcon    - Icon used for moving map left
  ## * mapArrowRightIcon   - Icon used for moving map right
  ## * mapArrowDownLeftIcon - Icon used for moving map down and left
  ## * mapArrowDownIcon    - Icon used for moving map down
  ## * mapArrowDownRightIcon - Icon used for moving map down and right
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
  showColoredIcon*: string
  negotiateColoredIcon*: string
  craftColoredIcon*: string
  studyColoredIcon*: string
  deconstructColoredIcon*: string
  giveOrderColoredIcon*: string
  foodIcon*: string
  fuelIcon*: string
  drinksIcon*: string
  expandIcon*: string
  contractIcon*: string
  moreOptionsIcon*: string
  mapMenuIcon*: string
  mapArrowUpLeftIcon*: string
  mapArrowUpRightIcon*: string
  mapArrowUpIcon*: string
  mapArrowLeftIcon*: string
  mapArrowRightIcon*: string
  mapArrowDownLeftIcon*: string
  mapArrowDownIcon*: string
  mapArrowDownRightIcon*: string

var themesList*: Table[string, ThemeRecord] = initTable[string, ThemeRecord]()
  ## The list of all available themes

let
  defaultThemeIconPath: string = dataDirectory.string & "ui" & DirSep & "images" & DirSep &
      "ui" & DirSep ## The path to the default theme's icons
  defaultTheme: ThemeRecord = ThemeRecord(name: "Default theme", fileName: dataDirectory.string &
      DirSep & "ui" & DirSep & "theme.tcl", enemyShipIcon: "\uf51c",
      attackOnBaseIcon: "\uf543", diseaseIcon: "\uf5a6",
      doublePriceIcon: "\uf0d6", fullDocksIcon: "\uf057",
      enemyPatrolIcon: "\uf51b", traderIcon: "\uf197",
      friendlyShipIcon: "\uf197", deliverIcon: "\uf53b", destroyIcon: "\ufc6a",
      patrolIcon: "\uf540", exploreIcon: "\uf707", passengerIcon: "\uf183",
      pilotIcon: defaultThemeIconPath & "pilot.svg",
      engineerIcon: defaultThemeIconPath & "engineer.svg",
      gunnerIcon: defaultThemeIconPath & "gunner.svg",
      crewTraderIcon: defaultThemeIconPath & "crewtrader.svg",
      repairIcon: defaultThemeIconPath & "repair.svg",
      noRepairIcon: defaultThemeIconPath & "repair-empty.svg",
      repairOrderIcon: defaultThemeIconPath & "repair-order.svg",
      upgradeIcon: defaultThemeIconPath & "upgrade.svg",
      noUpgradeIcon: defaultThemeIconPath & "upgrade-empty.svg",
      cleanIcon: defaultThemeIconPath & "clean.svg",
      noCleanIcon: defaultThemeIconPath & "clean-empty.svg",
      cleanOrderIcon: defaultThemeIconPath & "clean-order.svg",
      manufactureIcon: defaultThemeIconPath & "craft.svg",
      noManufactureIcon: defaultThemeIconPath & "craft-empty.svg",
      moveMapUpIcon: defaultThemeIconPath & "vertical-flip.svg",
      moveMapDownIcon: defaultThemeIconPath & "contract.svg",
      moveMapLeftIcon: defaultThemeIconPath & "horizontal-flip.svg",
      moveMapRightIcon: defaultThemeIconPath & "flip-right.svg",
      noFuelIcon: defaultThemeIconPath & "nofuel.svg",
      lowFuelIcon: defaultThemeIconPath & "lowfuel.svg",
      noFoodIcon: defaultThemeIconPath & "nofood.svg",
      lowFoodIcon: defaultThemeIconPath & "lowfood.svg",
      noDrinksIcon: defaultThemeIconPath & "nodrinks.svg",
      lowDrinksIcon: defaultThemeIconPath & "lowdrinks.svg",
      notVisitedBaseIcon: "\u229b", playerShipIcon: "\uf135",
      emptyMapIcon: "\uf0c8", targetIcon: "\uf05b", storyIcon: "\uf059",
      overloadedIcon: defaultThemeIconPath & "overloaded.svg",
      arrowUpIcon: defaultThemeIconPath & "arrow-up.svg",
      arrowDownIcon: defaultThemeIconPath & "arrow-down.svg",
      arrowLeftIcon: defaultThemeIconPath & "arrow-left.svg",
      arrowRightIcon: defaultThemeIconPath & "arrow-right.svg",
      arrowUpLeftIcon: defaultThemeIconPath & "arrow-up-left.svg",
      arrowUpRightIcon: defaultThemeIconPath & "arrow-up-right.svg",
      arrowDownRightIcon: defaultThemeIconPath & "arrow-down-right.svg",
      arrowDownLeftIcon: defaultThemeIconPath & "arrow-down-left.svg",
      waitIcon: defaultThemeIconPath & "wait.svg",
      moveStepIcon: defaultThemeIconPath & "movestep.svg",
      moveToIcon: defaultThemeIconPath & "moveto.svg",
      menuIcon: defaultThemeIconPath & "menu.svg",
      exitIcon: defaultThemeIconPath & "exit.svg",
      randomIcon: defaultThemeIconPath & "random.svg",
      maleIcon: defaultThemeIconPath & "male.svg",
      femaleIcon: defaultThemeIconPath & "female.svg",
      editIcon: defaultThemeIconPath & "edit.svg",
      showIcon: defaultThemeIconPath & "show.svg",
      cancelIcon: defaultThemeIconPath & "cancel.svg",
      helpIcon: defaultThemeIconPath & "help.svg", specialHelpColor: "yellow",
      underlineHelpColor: "cadet blue", boldHelpColor: "coral",
      italicHelpColor: "lime", giveIcon: defaultThemeIconPath & "give.svg",
      dropIcon: defaultThemeIconPath & "drop.svg",
      buyIcon: defaultThemeIconPath & "buy.svg",
      sellIcon: defaultThemeIconPath & "sell.svg",
      craftIcon: defaultThemeIconPath & "craft-order.svg",
      studyIcon: defaultThemeIconPath & "study.svg",
      deconstructIcon: defaultThemeIconPath & "deconstruct.svg",
      negotiateIcon: defaultThemeIconPath & "negotiate.svg",
      cargoIcon: defaultThemeIconPath & "cargo.svg",
      equipIcon: defaultThemeIconPath & "equip.svg",
      unequipIcon: defaultThemeIconPath & "unequip.svg",
      selectAllIcon: defaultThemeIconPath & "selectall.svg",
      unselectAllIcon: defaultThemeIconPath & "unselectall.svg",
      giveOrderIcon: defaultThemeIconPath & "giveorder.svg",
      noPilotIcon: defaultThemeIconPath & "nopilot.svg",
      noEngineerIcon: defaultThemeIconPath & "noengineer.svg",
      destinationIcon: defaultThemeIconPath & "destination.svg",
      inventoryIcon: defaultThemeIconPath & "inventory.svg",
      dismissIcon: defaultThemeIconPath & "dismiss.svg",
      goRestIcon: defaultThemeIconPath & "gorest.svg",
      repairPriorityIcon: defaultThemeIconPath & "repair-priority.svg",
      upgradeButtonIcon: defaultThemeIconPath & "upgrade-button.svg",
      powerIcon: defaultThemeIconPath & "power.svg",
      assignCrewIcon: defaultThemeIconPath & "giveorder.svg",
      assignAmmoIcon: defaultThemeIconPath & "assignammo.svg",
      buyDefaultIcon: defaultThemeIconPath & "buy2.svg",
      sellDefaultIcon: defaultThemeIconPath & "sell2.svg",
      moveIcon: defaultThemeIconPath & "cargo2.svg",
      giveColoredIcon: defaultThemeIconPath & "give2.svg",
      dropColoredIcon: defaultThemeIconPath & "drop2.svg",
      editColoredIcon: defaultThemeIconPath & "edit2.svg",
      showColoredIcon: defaultThemeIconPath & "show2.svg",
      negotiateColoredIcon: defaultThemeIconPath & "negotiate2.svg",
      craftColoredIcon: defaultThemeIconPath & "craft-order2.svg",
      studyColoredIcon: defaultThemeIconPath & "study2.svg",
      deconstructColoredIcon: defaultThemeIconPath & "deconstruct2.svg",
      giveOrderColoredIcon: defaultThemeIconPath & "giveorder2.svg",
      foodIcon: defaultThemeIconPath & "food.svg",
      fuelIcon: defaultThemeIconPath & "fuel.svg",
      drinksIcon: defaultThemeIconPath & "drinks.svg",
      expandIcon: defaultThemeIconPath & "expand.svg",
      contractIcon: defaultThemeIconPath & "contract2.svg",
      moreOptionsIcon: defaultThemeIconPath & "moreoptions.svg",
      mapMenuIcon: defaultThemeIconPath & "menu2.svg",
      mapArrowUpLeftIcon: defaultThemeIconPath & "arrow-up-left2.svg",
      mapArrowUpRightIcon: defaultThemeIconPath & "arrow-up-right2.svg",
      mapArrowUpIcon: defaultThemeIconPath & "arrow-up2.svg",
      mapArrowLeftIcon: defaultThemeIconPath & "arrow-left2.svg",
      mapArrowRightIcon: defaultThemeIconPath & "arrow-right2.svg",
      mapArrowDownLeftIcon: defaultThemeIconPath & "arrow-down-left2.svg",
      mapArrowDownIcon: defaultThemeIconPath & "arrow-down2.svg",
      mapArrowDownRightIcon: defaultThemeIconPath & "arrow-down-right2.svg")
    ## The default game'st theme

proc loadThemes*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect,
    ReadDirEffect, ReadIOEffect, RootEffect], contractual.} =
  ## Load all data of the game themes
  var theme: ThemeRecord = defaultTheme
  themesList["steamsky"] = theme
  try:
    for themeDir in walkDirs(pattern = themesDirectory):
      for configName in walkPattern(pattern = themeDir & DirSep & "*.cfg"):
        var configFile: FileStream = newFileStream(filename = configName, mode = fmRead)
        if configFile == nil:
          continue
        var parser: CfgParser = CfgParser()
        try:
          parser.open(input = configFile, filename = configName)
        except OSError, IOError, Exception:
          echo "Can't initialize configuration file parser. Reason: " &
              getCurrentExceptionMsg()
          return
        while true:
          try:
            let entry: CfgEvent = parser.next()
            case entry.kind
            of cfgEof:
              break
            of cfgKeyValuePair, cfgOption:
              case entry.key
              of "Name":
                theme.name = entry.value
              of "FileName":
                theme.fileName = themeDir & DirSep & entry.value
              of "EnemyShipIcon":
                theme.enemyShipIcon = fromHex[int32](
                    s = entry.value).Rune.toUTF8
              of "attackOnBaseIcon":
                theme.attackOnBaseIcon = fromHex[int32](
                    s = entry.value).Rune.toUTF8
              of "DiseaseIcon":
                theme.diseaseIcon = fromHex[int32](s = entry.value).Rune.toUTF8
              of "DoublePriceIcon":
                theme.doublePriceIcon = fromHex[int32](
                    s = entry.value).Rune.toUTF8
              of "FullDocksIcon":
                theme.fullDocksIcon = fromHex[int32](
                    s = entry.value).Rune.toUTF8
              of "EnemyPatrolIcon":
                theme.enemyPatrolIcon = fromHex[int32](
                    s = entry.value).Rune.toUTF8
              of "TraderIcon":
                theme.traderIcon = fromHex[int32](s = entry.value).Rune.toUTF8
              of "FriendlyShipIcon":
                theme.friendlyShipIcon = fromHex[int32](
                    s = entry.value).Rune.toUTF8
              of "DeliverIcon":
                theme.deliverIcon = fromHex[int32](s = entry.value).Rune.toUTF8
              of "DestroyIcon":
                theme.destroyIcon = fromHex[int32](s = entry.value).Rune.toUTF8
              of "PatrolIcon":
                theme.patrolIcon = fromHex[int32](s = entry.value).Rune.toUTF8
              of "ExploreIcon":
                theme.exploreIcon = fromHex[int32](s = entry.value).Rune.toUTF8
              of "PassengerIcon":
                theme.passengerIcon = fromHex[int32](
                    s = entry.value).Rune.toUTF8
              of "PilotIcon":
                theme.pilotIcon = entry.value.unixToNativePath
              of "EngineerIcon":
                theme.engineerIcon = entry.value.unixToNativePath
              of "GunnerIcon":
                theme.gunnerIcon = entry.value.unixToNativePath
              of "CrewTraderIcon":
                theme.crewTraderIcon = entry.value.unixToNativePath
              of "RepairIcon":
                theme.repairIcon = entry.value.unixToNativePath
              of "NoRepairIcon":
                theme.noRepairIcon = entry.value.unixToNativePath
              of "RepairOrderIcon":
                theme.repairOrderIcon = entry.value.unixToNativePath
              of "UpgradeIcon":
                theme.upgradeIcon = entry.value.unixToNativePath
              of "NoUpgradeIcon":
                theme.noUpgradeIcon = entry.value.unixToNativePath
              of "CleanIcon":
                theme.cleanIcon = entry.value.unixToNativePath
              of "NoCleanIcon":
                theme.noCleanIcon = entry.value.unixToNativePath
              of "CleanOrderIcon":
                theme.cleanOrderIcon = entry.value.unixToNativePath
              of "ManufactureIcon":
                theme.manufactureIcon = entry.value.unixToNativePath
              of "NoManufactureIcon":
                theme.noManufactureIcon = entry.value.unixToNativePath
              of "MoveMapUpIcon":
                theme.moveMapUpIcon = entry.value.unixToNativePath
              of "MoveMapDownIcon":
                theme.moveMapDownIcon = entry.value.unixToNativePath
              of "MoveMapLeftIcon":
                theme.moveMapLeftIcon = entry.value.unixToNativePath
              of "MoveMapRightIcon":
                theme.moveMapRightIcon = entry.value.unixToNativePath
              of "NoFuelIcon":
                theme.noFuelIcon = entry.value.unixToNativePath
              of "LowFuelIcon":
                theme.lowFuelIcon = entry.value.unixToNativePath
              of "NoFoodIcon":
                theme.noFoodIcon = entry.value.unixToNativePath
              of "LowFoodIcon":
                theme.lowFoodIcon = entry.value.unixToNativePath
              of "NoDrinksIcon":
                theme.noDrinksIcon = entry.value.unixToNativePath
              of "LowDrinksIcon":
                theme.lowDrinksIcon = entry.value.unixToNativePath
              of "NotVisitedBaseIcon":
                theme.notVisitedBaseIcon = fromHex[int32](
                    s = entry.value).Rune.toUTF8
              of "EmptyMapIcon":
                theme.emptyMapIcon = fromHex[int32](s = entry.value).Rune.toUTF8
              of "TargetIcon":
                theme.targetIcon = fromHex[int32](s = entry.value).Rune.toUTF8
              of "StoryIcon":
                theme.storyIcon = fromHex[int32](s = entry.value).Rune.toUTF8
              of "OverloadedIcon":
                theme.overloadedIcon = entry.value.unixToNativePath
              of "ArrowUpIcon":
                theme.arrowUpIcon = entry.value.unixToNativePath
              of "ArrowDownIcon":
                theme.arrowDownIcon = entry.value.unixToNativePath
              of "ArrowLeftIcon":
                theme.arrowLeftIcon = entry.value.unixToNativePath
              of "ArrowRightIcon":
                theme.arrowRightIcon = entry.value.unixToNativePath
              of "ArrowUpLeftIcon":
                theme.arrowUpLeftIcon = entry.value.unixToNativePath
              of "ArrowUpRightIcon":
                theme.arrowUpRightIcon = entry.value.unixToNativePath
              of "ArrowDownLeftIcon":
                theme.arrowDownIcon = entry.value.unixToNativePath
              of "ArrowDownRightIcon":
                theme.arrowDownRightIcon = entry.value.unixToNativePath
              of "WaitIcon":
                theme.waitIcon = entry.value.unixToNativePath
              of "MoveStepIcon":
                theme.moveStepIcon = entry.value.unixToNativePath
              of "MoveToIcon":
                theme.moveToIcon = entry.value.unixToNativePath
              of "MenuIcon":
                theme.menuIcon = entry.value.unixToNativePath
              of "ExitIcon":
                theme.exitIcon = entry.value.unixToNativePath
              of "RandomIcon":
                theme.randomIcon = entry.value.unixToNativePath
              of "MaleIcon":
                theme.maleIcon = entry.value.unixToNativePath
              of "FemaleIcon":
                theme.femaleIcon = entry.value.unixToNativePath
              of "EditIcon":
                theme.editIcon = entry.value.unixToNativePath
              of "ShowIcon":
                theme.showIcon = entry.value.unixToNativePath
              of "CancelIcon":
                theme.cancelIcon = entry.value.unixToNativePath
              of "HelpIcon":
                theme.helpIcon = entry.value.unixToNativePath
              of "SpecialHelpColor":
                theme.specialHelpColor = entry.value
              of "UnderlineHelpColor":
                theme.underlineHelpColor = entry.value
              of "BoldHelpColor":
                theme.boldHelpColor = entry.value
              of "ItalicHelpColor":
                theme.italicHelpColor = entry.value
              of "GiveIcon":
                theme.giveIcon = entry.value.unixToNativePath
              of "DropIcon":
                theme.dropIcon = entry.value.unixToNativePath
              of "BuyIcon":
                theme.buyIcon = entry.value.unixToNativePath
              of "SellIcon":
                theme.sellIcon = entry.value.unixToNativePath
              of "CraftIcon":
                theme.craftIcon = entry.value.unixToNativePath
              of "StudyIcon":
                theme.studyIcon = entry.value.unixToNativePath
              of "DeconstructIcon":
                theme.deconstructIcon = entry.value.unixToNativePath
              of "NegotiateIcon":
                theme.negotiateIcon = entry.value.unixToNativePath
              of "CargoIcon":
                theme.cargoIcon = entry.value.unixToNativePath
              of "EquipIcon":
                theme.equipIcon = entry.value.unixToNativePath
              of "UnequipIcon":
                theme.unequipIcon = entry.value.unixToNativePath
              of "SelectAllIcon":
                theme.selectAllIcon = entry.value.unixToNativePath
              of "UnselectAllIcon":
                theme.unselectAllIcon = entry.value.unixToNativePath
              of "GiveOrderIcon":
                theme.giveOrderIcon = entry.value.unixToNativePath
              of "NoPilotIcon":
                theme.noPilotIcon = entry.value.unixToNativePath
              of "NoEngineerIcon":
                theme.noEngineerIcon = entry.value.unixToNativePath
              of "DestinationIcon":
                theme.destinationIcon = entry.value.unixToNativePath
              of "InventoryIcon":
                theme.inventoryIcon = entry.value.unixToNativePath
              of "DismissIcon":
                theme.dismissIcon = entry.value.unixToNativePath
              of "GoRestIcon":
                theme.goRestIcon = entry.value.unixToNativePath
              of "repairPriorityIcon":
                theme.repairPriorityIcon = entry.value.unixToNativePath
              of "UpgradeButtonIcon":
                theme.upgradeButtonIcon = entry.value.unixToNativePath
              of "PowerIcon":
                theme.powerIcon = entry.value.unixToNativePath
              of "AssignCrewIcon":
                theme.assignCrewIcon = entry.value.unixToNativePath
              of "AssignAmmoIcon":
                theme.assignAmmoIcon = entry.value.unixToNativePath
              of "BuyDefaultIcon":
                theme.buyDefaultIcon = entry.value.unixToNativePath
              of "SellDefaultIcon":
                theme.sellDefaultIcon = entry.value.unixToNativePath
              of "MoveIcon":
                theme.moveIcon = entry.value.unixToNativePath
              of "GiveColoredIcon":
                theme.giveColoredIcon = entry.value.unixToNativePath
              of "DropColoredIcon":
                theme.dropColoredIcon = entry.value.unixToNativePath
              of "EditColoredIcon":
                theme.editColoredIcon = entry.value.unixToNativePath
              of "ShowColoredIcon":
                theme.showColoredIcon = entry.value.unixToNativePath
              of "NegotiateColoredIcon":
                theme.negotiateColoredIcon = entry.value.unixToNativePath
              of "CraftColoredIcon":
                theme.craftColoredIcon = entry.value.unixToNativePath
              of "StudyColoredIcon":
                theme.studyColoredIcon = entry.value.unixToNativePath
              of "DeconstructColoredIcon":
                theme.deconstructColoredIcon = entry.value.unixToNativePath
              of "GiveOrderColoredIcon":
                theme.giveOrderColoredIcon = entry.value.unixToNativePath
              of "FoodIcon":
                theme.foodIcon = entry.value.unixToNativePath
              of "FuelIcon":
                theme.fuelIcon = entry.value.unixToNativePath
              of "DrinksIcon":
                theme.drinksIcon = entry.value.unixToNativePath
              of "ExpandIcon":
                theme.expandIcon = entry.value.unixToNativePath
              of "ContractIcon":
                theme.contractIcon = entry.value.unixToNativePath
              of "MoreOptionsIcon":
                theme.moreOptionsIcon = entry.value.unixToNativePath
              of "MapMenuIcon":
                theme.mapMenuIcon = entry.value.unixToNativePath
              of "MapArrowUpLeftIcon":
                theme.mapArrowUpLeftIcon = entry.value.unixToNativePath
              of "MapArrowUpRightIcon":
                theme.mapArrowUpRightIcon = entry.value.unixToNativePath
              of "MapArrowUpIcon":
                theme.mapArrowUpIcon = entry.value.unixToNativePath
              of "MapArrowLeftIcon":
                theme.mapArrowLeftIcon = entry.value.unixToNativePath
              of "MapArrowRightIcon":
                theme.mapArrowRightIcon = entry.value.unixToNativePath
              of "MapArrowDownLeftIcon":
                theme.mapArrowDownLeftIcon = entry.value.unixToNativePath
              of "MapArrowDownIcon":
                theme.mapArrowDownIcon = entry.value.unixToNativePath
              of "MapArrowDownRightIcon":
                theme.mapArrowDownRightIcon = entry.value.unixToNativePath
              else:
                discard
            of cfgError:
              echo entry.msg
            of cfgSectionStart:
              discard
          except ValueError, OSError, IOError:
            echo "Invalid data in the theme configuration file. Details: " &
                getCurrentExceptionMsg()
            continue
        try:
          parser.close()
        except OSError, IOError, Exception:
          echo "Can't close configuration file parser. Reason: " &
              getCurrentExceptionMsg()
        themesList[themeDir.lastPathPart] = theme
        theme = defaultTheme
  except:
    discard
  if gameSettings.interfaceTheme notin themesList:
    gameSettings.interfaceTheme = "steamsky"

proc loadThemeImages*() {.raises: [], tags: [WriteIOEffect, TimeEffect,
    RootEffect], contractual.} =
  ## Load all images of the current game theme
  const imagesNames: array[97, string] = ["piloticon", "engineericon", "gunnericon",
      "crewtradericon", "repairicon", "norepairicon", "repairordericon",
      "upgradeicon", "noupgradeicon", "cleanicon", "nocleanicon",
      "cleanordericon", "manufactureicon", "nocrafticon", "nofuelicon",
      "nofoodicon", "lowfuelicon", "lowfoodicon", "nodrinksicon",
      "lowdrinksicon", "movemapupicon", "movemapdownicon", "movemaplefticon",
      "movemaprighticon", "overloadedicon", "arrowupicon", "arrowdownicon",
      "arrowlefticon", "arrowrighticon", "arrowuplefticon", "arrowuprighticon",
      "arrowdownrighticon", "arrowdownlefticon", "waiticon", "movestepicon",
      "movetoicon", "menuicon", "exiticon", "randomicon", "maleicon",
      "femaleicon", "editicon", "showicon", "cancelicon", "helpicon",
      "giveicon", "dropicon", "buyicon", "sellicon", "crafticon", "studyicon",
      "deconstructicon", "negotiateicon", "cargoicon", "equipicon",
      "unequipicon", "selectallicon", "unselectallicon", "giveordericon",
      "nopiloticon", "noengineericon", "destinationicon", "inventoryicon",
      "dismissicon", "goresticon", "repairpriorityicon", "upgradebuttonicon",
      "powericon", "assigncrewicon", "assignammoicon", "buy2icon", "sell2icon",
      "moveicon", "give2icon", "drop2icon", "edit2icon", "show2icon",
      "negotiate2icon", "craft2icon", "study2icon", "deconstruct2icon",
      "giveorder2icon", "foodicon", "fuelicon", "drinksicon", "expandicon",
      "contracticon", "moreoptionsicon", "mapmenuicon", "maparrowuplefticon",
      "maparrowuprighticon", "maparrowupicon", "maparrowlefticon",
      "maparrowrighticon", "maparrowdownlefticon", "maparrowdownicon", "maparrowdownrighticon"]
  let
    theme: ThemeRecord = try:
        themesList[gameSettings.interfaceTheme]
      except:
        showError(message = "Can't find theme '" & gameSettings.interfaceTheme & "'")
        return
    imagesFiles: array[97, string] = [theme.pilotIcon, theme.engineerIcon, theme.gunnerIcon,
        theme.crewTraderIcon, theme.repairIcon, theme.noRepairIcon,
        theme.repairOrderIcon, theme.upgradeIcon, theme.noUpgradeIcon,
        theme.cleanIcon, theme.noCleanIcon, theme.cleanOrderIcon,
        theme.manufactureIcon, theme.noManufactureIcon, theme.noFuelIcon,
        theme.noFoodIcon, theme.lowFuelIcon, theme.lowFoodIcon,
        theme.noDrinksIcon, theme.lowDrinksIcon, theme.moveMapUpIcon,
        theme.moveMapDownIcon, theme.moveMapLeftIcon, theme.moveMapRightIcon,
        theme.overloadedIcon, theme.arrowUpIcon, theme.arrowDownIcon,
        theme.arrowLeftIcon, theme.arrowRightIcon, theme.arrowUpLeftIcon,
        theme.arrowUpRightIcon, theme.arrowDownRightIcon,
        theme.arrowDownLeftIcon, theme.waitIcon, theme.moveStepIcon,
        theme.moveToIcon, theme.menuIcon, theme.exitIcon, theme.randomIcon,
        theme.maleIcon, theme.femaleIcon, theme.editIcon, theme.showIcon,
        theme.cancelIcon, theme.helpIcon, theme.giveIcon, theme.dropIcon,
        theme.buyIcon, theme.sellIcon, theme.craftIcon, theme.studyIcon,
        theme.deconstructIcon, theme.negotiateIcon, theme.cargoIcon,
        theme.equipIcon, theme.unequipIcon, theme.selectAllIcon,
        theme.unselectAllIcon, theme.giveOrderIcon, theme.noPilotIcon,
        theme.noEngineerIcon, theme.destinationIcon, theme.inventoryIcon,
        theme.dismissIcon, theme.goRestIcon, theme.repairPriorityIcon,
        theme.upgradeButtonIcon, theme.powerIcon, theme.assignCrewIcon,
        theme.assignAmmoIcon, theme.buyDefaultIcon, theme.sellDefaultIcon,
        theme.moveIcon, theme.giveColoredIcon, theme.dropColoredIcon,
        theme.editColoredIcon, theme.showColoredIcon,
        theme.negotiateColoredIcon, theme.craftColoredIcon,
        theme.studyColoredIcon, theme.deconstructColoredIcon,
        theme.giveOrderColoredIcon, theme.foodIcon, theme.fuelIcon,
        theme.drinksIcon, theme.expandIcon, theme.contractIcon,
        theme.moreOptionsIcon, theme.mapMenuIcon, theme.mapArrowUpLeftIcon,
        theme.mapArrowUpRightIcon, theme.mapArrowUpIcon, theme.mapArrowLeftIcon,
        theme.mapArrowRightIcon, theme.mapArrowDownLeftIcon,
        theme.mapArrowDownIcon, theme.mapArrowDownRightIcon]
  for index, name in imagesNames:
    tclEval(script = "image create photo " & name & " -file {" & imagesFiles[
        index] & "} -format {svg -scaletoheight " & $(
        gameSettings.interfaceFontSize + 8) & "}")
  tclEval(script = "ttk::theme::" & tclEval2(script = "ttk::style theme use") &
      "::LoadImages {" & theme.fileName.parentDir & "} " & $(
      gameSettings.interfaceFontSize + 8))

proc setTheme*() {.raises: [], tags: [WriteIOEffect, TimeEffect, RootEffect],
    contractual.} =
  ## Set images and buttons for the current game theme
  loadThemeImages()
  tclEval(script = gameHeader & ".fuel configure -image fuelicon")
  tclEval(script = gameHeader & ".food configure -image foodicon")
  tclEval(script = gameHeader & ".drinks configure -image drinksicon")
  tclEval(script = gameHeader & ".overloaded configure -image overloadedicon")
  tclEval(script = gameHeader & ".pilot configure -image piloticon")
  tclEval(script = gameHeader & ".engineer configure -image engineericon")
  tclEval(script = gameHeader & ".gunner configure -image gunnericon")
  tclEval(script = gameHeader & ".talk configure -image crewtradericon")
  tclEval(script = gameHeader & ".repairs configure -image repairicon")
  tclEval(script = gameHeader & ".upgrade configure -image upgradeicon")
  tclEval(script = gameHeader & ".clean configure -image cleanicon")
  tclEval(script = gameHeader & ".crafting configure -image crafticon")
  tclEval(script = mainPaned & ".mapframe.buttons.show configure -image movemapupicon")
  tclEval(script = mainPaned & ".mapframe.buttons.hide configure -image movemapdownicon")
  tclEval(script = mainPaned & ".mapframe.buttons.left configure -image movemaplefticon")
  tclEval(script = mainPaned & ".mapframe.buttons.right configure -image movemaprighticon")
  tclEval(script = mainPaned & ".mapframe.buttons.n configure -image maparrowupicon")
  tclEval(script = mainPaned & ".controls.buttons.n configure -image arrowupicon")
  tclEval(script = mainPaned & ".mapframe.buttons.s configure -image maparrowdownicon")
  tclEval(script = mainPaned & ".controls.buttons.s configure -image arrowdownicon")
  tclEval(script = mainPaned & ".mapframe.buttons.w configure -image maparrowlefticon")
  tclEval(script = mainPaned & ".controls.buttons.w configure -image arrowlefticon")
  tclEval(script = mainPaned & ".mapframe.buttons.e configure -image maparrowrighticon")
  tclEval(script = mainPaned & ".controls.buttons.e configure -image arrowrighticon")
  tclEval(script = mainPaned & ".mapframe.buttons.nw configure -image maparrowuplefticon")
  tclEval(script = mainPaned & ".controls.buttons.nw configure -image arrowuplefticon")
  tclEval(script = mainPaned & ".mapframe.buttons.ne configure -image maparrowuprighticon")
  tclEval(script = mainPaned & ".controls.buttons.ne configure -image arrowuprighticon")
  tclEval(script = mainPaned & ".mapframe.buttons.se configure -image maparrowdownrighticon")
  tclEval(script = mainPaned & ".controls.buttons.se configure -image arrowdownrighticon")
  tclEval(script = mainPaned & ".mapframe.buttons.sw configure -image maparrowdownlefticon")
  tclEval(script = mainPaned & ".controls.buttons.sw configure -image arrowdownlefticon")
  tclEval(script = mainPaned & ".controls.buttons.wait configure -image waiticon")
  tclEval(script = mainPaned & ".controls.buttons.box.moveto configure -image movetoicon")
  tclEval(script = mainPaned & ".mapframe.buttons.wait configure -image mapmenuicon")
  tclEval(script = gameHeader & ".menubutton configure -image menuicon")
  tclEval(script = gameHeader & ".closebutton configure -image exiticon")
  tclEval(script = gameHeader & ".morebutton configure -image moreoptionsicon")
