# Copyright 2023 Bartek thindil Jasicki
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

import std/tables
import ../[config, game, messages, shipscargo, shipsmovement, tk, types]
import coreui, utilsui2

proc updateHeader*() =
  var label = gameHeader & ".time"
  tclEval(script = label & " configure -text {" & formattedTime() & "}")
  if gameSettings.showNumbers:
    tclEval(script = label & " configure -text {" & formattedTime() &
        " Speed: " & $((realSpeed(ship = playerShip) * 60) / 1_000) & " km/h}")
    tclEval(script = "tooltip::tooltip " & label & " \"Game time and current ship speed.\"")
  label = gameHeader & ".nofuel"
  tclEval(script = "grid remove " & label)
  var itemAmount = getItemAmount(itemType = fuelType)
  if itemAmount == 0:
    tclEval(script = label & " configure -image nofuelicon")
    tclEval(script = "tooltip::tooltip " & label & " \"You can't travel anymore, because you don't have any fuel for ship.\"")
    tclEval(script = "grid " & label)
  elif itemAmount <= gameSettings.lowFuel:
    tclEval(script = label & " configure -image lowfuelicon")
    tclEval(script = "tooltip::tooltip " & label &
        " \"Low level of fuel on ship. Only " & $itemAmount & " left.\"")
    tclEval(script = "grid " & label)
  label = gameHeader & ".nodrink"
  tclEval(script = "grid remove " & label)
  itemAmount = getItemsAmount(iType = "Drinks")
  if itemAmount == 0:
    tclEval(script = label & " configure -image nodrinksicon")
    tclEval(script = "tooltip::tooltip " & label & " \"You don't have any drinks in ship but your crew needs them to live.\"")
    tclEval(script = "grid " & label)
  elif itemAmount <= gameSettings.lowDrinks:
    tclEval(script = label & " configure -image lowdrinksicon")
    tclEval(script = "tooltip::tooltip " & label &
        " \"Low level of drinks on ship. Only " & $itemAmount & " left.\"")
    tclEval(script = "grid " & label)
  itemAmount = getItemsAmount(iType = "Food")
  if itemAmount == 0:
    tclEval(script = label & " configure -image nofoodicon")
    tclEval(script = "tooltip::tooltip " & label & " \"You don't have any food in ship but your crew needs them to live.\"")
    tclEval(script = "grid " & label)
  elif itemAmount <= gameSettings.lowFood:
    tclEval(script = label & " configure -image lowfoodicon")
    tclEval(script = "tooltip::tooltip " & label &
        " \"Low level of food on ship. Only " & $itemAmount & " left.\"")
    tclEval(script = "grid " & label)
  var havePilot, haveEngineer, haveTrader, haveUpgrader, haveCleaner,
    haveRepairman = false
  for member in playerShip.crew:
    case member.order
    of pilot:
      havePilot = true
    of engineer:
      haveEngineer = true
    of talk:
      haveTrader = true
    of upgrading:
      haveUpgrader = true
    of clean:
      haveCleaner = true
    of repair:
      haveRepairman = true
    else:
      discard
  label = gameHeader & ".overloaded"
  tclEval(script = "grid remove " & label)
  let
    faction = factionsList[playerShip.crew[0].faction]
    frame = mainPaned & ".combat"
  if havePilot and (haveEngineer or "sentientships" in faction.flags) and (
      tclEval2(script = "winfo exists " & frame) == "0" or tclEval2(
      script = "winfo ismapped " & frame) == "0"):
    let speed = (if playerShip.speed != docked: realSpeed(
        ship = playerShip).float / 1_000.0 else: realSpeed(ship = playerShip,
        infoOnly = true).float / 1_000)
    if speed < 0.5:
      tclEval(script = "tooltip::tooltip " & label & " \"You can't fly with your ship, because it is overloaded.\"")
      tclEval(script = "grid " & label)
  var
    haveGunner, haveWorker = true
    needWorker, needCleaning, needRepairs = false
  for module in playerShip.modules:
    case modulesList[module.protoIndex].mType
    of gun, harpoonGun:
      if module.owner[0] == -1:
        haveGunner = false
      elif playerShip.crew[module.owner[0]].order != gunner:
        haveGunner = false
    of alchemyLab .. greenhouse:
      if module.craftingIndex.len > 0:
        needWorker = true
        for owner in module.owner:
          if owner == -1:
            haveWorker = false
          elif playerShip.crew[owner].order != craft:
            haveWorker = false
          if not haveWorker:
            break
    of cabin:
      if module.cleanliness != module.quality:
        needCleaning = true
    else:
      discard
    if module.durability != module.maxDurability:
      needRepairs = true
  label = gameHeader & ".pilot"

proc showSkyMap*(clear: bool = false) =
  tclSetVar(varName = "refreshmap", newValue = "1")
  if clear:
    showScreen(newScreenName = "mapframe")
  tclSetVar(varName = "gamestate", newValue = "general")
