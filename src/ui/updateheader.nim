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

import std/tables
import ../[config, game, maps, messages, shipscargo, shipsmovement, tk, types]
import coreui, dialogs, errordialog

proc updateHeader*() {.raises: [], tags: [WriteIOEffect, TimeEffect].} =
  ## Update in-game header with information about time, state of the crew
  ## members, etc.
  var label = gameHeader & ".time"
  tclEval(script = label & " configure -text {" & formattedTime() & "}")
  if gameSettings.showNumbers:
    try:
      discard tclEval(script = label & " configure -text {" & formattedTime() &
          " Speed: " & $((realSpeed(ship = playerShip) * 60) / 1_000) & " km/h}")
    except ValueError:
      showError(message = "Can't show the speed of the ship.")
      return
    tclEval(script = "tooltip::tooltip " & label & " \"Game time and current ship speed.\"")
  label = gameHeader & ".fuel"
  var itemAmount = try:
        getItemAmount(itemType = fuelType)
      except KeyError:
        showError(message = "Can't get items amount.")
        return
  if itemAmount == 0:
    tclEval(script = label & " configure -image nofuelicon -text {" &
        $itemAmount & "} -style Headerred.TLabel")
    tclEval(script = "tooltip::tooltip " & label & " \"You can't travel anymore, because you don't have any fuel for ship.\"")
  elif itemAmount <= gameSettings.lowFuel:
    tclEval(script = label & " configure -image lowfuelicon -text {" &
        $itemAmount & "} -style Golden.TLabel")
    tclEval(script = "tooltip::tooltip " & label &
        " \"Low level of fuel on ship. Only " & $itemAmount & " left.\"")
  else:
    tclEval(script = label & " configure -image fuelicon -text {" &
        $itemAmount & "} -style Headergreen.TLabel")
    tclEval(script = "tooltip::tooltip " & label & " \"The amount of fuel in the ship's cargo.\"")
  label = gameHeader & ".drinks"
  itemAmount = try:
      getItemsAmount(iType = "Drinks")
    except KeyError:
      showError("Can't get items amount (2).")
      return
  if itemAmount == 0:
    tclEval(script = label & " configure -image nodrinksicon -text {" &
        $itemAmount & "} -style Headerred.TLabel")
    tclEval(script = "tooltip::tooltip " & label & " \"You don't have any drinks in ship but your crew needs them to live.\"")
  elif itemAmount <= gameSettings.lowDrinks:
    tclEval(script = label & " configure -image lowdrinksicon -text {" &
        $itemAmount & "} -style Golden.TLabel")
    tclEval(script = "tooltip::tooltip " & label &
        " \"Low level of drinks on ship. Only " & $itemAmount & " left.\"")
  else:
    tclEval(script = label & " configure -image drinksicon -text {" &
        $itemAmount & "} -style Headergreen.TLabel")
    tclEval(script = "tooltip::tooltip " & label & " \"The amount of drinks in the ship's cargo.\"")
  label = gameHeader & ".food"
  itemAmount = try:
      getItemsAmount(iType = "Food")
    except KeyError:
      showError("Can't get items amount (3).")
      return
  if itemAmount == 0:
    tclEval(script = label & " configure -image nofoodicon -text {" &
        $itemAmount & "} -style Headerred.TLabel")
    tclEval(script = "tooltip::tooltip " & label & " \"You don't have any food in ship but your crew needs them to live.\"")
  elif itemAmount <= gameSettings.lowFood:
    tclEval(script = label & " configure -image lowfoodicon -text {" &
        $itemAmount & "} -style Golden.TLabel")
    tclEval(script = "tooltip::tooltip " & label &
        " \"Low level of food on ship. Only " & $itemAmount & " left.\"")
  else:
    tclEval(script = label & " configure -image foodicon -text {" &
        $itemAmount & "} -style Headergreen.TLabel")
    tclEval(script = "tooltip::tooltip " & label & " \"The amount of food in the ship's cargo.\"")
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
    faction = try:
        factionsList[playerShip.crew[0].faction]
      except KeyError:
        showError(message = "Can't get faction.")
        return
    frame = mainPaned & ".combat"
  if havePilot and (haveEngineer or "sentientships" in faction.flags) and (
      tclEval2(script = "winfo exists " & frame) == "0" or tclEval2(
      script = "winfo ismapped " & frame) == "0"):
    let speed = try:
          (if playerShip.speed != docked: realSpeed(
              ship = playerShip).float / 1_000.0 else: realSpeed(
                  ship = playerShip,
              infoOnly = true).float / 1_000)
        except ValueError:
          showError(message = "Can't coutn speed.")
          return
    if speed < 0.5:
      tclEval(script = "tooltip::tooltip " & label & " \"You can't fly with your ship, because it is overloaded.\"")
      tclEval(script = "grid " & label)
  var
    haveGunner, haveWorker = true
    needWorker, needCleaning, needRepairs = false
  for module in playerShip.modules:
    try:
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
    except KeyError:
      showError(message = "Can't check modules.")
      return
    if module.durability != module.maxDurability:
      needRepairs = true
  label = gameHeader & ".pilot"
  if havePilot:
    tclEval(script = "grid remove " & label)
  else:
    if "sentientships" in faction.flags:
      tclEval(script = label & " configure -image nopiloticon")
      tclEval(script = "tooltip::tooltip " & label & " \"No pilot assigned. Ship fly on it own.\"")
    else:
      tclEval(script = label & " configure -image piloticon")
      tclEval(script = "tooltip::tooltip " & label & " \"No pilot assigned. Ship can't move.\"")
    tclEval(script = "grid " & label)
  label = gameHeader & ".engineer"
  if haveEngineer:
    tclEval(script = "grid remove " & label)
  else:
    if "sentientships" in faction.flags:
      tclEval(script = label & " configure -image noengineericon")
      tclEval(script = "tooltip::tooltip " & label & " \"No engineer assigned. Ship fly on it own.\"")
    else:
      tclEval(script = label & " configure -image engineericon")
      tclEval(script = "tooltip::tooltip " & label & " \"No engineer assigned. Ship can't move.\"")
    tclEval(script = "grid " & label)
  label = gameHeader & ".gunner"
  if haveGunner:
    tclEval(script = "grid remove " & label)
  else:
    tclEval(script = label & " configure -style Headerred.TLabel")
    tclEval(script = "tooltip::tooltip " & label & " \"One or more guns don't have a gunner.\"")
    tclEval(script = "grid " & label)
  label = gameHeader & ".repairs"
  if needRepairs:
    if haveRepairman:
      tclEval(script = label & " configure -image repairicon")
      tclEval(script = "tooltip::tooltip " & label & " \"The ship is being repaired.\"")
    else:
      tclEval(script = label & " configure -image norepairicon")
      tclEval(script = "tooltip::tooltip " & label & " \"The ship needs repairs but no one is working them.\"")
    tclEval(script = "grid " & label)
  else:
    tclEval(script = "grid remove " & label)
  label = gameHeader & ".crafting"
  if needWorker:
    if haveWorker:
      tclEval(script = label & " configure -image manufactureicon")
      tclEval(script = "tooltip::tooltip " & label & " \"All crafting orders are being executed.\"")
    else:
      tclEval(script = label & " configure -image nocrafticon")
      tclEval(script = "tooltip::tooltip " & label & " \"You need to assign crew members to begin manufacturing.\"")
    tclEval(script = "grid " & label)
  else:
    tclEval(script = "grid remove " & label)
  label = gameHeader & ".upgrade"
  if playerShip.upgradeModule > -1:
    if haveUpgrader:
      tclEval(script = label & " configure -image upgradeicon")
      tclEval(script = "tooltip::tooltip " & label & " \"A ship module upgrade in progress.\"")
    else:
      tclEval(script = label & " configure -image noupgradeicon")
      tclEval(script = "tooltip::tooltip " & label & " \"A ship module upgrade is in progress but no one is working on it.\"")
    tclEval(script = "grid " & label)
  else:
    tclEval(script = "grid remove " & label)
  label = gameHeader & ".talk"
  if haveTrader:
    tclEval(script = "grid remove " & label)
  elif skyMap[playerShip.skyX][playerShip.skyY].baseIndex > 0:
    tclEval(script = "tooltip::tooltip " & label & " \"No trader assigned. You need one to talk/trade.\"")
    tclEval(script = "grid " & label)
  elif skyMap[playerShip.skyX][playerShip.skyY].eventIndex > -1:
    if eventsList[skyMap[playerShip.skyX][playerShip.skyY].eventIndex].eType == friendlyShip:
      tclEval(script = "tooltip::tooltip " & label & " \"No trader assigned. You need one to talk/trade.\"")
      tclEval(script = "grid " & label)
    else:
      tclEval(script = "grid remove " & label)
  else:
    tclEval(script = "grid remove " & label)
  label = gameHeader & ".clean"
  if needCleaning:
    if haveCleaner:
      tclEval(script = label & " configure -image cleanicon")
      tclEval(script = "tooltip::tooltip " & label & " \"Ship is cleaned.\"")
    else:
      tclEval(script = label & " configure -image nocleanicon")
      tclEval(script = "tooltip::tooltip " & label & " \"Ship is dirty but no one is cleaning it.\"")
    tclEval(script = "grid " & label)
  else:
    tclEval(script = "grid remove " & label)
  if playerShip.crew[0].health == 0:
    showQuestion(question = "You are dead. Would you like to see your game statistics?",
        res = "showstats")

