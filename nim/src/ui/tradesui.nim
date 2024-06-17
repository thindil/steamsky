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

import std/os
import ../[game, events, maps, tk, types]
import coreui, mapsui, table

type ItemsSortOrders = enum
  nameAsc, nameDesc, typeAsc, typeDesc, durabilityAsc, durabilityDesc, priceAsc,
    priceDesc, profitAsc, profitDesc, weightAsc, weightDesc, ownedAsc,
    ownedDesc, availableAsc, availableDesc, none

const defaultItemsSortOrder: ItemsSortOrders = none

var
  tradeTable: TableWidget
  itemsSortOrder: ItemsSortOrders = defaultItemsSortOrder
  itemsIndexes: seq[int]

proc showTradeCommand(clientData: cint; interp: PInterp; argc: cint;
    argv: cstringArray): TclResults {.exportc.} =
  var tradeFrame = mainPaned & ".tradeframe"
  let
    tradeCanvas = tradeFrame & ".canvas"
    baseIndex = skyMap[playerShip.skyX][playerShip.skyY].baseIndex
    eventIndex = skyMap[playerShip.skyX][playerShip.skyY].eventIndex
  var label = tradeCanvas & ".trade.options.typelabel"
  if tclEval2(script = "winfo exists " & label) == "0":
    tclEvalFile(fileName = dataDirectory & "ui" & DirSep & "trade.tcl")
    tclEval(script = "bind " & tradeFrame & " <Configure> {ResizeCanvas %W.canvas %w %h}")
    tradeFrame = tradeCanvas & ".trade"
    tradeTable = createTable(parent = tradeFrame, headers = @["Name", "Type",
        "Durability", "Price", "Profit", "Weight", "Owned", "Available"],
        scrollbar = mainPaned & ".tradeframe.scrolly",
        command = "SortTradeItems",
        tooltipTExt = "Press mouse button to sort the items.")
  elif tclEval2(script = "winfo ismapped " & label) == "1" and argc == 1:
    itemsSortOrder = defaultItemsSortOrder
    tclEval(script = "grid remove " & closeButton)
    tclEval(script = closeButton & " configure -command ShowSkyMap")
    if baseIndex == 0 and eventIndex > -1:
      deleteEvent(eventIndex = eventIndex)
    showSkyMap(clear = true)
    return tclOk
  tclSetVar(varName = "gamestate", newValue = "trade")
  let searchEntry = tradeCanvas & ".trade.options.search"
  if argc < 3:
    tclEval(script = searchEntry & " delete 0 end")
  tclEval(script = closeButton & " configure -command {ShowSkyMap ShowTrade}")
  tradeFrame = tradeCanvas & ".trade"
  clearTable(table = tradeTable)
  var
    baseType: string
    baseCargo: seq[BaseCargo]
  if baseIndex > 0:
    baseType = skyBases[baseIndex].baseType
    baseCargo = skyBases[baseIndex].cargo
  else:
    baseType = "0"
    baseCargo = traderCargo
  if itemsSortOrder == defaultItemsSortOrder:
    itemsIndexes = @[]
    for index in playerShip.cargo.low .. playerShip.cargo.high:
      itemsIndexes.add(y = index)
    itemsIndexes.add(y = -1)
    for index in baseCargo.low .. baseCargo.high:
      itemsIndexes.add(y = index)
  for i in itemsIndexes:
    if i == -1:
      break
  return tclOk

proc addCommands*() {.sideEffect, raises: [], tags: [].} =
  ## Adds Tcl commands related to the trades UI
  try:
    discard
#    addCommand("ShowTrade", showTradeCommand)
  except:
    showError(message = "Can't add a Tcl command.")
