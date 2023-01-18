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
import types

type
  EventsTypes* = enum
    ## Possible types of events on map
    none, enemyShip, attackOnBase, disease, doublePrice, baseRecovery,
        fullDocks, enemyPatrol, trader, friendlyShip

  EventData* = object
    ## Used to store data about an event
    skyX*: MapXRange         ## The X coordinate of the event on the map
    skyY*: MapYRange         ## The Y coordinate of the event on the map
    time*: Positive          ## The time in minutes by how long the event will be available
    case eType*: EventsTypes ## The type of the event
    of doublePrice:
      itemIndex: int         ## The index of the prototype item used by the event
    of attackOnBase, enemyShip, enemyPatrol, trader, friendlyShip:
      shipIndex: int         ## The index of the prototype ship used by the event
    else:
      data: int              ## General data of the event

var eventsList* = initTable[Positive, EventData]() ## The list of available events in the game


# Temporary code for interfacing with Ada

proc getAdaEvent(index, x, y, time, eType, data: cint) {.exportc.} =
  var event = EventData(skyX: x, skyY: y, time: time, eType: eType.EventsTypes)
  case event.eType
    of doublePrice:
      event.itemIndex = data
    of attackOnBase, enemyShip, enemyPatrol, trader, friendlyShip:
      event.shipIndex = data
    else:
      event.data = data
  eventsList[index] = event

