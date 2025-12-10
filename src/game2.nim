# Copyright 2023-2025 Bartek thindil Jasicki
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

## Provides code related to update the state of the game, like next turn,
## finishing the game or load the game's data. Split from game module to
## avoid circular dependencies.

import std/[logging, os, paths, strutils, tables, xmlparser, xmltree]
import contracts
import bases, basescargo, basesship, basestypes, careers, config, crafts, crew,
    events, factions, game, gamesaveload, goals, help, items, log, maps,
    messages, missions, mobs, reputation, shipmodules, ships, shipscrew,
        shipsrepairs,
    shipsupgrade, statistics, stories, types, utils

proc updateGame*(minutes: Positive; inCombat: bool = false) {.raises: [KeyError,
    IOError, Exception], tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Update the game (player ship, bases, crafting, etc)
  ##
  ## * minutes  - the amount of in-game minutes which passes
  ## * inCombat - if true, the player is in combat
  var needCleaning, needSaveGame: bool = false

  proc updateDay() {.raises: [CrewOrderError, KeyError,
      CrewNoSpaceError, Exception], tags: [RootEffect], contractual.} =
    ## Update the in-game day, check if the player's ship need cleaning, pay
    ## for docks and to the crew member and check if the game has to be saved
    gameDate.day.inc
    for module in playerShip.modules.mitems:
      if module.mType == ModuleType2.cabin and module.cleanliness > 0:
        module.cleanliness.dec
        needCleaning = true
    if needCleaning:
      updateOrders(ship = playerShip)
    if playerShip.speed == docked:
      payForDock()
    dailyPayment()
    if $gameSettings.autoSave == $daily:
      needSaveGame = true

  var tiredPoints: Natural = 0
  for i in 1..minutes:
    if (gameDate.minutes + i) mod 15 == 0:
      tiredPoints.inc
  let addedMinutes: Natural = minutes mod 60
  gameDate.minutes += addedMinutes
  if gameDate.minutes > 59:
    gameDate.minutes -= 60
    gameDate.hour.inc
  var addedHours: Natural = (minutes / 60).int
  while addedHours > 23:
    addedHours -= 24
    updateDay()
  gameDate.hour += addedHours
  while gameDate.hour > 23:
    gameDate.hour -= 24
    updateDay()
  if needSaveGame:
    saveGame()
  if gameDate.month > 12:
    gameDate.month = 1
    gameDate.year.inc
    if $gameSettings.autoSave == $yearly:
      saveGame()
  updateCrew(minutes = minutes, tiredPoints = tiredPoints, inCombat = inCombat)
  repairShip(minutes = minutes)
  manufacturing(minutes = minutes)
  upgradeShip(minutes = minutes)
  let baseIndex: ExtendedBasesRange = skyMap[playerShip.skyX][
      playerShip.skyY].baseIndex
  if baseIndex > 0:
    if skyBases[baseIndex].visited.year == 0:
      gameStats.basesVisited.inc
      gameStats.points.inc
      updateGoal(goalType = visit, targetIndex = skyBases[baseIndex].owner)
    skyBases[baseIndex].visited = gameDate
    if not skyBases[baseIndex].known:
      skyBases[baseIndex].known = true
      addMessage(message = "You discovered base " & skyBases[baseIndex].name &
          ".", mType = otherMessage)
    updatePopulation()
    generateRecruits()
    generateMissions()
    generateCargo()
    updatePrices()
    updateOrders(ship = playerShip)
  if not skyMap[playerShip.skyX][playerShip.skyY].visited:
    gameStats.mapVisited.inc
    gameStats.points.inc
    updateGoal(goalType = discover, targetIndex = "")
    skyMap[playerShip.skyX][playerShip.skyY].visited = true
  updateEvents(minutes = minutes)
  updateMissions(minutes = minutes)

proc loadGameData*(): string {.raises: [DataLoadingError, KeyError,
    OSError], tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Load the game's data from files
  ##
  ## Returns empty string if the data loaded properly, otherwise message with
  ## information what was wrong.
  result = ""
  if protoShipsList.len > 0:
    return

  {.hint[XCannotRaiseY]: off.}
  proc loadSelectedData(dataName: string; fileName: Path): string {.raises: [
      DataLoadingError, KeyError, OSError], tags: [WriteIOEffect, RootEffect],
      contractual.} =
    ## Load the selected game's data from the file
    ##
    ## * dataName - the name of the data to load
    ## * fileName - the path to the file from which the data will be loaded
    ##
    ## Returns an empty string if the data was loaded correctly, otherwise the
    ## message with information what was wrong.
    require:
      dataName.len > 0
      ($fileName).len > 0
    body:
      var localFileName: Path = "".Path
      proc loadDataFile(localDataName: string): string {.raises: [
          DataLoadingError, KeyError], tags: [WriteIOEffect, RootEffect],
              contractual.} =
        ## Load the data from the selected file
        ##
        ## * localDataName - the name of the data which will be loaded from the
        ##                   file
        ##
        ## Returns an empty string if the data was loaded correctly, otherwise
        ## the message with information what was wrong.
        require:
          localDataName.len > 0
        body:
          let dataXml: XmlNode = try:
              loadXml(path = $localFileName)
            except XmlError, ValueError, IOError, OSError, Exception:
              return getCurrentExceptionMsg()
          var dataType: string = ""
          dataType = dataXml.tag
          if dataType == localDataName or localDataName.len == 0:
            logMessage(message = "Loading " & dataType & " file: " &
                $localFileName, messageLevel = lvlInfo)
            case dataType
            of "factions":
              loadFactions(fileName = localFileName)
            of "goals":
              loadGoals(fileName = localFileName)
            of "help":
              loadHelp(fileName = localFileName)
            of "items":
              loadItems(fileName = localFileName)
            of "mobiles":
              loadMobs(fileName = localFileName)
            of "recipes":
              loadRecipes(fileName = localFileName)
            of "bases":
              loadBasesTypes(fileName = localFileName)
            of "modules":
              loadModules(fileName = localFileName)
            of "ships":
              loadShips(fileName = $localFileName)
            of "stories":
              loadStories(fileName = $localFileName)
            of "data":
              loadData(fileName = $localFileName)
            of "careers":
              loadCareers(fileName = $localFileName)
            else:
              return "Can't load the game data. Unknown type of data: " & dataType

      if ($fileName).len == 0:
        for file in walkFiles(pattern = dataName & DirSep & "*.dat"):
          localFileName = file.Path
          result = loadDataFile(localDataName = "")
          if result.len > 0:
            return
      else:
        localFileName = (dataDirectory & $fileName).Path
        result = loadDataFile(localDataName = dataName)
  {.hint[XCannotRaiseY]: on.}

  type
    DataName = string
    DataTypeRecord = object
      name: DataName
      fileName: Path

  proc newDataTypeRecord(name, fileName: string): DataTypeRecord {.raises: [], tags: [],
      contractual.} =
    ## Create a new data structure for the game data to read from a file
    ##
    ## * name     - the name of the data
    ## * fileName - the name of the file from which the data will be read
    ##
    ## Returns the new structure with information about the selected data
    return DataTypeRecord(name: name, fileName: fileName.Path)

  const dataTypes: array[1..12, DataTypeRecord] = [newDataTypeRecord(name = "data",
      fileName = "game.dat"), newDataTypeRecord(name = "items",
      fileName = "items.dat"), newDataTypeRecord(name = "modules",
      fileName = "shipmodules.dat"), newDataTypeRecord(name = "recipes",
      fileName = "recipes.dat"), newDataTypeRecord(name = "bases",
      fileName = "bases.dat"), newDataTypeRecord(name = "mobiles",
      fileName = "mobs.dat"), newDataTypeRecord(name = "careers",
      fileName = "careers.dat"), newDataTypeRecord(name = "factions",
      fileName = "factions.dat"), newDataTypeRecord(name = "help",
      fileName = "help.dat"), newDataTypeRecord(name = "ships",
      fileName = "ships.dat"), newDataTypeRecord(name = "goals",
      fileName = "goals.dat"), newDataTypeRecord(name = "stories",
      fileName = "stories.dat")]
  # Load the standard game data
  for dataType in dataTypes:
    result = loadSelectedData(dataName = dataType.name,
        fileName = dataType.fileName)
    if result.len > 0:
      return
  # Load the modifications
  for modDirectory in walkDirs(pattern = modsDirectory & "*"):
    result = loadSelectedData(dataName = modDirectory, fileName = "".Path)
    if result.len > 0:
      return
  setToolsList()

proc endGame*(save: bool) {.raises: [KeyError, IOError, OSError],
    tags: [WriteIOEffect, RootEffect], contractual.} =
  ## Save or not the game and clear the temporary data
  ##
  ## * save - if true, save the current game
  if save:
    saveGame()
  else:
    removeFile(file = saveName)
  saveConfig()
  clearGameStats()
  clearCurrentGoal()
  messagesList = @[]
  knownRecipes = @[]
  eventsList = @[]
  acceptedMissions = @[]

proc createPlayerShip(randomBase: Positive;
    playerFaction: FactionData) {.raises: [ValueError], tags: [],
    contractual.} =
  ## Create the player's ship and add the player's character to it
  ##
  ## * randomBase    - the index of the base to which the player's ship will
  ##                   be docked
  ## * playerFaction - the faction to which the player's character belongs
  require:
    randomBase < skyBases.len
  ensure:
    playerShip.crew.len > 0
  body:
    playerShip = createShip(protoIndex = playerFaction.careers[
        newGameSettings.playerCareer].shipIndex,
        name = newGameSettings.shipName,
        x = skyBases[randomBase].skyX, y = skyBases[randomBase].skyY,
        speed = docked, randomUpgrades = false)
    # Add the player to the ship
    let
      playerIndex2: Natural = playerFaction.careers[
          newGameSettings.playerCareer].playerIndex.parseInt
      protoPlayer: ProtoMobRecord = protoMobsList[playerIndex2]
      playerMorale: Natural = (if "nomorale" in
          playerFaction.flags: 50 else: 100)
    var tmpInventory: seq[InventoryData] = @[]
    for item in protoPlayer.inventory:
      let amount: Positive = (if item.maxAmount > 0: getRandom(
          min = item.minAmount, max = item.maxAmount) else: item.minAmount)
      tmpInventory.add(y = InventoryData(protoIndex: item.protoIndex,
          amount: amount, name: "", durability: 100, price: 0))
    {.warning[UnsafeSetLen]: off.}
    playerShip.crew.insert(item = MemberData(name: newGameSettings.playerName,
        gender: newGameSettings.playerGender, health: 100, tired: 0,
        skills: protoPlayer.skills, hunger: 0, thirst: 0,
        order: protoPlayer.order, previousOrder: rest, orderTime: 15,
        orders: protoPlayer.priorities, attributes: protoPlayer.attributes,
        inventory: tmpInventory, equipment: protoPlayer.equipment, payment: [0,
        0], contractLength: -1, morale: [1: playerMorale, 2: 0], loyalty: 100,
        homeBase: randomBase, faction: newGameSettings.playerFaction), i = 0)
    {.warning[UnsafeSetLen]: on.}
    var cabinAssigned: bool = false
    for module in playerShip.modules.mitems:
      for owner in module.owner.mitems:
        if owner > -1:
          owner.inc
      if modulesList[module.protoIndex].mType == ModuleType.cabin and
          not cabinAssigned:
        for index, owner in module.owner.mpairs:
          if owner == -1:
            owner = 0
            if index == 0:
              module.name = newGameSettings.playerName & "'s Cabin"
            cabinAssigned = true
            break

proc setGameStats(): FactionData {.raises: [KeyError], tags: [],
    contractual.} =
  ## Set the game statistics and the player's faction, needed for starting
  ## a new game
  ##
  ## Returns the player's faction's data
  body:
    clearGameStats()
    if newGameSettings.playerFaction == "random":
      newGameSettings.playerCareer = "random"
      var index: Positive = 1
      let roll: Positive = getRandom(min = 1, max = factionsList.len)
      for faction in factionsList.keys:
        if index == roll:
          newGameSettings.playerFaction = faction
          break
        index.inc
    result = factionsList[newGameSettings.playerFaction]
    if newGameSettings.playerCareer == "random":
      let roll: Positive = getRandom(min = 1, max = result.careers.len)
      var index: Positive = 1
      for career in result.careers.keys:
        if index == roll:
          newGameSettings.playerCareer = career
          break
        index.inc

proc setBases(maxSpawnRoll: Natural; basesArray: var Table[string, seq[
    Positive]]) {.raises: [KeyError], tags: [], contractual.} =
  ## Set bases in the new game
  ##
  ## * maxSpawnRoll - the max chance for any base to spawn
  ## * basesArray   - the temporary list of bases
  ##
  ## Returns the modified parameter basesArray
  var
    baseOwner, baseType: string = ""
    basePopulation: Natural = 0
    baseReputation: ReputationRange = 0
    baseSize: BasesSize = unknown
  for index, skyBase in skyBases.mpairs:
    var factionRoll: Natural = getRandom(min = 1, max = maxSpawnRoll)
    for index, faction in factionsList:
      if factionRoll < faction.spawnChance:
        baseOwner = index
        basePopulation = (if faction.population[2] == 0: faction.population[
            1] else: getRandom(min = faction.population[1],
                max = faction.population[2]))
        baseReputation = getReputation(
            sourceFaction = newGameSettings.playerFaction,
            targetFaction = index)
        var maxBaseSpawnRoll: Natural = 0
        for spawnChance in faction.basesTypes.values:
          maxBaseSpawnRoll += spawnChance
        var baseTypeRoll: Positive = getRandom(min = 1,
            max = maxBaseSpawnRoll)
        for tindex, baseTypeChance in faction.basesTypes:
          if baseTypeRoll <= baseTypeChance:
            baseType = tindex
            break
          baseTypeRoll -= baseTypeChance
        break
      factionRoll -= faction.spawnChance
    if baseOwner.len == 0:
      baseOwner = newGameSettings.playerFaction
    baseSize = case basePopulation
      of 0:
        getRandom(min = 0, max = 2).BasesSize
      of 1..149:
        small
      of 150..299:
        medium
      else: big
    skyBase.name = generateBaseName(factionIndex = baseOwner)
    skyBase.visited = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
    skyBase.skyX = 1
    skyBase.skyY = 1
    skyBase.baseType = baseType
    skyBase.population = basePopulation
    skyBase.recruitDate = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
    skyBase.known = false
    skyBase.askedForBases = false
    skyBase.askedForEvents = DateRecord(year: 0, month: 0, day: 0,
        hour: 0, minutes: 0)
    skyBase.reputation = ReputationData(level: baseReputation, experience: 0)
    skyBase.missionsDate = DateRecord(year: 0, month: 0, day: 0, hour: 0, minutes: 0)
    skyBase.missions = @[]
    skyBase.owner = baseOwner
    skyBase.size = baseSize
    skyBase.recruits = @[]
    let baseFaction: FactionData = factionsList[baseOwner]
    if "loner" in baseFaction.flags:
      factionRoll = getRandom(min = 1, max = maxSpawnRoll)
      for index, faction in factionsList:
        if factionRoll > faction.spawnChance:
          factionRoll -= faction.spawnChance
        else:
          baseOwner = index
    basesArray[baseOwner].add(y = index)

proc newGame*() {.raises: [OSError, KeyError, IOError, ValueError,
    Exception], tags: [WriteIOEffect, ReadIOEffect], contractual.} =
  ## Start a new game, save configuration, create bases, fill the map, create
  ## the player's ship and put it on the map
  # Save the game configuration
  ensure:
    playerShip.crew.len > 0
  body:
    saveConfig()
    getPlayerShips()
    # Set the game statistics
    let playerFaction: FactionData = setGameStats()
    # Set the game time
    gameDate = startDate
    # Generate the game's world
    for x in MapXRange.low..MapXRange.high:
      for y in MapYRange.low..MapYRange.high:
        skyMap[x][y] = SkyCell(baseIndex: 0, visited: false, eventIndex: -1,
            missionIndex: -1)
    var
      maxSpawnRoll: Natural = 0
      basesArray: Table[string, seq[Positive]] = initTable[string, seq[
          Positive]]()
    for index, faction in factionsList:
      maxSpawnRoll += faction.spawnChance
      basesArray[index] = @[]
    setBases(maxSpawnRoll = maxSpawnRoll, basesArray = basesArray)
    for factionBases in basesArray.values:
      for index, faction in factionBases:
        var
          attempts: Positive = 1
          posX, posY: int = 0
        while true:
          var validLocation: bool = true
          if index == factionBases.low or ("loner" in factionsList[skyBases[
              factionBases[0]].owner].flags and "loner" in factionsList[skyBases[
              faction].owner].flags):
            posX = getRandom(min = BasesRange.low + 10, max = BasesRange.high - 10)
            posY = getRandom(min = BasesRange.low + 10, max = BasesRange.high - 10)
          else:
            posX = getRandom(min = skyBases[factionBases[index - 1]].skyX - 20,
                max = skyBases[factionBases[index - 1]].skyX + 20)
            normalizeCoord(coord = posX)
            posY = getRandom(min = skyBases[factionBases[index - 1]].skyY - 20,
                max = skyBases[factionBases[index - 1]].skyY + 20)
            normalizeCoord(coord = posY, isXAxis = false)
            if posX < BasesRange.low + 10:
              posX = BasesRange.low + 10
            elif posX > BasesRange.high - 10:
              posX = BasesRange.high - 10
            if posY < BasesRange.low + 10:
              posY = BasesRange.low + 10
            elif posY > BasesRange.high - 10:
              posY = BasesRange.high - 10
            attempts.inc
            if attempts > 250:
              posX = getRandom(min = BasesRange.low + 10,
                  max = BasesRange.high - 10)
              posY = getRandom(min = BasesRange.low + 10,
                  max = BasesRange.high - 10)
              attempts = 1
          for j in -5..5:
            var tempX: int = posX + j
            normalizeCoord(coord = tempX)
            for k in -5..5:
              var tempY: int = posY + k
              normalizeCoord(coord = tempY, isXAxis = false)
              if skyMap[tempX][tempY].baseIndex > 0:
                validLocation = false
                break
            if not validLocation:
              break
          if skyMap[posX][posY].baseIndex > 0:
            validLocation = false
          if validLocation:
            break
        skyMap[posX][posY] = SkyCell(baseIndex: faction, visited: false,
            eventIndex: -1, missionIndex: -1)
        skyBases[faction].skyX = posX
        skyBases[faction].skyY = posY
    # Place the player's ship in a random large base
    var randomBase, attempts: Positive = 1
    while true:
      randomBase = getRandom(min = 1, max = 1024)
      if (attempts < 250 and getBasePopulation(baseIndex = randomBase) ==
          large and skyBases[randomBase].owner ==
          newGameSettings.playerFaction) or attempts > 249:
        if newGameSettings.startingBase == "Any":
          break
        elif skyBases[randomBase].baseType == newGameSettings.startingBase:
          break
      attempts.inc
    # Create the player's ship
    createPlayerShip(randomBase = randomBase, playerFaction = playerFaction)
    # Set current map field and sky base info
    skyBases[randomBase].visited = gameDate
    skyBases[randomBase].known = true
    skyMap[playerShip.skyX][playerShip.skyY].visited = true
    generateRecruits()
    generateMissions()
    generateCargo()
    # Set the player's goal if not set yet
    if currentGoal.goalType == random:
      var goalIndex: Positive = getRandom(min = 1, max = goalsList.len)
      while not goalsList.hasKey(key = goalIndex):
        goalIndex = getRandom(min = 1, max = goalsList.len)
      currentGoal = goalsList[goalIndex]
    # Set the name of the savegame file
    generateSaveName()
    # Set the player's career
    playerCareer = newGameSettings.playerCareer
    # Set the player's reputations
    resetReputations()
    # Add the welcoming message
    addMessage(message = "Welcome to Steam Sky. If it is your first game, please consider read help (entry 'Help' in Menu), especially topic 'First Steps'.",
        mType = otherMessage)
