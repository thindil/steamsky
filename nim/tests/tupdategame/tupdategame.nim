discard """
  exitcode: 0
"""

import ../../src/[game, types, updategame]

gameDate = DateRecord(year: 1600, month: 1, day: 1, hour: 8, minutes: 0)
playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: ModuleType2.engine, protoIndex: 3,
    durability: 100, fuelUsage: 4, power: 2000, disabled: false))
playerShip.cargo = @[]
playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100, durability: 100))

updateGame(1)
gameDate.minutes = 1
