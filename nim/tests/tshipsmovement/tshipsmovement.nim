discard """
  exitcode: 0
"""

import ../../src/[game, shipsmovement, types]

playerShip.modules = @[]
playerShip.modules.add(ModuleData(mType: ModuleType2.engine, protoIndex: 3,
    durability: 100, fuelUsage: 4, power: 2000, disabled: false))
playerShip.cargo = @[]
playerShip.cargo.add(InventoryData(protoIndex: 1, amount: 100, durability: 100))

waitInPlace(1)
