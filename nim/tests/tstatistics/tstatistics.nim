discard """
  exitcode: 0
"""

import ../../src/statistics

gameStats.craftingOrders = @[]
updateCraftingOrders("1")
assert gameStats.craftingOrders.len == 1
