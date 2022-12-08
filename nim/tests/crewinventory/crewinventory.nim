discard """
  exitcode: 0
"""

import ../../src/[crewinventory, ships, types]

var member = MemberData(homeBase: 1)
const attribute = MobAttributeRecord(level: 1, experience: 0)
member.attributes = @[attribute, attribute, attribute, attribute]
member.inventory.add(InventoryData(amount: 1, protoIndex: 1))
playerShip.crew.add(member)

discard freeInventory(0, 0)
