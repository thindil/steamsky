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

import std/unicode

type themeRecord* = object
  ## Data structure for themes settings
  ##
  ## * Name                  - Name of theme
  ## * File_Name             - Name of .tcl file of theme
  ## * Enemy_Ship_Icon       - Icon used for Enemy Ship event
  ## * Attack_On_Base_Icon   - Icon used for Attack on Base event
  ## * Disease_Icon          - Icon used for Disease event
  ## * Double_PriceIcon      - Icon used for Double Price event
  ## * Full_Docks_Icon       - Icon used for Full Docks event
  ## * Enemy_Patrol_Icon     - Icon used for Enemy Patrol event
  ## * Trader_Icon           - Icon used for Trader event
  ## * Friendly_Ship_Icon    - Icon used for Friendly Ship event
  ## * Deliver_Icon          - Icon used for Deliver Item mission
  ## * Destroy_Icon          - Icon used for Destroy Ship mission
  ## * Patrol_Icon           - Icon used for Patrol Area mission
  ## * Explore_Icon          - Icon used for Explore Area mission
  ## * Passenger_Icon        - Icon used for Transport Passenger mission
  ## * Pilot_Icon            - Icon used for Pilot info
  ## * Engineer_Icon         - Icon used for Engineer info
  ## * Gunner_Icon           - Icon used for Gunners info
  ## * Crew_Trader_Icon      - Icon used for Trader info
  ## * Repair_Icon           - Icon used for Repairs info when repairs are on
  ## * No_Repair_Icon        - Icon used for Repairs info when noone is assigned
  ## * Repair_Order_Icon     - Icon used for giving repair order to all crew members
  ## * Upgrade_Icon          - Icon used for Upgrade info when upgrade is on
  ## * No_Upgrade_Icon       - Icon used for Upgrade info when noone is assigned
  ## * Clean_Icon            - Icon used for Clean Ship info when cleaning is on
  ## * No_Clean_Icon         - Icon used for Clean Ship info when noone is assigned
  ## * Clean_Order_Icon      - Icon used for giving clean ship order to all crew members
  ## * Manufacture_Icon      - Icon used for Manufacturing info when someone working on it
  ## * No_Manufacture_Icon   - Icon used for Manufacturing info when noone is assigned
  ## * Move_Map_Up_Icon      - Icon used for move map up button
  ## * Move_Map_Down_Icon    - Icon used for move map down button
  ## * Move_Map_Left_Icon    - Icon used for move map left button
  ## * Move_Map_Right_Icon   - Icon used for move map right button
  ## * No_Fuel_Icon          - Icon used for show warning about no fuel
  ## * Low_Fuel_Icon         - Icon used for show warning about low level of fuel
  ## * No_Food_Icon          - Icon used for show warning about no food
  ## * Low_Food_Icon         - Icon used for show warning about low level of food
  ## * No_Drinks_Icon        - Icon used for show warning about no drinks
  ## * Low_Drinks_Icon       - Icon used for show warning about low level of drinks
  ## * Not_Visited_Base_Icon - Icon used for show not visited bases on map
  ## * Player_Ship_Icon      - Icon used for show player ship on map
  ## * Empty_Map_Icon        - Icon used for empty map fields
  ## * Target_Icon           - Icon used for player selected target on map
  ## * Story_Icon            - Icon used for show story event location on map
  ## * Overloaded_Icon       - Icon used for show warning about overloaded ship
  ## * Arrow_Up_Icon         - Icon used for moving map or ship up
  ## * Arrow_Down_Icon       - Icon used for moving map or ship down
  ## * Arrow_Left_Icon       - Icon used for moving map or ship left
  ## * Arrow_Right_Icon      - Icon used for moving map or ship right
  ## * Arrow_Up_Left_Icon    - Icon used for moving map or ship up and left
  ## * Arrow_Up_Right_Icon   - Icon used for moving map or ship up and right
  ## * Arrow_Down_Right_Icon - Icon used for moving map or ship down and right
  ## * Arrow_Down_Left_Icon  - Icon used for moving map or ship down and left
  ## * Wait_Icon             - Icon used for wait one minute
  ## * Move_Step_Icon        - Icon used for move one step towards destination
  ## * Move_To_Icon          - Icon used for move ship to destination
  ## * Menu_Icon             - Icon used for showing menus
  ## * Exit_Icon             - Icon used for exit button
  ## * Random_Icon           - Icon used for setting random value buttons
  ## * Male_Icon             - Icon used for male gender
  ## * Female_Icon           - Icon used for female gender
  ## * Edit_Icon             - Icon used for edit button
  ## * Show_Icon             - Icon used for show button
  ## * Cancel_Icon           - Icon used for cancel actions
  ## * Help_Icon             - Icon used for showing help
  ## * Special_Help_Color    - Name of color used to show keys and special names in the help.
  ##                           Can be any value accepted by Tcl.
  ## * Underline_Help_Color  - Name of color used for underlined text in the help. Can be any
  ##                           value accepted by Tcl.
  ## * Bold_Help_Color       - Name of color used for bold text in the help. Can be any value
  ##                           accepted by Tcl.
  ## * Italic_Help_Color     - Name of color used for italic text in the help. Can be any value
  ##                           accepted by Tcl.
  ## * Give_Icon             - Icon used for give items button
  ## * Drop_Icon             - Icon used for drop items button
  ## * Buy_Icon              - Icon used for buy items button
  ## * Sell_Icon             - Icon used for sell items button
  ## * Craft_Icon            - Icon used for set crafting order button
  ## * Study_Icon            - Icon used for set study order button
  ## * Deconstruct_Icon      - Icon used for set deconstruct order button
  ## * Negotiate_Icon        - Icon used for negotiation button
  ## * Cargo_Icon            - Icon used for represent the player's ship's cargo
  ## * Equip_Icon            - Icon used for equip item action
  ## * Unequip_Icon          - Icon used for unequip item action
  ## * Select_All_Icon       - Icon used for select all items on a list button
  ## * Unselect_All_Icon     - Icon used for unselect all items on a list button
  ## * Give_Order_Icon       - Icon used for give order to the crew member button
  ## * No_Pilot_Icon         - Icon used for Pilot info in sentient ships
  ## * No_Engineer_Icon      - Icon used for Engineer info in sentient ships
  ## * Destination_Icon      - Icon used for set the ship destination button
  ## * Inventory_Icon        - Icon used for show inventory button
  ## * Dismiss_Icon          - Icon used for dismiss crew member button
  ## * Go_Rest_Icon          - Icon used for give order to go rest for the whole crew
  ## * Repair_Priority_Icon  - Icon used for set the repair priority button
  ## * Upgrade_Button_Icon   - Icon used for the upgrade button
  ## * Power_Icon            - Icon used for the enable or disable engine button
  ## * Assign_Crew_Icon      - Icon used for assign crew members to ship's modules
  ## * Assign_Ammo_Icon      - Icon used for assign ammo to ship's guns
  ## * Buy_Default_Icon      - Icon used for buy items button with default color
  ## * Sell_Default_Icon     - Icon used for sell items button with default color
  ## * Move_Icon             - Icon used for moving items from inventory to cargo
  ## * Give_Colored_Icon     - Icon used for give items button with green color
  ## * Drop_Colored_Icon     - Icon used for drop items button with green color
  ## * Edit_Colored_Icon     - Icon used for edit button with green color
  Name*: string
  File_Name*: string
  Enemy_Ship_Icon*: Rune
  Attack_On_Base_Icon*: Rune
  Disease_Icon*: Rune
  Double_Price_Icon*: Rune
  Full_Docks_Icon*: Rune
  Enemy_Patrol_Icon*: Rune
  Trader_Icon*: Rune
  Friendly_Ship_Icon*: Rune
  Deliver_Icon*: Rune
  Destroy_Icon*: Rune
  Patrol_Icon*: Rune
  Explore_Icon*: Rune
  Passenger_Icon*: Rune
  Pilot_Icon*: string
  Engineer_Icon*: string
  Gunner_Icon*: string
  Crew_Trader_Icon*: string
  Repair_Icon*: string
  No_Repair_Icon*: string
  Repair_Order_Icon*: string
  Upgrade_Icon*: string
  No_Upgrade_Icon*: string
  Clean_Icon*: string
  No_Clean_Icon*: string
  Clean_Order_Icon*: string
  Manufacture_Icon*: string
  No_Manufacture_Icon*: string
  Move_Map_Up_Icon*: string
  Move_Map_Down_Icon*: string
  Move_Map_Left_Icon*: string
  Move_Map_Right_Icon*: string
  No_Fuel_Icon*: string
  Low_Fuel_Icon*: string
  No_Food_Icon*: string
  Low_Food_Icon*: string
  No_Drinks_Icon*: string
  Low_Drinks_Icon*: string
  Not_Visited_Base_Icon*: Rune
  Player_Ship_Icon*: Rune
  Empty_Map_Icon*: Rune
  Target_Icon*: Rune
  Story_Icon*: Rune
  Overloaded_Icon*: string
  Arrow_Up_Icon*: string
  Arrow_Down_Icon*: string
  Arrow_Left_Icon*: string
  Arrow_Right_Icon*: string
  Arrow_Up_Left_Icon*: string
  Arrow_Up_Right_Icon*: string
  Arrow_Down_Right_Icon*: string
  Arrow_Down_Left_Icon*: string
  Wait_Icon*: string
  Move_Step_Icon*: string
  Move_To_Icon*: string
  Menu_Icon*: string
  Exit_Icon*: string
  Random_Icon*: string
  Male_Icon*: string
  Female_Icon*: string
  Edit_Icon*: string
  Show_Icon*: string
  Cancel_Icon*: string
  Help_Icon*: string
  Special_Help_Color*: string
  Underline_Help_Color*: string
  Bold_Help_Color*: string
  Italic_Help_Color*: string
  Give_Icon*: string
  Drop_Icon*: string
  Buy_Icon*: string
  Sell_Icon*: string
  Craft_Icon*: string
  Study_Icon*: string
  Deconstruct_Icon*: string
  Negotiate_Icon*: string
  Cargo_Icon*: string
  Equip_Icon*: string
  Unequip_Icon*: string
  Select_All_Icon*: string
  Unselect_All_Icon*: string
  Give_Order_Icon*: string
  No_Pilot_Icon*: string
  No_Engineer_Icon*: string
  Destination_Icon*: string
  Inventory_Icon*: string
  Dismiss_Icon*: string
  Go_Rest_Icon*: string
  Repair_Priority_Icon*: string
  Upgrade_Button_Icon*: string
  Power_Icon*: string
  Assign_Crew_Icon*: string
  Assign_Ammo_Icon*: string
  Buy_Default_Icon*: string
  Sell_Default_Icon*: string
  Move_Icon*: string
  Give_Colored_Icon*: string
  Drop_Colored_Icon*: string
  Edit_Colored_Icon*: string
