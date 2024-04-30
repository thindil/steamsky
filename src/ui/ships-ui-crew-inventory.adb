-- Copyright (c) 2020-2024 Bartek thindil Jasicki
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Utils.UI;

package body Ships.UI.Crew.Inventory is

   -- ****iv* SUCI/SUCI.Member_Index
   -- FUNCTION
   -- The index of the selected crew member
   -- SOURCE
--   Member_Index: Positive := 1;
   -- ****

   -- ****o* SUCI/SUCI.Update_Inventory_Command
   -- FUNCTION
   -- Update inventory list of the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateInventory memberindex page
   -- MemberIndex is the index of the crew member to show inventory, page
   -- is a number of the page of inventory list to show
   -- SOURCE
   function Update_Inventory_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "updateInventoryCommand";
      -- ****

   -- ****o* SUCI/SUCI.Set_Use_Item_Command
   -- FUNCTION
   -- Set if item is used by a crew member or not
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetUseItem itemindex
   -- itemindex is the index of the item which will be set
   -- SOURCE
   function Set_Use_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setUseItemCommand";
      -- ****

   -- ****o* SUCI/SUCI.Move_Item_Command
   -- FUNCTION
   -- Move the selected item to the ship cargo
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MoveItem itemindex
   -- itemindex is the index of the item which will be set
   -- SOURCE
   function Move_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "moveItemCommand";
      -- ****

   -- ****o* SUCI/SUCI.Validate_Move_Amount_Command
   -- FUNCTION
   -- Validate amount of the item to move
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ValidateMoveAmount maxvalue amount button spinbox
   -- SOURCE
   function Validate_Move_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Validate_Move_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Tcl.Ada;
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;

      Amount: Natural := 0;
      Button: constant Ttk_Button :=
        Get_Widget(pathName => CArgv.Arg(Argv => Argv, N => 3));
      Max_Val: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Spin_Box: constant Ttk_SpinBox :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 4), Interp => Interp);
   begin
      if CArgv.Arg(Argv => Argv, N => 2)'Length > 0 then
         Amount := Natural'Value(CArgv.Arg(Argv => Argv, N => 2));
      end if;
      if Amount < 1 then
         Widgets.configure(Widgt => Button, options => "-state disabled");
         Tcl_SetResult(interp => Interp, str => "1");
         return TCL_OK;
      elsif Amount > Max_Val then
         Set(SpinBox => Spin_Box, Value => Positive'Image(Max_Val));
      end if;
      Widgets.configure(Widgt => Button, options => "-state normal");
      Tcl_SetResult(interp => Interp, str => "1");
      return TCL_OK;
   exception
      when Constraint_Error =>
         Tcl_SetResult(interp => Interp, str => "0");
         return TCL_OK;
   end Validate_Move_Amount_Command;

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****o* SUCI/SUCI.Show_Inventory_Item_Info_Command
   -- FUNCTION
   -- Show detailed information about the selected item in crew member
   -- inventory
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowInventoryItemInfo memberindex itemindex
   -- itemindex is the index of the item which will be show
   -- SOURCE
   function Show_Inventory_Item_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showInventoryItemInfoCommand";
      -- ****

   -- ****o* SUCI/SUCI.Toggle_Inventory_Item
   -- FUNCTION
   -- Select or deselect the selected item in the inventory
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleInventoryItem rowindex, itemindex
   -- Rowindex is the index of the row in which is the selected item,
   -- itemindex is the index of the selected item in crew member inventory.
   -- SOURCE
   function Toggle_Inventory_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "toggleInventoryItemCommand";
      -- ****

   -- ****o* SUCI/SUCI.Toggle_Inventory_Items_Command
   -- FUNCTION
   -- Equip or unequip the selected items
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleInventoryItems action
   -- Action is the action to do with the selected items. Possible values are
   -- equip and unequip
   -- SOURCE
   function Toggle_Inventory_Items_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "toggleInventoryItemsCommand";
      -- ****

   -- ****o* SUCI/SUCI.Move_Items_Command
   -- FUNCTION
   -- Move the selected items to the ships's cargo
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MoveItems
   -- SOURCE
   function Move_Items_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "moveItemsCommand";
      -- ****

   procedure Add_Inventory_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "UpdateInventory",
         Ada_Command => Update_Inventory_Command'Access);
      Add_Command
        (Name => "SetUseItem", Ada_Command => Set_Use_Item_Command'Access);
      Add_Command(Name => "MoveItem", Ada_Command => Move_Item_Command'Access);
      Add_Command
        (Name => "ValidateMoveAmount",
         Ada_Command => Validate_Move_Amount_Command'Access);
      Add_Command
        (Name => "ShowInventoryItemInfo",
         Ada_Command => Show_Inventory_Item_Info_Command'Access);
      Add_Command
        (Name => "ToggleInventoryItem",
         Ada_Command => Toggle_Inventory_Item_Command'Access);
      Add_Command
        (Name => "ToggleInventoryItems",
         Ada_Command => Toggle_Inventory_Items_Command'Access);
      Add_Command
        (Name => "MoveItems", Ada_Command => Move_Items_Command'Access);
   end Add_Inventory_Commands;

end Ships.UI.Crew.Inventory;
