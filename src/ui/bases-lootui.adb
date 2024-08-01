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

with Ada.Containers.Vectors;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo;
with Bases.Cargo;
with CoreUI;
with Maps; use Maps;
with Table; use Table;
with Utils.UI;

package body Bases.LootUI is

   -- ****iv* LUI/LUI.Loot_Table
   -- FUNCTION
   -- Table with info about the available items to loot
   -- SOURCE
   Loot_Table: Table_Widget (Amount => 5);
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****iv* LUI/LUI.Items_Indexes
   -- FUNCTION
   -- Indexes of the items for loot
   -- SOURCE
   Items_Indexes: Natural_Container.Vector;
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****it* LUI/LUI.Items_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the looting list
   -- OPTIONS
   -- NAMEASC        - Sort items by name ascending
   -- NAMEDESC       - Sort items by name descending
   -- TYPEASC        - Sort items by type ascending
   -- TYPEDESC       - Sort items by type descending
   -- DURABILITYASC  - Sort items by durability ascending
   -- DURABILITYDESC - Sort items by durability descending
   -- OWNEDASC       - Sort items by owned amount ascending
   -- OWNEDDESC      - Sort items by owned amount descending
   -- AVAILABLEASC   - Sort items by available amount ascending
   -- AVAILABLEDESC  - Sort items by available amount descending
   -- NONE           - No sorting modules (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Items_Sort_Orders is
     (NAMEASC, NAMEDESC, TYPEASC, TYPEDESC, DURABILITYASC, DURABILITYDESC,
      OWNEDASC, OWNEDDESC, AVAILABLEASC, AVAILABLEDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* LUI/LUI.Default_Items_Sort_Order
      -- FUNCTION
      -- Default sorting order for the looting list
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Items_Sort_Order: constant Items_Sort_Orders := NONE;
   -- ****

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* LUI/LUI.Items_Sort_Order
   -- FUNCTION
   -- The current sorting order for the looting list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Items_Sort_Order: Items_Sort_Orders := Default_Items_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* LUI/LUI.Show_Loot_Command
   -- FUNCTION
   -- Show information about looting
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLoot
   -- SOURCE
   function Show_Loot_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Loot_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.Canvas;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use Tcl.Tk.Ada.Winfo;
      use CoreUI;

      Loot_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".lootframe", Interp => Interp);
      Loot_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Loot_Frame & ".canvas", Interp => Interp);
      Label: constant Ttk_Label :=
        Get_Widget
          (pathName => Loot_Canvas & ".loot.options.typelabel",
           Interp => Interp);
      function Show_Ada_Loot_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Convention => C,
         Import => True,
         External_Name => "showLootCommand";
   begin
      if Show_Ada_Loot_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv) =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
      if Winfo_Get(Widgt => Label, Info => "exists") = "0" then
         Loot_Frame := Get_Widget(pathName => Loot_Canvas & ".loot");
         Loot_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Loot_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Type"),
                 3 => To_Unbounded_String(Source => "Durability"),
                 4 => To_Unbounded_String(Source => "Owned"),
                 5 => To_Unbounded_String(Source => "Available")),
              Scrollbar =>
                Get_Widget
                  (pathName => ".gameframe.paned.lootframe.scrolly",
                   Interp => Interp),
              Command => "SortLootItems",
              Tooltip_Text => "Press mouse button to sort the items.");
      end if;
      return TCL_OK;
   end Show_Loot_Command;

   -- ****if* LUI/LUI.Item_Index
   -- FUNCTION
   -- Index of the currently selected item
   -- SOURCE
   Item_Index: Integer;
   -- ****

   -- ****if* LUI/LUI.Get_Item_Index
   -- FUNCTION
   -- Get the index of the currently selected item
   -- RESULT
   -- The index of the currently selected item
   -- SOURCE
   function Get_Item_Index return Integer is
      -- ****
   begin
      return Item_Index;
   end Get_Item_Index;

   -- ****o* LUI/LUI.Show_Trade_Loot_Info_Command
   -- FUNCTION
   -- Show information about the selected item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLootItemInfo itemindex
   -- ItemIndex is a index of the item which info will be shown.
   -- SOURCE
   function Show_Loot_Item_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Loot_Item_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      procedure Get_Ada_Loot_Item_Index(I_Index: Integer) with
         Convention => C,
         Import => True,
         External_Name => "getLootItemIndex";
      function Show_Ada_Loot_Item_Info_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Convention => C,
         Import => True,
         External_Name => "showLootItemInfoCommand";
   begin
      Item_Index := Integer'Value(CArgv.Arg(Argv => Argv, N => 1));
      Get_Ada_Loot_Item_Index(I_Index => Get_Item_Index);
      return
        Show_Ada_Loot_Item_Info_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv);
   end Show_Loot_Item_Info_Command;

   -- ****o* LUI/LUI.Loot_Item_Command
   -- FUNCTION
   -- Take or drop the selected item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- LootItem actiontype
   -- actiontype can be: drop, dropall, take, takeall
   -- SOURCE
   function Loot_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "lootItemCommand";
      -- ****

   -- ****o* LUI/LUI.Loot_Amount_Command
   -- FUNCTION
   -- Show dialog to enter amount of items to drop or take
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- LootAmount action maxamount
   -- Action which will be taken. Can be take or drop. Maxamount is the
   -- maximum allowed amount of items to take
   -- SOURCE
   function Loot_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "lootAmountCommand";
      -- ****

   -- ****o* LUI/LUI.Sort_Items_Command
   -- FUNCTION
   -- Sort the looting list
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortLootItems x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Items_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Items_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Bases.Cargo;
      use Tiny_String;

      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Column: constant Positive :=
        Get_Column_Number
          (Table => Loot_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      --## rule off TYPE_INITIAL_VALUES
      type Local_Item_Data is record
         Name: Unbounded_String;
         I_Type: Bounded_String;
         Damage: Float;
         Owned: Natural;
         Available: Natural;
         Id: Positive;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Local_Base_Cargo: BaseCargo_Container.Vector
        (Capacity =>
           BaseCargo_Container.Length
             (Container => Sky_Bases(Base_Index).Cargo));
      Base_Cargo_Index: Natural := 0;
      Proto_Index: Natural := 0;
      package Items_Container is new Vectors
        (Index_Type => Positive, Element_Type => Local_Item_Data);
      --## rule off IMPROPER_INITIALIZATION
      Local_Items: Items_Container.Vector;
      Indexes_List: Positive_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Local_Item_Data) return Boolean is
      begin
         if Items_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Items_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Items_Sort_Order = TYPEASC and then Left.I_Type < Right.I_Type then
            return True;
         end if;
         if Items_Sort_Order = TYPEDESC
           and then Left.I_Type > Right.I_Type then
            return True;
         end if;
         if Items_Sort_Order = DURABILITYASC
           and then Left.Damage < Right.Damage then
            return True;
         end if;
         if Items_Sort_Order = DURABILITYDESC
           and then Left.Damage > Right.Damage then
            return True;
         end if;
         if Items_Sort_Order = OWNEDASC and then Left.Owned < Right.Owned then
            return True;
         end if;
         if Items_Sort_Order = OWNEDDESC and then Left.Owned > Right.Owned then
            return True;
         end if;
         if Items_Sort_Order = AVAILABLEASC
           and then Left.Available < Right.Available then
            return True;
         end if;
         if Items_Sort_Order = AVAILABLEDESC
           and then Left.Available > Right.Available then
            return True;
         end if;
         return False;
      end "<";
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      package Sort_Items is new Items_Container.Generic_Sorting;
   begin
      BaseCargo_Container.Assign
        (Target => Local_Base_Cargo, Source => Sky_Bases(Base_Index).Cargo);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      case Column is
         when 1 =>
            if Items_Sort_Order = NAMEASC then
               Items_Sort_Order := NAMEDESC;
            else
               Items_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Items_Sort_Order = TYPEASC then
               Items_Sort_Order := TYPEDESC;
            else
               Items_Sort_Order := TYPEASC;
            end if;
         when 3 =>
            if Items_Sort_Order = DURABILITYASC then
               Items_Sort_Order := DURABILITYDESC;
            else
               Items_Sort_Order := DURABILITYASC;
            end if;
         when 4 =>
            if Items_Sort_Order = OWNEDASC then
               Items_Sort_Order := OWNEDDESC;
            else
               Items_Sort_Order := OWNEDASC;
            end if;
         when 5 =>
            if Items_Sort_Order = AVAILABLEASC then
               Items_Sort_Order := AVAILABLEDESC;
            else
               Items_Sort_Order := AVAILABLEASC;
            end if;
         when others =>
            null;
      end case;
      if Items_Sort_Order = Default_Items_Sort_Order then
         return TCL_OK;
      end if;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Add_Cargo_Items_Loop :
      for I in
        Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
          Inventory_Container.Last_Index(Container => Player_Ship.Cargo) loop
         Proto_Index :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => I)
             .Proto_Index;
         Base_Cargo_Index :=
           Find_Base_Cargo
             (Proto_Index => Proto_Index,
              Durability =>
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => I)
                  .Durability);
         if Base_Cargo_Index > 0 then
            Indexes_List.Append(New_Item => Base_Cargo_Index);
         end if;
         Local_Items.Append
           (New_Item =>
              (Name =>
                 To_Unbounded_String
                   (Source =>
                      Get_Item_Name
                        (Item =>
                           Inventory_Container.Element
                             (Container => Player_Ship.Cargo, Index => I))),
               I_Type =>
                 (if
                    Get_Proto_Item(Index => Proto_Index).Show_Type =
                    Null_Bounded_String
                  then Get_Proto_Item(Index => Proto_Index).I_Type
                  else Get_Proto_Item(Index => Proto_Index).Show_Type),
               Damage =>
                 Float
                   (Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => I)
                      .Durability) /
                 Float(Default_Item_Durability),
               Owned =>
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => I)
                   .Amount,
               Available =>
                 (if Base_Cargo_Index > 0 then
                    BaseCargo_Container.Element
                      (Container => Local_Base_Cargo,
                       Index => Base_Cargo_Index)
                      .Amount
                  else 0),
               Id => I));
      end loop Add_Cargo_Items_Loop;
      Sort_Items.Sort(Container => Local_Items);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Items_Indexes.Clear;
      Fill_Items_Indexes_Loop :
      for Item of Local_Items loop
         Items_Indexes.Append(New_Item => Item.Id);
      end loop Fill_Items_Indexes_Loop;
      Items_Indexes.Append(New_Item => 0);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Local_Items.Clear;
      Add_Base_Items_Loop :
      for I in
        BaseCargo_Container.First_Index(Container => Local_Base_Cargo) ..
          BaseCargo_Container.Last_Index(Container => Local_Base_Cargo) loop
         if Indexes_List.Find_Index(Item => I) = 0 then
            Proto_Index :=
              BaseCargo_Container.Element
                (Container => Local_Base_Cargo, Index => I)
                .Proto_Index;
            Local_Items.Append
              (New_Item =>
                 (Name =>
                    To_Unbounded_String
                      (Source =>
                         To_String
                           (Source =>
                              Get_Proto_Item(Index => Proto_Index).Name)),
                  I_Type =>
                    (if
                       Get_Proto_Item(Index => Proto_Index).Show_Type =
                       Null_Bounded_String
                     then Get_Proto_Item(Index => Proto_Index).I_Type
                     else Get_Proto_Item(Index => Proto_Index).Show_Type),
                  Damage =>
                    Float
                      (BaseCargo_Container.Element
                         (Container => Local_Base_Cargo, Index => I)
                         .Durability) /
                    Float(Default_Item_Durability),
                  Owned => 0,
                  Available =>
                    BaseCargo_Container.Element
                      (Container => Local_Base_Cargo, Index => I)
                      .Amount,
                  Id => I));
         end if;
      end loop Add_Base_Items_Loop;
      Sort_Items.Sort(Container => Local_Items);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Fill_Items_Indexes_Base_Loop :
      for Item of Local_Items loop
         Items_Indexes.Append(New_Item => Item.Id);
      end loop Fill_Items_Indexes_Base_Loop;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      return
        Show_Loot_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => CArgv.Empty & "ShowLoot" & "All");
   end Sort_Items_Command;

   procedure Add_Commands is
      use Utils.UI;
   begin
      Add_Command(Name => "ShowLoot", Ada_Command => Show_Loot_Command'Access);
      Add_Command
        (Name => "ShowLootItemInfo",
         Ada_Command => Show_Loot_Item_Info_Command'Access);
      Add_Command(Name => "LootItem", Ada_Command => Loot_Item_Command'Access);
      Add_Command
        (Name => "LootAmount", Ada_Command => Loot_Amount_Command'Access);
      Add_Command
        (Name => "SortLootItems", Ada_Command => Sort_Items_Command'Access);
   end Add_Commands;

end Bases.LootUI;
