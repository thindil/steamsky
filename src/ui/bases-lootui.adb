-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases.Cargo; use Bases.Cargo;
with BasesTypes; use BasesTypes;
with Config; use Config;
with CoreUI; use CoreUI;
with Dialogs; use Dialogs;
with Events; use Events;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Bases.LootUI is

   -- ****iv* LUI/LUI.Loot_Table
   -- FUNCTION
   -- Table with info about the available items to loot
   -- SOURCE
   Loot_Table: Table_Widget (Amount => 5);
   -- ****

   -- ****iv* LUI/LUI.Items_Indexes
   -- FUNCTION
   -- Indexes of the items for loot
   -- SOURCE
   Items_Indexes: Natural_Container.Vector;
   -- ****

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

   -- ****iv* LUI/LUI.Items_Sort_Order
   -- FUNCTION
   -- The current sorting order for the looting list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Items_Sort_Order: Items_Sort_Orders := Default_Items_Sort_Order;
   -- ****

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
      pragma Unreferenced(Client_Data);
      use Tiny_String;

      Loot_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".lootframe", Interp => Interp);
      Loot_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Loot_Frame & ".canvas", Interp => Interp);
      Label: Ttk_Label :=
        Get_Widget
          (pathName => Loot_Canvas & ".loot.options.typelabel",
           Interp => Interp);
      Item_Name, Item_Type: Bounded_String;
      Item_Durability, Trade_Info: Unbounded_String;
      Items_Types: Unbounded_String := To_Unbounded_String(Source => "All");
      Combo_Box: Ttk_ComboBox;
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Base_Cargo: BaseCargo_Container.Vector (Capacity => 16);
      Base_Cargo_Index, Base_Amount: Natural;
      Indexes_List: Positive_Container.Vector;
      Page: constant Positive :=
        (if Argc = 3 then Positive'Value(CArgv.Arg(Argv => Argv, N => 2))
         else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      Arguments: constant String :=
        (if Argc > 1 then "{" & CArgv.Arg(Argv => Argv, N => 1) & "}"
         else "All");
      Current_Item_Index: Positive := 1;
      Proto_Index: Objects_Container.Extended_Index;
   begin
      if Winfo_Get(Widgt => Label, Info => "exists") = "0" then
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "loot.tcl");
         Bind
           (Widgt => Loot_Frame, Sequence => "<Configure>",
            Script => "{ResizeCanvas %W.canvas %w %h}");
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
              Tooltip => "Press mouse button to sort the items.");
      elsif Winfo_Get(Widgt => Label, Info => "ismapped") = "1" and
        Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         Show_Sky_Map(Clear => True);
         return TCL_OK;
      end if;
      Loot_Frame.Name := New_String(Str => Loot_Canvas & ".loot");
      Combo_Box :=
        Get_Widget(pathName => Loot_Frame & ".options.type", Interp => Interp);
      BaseCargo_Container.Assign
        (Target => Base_Cargo, Source => Sky_Bases(Base_Index).Cargo);
      if Items_Sort_Order = Default_Items_Sort_Order then
         Items_Indexes.Clear;
         Add_Cargo_Indexes_Loop :
         for I in
           Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
             Inventory_Container.Last_Index
               (Container => Player_Ship.Cargo) loop
            Items_Indexes.Append(New_Item => I);
         end loop Add_Cargo_Indexes_Loop;
         Items_Indexes.Append(New_Item => 0);
         Add_Base_Indexes_Loop :
         for I in
           BaseCargo_Container.First_Index(Container => Base_Cargo) ..
             BaseCargo_Container.Last_Index(Container => Base_Cargo) loop
            Items_Indexes.Append(New_Item => I);
         end loop Add_Base_Indexes_Loop;
      end if;
      Clear_Table(Table => Loot_Table);
      Add_Player_Cargo_Loop :
      for I of Items_Indexes loop
         Current_Item_Index := Current_Item_Index + 1;
         exit Add_Player_Cargo_Loop when I = 0;
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
         Item_Type :=
           (if
              Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .Show_Type =
              Null_Bounded_String
            then
              Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .I_Type
            else Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .Show_Type);
         if Index
             (Source => Items_Types,
              Pattern => To_String(Source => "{" & Item_Type & "}")) =
           0 then
            Append
              (Source => Items_Types,
               New_Item => " {" & To_String(Source => Item_Type) & "}");
         end if;
         if Argc > 1 and then CArgv.Arg(Argv => Argv, N => 1) /= "All"
           and then To_String(Source => Item_Type) /=
             CArgv.Arg(Argv => Argv, N => 1) then
            goto End_Of_Cargo_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Cargo_Loop;
         end if;
         Item_Name :=
           To_Bounded_String
             (Source =>
                Get_Item_Name
                  (Item =>
                     Inventory_Container.Element
                       (Container => Player_Ship.Cargo, Index => I),
                   Damage_Info => False, To_Lower => False));
         Add_Button
           (Table => Loot_Table, Text => To_String(Source => Item_Name),
            Tooltip => "Show available options for item",
            Command => "ShowLootItemMenu" & Positive'Image(I), Column => 1);
         Add_Button
           (Table => Loot_Table, Text => To_String(Source => Item_Type),
            Tooltip => "Show available options for item",
            Command => "ShowLootItemMenu" & Positive'Image(I), Column => 2);
         Item_Durability :=
           (if
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I)
                .Durability <
              100
            then
              To_Unbounded_String
                (Source =>
                   Get_Item_Damage
                     (Item_Durability =>
                        Inventory_Container.Element
                          (Container => Player_Ship.Cargo, Index => I)
                          .Durability))
            else To_Unbounded_String(Source => "Unused"));
         Add_Progress_Bar
           (Table => Loot_Table,
            Value =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I)
                .Durability,
            Max_Value => Default_Item_Durability,
            Tooltip => To_String(Source => Item_Durability),
            Command => "ShowLootItemMenu" & Positive'Image(I), Column => 3);
         Add_Button
           (Table => Loot_Table,
            Text =>
              Natural'Image
                (Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => I)
                   .Amount),
            Tooltip => "Show available options for item",
            Command => "ShowLootItemMenu" & Positive'Image(I), Column => 4);
         Base_Amount :=
           (if Base_Cargo_Index > 0 then
              BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Base_Cargo_Index)
                .Amount
            else 0);
         Add_Button
           (Table => Loot_Table, Text => Natural'Image(Base_Amount),
            Tooltip => "Show available options for item",
            Command => "ShowLootItemMenu" & Positive'Image(I), Column => 5,
            New_Row => True);
         exit Add_Player_Cargo_Loop when Loot_Table.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Cargo_Loop>>
      end loop Add_Player_Cargo_Loop;
      Add_Base_Cargo_Loop :
      for I in Current_Item_Index .. Items_Indexes.Last_Index loop
         exit Add_Base_Cargo_Loop when Loot_Table.Row =
           Game_Settings.Lists_Limit + 1;
         if Indexes_List.Find_Index(Item => Items_Indexes(I)) > 0 then
            goto End_Of_Base_Cargo_Loop;
         end if;
         Proto_Index :=
           BaseCargo_Container.Element
             (Container => Base_Cargo, Index => Items_Indexes(I))
             .Proto_Index;
         Item_Type :=
           (if
              Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .Show_Type =
              Null_Bounded_String
            then
              Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .I_Type
            else Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .Show_Type);
         if Index
             (Source => Items_Types,
              Pattern => To_String(Source => "{" & Item_Type & "}")) =
           0 then
            Append
              (Source => Items_Types,
               New_Item => " {" & To_String(Source => Item_Type) & "}");
         end if;
         if Argc = 2 and then CArgv.Arg(Argv => Argv, N => 1) /= "All"
           and then To_String(Source => Item_Type) /=
             CArgv.Arg(Argv => Argv, N => 1) then
            goto End_Of_Base_Cargo_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Base_Cargo_Loop;
         end if;
         Item_Name :=
           Objects_Container.Element
             (Container => Items_List, Index => Proto_Index)
             .Name;
         Add_Button
           (Table => Loot_Table, Text => To_String(Source => Item_Name),
            Tooltip => "Show available options for item",
            Command =>
              "ShowLootItemMenu -" &
              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
            Column => 1);
         Add_Button
           (Table => Loot_Table, Text => To_String(Source => Item_Type),
            Tooltip => "Show available options for item",
            Command =>
              "ShowLootItemMenu -" &
              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
            Column => 2);
         Item_Durability :=
           (if
              BaseCargo_Container.Element
                (Container => Base_Cargo, Index => Items_Indexes(I))
                .Durability <
              100
            then
              To_Unbounded_String
                (Source =>
                   Get_Item_Damage
                     (Item_Durability =>
                        BaseCargo_Container.Element
                          (Container => Base_Cargo, Index => I)
                          .Durability))
            else To_Unbounded_String(Source => "Unused"));
         Add_Progress_Bar
           (Table => Loot_Table,
            Value =>
              BaseCargo_Container.Element
                (Container => Base_Cargo, Index => Items_Indexes(I))
                .Durability,
            Max_Value => Default_Item_Durability,
            Tooltip => To_String(Source => Item_Durability),
            Command =>
              "ShowLootItemMenu -" &
              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
            Column => 3);
         Add_Button
           (Table => Loot_Table, Text => "0",
            Tooltip => "Show available options for item",
            Command =>
              "ShowLootItemMenu -" &
              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
            Column => 4);
         Base_Amount :=
           BaseCargo_Container.Element
             (Container => Sky_Bases(Base_Index).Cargo,
              Index => Items_Indexes(I))
             .Amount;
         Add_Button
           (Table => Loot_Table, Text => Natural'Image(Base_Amount),
            Tooltip => "Show available options for item",
            Command =>
              "ShowLootItemMenu -" &
              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
            Column => 5, New_Row => True);
         <<End_Of_Base_Cargo_Loop>>
      end loop Add_Base_Cargo_Loop;
      if Page > 1 then
         if Loot_Table.Row < Game_Settings.Lists_Limit + 1 then
            Add_Pagination
              (Table => Loot_Table,
               Previous_Command =>
                 "ShowLoot " & Arguments & Positive'Image(Page - 1));
         else
            Add_Pagination
              (Table => Loot_Table,
               Previous_Command =>
                 "ShowLoot " & Arguments & Positive'Image(Page - 1),
               Next_Command =>
                 "ShowLoot " & Arguments & Positive'Image(Page + 1));
         end if;
      elsif Loot_Table.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (Table => Loot_Table,
            Next_Command =>
              "ShowLoot " & Arguments & Positive'Image(Page + 1));
      end if;
      Update_Table(Table => Loot_Table);
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Loot_Table.Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Loot_Table.Canvas, TagOrId => "all") & "]");
      configure
        (Widgt => Combo_Box,
         options => "-values [list " & To_String(Source => Items_Types) & "]");
      if Argc = 1 then
         Current(ComboBox => Combo_Box, NewIndex => "0");
      end if;
      Count_Free_Space_Block :
      declare
         Free_Space: Integer := Free_Cargo(Amount => 0);
      begin
         if Free_Space < 0 then
            Free_Space := 0;
         end if;
         Append
           (Source => Trade_Info,
            New_Item =>
              "Free cargo space:" & Integer'Image(Free_Space) & " kg.");
      end Count_Free_Space_Block;
      Label.Name :=
        New_String(Str => Loot_Canvas & ".loot.options.playerinfo");
      configure
        (Widgt => Label,
         options => "-text {" & To_String(Source => Trade_Info) & "}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Button, Options => "-row 0 -column 1");
      configure
        (Widgt => Loot_Canvas,
         options =>
           "-height [expr " & SashPos(Paned => Main_Paned, Index => "0") &
           " - 20] -width " & cget(Widgt => Main_Paned, option => "-width"));
      Tcl_Eval(interp => Get_Context, strng => "update");
      Canvas_Create
        (Parent => Loot_Canvas, Child_Type => "window",
         Options => "0 0 -anchor nw -window " & Loot_Frame);
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Loot_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Loot_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Loot_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Loot_Canvas, Fraction => "0.0");
      Show_Screen(New_Screen_Name => "lootframe");
      Tcl_SetResult(interp => Interp, str => "1");
      return TCL_OK;
   end Show_Loot_Command;

   -- ****if* LUI/LUI.Item_Index
   -- FUNCTION
   -- Index of the currently selected item
   -- SOURCE
   Item_Index: Integer;
   -- ****

   -- ****o* LUI/LUI.Show_Trade_Loot_Info_Command
   -- FUNCTION
   -- Show information about the selected item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLootItemInfo
   -- SOURCE
   function Show_Loot_Item_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Loot_Item_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
      use Short_String;
      use Tiny_String;

      Item_Info: Unbounded_String;
      Proto_Index: Objects_Container.Extended_Index;
      Cargo_Index, Base_Cargo_Index: Natural := 0;
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Item_Types: constant array(1 .. 6) of Tiny_String.Bounded_String :=
        (1 => Weapon_Type, 2 => Chest_Armor, 3 => Head_Armor, 4 => Arms_Armor,
         5 => Legs_Armor, 6 => Shield_Type);
   begin
      if Item_Index < 0 then
         Base_Cargo_Index := abs (Item_Index);
      else
         Cargo_Index := Item_Index;
      end if;
      if Cargo_Index >
        Natural(Inventory_Container.Length(Container => Player_Ship.Cargo)) or
        Base_Cargo_Index >
          Natural
            (BaseCargo_Container.Length
               (Container => Sky_Bases(Base_Index).Cargo)) then
         return TCL_OK;
      end if;
      Proto_Index :=
        (if Cargo_Index > 0 then
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Cargo_Index)
             .Proto_Index
         else BaseCargo_Container.Element
             (Container => Sky_Bases(Base_Index).Cargo,
              Index => Base_Cargo_Index)
             .Proto_Index);
      Append
        (Source => Item_Info,
         New_Item =>
           "Weight:" &
           Integer'Image
             (Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .Weight) &
           " kg");
      if Objects_Container.Element
          (Container => Items_List, Index => Proto_Index)
          .I_Type =
        Weapon_Type then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & "Skill: " &
              To_String
                (Source =>
                   SkillsData_Container.Element
                     (Container => Skills_List,
                      Index =>
                        Skills_Amount_Range
                          (Objects_Container.Element
                             (Container => Items_List, Index => Proto_Index)
                             .Value
                             (3)))
                     .Name) &
              "/" &
              To_String
                (Source =>
                   AttributesData_Container.Element
                     (Container => Attributes_List,
                      Index =>
                        SkillsData_Container.Element
                          (Container => Skills_List,
                           Index =>
                             Skills_Amount_Range
                               (Objects_Container.Element
                                  (Container => Items_List,
                                   Index => Proto_Index)
                                  .Value
                                  (3)))
                          .Attribute)
                     .Name));
         if Objects_Container.Element
             (Container => Items_List, Index => Proto_Index)
             .Value
             (4) =
           1 then
            Append
              (Source => Item_Info,
               New_Item => LF & "Can be used with shield.");
         else
            Append
              (Source => Item_Info,
               New_Item =>
                 LF & "Can't be used with shield (two-handed weapon).");
         end if;
         Append(Source => Item_Info, New_Item => LF & "Damage type: ");
         case Objects_Container.Element
           (Container => Items_List, Index => Proto_Index)
           .Value
           (5) is
            when 1 =>
               Append(Source => Item_Info, New_Item => "cutting");
            when 2 =>
               Append(Source => Item_Info, New_Item => "impaling");
            when 3 =>
               Append(Source => Item_Info, New_Item => "blunt");
            when others =>
               null;
         end case;
      end if;
      Show_Weapon_Info_Loop :
      for ItemType of Item_Types loop
         if Objects_Container.Element
             (Container => Items_List, Index => Proto_Index)
             .I_Type =
           ItemType then
            Append
              (Source => Item_Info,
               New_Item =>
                 LF & "Damage chance: " &
                 Get_Item_Chance_To_Damage
                   (Item_Data =>
                      Objects_Container.Element
                        (Container => Items_List, Index => Proto_Index)
                        .Value
                        (1)));
            Append
              (Source => Item_Info,
               New_Item =>
                 LF & "Strength:" &
                 Integer'Image
                   (Objects_Container.Element
                      (Container => Items_List, Index => Proto_Index)
                      .Value
                      (2)));
            exit Show_Weapon_Info_Loop;
         end if;
      end loop Show_Weapon_Info_Loop;
      if TinyString_Indefinite_Container.Contains
          (Container => Tools_List,
           Item =>
             Objects_Container.Element
               (Container => Items_List, Index => Proto_Index)
               .I_Type) then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & "Damage chance: " &
              Get_Item_Chance_To_Damage
                (Item_Data =>
                   Objects_Container.Element
                     (Container => Items_List, Index => Proto_Index)
                     .Value
                     (1)));
      end if;
      if Length
          (Source =>
             Objects_Container.Element
               (Container => Items_List, Index => Proto_Index)
               .I_Type) >
        4
        and then
        (Slice
           (Source =>
              Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .I_Type,
            Low => 1, High => 4) =
         "Ammo" or
         Objects_Container.Element
             (Container => Items_List, Index => Proto_Index)
             .I_Type =
           To_Bounded_String(Source => "Harpoon")) then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & "Strength:" &
              Integer'Image
                (Objects_Container.Element
                   (Container => Items_List, Index => Proto_Index)
                   .Value
                   (1)));
      end if;
      if Objects_Container.Element
          (Container => Items_List, Index => Proto_Index)
          .Description /=
        Short_String.Null_Bounded_String then
         Append
           (Source => Item_Info,
            New_Item =>
              LF & LF &
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List, Index => Proto_Index)
                     .Description));
      end if;
      Show_Info
        (Text => To_String(Source => Item_Info),
         Title =>
           To_String
             (Source =>
                Objects_Container.Element
                  (Container => Items_List, Index => Proto_Index)
                  .Name));
      return TCL_OK;
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
      Convention => C;
      -- ****

   function Loot_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Base_Cargo_Index, Cargo_Index: Natural := 0;
      Amount: Natural;
      Proto_Index: Objects_Container.Extended_Index;
      Amount_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => ".itemdialog.amount", Interp => Interp);
      Type_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => Main_Paned & ".lootframe.canvas.loot.options.type",
           Interp => Interp);
   begin
      if Item_Index < 0 then
         Base_Cargo_Index := abs (Item_Index);
      else
         Cargo_Index := Item_Index;
      end if;
      if Cargo_Index > 0 then
         Proto_Index :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Cargo_Index)
             .Proto_Index;
         if Base_Cargo_Index = 0 then
            Base_Cargo_Index := Find_Base_Cargo(Proto_Index => Proto_Index);
         end if;
      else
         Proto_Index :=
           BaseCargo_Container.Element
             (Container => Sky_Bases(Base_Index).Cargo,
              Index => Base_Cargo_Index)
             .Proto_Index;
      end if;
      if CArgv.Arg(Argv => Argv, N => 1) in "drop" | "dropall" then
         Amount :=
           (if CArgv.Arg(Argv => Argv, N => 1) = "drop" then
              Positive'Value(Get(Widgt => Amount_Box))
            else Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Cargo_Index)
                .Amount);
         if Base_Cargo_Index > 0 then
            Update_Base_Cargo
              (Cargo_Index => Base_Cargo_Index, Amount => Amount,
               Durability =>
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => Cargo_Index)
                   .Durability);
         else
            Update_Base_Cargo
              (Proto_Index => Proto_Index, Amount => Amount,
               Durability =>
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => Cargo_Index)
                   .Durability);
         end if;
         Update_Cargo
           (Ship => Player_Ship, Cargo_Index => Cargo_Index,
            Amount => (0 - Amount),
            Durability =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Cargo_Index)
                .Durability);
         Add_Message
           (Message =>
              "You drop" & Positive'Image(Amount) & " " &
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List, Index => Proto_Index)
                     .Name) &
              ".",
            M_Type => ORDERMESSAGE);
      else
         Amount :=
           (if CArgv.Arg(Argv => Argv, N => 1) = "take" then
              Positive'Value(Get(Widgt => Amount_Box))
            else Positive'Value(CArgv.Arg(Argv => Argv, N => 2)));
         if Free_Cargo
             (Amount =>
                0 -
                (Amount *
                 Objects_Container.Element
                   (Container => Items_List, Index => Proto_Index)
                   .Weight)) <
           0 then
            Show_Message
              (Text =>
                 "You can't take that much " &
                 To_String
                   (Source =>
                      Objects_Container.Element
                        (Container => Items_List, Index => Proto_Index)
                        .Name) &
                 ".",
               Title => "Too much taken");
            return TCL_OK;
         end if;
         if Cargo_Index > 0 then
            Update_Cargo
              (Ship => Player_Ship, Cargo_Index => Cargo_Index,
               Amount => Amount,
               Durability =>
                 BaseCargo_Container.Element
                   (Container => Sky_Bases(Base_Index).Cargo,
                    Index => Base_Cargo_Index)
                   .Durability);
         else
            Update_Cargo
              (Ship => Player_Ship, Proto_Index => Proto_Index,
               Amount => Amount,
               Durability =>
                 BaseCargo_Container.Element
                   (Container => Sky_Bases(Base_Index).Cargo,
                    Index => Base_Cargo_Index)
                   .Durability);
         end if;
         Update_Base_Cargo
           (Cargo_Index => Base_Cargo_Index, Amount => (0 - Amount),
            Durability =>
              BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Base_Cargo_Index)
                .Durability);
         Add_Message
           (Message =>
              "You took" & Positive'Image(Amount) & " " &
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List, Index => Proto_Index)
                     .Name) &
              ".",
            M_Type => ORDERMESSAGE);
      end if;
      if CArgv.Arg(Argv => Argv, N => 1) in "take" | "drop" then
         if Close_Dialog_Command
             (Client_Data => Client_Data, Interp => Interp, Argc => 2,
              Argv => CArgv.Empty & "CloseDialog" & ".itemdialog") =
           TCL_ERROR then
            return TCL_ERROR;
         end if;
      end if;
      Update_Header;
      Update_Messages;
      return
        Show_Loot_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => CArgv.Empty & "ShowLoot" & Get(Widgt => Type_Box));
   end Loot_Item_Command;

   -- ****o* LUI/LUI.Show_Item_Menu_Command
   -- FUNCTION
   -- Show menu with actions for the selected item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLootItemMenu itemindex
   -- ItemIndex is a index of the item which menu will be shown.
   -- SOURCE
   function Show_Item_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Item_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Base_Cargo_Index, Cargo_Index: Natural := 0;
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Item_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".itemmenu", Title => "Item actions", Parent_Name => ".");
      Can_Take, Can_Drop: Boolean := False;
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Item_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Item_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Item_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script =>
                 "{focus " & Item_Menu & "." &
                 (if Can_Take then "take" elsif Can_Drop then "drop"
                  else "info") &
                 ";break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Item_Index := Integer'Value(CArgv.Arg(Argv => Argv, N => 1));
      if Item_Index < 0 then
         Base_Cargo_Index := abs (Item_Index);
         Change_Title
           (Dialog => Item_Menu,
            New_Title =>
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List,
                      Index =>
                        BaseCargo_Container.Element
                          (Container => Sky_Bases(Base_Index).Cargo,
                           Index => Base_Cargo_Index)
                          .Proto_Index)
                     .Name) &
              " actions");
      else
         Cargo_Index := Item_Index;
         Change_Title
           (Dialog => Item_Menu,
            New_Title =>
              Get_Item_Name
                (Item =>
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Cargo_Index),
                 Damage_Info => False, To_Lower => False) &
              " actions");
      end if;
      if Cargo_Index > 0 and then Base_Cargo_Index = 0 then
         Base_Cargo_Index :=
           Find_Base_Cargo
             (Proto_Index =>
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => Cargo_Index)
                  .Proto_Index);
      end if;
      if Base_Cargo_Index > 0 then
         Can_Take := True;
         Add_Take_Buttons_Block :
         declare
            Max_Amount: Natural :=
              BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Base_Cargo_Index)
                .Amount;
            Free_Amount: constant Natural :=
              Free_Cargo(Amount => 0) /
              Objects_Container.Element
                (Container => Items_List,
                 Index =>
                   BaseCargo_Container.Element
                     (Container => Sky_Bases(Base_Index).Cargo,
                      Index => Base_Cargo_Index)
                     .Proto_Index)
                .Weight;
         begin
            if Max_Amount > Free_Amount then
               Max_Amount := Free_Amount;
            end if;
            if Max_Amount > 0 then
               Add_Button
                 (Name => ".take", Label => "Take selected amount",
                  Command => "LootAmount take" & Natural'Image(Max_Amount));
               Add_Button
                 (Name => ".takeall",
                  Label => "Take" & Natural'Image(Max_Amount) & " of them",
                  Command => "LootItem takeall" & Natural'Image(Max_Amount));
            end if;
         end Add_Take_Buttons_Block;
      end if;
      if Cargo_Index > 0 then
         Can_Drop := True;
         Add_Button
           (Name => ".drop", Label => "Drop selected amount",
            Command =>
              "LootAmount drop" &
              Natural'Image
                (Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => Cargo_Index)
                   .Amount));
         Add_Button
           (Name => ".dropall", Label => "Drop all owned",
            Command => "LootAmount dropall");
      end if;
      Add_Button
        (Name => ".info", Label => "Show item details",
         Command => "ShowLootItemInfo");
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Item_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Item_Menu_Command;

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
      Convention => C;
      -- ****

   function Loot_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
   begin
      if CArgv.Arg(Argv => Argv, N => 1) = "drop" then
         Show_Manipulate_Item
           (Title =>
              "Drop " &
              Get_Item_Name
                (Item =>
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Item_Index)),
            Command => "LootItem drop", Action => "drop",
            Item_Index => Item_Index);
      else
         if Item_Index > 0 then
            Show_Manipulate_Item
              (Title =>
                 "Take " &
                 Get_Item_Name
                   (Item =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => Item_Index)),
               Command => "LootItem take", Action => "take",
               Item_Index => Item_Index,
               Max_Amount => Natural'Value(CArgv.Arg(Argv => Argv, N => 2)));
         else
            Show_Manipulate_Item
              (Title =>
                 "Take " &
                 To_String
                   (Source =>
                      Objects_Container.Element
                        (Container => Items_List,
                         Index =>
                           BaseCargo_Container.Element
                             (Container => Sky_Bases(Base_Index).Cargo,
                              Index => abs (Item_Index))
                             .Proto_Index)
                        .Name),
               Command => "LootItem take", Action => "take",
               Item_Index => abs (Item_Index),
               Max_Amount => Natural'Value(CArgv.Arg(Argv => Argv, N => 2)));
         end if;
      end if;
      return TCL_OK;
   end Loot_Amount_Command;

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
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number
          (Table => Loot_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
      type Local_Item_Data is record
         Name: Unbounded_String;
         I_Type: Bounded_String;
         Damage: Float;
         Owned: Natural;
         Available: Natural;
         Id: Positive;
      end record;
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Indexes_List: Positive_Container.Vector;
      Base_Cargo: BaseCargo_Container.Vector
        (Capacity =>
           BaseCargo_Container.Length
             (Container => Sky_Bases(Base_Index).Cargo));
      Base_Cargo_Index: Natural;
      Proto_Index: Objects_Container.Extended_Index;
      package Items_Container is new Vectors
        (Index_Type => Positive, Element_Type => Local_Item_Data);
      Local_Items: Items_Container.Vector;
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
      package Sort_Items is new Items_Container.Generic_Sorting;
   begin
      BaseCargo_Container.Assign
        (Target => Base_Cargo, Source => Sky_Bases(Base_Index).Cargo);
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
                    Objects_Container.Element
                      (Container => Items_List, Index => Proto_Index)
                      .Show_Type =
                    Null_Bounded_String
                  then
                    Objects_Container.Element
                      (Container => Items_List, Index => Proto_Index)
                      .I_Type
                  else Objects_Container.Element
                      (Container => Items_List, Index => Proto_Index)
                      .Show_Type),
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
                      (Container => Base_Cargo, Index => Base_Cargo_Index)
                      .Amount
                  else 0),
               Id => I));
      end loop Add_Cargo_Items_Loop;
      Sort_Items.Sort(Container => Local_Items);
      Items_Indexes.Clear;
      Fill_Items_Indexes_Loop :
      for Item of Local_Items loop
         Items_Indexes.Append(New_Item => Item.Id);
      end loop Fill_Items_Indexes_Loop;
      Items_Indexes.Append(New_Item => 0);
      Local_Items.Clear;
      Add_Base_Items_Loop :
      for I in
        BaseCargo_Container.First_Index(Container => Base_Cargo) ..
          BaseCargo_Container.Last_Index(Container => Base_Cargo) loop
         if Indexes_List.Find_Index(Item => I) = 0 then
            Proto_Index :=
              BaseCargo_Container.Element(Container => Base_Cargo, Index => I)
                .Proto_Index;
            Local_Items.Append
              (New_Item =>
                 (Name =>
                    To_Unbounded_String
                      (Source =>
                         To_String
                           (Source =>
                              Objects_Container.Element
                                (Container => Items_List, Index => Proto_Index)
                                .Name)),
                  I_Type =>
                    (if
                       Objects_Container.Element
                         (Container => Items_List, Index => Proto_Index)
                         .Show_Type =
                       Null_Bounded_String
                     then
                       Objects_Container.Element
                         (Container => Items_List, Index => Proto_Index)
                         .I_Type
                     else Objects_Container.Element
                         (Container => Items_List, Index => Proto_Index)
                         .Show_Type),
                  Damage =>
                    Float
                      (BaseCargo_Container.Element
                         (Container => Base_Cargo, Index => I)
                         .Durability) /
                    Float(Default_Item_Durability),
                  Owned => 0,
                  Available =>
                    BaseCargo_Container.Element
                      (Container => Base_Cargo, Index => I)
                      .Amount,
                  Id => I));
         end if;
      end loop Add_Base_Items_Loop;
      Sort_Items.Sort(Container => Local_Items);
      Fill_Items_Indexes_Base_Loop :
      for Item of Local_Items loop
         Items_Indexes.Append(New_Item => Item.Id);
      end loop Fill_Items_Indexes_Base_Loop;
      return
        Show_Loot_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => CArgv.Empty & "ShowLoot" & "All");
   end Sort_Items_Command;

   procedure Add_Commands is
   begin
      Add_Command(Name => "ShowLoot", Ada_Command => Show_Loot_Command'Access);
      Add_Command
        (Name => "ShowLootItemInfo",
         Ada_Command => Show_Loot_Item_Info_Command'Access);
      Add_Command(Name => "LootItem", Ada_Command => Loot_Item_Command'Access);
      Add_Command
        (Name => "ShowLootItemMenu",
         Ada_Command => Show_Item_Menu_Command'Access);
      Add_Command
        (Name => "LootAmount", Ada_Command => Loot_Amount_Command'Access);
      Add_Command
        (Name => "SortLootItems", Ada_Command => Sort_Items_Command'Access);
   end Add_Commands;

end Bases.LootUI;
