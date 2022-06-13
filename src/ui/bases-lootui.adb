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
      Label: constant Ttk_Label :=
        Get_Widget
          (pathName => Loot_Canvas & ".loot.options.typelabel",
           Interp => Interp);
      Item_Name, Item_Type: Bounded_String;
      Item_Durability: Unbounded_String;
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
            Append(Source => Items_Types, New_Item => " {" & To_String(Source => Item_Type) & "}");
         end if;
         if Argc = 2 and then CArgv.Arg(Argv => Argv, N => 1) /= "All"
           and then To_String(Source => Item_Type) /= CArgv.Arg(Argv => Argv, N => 1) then
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
            Command => "ShowLootItemMenu -" &
            Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
            Column => 1);
         Add_Button
           (Table => Loot_Table, Text => To_String(Source => Item_Type),
            Tooltip => "Show available options for item",
            Command => "ShowLootItemMenu -" &
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
                (Source => Get_Item_Damage
                   (Item_Durability => BaseCargo_Container.Element
                      (Container => Base_Cargo, Index => I)
                      .Durability))
            else To_Unbounded_String(Source => "Unused"));
         Add_Progress_Bar
           (Table => Loot_Table,
            Value => BaseCargo_Container.Element
              (Container => Base_Cargo, Index => Items_Indexes(I))
              .Durability,
            Max_Value => Default_Item_Durability, Tooltip => To_String(Source => Item_Durability),
            Command => "ShowLootItemMenu -" &
            Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
            Column => 3);
         Add_Button
           (Table => Loot_Table, Text => "0", Tooltip => "Show available options for item",
            Command => "ShowLootItemMenu -" &
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
            Command => "ShowLootItemMenu -" &
            Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
            Column => 5, New_Row => True);
         <<End_Of_Base_Cargo_Loop>>
      end loop Add_Base_Cargo_Loop;
      if Page > 1 then
         if Loot_Table.Row < Game_Settings.Lists_Limit + 1 then
            Add_Pagination
              (Table => Loot_Table, Previous_Command => "ShowLoot " & Arguments & Positive'Image(Page - 1),
               Next_Command => "");
         else
            Add_Pagination
              (Table => Loot_Table, Previous_Command => "ShowLoot " & Arguments & Positive'Image(Page - 1),
               Next_Command => "ShowLoot " & Arguments & Positive'Image(Page + 1));
         end if;
      elsif Loot_Table.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (Table => Loot_Table, Previous_Command => "",
            Next_Command => "ShowLoot " & Arguments & Positive'Image(Page + 1));
      end if;
      Update_Table(Table => Loot_Table);
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Loot_Table.Canvas,
         options => "-scrollregion [list " & BBox(CanvasWidget => Loot_Table.Canvas, TagOrId => "all") & "]");
      configure(Widgt => Combo_Box, options => "-values [list " & To_String(Source => Items_Types) & "]");
      if Argc = 1 then
         Current(ComboBox => Combo_Box, NewIndex => "0");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Slave => Close_Button, Options => "-row 0 -column 1");
      configure
        (Widgt => Loot_Canvas,
         options => "-height [expr " & SashPos(Paned => Main_Paned, Index => "0") & " - 20] -width " &
         cget(Widgt => Main_Paned, option => "-width"));
      Tcl_Eval(interp => Get_Context, strng => "update");
      Canvas_Create
        (Parent => Loot_Canvas, Child_Type => "window", Options => "0 0 -anchor nw -window " & Loot_Frame);
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Loot_Canvas, options => "-scrollregion [list " & BBox(CanvasWidget => Loot_Canvas, TagOrId => "all") & "]");
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
        (1 => Weapon_Type, 2 => Chest_Armor, 3 => Head_Armor, 4 => Arms_Armor, 5 => Legs_Armor,
         6 => Shield_Type);
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
             (Container => Sky_Bases(Base_Index).Cargo, Index => Base_Cargo_Index)
             .Proto_Index);
      Append
        (Item_Info,
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
           (Item_Info,
            LF & "Skill: " &
            To_String
              (SkillsData_Container.Element
                 (Skills_List,
                  Skills_Amount_Range
                    (Objects_Container.Element
                       (Container => Items_List, Index => Proto_Index)
                       .Value
                       (3)))
                 .Name) &
            "/" &
            To_String
              (AttributesData_Container.Element
                 (Attributes_List,
                  SkillsData_Container.Element
                    (Skills_List,
                     Skills_Amount_Range
                       (Objects_Container.Element
                          (Container => Items_List, Index => Proto_Index)
                          .Value
                          (3)))
                    .Attribute)
                 .Name));
         if Objects_Container.Element
             (Container => Items_List, Index => Proto_Index)
             .Value
             (4) =
           1 then
            Append(Item_Info, LF & "Can be used with shield.");
         else
            Append
              (Item_Info,
               LF & "Can't be used with shield (two-handed weapon).");
         end if;
         Append(Item_Info, LF & "Damage type: ");
         case Objects_Container.Element
           (Container => Items_List, Index => Proto_Index)
           .Value
           (5) is
            when 1 =>
               Append(Item_Info, "cutting");
            when 2 =>
               Append(Item_Info, "impaling");
            when 3 =>
               Append(Item_Info, "blunt");
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
              (Item_Info,
               LF & "Damage chance: " &
               Get_Item_Chance_To_Damage
                 (Objects_Container.Element
                    (Container => Items_List, Index => Proto_Index)
                    .Value
                    (1)));
            Append
              (Item_Info,
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
           (Item_Info,
            LF & "Damage chance: " &
            Get_Item_Chance_To_Damage
              (Objects_Container.Element
                 (Container => Items_List, Index => Proto_Index)
                 .Value
                 (1)));
      end if;
      if Length
          (Objects_Container.Element
             (Container => Items_List, Index => Proto_Index)
             .I_Type) >
        4
        and then
        (Slice
           (Objects_Container.Element
              (Container => Items_List, Index => Proto_Index)
              .I_Type,
            1, 4) =
         "Ammo" or
         Objects_Container.Element
             (Container => Items_List, Index => Proto_Index)
             .I_Type =
           To_Bounded_String("Harpoon")) then
         Append
           (Item_Info,
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
           (Item_Info,
            LF & LF &
            To_String
              (Objects_Container.Element
                 (Container => Items_List, Index => Proto_Index)
                 .Description));
      end if;
      Show_Info
        (Text => To_String(Item_Info),
         Title =>
           To_String
             (Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .Name));
      return TCL_OK;
   end Show_Loot_Item_Info_Command;

   -- ****o* LUI/LUI.Loot_Item_Command
   -- FUNCTION
   -- Take or drop the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- LootItem actiontype
   -- actiontype can be: drop, dropall, take, takeall
   -- SOURCE
   function Loot_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Loot_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      BaseCargoIndex, CargoIndex: Natural := 0;
      Amount: Natural;
      ProtoIndex: Objects_Container.Extended_Index;
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".itemdialog.amount", Interp);
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget(Main_Paned & ".lootframe.canvas.loot.options.type", Interp);
   begin
      if Item_Index < 0 then
         BaseCargoIndex := abs (Item_Index);
      else
         CargoIndex := Item_Index;
      end if;
      if CargoIndex > 0 then
         ProtoIndex :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => CargoIndex)
             .Proto_Index;
         if BaseCargoIndex = 0 then
            BaseCargoIndex := Find_Base_Cargo(ProtoIndex);
         end if;
      else
         ProtoIndex :=
           BaseCargo_Container.Element
             (Container => Sky_Bases(BaseIndex).Cargo, Index => BaseCargoIndex)
             .Proto_Index;
      end if;
      if CArgv.Arg(Argv, 1) in "drop" | "dropall" then
         Amount :=
           (if CArgv.Arg(Argv, 1) = "drop" then Positive'Value(Get(AmountBox))
            else Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => CargoIndex)
                .Amount);
         if BaseCargoIndex > 0 then
            Update_Base_Cargo
              (Cargo_Index => BaseCargoIndex, Amount => Amount,
               Durability =>
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => CargoIndex)
                   .Durability);
         else
            Update_Base_Cargo
              (ProtoIndex, Amount,
               Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => CargoIndex)
                 .Durability);
         end if;
         Update_Cargo
           (Ship => Player_Ship, Cargo_Index => CargoIndex,
            Amount => (0 - Amount),
            Durability =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => CargoIndex)
                .Durability);
         Add_Message
           ("You drop" & Positive'Image(Amount) & " " &
            To_String
              (Objects_Container.Element
                 (Container => Items_List, Index => ProtoIndex)
                 .Name) &
            ".",
            ORDERMESSAGE);
      else
         Amount :=
           (if CArgv.Arg(Argv, 1) = "take" then Positive'Value(Get(AmountBox))
            else BaseCargo_Container.Element
                (Container => Sky_Bases(BaseIndex).Cargo,
                 Index => BaseCargoIndex)
                .Amount);
         if Free_Cargo
             (0 -
              (Amount *
               Objects_Container.Element
                 (Container => Items_List, Index => ProtoIndex)
                 .Weight)) <
           0 then
            Show_Message
              (Text =>
                 "You can't take that much " &
                 To_String
                   (Objects_Container.Element
                      (Container => Items_List, Index => ProtoIndex)
                      .Name) &
                 ".",
               Title => "Too much taken");
            return TCL_OK;
         end if;
         if CargoIndex > 0 then
            Update_Cargo
              (Ship => Player_Ship, Cargo_Index => CargoIndex,
               Amount => Amount,
               Durability =>
                 BaseCargo_Container.Element
                   (Container => Sky_Bases(BaseIndex).Cargo,
                    Index => BaseCargoIndex)
                   .Durability);
         else
            Update_Cargo
              (Player_Ship, ProtoIndex, Amount,
               BaseCargo_Container.Element
                 (Container => Sky_Bases(BaseIndex).Cargo,
                  Index => BaseCargoIndex)
                 .Durability);
         end if;
         Update_Base_Cargo
           (Cargo_Index => BaseCargoIndex, Amount => (0 - Amount),
            Durability =>
              BaseCargo_Container.Element
                (Container => Sky_Bases(BaseIndex).Cargo,
                 Index => BaseCargoIndex)
                .Durability);
         Add_Message
           ("You took" & Positive'Image(Amount) & " " &
            To_String
              (Objects_Container.Element
                 (Container => Items_List, Index => ProtoIndex)
                 .Name) &
            ".",
            ORDERMESSAGE);
      end if;
      if CArgv.Arg(Argv, 1) in "take" | "drop" then
         if Close_Dialog_Command
             (ClientData, Interp, 2,
              CArgv.Empty & "CloseDialog" & ".itemdialog") =
           TCL_ERROR then
            return TCL_ERROR;
         end if;
      end if;
      Update_Header;
      Update_Messages;
      return
        Show_Loot_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowLoot" & Get(TypeBox));
   end Loot_Item_Command;

   -- ****o* LUI/LUI.Show_Module_Menu_Command
   -- FUNCTION
   -- Show menu with actions for the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLootItemMenu itemindex
   -- ItemIndex is a index of the item which menu will be shown.
   -- SOURCE
   function Show_Item_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Item_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      BaseCargoIndex, CargoIndex: Natural := 0;
      BaseIndex: constant Natural :=
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
      Item_Index := Integer'Value(CArgv.Arg(Argv, 1));
      if Item_Index < 0 then
         BaseCargoIndex := abs (Item_Index);
         Change_Title
           (Item_Menu,
            To_String
              (Objects_Container.Element
                 (Container => Items_List,
                  Index =>
                    BaseCargo_Container.Element
                      (Container => Sky_Bases(BaseIndex).Cargo,
                       Index => BaseCargoIndex)
                      .Proto_Index)
                 .Name) &
            " actions");
      else
         CargoIndex := Item_Index;
         Change_Title
           (Item_Menu,
            Get_Item_Name
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => CargoIndex),
               False, False) &
            " actions");
      end if;
      if CargoIndex > 0 and then BaseCargoIndex = 0 then
         BaseCargoIndex :=
           Find_Base_Cargo
             (Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => CargoIndex)
                .Proto_Index);
      end if;
      if BaseCargoIndex > 0 then
         Can_Take := True;
         Add_Button
           (Name => ".take", Label => "Take selected amount",
            Command =>
              "LootAmount take" &
              Natural'Image
                (BaseCargo_Container.Element
                   (Container => Sky_Bases(BaseIndex).Cargo,
                    Index => BaseCargoIndex)
                   .Amount));
         Add_Button
           (Name => ".takeall", Label => "Take all available",
            Command => "LootItem takeall");
      end if;
      if CargoIndex > 0 then
         Can_Drop := True;
         Add_Button
           (Name => ".drop", Label => "Drop selected amount",
            Command =>
              "LootAmount drop" &
              Natural'Image
                (Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => CargoIndex)
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
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- LootAmount action baseindex
   -- Action which will be taken. Can be take or drop. BaseIndex is the index
   -- of the base from which item will be take.
   -- SOURCE
   function Loot_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Loot_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
   begin
      if CArgv.Arg(Argv, 1) = "drop" then
         Show_Manipulate_Item
           ("Drop " &
            Get_Item_Name
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => Item_Index)),
            "LootItem drop", "drop", Item_Index);
      else
         if Item_Index > 0 then
            Show_Manipulate_Item
              ("Take " &
               Get_Item_Name
                 (Inventory_Container.Element
                    (Container => Player_Ship.Cargo, Index => Item_Index)),
               "LootItem take", "take", Item_Index,
               Natural'Value(CArgv.Arg(Argv, 2)));
         else
            Show_Manipulate_Item
              ("Take " &
               To_String
                 (Objects_Container.Element
                    (Container => Items_List,
                     Index =>
                       BaseCargo_Container.Element
                         (Container => Sky_Bases(BaseIndex).Cargo,
                          Index => abs (Item_Index))
                         .Proto_Index)
                    .Name),
               "LootItem take", "take", abs (Item_Index),
               Natural'Value(CArgv.Arg(Argv, 2)));
         end if;
      end if;
      return TCL_OK;
   end Loot_Amount_Command;

   -- ****o* LUI/LUI.Sort_Items_Command
   -- FUNCTION
   -- Sort the looting list
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortLootItems x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Items_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Items_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number(Loot_Table, Natural'Value(CArgv.Arg(Argv, 1)));
      type Local_Item_Data is record
         Name: Unbounded_String;
         IType: Bounded_String;
         Damage: Float;
         Owned: Natural;
         Available: Natural;
         Id: Positive;
      end record;
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Indexes_List: Positive_Container.Vector;
      BaseCargo: BaseCargo_Container.Vector
        (Capacity =>
           BaseCargo_Container.Length
             (Container => Sky_Bases(BaseIndex).Cargo));
      BaseCargoIndex: Natural;
      ProtoIndex: Objects_Container.Extended_Index;
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
         if Items_Sort_Order = TYPEASC and then Left.IType < Right.IType then
            return True;
         end if;
         if Items_Sort_Order = TYPEDESC and then Left.IType > Right.IType then
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
        (Target => BaseCargo, Source => Sky_Bases(BaseIndex).Cargo);
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
      for I in
        Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
          Inventory_Container.Last_Index(Container => Player_Ship.Cargo) loop
         ProtoIndex :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => I)
             .Proto_Index;
         BaseCargoIndex :=
           Find_Base_Cargo
             (ProtoIndex,
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I)
                .Durability);
         if BaseCargoIndex > 0 then
            Indexes_List.Append(New_Item => BaseCargoIndex);
         end if;
         Local_Items.Append
           (New_Item =>
              (Name =>
                 To_Unbounded_String
                   (Get_Item_Name
                      (Inventory_Container.Element
                         (Container => Player_Ship.Cargo, Index => I))),
               IType =>
                 (if
                    Objects_Container.Element
                      (Container => Items_List, Index => ProtoIndex)
                      .Show_Type =
                    Null_Bounded_String
                  then
                    Objects_Container.Element
                      (Container => Items_List, Index => ProtoIndex)
                      .I_Type
                  else Objects_Container.Element
                      (Container => Items_List, Index => ProtoIndex)
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
                 (if BaseCargoIndex > 0 then
                    BaseCargo_Container.Element
                      (Container => BaseCargo, Index => BaseCargoIndex)
                      .Amount
                  else 0),
               Id => I));
      end loop;
      Sort_Items.Sort(Local_Items);
      Items_Indexes.Clear;
      for Item of Local_Items loop
         Items_Indexes.Append(Item.Id);
      end loop;
      Items_Indexes.Append(0);
      Local_Items.Clear;
      for I in
        BaseCargo_Container.First_Index(Container => BaseCargo) ..
          BaseCargo_Container.Last_Index(Container => BaseCargo) loop
         if Indexes_List.Find_Index(Item => I) = 0 then
            ProtoIndex :=
              BaseCargo_Container.Element(Container => BaseCargo, Index => I)
                .Proto_Index;
            Local_Items.Append
              (New_Item =>
                 (Name =>
                    To_Unbounded_String
                      (Source =>
                         To_String
                           (Source =>
                              Objects_Container.Element
                                (Container => Items_List, Index => ProtoIndex)
                                .Name)),
                  IType =>
                    (if
                       Objects_Container.Element
                         (Container => Items_List, Index => ProtoIndex)
                         .Show_Type =
                       Null_Bounded_String
                     then
                       Objects_Container.Element
                         (Container => Items_List, Index => ProtoIndex)
                         .I_Type
                     else Objects_Container.Element
                         (Container => Items_List, Index => ProtoIndex)
                         .Show_Type),
                  Damage =>
                    Float
                      (BaseCargo_Container.Element
                         (Container => BaseCargo, Index => I)
                         .Durability) /
                    Float(Default_Item_Durability),
                  Owned => 0,
                  Available =>
                    BaseCargo_Container.Element
                      (Container => BaseCargo, Index => I)
                      .Amount,
                  Id => I));
         end if;
      end loop;
      Sort_Items.Sort(Local_Items);
      for Item of Local_Items loop
         Items_Indexes.Append(Item.Id);
      end loop;
      return
        Show_Loot_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowLoot" & "All");
   end Sort_Items_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowLoot", Show_Loot_Command'Access);
      Add_Command("ShowLootItemInfo", Show_Loot_Item_Info_Command'Access);
      Add_Command("LootItem", Loot_Item_Command'Access);
      Add_Command("ShowLootItemMenu", Show_Item_Menu_Command'Access);
      Add_Command("LootAmount", Loot_Amount_Command'Access);
      Add_Command("SortLootItems", Sort_Items_Command'Access);
   end Add_Commands;

end Bases.LootUI;
