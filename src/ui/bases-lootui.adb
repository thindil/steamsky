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

   -- ****iv* LUI/LUI.LootTable
   -- FUNCTION
   -- Table with info about the available items to loot
   -- SOURCE
   LootTable: Table_Widget (5);
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
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLoot
   -- SOURCE
   function Show_Loot_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Loot_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      LootFrame: Ttk_Frame := Get_Widget(Main_Paned & ".lootframe", Interp);
      LootCanvas: constant Tk_Canvas :=
        Get_Widget(LootFrame & ".canvas", Interp);
      Label: constant Ttk_Label :=
        Get_Widget(LootCanvas & ".loot.options.typelabel", Interp);
      ItemDurability, ItemType, ItemName: Unbounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      ComboBox: Ttk_ComboBox;
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      BaseCargo: BaseCargo_Container.Vector(Capacity => 16);
      BaseCargoIndex, BaseAmount: Natural;
      IndexesList: Positive_Container.Vector;
      Page: constant Positive :=
        (if Argc = 3 then Positive'Value(CArgv.Arg(Argv, 2)) else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      Arguments: constant String :=
        (if Argc > 1 then "{" & CArgv.Arg(Argv, 1) & "}" else "All");
      Current_Item_Index: Positive := 1;
      ProtoIndex: Tiny_String.Bounded_String;
   begin
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "loot.tcl");
         Bind(LootFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         LootFrame := Get_Widget(LootCanvas & ".loot");
         LootTable :=
           Create_Table
             (Widget_Image(LootFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Type"),
               To_Unbounded_String("Durability"), To_Unbounded_String("Owned"),
               To_Unbounded_String("Available")),
              Get_Widget(".gameframe.paned.lootframe.scrolly", Interp),
              "SortLootItems", "Press mouse button to sort the items.");
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         Show_Sky_Map(True);
         return TCL_OK;
      end if;
      LootFrame.Name := New_String(LootCanvas & ".loot");
      ComboBox := Get_Widget(LootFrame & ".options.type", Interp);
      BaseCargo_Container.Assign(Target => BaseCargo, Source => Sky_Bases(BaseIndex).Cargo);
      if Items_Sort_Order = Default_Items_Sort_Order then
         Items_Indexes.Clear;
         for I in
           Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
             Inventory_Container.Last_Index
               (Container => Player_Ship.Cargo) loop
            Items_Indexes.Append(I);
         end loop;
         Items_Indexes.Append(0);
         for I in BaseCargo_Container.First_Index(Container => BaseCargo) .. BaseCargo_Container.Last_Index(Container => BaseCargo) loop
            Items_Indexes.Append(I);
         end loop;
      end if;
      Clear_Table(LootTable);
      Add_Player_Cargo_Loop :
      for I of Items_Indexes loop
         Current_Item_Index := Current_Item_Index + 1;
         exit Add_Player_Cargo_Loop when I = 0;
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
            IndexesList.Append(New_Item => BaseCargoIndex);
         end if;
         ItemType :=
           (if Items_List(ProtoIndex).Show_Type = Null_Unbounded_String then
              Items_List(ProtoIndex).I_Type
            else Items_List(ProtoIndex).Show_Type);
         if Index(ItemsTypes, To_String("{" & ItemType & "}")) = 0 then
            Append(ItemsTypes, " {" & ItemType & "}");
         end if;
         if Argc > 1 and then CArgv.Arg(Argv, 1) /= "All"
           and then To_String(ItemType) /= CArgv.Arg(Argv, 1) then
            goto End_Of_Cargo_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Cargo_Loop;
         end if;
         ItemName :=
           To_Unbounded_String
             (Get_Item_Name
                (Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => I),
                 False, False));
         Add_Button
           (LootTable, To_String(ItemName), "Show available options for item",
            "ShowLootItemMenu" & Positive'Image(I), 1);
         Add_Button
           (LootTable, To_String(ItemType), "Show available options for item",
            "ShowLootItemMenu" & Positive'Image(I), 2);
         ItemDurability :=
           (if
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I)
                .Durability <
              100
            then
              To_Unbounded_String
                (Get_Item_Damage
                   (Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => I)
                      .Durability))
            else To_Unbounded_String("Unused"));
         Add_Progress_Bar
           (LootTable,
            Inventory_Container.Element
              (Container => Player_Ship.Cargo, Index => I)
              .Durability,
            Default_Item_Durability, To_String(ItemDurability),
            "ShowLootItemMenu" & Positive'Image(I), 3);
         Add_Button
           (LootTable,
            Natural'Image
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => I)
                 .Amount),
            "Show available options for item",
            "ShowLootItemMenu" & Positive'Image(I), 4);
         BaseAmount :=
           (if BaseCargoIndex > 0 then
              BaseCargo_Container.Element(Container => Sky_Bases(BaseIndex).Cargo, Index => BaseCargoIndex).Amount
            else 0);
         Add_Button
           (LootTable, Natural'Image(BaseAmount),
            "Show available options for item",
            "ShowLootItemMenu" & Positive'Image(I), 5, True);
         exit Add_Player_Cargo_Loop when LootTable.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Cargo_Loop>>
      end loop Add_Player_Cargo_Loop;
      Add_Base_Cargo_Loop :
      for I in Current_Item_Index .. Items_Indexes.Last_Index loop
         exit Add_Base_Cargo_Loop when LootTable.Row =
           Game_Settings.Lists_Limit + 1;
         if IndexesList.Find_Index(Item => Items_Indexes(I)) > 0 then
            goto End_Of_Base_Cargo_Loop;
         end if;
         ProtoIndex := BaseCargo_Container.Element(Container => BaseCargo, Index => Items_Indexes(I)).Proto_Index;
         ItemType :=
           (if Items_List(ProtoIndex).Show_Type = Null_Unbounded_String then
              Items_List(ProtoIndex).I_Type
            else Items_List(ProtoIndex).Show_Type);
         if Index(ItemsTypes, To_String("{" & ItemType & "}")) = 0 then
            Append(ItemsTypes, " {" & ItemType & "}");
         end if;
         if Argc = 2 and then CArgv.Arg(Argv, 1) /= "All"
           and then To_String(ItemType) /= CArgv.Arg(Argv, 1) then
            goto End_Of_Base_Cargo_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Base_Cargo_Loop;
         end if;
         ItemName := Items_List(ProtoIndex).Name;
         Add_Button
           (LootTable, To_String(ItemName), "Show available options for item",
            "ShowLootItemMenu -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            1);
         Add_Button
           (LootTable, To_String(ItemType), "Show available options for item",
            "ShowLootItemMenu -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            2);
         ItemDurability :=
           (if BaseCargo_Container.Element(Container => BaseCargo, Index => Items_Indexes(I)).Durability < 100 then
              To_Unbounded_String(Get_Item_Damage(BaseCargo_Container.Element(Container => BaseCargo, Index => I).Durability))
            else To_Unbounded_String("Unused"));
         Add_Progress_Bar
           (LootTable,  BaseCargo_Container.Element(Container => BaseCargo, Index => Items_Indexes(I)).Durability,
            Default_Item_Durability, To_String(ItemDurability),
            "ShowLootItemMenu -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            3);
         Add_Button
           (LootTable, "0", "Show available options for item",
            "ShowLootItemMenu -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            4);
         BaseAmount := BaseCargo_Container.Element(Container => Sky_Bases(BaseIndex).Cargo, Index => Items_Indexes(I)).Amount;
         Add_Button
           (LootTable, Natural'Image(BaseAmount),
            "Show available options for item",
            "ShowLootItemMenu -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            5, True);
         <<End_Of_Base_Cargo_Loop>>
      end loop Add_Base_Cargo_Loop;
      if Page > 1 then
         if LootTable.Row < Game_Settings.Lists_Limit + 1 then
            Add_Pagination
              (LootTable, "ShowLoot " & Arguments & Positive'Image(Page - 1),
               "");
         else
            Add_Pagination
              (LootTable, "ShowLoot " & Arguments & Positive'Image(Page - 1),
               "ShowLoot " & Arguments & Positive'Image(Page + 1));
         end if;
      elsif LootTable.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (LootTable, "", "ShowLoot " & Arguments & Positive'Image(Page + 1));
      end if;
      Update_Table(LootTable);
      Tcl_Eval(Get_Context, "update");
      configure
        (LootTable.Canvas,
         "-scrollregion [list " & BBox(LootTable.Canvas, "all") & "]");
      configure(ComboBox, "-values [list " & To_String(ItemsTypes) & "]");
      if Argc = 1 then
         Current(ComboBox, "0");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      configure
        (LootCanvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (LootCanvas, "window", "0 0 -anchor nw -window " & LootFrame);
      Tcl_Eval(Get_Context, "update");
      configure
        (LootCanvas, "-scrollregion [list " & BBox(LootCanvas, "all") & "]");
      Xview_Move_To(LootCanvas, "0.0");
      Yview_Move_To(LootCanvas, "0.0");
      Show_Screen("lootframe");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Loot_Command;

   -- ****if* LUI/LUI.ItemIndex
   -- FUNCTION
   -- Index of the currently selected item
   -- SOURCE
   ItemIndex: Integer;
   -- ****

   -- ****o* LUI/LUI.Show_Trade_Loot_Info_Command
   -- FUNCTION
   -- Show information about the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLootItemInfo
   -- SOURCE
   function Show_Loot_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Loot_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      use Tiny_String;

      ItemInfo: Unbounded_String;
      ProtoIndex: Bounded_String;
      CargoIndex, BaseCargoIndex: Natural := 0;
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      ItemTypes: constant array(1 .. 6) of Unbounded_String :=
        (Weapon_Type, Chest_Armor, Head_Armor, Arms_Armor, Legs_Armor,
         Shield_Type);
   begin
      if ItemIndex < 0 then
         BaseCargoIndex := abs (ItemIndex);
      else
         CargoIndex := ItemIndex;
      end if;
      if CargoIndex >
        Natural(Inventory_Container.Length(Container => Player_Ship.Cargo)) or
        BaseCargoIndex > Natural(BaseCargo_Container.Length(Container => Sky_Bases(BaseIndex).Cargo)) then
         return TCL_OK;
      end if;
      ProtoIndex :=
        (if CargoIndex > 0 then
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => CargoIndex)
             .Proto_Index
         else BaseCargo_Container.Element(Container => Sky_Bases(BaseIndex).Cargo, Index => BaseCargoIndex).Proto_Index);
      Append
        (ItemInfo,
         "Weight:" & Integer'Image(Items_List(ProtoIndex).Weight) & " kg");
      if Items_List(ProtoIndex).I_Type = Weapon_Type then
         Append
           (ItemInfo,
            LF & "Skill: " &
            To_String
              (SkillsData_Container.Element
                 (Skills_List,
                  Skills_Amount_Range(Items_List(ProtoIndex).Value.Element(3)))
                 .Name) &
            "/" &
            To_String
              (AttributesData_Container.Element
                 (Attributes_List,
                  SkillsData_Container.Element
                    (Skills_List,
                     Skills_Amount_Range
                       (Items_List(ProtoIndex).Value.Element(3)))
                    .Attribute)
                 .Name));
         if Items_List(ProtoIndex).Value(4) = 1 then
            Append(ItemInfo, LF & "Can be used with shield.");
         else
            Append
              (ItemInfo,
               LF & "Can't be used with shield (two-handed weapon).");
         end if;
         Append(ItemInfo, LF & "Damage type: ");
         case Items_List(ProtoIndex).Value(5) is
            when 1 =>
               Append(ItemInfo, "cutting");
            when 2 =>
               Append(ItemInfo, "impaling");
            when 3 =>
               Append(ItemInfo, "blunt");
            when others =>
               null;
         end case;
      end if;
      Show_Weapon_Info_Loop :
      for ItemType of ItemTypes loop
         if Items_List(ProtoIndex).I_Type = ItemType then
            Append
              (ItemInfo,
               LF & "Damage chance: " &
               Get_Item_Chance_To_Damage(Items_List(ProtoIndex).Value(1)));
            Append
              (ItemInfo,
               LF & "Strength:" &
               Integer'Image(Items_List(ProtoIndex).Value(2)));
            exit Show_Weapon_Info_Loop;
         end if;
      end loop Show_Weapon_Info_Loop;
      if Tools_List.Contains(Items_List(ProtoIndex).I_Type) then
         Append
           (ItemInfo,
            LF & "Damage chance: " &
            Get_Item_Chance_To_Damage(Items_List(ProtoIndex).Value(1)));
      end if;
      if Length(Items_List(ProtoIndex).I_Type) > 4
        and then
        (Slice(Items_List(ProtoIndex).I_Type, 1, 4) = "Ammo" or
         Items_List(ProtoIndex).I_Type = To_Unbounded_String("Harpoon")) then
         Append
           (ItemInfo,
            LF & "Strength:" & Integer'Image(Items_List(ProtoIndex).Value(1)));
      end if;
      if Items_List(ProtoIndex).Description /= Null_Unbounded_String then
         Append
           (ItemInfo, LF & LF & To_String(Items_List(ProtoIndex).Description));
      end if;
      Show_Info
        (Text => To_String(ItemInfo),
         Title => To_String(Items_List(ProtoIndex).Name));
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
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      BaseCargoIndex, CargoIndex: Natural := 0;
      Amount: Natural;
      ProtoIndex: Tiny_String.Bounded_String;
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".itemdialog.amount", Interp);
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget(Main_Paned & ".lootframe.canvas.loot.options.type", Interp);
   begin
      if ItemIndex < 0 then
         BaseCargoIndex := abs (ItemIndex);
      else
         CargoIndex := ItemIndex;
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
         ProtoIndex := BaseCargo_Container.Element(Container => Sky_Bases(BaseIndex).Cargo, Index => BaseCargoIndex).Proto_Index;
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
         UpdateCargo
           (Ship => Player_Ship, CargoIndex => CargoIndex,
            Amount => (0 - Amount),
            Durability =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => CargoIndex)
                .Durability);
         Add_Message
           ("You drop" & Positive'Image(Amount) & " " &
            To_String(Items_List(ProtoIndex).Name) & ".",
            ORDERMESSAGE);
      else
         Amount :=
           (if CArgv.Arg(Argv, 1) = "take" then Positive'Value(Get(AmountBox))
            else BaseCargo_Container.Element(Container => Sky_Bases(BaseIndex).Cargo, Index => BaseCargoIndex).Amount);
         if FreeCargo(0 - (Amount * Items_List(ProtoIndex).Weight)) < 0 then
            Show_Message
              (Text =>
                 "You can't take that much " &
                 To_String(Items_List(ProtoIndex).Name) & ".",
               Title => "Too much taken");
            return TCL_OK;
         end if;
         if CargoIndex > 0 then
            UpdateCargo
              (Ship => Player_Ship, CargoIndex => CargoIndex, Amount => Amount,
               Durability =>
                 BaseCargo_Container.Element(Container => Sky_Bases(BaseIndex).Cargo, Index => BaseCargoIndex).Durability);
         else
            UpdateCargo
              (Player_Ship, ProtoIndex, Amount,
               BaseCargo_Container.Element(Container => Sky_Bases(BaseIndex).Cargo, Index => BaseCargoIndex).Durability);
         end if;
         Update_Base_Cargo
           (Cargo_Index => BaseCargoIndex, Amount => (0 - Amount),
            Durability =>
              BaseCargo_Container.Element(Container => Sky_Bases(BaseIndex).Cargo, Index => BaseCargoIndex).Durability);
         Add_Message
           ("You took" & Positive'Image(Amount) & " " &
            To_String(Items_List(ProtoIndex).Name) & ".",
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
      ItemIndex := Integer'Value(CArgv.Arg(Argv, 1));
      if ItemIndex < 0 then
         BaseCargoIndex := abs (ItemIndex);
         Change_Title
           (Item_Menu,
            To_String
              (Items_List
                 (BaseCargo_Container.Element(Container => Sky_Bases(BaseIndex).Cargo, Index => BaseCargoIndex).Proto_Index)
                 .Name) &
            " actions");
      else
         CargoIndex := ItemIndex;
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
                (BaseCargo_Container.Element(Container => Sky_Bases(BaseIndex).Cargo, Index => BaseCargoIndex).Amount));
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
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
   begin
      if CArgv.Arg(Argv, 1) = "drop" then
         Show_Manipulate_Item
           ("Drop " &
            Get_Item_Name
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => ItemIndex)),
            "LootItem drop", "drop", ItemIndex);
      else
         if ItemIndex > 0 then
            Show_Manipulate_Item
              ("Take " &
               Get_Item_Name
                 (Inventory_Container.Element
                    (Container => Player_Ship.Cargo, Index => ItemIndex)),
               "LootItem take", "take", ItemIndex,
               Natural'Value(CArgv.Arg(Argv, 2)));
         else
            Show_Manipulate_Item
              ("Take " &
               To_String
                 (Items_List
                    (BaseCargo_Container.Element(Container => Sky_Bases(BaseIndex).Cargo, Index => abs (ItemIndex)).Proto_Index)
                    .Name),
               "LootItem take", "take", abs (ItemIndex),
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
      Column: constant Positive :=
        Get_Column_Number(LootTable, Natural'Value(CArgv.Arg(Argv, 1)));
      type Local_Item_Data is record
         Name: Unbounded_String;
         IType: Unbounded_String;
         Damage: Float;
         Owned: Natural;
         Available: Natural;
         Id: Positive;
      end record;
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Indexes_List: Positive_Container.Vector;
      BaseCargo: BaseCargo_Container.Vector(Capacity => BaseCargo_Container.Length(Container => Sky_Bases(BaseIndex).Cargo));
      BaseCargoIndex: Natural;
      ProtoIndex: Tiny_String.Bounded_String;
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
      BaseCargo_Container.Assign(Target => BaseCargo, Source => Sky_Bases(BaseIndex).Cargo);
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
                 (if Items_List(ProtoIndex).Show_Type = Null_Unbounded_String
                  then Items_List(ProtoIndex).I_Type
                  else Items_List(ProtoIndex).Show_Type),
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
                 (if BaseCargoIndex > 0 then BaseCargo(BaseCargoIndex).Amount
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
      for I in BaseCargo.First_Index .. BaseCargo.Last_Index loop
         if Indexes_List.Find_Index(Item => I) = 0 then
            ProtoIndex := BaseCargo(I).Proto_Index;
            Local_Items.Append
              (New_Item =>
                 (Name => Items_List(ProtoIndex).Name,
                  IType =>
                    (if
                       Items_List(ProtoIndex).Show_Type = Null_Unbounded_String
                     then Items_List(ProtoIndex).I_Type
                     else Items_List(ProtoIndex).Show_Type),
                  Damage =>
                    Float(BaseCargo(I).Durability) /
                    Float(Default_Item_Durability),
                  Owned => 0, Available => BaseCargo(I).Amount, Id => I));
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

   procedure AddCommands is
   begin
      Add_Command("ShowLoot", Show_Loot_Command'Access);
      Add_Command("ShowLootItemInfo", Show_Loot_Item_Info_Command'Access);
      Add_Command("LootItem", Loot_Item_Command'Access);
      Add_Command("ShowLootItemMenu", Show_Item_Menu_Command'Access);
      Add_Command("LootAmount", Loot_Amount_Command'Access);
      Add_Command("SortLootItems", Sort_Items_Command'Access);
   end AddCommands;

end Bases.LootUI;
