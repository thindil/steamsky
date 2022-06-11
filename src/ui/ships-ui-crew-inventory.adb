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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Generic_Array_Sort;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Event; use Tcl.Tk.Ada.Event;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Config; use Config;
with CoreUI; use CoreUI;
with Crew.Inventory; use Crew.Inventory;
with Dialogs; use Dialogs;
with Factions; use Factions;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Table; use Table;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Ships.UI.Crew.Inventory is

   -- ****iv* SUCI/SUCI.InventoryTable
   -- FUNCTION
   -- Table with info about the crew member inventory
   -- SOURCE
   InventoryTable: Table_Widget (5);
   -- ****

   -- ****iv* SUCI/SUCI.MemberIndex
   -- FUNCTION
   -- The index of the selected crew member
   -- SOURCE
   MemberIndex: Positive;
   -- ****

   -- ****iv* SUCI/SUCI.Inventory_Indexes
   -- FUNCTION
   -- Indexes of the crew member items in inventory
   -- SOURCE
   Inventory_Indexes: Positive_Container.Vector;
   -- ****

   -- ****o* SUCI/SUCI.Update_Inventory_Command
   -- FUNCTION
   -- Update inventory list of the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateInventory memberindex page
   -- MemberIndex is the index of the crew member to show inventory, page
   -- is a number of the page of inventory list to show
   -- SOURCE
   function Update_Inventory_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Inventory_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp);
      Member: Member_Data
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount);
      Page: constant Positive :=
        (if Argc = 3 then Positive'Value(CArgv.Arg(Argv, 2)) else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
   begin
      MemberIndex := Positive'Value(CArgv.Arg(Argv, 1));
      Member := Player_Ship.Crew(MemberIndex);
      if InventoryTable.Row > 1 then
         Clear_Table(InventoryTable);
      end if;
      if Inventory_Indexes.Length /=
        Inventory_Container.Length(Container => Member.Inventory) then
         Inventory_Indexes.Clear;
         for I in
           Inventory_Container.First_Index(Container => Member.Inventory) ..
             Inventory_Container.Last_Index(Container => Member.Inventory) loop
            Inventory_Indexes.Append(I);
         end loop;
      end if;
      Load_Inventory_Loop :
      for I of Inventory_Indexes loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         Add_Button
           (InventoryTable,
            Get_Item_Name
              (Inventory_Container.Element
                 (Container => Member.Inventory, Index => I),
               False, False),
            "Show available item's options",
            "ShowInventoryMenu " & CArgv.Arg(Argv, 1) & Positive'Image(I), 1);
         Add_Progress_Bar
           (InventoryTable,
            Inventory_Container.Element
              (Container => Member.Inventory, Index => I)
              .Durability,
            Default_Item_Durability,
            "The current durability level of the selected item.",
            "ShowInventoryMenu " & CArgv.Arg(Argv, 1) & Positive'Image(I), 2);
         if Item_Is_Used(MemberIndex, I) then
            Add_Check_Button
              (InventoryTable, "The item is used by the crew member",
               "ShowInventoryMenu " & CArgv.Arg(Argv, 1) & Positive'Image(I),
               True, 3);
         else
            Add_Check_Button
              (InventoryTable, "The item isn't used by the crew member",
               "ShowInventoryMenu " & CArgv.Arg(Argv, 1) & Positive'Image(I),
               False, 3);
         end if;
         Add_Button
           (InventoryTable,
            Positive'Image
              (Inventory_Container.Element
                 (Container => Member.Inventory, Index => I)
                 .Amount),
            "The amount of the item owned by the crew member",
            "ShowInventoryMenu " & CArgv.Arg(Argv, 1) & Positive'Image(I), 4);
         Add_Button
           (InventoryTable,
            Positive'Image
              (Inventory_Container.Element
                 (Container => Member.Inventory, Index => I)
                 .Amount *
               Objects_Container.Element
                 (Container => Items_List,
                  Index =>
                    Inventory_Container.Element
                      (Container => Member.Inventory, Index => I)
                      .Proto_Index)
                 .Weight) &
            " kg",
            "The total weight of the items",
            "ShowInventoryMenu " & CArgv.Arg(Argv, 1) & Positive'Image(I), 5,
            True);
         exit Load_Inventory_Loop when InventoryTable.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Loop>>
      end loop Load_Inventory_Loop;
      if Page > 1 then
         Add_Pagination
           (InventoryTable,
            "UpdateInventory " & CArgv.Arg(Argv, 1) & Positive'Image(Page - 1),
            (if InventoryTable.Row < Game_Settings.Lists_Limit + 1 then ""
             else "UpdateInventory " & CArgv.Arg(Argv, 1) &
               Positive'Image(Page + 1)));
      elsif InventoryTable.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (InventoryTable, "",
            "UpdateInventory " & CArgv.Arg(Argv, 1) &
            Positive'Image(Page + 1));
      end if;
      Update_Table(InventoryTable);
      return TCL_OK;
   end Update_Inventory_Command;

   -- ****it* SUCI/SUCI.Inventory_Sort_Orders
   -- FUNCTION
   -- Sorting orders for items inside various inventories
   -- OPTIONS
   -- NAMEASC        - Sort items by name ascending
   -- NAMEDESC       - Sort items by name descending
   -- DURABILITYASC  - Sort items by durability ascending
   -- DURABILITYDESC - Sort items by durability descending
   -- TYPEASC        - Sort items by type ascending
   -- TYPEDESC       - Sort items by type descending
   -- AMOUNTASC      - Sort items by amount ascending
   -- AMOUNTDESC     - Sort items by amount descending
   -- WEIGHTASC      - Sort items by total weight ascending
   -- WEIGHTDESC     - Sort items by total weight descending
   -- USEDASC        - Sort items by use status (mobs inventory only) ascending
   -- USEDDESC       - Sort items by use status (mobs inventory only) descending
   -- NONE           - No sorting items (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Inventory_Sort_Orders is
     (NAMEASC, NAMEDESC, DURABILITYASC, DURABILITYDESC, TYPEASC, TYPEDESC,
      AMOUNTASC, AMOUNTDESC, WEIGHTASC, WEIGHTDESC, USEDASC, USEDDESC,
      NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* SUCI/SUCI.Default_Inventory_Sort_Order
      -- FUNCTION
      -- Default sorting order for items in various inventories
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Inventory_Sort_Order: constant Inventory_Sort_Orders := NONE;
   -- ****

   -- ****iv* SUCI/SUCI.Inventory_Sort_Order
   -- FUNCTION
   -- The current sorting order of items in various inventories
   -- SOURCE
   Inventory_Sort_Order: Inventory_Sort_Orders := Default_Inventory_Sort_Order;
   -- ****

   -- ****o* SUCI/SUCI.Sort_Crew_Inventory_Command
   -- FUNCTION
   -- Sort the selected crew member inventory
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortCrewInventory x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Crew_Inventory_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Crew_Inventory_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      Column: constant Positive :=
        (if CArgv.Arg(Argv, 1) = "-1" then Positive'Last
         else Get_Column_Number
             (InventoryTable, Natural'Value(CArgv.Arg(Argv, 1))));
      type Local_Item_Data is record
         Name: Unbounded_String;
         Damage: Float;
         Item_Type: Bounded_String;
         Amount: Positive;
         Weight: Positive;
         Used: Boolean;
         Id: Positive;
      end record;
      type Inventory_Array is array(Positive range <>) of Local_Item_Data;
      Local_Inventory: Inventory_Array
        (1 ..
             Positive
               (Inventory_Container.Length
                  (Container => Player_Ship.Crew(MemberIndex).Inventory)));
      function "<"(Left, Right: Local_Item_Data) return Boolean is
      begin
         if Inventory_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Inventory_Sort_Order = NAMEDESC
           and then Left.Name > Right.Name then
            return True;
         end if;
         if Inventory_Sort_Order = DURABILITYASC
           and then Left.Damage < Right.Damage then
            return True;
         end if;
         if Inventory_Sort_Order = DURABILITYDESC
           and then Left.Damage > Right.Damage then
            return True;
         end if;
         if Inventory_Sort_Order = TYPEASC
           and then Left.Item_Type < Right.Item_Type then
            return True;
         end if;
         if Inventory_Sort_Order = TYPEDESC
           and then Left.Item_Type > Right.Item_Type then
            return True;
         end if;
         if Inventory_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Inventory_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         if Inventory_Sort_Order = WEIGHTASC
           and then Left.Weight < Right.Weight then
            return True;
         end if;
         if Inventory_Sort_Order = WEIGHTDESC
           and then Left.Weight > Right.Weight then
            return True;
         end if;
         if Inventory_Sort_Order = USEDASC and then Left.Used < Right.Used then
            return True;
         end if;
         if Inventory_Sort_Order = USEDDESC
           and then Left.Used > Right.Used then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Inventory is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Item_Data,
         Array_Type => Inventory_Array);
   begin
      case Column is
         when 1 =>
            if Inventory_Sort_Order = NAMEASC then
               Inventory_Sort_Order := NAMEDESC;
            else
               Inventory_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Inventory_Sort_Order = DURABILITYASC then
               Inventory_Sort_Order := DURABILITYDESC;
            else
               Inventory_Sort_Order := DURABILITYASC;
            end if;
         when 3 =>
            if Inventory_Sort_Order = USEDASC then
               Inventory_Sort_Order := USEDDESC;
            else
               Inventory_Sort_Order := USEDASC;
            end if;
         when 4 =>
            if Inventory_Sort_Order = AMOUNTASC then
               Inventory_Sort_Order := AMOUNTDESC;
            else
               Inventory_Sort_Order := AMOUNTASC;
            end if;
         when 5 =>
            if Inventory_Sort_Order = WEIGHTASC then
               Inventory_Sort_Order := WEIGHTDESC;
            else
               Inventory_Sort_Order := WEIGHTASC;
            end if;
         when others =>
            null;
      end case;
      if Inventory_Sort_Order = NONE then
         return
           Update_Inventory_Command
             (ClientData, Interp, 2,
              CArgv.Empty & "UpdateInventory" &
              Trim(Positive'Image(MemberIndex), Left));
      end if;
      for I in
        Inventory_Container.First_Index
          (Container => Player_Ship.Crew(MemberIndex).Inventory) ..
          Inventory_Container.Last_Index
            (Container => Player_Ship.Crew(MemberIndex).Inventory) loop
         Local_Inventory(I) :=
           (Name =>
              To_Unbounded_String
                (Get_Item_Name
                   (Inventory_Container.Element
                      (Container => Player_Ship.Crew(MemberIndex).Inventory,
                       Index => I),
                    False, False)),
            Damage =>
              Float
                (Inventory_Container.Element
                   (Container => Player_Ship.Crew(MemberIndex).Inventory,
                    Index => I)
                   .Durability) /
              Float(Default_Item_Durability),
            Item_Type =>
              (if
                 Objects_Container.Element
                   (Container => Items_List,
                    Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Crew(MemberIndex).Inventory,
                         Index => I)
                        .Proto_Index)
                   .Show_Type /=
                 Null_Bounded_String
               then
                 Objects_Container.Element
                   (Container => Items_List,
                    Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Crew(MemberIndex).Inventory,
                         Index => I)
                        .Proto_Index)
                   .Show_Type
               else Objects_Container.Element
                   (Container => Items_List,
                    Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Crew(MemberIndex).Inventory,
                         Index => I)
                        .Proto_Index)
                   .I_Type),
            Amount =>
              Inventory_Container.Element
                (Container => Player_Ship.Crew(MemberIndex).Inventory,
                 Index => I)
                .Amount,
            Weight =>
              Inventory_Container.Element
                (Container => Player_Ship.Crew(MemberIndex).Inventory,
                 Index => I)
                .Amount *
              Objects_Container.Element
                (Container => Items_List,
                 Index =>
                   Inventory_Container.Element
                     (Container => Player_Ship.Crew(MemberIndex).Inventory,
                      Index => I)
                     .Proto_Index)
                .Weight,
            Used => Item_Is_Used(MemberIndex, I), Id => I);
      end loop;
      Sort_Inventory(Local_Inventory);
      Inventory_Indexes.Clear;
      for Item of Local_Inventory loop
         Inventory_Indexes.Append(Item.Id);
      end loop;
      return
        Update_Inventory_Command
          (ClientData, Interp, 2,
           CArgv.Empty & "UpdateInventory" &
           Trim(Positive'Image(MemberIndex), Left));
   end Sort_Crew_Inventory_Command;

   -- ****o* SUCI/SUCI.Show_Member_Inventory_Command
   -- FUNCTION
   -- Show inventory of the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberInventory memberindex
   -- MemberIndex is the index of the crew member to show inventory
   -- SOURCE
   function Show_Member_Inventory_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Inventory_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      Member_Index: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      MemberDialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".memberdialog",
           Title =>
             "Inventory of " & To_String(Player_Ship.Crew(Member_Index).Name),
           Columns => 2);
      YScroll: constant Ttk_Scrollbar :=
        Create
          (MemberDialog & ".yscroll",
           "-orient vertical -command [list .memberdialog.canvas yview]");
      MemberCanvas: constant Tk_Canvas :=
        Create
          (MemberDialog & ".canvas",
           "-yscrollcommand [list " & YScroll & " set]");
      MemberFrame: constant Ttk_Frame := Create(MemberCanvas & ".frame");
      Height, Width: Positive := 10;
      FreeSpaceLabel: constant Ttk_Label :=
        Create
          (MemberFrame & ".freespace",
           "-text {Free inventory space:" &
           Integer'Image
             (Free_Inventory(Positive'Value(CArgv.Arg(Argv, 1)), 0)) &
           " kg} -wraplength 400");
      Close_Button: constant Ttk_Button :=
        Create
          (MemberDialog & ".button",
           "-text Close -command {CloseDialog " & MemberDialog & "}");
   begin
      if Inventory_Container.Length
          (Container => Player_Ship.Crew(Member_Index).Inventory) =
        0 then
         Tcl_Eval(Interp, "CloseDialog .memberdialog");
         Show_Message
           (Text =>
              To_String(Player_Ship.Crew(Member_Index).Name) &
              " doesn't own any items.",
            Title =>
              "Inventory of " &
              To_String(Player_Ship.Crew(Member_Index).Name));
         return TCL_OK;
      end if;
      Tcl.Tk.Ada.Grid.Grid(MemberCanvas, "-padx 5 -pady 5");
      Tcl.Tk.Ada.Grid.Grid
        (YScroll, "-row 1 -column 1 -padx 5 -pady 5 -sticky ns");
      Autoscroll(YScroll);
      Tcl.Tk.Ada.Grid.Grid(FreeSpaceLabel);
      Height :=
        Height + Positive'Value(Winfo_Get(FreeSpaceLabel, "reqheight"));
      InventoryTable :=
        Create_Table
          (Widget_Image(MemberFrame),
           (To_Unbounded_String("Name"), To_Unbounded_String("Durability"),
            To_Unbounded_String("Used"), To_Unbounded_String("Amount"),
            To_Unbounded_String("Weight")),
           YScroll, "SortCrewInventory",
           "Press mouse button to sort the inventory.");
      if Update_Inventory_Command(ClientData, Interp, Argc, Argv) =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
      Height :=
        Height + Positive'Value(Winfo_Get(InventoryTable.Canvas, "reqheight"));
      Width := Positive'Value(Winfo_Get(InventoryTable.Canvas, "reqwidth"));
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-pady 5");
      Widgets.Focus(InventoryTable.Canvas);
      Bind
        (Close_Button, "<Tab>", "{focus " & InventoryTable.Canvas & ";break}");
      Bind(Close_Button, "<Escape>", "{" & Close_Button & " invoke;break}");
      Bind
        (InventoryTable.Canvas, "<Escape>",
         "{" & Close_Button & " invoke;break}");
      if Height > 500 then
         Height := 500;
      end if;
      configure
        (MemberFrame,
         "-height" & Positive'Image(Height) & " -width" &
         Positive'Image(Width));
      configure
        (MemberCanvas,
         "-height" & Positive'Image(Height) & " -width" &
         Positive'Image(Width + 15));
      Canvas_Create
        (MemberCanvas, "window", "0 0 -anchor nw -window " & MemberFrame);
      Tcl_Eval(Interp, "update");
      configure
        (MemberCanvas,
         "-scrollregion [list " & BBox(MemberCanvas, "all") & "]");
      Show_Dialog
        (Dialog => MemberDialog, Relative_X => 0.2, Relative_Y => 0.2);
      return TCL_OK;
   end Show_Member_Inventory_Command;

   -- ****o* SUCI/SUCI.Set_Use_Item_Command
   -- FUNCTION
   -- Set if item is used by a crew member or not
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetUseItem memberindex itemindex
   -- Memberindex is the index of the crew member in which inventory item will
   -- be set, itemindex is the index of the item which will be set
   -- SOURCE
   function Set_Use_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Use_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 2));
      ItemType: constant Bounded_String :=
        Objects_Container.Element
          (Container => Items_List,
           Index =>
             Inventory_Container.Element
               (Container => Player_Ship.Crew(MemberIndex).Inventory,
                Index => ItemIndex)
               .Proto_Index)
          .I_Type;
   begin
      if Item_Is_Used(MemberIndex, ItemIndex) then
         Take_Off_Item(MemberIndex, ItemIndex);
         return
           Sort_Crew_Inventory_Command
             (ClientData, Interp, 2, CArgv.Empty & "SortCrewInventory" & "-1");
      end if;
      if ItemType = Weapon_Type then
         if Objects_Container.Element
             (Container => Items_List,
              Index =>
                Inventory_Container.Element
                  (Container => Player_Ship.Crew(MemberIndex).Inventory,
                   Index => ItemIndex)
                  .Proto_Index)
             .Value
             (4) =
           2 and
           Player_Ship.Crew(MemberIndex).Equipment(SHIELD) /= 0 then
            Show_Message
              (Text =>
                 To_String(Player_Ship.Crew(MemberIndex).Name) &
                 " can't use this weapon because have shield equiped. Take off shield first.",
               Title => "Shield in use");
            return TCL_OK;
         end if;
         Player_Ship.Crew(MemberIndex).Equipment(WEAPON) := ItemIndex;
      elsif ItemType = Shield_Type then
         if Player_Ship.Crew(MemberIndex).Equipment(WEAPON) > 0 then
            if Objects_Container.Element
                (Container => Items_List,
                 Index =>
                   Inventory_Container.Element
                     (Container => Player_Ship.Crew(MemberIndex).Inventory,
                      Index => Player_Ship.Crew(MemberIndex).Equipment(WEAPON))
                     .Proto_Index)
                .Value
                (4) =
              2 then
               Show_Message
                 (Text =>
                    To_String(Player_Ship.Crew(MemberIndex).Name) &
                    " can't use shield because have equiped two-hand weapon. Take off weapon first.",
                  Title => "Two handed weapon in use");
               return TCL_OK;
            end if;
         end if;
         Player_Ship.Crew(MemberIndex).Equipment(SHIELD) := ItemIndex;
      elsif ItemType = Head_Armor then
         Player_Ship.Crew(MemberIndex).Equipment(HELMET) := ItemIndex;
      elsif ItemType = Chest_Armor then
         Player_Ship.Crew(MemberIndex).Equipment(TORSO) := ItemIndex;
      elsif ItemType = Arms_Armor then
         Player_Ship.Crew(MemberIndex).Equipment(ARMS) := ItemIndex;
      elsif ItemType = Legs_Armor then
         Player_Ship.Crew(MemberIndex).Equipment(LEGS) := ItemIndex;
      elsif TinyString_Indefinite_Container.Find_Index
          (Container => Tools_List, Item => ItemType) /=
        TinyString_Indefinite_Container.No_Index then
         Player_Ship.Crew(MemberIndex).Equipment(TOOL) := ItemIndex;
      end if;
      return
        Sort_Crew_Inventory_Command
          (ClientData, Interp, 2, CArgv.Empty & "SortCrewInventory" & "-1");
   end Set_Use_Item_Command;

   -- ****o* SUCI/SUCI.Show_Move_Item_Command
   -- FUNCTION
   -- Show UI to move the selected item to the ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMoveItem memberindex itemindex
   -- Memberindex is the index of the crew member in which inventory item will
   -- be set, itemindex is the index of the item which will be set
   -- SOURCE
   function Show_Move_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Move_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 2));
      ItemDialog: constant Ttk_Frame :=
        Create_Dialog
          (".itemdialog",
           "Move " &
           Get_Item_Name
             (Inventory_Container.Element
                (Container => Player_Ship.Crew(MemberIndex).Inventory,
                 Index => ItemIndex)) &
           " to ship cargo",
           400, 2, ".memberdialog");
      Button: Ttk_Button :=
        Create
          (ItemDialog & ".movebutton",
           "-text Move -command {MoveItem " & CArgv.Arg(Argv, 1) & " " &
           CArgv.Arg(Argv, 2) & "}");
      Max_Amount_Button: Ttk_Button;
      MaxAmount: constant Positive :=
        Inventory_Container.Element
          (Container => Player_Ship.Crew(MemberIndex).Inventory,
           Index => ItemIndex)
          .Amount;
      AmountBox: constant Ttk_SpinBox :=
        Create
          (ItemDialog & ".amount",
           "-width 5 -from 1.0 -to" & Float'Image(Float(MaxAmount)) &
           " -validate key -validatecommand {ValidateMoveAmount" &
           Positive'Image(MaxAmount) & " %P}");
   begin
      Max_Amount_Button :=
        Create
          (ItemDialog & ".amountlbl",
           "-text {Amount (max:" & Positive'Image(MaxAmount) &
           "):} -command {" & AmountBox & " set" & Positive'Image(MaxAmount) &
           "}");
      Tcl.Tk.Ada.Grid.Grid(Max_Amount_Button, "-padx 5");
      Set(AmountBox, "1");
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-column 1 -row 1");
      Bind
        (AmountBox, "<Escape>",
         "{" & ItemDialog & ".cancelbutton invoke;break}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-padx {5 0} -pady {0 5}");
      Bind
        (Button, "<Escape>", "{" & ItemDialog & ".cancelbutton invoke;break}");
      Button :=
        Create
          (ItemDialog & ".cancelbutton",
           "-text Cancel -command {CloseDialog " & ItemDialog &
           " .memberdialog;focus .memberdialog.button}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 2 -padx {0 5} -pady {0 5}");
      Focus(Button);
      Bind(Button, "<Tab>", "{focus " & ItemDialog & ".movebutton;break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      Show_Dialog(ItemDialog);
      return TCL_OK;
   end Show_Move_Item_Command;

   -- ****o* SUCI/SUCI.Move_Item_Command
   -- FUNCTION
   -- Move the selected item to the ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MoveItem memberindex itemindex
   -- Memberindex is the index of the crew member in which inventory item will
   -- be set, itemindex is the index of the item which will be set
   -- SOURCE
   function Move_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      Amount: Positive;
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 2));
      ItemDialog: Tk_Toplevel := Get_Widget(".itemdialog", Interp);
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(ItemDialog & ".amount", Interp);
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget
          (Main_Paned & ".shipinfoframe.cargo.canvas.frame.selecttype.combo",
           Interp);
   begin
      Amount := Positive'Value(Get(AmountBox));
      if Free_Cargo
          (0 -
           (Objects_Container.Element
              (Container => Items_List,
               Index =>
                 Inventory_Container.Element
                   (Container => Player_Ship.Crew(MemberIndex).Inventory,
                    Index => ItemIndex)
                   .Proto_Index)
              .Weight *
            Amount)) <
        0 then
         Show_Message
           (Text =>
              "No free space in ship cargo for that amount of " &
              Get_Item_Name
                (Inventory_Container.Element
                   (Container => Player_Ship.Crew(MemberIndex).Inventory,
                    Index => ItemIndex)),
            Title => "No free space in cargo");
         return TCL_OK;
      end if;
      Update_Cargo
        (Ship => Player_Ship,
         Proto_Index =>
           Inventory_Container.Element
             (Container => Player_Ship.Crew(MemberIndex).Inventory,
              Index => ItemIndex)
             .Proto_Index,
         Amount => Amount,
         Durability =>
           Inventory_Container.Element
             (Container => Player_Ship.Crew(MemberIndex).Inventory,
              Index => ItemIndex)
             .Durability,
         Price =>
           Inventory_Container.Element
             (Container => Player_Ship.Crew(MemberIndex).Inventory,
              Index => ItemIndex)
             .Price);
      Update_Inventory
        (Member_Index => MemberIndex, Amount => (0 - Amount),
         Inventory_Index => ItemIndex, Ship => Player_Ship);
      if
        (Player_Ship.Crew(MemberIndex).Order = CLEAN and
         Find_Item
             (Inventory => Player_Ship.Crew(MemberIndex).Inventory,
              Item_Type => Cleaning_Tools) =
           0) or
        ((Player_Ship.Crew(MemberIndex).Order = UPGRADING or
          Player_Ship.Crew(MemberIndex).Order = REPAIR) and
         Find_Item
             (Inventory => Player_Ship.Crew(MemberIndex).Inventory,
              Item_Type => Repair_Tools) =
           0) then
         Give_Orders(Player_Ship, MemberIndex, REST);
      end if;
      Destroy(ItemDialog);
      Generate(TypeBox, "<<ComboboxSelected>>");
      Tcl_Eval(Interp, "CloseDialog .itemdialog .memberdialog");
      if Inventory_Container.Length
          (Container => Player_Ship.Crew(MemberIndex).Inventory) =
        0 then
         Tcl_Eval(Interp, "CloseDialog .memberdialog");
         return TCL_OK;
      end if;
      return
        Sort_Crew_Inventory_Command
          (ClientData, Interp, 2, CArgv.Empty & "SortCrewInventory" & "-1");
   end Move_Item_Command;

   -- ****o* SUCI/SUCI.Validate_Move_Amount_Command
   -- FUNCTION
   -- Validate amount of the item to move
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ValidateMoveAmount
   -- SOURCE
   function Validate_Move_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Validate_Move_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Amount: Positive;
   begin
      Amount := Positive'Value(CArgv.Arg(Argv, 2));
      if Amount > Positive'Value(CArgv.Arg(Argv, 1)) then
         Tcl_SetResult(Interp, "0");
         return TCL_OK;
      end if;
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   exception
      when Constraint_Error =>
         Tcl_SetResult(Interp, "0");
         return TCL_OK;
   end Validate_Move_Amount_Command;

   -- ****o* SUCI/SUCI.Show_Inventory_Item_Info_Command
   -- FUNCTION
   -- Show detailed information about the selected item in crew member
   -- inventory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ValidateMoveAmount
   -- SOURCE
   function Show_Inventory_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Inventory_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      Show_Inventory_Item_Info
        (".memberdialog", Positive'Value(CArgv.Arg(Argv, 2)),
         Positive'Value(CArgv.Arg(Argv, 1)));
      return TCL_OK;
   end Show_Inventory_Item_Info_Command;

   -- ****if* SUCI/SUCI.Show_Inventory_Menu_Command
   -- FUNCTION
   -- Show the menu with available the selected item options
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowInventoryMenu itemindex
   -- ItemIndex is the index of the item's which actions' menu will be show
   -- SOURCE
   function Show_Inventory_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Inventory_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Item_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".inventoryitemmenu",
           Title =>
             Get_Item_Name
               (Inventory_Container.Element
                  (Container => Player_Ship.Crew(MemberIndex).Inventory,
                   Index => Positive'Value(CArgv.Arg(Argv, 2))),
                False, False) &
             " actions",
           Parent_Name => ".");
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
               Script => "{focus " & Item_Menu & ".equip;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Add_Button
        (Name => ".equip",
         Label =>
           (if Item_Is_Used(MemberIndex, Positive'Value(CArgv.Arg(Argv, 2)))
            then "Unequip"
            else "Equip"),
         Command =>
           "SetUseItem " & CArgv.Arg(Argv => Argv, N => 1) & " " &
           CArgv.Arg(Argv, 2));
      Add_Button
        (Name => ".move", Label => "Move the item to the ship cargo",
         Command =>
           "ShowMoveItem " & CArgv.Arg(Argv => Argv, N => 1) & " " &
           CArgv.Arg(Argv, 2));
      Add_Button
        (Name => ".info", Label => "Show more info about the item",
         Command =>
           "ShowInventoryItemInfo " & CArgv.Arg(Argv => Argv, N => 1) & " " &
           CArgv.Arg(Argv, 2));
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Item_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Inventory_Menu_Command;

   procedure AddCommands is
   begin
      Add_Command("UpdateInventory", Update_Inventory_Command'Access);
      Add_Command("ShowMemberInventory", Show_Member_Inventory_Command'Access);
      Add_Command("SetUseItem", Set_Use_Item_Command'Access);
      Add_Command("ShowMoveItem", Show_Move_Item_Command'Access);
      Add_Command("MoveItem", Move_Item_Command'Access);
      Add_Command("ValidateMoveAmount", Validate_Move_Amount_Command'Access);
      Add_Command
        ("ShowInventoryItemInfo", Show_Inventory_Item_Info_Command'Access);
      Add_Command("ShowInventoryMenu", Show_Inventory_Menu_Command'Access);
      Add_Command("SortCrewInventory", Sort_Crew_Inventory_Command'Access);
   end AddCommands;

end Ships.UI.Crew.Inventory;
