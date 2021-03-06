-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Event; use Tcl.Tk.Ada.Event;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
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
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Member: constant Member_Data := Player_Ship.Crew(MemberIndex);
      Page: constant Positive :=
        (if Argc = 3 then Positive'Value(CArgv.Arg(Argv, 2)) else 1);
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
      Current_Row: Positive := 1;
   begin
      if InventoryTable.Row > 1 then
         ClearTable(InventoryTable);
      end if;
      Load_Inventory_Loop :
      for I in Member.Inventory.Iterate loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         AddButton
           (InventoryTable, GetItemName(Member.Inventory(I), False, False),
            "Show available item's options",
            "ShowInventoryMenu " & CArgv.Arg(Argv, 1) &
            Positive'Image(Inventory_Container.To_Index(I)),
            1);
         AddProgressBar
           (InventoryTable, Member.Inventory(I).Durability,
            Default_Item_Durability,
            "The current durability level of the selected item.",
            "ShowInventoryMenu " & CArgv.Arg(Argv, 1) &
            Positive'Image(Inventory_Container.To_Index(I)),
            2);
         if ItemIsUsed(MemberIndex, Inventory_Container.To_Index(I)) then
            AddCheckButton
              (InventoryTable, "The item is used by the crew member",
               "ShowInventoryMenu " & CArgv.Arg(Argv, 1) &
               Positive'Image(Inventory_Container.To_Index(I)),
               True, 3);
         else
            AddCheckButton
              (InventoryTable, "The item isn't used by the crew member",
               "ShowInventoryMenu " & CArgv.Arg(Argv, 1) &
               Positive'Image(Inventory_Container.To_Index(I)),
               False, 3);
         end if;
         AddButton
           (InventoryTable, Positive'Image(Member.Inventory(I).Amount),
            "The amount of the item owned by the crew member",
            "ShowInventoryMenu " & CArgv.Arg(Argv, 1) &
            Positive'Image(Inventory_Container.To_Index(I)),
            4);
         AddButton
           (InventoryTable,
            Positive'Image
              (Member.Inventory(I).Amount *
               Items_List(Member.Inventory(I).ProtoIndex).Weight) &
            " kg",
            "The total weight of the items",
            "ShowInventoryMenu " & CArgv.Arg(Argv, 1) &
            Positive'Image(Inventory_Container.To_Index(I)),
            5, True);
         exit Load_Inventory_Loop when InventoryTable.Row = 26;
         <<End_Of_Loop>>
      end loop Load_Inventory_Loop;
      if Page > 1 then
         AddPagination
           (InventoryTable,
            "UpdateInventory " & CArgv.Arg(Argv, 1) & Positive'Image(Page - 1),
            (if InventoryTable.Row < 26 then ""
             else "UpdateInventory " & CArgv.Arg(Argv, 1) &
               Positive'Image(Page + 1)));
      elsif InventoryTable.Row = 26 then
         AddPagination
           (InventoryTable, "",
            "UpdateInventory " & CArgv.Arg(Argv, 1) &
            Positive'Image(Page + 1));
      end if;
      UpdateTable(InventoryTable);
      return TCL_OK;
   end Update_Inventory_Command;

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
      MemberDialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".memberdialog",
           Title =>
             "Inventory of " &
             To_String
               (Player_Ship.Crew(Positive'Value(CArgv.Arg(Argv, 1))).Name),
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
      CloseButton: constant Ttk_Button := Get_Widget(MemberFrame & ".button");
      Height, Width: Positive := 10;
      FreeSpaceLabel: constant Ttk_Label :=
        Create
          (MemberFrame & ".freespace",
           "-text {Free inventory space:" &
           Integer'Image
             (FreeInventory(Positive'Value(CArgv.Arg(Argv, 1)), 0)) &
           " kg} -wraplength 400");
   begin
      Tcl.Tk.Ada.Grid.Grid(MemberCanvas, "-padx 5 -pady 5");
      Tcl.Tk.Ada.Grid.Grid
        (YScroll, "-row 1 -column 1 -padx 5 -pady 5 -sticky ns");
      Autoscroll(YScroll);
      Tcl.Tk.Ada.Grid.Grid(FreeSpaceLabel);
      Height :=
        Height + Positive'Value(Winfo_Get(FreeSpaceLabel, "reqheight"));
      InventoryTable :=
        CreateTable
          (Widget_Image(MemberFrame),
           (To_Unbounded_String("Name"), To_Unbounded_String("Durability"),
            To_Unbounded_String("Used"), To_Unbounded_String("Amount"),
            To_Unbounded_String("Weight")),
           YScroll);
      if Update_Inventory_Command(ClientData, Interp, Argc, Argv) =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
      Height :=
        Height + Positive'Value(Winfo_Get(InventoryTable.Canvas, "reqheight"));
      Width := Positive'Value(Winfo_Get(InventoryTable.Canvas, "reqwidth"));
      Add_Close_Button
        (MemberFrame & ".button", "Close", "CloseDialog " & MemberDialog);
      Height := Height + Positive'Value(Winfo_Get(CloseButton, "reqheight"));
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
   -- Argc       - Number of arguments passed to the command.
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
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 2));
      ItemType: constant Unbounded_String :=
        Items_List
          (Player_Ship.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
          .IType;
   begin
      if ItemIsUsed(MemberIndex, ItemIndex) then
         TakeOffItem(MemberIndex, ItemIndex);
         return Update_Inventory_Command(ClientData, Interp, Argc, Argv);
      end if;
      if ItemType = Weapon_Type then
         if Items_List
             (Player_Ship.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
             .Value
             (4) =
           2 and
           Player_Ship.Crew(MemberIndex).Equipment(2) /= 0 then
            ShowMessage
              (Text =>
                 To_String(Player_Ship.Crew(MemberIndex).Name) &
                 " can't use this weapon because have shield equiped. Take off shield first.",
               Title => "Shield in use");
            return TCL_OK;
         end if;
         Player_Ship.Crew(MemberIndex).Equipment(1) := ItemIndex;
      elsif ItemType = Shield_Type then
         if Player_Ship.Crew(MemberIndex).Equipment(1) > 0 then
            if Items_List
                (Player_Ship.Crew(MemberIndex).Inventory
                   (Player_Ship.Crew(MemberIndex).Equipment(1))
                   .ProtoIndex)
                .Value
                (4) =
              2 then
               ShowMessage
                 (Text =>
                    To_String(Player_Ship.Crew(MemberIndex).Name) &
                    " can't use shield because have equiped two-hand weapon. Take off weapon first.",
                  Title => "Two handed weapon in use");
               return TCL_OK;
            end if;
         end if;
         Player_Ship.Crew(MemberIndex).Equipment(2) := ItemIndex;
      elsif ItemType = Head_Armor then
         Player_Ship.Crew(MemberIndex).Equipment(3) := ItemIndex;
      elsif ItemType = Chest_Armor then
         Player_Ship.Crew(MemberIndex).Equipment(4) := ItemIndex;
      elsif ItemType = Arms_Armor then
         Player_Ship.Crew(MemberIndex).Equipment(5) := ItemIndex;
      elsif ItemType = Legs_Armor then
         Player_Ship.Crew(MemberIndex).Equipment(6) := ItemIndex;
      elsif Tools_List.Find_Index(Item => ItemType) /=
        UnboundedString_Container.No_Index then
         Player_Ship.Crew(MemberIndex).Equipment(7) := ItemIndex;
      end if;
      return Update_Inventory_Command(ClientData, Interp, Argc, Argv);
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
           GetItemName(Player_Ship.Crew(MemberIndex).Inventory(ItemIndex)) &
           " to ship cargo",
           400, 2, ".memberdialog");
      Button: Ttk_Button :=
        Create
          (ItemDialog & ".movebutton",
           "-text Move -command {MoveItem " & CArgv.Arg(Argv, 1) & " " &
           CArgv.Arg(Argv, 2) & "}");
      Label: Ttk_Label;
      MaxAmount: constant Positive :=
        Player_Ship.Crew(MemberIndex).Inventory(ItemIndex).Amount;
      AmountBox: constant Ttk_SpinBox :=
        Create
          (ItemDialog & ".amount",
           "-width 5 -from 1.0 -to" & Float'Image(Float(MaxAmount)) &
           " -validate key -validatecommand {ValidateMoveAmount" &
           Positive'Image(MaxAmount) & " %P}");
   begin
      Label :=
        Create
          (ItemDialog & ".amountlbl",
           "-text {Amount (max:" & Positive'Image(MaxAmount) & "):}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-padx 5");
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
           " .memberdialog;focus .memberdialog.canvas.frame.button}");
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
   -- Argc       - Number of arguments passed to the command.
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
      if FreeCargo
          (0 -
           (Items_List
              (Player_Ship.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
              .Weight *
            Amount)) <
        0 then
         ShowMessage
           (Text =>
              "No free space in ship cargo for that amount of " &
              GetItemName(Player_Ship.Crew(MemberIndex).Inventory(ItemIndex)),
            Title => "No free space in cargo");
         return TCL_OK;
      end if;
      UpdateCargo
        (Ship => Player_Ship,
         ProtoIndex =>
           Player_Ship.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex,
         Amount => Amount,
         Durability =>
           Player_Ship.Crew(MemberIndex).Inventory(ItemIndex).Durability,
         Price => Player_Ship.Crew(MemberIndex).Inventory(ItemIndex).Price);
      UpdateInventory
        (MemberIndex => MemberIndex, Amount => (0 - Amount),
         InventoryIndex => ItemIndex);
      if
        (Player_Ship.Crew(MemberIndex).Order = Clean and
         FindItem
             (Inventory => Player_Ship.Crew(MemberIndex).Inventory,
              ItemType => Cleaning_Tools) =
           0) or
        ((Player_Ship.Crew(MemberIndex).Order = Upgrading or
          Player_Ship.Crew(MemberIndex).Order = Repair) and
         FindItem
             (Inventory => Player_Ship.Crew(MemberIndex).Inventory,
              ItemType => Repair_Tools) =
           0) then
         GiveOrders(Player_Ship, MemberIndex, Rest);
      end if;
      Destroy(ItemDialog);
      Generate(TypeBox, "<<ComboboxSelected>>");
      Tcl_Eval(Interp, "CloseDialog {.itemdialog .memberdialog}");
      return Show_Member_Inventory_Command(ClientData, Interp, Argc, Argv);
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
      ShowInventoryItemInfo
        (".memberdialog", Positive'Value(CArgv.Arg(Argv, 2)),
         Positive'Value(CArgv.Arg(Argv, 1)));
      return TCL_OK;
   end Show_Inventory_Item_Info_Command;

   -- ****if* SUCI/SUCI.Show_Inventory_Menu_Command
   -- FUNCTION
   -- Show the menu with available the selected item options
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowInventoryMenu moduleindex
   -- ModuleIndex is the index of the item's menu to show
   -- SOURCE
   function Show_Inventory_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Inventory_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ItemMenu: Tk_Menu := Get_Widget(".itemmenu", Interp);
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      if (Winfo_Get(ItemMenu, "exists")) = "0" then
         ItemMenu := Create(".itemmenu", "-tearoff false");
      end if;
      Delete(ItemMenu, "0", "end");
      if ItemIsUsed(MemberIndex, Positive'Value(CArgv.Arg(Argv, 2))) then
         Menu.Add
           (ItemMenu, "command",
            "-label {Unequip} -command {SetUseItem " & CArgv.Arg(Argv, 1) &
            " " & CArgv.Arg(Argv, 2) & "}");
      else
         Menu.Add
           (ItemMenu, "command",
            "-label {Equip} -command {SetUseItem " & CArgv.Arg(Argv, 1) & " " &
            CArgv.Arg(Argv, 2) & "}");
      end if;
      Menu.Add
        (ItemMenu, "command",
         "-label {Move the item to the ship cargo} -command {ShowMoveItem " &
         CArgv.Arg(Argv, 1) & " " & CArgv.Arg(Argv, 2) & "}");
      Menu.Add
        (ItemMenu, "command",
         "-label {Show more info about the item} -command {ShowInventoryItemInfo " &
         CArgv.Arg(Argv, 1) & " " & CArgv.Arg(Argv, 2) & "}");
      Tk_Popup
        (ItemMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
         Winfo_Get(Get_Main_Window(Interp), "pointery"));
      return TCL_OK;
   end Show_Inventory_Menu_Command;

   procedure AddCommands is
   begin
      AddCommand("UpdateInventory", Update_Inventory_Command'Access);
      AddCommand("ShowMemberInventory", Show_Member_Inventory_Command'Access);
      AddCommand("SetUseItem", Set_Use_Item_Command'Access);
      AddCommand("ShowMoveItem", Show_Move_Item_Command'Access);
      AddCommand("MoveItem", Move_Item_Command'Access);
      AddCommand("ValidateMoveAmount", Validate_Move_Amount_Command'Access);
      AddCommand
        ("ShowInventoryItemInfo", Show_Inventory_Item_Info_Command'Access);
      AddCommand("ShowInventoryMenu", Show_Inventory_Menu_Command'Access);
   end AddCommands;

end Ships.UI.Crew.Inventory;
