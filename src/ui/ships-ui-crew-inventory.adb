-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Crew.Inventory; use Crew.Inventory;
with Factions; use Factions;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Ships.UI.Crew.Inventory is

   -- ****o* SUCI/Show_Member_Inventory_Command
   -- FUNCTION
   -- Show inventory of the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberInventory memberindex
   -- MemberIndex is the index of the crew member to show inventory
   -- SOURCE
   function Show_Member_Inventory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Inventory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Member: constant Member_Data := PlayerShip.Crew(MemberIndex);
      MemberDialog: constant Tk_Toplevel :=
        Create
          (".memberdialog",
           "-class Dialog -background [ttk::style lookup . -background] -relief solid -borderwidth 2");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      XScroll: constant Ttk_Scrollbar :=
        Create
          (MemberDialog & ".xscroll",
           "-orient horizontal -command [list .memberdialog.canvas xview]");
      YScroll: constant Ttk_Scrollbar :=
        Create
          (MemberDialog & ".yscroll",
           "-orient vertical -command [list .memberdialog.canvas yview]");
      MemberCanvas: constant Tk_Canvas :=
        Create
          (MemberDialog & ".canvas",
           "-yscrollcommand [list " & YScroll &
           " set] -xscrollcommand [list " & XScroll & " set]");
      MemberFrame: constant Ttk_Frame := Create(MemberCanvas & ".frame");
      CloseButton: constant Ttk_Button :=
        Create
          (MemberFrame & ".button",
           "-text Close -command {CloseDialog " & MemberDialog & "}");
      Height, Width, NewWidth: Positive := 10;
      Label, AmountLabel, WeightLabel, ItemLabel: Ttk_Label;
      ItemButton: Ttk_MenuButton;
      DamageBar: Ttk_ProgressBar;
      ProgressBarStyle: Unbounded_String;
      ItemMenu: Tk_Menu;
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      Wm_Set
        (MemberDialog, "title",
         "{Steam Sky - " & To_String(Member.Name) & " Orders Priorities}");
      Wm_Set(MemberDialog, "transient", ".");
      if Tcl_GetVar(Interp, "tcl_platform(os)") = "Linux" then
         Wm_Set(MemberDialog, "attributes", "-type dialog");
      end if;
      Tcl.Tk.Ada.Pack.Pack(YScroll, " -side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(MemberCanvas, "-expand true -fill both");
      Tcl.Tk.Ada.Pack.Pack(XScroll, "-fill x");
      Autoscroll(YScroll);
      Autoscroll(XScroll);
      Label := Create(MemberFrame & ".name", "-text {Name}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Label := Create(MemberFrame & ".durability", "-text {Durability}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 0");
      Label := Create(MemberFrame & ".used", "-text {Used}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 2 -row 0");
      AmountLabel := Create(MemberFrame & ".amount", "-text {Amount}");
      Tcl.Tk.Ada.Grid.Grid(AmountLabel, "-column 3 -row 0");
      WeightLabel := Create(MemberFrame & ".weight", "-text {Weight}");
      Tcl.Tk.Ada.Grid.Grid(WeightLabel, "-column 4 -row 0");
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      ItemMenu.Interp := Interp;
      for I in Member.Inventory.Iterate loop
         ItemMenu.Name :=
           New_String
             (".itemmenu" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left));
         if (Winfo_Get(ItemMenu, "exists")) = "0" then
            ItemMenu :=
              Create
                (".itemmenu" &
                 Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
                 "-tearoff false");
         end if;
         Delete(ItemMenu, "0", "end");
         if ItemIsUsed(MemberIndex, Inventory_Container.To_Index(I)) then
            Menu.Add
              (ItemMenu, "command",
               "-label {Unequip} -command {SetUseItem " & CArgv.Arg(Argv, 1) &
               Positive'Image(Inventory_Container.To_Index(I)) & "}");
            Label :=
              Create
                (MemberFrame & ".used" &
                 Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
                 "-text {Yes}");
         else
            Menu.Add
              (ItemMenu, "command",
               "-label {Equip} -command {SetUseItem " & CArgv.Arg(Argv, 1) &
               Positive'Image(Inventory_Container.To_Index(I)) & "}");
            Label :=
              Create
                (MemberFrame & ".used" &
                 Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
                 "-text {No}");
         end if;
         Menu.Add
           (ItemMenu, "command",
            "-label {Move the item to the ship cargo} -command {ShowMoveItem " &
            CArgv.Arg(Argv, 1) &
            Positive'Image(Inventory_Container.To_Index(I)) & "}");
         Menu.Add
           (ItemMenu, "command", "-label {Show more info about the item}");
         ItemButton :=
           Create
             (MemberFrame & ".name" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & GetItemName(Member.Inventory(I), False, False) &
              "} -menu " & ItemMenu);
         Add(ItemButton, "Show available item's options");
         Tcl.Tk.Ada.Grid.Grid(ItemButton, "-sticky w");
         Height := Height + Positive'Value(Winfo_Get(ItemButton, "reqheight"));
         if Member.Inventory(I).Durability > 74 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif Member.Inventory(I).Durability > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         DamageBar :=
           Create
             (MemberFrame & ".durability" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-value {" & Positive'Image(Member.Inventory(I).Durability) &
              "}" & To_String(ProgressBarStyle));
         Add(DamageBar, "The current durability level of the selected item.");
         Tcl.Tk.Ada.Grid.Grid
           (DamageBar,
            "-row" & Positive'Image(Inventory_Container.To_Index(I)) &
            " -column 1");
         Tcl.Tk.Ada.Grid.Grid
           (Label,
            "-row" & Positive'Image(Inventory_Container.To_Index(I)) &
            " -column 2");
         ItemLabel :=
           Create
             (MemberFrame & ".amount" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & Positive'Image(Member.Inventory(I).Amount) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (ItemLabel,
            "-row" & Positive'Image(Inventory_Container.To_Index(I)) &
            " -column 3");
         ItemLabel :=
           Create
             (MemberFrame & ".weight" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" &
              Positive'Image
                (Member.Inventory(I).Amount *
                 Items_List(Member.Inventory(I).ProtoIndex).Weight) &
              "}");
         Tcl.Tk.Ada.Grid.Grid
           (ItemLabel,
            "-row" & Positive'Image(Inventory_Container.To_Index(I)) &
            " -column 4");
         NewWidth :=
           Positive'Value(Winfo_Get(ItemButton, "reqwidth")) +
           Positive'Value(Winfo_Get(DamageBar, "reqwidth")) +
           Positive'Value(Winfo_Get(Label, "reqwidth")) +
           Positive'Value(Winfo_Get(AmountLabel, "reqwidth")) +
           Positive'Value(Winfo_Get(WeightLabel, "reqwidth"));
         if NewWidth > Width then
            Width := NewWidth;
         end if;
      end loop;
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-columnspan 5");
      Height := Height + Positive'Value(Winfo_Get(CloseButton, "reqheight"));
      Focus(CloseButton);
      if Height > 500 then
         Height := 500;
      end if;
      configure
        (MemberFrame,
         "-height" & Positive'Image(Height) & " -width" &
         Positive'Image(Width));
      Canvas_Create
        (MemberCanvas, "window", "0 0 -anchor nw -window " & MemberFrame);
      configure
        (MemberCanvas,
         "-scrollregion [list " & BBox(MemberCanvas, "all") & "]");
      Height := Height + 30;
      Width := Width + 30;
      declare
         X, Y: Integer;
      begin
         X :=
           (Positive'Value(Winfo_Get(MemberDialog, "vrootwidth")) - Width) / 2;
         if X < 0 then
            X := 0;
         end if;
         Y :=
           (Positive'Value(Winfo_Get(MemberDialog, "vrootheight")) - Height) /
           2;
         if Y < 0 then
            Y := 0;
         end if;
         Wm_Set
           (MemberDialog, "geometry",
            Trim(Positive'Image(Width), Left) & "x" &
            Trim(Positive'Image(Height), Left) & "+" &
            Trim(Positive'Image(X), Left) & "+" &
            Trim(Positive'Image(Y), Left));
         Bind(MemberDialog, "<Destroy>", "{CloseDialog " & MemberDialog & "}");
         Bind(MemberDialog, "<Escape>", "{CloseDialog " & MemberDialog & "}");
         Tcl_Eval(Interp, "update");
      end;
      return TCL_OK;
   end Show_Member_Inventory_Command;

   -- ****o* SUCI/Set_Use_Item_Command
   -- FUNCTION
   -- Set if item is used by a crew member or not
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Use_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 2));
      ItemType: constant Unbounded_String :=
        Items_List
          (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
          .IType;
      UsedLabel: Ttk_Label;
      ItemMenu: Tk_Menu;
   begin
      UsedLabel.Interp := Interp;
      UsedLabel.Name :=
        New_String(".memberdialog.canvas.frame.used" & CArgv.Arg(Argv, 2));
      ItemMenu.Interp := Interp;
      ItemMenu.Name := New_String(".itemmenu" & CArgv.Arg(Argv, 2));
      if not ItemIsUsed(MemberIndex, ItemIndex) then
         if ItemType = WeaponType then
            if Items_List
                (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
                .Value
                (4) =
              2 and
              PlayerShip.Crew(MemberIndex).Equipment(2) /= 0 then
               ShowMessage
                 (To_String(PlayerShip.Crew(MemberIndex).Name) &
                  " can't use this weapon because have shield equiped. Take off shield first.");
               return TCL_OK;
            end if;
            PlayerShip.Crew(MemberIndex).Equipment(1) := ItemIndex;
         elsif ItemType = ShieldType then
            if PlayerShip.Crew(MemberIndex).Equipment(1) > 0 then
               if Items_List
                   (PlayerShip.Crew(MemberIndex).Inventory
                      (PlayerShip.Crew(MemberIndex).Equipment(1))
                      .ProtoIndex)
                   .Value
                   (4) =
                 2 then
                  ShowMessage
                    (To_String(PlayerShip.Crew(MemberIndex).Name) &
                     " can't use shield because have equiped two-hand weapon. Take off weapon first.");
                  return TCL_OK;
               end if;
            end if;
            PlayerShip.Crew(MemberIndex).Equipment(2) := ItemIndex;
         elsif ItemType = HeadArmor then
            PlayerShip.Crew(MemberIndex).Equipment(3) := ItemIndex;
         elsif ItemType = ChestArmor then
            PlayerShip.Crew(MemberIndex).Equipment(4) := ItemIndex;
         elsif ItemType = ArmsArmor then
            PlayerShip.Crew(MemberIndex).Equipment(5) := ItemIndex;
         elsif ItemType = LegsArmor then
            PlayerShip.Crew(MemberIndex).Equipment(6) := ItemIndex;
         elsif Tools_List.Find_Index(Item => ItemType) /=
           UnboundedString_Container.No_Index then
            PlayerShip.Crew(MemberIndex).Equipment(7) := ItemIndex;
         end if;
         configure(UsedLabel, "-text {Yes}");
         Entry_Configure(ItemMenu, "0", "-label {Unequip}");
      else
         TakeOffItem(MemberIndex, ItemIndex);
         configure(UsedLabel, "-text {No}");
         Entry_Configure(ItemMenu, "0", "-label {Equip}");
      end if;
      return TCL_OK;
   end Set_Use_Item_Command;

   -- ****o* SUCI/Show_Move_Item_Command
   -- FUNCTION
   -- Show UI to move the selected item to the ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMoveItem memberindex itemindex
   -- Memberindex is the index of the crew member in which inventory item will
   -- be set, itemindex is the index of the item which will be set
   -- SOURCE
   function Show_Move_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Move_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 2));
      ItemDialog: constant Tk_Toplevel :=
        Create
          (".itemdialog",
           "-class Dialog -background [ttk::style lookup . -background] -relief solid -borderwidth 2");
      XScroll: constant Ttk_Scrollbar :=
        Create
          (ItemDialog & ".xscroll",
           "-orient horizontal -command [list .itemdialog.canvas xview]");
      YScroll: constant Ttk_Scrollbar :=
        Create
          (ItemDialog & ".yscroll",
           "-orient vertical -command [list .itemdialog.canvas yview]");
      ItemCanvas: constant Tk_Canvas :=
        Create
          (ItemDialog & ".canvas",
           "-yscrollcommand [list " & YScroll &
           " set] -xscrollcommand [list " & XScroll & " set]");
      ItemFrame: constant Ttk_Frame := Create(ItemCanvas & ".frame");
      Button: Ttk_Button :=
        Create
          (ItemFrame & ".movebutton",
           "-text Move -command {MoveItem " & CArgv.Arg(Argv, 1) & " " &
           CArgv.Arg(Argv, 2) & "}");
      Height, Width: Positive := 10;
      Label: Ttk_Label;
      AmountBox: constant Ttk_SpinBox :=
        Create
          (ItemFrame & ".amount",
           "-width 5 -from 1.0 -to" &
           Float'Image
             (Float
                (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Amount)) &
           " -validate key -validatecommand {ValidateMoveAmount" &
           Positive'Image
             (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Amount) &
           " %P}");
   begin
      Wm_Set(ItemDialog, "title", "{Steam Sky - Move Item}");
      Wm_Set(ItemDialog, "transient", ".");
      if Tcl_GetVar(Interp, "tcl_platform(os)") = "Linux" then
         Wm_Set(ItemDialog, "attributes", "-type dialog");
      end if;
      Tcl.Tk.Ada.Pack.Pack(YScroll, " -side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(ItemCanvas, "-expand true -fill both");
      Tcl.Tk.Ada.Pack.Pack(XScroll, "-fill x");
      Autoscroll(YScroll);
      Autoscroll(XScroll);
      Label :=
        Create
          (ItemFrame & ".title",
           "-text {Move " &
           GetItemName(PlayerShip.Crew(MemberIndex).Inventory(ItemIndex)) &
           " to ship cargo} -wraplength 400");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2");
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      Width := Width + Positive'Value(Winfo_Get(Label, "reqwidth"));
      Label := Create(ItemFrame & ".amountlbl", "-text {Amount:}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Set(AmountBox, "1");
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-column 1 -row 1");
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      Tcl.Tk.Ada.Grid.Grid(Button);
      Button :=
        Create
          (ItemFrame & ".cancelbutton",
           "-text Cancel -command {focus .memberdialog.canvas.frame.button;destroy " &
           ItemDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 2");
      Height := Height + Positive'Value(Winfo_Get(Button, "reqheight"));
      Focus(Button);
      if Height > 500 then
         Height := 500;
      end if;
      configure
        (ItemFrame,
         "-height" & Positive'Image(Height) & " -width" &
         Positive'Image(Width));
      Canvas_Create
        (ItemCanvas, "window", "0 0 -anchor nw -window " & ItemFrame);
      configure
        (ItemCanvas, "-scrollregion [list " & BBox(ItemCanvas, "all") & "]");
      Height := Height + 30;
      Width := Width + 30;
      declare
         X, Y: Integer;
      begin
         X :=
           (Positive'Value(Winfo_Get(ItemDialog, "vrootwidth")) - Width) / 2;
         if X < 0 then
            X := 0;
         end if;
         Y :=
           (Positive'Value(Winfo_Get(ItemDialog, "vrootheight")) - Height) / 2;
         if Y < 0 then
            Y := 0;
         end if;
         Wm_Set
           (ItemDialog, "geometry",
            Trim(Positive'Image(Width), Left) & "x" &
            Trim(Positive'Image(Height), Left) & "+" &
            Trim(Positive'Image(X), Left) & "+" &
            Trim(Positive'Image(Y), Left));
         Bind
           (ItemDialog, "<Escape>",
            "{focus .memberdialog.canvas.frame.button;destroy " & ItemDialog &
            "}");
         Tcl_Eval(Interp, "update");
      end;
      return TCL_OK;
   end Show_Move_Item_Command;

   -- ****o* SUCI/Move_Item_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      Amount: Positive;
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 2));
      ItemDialog: Tk_Toplevel;
      AmountBox: Ttk_SpinBox;
   begin
      ItemDialog.Interp := Interp;
      ItemDialog.Name := New_String(".itemdialog");
      AmountBox.Interp := Interp;
      AmountBox.Name := New_String(ItemDialog & ".canvas.frame.amount");
      Amount := Positive'Value(Get(AmountBox));
      if FreeCargo
          (0 -
           (Items_List
              (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
              .Weight *
            Amount)) <
        0 then
         ShowMessage
           ("No free space in ship cargo for that amount of " &
            GetItemName(PlayerShip.Crew(MemberIndex).Inventory(ItemIndex)));
         return TCL_OK;
      end if;
      UpdateCargo
        (Ship => PlayerShip,
         ProtoIndex =>
           PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex,
         Amount => Amount,
         Durability =>
           PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Durability,
         Price => PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Price);
      UpdateInventory
        (MemberIndex => MemberIndex, Amount => (0 - Amount),
         InventoryIndex => ItemIndex);
      if
        (PlayerShip.Crew(MemberIndex).Order = Clean and
         FindItem
             (Inventory => PlayerShip.Crew(MemberIndex).Inventory,
              ItemType => CleaningTools) =
           0) or
        ((PlayerShip.Crew(MemberIndex).Order = Upgrading or
          PlayerShip.Crew(MemberIndex).Order = Repair) and
         FindItem
             (Inventory => PlayerShip.Crew(MemberIndex).Inventory,
              ItemType => RepairTools) =
           0) then
         GiveOrders(PlayerShip, MemberIndex, Rest);
      end if;
      Destroy(ItemDialog);
      Tcl_Eval(Interp, "CloseDialog .memberdialog");
      return Show_Member_Inventory_Command(ClientData, Interp, Argc, Argv);
   end Move_Item_Command;

   -- ****o* SUCI/Validate_Move_Amount_Command
   -- FUNCTION
   -- Validate amount of the item to move
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
   function Validate_Move_Amount_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Validate_Move_Amount_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
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

   procedure AddCommands is
   begin
      AddCommand("ShowMemberInventory", Show_Member_Inventory_Command'Access);
      AddCommand("SetUseItem", Set_Use_Item_Command'Access);
      AddCommand("ShowMoveItem", Show_Move_Item_Command'Access);
      AddCommand("MoveItem", Move_Item_Command'Access);
      AddCommand("ValidateMoveAmount", Validate_Move_Amount_Command'Access);
   end AddCommands;

end Ships.UI.Crew.Inventory;
