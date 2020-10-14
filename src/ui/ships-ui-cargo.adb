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
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
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
with Utils.UI; use Utils.UI;

package body Ships.UI.Cargo is

   -- ****o* SUCargo/Show_Cargo_Command
   -- FUNCTION
   -- Show the cargo of the player ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateCargo
   -- SOURCE
   function Show_Cargo_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ShipCanvas: constant Tk_Canvas :=
        Get_Widget(".paned.shipinfoframe.cargo.canvas", Interp);
      CargoInfoFrame: constant Ttk_Frame :=
        Get_Widget(ShipCanvas & ".frame", Interp);
      Item: Ttk_Frame;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Row: Positive := 3;
      ItemType, ProtoIndex, ProgressBarStyle: Unbounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      CargoButton: Ttk_MenuButton;
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget(CargoInfoFrame & ".selecttype.combo", Interp);
      DurabilityBar: Ttk_ProgressBar;
      ItemLabel: Ttk_Label;
      ItemsType: constant String := Get(TypeBox);
      ItemMenu: Tk_Menu;
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(CargoInfoFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 3 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (CargoInfoFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item := Get_Widget(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      for I in PlayerShip.Cargo.Iterate loop
         ItemMenu :=
           Get_Widget
             (".cargoitemmenu" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              Interp);
         if (Winfo_Get(ItemMenu, "exists")) = "0" then
            ItemMenu :=
              Create
                (".cargoitemmenu" &
                 Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
                 "-tearoff false");
         end if;
         Delete(ItemMenu, "0", "end");
         Menu.Add
           (ItemMenu, "command",
            "-label {Give the item to a crew member} -command {ShowGiveItem " &
            Positive'Image(Inventory_Container.To_Index(I)) & "}");
         Menu.Add
           (ItemMenu, "command",
            "-label {Drop the item from the ship's cargo} -command {ShowDropItem " &
            Positive'Image(Inventory_Container.To_Index(I)) & "}");
         Menu.Add
           (ItemMenu, "command",
            "-label {Show more info about the item} -command {ShowCargoItemInfo " &
            Positive'Image(Inventory_Container.To_Index(I)) & "}");
         ProtoIndex := PlayerShip.Cargo(I).ProtoIndex;
         if Items_List(ProtoIndex).ShowType /= Null_Unbounded_String then
            ItemType := Items_List(ProtoIndex).ShowType;
         else
            ItemType := Items_List(ProtoIndex).IType;
         end if;
         if Index(ItemsTypes, "{" & To_String(ItemType) & "}") = 0 then
            Append(ItemsTypes, " {" & To_String(ItemType) & "}");
         end if;
         if ItemsType /= "All" and then To_String(ItemType) /= ItemsType then
            goto End_Of_Loop;
         end if;
         CargoButton :=
           Create
             (CargoInfoFrame & ".name" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & GetItemName(PlayerShip.Cargo(I)) & "} -menu " &
              ItemMenu);
         Add(CargoButton, "Show available item's options");
         Tcl.Tk.Ada.Grid.Grid
           (CargoButton, "-row" & Natural'Image(Row) & " -sticky w");
         if PlayerShip.Cargo(I).Durability > 74 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif PlayerShip.Cargo(I).Durability > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         DurabilityBar :=
           Create
             (CargoInfoFrame & ".durability" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(PlayerShip.Cargo(I).Durability) &
              "}" & To_String(ProgressBarStyle));
         Add(DurabilityBar, "The current durability of the selected item.");
         Tcl.Tk.Ada.Grid.Grid
           (DurabilityBar, "-row" & Natural'Image(Row) & " -column 1");
         ItemLabel :=
           Create
             (CargoInfoFrame & ".type" & Trim(Natural'Image(Row), Left),
              "-text {" & To_String(ItemType) & "}");
         Add(ItemLabel, "The type of the selected item.");
         Tcl.Tk.Ada.Grid.Grid
           (ItemLabel, "-row" & Natural'Image(Row) & " -column 2");
         ItemLabel :=
           Create
             (CargoInfoFrame & ".amount" & Trim(Natural'Image(Row), Left),
              "-text {" & Positive'Image(PlayerShip.Cargo(I).Amount) & "}");
         Add(ItemLabel, "The amount of the selected item.");
         Tcl.Tk.Ada.Grid.Grid
           (ItemLabel, "-row" & Natural'Image(Row) & " -column 3");
         ItemLabel :=
           Create
             (CargoInfoFrame & ".weight" & Trim(Natural'Image(Row), Left),
              "-text {" &
              Positive'Image
                (PlayerShip.Cargo(I).Amount * Items_List(ProtoIndex).Weight) &
              " kg}");
         Add(ItemLabel, "The total weight of the selected item.");
         Tcl.Tk.Ada.Grid.Grid
           (ItemLabel, "-row" & Natural'Image(Row) & " -column 4");
         <<End_Of_Loop>>
         Row := Row + 1;
      end loop;
      configure(TypeBox, "-values [list " & To_String(ItemsTypes) & "]");
      Tcl_Eval(Get_Context, "update");
      configure
        (ShipCanvas, "-scrollregion [list " & BBox(ShipCanvas, "all") & "]");
      Xview_Move_To(ShipCanvas, "0.0");
      Yview_Move_To(ShipCanvas, "0.0");
      return TCL_OK;
   end Show_Cargo_Command;

   -- ****o* SUCargo/Show_Drop_Item_Command
   -- FUNCTION
   -- Show UI to drop the selected item from the ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowDropItem itemindex
   -- Itemindex is the index of the item which will be set
   -- SOURCE
   function Show_Drop_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Drop_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
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
          (ItemFrame & ".dropbutton",
           "-text Drop -command {DropItem " & CArgv.Arg(Argv, 1) & "}");
      Height, Width: Positive := 10;
      Label: Ttk_Label;
      AmountBox: constant Ttk_SpinBox :=
        Create
          (ItemFrame & ".amount",
           "-width 5 -from 1.0 -to" &
           Float'Image(Float(PlayerShip.Cargo(ItemIndex).Amount)) &
           " -validate key -validatecommand {CheckAmount %W" &
           Positive'Image(PlayerShip.Cargo(ItemIndex).Amount) &
           " %P} -command {ValidateAmount " & ItemFrame & ".amount" &
           Positive'Image(ItemIndex) & "}");
   begin
      Wm_Set(ItemDialog, "title", "{Steam Sky - Drop Item}");
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
           "-text {Drop " & GetItemName(PlayerShip.Cargo(ItemIndex)) &
           " from ship cargo} -wraplength 400");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2");
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      Width := Width + Positive'Value(Winfo_Get(Label, "reqwidth"));
      Label := Create(ItemFrame & ".amountlbl", "-text {Amount:}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Set(AmountBox, "1");
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-column 1 -row 1");
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      Label := Create(ItemFrame & ".errorlbl", "-style Headerred.TLabel -wraplength 400");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2");
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 0 -row 3");
      Button :=
        Create
          (ItemFrame & ".cancelbutton",
           "-text Cancel -command {destroy " & ItemDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 3");
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
         Bind(ItemDialog, "<Escape>", "{destroy " & ItemDialog & "}");
         Tcl_Eval(Interp, "update");
      end;
      return TCL_OK;
   end Show_Drop_Item_Command;

   -- ****o* SUCargo/Show_Cargo_Item_Info_Command
   -- FUNCTION
   -- Show detailed information about the selected item in the player ship
   -- cargo
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
   function Show_Cargo_Item_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Item_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      ShowInventoryItemInfo(".", Positive'Value(CArgv.Arg(Argv, 1)), 0);
      return TCL_OK;
   end Show_Cargo_Item_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowCargo", Show_Cargo_Command'Access);
      AddCommand("ShowCargoItemInfo", Show_Cargo_Item_Info_Command'Access);
      AddCommand("ShowDropItem", Show_Drop_Item_Command'Access);
   end AddCommands;

end Ships.UI.Cargo;
