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
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Place;
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
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Crew.Inventory; use Crew.Inventory;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Missions; use Missions;
with Ships.Cargo; use Ships.Cargo;
with Stories; use Stories;
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
        Get_Widget(".gameframe.paned.shipinfoframe.cargo.canvas", Interp);
      CargoInfoFrame: constant Ttk_Frame :=
        Get_Widget(ShipCanvas & ".frame", Interp);
      Item: Ttk_Frame;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Row: Positive := 3;
      ItemType, ProtoIndex, ProgressBarStyle: Unbounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      CargoButton: Ttk_Button;
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget(CargoInfoFrame & ".selecttype.combo", Interp);
      DurabilityBar: Ttk_ProgressBar;
      ItemLabel: Ttk_Label;
      ItemsType: constant String := Get(TypeBox);
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
              "-text {" & GetItemName(PlayerShip.Cargo(I)) &
              "} -command {ShowCargoMenu" &
              Positive'Image(Inventory_Container.To_Index(I)) & "}");
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

   -- ****o* SUCargo/Show_Give_Item_Command
   -- FUNCTION
   -- Show UI to give the selected item from the ship cargo to the selected
   -- crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowGiveItem itemindex
   -- Itemindex is the index of the item which will be set
   -- SOURCE
   function Show_Give_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Give_Item_Command
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
          (ItemFrame & ".givebutton",
           "-text Give -command {GiveItem " & CArgv.Arg(Argv, 1) & "}");
      Height: Positive := 10;
      Label: Ttk_Label;
      AmountBox: constant Ttk_SpinBox :=
        Create
          (ItemFrame & ".giveamount",
           "-width 15 -from 1.0 -to" &
           Float'Image(Float(PlayerShip.Cargo(ItemIndex).Amount)) &
           " -validate key -validatecommand {CheckAmount %W" &
           Positive'Image(ItemIndex) & " %P} -command {ValidateAmount " &
           ItemFrame & ".giveamount" & Positive'Image(ItemIndex) & "}");
      CrewBox: constant Ttk_ComboBox :=
        Create(ItemFrame & ".member", "-state readonly -width 14");
      MembersNames: Unbounded_String;
   begin
      Wm_Set(ItemDialog, "title", "{Steam Sky - Give Item}");
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
           "-text {Give " & GetItemName(PlayerShip.Cargo(ItemIndex)) &
           " from ship cargo to the selected crew member} -wraplength 370");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2");
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      Label := Create(ItemFrame & ".amountlbl", "-text {Amount:}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Set(AmountBox, "1");
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-column 1 -row 1");
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      Label := Create(ItemFrame & ".memberlbl", "-text {To:}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      for Member of PlayerShip.Crew loop
         Append(MembersNames, " " & Member.Name);
      end loop;
      configure(CrewBox, "-values [list" & To_String(MembersNames) & "]");
      Current(CrewBox, "0");
      Tcl.Tk.Ada.Grid.Grid(CrewBox, "-column 1 -row 2");
      Height := Height + Positive'Value(Winfo_Get(CrewBox, "reqheight"));
      Label :=
        Create
          (ItemFrame & ".errorlbl", "-style Headerred.TLabel -wraplength 370");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2");
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 0 -row 4");
      Button :=
        Create
          (ItemFrame & ".cancelbutton",
           "-text Cancel -command {destroy " & ItemDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 4");
      Height := Height + Positive'Value(Winfo_Get(Button, "reqheight"));
      Focus(Button);
      if Height > 500 then
         Height := 500;
      end if;
      configure(ItemFrame, "-height" & Positive'Image(Height) & " -width 370");
      Canvas_Create
        (ItemCanvas, "window", "0 0 -anchor nw -window " & ItemFrame);
      configure
        (ItemCanvas, "-scrollregion [list " & BBox(ItemCanvas, "all") & "]");
      Height := Height + 30;
      declare
         X, Y: Integer;
      begin
         X := (Positive'Value(Winfo_Get(ItemDialog, "vrootwidth")) - 400) / 2;
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
            "400x" & Trim(Positive'Image(Height), Left) & "+" &
            Trim(Positive'Image(X), Left) & "+" &
            Trim(Positive'Image(Y), Left));
         Bind(ItemDialog, "<Escape>", "{destroy " & ItemDialog & "}");
         Tcl_Eval(Interp, "update");
      end;
      return TCL_OK;
   end Show_Give_Item_Command;

   -- ****o* CUI2/Give_Item_Command
   -- FUNCTION
   -- Give selected amount of the selected item from the ship's cargo to the
   -- selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GiveItem
   -- SOURCE
   function Give_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Give_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      MemberIndex, Amount: Positive;
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Item: constant InventoryData := PlayerShip.Cargo(ItemIndex);
      ItemDialog: Tk_Toplevel := Get_Widget(".itemdialog", Interp);
      SpinBox: constant Ttk_SpinBox :=
        Get_Widget(ItemDialog & ".canvas.frame.giveamount");
      ComboBox: constant Ttk_ComboBox :=
        Get_Widget(ItemDialog & ".canvas.frame.member");
   begin
      Amount := Natural'Value(Get(SpinBox));
      MemberIndex := Natural'Value(Current(ComboBox)) + 1;
      if FreeInventory
          (MemberIndex, 0 - (Items_List(Item.ProtoIndex).Weight * Amount)) <
        0 then
         ShowMessage
           ("No free space in " &
            To_String(PlayerShip.Crew(MemberIndex).Name) &
            "'s inventory for that amount of " & GetItemName(Item));
         return TCL_OK;
      end if;
      AddMessage
        ("You gave" & Positive'Image(Amount) & " " &
         GetItemName(PlayerShip.Cargo(ItemIndex)) & " to " &
         To_String(PlayerShip.Crew(MemberIndex).Name) & ".",
         OtherMessage);
      UpdateInventory
        (MemberIndex => MemberIndex, Amount => Amount,
         ProtoIndex => Item.ProtoIndex, Durability => Item.Durability,
         Price => Item.Price);
      UpdateCargo
        (Ship => PlayerShip, Amount => (0 - Amount), CargoIndex => ItemIndex,
         Price => Item.Price);
      Destroy(ItemDialog);
      UpdateHeader;
      UpdateMessages;
      return Show_Cargo_Command(ClientData, Interp, Argc, Argv);
   end Give_Item_Command;

   -- ****o* SUCargo/Show_Drop_Item_Command
   -- FUNCTION
   -- Show UI to drop the selected item from the ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
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
      pragma Unreferenced(ClientData, Argc, Interp);
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ItemDialog: constant Ttk_Frame :=
        Create(".itemdialog", "-style Dialog.TFrame");
      Button: Ttk_Button :=
        Create
          (ItemDialog & ".dropbutton",
           "-text Drop -command {DropItem " & CArgv.Arg(Argv, 1) & "}");
      Label: Ttk_Label;
      AmountBox: constant Ttk_SpinBox :=
        Create
          (ItemDialog & ".amount",
           "-width 10 -from 1.0 -to" &
           Float'Image(Float(PlayerShip.Cargo(ItemIndex).Amount)) &
           " -validate key -validatecommand {CheckAmount %W" &
           Positive'Image(ItemIndex) & " %P} -command {ValidateAmount " &
           ItemDialog & ".amount" & Positive'Image(ItemIndex) & "}");
      Frame: Ttk_Frame := Get_Widget(".gameframe.header");
   begin
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Get_Widget(".gameframe.paned");
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Label :=
        Create
          (ItemDialog & ".title",
           "-text {Drop " & GetItemName(PlayerShip.Cargo(ItemIndex)) &
           " from ship cargo} -wraplength 370");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2");
      Label :=
        Create(ItemDialog & ".amountlbl", "-text {Amount:} -takefocus 0");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Set(AmountBox, "1");
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-column 1 -row 1");
      Label :=
        Create
          (ItemDialog & ".errorlbl",
           "-style Headerred.TLabel -wraplength 370");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 0 -row 3");
      Button :=
        Create
          (ItemDialog & ".cancelbutton",
           "-text Cancel -command {CloseDialog " & ItemDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 3");
      Focus(Button);
      Tcl.Tk.Ada.Place.Place(ItemDialog, "-in .gameframe -relx 0.3 -rely 0.3");
      Bind(Button, "<Tab>", "{focus .itemdialog.dropbutton;break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      return TCL_OK;
   end Show_Drop_Item_Command;

   -- ****o* SUCargo/Drop_Item_Command
   -- FUNCTION
   -- Drop selected amount of the selected item from the ship's cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DropItem
   -- SOURCE
   function Drop_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Drop_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      DropAmount, DropAmount2: Natural;
      ItemDialog: constant Ttk_Frame := Get_Widget(".itemdialog", Interp);
      SpinBox: constant Ttk_SpinBox :=
        Get_Widget(ItemDialog & ".amount", Interp);
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      DropAmount := Natural'Value(Get(SpinBox));
      DropAmount2 := DropAmount;
      if Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).IType =
        MissionItemsType then
         for J in 1 .. DropAmount2 loop
            for I in
              AcceptedMissions.First_Index .. AcceptedMissions.Last_Index loop
               if AcceptedMissions(I).MType = Deliver and
                 AcceptedMissions(I).ItemIndex =
                   PlayerShip.Cargo(ItemIndex).ProtoIndex then
                  DeleteMission(I);
                  DropAmount := DropAmount - 1;
                  exit;
               end if;
            end loop;
         end loop;
      elsif CurrentStory.Index /= Null_Unbounded_String then
         if Stories_List(CurrentStory.Index).StartData(1) =
           PlayerShip.Cargo(ItemIndex).ProtoIndex then
            FinishedStories.Delete(FinishedStories.Last_Index);
            ClearCurrentStory;
         end if;
      end if;
      if DropAmount > 0 then
         AddMessage
           ("You dropped" & Positive'Image(DropAmount) & " " &
            GetItemName(PlayerShip.Cargo(ItemIndex)) & ".",
            OtherMessage);
         UpdateCargo
           (Ship => PlayerShip,
            ProtoIndex => PlayerShip.Cargo.Element(ItemIndex).ProtoIndex,
            Amount => (0 - DropAmount),
            Durability => PlayerShip.Cargo.Element(ItemIndex).Durability,
            Price => PlayerShip.Cargo.Element(ItemIndex).Price);
      end if;
      if Close_Dialog_Command
          (ClientData, Interp, 2,
           CArgv.Empty & "CloseDialog" & ".itemdialog") =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
      UpdateHeader;
      UpdateMessages;
      return Show_Cargo_Command(ClientData, Interp, Argc, Argv);
   end Drop_Item_Command;

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

   -- ****if* SUCargo/Show_Cargo_Menu_Command
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
   -- ShowCargoMenu moduleindex
   -- ModuleIndex is the index of the item's menu to show
   -- SOURCE
   function Show_Cargo_Menu_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Menu_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ItemMenu: Tk_Menu := Get_Widget(".cargoitemmenu", Interp);
   begin
      if (Winfo_Get(ItemMenu, "exists")) = "0" then
         ItemMenu := Create(".cargoitemmenu", "-tearoff false");
      end if;
      Delete(ItemMenu, "0", "end");
      Menu.Add
        (ItemMenu, "command",
         "-label {Give the item to a crew member} -command {ShowGiveItem " &
         CArgv.Arg(Argv, 1) & "}");
      Menu.Add
        (ItemMenu, "command",
         "-label {Drop the item from the ship's cargo} -command {ShowDropItem " &
         CArgv.Arg(Argv, 1) & "}");
      Menu.Add
        (ItemMenu, "command",
         "-label {Show more info about the item} -command {ShowCargoItemInfo " &
         CArgv.Arg(Argv, 1) & "}");
      Tk_Popup
        (ItemMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
         Winfo_Get(Get_Main_Window(Interp), "pointery"));
      return TCL_OK;
   end Show_Cargo_Menu_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowCargo", Show_Cargo_Command'Access);
      AddCommand("ShowCargoItemInfo", Show_Cargo_Item_Info_Command'Access);
      AddCommand("ShowGiveItem", Show_Give_Item_Command'Access);
      AddCommand("GiveItem", Give_Item_Command'Access);
      AddCommand("ShowDropItem", Show_Drop_Item_Command'Access);
      AddCommand("DropItem", Drop_Item_Command'Access);
      AddCommand("ShowCargoMenu", Show_Cargo_Menu_Command'Access);
   end AddCommands;

end Ships.UI.Cargo;
