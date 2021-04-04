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

with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
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
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Crew.Inventory; use Crew.Inventory;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Missions; use Missions;
with Ships.Cargo; use Ships.Cargo;
with Stories; use Stories;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Ships.UI.Cargo is

   -- ****iv* SUCargo/SUCargo.CargoTable
   -- FUNCTION
   -- Table with info about the player ship cargo
   -- SOURCE
   CargoTable: Table_Widget (5);
   -- ****

   -- ****o* SUCargo/SUCargo.Show_Cargo_Command
   -- FUNCTION
   -- Show the cargo of the player ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCargo ?page?
   -- Optional paramater page is the number of the page of cargo list to show
   -- SOURCE
   function Show_Cargo_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      ShipCanvas: constant Tk_Canvas :=
        Get_Widget(".gameframe.paned.shipinfoframe.cargo.canvas", Interp);
      CargoInfoFrame: constant Ttk_Frame :=
        Get_Widget(ShipCanvas & ".frame", Interp);
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ItemType, ProtoIndex: Unbounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget(CargoInfoFrame & ".selecttype.combo", Interp);
      ItemsType: constant String := Get(TypeBox);
      Page: constant Positive :=
        (if Argc = 2 then Positive'Value(CArgv.Arg(Argv, 1)) else 1);
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
      Current_Row: Positive := 1;
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(CargoInfoFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(2, Rows - 1, CargoInfoFrame);
      CargoTable :=
        CreateTable
          (Widget_Image(CargoInfoFrame),
           (To_Unbounded_String("Name"), To_Unbounded_String("Durability"),
            To_Unbounded_String("Type"), To_Unbounded_String("Amount"),
            To_Unbounded_String("Weight")),
           False);
      Load_Cargo_Loop :
      for I in PlayerShip.Cargo.Iterate loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         ProtoIndex := PlayerShip.Cargo(I).ProtoIndex;
         ItemType :=
           (if Items_List(ProtoIndex).ShowType /= Null_Unbounded_String then
              Items_List(ProtoIndex).ShowType
            else Items_List(ProtoIndex).IType);
         if Index(ItemsTypes, "{" & To_String(ItemType) & "}") = 0 then
            Append(ItemsTypes, " {" & To_String(ItemType) & "}");
         end if;
         if ItemsType /= "All" and then To_String(ItemType) /= ItemsType then
            goto End_Of_Loop;
         end if;
         AddButton
           (CargoTable, GetItemName(PlayerShip.Cargo(I)),
            "Show available item's options",
            "ShowCargoMenu" & Positive'Image(Inventory_Container.To_Index(I)),
            1);
         AddProgressBar
           (CargoTable, PlayerShip.Cargo(I).Durability,
            Default_Item_Durability,
            "The current durability of the selected crew member",
            "ShowCargoMenu" & Positive'Image(Inventory_Container.To_Index(I)),
            2);
         AddButton
           (CargoTable, To_String(ItemType), "The type of the selected item",
            "ShowCargoMenu" & Positive'Image(Inventory_Container.To_Index(I)),
            3);
         AddButton
           (CargoTable, Positive'Image(PlayerShip.Cargo(I).Amount),
            "The amount of the selected item",
            "ShowCargoMenu" & Positive'Image(Inventory_Container.To_Index(I)),
            4);
         AddButton
           (CargoTable,
            Positive'Image
              (PlayerShip.Cargo(I).Amount * Items_List(ProtoIndex).Weight) &
            " kg",
            "The total weight of the selected item",
            "ShowCargoMenu" & Positive'Image(Inventory_Container.To_Index(I)),
            5, True);
         exit Load_Cargo_Loop when CargoTable.Row = 26;
         <<End_Of_Loop>>
      end loop Load_Cargo_Loop;
      if Page > 1 then
         if CargoTable.Row < 26 then
            AddPagination
              (CargoTable, "ShowCargo" & Positive'Image(Page - 1), "");
         else
            AddPagination
              (CargoTable, "ShowCargo" & Positive'Image(Page - 1),
               "ShowCargo" & Positive'Image(Page + 1));
         end if;
      elsif CargoTable.Row = 26 then
         AddPagination(CargoTable, "", "ShowCargo" & Positive'Image(Page + 1));
      end if;
      UpdateTable(CargoTable);
      configure(TypeBox, "-values [list " & To_String(ItemsTypes) & "]");
      Tcl_Eval(Get_Context, "update");
      configure
        (ShipCanvas, "-scrollregion [list " & BBox(ShipCanvas, "all") & "]");
      Xview_Move_To(ShipCanvas, "0.0");
      Yview_Move_To(ShipCanvas, "0.0");
      return TCL_OK;
   end Show_Cargo_Command;

   -- ****o* SUCargo/SUCargo.Show_Give_Item_Command
   -- FUNCTION
   -- Show UI to give the selected item from the ship cargo to the selected
   -- crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowGiveItem itemindex
   -- Itemindex is the index of the item which will be set
   -- SOURCE
   function Show_Give_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Give_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      ItemDialog: constant Ttk_Frame :=
        Create(".itemdialog", "-style Dialog.TFrame");
      Button: Ttk_Button :=
        Create
          (ItemDialog & ".givebutton",
           "-text Give -command {GiveItem " & CArgv.Arg(Argv, 1) & "}");
      Label: Ttk_Label;
      AmountBox: constant Ttk_SpinBox :=
        Create
          (ItemDialog & ".giveamount",
           "-width 15 -from 1.0 -to" &
           Float'Image(Float(PlayerShip.Cargo(ItemIndex).Amount)) &
           " -validate key -validatecommand {CheckAmount %W" &
           Positive'Image(ItemIndex) & " %P} -command {ValidateAmount " &
           ItemDialog & ".giveamount" & Positive'Image(ItemIndex) & "}");
      CrewBox: constant Ttk_ComboBox :=
        Create(ItemDialog & ".member", "-state readonly -width 14");
      MembersNames: Unbounded_String;
      Frame: Ttk_Frame := Get_Widget(".gameframe.header");
   begin
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Get_Widget(".gameframe.paned");
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Label :=
        Create
          (ItemDialog & ".title",
           "-text {Give " & GetItemName(PlayerShip.Cargo(ItemIndex)) &
           " from the ship's cargo to the selected crew member} -wraplength 370");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2 -padx 5 -pady {5 0}");
      Label := Create(ItemDialog & ".amountlbl", "-text {Amount:}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-pady {0 5}");
      Set(AmountBox, "1");
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-column 1 -row 1 -pady {0 5}");
      Bind
        (AmountBox, "<Escape>",
         "{" & ItemDialog & ".cancelbutton invoke;break}");
      Label := Create(ItemDialog & ".memberlbl", "-text {To:}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Load_Crew_Names_Loop :
      for Member of PlayerShip.Crew loop
         Append(MembersNames, " " & Member.Name);
      end loop Load_Crew_Names_Loop;
      configure(CrewBox, "-values [list" & To_String(MembersNames) & "]");
      Current(CrewBox, "0");
      Tcl.Tk.Ada.Grid.Grid(CrewBox, "-column 1 -row 2");
      Bind
        (CrewBox, "<Escape>",
         "{" & ItemDialog & ".cancelbutton invoke;break}");
      Label :=
        Create
          (ItemDialog & ".errorlbl",
           "-style Headerred.TLabel -wraplength 370");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2 -padx 5");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 0 -row 4 -padx {5 0} -pady 5");
      Bind
        (Button, "<Escape>", "{" & ItemDialog & ".cancelbutton invoke;break}");
      Button :=
        Create
          (ItemDialog & ".cancelbutton",
           "-text Cancel -command {CloseDialog " & ItemDialog & "}");
      Tcl.Tk.Ada.Grid.Grid
        (Button, "-column 1 -row 4 -padx {0 5} -pady 5 -sticky e");
      Focus(Button);
      Tcl.Tk.Ada.Place.Place(ItemDialog, "-in .gameframe -relx 0.3 -rely 0.3");
      Bind(Button, "<Tab>", "{focus .itemdialog.givebutton;break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      return TCL_OK;
   end Show_Give_Item_Command;

   -- ****o* SUCargo/SUCargo.Give_Item_Command
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Give_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      MemberIndex, Amount: Positive;
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Item: constant InventoryData := PlayerShip.Cargo(ItemIndex);
      ItemDialog: Tk_Toplevel := Get_Widget(".itemdialog", Interp);
      SpinBox: constant Ttk_SpinBox := Get_Widget(ItemDialog & ".giveamount");
      ComboBox: constant Ttk_ComboBox := Get_Widget(ItemDialog & ".member");
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

   -- ****o* SUCargo/SUCargo.Show_Drop_Item_Command
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Drop_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Interp);
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      ShowManipulateItem
        ("Drop " & GetItemName(PlayerShip.Cargo(ItemIndex)) &
         " from the ship's cargo",
         "DropItem " & CArgv.Arg(Argv, 1), "drop", ItemIndex);
      return TCL_OK;
   end Show_Drop_Item_Command;

   -- ****o* SUCargo/SUCargo.Drop_Item_Command
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Drop_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      DropAmount, DropAmount2: Natural;
      ItemDialog: constant Ttk_Frame := Get_Widget(".itemdialog", Interp);
      SpinBox: constant Ttk_SpinBox :=
        Get_Widget(ItemDialog & ".amount", Interp);
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      DropAmount := Natural'Value(Get(SpinBox));
      DropAmount2 := DropAmount;
      if Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).IType =
        Mission_Items_Type then
         Check_Drop_Items_Loop :
         for J in 1 .. DropAmount2 loop
            Delete_Missions_Loop :
            for I in
              AcceptedMissions.First_Index .. AcceptedMissions.Last_Index loop
               if AcceptedMissions(I).MType = Deliver and
                 AcceptedMissions(I).ItemIndex =
                   PlayerShip.Cargo(ItemIndex).ProtoIndex then
                  DeleteMission(I);
                  DropAmount := DropAmount - 1;
                  exit Delete_Missions_Loop;
               end if;
            end loop Delete_Missions_Loop;
         end loop Check_Drop_Items_Loop;
      elsif CurrentStory.Index /= Null_Unbounded_String
        and then Stories_List(CurrentStory.Index).StartData(1) =
          PlayerShip.Cargo(ItemIndex).ProtoIndex then
         FinishedStories.Delete(FinishedStories.Last_Index);
         ClearCurrentStory;
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

   -- ****o* SUCargo/SUCargo.Show_Cargo_Item_Info_Command
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      ShowInventoryItemInfo(".", Positive'Value(CArgv.Arg(Argv, 1)), 0);
      return TCL_OK;
   end Show_Cargo_Item_Info_Command;

   -- ****if* SUCargo/SUCargo.Show_Cargo_Menu_Command
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
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
