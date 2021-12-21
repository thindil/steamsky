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

with Ada.Containers.Generic_Array_Sort;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
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
with Config; use Config;
with CoreUI; use CoreUI;
with Crew.Inventory; use Crew.Inventory;
with Dialogs; use Dialogs;
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

   -- ****iv* SUCargo/SUCargo.Cargo_Indexes
   -- FUNCTION
   -- Indexes of the player ship cargo
   -- SOURCE
   Cargo_Indexes: Positive_Container.Vector;
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
      use Tiny_String;

      ShipCanvas: constant Tk_Canvas :=
        Get_Widget(Main_Paned & ".shipinfoframe.cargo.canvas", Interp);
      CargoInfoFrame: constant Ttk_Frame :=
        Get_Widget(ShipCanvas & ".frame", Interp);
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ItemType: Unbounded_String;
      ProtoIndex: Bounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget(CargoInfoFrame & ".selecttype.combo", Interp);
      ItemsType: constant String := Get(TypeBox);
      Page: constant Positive :=
        (if Argc = 2 then Positive'Value(CArgv.Arg(Argv, 1)) else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      Free_Space_Label: constant Ttk_Label :=
        Get_Widget(CargoInfoFrame & ".freespace", Interp);
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(CargoInfoFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(3, Rows - 1, CargoInfoFrame);
      CargoTable :=
        CreateTable
          (Widget_Image(CargoInfoFrame),
           (To_Unbounded_String("Name"), To_Unbounded_String("Durability"),
            To_Unbounded_String("Type"), To_Unbounded_String("Amount"),
            To_Unbounded_String("Weight")),
           Get_Widget(Main_Paned & ".shipinfoframe.cargo.scrolly"),
           "SortShipCargo", "Press mouse button to sort the cargo.");
      if Cargo_Indexes.Length /= Player_Ship.Cargo.Length then
         Cargo_Indexes.Clear;
         for I in Player_Ship.Cargo.Iterate loop
            Cargo_Indexes.Append(Inventory_Container.To_Index(I));
         end loop;
      end if;
      configure
        (Free_Space_Label,
         "-text {Free cargo space:" & Integer'Image(FreeCargo(0)) & " kg}");
      Load_Cargo_Loop :
      for I of Cargo_Indexes loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         ProtoIndex := Player_Ship.Cargo(I).Proto_Index;
         ItemType :=
           (if Items_List(ProtoIndex).Show_Type /= Null_Unbounded_String then
              Items_List(ProtoIndex).Show_Type
            else Items_List(ProtoIndex).I_Type);
         if Index(ItemsTypes, "{" & To_String(ItemType) & "}") = 0 then
            Append(ItemsTypes, " {" & To_String(ItemType) & "}");
         end if;
         if ItemsType /= "All" and then To_String(ItemType) /= ItemsType then
            goto End_Of_Loop;
         end if;
         AddButton
           (CargoTable, Get_Item_Name(Player_Ship.Cargo(I)),
            "Show available item's options",
            "ShowCargoMenu" & Positive'Image(I), 1);
         AddProgressBar
           (CargoTable, Player_Ship.Cargo(I).Durability,
            Default_Item_Durability,
            "The current durability of the selected crew member",
            "ShowCargoMenu" & Positive'Image(I), 2);
         AddButton
           (CargoTable, To_String(ItemType), "The type of the selected item",
            "ShowCargoMenu" & Positive'Image(I), 3);
         AddButton
           (CargoTable, Positive'Image(Player_Ship.Cargo(I).Amount),
            "The amount of the selected item",
            "ShowCargoMenu" & Positive'Image(I), 4);
         AddButton
           (CargoTable,
            Positive'Image
              (Player_Ship.Cargo(I).Amount * Items_List(ProtoIndex).Weight) &
            " kg",
            "The total weight of the selected item",
            "ShowCargoMenu" & Positive'Image(I), 5, True);
         exit Load_Cargo_Loop when CargoTable.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Loop>>
      end loop Load_Cargo_Loop;
      if Page > 1 then
         AddPagination
           (CargoTable, "ShowCargo" & Positive'Image(Page - 1),
            (if CargoTable.Row < Game_Settings.Lists_Limit + 1 then ""
             else "ShowCargo" & Positive'Image(Page + 1)));
      elsif CargoTable.Row = Game_Settings.Lists_Limit + 1 then
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

   -- ****it* SUCargo/SUCargo.Cargo_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the player ship cargo
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
   -- NONE           - No sorting items (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Cargo_Sort_Orders is
     (NAMEASC, NAMEDESC, DURABILITYASC, DURABILITYDESC, TYPEASC, TYPEDESC,
      AMOUNTASC, AMOUNTDESC, WEIGHTASC, WEIGHTDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* SUCargo/SUCargo.Default_Cargo_Sort_Order
      -- FUNCTION
      -- Default sorting order for items in the player's ship cargo
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Cargo_Sort_Order: constant Cargo_Sort_Orders := NONE;
   -- ****

   -- ****iv* SUCargo/SUCargo.Cargo_Sort_Order
   -- FUNCTION
   -- The current sorting order of items in the player's ship cargo
   -- SOURCE
   Cargo_Sort_Order: Cargo_Sort_Orders := Default_Cargo_Sort_Order;
   -- ****

   -- ****o* SUCargo/SUCargo.Sort_Cargo_Command
   -- FUNCTION
   -- Sort the player's ship's cargo list
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipCargo x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Cargo_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Cargo_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      Column: constant Positive :=
        (if CArgv.Arg(Argv, 1) = "-1" then Positive'Last
         else Get_Column_Number
             (CargoTable, Natural'Value(CArgv.Arg(Argv, 1))));
      type Local_Cargo_Data is record
         Name: Unbounded_String;
         Damage: Float;
         Item_Type: Unbounded_String;
         Amount: Positive;
         Weight: Positive;
         Id: Positive;
      end record;
      type Cargo_Array is array(Positive range <>) of Local_Cargo_Data;
      Local_Cargo: Cargo_Array(1 .. Positive(Player_Ship.Cargo.Length));
      function "<"(Left, Right: Local_Cargo_Data) return Boolean is
      begin
         if Cargo_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Cargo_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Cargo_Sort_Order = DURABILITYASC
           and then Left.Damage < Right.Damage then
            return True;
         end if;
         if Cargo_Sort_Order = DURABILITYDESC
           and then Left.Damage > Right.Damage then
            return True;
         end if;
         if Cargo_Sort_Order = TYPEASC
           and then Left.Item_Type < Right.Item_Type then
            return True;
         end if;
         if Cargo_Sort_Order = TYPEDESC
           and then Left.Item_Type > Right.Item_Type then
            return True;
         end if;
         if Cargo_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Cargo_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         if Cargo_Sort_Order = WEIGHTASC
           and then Left.Weight < Right.Weight then
            return True;
         end if;
         if Cargo_Sort_Order = WEIGHTDESC
           and then Left.Weight > Right.Weight then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Cargo is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Cargo_Data,
         Array_Type => Cargo_Array);
   begin
      case Column is
         when 1 =>
            if Cargo_Sort_Order = NAMEASC then
               Cargo_Sort_Order := NAMEDESC;
            else
               Cargo_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Cargo_Sort_Order = DURABILITYASC then
               Cargo_Sort_Order := DURABILITYDESC;
            else
               Cargo_Sort_Order := DURABILITYASC;
            end if;
         when 3 =>
            if Cargo_Sort_Order = TYPEASC then
               Cargo_Sort_Order := TYPEDESC;
            else
               Cargo_Sort_Order := TYPEASC;
            end if;
         when 4 =>
            if Cargo_Sort_Order = AMOUNTASC then
               Cargo_Sort_Order := AMOUNTDESC;
            else
               Cargo_Sort_Order := AMOUNTASC;
            end if;
         when 5 =>
            if Cargo_Sort_Order = WEIGHTASC then
               Cargo_Sort_Order := WEIGHTDESC;
            else
               Cargo_Sort_Order := WEIGHTASC;
            end if;
         when others =>
            null;
      end case;
      if Cargo_Sort_Order = NONE then
         return
           Show_Cargo_Command
             (ClientData, Interp, 1, CArgv.Empty & "ShowCargo");
      end if;
      for I in Player_Ship.Cargo.Iterate loop
         Local_Cargo(Inventory_Container.To_Index(I)) :=
           (Name =>
              To_Unbounded_String
                (Get_Item_Name(Player_Ship.Cargo(I), False, False)),
            Damage =>
              Float(Player_Ship.Cargo(I).Durability) /
              Float(Default_Item_Durability),
            Item_Type =>
              (if
                 Items_List(Player_Ship.Cargo(I).Proto_Index).Show_Type /=
                 Null_Unbounded_String
               then Items_List(Player_Ship.Cargo(I).Proto_Index).Show_Type
               else Items_List(Player_Ship.Cargo(I).Proto_Index).I_Type),
            Amount => Player_Ship.Cargo(I).Amount,
            Weight =>
              Player_Ship.Cargo(I).Amount *
              Items_List(Player_Ship.Cargo(I).Proto_Index).Weight,
            Id => Inventory_Container.To_Index(I));
      end loop;
      Sort_Cargo(Local_Cargo);
      Cargo_Indexes.Clear;
      for Item of Local_Cargo loop
         Cargo_Indexes.Append(Item.Id);
      end loop;
      return
        Show_Cargo_Command(ClientData, Interp, 1, CArgv.Empty & "ShowCargo");
   end Sort_Cargo_Command;

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
        Create_Dialog
          (".itemdialog",
           "Give " & Get_Item_Name(Player_Ship.Cargo(ItemIndex)) &
           " from the ship's cargo to the selected crew member",
           370, 2);
      Button: Ttk_Button :=
        Create
          (ItemDialog & ".givebutton",
           "-text Give -command {GiveItem " & CArgv.Arg(Argv, 1) & "}");
      Label: Ttk_Label;
      AmountBox: constant Ttk_SpinBox :=
        Create
          (ItemDialog & ".giveamount",
           "-width 15 -from 1 -to" &
           Positive'Image(Player_Ship.Cargo(ItemIndex).Amount) &
           " -validate key -validatecommand {CheckAmount %W" &
           Positive'Image(ItemIndex) & " %P} -command {ValidateAmount " &
           ItemDialog & ".giveamount" & Positive'Image(ItemIndex) & "}");
      CrewBox: constant Ttk_ComboBox :=
        Create(ItemDialog & ".member", "-state readonly -width 14");
      MembersNames: Unbounded_String;
   begin
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
      for Member of Player_Ship.Crew loop
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
      Bind(Button, "<Tab>", "{focus .itemdialog.givebutton;break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      Show_Dialog(ItemDialog);
      return TCL_OK;
   end Show_Give_Item_Command;

   -- ****o* SUCargo/SUCargo.Give_Item_Command
   -- FUNCTION
   -- Give selected amount of the selected item from the ship's cargo to the
   -- selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
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
      pragma Unreferenced(Argc);
      MemberIndex, Amount: Positive;
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Item: constant Inventory_Data := Player_Ship.Cargo(ItemIndex);
      ItemDialog: Tk_Toplevel := Get_Widget(".itemdialog", Interp);
      SpinBox: constant Ttk_SpinBox := Get_Widget(ItemDialog & ".giveamount");
      ComboBox: constant Ttk_ComboBox := Get_Widget(ItemDialog & ".member");
   begin
      Amount := Natural'Value(Get(SpinBox));
      MemberIndex := Natural'Value(Current(ComboBox)) + 1;
      if FreeInventory
          (MemberIndex, 0 - (Items_List(Item.Proto_Index).Weight * Amount)) <
        0 then
         ShowMessage
           (Text =>
              "No free space in " &
              To_String(Player_Ship.Crew(MemberIndex).Name) &
              "'s inventory for that amount of " & Get_Item_Name(Item),
            Title => "Can't give item");
         return TCL_OK;
      end if;
      Add_Message
        ("You gave" & Positive'Image(Amount) & " " &
         Get_Item_Name(Player_Ship.Cargo(ItemIndex)) & " to " &
         To_String(Player_Ship.Crew(MemberIndex).Name) & ".",
         OTHERMESSAGE);
      UpdateInventory
        (MemberIndex => MemberIndex, Amount => Amount,
         ProtoIndex => Item.Proto_Index, Durability => Item.Durability,
         Price => Item.Price, Ship => Player_Ship);
      UpdateCargo
        (Ship => Player_Ship, Amount => (0 - Amount), CargoIndex => ItemIndex,
         Price => Item.Price);
      Destroy(ItemDialog);
      Tcl.Tk.Ada.Busy.Forget(Main_Paned);
      Tcl.Tk.Ada.Busy.Forget(Game_Header);
      UpdateHeader;
      Update_Messages;
      return
        Sort_Cargo_Command
          (ClientData, Interp, 2, CArgv.Empty & "SortShipCargo" & "-1");
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
        ("Drop " & Get_Item_Name(Player_Ship.Cargo(ItemIndex)) &
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
   -- Argc       - Number of arguments passed to the command. Unused
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
      pragma Unreferenced(Argc);
      use Tiny_String;

      DropAmount, DropAmount2: Natural;
      ItemDialog: constant Ttk_Frame := Get_Widget(".itemdialog", Interp);
      SpinBox: constant Ttk_SpinBox :=
        Get_Widget(ItemDialog & ".amount", Interp);
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      DropAmount := Natural'Value(Get(SpinBox));
      DropAmount2 := DropAmount;
      if Items_List(Player_Ship.Cargo(ItemIndex).Proto_Index).I_Type =
        Mission_Items_Type then
         Check_Drop_Items_Loop :
         for J in 1 .. DropAmount2 loop
            Delete_Missions_Loop :
            for I in Accepted_Missions.Iterate loop
               if Accepted_Missions(I).M_Type = DELIVER and
                 Accepted_Missions(I).Item_Index =
                   Player_Ship.Cargo(ItemIndex).Proto_Index then
                  Delete_Mission(Mission_Container.To_Index(I));
                  DropAmount := DropAmount - 1;
                  exit Delete_Missions_Loop;
               end if;
            end loop Delete_Missions_Loop;
         end loop Check_Drop_Items_Loop;
      elsif CurrentStory.Index /= Null_Unbounded_String
        and then
          To_Bounded_String
            (Source =>
               To_String
                 (Source => Stories_List(CurrentStory.Index).StartData(1))) =
          Player_Ship.Cargo(ItemIndex).Proto_Index then
         FinishedStories.Delete(FinishedStories.Last_Index);
         ClearCurrentStory;
      end if;
      if DropAmount > 0 then
         Add_Message
           ("You dropped" & Positive'Image(DropAmount) & " " &
            Get_Item_Name(Player_Ship.Cargo(ItemIndex)) & ".",
            OTHERMESSAGE);
         UpdateCargo
           (Ship => Player_Ship,
            ProtoIndex => Player_Ship.Cargo.Element(ItemIndex).Proto_Index,
            Amount => (0 - DropAmount),
            Durability => Player_Ship.Cargo.Element(ItemIndex).Durability,
            Price => Player_Ship.Cargo.Element(ItemIndex).Price);
      end if;
      if Close_Dialog_Command
          (ClientData, Interp, 2,
           CArgv.Empty & "CloseDialog" & ".itemdialog") =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
      UpdateHeader;
      Update_Messages;
      return
        Sort_Cargo_Command
          (ClientData, Interp, 2, CArgv.Empty & "SortShipCargo" & "-1");
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
      Show_Inventory_Item_Info(".", Positive'Value(CArgv.Arg(Argv, 1)), 0);
      return TCL_OK;
   end Show_Cargo_Item_Info_Command;

   -- ****if* SUCargo/SUCargo.Show_Cargo_Menu_Command
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
   -- ShowCargoMenu itemindex
   -- ItemIndex is the index of the item's menu to show
   -- SOURCE
   function Show_Cargo_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Item_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".cargoitemmenu",
           Title =>
             Get_Item_Name
               (Player_Ship.Cargo
                  (Positive'Value(CArgv.Arg(Argv => Argv, N => 1)))) &
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
               Script => "{focus " & Item_Menu & ".give;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Add_Button
        (Name => ".give", Label => "Give the item to a crew member",
         Command => "ShowGiveItem " & CArgv.Arg(Argv => Argv, N => 1));
      Add_Button
        (Name => ".drop", Label => "Drop the item from the ship's cargo",
         Command => "ShowDropItem " & CArgv.Arg(Argv => Argv, N => 1));
      Add_Button
        (Name => ".info", Label => "Show more info about the item",
         Command => "ShowCargoItemInfo " & CArgv.Arg(Argv => Argv, N => 1));
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Item_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Cargo_Menu_Command;

   procedure AddCommands is
   begin
      Add_Command("ShowCargo", Show_Cargo_Command'Access);
      Add_Command("ShowCargoItemInfo", Show_Cargo_Item_Info_Command'Access);
      Add_Command("ShowGiveItem", Show_Give_Item_Command'Access);
      Add_Command("GiveItem", Give_Item_Command'Access);
      Add_Command("ShowDropItem", Show_Drop_Item_Command'Access);
      Add_Command("DropItem", Drop_Item_Command'Access);
      Add_Command("ShowCargoMenu", Show_Cargo_Menu_Command'Access);
      Add_Command("SortShipCargo", Sort_Cargo_Command'Access);
   end AddCommands;

end Ships.UI.Cargo;
