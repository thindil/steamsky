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

with Ada.Containers.Generic_Array_Sort;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
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
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
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

   -- ****iv* SUCargo/SUCargo.Cargo_Table
   -- FUNCTION
   -- Table with info about the player ship cargo
   -- SOURCE
   Cargo_Table: Table_Widget (Amount => 5);
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
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCargo ?page?
   -- Optional paramater page is the number of the page of cargo list to show
   -- SOURCE
   function Show_Cargo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
      use Tiny_String;

      Ship_Canvas: constant Tk_Canvas :=
        Get_Widget
          (pathName => Main_Paned & ".shipinfoframe.cargo.canvas",
           Interp => Interp);
      Cargo_Info_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Ship_Canvas & ".frame", Interp => Interp);
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Item_Type: Bounded_String;
      Items_Types: Unbounded_String := To_Unbounded_String(Source => "All");
      Type_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => Cargo_Info_Frame & ".selecttype.combo",
           Interp => Interp);
      Items_Type: constant String := Get(Widgt => Type_Box);
      Page: constant Positive :=
        (if Argc = 2 then Positive'Value(CArgv.Arg(Argv => Argv, N => 1))
         else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      Free_Space_Label: constant Ttk_Label :=
        Get_Widget
          (pathName => Cargo_Info_Frame & ".freespace", Interp => Interp);
   begin
      Create
        (S => Tokens,
         From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Cargo_Info_Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets
        (Start_Index => 3, End_Index => Rows - 1, Frame => Cargo_Info_Frame);
      Cargo_Table :=
        Create_Table
          (Parent => Widget_Image(Win => Cargo_Info_Frame),
           Headers =>
             (1 => To_Unbounded_String(Source => "Name"),
              2 => To_Unbounded_String(Source => "Durability"),
              3 => To_Unbounded_String(Source => "Type"),
              4 => To_Unbounded_String(Source => "Amount"),
              5 => To_Unbounded_String(Source => "Weight")),
           Scrollbar =>
             Get_Widget
               (pathName => Main_Paned & ".shipinfoframe.cargo.scrolly"),
           Command => "SortShipCargo",
           Tooltip => "Press mouse button to sort the cargo.");
      if Cargo_Indexes.Length /=
        Inventory_Container.Length(Container => Player_Ship.Cargo) then
         Cargo_Indexes.Clear;
         Fill_Cargo_Indexes_Loop :
         for I in
           Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
             Inventory_Container.Last_Index
               (Container => Player_Ship.Cargo) loop
            Cargo_Indexes.Append(New_Item => I);
         end loop Fill_Cargo_Indexes_Loop;
      end if;
      configure
        (Widgt => Free_Space_Label,
         options =>
           "-text {Free cargo space:" &
           Integer'Image(Free_Cargo(Amount => 0)) & " kg}");
      Load_Cargo_Loop :
      for I of Cargo_Indexes loop
         Show_Item_Block :
         declare
            Item: constant Inventory_Data :=
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I);
            Proto_Item: constant Object_Data :=
              Objects_Container.Element
                (Container => Items_List, Index => Item.Proto_Index);
         begin
            if Current_Row < Start_Row then
               Current_Row := Current_Row + 1;
               goto End_Of_Loop;
            end if;
            Item_Type :=
              (if Proto_Item.Show_Type /= Null_Bounded_String then
                 Proto_Item.Show_Type
               else Proto_Item.I_Type);
            if Index
                (Source => Items_Types,
                 Pattern => "{" & To_String(Source => Item_Type) & "}") =
              0 then
               Append
                 (Source => Items_Types,
                  New_Item => " {" & To_String(Source => Item_Type) & "}");
            end if;
            if Items_Type /= "All"
              and then To_String(Source => Item_Type) /= Items_Type then
               goto End_Of_Loop;
            end if;
            Add_Button
              (Table => Cargo_Table, Text => Get_Item_Name(Item => Item),
               Tooltip => "Show item's description and actions",
               Command => "ShowCargoItemInfo" & Positive'Image(I),
               Column => 1);
            Add_Progress_Bar
              (Table => Cargo_Table, Value => Item.Durability,
               Max_Value => Default_Item_Durability,
               Tooltip => "The current durability of the selected crew member",
               Command => "ShowCargoItemInfo" & Positive'Image(I),
               Column => 2);
            Add_Button
              (Table => Cargo_Table, Text => To_String(Source => Item_Type),
               Tooltip => "The type of the selected item",
               Command => "ShowCargoItemInfo" & Positive'Image(I),
               Column => 3);
            Add_Button
              (Table => Cargo_Table, Text => Positive'Image(Item.Amount),
               Tooltip => "The amount of the selected item",
               Command => "ShowCargoItemInfo" & Positive'Image(I),
               Column => 4);
            Add_Button
              (Table => Cargo_Table,
               Text => Positive'Image(Item.Amount * Proto_Item.Weight) & " kg",
               Tooltip => "The total weight of the selected item",
               Command => "ShowCargoItemInfo" & Positive'Image(I), Column => 5,
               New_Row => True);
            exit Load_Cargo_Loop when Cargo_Table.Row =
              Game_Settings.Lists_Limit + 1;
            <<End_Of_Loop>>
         end Show_Item_Block;
      end loop Load_Cargo_Loop;
      if Page > 1 then
         Add_Pagination
           (Table => Cargo_Table,
            Previous_Command => "ShowCargo" & Positive'Image(Page - 1),
            Next_Command =>
              (if Cargo_Table.Row < Game_Settings.Lists_Limit + 1 then ""
               else "ShowCargo" & Positive'Image(Page + 1)));
      elsif Cargo_Table.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (Table => Cargo_Table, Previous_Command => "",
            Next_Command => "ShowCargo" & Positive'Image(Page + 1));
      end if;
      Update_Table(Table => Cargo_Table);
      configure
        (Widgt => Type_Box,
         options => "-values [list " & To_String(Source => Items_Types) & "]");
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Ship_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Ship_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Ship_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Ship_Canvas, Fraction => "0.0");
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
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipCargo x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Cargo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Cargo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      Column: constant Positive :=
        (if CArgv.Arg(Argv => Argv, N => 1) = "-1" then Positive'Last
         else Get_Column_Number
             (Table => Cargo_Table,
              X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1))));
      type Local_Cargo_Data is record
         Name: Unbounded_String;
         Damage: Float;
         Item_Type: Bounded_String;
         Amount: Positive;
         Weight: Positive;
         Id: Positive;
      end record;
      type Cargo_Array is array(Positive range <>) of Local_Cargo_Data;
      Local_Cargo: Cargo_Array
        (1 ..
             Natural
               (Inventory_Container.Length(Container => Player_Ship.Cargo)));
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
             (Client_Data => Client_Data, Interp => Interp, Argc => 1,
              Argv => CArgv.Empty & "ShowCargo");
      end if;
      Fill_Local_Cargo_Loop :
      for I in
        Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
          Inventory_Container.Last_Index(Container => Player_Ship.Cargo) loop
         Local_Cargo(I) :=
           (Name =>
              To_Unbounded_String
                (Source =>
                   Get_Item_Name
                     (Item =>
                        Inventory_Container.Element
                          (Container => Player_Ship.Cargo, Index => I),
                      Damage_Info => False, To_Lower => False)),
            Damage =>
              Float
                (Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => I)
                   .Durability) /
              Float(Default_Item_Durability),
            Item_Type =>
              (if
                 Objects_Container.Element
                   (Container => Items_List,
                    Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => I)
                        .Proto_Index)
                   .Show_Type /=
                 Null_Bounded_String
               then
                 Objects_Container.Element
                   (Container => Items_List,
                    Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => I)
                        .Proto_Index)
                   .Show_Type
               else Objects_Container.Element
                   (Container => Items_List,
                    Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => I)
                        .Proto_Index)
                   .I_Type),
            Amount =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I)
                .Amount,
            Weight =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I)
                .Amount *
              Objects_Container.Element
                (Container => Items_List,
                 Index =>
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => I)
                     .Proto_Index)
                .Weight,
            Id => I);
      end loop Fill_Local_Cargo_Loop;
      Sort_Cargo(Container => Local_Cargo);
      Cargo_Indexes.Clear;
      Fill_Cargo_Indexes_Loop :
      for Item of Local_Cargo loop
         Cargo_Indexes.Append(New_Item => Item.Id);
      end loop Fill_Cargo_Indexes_Loop;
      return
        Show_Cargo_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 1,
           Argv => CArgv.Empty & "ShowCargo");
   end Sort_Cargo_Command;

   -- ****o* SUCargo/SUCargo.Show_Give_Item_Command
   -- FUNCTION
   -- Show UI to give the selected item from the ship cargo to the selected
   -- crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowGiveItem itemindex
   -- Itemindex is the index of the item which will be set
   -- SOURCE
   function Show_Give_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Give_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Item_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Item_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".itemdialog",
           Title =>
             "Give " &
             Get_Item_Name
               (Item =>
                  Inventory_Container.Element
                    (Container => Player_Ship.Cargo, Index => Item_Index)) &
             " from the ship's cargo to the selected crew member",
           Title_Width => 370, Columns => 3);
      Button: Ttk_Button := Create(pathName => Item_Dialog & ".maxbutton");
      Label: Ttk_Label;
      Amount_Box: constant Ttk_SpinBox :=
        Create
          (pathName => Item_Dialog & ".giveamount",
           options =>
             "-width 14 -from 1 -to" &
             Positive'Image
               (Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => Item_Index)
                  .Amount) &
             " -validate key -validatecommand {CheckAmount %W" &
             Positive'Image(Item_Index) & " %P} -command {ValidateAmount " &
             Item_Dialog & ".giveamount" & Positive'Image(Item_Index) & "}");
      Crew_Box: constant Ttk_ComboBox :=
        Create
          (pathName => Item_Dialog & ".member",
           options => "-state readonly -width 14");
      Members_Names: Unbounded_String;
   begin
      Label := Create(pathName => Item_Dialog & ".memberlbl", options => "-text {To:}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      Load_Crew_Names_Loop :
      for Member of Player_Ship.Crew loop
         Append(Source => Members_Names, New_Item => " " & To_String(Source => Member.Name));
      end loop Load_Crew_Names_Loop;
      configure(Widgt => Crew_Box, options => "-values [list" & To_String(Source => Members_Names) & "]");
      Current(ComboBox => Crew_Box, NewIndex => "0");
      Tcl.Tk.Ada.Grid.Grid(Slave => Crew_Box, Options => "-column 1 -row 1");
      Bind
        (Widgt => Crew_Box, Sequence => "<Escape>",
         Script => "{" & Item_Dialog & ".cancelbutton invoke;break}");
      Bind
        (Widgt => Crew_Box, Sequence => "<<ComboboxSelected>>",
         Script => "{UpdateMaxGiveAmount " & CArgv.Arg(Argv => Argv, N => 1) & "}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-row 2 -pady {0 5}");
      Bind
        (Button, "<Escape>",
         "{" & Item_Dialog & ".cancelbutton invoke;break}");
      Add
        (Button,
         "Set the max amount as amount to give for the selected crew member.");
      Set(Amount_Box, "1");
      Tcl.Tk.Ada.Grid.Grid(Amount_Box, "-column 1 -row 2 -pady {0 5}");
      Bind
        (Amount_Box, "<Escape>",
         "{" & Item_Dialog & ".cancelbutton invoke;break}");
      Label :=
        Create
          (Item_Dialog & ".errorlbl",
           "-style Headerred.TLabel -wraplength 350");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2 -padx 5");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Button :=
        Create
          (Item_Dialog & ".givebutton",
           "-image giveicon -command {GiveItem " & CArgv.Arg(Argv, 1) &
           "} -style Dialog.TButton -text Give");
      Tcl.Tk.Ada.Grid.Grid
        (Button, "-column 0 -row 4 -padx 5 -pady 5 -sticky e");
      Add(Button, "Give the item");
      Bind
        (Button, "<Escape>",
         "{" & Item_Dialog & ".cancelbutton invoke;break}");
      Button :=
        Create
          (Item_Dialog & ".cancelbutton",
           "-image cancelicon -command {CloseDialog " & Item_Dialog &
           "} -style Dialog.TButton -text Close");
      Tcl.Tk.Ada.Grid.Grid
        (Button, "-column 1 -row 4 -padx {5 15} -pady 5 -sticky w");
      Add(Button, "Cancel giving and close dialog. \[Escape key\]");
      Focus(Button);
      Bind(Button, "<Tab>", "{focus .itemdialog.maxbutton;break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      Show_Dialog(Item_Dialog);
      Generate(Crew_Box, "<<ComboboxSelected>>");
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
      use Tiny_String;

      MemberIndex, Amount: Positive;
      ItemIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Item: constant Inventory_Data :=
        Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => ItemIndex);
      ItemDialog: Tk_Toplevel := Get_Widget(".itemdialog", Interp);
      SpinBox: constant Ttk_SpinBox := Get_Widget(ItemDialog & ".giveamount");
      ComboBox: constant Ttk_ComboBox := Get_Widget(ItemDialog & ".member");
   begin
      Amount := Natural'Value(Get(SpinBox));
      MemberIndex := Natural'Value(Current(ComboBox)) + 1;
      if Free_Inventory
          (MemberIndex,
           0 -
           (Objects_Container.Element
              (Container => Items_List, Index => Item.Proto_Index)
              .Weight *
            Amount)) <
        0 then
         Show_Message
           (Text =>
              "No free space in " &
              To_String(Player_Ship.Crew(MemberIndex).Name) &
              "'s inventory for that amount of " & Get_Item_Name(Item),
            Title => "Can't give item");
         return TCL_OK;
      end if;
      Add_Message
        ("You gave" & Positive'Image(Amount) & " " &
         Get_Item_Name
           (Inventory_Container.Element
              (Container => Player_Ship.Cargo, Index => ItemIndex)) &
         " to " & To_String(Player_Ship.Crew(MemberIndex).Name) & ".",
         OTHERMESSAGE);
      Update_Inventory
        (Member_Index => MemberIndex, Amount => Amount,
         Proto_Index => Item.Proto_Index, Durability => Item.Durability,
         Price => Item.Price, Ship => Player_Ship);
      Update_Cargo
        (Ship => Player_Ship, Amount => (0 - Amount), Cargo_Index => ItemIndex,
         Price => Item.Price);
      Destroy(ItemDialog);
      Tcl.Tk.Ada.Busy.Forget(Main_Paned);
      Tcl.Tk.Ada.Busy.Forget(Game_Header);
      Update_Header;
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
      Show_Manipulate_Item
        ("Drop " &
         Get_Item_Name
           (Inventory_Container.Element
              (Container => Player_Ship.Cargo, Index => ItemIndex)) &
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
      if To_String
          (Source =>
             Objects_Container.Element
               (Container => Items_List,
                Index =>
                  Inventory_Container.Element
                    (Container => Player_Ship.Cargo, Index => ItemIndex)
                    .Proto_Index)
               .I_Type) =
        To_String(Source => Mission_Items_Type) then
         Check_Drop_Items_Loop :
         for J in 1 .. DropAmount2 loop
            Delete_Missions_Loop :
            for I in Accepted_Missions.Iterate loop
               if Accepted_Missions(I).M_Type = DELIVER and
                 Accepted_Missions(I).Item_Index =
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => ItemIndex)
                     .Proto_Index then
                  Delete_Mission(Mission_Container.To_Index(I));
                  DropAmount := DropAmount - 1;
                  exit Delete_Missions_Loop;
               end if;
            end loop Delete_Missions_Loop;
         end loop Check_Drop_Items_Loop;
      elsif Current_Story.Index /= Null_Unbounded_String
        and then
          Positive'Value
            (To_String
               (Source => Stories_List(Current_Story.Index).Start_Data(1))) =
          Inventory_Container.Element
            (Container => Player_Ship.Cargo, Index => ItemIndex)
            .Proto_Index then
         Finished_Stories.Delete(Finished_Stories.Last_Index);
         Clear_Current_Story;
      end if;
      if DropAmount > 0 then
         Add_Message
           ("You dropped" & Positive'Image(DropAmount) & " " &
            Get_Item_Name
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => ItemIndex)) &
            ".",
            OTHERMESSAGE);
         Update_Cargo
           (Ship => Player_Ship,
            Proto_Index =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => ItemIndex)
                .Proto_Index,
            Amount => (0 - DropAmount),
            Durability =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => ItemIndex)
                .Durability,
            Price =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => ItemIndex)
                .Price);
      end if;
      if Close_Dialog_Command
          (ClientData, Interp, 2,
           CArgv.Empty & "CloseDialog" & ".itemdialog") =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
      Update_Header;
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
   -- ShowCargoItemInfo itemindex
   -- Itemindex is the index of the item which information will be show
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
      Show_Inventory_Item_Info
        (Parent => ".",
         Item_Index => Positive'Value(CArgv.Arg(Argv => Argv, N => 1)),
         Member_Index => 0,
         Button_1 =>
           (Tooltip =>
              To_Unbounded_String(Source => "Give item to a crew member"),
            Command =>
              To_Unbounded_String
                (Source => "ShowGiveItem " & CArgv.Arg(Argv => Argv, N => 1)),
            Icon => To_Unbounded_String(Source => "giveicon"),
            Text => To_Unbounded_String(Source => "Give")),
         Button_2 =>
           (Tooltip =>
              To_Unbounded_String(Source => "Drop item from the ship cargo"),
            Command =>
              To_Unbounded_String
                (Source => "ShowDropItem " & CArgv.Arg(Argv => Argv, N => 1)),
            Icon => To_Unbounded_String(Source => "dropicon"),
            Text => To_Unbounded_String(Source => "Drop")));
      return TCL_OK;
   end Show_Cargo_Item_Info_Command;

   -- ****o* SUCargo/SUCargo.Update_Max_Give_Amount_Command
   -- FUNCTION
   -- Update max give amount after selecting the crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateMaxGiveAmount itemindex
   -- ItemIndex is the index of the item to give
   -- SOURCE
   function Update_Max_Give_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Max_Give_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      CrewBox: constant Ttk_ComboBox :=
        Get_Widget(".itemdialog.member", Interp);
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".itemdialog.giveamount", Interp);
      MemberIndex: constant Positive := Natural'Value(Current(CrewBox)) + 1;
      Item: constant Inventory_Data :=
        Inventory_Container.Element
          (Container => Player_Ship.Cargo,
           Index => Positive'Value(CArgv.Arg(Argv, 1)));
      MaxAmount: Natural :=
        Free_Inventory(MemberIndex, 0) /
        Objects_Container.Element
          (Container => Items_List, Index => Item.Proto_Index)
          .Weight;
      MaxButton: constant Ttk_Button :=
        Get_Widget(".itemdialog.maxbutton", Interp);
   begin
      if Item.Amount < MaxAmount then
         MaxAmount := Item.Amount;
      end if;
      if Natural'Value(Get(AmountBox)) > MaxAmount then
         Set(AmountBox, Natural'Image(MaxAmount));
      end if;
      configure(AmountBox, "-to" & Natural'Image(MaxAmount));
      configure
        (MaxButton,
         "-text {Amount (max:" & Natural'Image(MaxAmount) & "):} -command {" &
         AmountBox & " set" & Natural'Image(MaxAmount) & "}");
      return TCL_OK;
   end Update_Max_Give_Amount_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowCargo", Show_Cargo_Command'Access);
      Add_Command("ShowCargoItemInfo", Show_Cargo_Item_Info_Command'Access);
      Add_Command("ShowGiveItem", Show_Give_Item_Command'Access);
      Add_Command("GiveItem", Give_Item_Command'Access);
      Add_Command("ShowDropItem", Show_Drop_Item_Command'Access);
      Add_Command("DropItem", Drop_Item_Command'Access);
      Add_Command("SortShipCargo", Sort_Cargo_Command'Access);
      Add_Command
        ("UpdateMaxGiveAmount", Update_Max_Give_Amount_Command'Access);
   end Add_Commands;

end Ships.UI.Cargo;
