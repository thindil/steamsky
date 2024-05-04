-- Copyright (c) 2020-2024 Bartek thindil Jasicki
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
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Event;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tklib.Ada.Tooltip;
with CoreUI; use CoreUI;
with Crew.Inventory; use Crew.Inventory;
with Dialogs; use Dialogs;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Missions;
with Ships.Cargo; use Ships.Cargo;
with Stories;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Ships.UI.Cargo is

   -- ****iv* SUCargo/SUCargo.Cargo_Table
   -- FUNCTION
   -- Table with info about the player ship cargo
   -- SOURCE
   Cargo_Table: Table_Widget (Amount => 5);
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****iv* SUCargo/SUCargo.Cargo_Indexes
   -- FUNCTION
   -- Indexes of the player ship cargo
   -- SOURCE
   Cargo_Indexes: Positive_Container.Vector;
   -- ****
   --## rule on REDUCEABLE_SCOPE

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
      use Tcl.Tk.Ada.Widgets.Canvas;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
--
      Ship_Canvas: constant Tk_Canvas :=
        Get_Widget
          (pathName => Main_Paned & ".shipinfoframe.cargo.canvas",
           Interp => Interp);
      Cargo_Info_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Ship_Canvas & ".frame", Interp => Interp);
      function Show_Ada_Cargo_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Import => True,
         Convention => C,
         External_Name => "showCargoCommand";
   begin
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
           Tooltip_Text => "Press mouse button to sort the cargo.");
      return
        Show_Ada_Cargo_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv);
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

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* SUCargo/SUCargo.Cargo_Sort_Order
   -- FUNCTION
   -- The current sorting order of items in the player's ship cargo
   -- SOURCE
   Cargo_Sort_Order: Cargo_Sort_Orders := Default_Cargo_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

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

      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Column: constant Positive :=
        (if CArgv.Arg(Argv => Argv, N => 1) = "-1" then Positive'Last
         else Get_Column_Number
             (Table => Cargo_Table,
              X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1))));
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      --## rule off TYPE_INITIAL_VALUES
      type Local_Cargo_Data is record
         Name: Unbounded_String;
         Damage: Float;
         Item_Type: Bounded_String;
         Amount: Positive;
         Weight: Positive;
         Id: Positive;
      end record;
      type Cargo_Array is array(Positive range <>) of Local_Cargo_Data;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Local_Cargo: Cargo_Array
        (1 ..
             Natural
               (Inventory_Container.Length(Container => Player_Ship.Cargo)));
      --## rule on IMPROPER_INITIALIZATION
      --## rule off DIRECTLY_ACCESSED_GLOBALS
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
      --## rule on DIRECTLY_ACCESSED_GLOBALS
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
                 Get_Proto_Item
                   (Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => I)
                        .Proto_Index)
                   .Show_Type /=
                 Null_Bounded_String
               then
                 Get_Proto_Item
                   (Index =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => I)
                        .Proto_Index)
                   .Show_Type
               else Get_Proto_Item
                   (Index =>
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
              Get_Proto_Item
                (Index =>
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => I)
                     .Proto_Index)
                .Weight,
            Id => I);
      end loop Fill_Local_Cargo_Loop;
      Sort_Cargo(Container => Local_Cargo);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Cargo_Indexes.Clear;
      Fill_Cargo_Indexes_Loop :
      for Item of Local_Cargo loop
         Cargo_Indexes.Append(New_Item => Item.Id);
      end loop Fill_Cargo_Indexes_Loop;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
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
      use Tcl.Tk.Ada.Event;
      use Tcl.Tklib.Ada.Tooltip;
      use Tiny_String;
      use Tcl.Tk.Ada.Widgets.TtkLabel;

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
             Positive'Image(Item_Index) & " %P " & Item_Dialog &
             ".givebutton} -command {ValidateAmount " & Item_Dialog &
             ".giveamount" & Positive'Image(Item_Index) & " " & Item_Dialog &
             ".givebutton}");
      Crew_Box: constant Ttk_ComboBox :=
        Create
          (pathName => Item_Dialog & ".member",
           options => "-state readonly -width 14");
      Members_Names: Unbounded_String := Null_Unbounded_String;
   begin
      Label :=
        Create
          (pathName => Item_Dialog & ".memberlbl", options => "-text {To:}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label);
      Load_Crew_Names_Loop :
      for Member of Player_Ship.Crew loop
         Append
           (Source => Members_Names,
            New_Item => " " & To_String(Source => Member.Name));
      end loop Load_Crew_Names_Loop;
      configure
        (Widgt => Crew_Box,
         options =>
           "-values [list" & To_String(Source => Members_Names) & "]");
      Current(ComboBox => Crew_Box, NewIndex => "0");
      Tcl.Tk.Ada.Grid.Grid(Slave => Crew_Box, Options => "-column 1 -row 1");
      Bind
        (Widgt => Crew_Box, Sequence => "<Escape>",
         Script => "{" & Item_Dialog & ".cancelbutton invoke;break}");
      Bind
        (Widgt => Crew_Box, Sequence => "<<ComboboxSelected>>",
         Script =>
           "{UpdateMaxGiveAmount " & CArgv.Arg(Argv => Argv, N => 1) & "}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-row 2 -pady {0 5}");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Item_Dialog & ".cancelbutton invoke;break}");
      Add
        (Widget => Button,
         Message =>
           "Set the max amount as amount to give for the selected crew member.");
      Set(SpinBox => Amount_Box, Value => "1");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Amount_Box, Options => "-column 1 -row 2 -pady {0 5}");
      Bind
        (Widgt => Amount_Box, Sequence => "<Escape>",
         Script => "{" & Item_Dialog & ".cancelbutton invoke;break}");
      Label :=
        Create
          (pathName => Item_Dialog & ".errorlbl",
           options => "-style Headerred.TLabel -wraplength 350");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-columnspan 2 -padx 5");
      Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Label);
      Button :=
        Create
          (pathName => Item_Dialog & ".givebutton",
           options =>
             "-image give2icon -command {GiveItem " &
             CArgv.Arg(Argv => Argv, N => 1) &
             "} -style Dialoggreen.TButton -text Give");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button,
         Options => "-column 0 -row 4 -padx 5 -pady 5 -sticky e");
      Add(Widget => Button, Message => "Give the item");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Item_Dialog & ".cancelbutton invoke;break}");
      Button :=
        Create
          (pathName => Item_Dialog & ".cancelbutton",
           options =>
             "-image cancelicon -command {CloseDialog " & Item_Dialog &
             "} -style Dialogred.TButton -text Close");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button,
         Options => "-column 1 -row 4 -padx {5 15} -pady 5 -sticky w");
      Add
        (Widget => Button,
         Message => "Cancel giving and close dialog. \[Escape key\]");
      Focus(Widgt => Button);
      Bind
        (Widgt => Button, Sequence => "<Tab>",
         Script => "{focus .itemdialog.maxbutton;break}");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Button & " invoke;break}");
      Show_Dialog(Dialog => Item_Dialog);
      Generate(Window => Crew_Box, EventName => "<<ComboboxSelected>>");
      return TCL_OK;
   end Show_Give_Item_Command;

   -- ****o* SUCargo/SUCargo.Give_Item_Command
   -- FUNCTION
   -- Give selected amount of the selected item from the ship's cargo to the
   -- selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GiveItem
   -- SOURCE
   function Give_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Give_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tcl.Tk.Ada.Widgets.Toplevel;
      use Tiny_String;

      Member_Index, Amount: Positive;
      Item_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Item: constant Inventory_Data :=
        Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => Item_Index);
      Item_Dialog: Tk_Toplevel :=
        Get_Widget(pathName => ".itemdialog", Interp => Interp);
      Spin_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => Item_Dialog & ".giveamount");
      Combo_Box: constant Ttk_ComboBox :=
        Get_Widget(pathName => Item_Dialog & ".member");
   begin
      Amount := Natural'Value(Get(Widgt => Spin_Box));
      Member_Index := Natural'Value(Current(ComboBox => Combo_Box)) + 1;
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      if Free_Inventory
          (Member_Index => Member_Index,
           Amount =>
             -(Get_Proto_Item(Index => Item.Proto_Index).Weight * Amount)) <
        0 then
         Show_Message
           (Text =>
              "No free space in " &
              To_String(Source => Player_Ship.Crew(Member_Index).Name) &
              "'s inventory for that amount of " & Get_Item_Name(Item => Item),
            Title => "Can't give item");
         return TCL_OK;
      end if;
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      Add_Message
        (Message =>
           "You gave" & Positive'Image(Amount) & " " &
           Get_Item_Name
             (Item =>
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => Item_Index)) &
           " to " & To_String(Source => Player_Ship.Crew(Member_Index).Name) &
           ".",
         M_Type => OTHERMESSAGE);
      Update_Inventory
        (Member_Index => Member_Index, Amount => Amount,
         Proto_Index => Item.Proto_Index, Durability => Item.Durability,
         Price => Item.Price, Ship => Player_Ship);
      Update_Cargo
        (Ship => Player_Ship, Amount => -Amount, Cargo_Index => Item_Index,
         Price => Item.Price);
      Destroy(Widgt => Item_Dialog);
      Tcl.Tk.Ada.Busy.Forget(Window => Main_Paned);
      Tcl.Tk.Ada.Busy.Forget(Window => Game_Header);
      Update_Header;
      Update_Messages;
      return
        Sort_Cargo_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => CArgv.Empty & "SortShipCargo" & "-1");
   end Give_Item_Command;

   -- ****o* SUCargo/SUCargo.Show_Drop_Item_Command
   -- FUNCTION
   -- Show UI to drop the selected item from the ship cargo
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowDropItem itemindex
   -- Itemindex is the index of the item which will be set
   -- SOURCE
   function Show_Drop_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Drop_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Interp);
      Item_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
   begin
      Show_Manipulate_Item
        (Title =>
           "Drop " &
           Get_Item_Name
             (Item =>
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => Item_Index)) &
           " from the ship's cargo",
         Command => "DropItem " & CArgv.Arg(Argv => Argv, N => 1),
         Action => "drop", Item_Index => Item_Index);
      return TCL_OK;
   end Show_Drop_Item_Command;

   -- ****o* SUCargo/SUCargo.Drop_Item_Command
   -- FUNCTION
   -- Drop selected amount of the selected item from the ship's cargo
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DropItem
   -- SOURCE
   function Drop_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Drop_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Missions;
      use Stories;
      use Tiny_String;

      Drop_Amount, Drop_Amount_2: Natural;
      Item_Dialog: constant Ttk_Frame :=
        Get_Widget(pathName => ".itemdialog", Interp => Interp);
      Spin_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => Item_Dialog & ".amount", Interp => Interp);
      Item_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      --## rule off IMPROPER_INITIALIZATION
      Accepted_Mission: Mission_Data;
      --## rule on IMPROPER_INITIALIZATION
   begin
      Drop_Amount := Natural'Value(Get(Widgt => Spin_Box));
      Drop_Amount_2 := Drop_Amount;
      if To_String
          (Source =>
             Get_Proto_Item
               (Index =>
                  Inventory_Container.Element
                    (Container => Player_Ship.Cargo, Index => Item_Index)
                    .Proto_Index)
               .I_Type) =
        To_String(Source => Mission_Items_Type) then
         Check_Drop_Items_Loop :
         for J in 1 .. Drop_Amount_2 loop
            Delete_Missions_Loop :
            for I in 1 .. Get_Accepted_Missions_Amount loop
               Accepted_Mission := Get_Accepted_Mission(Mission_Index => I);
               if Accepted_Mission.M_Type = DELIVER and
                 Accepted_Mission.Item_Index =
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Item_Index)
                     .Proto_Index then
                  Delete_Mission(Mission_Index => I);
                  Drop_Amount := Drop_Amount - 1;
                  exit Delete_Missions_Loop;
               end if;
            end loop Delete_Missions_Loop;
         end loop Check_Drop_Items_Loop;
      elsif Get_Current_Story.Index /= Null_Unbounded_String
        and then
          Positive'Value
            (To_String
               (Source =>
                  Get_Story(Index => Get_Current_Story.Index).Start_Data(1))) =
          Inventory_Container.Element
            (Container => Player_Ship.Cargo, Index => Item_Index)
            .Proto_Index then
         Clear_Current_Story;
      end if;
      if Drop_Amount > 0 then
         Add_Message
           (Message =>
              "You dropped" & Positive'Image(Drop_Amount) & " " &
              Get_Item_Name
                (Item =>
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Item_Index)) &
              ".",
            M_Type => OTHERMESSAGE);
         Update_Cargo
           (Ship => Player_Ship,
            Proto_Index =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Item_Index)
                .Proto_Index,
            Amount => -Drop_Amount,
            Durability =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Item_Index)
                .Durability,
            Price =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Item_Index)
                .Price);
      end if;
      if Close_Dialog_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => CArgv.Empty & "CloseDialog" & ".itemdialog") =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
      Update_Header;
      Update_Messages;
      return
        Sort_Cargo_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => CArgv.Empty & "SortShipCargo" & "-1");
   end Drop_Item_Command;

   -- ****o* SUCargo/SUCargo.Show_Cargo_Item_Info_Command
   -- FUNCTION
   -- Show detailed information about the selected item in the player ship
   -- cargo
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCargoItemInfo itemindex
   -- Itemindex is the index of the item which information will be show
   -- SOURCE
   function Show_Cargo_Item_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Item_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
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
            Text => To_Unbounded_String(Source => "Give"),
            Color => Null_Unbounded_String),
         Button_2 =>
           (Tooltip =>
              To_Unbounded_String(Source => "Drop item from the ship cargo"),
            Command =>
              To_Unbounded_String
                (Source => "ShowDropItem " & CArgv.Arg(Argv => Argv, N => 1)),
            Icon => To_Unbounded_String(Source => "dropicon"),
            Text => To_Unbounded_String(Source => "Drop"),
            Color => Null_Unbounded_String));
      return TCL_OK;
   end Show_Cargo_Item_Info_Command;

   -- ****o* SUCargo/SUCargo.Update_Max_Give_Amount_Command
   -- FUNCTION
   -- Update max give amount after selecting the crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateMaxGiveAmount itemindex
   -- ItemIndex is the index of the item to give
   -- SOURCE
   function Update_Max_Give_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Max_Give_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Crew_Box: constant Ttk_ComboBox :=
        Get_Widget(pathName => ".itemdialog.member", Interp => Interp);
      Amount_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => ".itemdialog.giveamount", Interp => Interp);
      Member_Index: constant Positive :=
        Natural'Value(Current(ComboBox => Crew_Box)) + 1;
      Item: constant Inventory_Data :=
        Inventory_Container.Element
          (Container => Player_Ship.Cargo,
           Index => Positive'Value(CArgv.Arg(Argv => Argv, N => 1)));
      Max_Amount: Natural :=
        Free_Inventory(Member_Index => Member_Index, Amount => 0) /
        Get_Proto_Item(Index => Item.Proto_Index).Weight;
      Max_Button: constant Ttk_Button :=
        Get_Widget(pathName => ".itemdialog.maxbutton", Interp => Interp);
   begin
      if Item.Amount < Max_Amount then
         Max_Amount := Item.Amount;
      end if;
      if Natural'Value(Get(Widgt => Amount_Box)) > Max_Amount then
         Set(SpinBox => Amount_Box, Value => Natural'Image(Max_Amount));
      end if;
      configure
        (Widgt => Amount_Box, options => "-to" & Natural'Image(Max_Amount));
      configure
        (Widgt => Max_Button,
         options =>
           "-text {Amount (max:" & Natural'Image(Max_Amount) &
           "):} -command {" & Amount_Box & " set" & Natural'Image(Max_Amount) &
           ";" & Amount_Box & " validate}");
      return TCL_OK;
   end Update_Max_Give_Amount_Command;

   procedure Add_Cargo_Commands is
   begin
      Add_Command
        (Name => "ShowCargo", Ada_Command => Show_Cargo_Command'Access);
      Add_Command
        (Name => "ShowCargoItemInfo",
         Ada_Command => Show_Cargo_Item_Info_Command'Access);
      Add_Command
        (Name => "ShowGiveItem", Ada_Command => Show_Give_Item_Command'Access);
      Add_Command(Name => "GiveItem", Ada_Command => Give_Item_Command'Access);
      Add_Command
        (Name => "ShowDropItem", Ada_Command => Show_Drop_Item_Command'Access);
      Add_Command(Name => "DropItem", Ada_Command => Drop_Item_Command'Access);
      Add_Command
        (Name => "SortShipCargo", Ada_Command => Sort_Cargo_Command'Access);
      Add_Command
        (Name => "UpdateMaxGiveAmount",
         Ada_Command => Update_Max_Give_Amount_Command'Access);
   end Add_Cargo_Commands;

end Ships.UI.Cargo;
