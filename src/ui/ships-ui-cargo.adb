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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
-- with Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with CoreUI;
with Crew.Inventory; use Crew.Inventory;
with Dialogs; use Dialogs;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Missions;
with Ships.Cargo; use Ships.Cargo;
with Stories;
with Utils.UI; use Utils.UI;

package body Ships.UI.Cargo is

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
      Import => True,
      Convention => C,
      External_Name => "showCargoCommand";
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
      Import => True,
      Convention => C,
      External_Name => "sortCargoCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "showGiveItemCommand";
      -- ****

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
--      pragma Unreferenced(Argc);
--      use Tcl.Tk.Ada.Widgets.Toplevel;
      use CoreUI;
--      use Tiny_String;
--
--      Member_Index, Amount: Positive;
--      Item_Index: constant Positive :=
--        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
--      Item: constant Inventory_Data :=
--        Inventory_Container.Element
--          (Container => Player_Ship.Cargo, Index => Item_Index);
--      Item_Dialog: Tk_Toplevel :=
--        Get_Widget(pathName => ".itemdialog", Interp => Interp);
--      Spin_Box: constant Ttk_SpinBox :=
--        Get_Widget(pathName => Item_Dialog & ".giveamount");
--      Combo_Box: constant Ttk_ComboBox :=
--        Get_Widget(pathName => Item_Dialog & ".member");
      function Give_Ada_Item_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Import => True,
         Convention => C,
         External_Name => "giveItemCommand";
   begin
--      Amount := Natural'Value(Get(Widgt => Spin_Box));
--      Member_Index := Natural'Value(Current(ComboBox => Combo_Box)) + 1;
--      --## rule off SIMPLIFIABLE_EXPRESSIONS
--      if Free_Inventory
--          (Member_Index => Member_Index,
--           Amount =>
--             -(Get_Proto_Item(Index => Item.Proto_Index).Weight * Amount)) <
--        0 then
--         Show_Message
--           (Text =>
--              "No free space in " &
--              To_String(Source => Player_Ship.Crew(Member_Index).Name) &
--              "'s inventory for that amount of " & Get_Item_Name(Item => Item),
--            Title => "Can't give item");
--         return TCL_OK;
--      end if;
--      --## rule on SIMPLIFIABLE_EXPRESSIONS
--      Add_Message
--        (Message =>
--           "You gave" & Positive'Image(Amount) & " " &
--           Get_Item_Name
--             (Item =>
--                Inventory_Container.Element
--                  (Container => Player_Ship.Cargo, Index => Item_Index)) &
--           " to " & To_String(Source => Player_Ship.Crew(Member_Index).Name) &
--           ".",
--         M_Type => OTHERMESSAGE);
--      Update_Inventory
--        (Member_Index => Member_Index, Amount => Amount,
--         Proto_Index => Item.Proto_Index, Durability => Item.Durability,
--         Price => Item.Price, Ship => Player_Ship);
--      Update_Cargo
--        (Ship => Player_Ship, Amount => -Amount, Cargo_Index => Item_Index,
--         Price => Item.Price);
--      Destroy(Widgt => Item_Dialog);
      Tcl.Tk.Ada.Busy.Forget(Window => Main_Paned);
      Tcl.Tk.Ada.Busy.Forget(Window => Game_Header);
      return
        Give_Ada_Item_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv);
--      Update_Header;
--      Update_Messages;
--      return
--        Sort_Cargo_Command
--          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
--           Argv => CArgv.Empty & "SortShipCargo" & "-1");
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
      use Tcl.Tk.Ada.Widgets.TtkFrame;
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
      use Tcl.Tk.Ada.Widgets.TtkButton;

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
