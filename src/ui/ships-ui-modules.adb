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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
-- with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkFrame;
-- with Tcl.Tk.Ada.Widgets.TtkWidget;
with Dialogs;
with Table; use Table;
with Utils.UI;
with ShipModules;

package body Ships.UI.Modules is

   -- ****iv* SUModules/SUModules.Modules_Table
   -- FUNCTION
   -- Table with info about the installed modules on the player ship
   -- SOURCE
   Modules_Table: Table_Widget (Amount => 3);
   -- ****

   -- ****iv* SUModules/SUModules.Modules_Indexes
   -- FUNCTION
   -- Indexes of the player ship modules
   -- SOURCE
   Modules_Indexes: Positive_Container.Vector;
   -- ****

   -- ****o* SUModules/SUModules.Get_Active_Button_Command
   -- FUNCTION
   -- Get the next active button in assing crew dialog
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GetActiveButton crewindex
   -- Crewindex is the index of the crew member which is currently selected
   -- or 0 for close button
   -- SOURCE
--   function Get_Active_Button_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
--      Convention => C;
--      -- ****
--
--   function Get_Active_Button_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Client_Data, Argc);
--      use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
--      use Tcl.Tk.Ada.Widgets.TtkWidget;
--
--      Crew_Index: constant Natural :=
--        Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
--      Button_Name: Unbounded_String := Null_Unbounded_String;
--      Button: Ttk_CheckButton; --## rule line off IMPROPER_INITIALIZATION
--   begin
--      Find_Active_Button_Loop :
--      for I in Player_Ship.Crew.Iterate loop
--         Button_Name :=
--           To_Unbounded_String
--             (Source =>
--                ".moduledialog.canvas.frame.crewbutton" &
--                Trim
--                  (Source =>
--                     Positive'Image(Crew_Container.To_Index(Position => I)),
--                   Side => Left));
--         Button :=
--           Get_Widget
--             (pathName => To_String(Source => Button_Name), Interp => Interp);
--         exit Find_Active_Button_Loop when InState
--             (Widget => Button, StateSpec => "disabled") =
--           "0" and
--           Crew_Container.To_Index(Position => I) > Crew_Index;
--         Button_Name := Null_Unbounded_String;
--      end loop Find_Active_Button_Loop;
--      if Button_Name = Null_Unbounded_String then
--         Button_Name := To_Unbounded_String(Source => ".moduledialog.button");
--      end if;
--      Button :=
--        Get_Widget
--          (pathName => To_String(Source => Button_Name), Interp => Interp);
--      Focus(Widgt => Button);
--      return TCL_OK;
--   end Get_Active_Button_Command;

   procedure Update_Modules_Info(Page: Positive := 1) is
      --## rule off TYPE_INITIAL_VALUES
      type Modules_Array is array(0 .. 50) of Natural;
      --## rule on TYPE_INITIAL_VALUES
      M_Array: Modules_Array := (others => 0);
      N_Width: Nim_Width := (others => 0);
      Index: Natural := 0;
      procedure Update_Ada_Modules_Info
        (P: Positive; M: Modules_Array; W: out Nim_Width) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaModulesInfo";
   begin
      if Modules_Indexes.Length /= Player_Ship.Modules.Length then
         Modules_Indexes.Clear;
         Update_Modules_Indexes_Loop :
         for I in Player_Ship.Modules.Iterate loop
            Modules_Indexes.Append
              (New_Item => Modules_Container.To_Index(Position => I));
         end loop Update_Modules_Indexes_Loop;
      end if;
      Convert_Modules_Indexes_Loop :
      for M_Index of Modules_Indexes loop
         M_Array(Index) := M_Index;
         Index := Index + 1;
      end loop Convert_Modules_Indexes_Loop;
      Update_Ada_Modules_Info(P => Page, M => M_Array, W => N_Width);
      Index := 1;
      Convert_Headers_Width_Loop :
      for Width of N_Width loop
         exit Convert_Headers_Width_Loop when Width = 0;
         Modules_Table.Columns_Width(Index) := Width;
         Index := Index + 1;
      end loop Convert_Headers_Width_Loop;
   end Update_Modules_Info;

   -- ****o* SUModules/SUModules.Show_Modules_Command
   -- FUNCTION
   -- Show the list of the player's ship modules to a player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowModules ?page?
   -- Page parameter is a index of page from which starts showing
   -- modules.
   -- SOURCE
   function Show_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
   begin
      Update_Modules_Info
        (Page => Positive'Value(CArgv.Arg(Argv => Argv, N => 1)));
      return TCL_OK;
   end Show_Modules_Command;

   -- ****it* SUModules/SUModules.Modules_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the ship modules list
   -- OPTIONS
   -- NAMEASC    - Sort modules by name ascending
   -- NAMEDESC   - Sort modules by name descending
   -- DAMAGEASC  - Sort modules by damage ascending
   -- DAMAGEDESC - Sort modules by damage descending
   -- INFOASC    - Sort modules by info ascending
   -- INFODESC   - Sort modules by info descending
   -- NONE       - No sorting modules (default)
   -- HISTORY
   -- 6.4 - Added
   -- 8.9 - Added sorting by info column
   -- SOURCE
   type Modules_Sort_Orders is
     (NAMEASC, NAMEDESC, DAMAGEASC, DAMAGEDESC, INFOASC, INFODESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* SUModules/SUModules.Default_Modules_Sort_Order
      -- FUNCTION
      -- Default sorting order for the player's ship's modules
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Modules_Sort_Order: constant Modules_Sort_Orders := NONE;
   -- ****

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* SUModules/SUModules.Modules_Sort_Order
   -- FUNCTION
   -- The current sorting order for modules list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Modules_Sort_Order: Modules_Sort_Orders := Default_Modules_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* SUModules/SUModules.Sort_Modules_Command
   -- FUNCTION
   -- Sort the player's ship's modules list
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipModules x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Column: constant Positive :=
        Get_Column_Number
          (Table => Modules_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      --## rule off TYPE_INITIAL_VALUES
      type Local_Module_Data is record
         Name: Bounded_String;
         Damage: Float;
         Id: Positive;
         Info: Unbounded_String;
      end record;
      type Modules_Array is array(Positive range <>) of Local_Module_Data;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Local_Modules: Modules_Array(1 .. Positive(Player_Ship.Modules.Length));
      --## rule on IMPROPER_INITIALIZATION
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      function Get_Modules_Sort_Order return Modules_Sort_Orders is
      begin
         return Modules_Sort_Order;
      end Get_Modules_Sort_Order;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Local_Module_Data) return Boolean is
      begin
         if Get_Modules_Sort_Order = NAMEASC
           and then Left.Name < Right.Name then
            return True;
         end if;
         if Get_Modules_Sort_Order = NAMEDESC
           and then Left.Name > Right.Name then
            return True;
         end if;
         if Get_Modules_Sort_Order = DAMAGEASC
           and then Left.Damage < Right.Damage then
            return True;
         end if;
         if Get_Modules_Sort_Order = DAMAGEDESC
           and then Left.Damage > Right.Damage then
            return True;
         end if;
         if Get_Modules_Sort_Order = INFOASC
           and then Left.Info < Right.Info then
            return True;
         end if;
         if Get_Modules_Sort_Order = INFODESC
           and then Left.Info > Right.Info then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Modules is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Module_Data,
         Array_Type => Modules_Array);
      function Get_Module_Info(Module_Index: Positive) return String is
         use Interfaces.C.Strings;
         function Get_Ada_Module_Info(M_Index: Positive) return chars_ptr with
            Import => True,
            Convention => C,
            External_Name => "getAdaModuleInfo";
      begin
         return Value(Item => Get_Ada_Module_Info(M_Index => Module_Index));
      end Get_Module_Info;
   begin
      case Column is
         when 1 =>
            if Get_Modules_Sort_Order = NAMEASC then
               Modules_Sort_Order := NAMEDESC;
            else
               Modules_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Get_Modules_Sort_Order = DAMAGEASC then
               Modules_Sort_Order := DAMAGEDESC;
            else
               Modules_Sort_Order := DAMAGEASC;
            end if;
         when 3 =>
            if Get_Modules_Sort_Order = INFOASC then
               Modules_Sort_Order := INFODESC;
            else
               Modules_Sort_Order := INFOASC;
            end if;
         when others =>
            null;
      end case;
      if Get_Modules_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Modules_Loop :
      for I in Player_Ship.Modules.Iterate loop
         Local_Modules(Modules_Container.To_Index(Position => I)) :=
           (Name => Player_Ship.Modules(I).Name,
            Damage =>
              Float(Player_Ship.Modules(I).Durability) /
              Float(Player_Ship.Modules(I).Max_Durability),
            Id => Modules_Container.To_Index(Position => I),
            Info =>
              To_Unbounded_String
                (Source =>
                   Get_Module_Info
                     (Module_Index =>
                        Modules_Container.To_Index(Position => I))));
      end loop Fill_Local_Modules_Loop;
      Sort_Modules(Container => Local_Modules);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Modules_Indexes.Clear;
      Fill_Modules_Indexes_Loop :
      for Module of Local_Modules loop
         Modules_Indexes.Append(New_Item => Module.Id);
      end loop Fill_Modules_Indexes_Loop;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Update_Modules_Info;
      return TCL_OK;
   end Sort_Modules_Command;

   -- ****o* SUModules/SUModules.Show_Assign_Ammo_Command
   -- FUNCTION
   -- Show the list of available ammo for the selected gun
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssingAmmo index
   -- Index is the module index of the selected gun which will be have
   -- assigned a new ammo
   -- SOURCE
   function Show_Assign_Ammo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Ammo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Dialogs;
      use ShipModules;
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Ammo_Index: constant Natural :=
        (if Player_Ship.Modules(Module_Index).M_Type = GUN then
           Player_Ship.Modules(Module_Index).Ammo_Index
         else Player_Ship.Modules(Module_Index).Harpoon_Index);
      Ammo_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".ammomenu", Title => "Available ammo", Parent_Name => ".");
      Row: Positive := 1;
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Ammo_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Ammo_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Ammo_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Ammo_Menu & ".ammo1;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Find_Ammo_Loop :
      for I in
        Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
          Inventory_Container.Last_Index(Container => Player_Ship.Cargo) loop
         if Get_Proto_Item
             (Index =>
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => I)
                  .Proto_Index)
             .I_Type =
           Get_Ada_Item_Type
             (Item_Index =>
                Get_Module
                  (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                  .Value -
                1) and
           I /= Ammo_Index then
            Add_Button
              (Name =>
                 ".ammo" & Trim(Source => Positive'Image(Row), Side => Left),
               Label =>
                 To_String
                   (Source =>
                      Get_Proto_Item
                        (Index =>
                           Inventory_Container.Element
                             (Container => Player_Ship.Cargo, Index => I)
                             .Proto_Index)
                        .Name),
               Command =>
                 "AssignModule ammo " & CArgv.Arg(Argv => Argv, N => 1) &
                 Positive'Image(I));
            Row := Row + 1;
         end if;
      end loop Find_Ammo_Loop;
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Ammo_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Assign_Ammo_Command;

   procedure Add_Modules_Commands is
      use Utils.UI;
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaModulesCommands";
   begin
      Add_Ada_Commands;
--      Add_Command
--        (Name => "GetActiveButton",
--         Ada_Command => Get_Active_Button_Command'Access);
      Add_Command
        (Name => "ShowModules", Ada_Command => Show_Modules_Command'Access);
      Add_Command
        (Name => "SortShipModules",
         Ada_Command => Sort_Modules_Command'Access);
      Add_Command
        (Name => "ShowAssignAmmo",
         Ada_Command => Show_Assign_Ammo_Command'Access);
   end Add_Modules_Commands;

end Ships.UI.Modules;
