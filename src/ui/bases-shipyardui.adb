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

-- with Ada.Containers.Generic_Array_Sort;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
-- with Tcl.Tk.Ada;
-- with Tcl.Tk.Ada.Widgets;
-- with Tcl.Tk.Ada.Widgets.Canvas;
-- with Tcl.Tk.Ada.Widgets.TtkFrame;
-- with Tcl.Tk.Ada.Widgets.TtkScrollbar;
-- with Tcl.Tk.Ada.Winfo;
-- with CoreUI;
-- with ShipModules; use ShipModules;
-- with Ships.Crew;
-- with Table; use Table;
with Utils.UI;

package body Bases.ShipyardUI is

   -- ****iv* ShipyardUI/ShipyardUI.Install_Table
   -- FUNCTION
   -- Table with info about the available modules
   -- SOURCE
--   Install_Table: Table_Widget (Amount => 5);
--   -- ****
--
--   -- ****iv* ShipyardUI/ShipyardUI.Remove_Table
--   -- FUNCTION
--   -- Table with info about the installed modules
--   -- SOURCE
--   Remove_Table: Table_Widget (Amount => 5);
   -- ****

   -- ****iv* ShipyardUI/ShipyardUI.Install_Indexes
   -- FUNCTION
   -- Indexes of the available modules to install
   -- SOURCE
--   Install_Indexes: Positive_Container.Vector;
--   -- ****
--
--   -- ****iv* ShipyardUI/ShipyardUI.Remove_Indexes
--   -- FUNCTION
--   -- Indexes of the modules in the player's ship (to remove)
--   -- SOURCE
--   Remove_Indexes: Positive_Container.Vector;
   -- ****

   -- ****f* ShipyardUI/ShipyardUI.Show_Shipyard_Command
   -- FUNCTION
   -- Show the selected base shipyard
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- COMMAND
   -- ShowShipyard ?moduletype? ?modulename?
   -- Show the base shipyard and load all available and installed modules
   -- lists. Moduletype is the type of modules to show in available modules,
   -- modulename is the name of the module to search in available modules.
   -- SOURCE
   function Show_Shipyard_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "showShipyardCommand";
      -- ****

--   function Show_Shipyard_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      use Tcl.Tk.Ada.Widgets;
--      use Tcl.Tk.Ada.Widgets.Canvas;
--      use Tcl.Tk.Ada.Widgets.TtkFrame;
--      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
--      use Tcl.Tk.Ada.Winfo;
--      use CoreUI;
--
--      Shipyard_Frame: Ttk_Frame :=
--        Get_Widget
--          (pathName => Main_Paned & ".shipyardframe", Interp => Interp);
--      Shipyard_Canvas: constant Tk_Canvas :=
--        Get_Widget(pathName => Shipyard_Frame & ".canvas", Interp => Interp);
--      function Show_Ada_Shipyard_Command
--        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
--         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
--         Convention => C,
--         Import => True,
--         External_Name => "showShipyardCommand";
--   begin
--      if Show_Ada_Shipyard_Command
--          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv) =
--        TCL_ERROR then
--         return TCL_ERROR;
--      end if;
--      if Winfo_Get(Widgt => Shipyard_Canvas, Info => "exists") = "0" then
--         Shipyard_Frame :=
--           Get_Widget
--             (pathName => Shipyard_Canvas & ".shipyard.install",
--              Interp => Interp);
--         Install_Table :=
--           Create_Table
--             (Parent => Widget_Image(Win => Shipyard_Frame),
--              Headers =>
--                (1 => To_Unbounded_String(Source => "Name"),
--                 2 => To_Unbounded_String(Source => "Type"),
--                 3 => To_Unbounded_String(Source => "Size"),
--                 4 => To_Unbounded_String(Source => "Materials"),
--                 5 => To_Unbounded_String(Source => "Cost")),
--              Scrollbar =>
--                Get_Widget
--                  (pathName => ".gameframe.paned.shipyardframe.scrolly"),
--              Command => "",
--              Tooltip_Text => "Press mouse button to sort the modules.");
--         --## rule off ASSIGNMENTS
--         Shipyard_Frame :=
--           Get_Widget
--             (pathName => Shipyard_Canvas & ".shipyard.remove",
--              Interp => Interp);
--         --## rule on ASSIGNMENTS
--         Remove_Table :=
--           Create_Table
--             (Parent => Widget_Image(Win => Shipyard_Frame),
--              Headers =>
--                (1 => To_Unbounded_String(Source => "Name"),
--                 2 => To_Unbounded_String(Source => "Type"),
--                 3 => To_Unbounded_String(Source => "Size"),
--                 4 => To_Unbounded_String(Source => "Materials"),
--                 5 => To_Unbounded_String(Source => "Price")),
--              Scrollbar =>
--                Get_Widget
--                  (pathName => ".gameframe.paned.shipyardframe.scrolly"),
--              Command => "SortShipyardModules remove 0 {}",
--              Tooltip_Text => "Press mouse button to sort the modules.");
--      end if;
--      if Install_Indexes.Length = 0 then
--         Fill_Install_Indexes_Loop :
--         for I in 1 .. Get_Modules_Amount loop
--            Install_Indexes.Append(New_Item => I);
--         end loop Fill_Install_Indexes_Loop;
--      end if;
--      if Remove_Indexes.Length /= Player_Ship.Modules.Length then
--         Remove_Indexes.Clear;
--         Fill_Remove_Indexes_Loop :
--         for I in Player_Ship.Modules.Iterate loop
--            Remove_Indexes.Append
--              (New_Item => Modules_Container.To_Index(Position => I));
--         end loop Fill_Remove_Indexes_Loop;
--      end if;
--      return TCL_OK;
--   end Show_Shipyard_Command;

   -- ****iv* ShipyardUI/ShipyardUI.Module_Index
   -- SOURCE
   Module_Index: Positive;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****if* ShipyardUI/ShipyardUI.Get_Module_Index
   -- FUNCTION
   -- Get the index of the currently selected module
   -- RESULT
   -- The index of the currently selected module
   -- SOURCE
   function Get_Module_Index return Positive is
      -- ****
   begin
      return Module_Index;
   end Get_Module_Index;

   -- ****if* ShipyardUI/ShipyardUI.Set_Module_Info
   -- FUNCTION
   -- Show information about selected module
   -- PARAMETERS
   -- Installing - If true, player looking at installing modules list
   -- Row        - The current row in the dialog
   -- New_Info   - If true, create the new UI for the info, otherwise reuse old
   --              one. Default value is True.
   -- SOURCE
   procedure Set_Module_Info
     (Installing: Boolean; Row: in out Positive; New_Info: Boolean := True) is
      -- ****
      procedure Set_Ada_Module_Info
        (I: Integer; R: in out Positive; N_Info, M_Index: Integer) with
         Convention => C,
         Import => True,
         External_Name => "setAdaModuleInfo";
   begin
      Set_Ada_Module_Info
        (I => (if Installing then 1 else 0), R => Row,
         N_Info => (if New_Info then 1 else 0), M_Index => Get_Module_Index);
   end Set_Module_Info;
   --## rule on REDUCEABLE_SCOPE

   -- ****f* ShipyardUI/ShipyardUI.Show_Install_Info_Command
   -- FUNCTION
   -- Show information about the selected module to install
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Install_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Install_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      function Show_Ada_Install_Info_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Convention => C,
         Import => True,
         External_Name => "showInstallInfoCommand";
   begin
      Module_Index := Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
      return
        Show_Ada_Install_Info_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv);
   end Show_Install_Info_Command;

   -- ****f* ShipyardUI/ShipyardUI.Manipulate_Module_Command
   -- FUNCTION
   -- Install or remove the selected module
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- SOURCE
   function Manipulate_Module_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "manipulateModuleCommand";
      -- ****

   -- ****f* ShipyardUI/ShipyardUI.Show_Remove_Info_Command
   -- FUNCTION
   -- Show information about the selected module to remove
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- SOURCE
   function Show_Remove_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Remove_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      function Show_Ada_Remove_Info_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Convention => C,
         Import => True,
         External_Name => "showRemoveInfoCommand";
   begin
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Module_Index := Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      return
        Show_Ada_Remove_Info_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv);
   end Show_Remove_Info_Command;

   -- ****o* ShipyardUI/ShipyardUI.Show_Shipyard_Tab_Command
   -- FUNCTION
   -- Show the install or remove modules options in shipyard
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowShipyardTab
   -- SOURCE
   function Show_Shipyard_Tab_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "showShipyardTabCommand";
      -- ****

   -- ****it* ShipyardUI/ShipyardUI.Modules_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the ship modules list
   -- OPTIONS
   -- NAMEASC      - Sort modules by name ascending
   -- NAMEDESC     - Sort modules by name descending
   -- TYPEASC      - Sort modules by type ascending
   -- TYPEDESC     - Sort modules by type descending
   -- SIZEASC      - Sort modules by size ascending
   -- SIZEDESC     - Sort modules by size descending
   -- MATERIALASC  - Sort modules by material ascending
   -- MATERIALDESC - Sort modules by material descending
   -- PRICEASC     - Sort modules by price ascending
   -- PRICEDESC    - Sort modules by price descending
   -- NONE       - No sorting modules (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
--   type Modules_Sort_Orders is
--     (NAMEASC, NAMEDESC, TYPEASC, TYPEDESC, SIZEASC, SIZEDESC, MATERIALASC,
--      MATERIALDESC, PRICEASC, PRICEDESC, NONE) with
--      Default_Value => NONE;
--      -- ****
--
--      -- ****id* ShipyardUI/ShipyardUI.Default_Modules_Sort_Order
--      -- FUNCTION
--      -- Default sorting order for the player's ship's modules
--      -- HISTORY
--      -- 6.4 - Added
--      -- SOURCE
--   Default_Modules_Sort_Order: constant Modules_Sort_Orders := NONE;
--   -- ****
--
--   --## rule off DIRECTLY_ACCESSED_GLOBALS
--   -- ****iv* ShipyardUI/ShipyardUI.Modules_Sort_Order
--   -- FUNCTION
--   -- The current sorting order for modules list
--   -- HISTORY
--   -- 6.4 - Added
--   -- SOURCE
--   Modules_Sort_Order: Modules_Sort_Orders := Default_Modules_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* ShipyardUI/ShipyardUI.Sort_Modules_Command
   -- FUNCTION
   -- Sort the ship modules lists
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipModules action moduletype page x
   -- Action is a type of action, can be install or remove, moduletype is a
   -- type of modules to show, page is the number of currently showed page
   -- of list and x is X axis coordinate where the player clicked the mouse
   -- button
   -- SOURCE
   function Sort_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C,
      Import => True,
      External_Name => "sortShipyardModulesCommand";
      -- ****

--   function Sort_Modules_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Argc);
--      use Ships.Crew;
--      use Tiny_String;
--
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
--      Column: constant Positive :=
--        Get_Column_Number
--          (Table =>
--             (if CArgv.Arg(Argv => Argv, N => 1) = "install" then Install_Table
--              else Remove_Table),
--           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 4)));
--      --## rule on DIRECTLY_ACCESSED_GLOBALS
--      --## rule off TYPE_INITIAL_VALUES
--      type Local_Module_Data is record
--         Name: Bounded_String;
--         M_Type: Unbounded_String;
--         Size: Natural;
--         Material: Bounded_String;
--         Price: Positive;
--         Id: Positive;
--      end record;
--      type Modules_Array is array(Positive range <>) of Local_Module_Data;
--      --## rule on TYPE_INITIAL_VALUES
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
--      --## rule off IMPROPER_INITIALIZATION
--      Local_Modules: Modules_Array
--        (1 ..
--             (if CArgv.Arg(Argv => Argv, N => 1) = "install" then
--                Get_Modules_Amount
--              else Positive(Player_Ship.Modules.Length)));
--      --## rule on IMPROPER_INITIALIZATION
--      --## rule on DIRECTLY_ACCESSED_GLOBALS
--      Index: Positive := 1;
--      Cost: Natural := 0;
--      Damage: Float := 0.0;
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
--      function "<"(Left, Right: Local_Module_Data) return Boolean is
--      begin
--         if Modules_Sort_Order = NAMEASC and then Left.Name < Right.Name then
--            return True;
--         end if;
--         if Modules_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
--            return True;
--         end if;
--         if Modules_Sort_Order = TYPEASC
--           and then Left.M_Type < Right.M_Type then
--            return True;
--         end if;
--         if Modules_Sort_Order = TYPEDESC
--           and then Left.M_Type > Right.M_Type then
--            return True;
--         end if;
--         if Modules_Sort_Order = SIZEASC and then Left.Size < Right.Size then
--            return True;
--         end if;
--         if Modules_Sort_Order = SIZEDESC and then Left.Size > Right.Size then
--            return True;
--         end if;
--         if Modules_Sort_Order = MATERIALASC
--           and then Left.Material < Right.Material then
--            return True;
--         end if;
--         if Modules_Sort_Order = MATERIALDESC
--           and then Left.Material > Right.Material then
--            return True;
--         end if;
--         if Modules_Sort_Order = PRICEASC
--           and then Left.Price < Right.Price then
--            return True;
--         end if;
--         if Modules_Sort_Order = PRICEDESC
--           and then Left.Price > Right.Price then
--            return True;
--         end if;
--         return False;
--      end "<";
--      --## rule on DIRECTLY_ACCESSED_GLOBALS
--      procedure Sort_Modules is new Ada.Containers.Generic_Array_Sort
--        (Index_Type => Positive, Element_Type => Local_Module_Data,
--         Array_Type => Modules_Array);
--   begin
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
--      case Column is
--         when 1 =>
--            if Modules_Sort_Order = NAMEASC then
--               Modules_Sort_Order := NAMEDESC;
--            else
--               Modules_Sort_Order := NAMEASC;
--            end if;
--         when 2 =>
--            if Modules_Sort_Order = TYPEASC then
--               Modules_Sort_Order := TYPEDESC;
--            else
--               Modules_Sort_Order := TYPEASC;
--            end if;
--         when 3 =>
--            if Modules_Sort_Order = SIZEASC then
--               Modules_Sort_Order := SIZEDESC;
--            else
--               Modules_Sort_Order := SIZEASC;
--            end if;
--         when 4 =>
--            if Modules_Sort_Order = MATERIALASC then
--               Modules_Sort_Order := MATERIALDESC;
--            else
--               Modules_Sort_Order := MATERIALASC;
--            end if;
--         when 5 =>
--            if Modules_Sort_Order = PRICEASC then
--               Modules_Sort_Order := PRICEDESC;
--            else
--               Modules_Sort_Order := PRICEASC;
--            end if;
--         when others =>
--            null;
--      end case;
--      if Modules_Sort_Order = NONE then
--         return TCL_OK;
--      end if;
--      --## rule on DIRECTLY_ACCESSED_GLOBALS
--      if CArgv.Arg(Argv => Argv, N => 1) = "install" then
--         Fill_Local_Install_Modules_Loop :
--         for I in 1 .. Get_Modules_Amount loop
--            Cost := Get_Module(Index => I).Price;
--            Count_Price
--              (Price => Cost, Trader_Index => Find_Member(Order => TALK));
--            if Cost = 0 then
--               Cost := 1;
--            end if;
--            Local_Modules(Index) :=
--              (Name => Get_Module(Index => I).Name,
--               M_Type =>
--                 To_Unbounded_String
--                   (Source => Get_Module_Type(Module_Index => I)),
--               Size =>
--                 (if Get_Module(Index => I).M_Type = HULL then
--                    Get_Module(Index => I).Max_Value
--                  else Get_Module(Index => I).Size),
--               Material => Get_Module(Index => I).Repair_Material,
--               Price => Cost, Id => I);
--            Index := Index + 1;
--         end loop Fill_Local_Install_Modules_Loop;
--      else
--         Fill_Local_Remove_Modules_Loop :
--         for I in Player_Ship.Modules.Iterate loop
--            Damage :=
--              1.0 -
--              Float(Player_Ship.Modules(I).Durability) /
--                Float(Player_Ship.Modules(I).Max_Durability);
--            Cost :=
--              Get_Module(Index => Player_Ship.Modules(I).Proto_Index).Price -
--              Integer
--                (Float
--                   (Get_Module(Index => Player_Ship.Modules(I).Proto_Index)
--                      .Price) *
--                 Damage);
--            if Cost = 0 then
--               Cost := 1;
--            end if;
--            Count_Price
--              (Price => Cost, Trader_Index => Find_Member(Order => TALK),
--               Reduce => False);
--            Local_Modules(Index) :=
--              (Name =>
--                 To_Bounded_String
--                   (Source =>
--                      To_String(Source => Player_Ship.Modules(I).Name)),
--               M_Type =>
--                 To_Unbounded_String
--                   (Source =>
--                      Get_Module_Type
--                        (Module_Index => Player_Ship.Modules(I).Proto_Index)),
--               Size =>
--                 Get_Module(Index => Player_Ship.Modules(I).Proto_Index).Size,
--               Material =>
--                 Get_Module(Index => Player_Ship.Modules(I).Proto_Index)
--                   .Repair_Material,
--               Price => Cost, Id => Modules_Container.To_Index(Position => I));
--            Index := Index + 1;
--         end loop Fill_Local_Remove_Modules_Loop;
--      end if;
--      Sort_Modules(Container => Local_Modules);
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
--      if CArgv.Arg(Argv => Argv, N => 1) = "install" then
--         Install_Indexes.Clear;
--         Fill_Install_Indexes_Loop :
--         for Module of Local_Modules loop
--            Install_Indexes.Append(New_Item => Module.Id);
--         end loop Fill_Install_Indexes_Loop;
--      else
--         Remove_Indexes.Clear;
--         Fill_Remove_Indexes_Loop :
--         for Module of Local_Modules loop
--            Remove_Indexes.Append(New_Item => Module.Id);
--         end loop Fill_Remove_Indexes_Loop;
--      end if;
--      --## rule on DIRECTLY_ACCESSED_GLOBALS
--      return
--        Show_Shipyard_Command
--          (Client_Data => Client_Data, Interp => Interp, Argc => 3,
--           Argv =>
--             CArgv.Empty & "ShowShipyard" & CArgv.Arg(Argv => Argv, N => 2) &
--             CArgv.Arg(Argv => Argv, N => 3));
--   end Sort_Modules_Command;

   -- ****o* ShipyardUI/ShipyardUI.Compare_Modules_Command
   -- FUNCTION
   -- Show the comparison between the selected modules in install info
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CompareModules
   -- SOURCE
   function Compare_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Compare_Modules_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
      Row: Positive := 3;
   begin
      Set_Module_Info(Installing => True, Row => Row, New_Info => False);
      return TCL_OK;
   end Compare_Modules_Command;

   procedure Add_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "ShowShipyard", Ada_Command => Show_Shipyard_Command'Access);
      Add_Command
        (Name => "ShowInstallInfo",
         Ada_Command => Show_Install_Info_Command'Access);
      Add_Command
        (Name => "ManipulateModule",
         Ada_Command => Manipulate_Module_Command'Access);
      Add_Command
        (Name => "ShowRemoveInfo",
         Ada_Command => Show_Remove_Info_Command'Access);
      Add_Command
        (Name => "ShowShipyardTab",
         Ada_Command => Show_Shipyard_Tab_Command'Access);
      Add_Command
        (Name => "SortShipyardModules",
         Ada_Command => Sort_Modules_Command'Access);
      Add_Command
        (Name => "CompareModules",
         Ada_Command => Compare_Modules_Command'Access);
   end Add_Commands;

end Bases.ShipyardUI;
