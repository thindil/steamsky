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
with Ada.Strings;
with Ada.Strings.Fixed;
with Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Game; use Game;
-- with Goals;
with Ships;
with Utils.UI;

package body Statistics.UI is

   -- ****iv* SUI/SUI.Goals_Indexes
   -- FUNCTION
   -- Indexes of the finished goals
   -- SOURCE
--   Goals_Indexes: Positive_Container.Vector;
   -- ****

   -- ****iv* SUI/SUI.Destroyed_Indexes
   -- FUNCTION
   -- Indexes of the destroyed ships
   -- SOURCE
   Destroyed_Indexes: Positive_Container.Vector;
   -- ****

   -- ****iv* SUI/SUI.Killed_Indexes
   -- FUNCTION
   -- Indexes of the killed mobs
   -- SOURCE
   Killed_Indexes: Positive_Container.Vector;
   -- ****
   --## rule on REDUCEABLE_SCOPE

   procedure Show_Statistics(Refresh: Boolean := False) is
      procedure Show_Ada_Statistics(Refr: Integer) with
         Import => True,
         Convention => C,
         External_Name => "showAdaStatistics";
   begin
      Show_Ada_Statistics(Refr => (if Refresh then 1 else 0));
   end Show_Statistics;

   -- ****it* SUI/SUI.Lists_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the various lists
   -- OPTIONS
   -- NAMEASC    - Sort list by name ascending
   -- NAMEDESC   - Sort list by name descending
   -- AMOUNTASC  - Sort list by amount ascending
   -- AMOUNTDESC - Sort list by amount descending
   -- NONE       - No sorting list (default)
   -- HISTORY
   -- 6.5 - Added
   -- 6.6 - Changed to List_Sort_Orders
   -- SOURCE
   type List_Sort_Orders is
     (NAMEASC, NAMEDESC, AMOUNTASC, AMOUNTDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* SUI/SUI.Default_List_Sort_Order
      -- FUNCTION
      -- Default sorting order for the various lists
      -- HISTORY
      -- 6.5 - Added
      -- 6.6 - Changed to Default_List_Sort_Order
      -- SOURCE
   Default_List_Sort_Order: constant List_Sort_Orders := NONE;
   -- ****

   --## rule off TYPE_INITIAL_VALUES
   -- ****is* SUI/SUI.Sorting_Data
   -- FUNCTION
   -- Data structure used to sort various lists
   -- PARAMETERS
   -- Name   - The name of the item (mission, goal, crafting order, etc)
   -- Amount - The amount of the item (mission, goal, crafting order, etc)
   -- Id     - The index of the item on the list
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   type Sorting_Data is record
      Name: Unbounded_String;
      Amount: Positive;
      Id: Positive;
   end record;
   -- ****

   -- ****it* SUI/SUI.Sorting_Array
   -- FUNCTION
   -- Array used to sort various lists
   -- SOURCE
   type Sorting_Array is array(Positive range <>) of Sorting_Data;
   -- ****
   --## rule on TYPE_INITIAL_VALUES

   -- ****if* SUI/SUI.Set_Sorting_Order
   -- FUNCTION
   -- Set sorting order for the selected list
   -- PARAMETERS
   -- Sorting_Order - The sorting order to set
   -- Column        - The column in ttk_tree_view whith was clicked
   -- OUTPUT
   -- Parameter Sorting_Order
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   procedure Set_Sorting_Order
     (Sorting_Order: in out List_Sort_Orders; Column: Positive) is
     -- ****
      New_Order: Integer;
      procedure Set_Ada_Sorting_Order(S_Order: in out Integer; Col: Positive) with
         Import => True,
         Convention => C,
         External_Name => "setAdaSortingOrder";
   begin
      if Sorting_Order = NONE then
         New_Order := 4;
      else
         New_Order := List_Sort_Orders'Pos(Sorting_Order) + 1;
      end if;
      Set_Ada_Sorting_Order(S_Order => New_Order, Col => Column);
      if New_Order > 0 then
         Sorting_Order := List_Sort_Orders'Val(New_Order - 1);
      else
         Sorting_Order := NONE;
      end if;
   end Set_Sorting_Order;

   -- ****o* SUI/SUI.Sort_Crafting_Command
   -- FUNCTION
   -- Sort the list of finished crafting orders
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortFinishedCrafting x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Crafting_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "sortFinishedCraftingCommand";
      -- ****

   -- ****o* SUI/SUI.Sort_Missions_Command
   -- FUNCTION
   -- Sort the list of finished missions
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortFinishedMissions x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Missions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "sortFinishedMissionsCommand";
      -- ****

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* SUI/SUI.Goals_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of finished goals
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
--   Goals_Sort_Order: List_Sort_Orders := Default_List_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* SUI/SUI.Sort_Goals_Command
   -- FUNCTION
   -- Sort the list of finished goals
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortFinishedGoals x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Goals_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "sortFinishedGoalsCommand";
      -- ****

--   function Sort_Goals_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Client_Data, Interp, Argc);
--      use Goals;
--
--      Column: constant Positive :=
--        Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
--      Proto_Index: Positive := 1;
--      Finished_Goals: constant Statistics_Container.Vector :=
--        Get_Game_Stats_List(Name => "finishedGoals");
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
--      --## rule off IMPROPER_INITIALIZATION
--      Local_Goals: Sorting_Array(1 .. Positive(Finished_Goals.Length));
--      --## rule on IMPROPER_INITIALIZATION
--      function Get_Goals_Sort_Order return List_Sort_Orders is
--      begin
--         return Goals_Sort_Order;
--      end Get_Goals_Sort_Order;
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
--      function "<"(Left, Right: Sorting_Data) return Boolean is
--      begin
--         if Get_Goals_Sort_Order = NAMEASC and then Left.Name < Right.Name then
--            return True;
--         end if;
--         if Get_Goals_Sort_Order = NAMEDESC
--           and then Left.Name > Right.Name then
--            return True;
--         end if;
--         if Get_Goals_Sort_Order = AMOUNTASC
--           and then Left.Amount < Right.Amount then
--            return True;
--         end if;
--         if Get_Goals_Sort_Order = AMOUNTDESC
--           and then Left.Amount > Right.Amount then
--            return True;
--         end if;
--         return False;
--      end "<";
--      procedure Sort_Goals is new Ada.Containers.Generic_Array_Sort
--        (Index_Type => Positive, Element_Type => Sorting_Data,
--         Array_Type => Sorting_Array);
--   begin
--      Set_Sorting_Order(Sorting_Order => Goals_Sort_Order, Column => Column);
--      if Get_Goals_Sort_Order = NONE then
--         return TCL_OK;
--      end if;
--      Fill_Local_Goals_Loop :
--      for I in Finished_Goals.Iterate loop
--         Get_Proto_Goal_Loop :
--         for J in 1 .. 256 loop
--            if Get_Goal(Index => J).Index = Finished_Goals(I).Index then
--               Proto_Index := J;
--               exit Get_Proto_Goal_Loop;
--            end if;
--         end loop Get_Proto_Goal_Loop;
--         Local_Goals(Statistics_Container.To_Index(Position => I)) :=
--           (Name =>
--              To_Unbounded_String(Source => Goal_Text(Index => Proto_Index)),
--            Amount => Finished_Goals(I).Amount,
--            Id => Statistics_Container.To_Index(Position => I));
--      end loop Fill_Local_Goals_Loop;
--      Sort_Goals(Container => Local_Goals);
--      Goals_Indexes.Clear;
--      Fill_Goals_Indexes_Loop :
--      for Goal of Local_Goals loop
--         Goals_Indexes.Append(New_Item => Goal.Id);
--      end loop Fill_Goals_Indexes_Loop;
--      Show_Statistics(Refresh => True);
--      return TCL_OK;
--   end Sort_Goals_Command;

   -- ****iv* SUI/SUI.Destroyed_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of destroyed enemy ships
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   Destroyed_Sort_Order: List_Sort_Orders := Default_List_Sort_Order;
   -- ****

   -- ****o* SUI/SUI.Sort_Destroyed_Command
   -- FUNCTION
   -- Sort the list of destroyed enemy ships
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortDestroyedShips x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Destroyed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Destroyed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ships;

      Column: constant Positive :=
        Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
      Destroyed_Ships: constant Statistics_Container.Vector :=
        Get_Game_Stats_List(Name => "destroyedShips");
      --## rule off IMPROPER_INITIALIZATION
      Local_Destroyed: Sorting_Array(1 .. Positive(Destroyed_Ships.Length));
      --## rule on IMPROPER_INITIALIZATION
      function "<"(Left, Right: Sorting_Data) return Boolean is
      begin
         if Destroyed_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Destroyed_Sort_Order = NAMEDESC
           and then Left.Name > Right.Name then
            return True;
         end if;
         if Destroyed_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Destroyed_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Destroyed is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Sorting_Data,
         Array_Type => Sorting_Array);
   begin
      Set_Sorting_Order
        (Sorting_Order => Destroyed_Sort_Order, Column => Column);
      if Destroyed_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Destroyed_Loop :
      for I in Destroyed_Ships.Iterate loop
         Get_Proto_Ship_Loop :
         for J in 1 .. Get_Proto_Ships_Amount loop
            if To_Unbounded_String
                (Source => Trim(Source => Positive'Image(J), Side => Left)) =
              Destroyed_Ships(I).Index then
               Local_Destroyed(Statistics_Container.To_Index(Position => I)) :=
                 (Name =>
                    To_Unbounded_String
                      (Source =>
                         Tiny_String.To_String
                           (Source => Get_Proto_Ship(Proto_Index => J).Name)),
                  Amount => Destroyed_Ships(I).Amount,
                  Id => Statistics_Container.To_Index(Position => I));
               exit Get_Proto_Ship_Loop;
            end if;
         end loop Get_Proto_Ship_Loop;
      end loop Fill_Local_Destroyed_Loop;
      Sort_Destroyed(Container => Local_Destroyed);
      Destroyed_Indexes.Clear;
      Fill_Destroyed_Indexes_Loop :
      for Ship of Local_Destroyed loop
         Destroyed_Indexes.Append(New_Item => Ship.Id);
      end loop Fill_Destroyed_Indexes_Loop;
      Show_Statistics(Refresh => True);
      return TCL_OK;
   end Sort_Destroyed_Command;

   -- ****iv* SUI/SUI.Killed_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of killed enemies
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   Killed_Sort_Order: List_Sort_Orders := Default_List_Sort_Order;
   -- ****

   -- ****o* SUI/SUI.Sort_Killed_Command
   -- FUNCTION
   -- Sort the list of killed enemies
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortKilledEnemies x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Killed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Killed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Column: constant Positive :=
        Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
      Killed_Mobs: constant Statistics_Container.Vector :=
        Get_Game_Stats_List(Name => "killedMobs");
      --## rule off IMPROPER_INITIALIZATION
      Local_Killed: Sorting_Array(1 .. Positive(Killed_Mobs.Length));
      --## rule on IMPROPER_INITIALIZATION
      function "<"(Left, Right: Sorting_Data) return Boolean is
      begin
         if Killed_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Killed_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Killed_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Killed_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Killed is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Sorting_Data,
         Array_Type => Sorting_Array);
   begin
      Set_Sorting_Order(Sorting_Order => Killed_Sort_Order, Column => Column);
      if Killed_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Killed_Loop :
      for I in Killed_Mobs.Iterate loop
         Local_Killed(Statistics_Container.To_Index(Position => I)) :=
           (Name => Killed_Mobs(I).Index, Amount => Killed_Mobs(I).Amount,
            Id => Statistics_Container.To_Index(Position => I));
      end loop Fill_Local_Killed_Loop;
      Sort_Killed(Container => Local_Killed);
      Killed_Indexes.Clear;
      Fill_Killed_Indexes_Loop :
      for Mob of Local_Killed loop
         Killed_Indexes.Append(New_Item => Mob.Id);
      end loop Fill_Killed_Indexes_Loop;
      Show_Statistics(Refresh => True);
      return TCL_OK;
   end Sort_Killed_Command;

   procedure Add_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "SortFinishedCrafting",
         Ada_Command => Sort_Crafting_Command'Access);
      Add_Command
        (Name => "SortFinishedMissions",
         Ada_Command => Sort_Missions_Command'Access);
      Add_Command
        (Name => "SortFinishedGoals",
         Ada_Command => Sort_Goals_Command'Access);
      Add_Command
        (Name => "SortDestroyedShips",
         Ada_Command => Sort_Destroyed_Command'Access);
      Add_Command
        (Name => "SortKilledMobs", Ada_Command => Sort_Killed_Command'Access);
   end Add_Commands;

end Statistics.UI;
