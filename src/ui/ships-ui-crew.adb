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
-- with Ada.Strings; use Ada.Strings;
-- with Ada.Strings.Fixed; use Ada.Strings.Fixed;
-- with Interfaces.C; use Interfaces.C;
-- with Interfaces.C.Strings;
-- with CArgv; use CArgv;
with Tcl; use Tcl;
-- with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
-- with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with CoreUI; use CoreUI;
-- with Game;
-- with Ships.Crew;
with Ships.UI.Crew.Inventory;
with Table; use Table;
-- with Utils;
-- with Utils.UI;

package body Ships.UI.Crew is

   -- ****iv* SUCrew/SUCrew.Crew_Table
   -- FUNCTION
   -- Table with info about the player's ship crew
   -- SOURCE
   Crew_Table: Table_Widget (Amount => 9);
   -- ****

   -- ****iv* SUCrew/SUCrew.Modules_Indexes
   -- FUNCTION
   -- Indexes of the player ship crew
   -- SOURCE
   Crew_Indexes: Positive_Container.Vector;
   -- ****

   procedure Update_Crew_Info(Page: Positive := 1; Skill: Natural := 0) is
      use Tcl.Tk.Ada.Widgets.Canvas;

      --## rule off TYPE_INITIAL_VALUES
      type Crew_Array is array(0 .. 50) of Natural;
      --## rule on TYPE_INITIAL_VALUES
      C_Array: Crew_Array := (others => 0);
      N_Width: Nim_Width := (others => 0);
      Index: Natural := 0;
      procedure Update_Ada_Crew_Info
        (P: Positive; S: Natural; M: Crew_Array; W: out Nim_Width;
         Row, Height: out Positive) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaCrewInfo";
   begin
      if Crew_Indexes.Length /= Player_Ship.Crew.Length then
         Crew_Indexes.Clear;
         Update_Crew_Indexes_Loop :
         for I in Player_Ship.Crew.Iterate loop
            Crew_Indexes.Append
              (New_Item => Crew_Container.To_Index(Position => I));
         end loop Update_Crew_Indexes_Loop;
      end if;
      Convert_Crew_Indexes_Loop :
      for C_Index of Crew_Indexes loop
         C_Array(Index) := C_Index;
         Index := Index + 1;
      end loop Convert_Crew_Indexes_Loop;
      Update_Ada_Crew_Info
        (P => Page, S => Skill, M => C_Array, W => N_Width,
         Row => Crew_Table.Row, Height => Crew_Table.Row_Height);
      Index := 1;
      Convert_Headers_Width_Loop :
      for Width of N_Width loop
         exit Convert_Headers_Width_Loop when Width = 0;
         Crew_Table.Columns_Width(Index) := Width;
         Index := Index + 1;
      end loop Convert_Headers_Width_Loop;
      Crew_Table.Canvas :=
        Get_Widget
          (pathName => Main_Paned & ".shipinfoframe.crew.canvas.frame.table");
   end Update_Crew_Info;

   -- ****it* SUCrew/SUCrew.Crew_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the player ship crew list
   -- OPTIONS
   -- SELECTEDASC - Sort members by selected ascending
   -- SELETEDDESC - Sort members by selected descending
   -- NAMEASC     - Sort members by name ascending
   -- NAMEDESC    - Sort members by name descending
   -- ORDERASC    - Sort members by order ascending
   -- ORDERDESC   - Sort members by order descending
   -- SKILLASC    - Sort members by skill ascending
   -- SKILLDESC   - Sort members by skill descending
   -- HEALTHASC   - Sort members by health ascending
   -- HEALTHDESC  - Sort members by health descending
   -- FATIGUEASC  - Sort members by fatigue ascending
   -- FATIGUEDESC - Sort members by fatigue descending
   -- THIRTSASC   - Sort members by thirst ascending
   -- THIRSTDESC  - Sort members by thirst descending
   -- HUNGERASC   - Sort members by hunger ascending
   -- HUNGERDESC  - Sort members by hunger descending
   -- MORALEASC   - Sort members by morale ascending
   -- MORALEDESC  - Sort members by morale descending
   -- NONE        - No sorting crew (default)
   -- HISTORY
   -- 6.4 - Added
   -- 8.5 - Added SELECTEDASC and SELECTEDDESC values
   -- SOURCE
--   type Crew_Sort_Orders is
--     (SELECTEDASC, SELECTEDDESC, NAMEASC, NAMEDESC, ORDERASC, ORDERDESC,
--      SKILLASC, SKILLDESC, HEALTHASC, HEALTHDESC, FATIGUEASC, FATIGUEDESC,
--      THIRSTASC, THIRSTDESC, HUNGERASC, HUNGERDESC, MORALEASC, MORALEDESC,
--      NONE) with
--      Default_Value => NONE;
      -- ****

      -- ****id* SUCrew/SUCrew.Default_Crew_Sort_Order
      -- FUNCTION
      -- Default sorting order for the player's ship's crew
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
--   Default_Crew_Sort_Order: constant Crew_Sort_Orders := NONE;
   -- ****

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* SUCrew/SUCrew.Crew_Sort_Order
   -- FUNCTION
   -- The current sorting order of the player's ship's crew
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
--   Crew_Sort_Order: Crew_Sort_Orders := Default_Crew_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* SUCrew/SUCrew.Sort_Crew_Command
   -- FUNCTION
   -- Sort the player's ship's crew list
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipCrew x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
--   function Sort_Crew_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
--      Convention => C;
--      -- ****
--
--   function Sort_Crew_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Client_Data, Argc);
--      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
--      use Game.Tiny_String;
--      use Ships.Crew;
--
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
--      Column: constant Positive :=
--        (if CArgv.Arg(Argv => Argv, N => 1) = "-1" then Positive'Last
--         else Get_Column_Number
--             (Table => Crew_Table,
--              X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1))));
--      --## rule on DIRECTLY_ACCESSED_GLOBALS
--      Skill_Box: constant Ttk_ComboBox :=
--        Get_Widget
--          (pathName =>
--             Main_Paned &
--             ".shipinfoframe.crew.canvas.frame.selectskill.combox",
--           Interp => Interp);
--      Skill_Index: constant Natural :=
--        Natural(Find_Skill_Index(Skill_Name => Get(Widgt => Skill_Box)));
--      --## rule off TYPE_INITIAL_VALUES
--      type Local_Member_Data is record
--         Selected: Boolean;
--         Name: Tiny_String.Bounded_String;
--         Order: Crew_Orders;
--         Skill: Tiny_String.Bounded_String;
--         Health: Skill_Range;
--         Fatigue: Integer;
--         Thirst: Skill_Range;
--         Hunger: Skill_Range;
--         Morale: Skill_Range;
--         Id: Positive;
--      end record;
--      type Crew_Array is array(Positive range <>) of Local_Member_Data;
--      --## rule on TYPE_INITIAL_VALUES
--      Local_Crew: Crew_Array(1 .. Positive(Player_Ship.Crew.Length)) :=
--        (others => <>);
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
--      function Get_Crew_Sort_Order return Crew_Sort_Orders is
--      begin
--         return Crew_Sort_Order;
--      end Get_Crew_Sort_Order;
--      --## rule on DIRECTLY_ACCESSED_GLOBALS
--      function "<"(Left, Right: Local_Member_Data) return Boolean is
--      begin
--         if Get_Crew_Sort_Order = SELECTEDASC
--           and then Left.Selected < Right.Selected then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = SELECTEDDESC
--           and then Left.Selected > Right.Selected then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = NAMEASC and then Left.Name < Right.Name then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = ORDERASC
--           and then Crew_Orders'Image(Left.Order) <
--             Crew_Orders'Image(Right.Order) then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = ORDERDESC
--           and then Crew_Orders'Image(Left.Order) >
--             Crew_Orders'Image(Right.Order) then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = SKILLASC
--           and then Left.Skill < Right.Skill then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = SKILLDESC
--           and then Left.Skill > Right.Skill then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = HEALTHASC
--           and then Left.Health < Right.Health then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = HEALTHDESC
--           and then Left.Health > Right.Health then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = FATIGUEASC
--           and then Left.Fatigue < Right.Fatigue then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = FATIGUEDESC
--           and then Left.Fatigue > Right.Fatigue then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = THIRSTASC
--           and then Left.Thirst < Right.Thirst then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = THIRSTDESC
--           and then Left.Thirst > Right.Thirst then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = HUNGERASC
--           and then Left.Hunger < Right.Hunger then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = HUNGERDESC
--           and then Left.Hunger > Right.Hunger then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = MORALEASC
--           and then Left.Morale < Right.Morale then
--            return True;
--         end if;
--         if Get_Crew_Sort_Order = MORALEDESC
--           and then Left.Morale > Right.Morale then
--            return True;
--         end if;
--         return False;
--      end "<";
--      procedure Sort_Crew is new Ada.Containers.Generic_Array_Sort
--        (Index_Type => Positive, Element_Type => Local_Member_Data,
--         Array_Type => Crew_Array);
--      function Get_Highest_Skill(Member_Index: Positive) return String is
--         use Interfaces.C.Strings;
--
--         function Get_Ada_Highest_Skill
--           (M_Index: Positive) return chars_ptr with
--            Import => True,
--            Convention => C,
--            External_Name => "getAdaHighestSkill";
--      begin
--         return Value(Item => Get_Ada_Highest_Skill(M_Index => Member_Index));
--      end Get_Highest_Skill;
--   begin
--      case Column is
--         when 1 =>
--            if Get_Crew_Sort_Order = SELECTEDASC then
--               Crew_Sort_Order := SELECTEDDESC;
--            else
--               Crew_Sort_Order := SELECTEDASC;
--            end if;
--         when 2 =>
--            if Get_Crew_Sort_Order = NAMEASC then
--               Crew_Sort_Order := NAMEDESC;
--            else
--               Crew_Sort_Order := NAMEASC;
--            end if;
--         when 3 =>
--            if Get_Crew_Sort_Order = ORDERASC then
--               Crew_Sort_Order := ORDERDESC;
--            else
--               Crew_Sort_Order := ORDERASC;
--            end if;
--         when 4 =>
--            if Get_Crew_Sort_Order = SKILLASC then
--               Crew_Sort_Order := SKILLDESC;
--            else
--               Crew_Sort_Order := SKILLASC;
--            end if;
--         when 5 =>
--            if Get_Crew_Sort_Order = HEALTHASC then
--               Crew_Sort_Order := HEALTHDESC;
--            else
--               Crew_Sort_Order := HEALTHASC;
--            end if;
--         when 6 =>
--            if Get_Crew_Sort_Order = FATIGUEASC then
--               Crew_Sort_Order := FATIGUEDESC;
--            else
--               Crew_Sort_Order := FATIGUEASC;
--            end if;
--         when 7 =>
--            if Get_Crew_Sort_Order = THIRSTASC then
--               Crew_Sort_Order := THIRSTDESC;
--            else
--               Crew_Sort_Order := THIRSTASC;
--            end if;
--         when 8 =>
--            if Get_Crew_Sort_Order = HUNGERASC then
--               Crew_Sort_Order := HUNGERDESC;
--            else
--               Crew_Sort_Order := HUNGERASC;
--            end if;
--         when 9 =>
--            if Get_Crew_Sort_Order = MORALEASC then
--               Crew_Sort_Order := MORALEDESC;
--            else
--               Crew_Sort_Order := MORALEASC;
--            end if;
--         when others =>
--            null;
--      end case;
--      if Get_Crew_Sort_Order = NONE then
--         if Column = Positive'Last then
--            Update_Crew_Info(Skill => Skill_Index);
--         end if;
--         return TCL_OK;
--      end if;
--      Fill_Local_Crew_Loop :
--      for I in Player_Ship.Crew.Iterate loop
--         Local_Crew(Crew_Container.To_Index(Position => I)) :=
--           (Selected =>
--              (if
--                 Tcl_GetVar
--                   (interp => Interp,
--                    varName =>
--                      "crewindex" &
--                      Trim
--                        (Source => Crew_Container.To_Index(Position => I)'Img,
--                         Side => Left)) =
--                 "1"
--               then True
--               else False),
--            Name => Player_Ship.Crew(I).Name,
--            Order => Player_Ship.Crew(I).Order,
--            Skill =>
--              To_Bounded_String
--                (Source =>
--                   (if Skill_Index = 0 then
--                      Get_Highest_Skill
--                        (Member_Index =>
--                           Crew_Container.To_Index(Position => I))
--                    else Get_Skill_Level_Name
--                        (Skill_Level =>
--                           Get_Skill_Level
--                             (Member => Player_Ship.Crew(I),
--                              Skill_Index =>
--                                Skills_Amount_Range(Skill_Index))))),
--            Health => Player_Ship.Crew(I).Health,
--            Fatigue =>
--              Player_Ship.Crew(I).Tired -
--              Player_Ship.Crew(I).Attributes(Positive(Condition_Index)).Level,
--            Thirst => Player_Ship.Crew(I).Thirst,
--            Hunger => Player_Ship.Crew(I).Hunger,
--            Morale => Player_Ship.Crew(I).Morale(1),
--            Id => Crew_Container.To_Index(Position => I));
--      end loop Fill_Local_Crew_Loop;
--      Sort_Crew(Container => Local_Crew);
--      Crew_Indexes.Clear; --## rule line off DIRECTLY_ACCESSED_GLOBALS
--      Fill_Crew_Indexes_Loop :
--      for Member of Local_Crew loop
--         --## rule off DIRECTLY_ACCESSED_GLOBALS
--         Crew_Indexes.Append(New_Item => Member.Id);
--         --## rule on DIRECTLY_ACCESSED_GLOBALS
--      end loop Fill_Crew_Indexes_Loop;
--      Update_Crew_Info(Skill => Natural'Value(Current(ComboBox => Skill_Box)));
--      return TCL_OK;
--   end Sort_Crew_Command;
   --## rule on REDUCEABLE_SCOPE

   -- ****o* SUCrew/SUCrew.Toggle_All_Crew_Command
   -- FUNCTION
   -- Select or deselect all crew members
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleAllCrew action
   -- Action is the action which will be performed. Possible values are
   -- select or deselect
   -- SOURCE
--   function Toggle_All_Crew_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
--      Convention => C;
--      -- ****
--
--   function Toggle_All_Crew_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Argc);
--      procedure Reset_Selection(Interpreter: Tcl_Interp) is
--      begin
--         Reset_Crew_Selection_Loop :
--         for I in
--           1 .. Crew_Container.Length(Container => Player_Ship.Crew) loop
--            if Tcl_GetVar
--                (interp => Interpreter,
--                 varName =>
--                   "crewindex" & Trim(Source => I'Img, Side => Left)) =
--              "1" then
--               Tcl_UnsetVar
--                 (interp => Interpreter,
--                  varName =>
--                    "crewindex" & Trim(Source => I'Img, Side => Left));
--            end if;
--         end loop Reset_Crew_Selection_Loop;
--      end Reset_Selection;
--   begin
--      if CArgv.Arg(Argv => Argv, N => 1) = "unselect" then
--         Reset_Selection(Interpreter => Interp);
--      else
--         Set_Crew_Selection_Loop :
--         for I in
--           1 .. Crew_Container.Length(Container => Player_Ship.Crew) loop
--            Tcl_SetVar
--              (interp => Interp,
--               varName => "crewindex" & Trim(Source => I'Img, Side => Left),
--               newValue => "1");
--         end loop Set_Crew_Selection_Loop;
--      end if;
--      return
--        Sort_Crew_Command
--          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
--           Argv => CArgv.Empty & "SortShipCrew" & "-1");
--   end Toggle_All_Crew_Command;

   procedure Add_Crew_Commands is
--      use Utils.UI;

      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaCrewCommands";
   begin
      Add_Ada_Commands;
--      Add_Command
--        (Name => "ToggleAllCrew",
--         Ada_Command => Toggle_All_Crew_Command'Access);
      Ships.UI.Crew.Inventory.Add_Inventory_Commands;
   end Add_Crew_Commands;

end Ships.UI.Crew;
