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

with Ada.Containers;
with Ada.Containers.Generic_Array_Sort;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Bases; use Bases;
with CoreUI;
with Game; use Game;
with Items; use Items;
with Maps; use Maps;
with Missions; use Missions;
with Ships; use Ships;
with Table; use Table;
with Utils;
with Utils.UI; use Utils.UI;

package body Knowledge.Missions is

   -- ****if* KMissions/KMissions.Show_Missions_Menu_Command
   -- FUNCTION
   -- Show the menu with available the selected mission options
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMissionsMenu missionindex
   -- MissionIndex is the index of the mission's menu to show
   -- SOURCE
   function Show_Missions_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showMissionsMenuCommand";
      -- ****

   -- ****o* KMissions/KMissions.Show_Missions_Command
   -- FUNCTION
   -- Show the list of known missions to the player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMissions ?startindex?
   -- Page parameter is a page number which will be show
   -- SOURCE
   function Show_Missions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Missions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
   begin
      if Argc = 2 then
         Update_Missions_List
           (Page => Positive'Value(CArgv.Arg(Argv => Argv, N => 1)));
      else
         Update_Missions_List;
      end if;
      Tcl_SetResult(interp => Interp, str => "1");
      return TCL_OK;
   end Show_Missions_Command;

   -- ****iv* KMissions/KMissions.Missions_Table
   -- FUNCTION
   -- Table with info about the known Missions
   -- SOURCE
   Missions_Table: Table_Widget (Amount => 6);
   -- ****

   -- ****it* KMissions/KMissions.Missions_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the accepted missions list
   -- OPTIONS
   -- TYPEASC      - Sort missions by type ascending
   -- TYPEDESC     - Sort missions by type descending
   -- DISTANCEASC  - Sort missions by distance ascending
   -- DISTANCEDESC - Sort missions by distance descending
   -- DETAILSASC   - Sort missions by details ascending
   -- DETAILSDESC  - Sort missions by details descending
   -- TIMEASC      - Sort missions by time ascending
   -- TIMEDESC     - Sort missions by time descending
   -- REWARDASC    - Sort missions by reward ascending
   -- REWARDDESC   - Sort missions by reward descending
   -- COORDASC     - Sort missions by coordinates ascending
   -- COORDDESC    - Sort missions by coordinates descending
   -- NONE         - No sorting missions (default)
   -- HISTORY
   -- 6.4 - Added
   -- 8.4 - Added sorting by coordinates
   -- SOURCE
   type Missions_Sort_Orders is
     (TYPEASC, TYPEDESC, DISTANCEASC, DISTANCEDESC, DETAILSASC, DETAILSDESC,
      TIMEASC, TIMEDESC, REWARDASC, REWARDDESC, COORDASC, COORDDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* KMissions/KMissions.Default_Missions_Sort_Order
      -- FUNCTION
      -- Default sorting order for the known missions
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Missions_Sort_Order: constant Missions_Sort_Orders := NONE;
   -- ****

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* KMissions/KMissions.Missions_Sort_Order
   -- FUNCTION
   -- The current sorting order for accepted missions list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Missions_Sort_Order: Missions_Sort_Orders := Default_Missions_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****iv* KMissions/KMissions.Missions_Indexes
   -- FUNCTION
   -- Indexes of the accepted missions
   -- SOURCE
   Missions_Indexes: Positive_Container.Vector;
   -- ****

   -- ****o* KMissions/KMissions.Sort_Missions_Command
   -- FUNCTION
   -- Sort the accepted missions list
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortAccepted_Missions x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Missions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Missions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number
          (Table => Missions_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
      --## rule off TYPE_INITIAL_VALUES
      type Local_Mission_Data is record
         M_Type: Missions_Types;
         Distance: Natural;
         Coords: Unbounded_String;
         Details: Unbounded_String;
         Time: Natural;
         Reward: Natural;
         Id: Positive;
      end record;
      type Missions_Array is array(Positive range <>) of Local_Mission_Data;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Local_Missions: Missions_Array(1 .. Get_Accepted_Missions_Amount);
      Accepted_Mission: Mission_Data;
      --## rule on IMPROPER_INITIALIZATION
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Local_Mission_Data) return Boolean is
      begin
         if Missions_Sort_Order = TYPEASC
           and then Left.M_Type < Right.M_Type then
            return True;
         end if;
         if Missions_Sort_Order = TYPEDESC
           and then Left.M_Type > Right.M_Type then
            return True;
         end if;
         if Missions_Sort_Order = DISTANCEASC
           and then Left.Distance < Right.Distance then
            return True;
         end if;
         if Missions_Sort_Order = DISTANCEDESC
           and then Left.Distance > Right.Distance then
            return True;
         end if;
         if Missions_Sort_Order = DETAILSASC
           and then Left.Details < Right.Details then
            return True;
         end if;
         if Missions_Sort_Order = DETAILSDESC
           and then Left.Details > Right.Details then
            return True;
         end if;
         if Missions_Sort_Order = TIMEASC and then Left.Time < Right.Time then
            return True;
         end if;
         if Missions_Sort_Order = TIMEDESC and then Left.Time > Right.Time then
            return True;
         end if;
         if Missions_Sort_Order = REWARDASC
           and then Left.Reward < Right.Reward then
            return True;
         end if;
         if Missions_Sort_Order = REWARDDESC
           and then Left.Reward > Right.Reward then
            return True;
         end if;
         if Missions_Sort_Order = COORDASC
           and then Left.Coords < Right.Coords then
            return True;
         end if;
         if Missions_Sort_Order = COORDDESC
           and then Left.Coords > Right.Coords then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Missions is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Mission_Data,
         Array_Type => Missions_Array);
   begin
      case Column is
         when 1 =>
            if Missions_Sort_Order = TYPEASC then
               Missions_Sort_Order := TYPEDESC;
            else
               Missions_Sort_Order := TYPEASC;
            end if;
         when 2 =>
            if Missions_Sort_Order = DISTANCEASC then
               Missions_Sort_Order := DISTANCEDESC;
            else
               Missions_Sort_Order := DISTANCEASC;
            end if;
         when 3 =>
            if Missions_Sort_Order = COORDASC then
               Missions_Sort_Order := COORDDESC;
            else
               Missions_Sort_Order := COORDASC;
            end if;
         when 4 =>
            if Missions_Sort_Order = DETAILSASC then
               Missions_Sort_Order := DETAILSDESC;
            else
               Missions_Sort_Order := DETAILSASC;
            end if;
         when 5 =>
            if Missions_Sort_Order = TIMEASC then
               Missions_Sort_Order := TIMEDESC;
            else
               Missions_Sort_Order := TIMEASC;
            end if;
         when 6 =>
            if Missions_Sort_Order = REWARDASC then
               Missions_Sort_Order := REWARDDESC;
            else
               Missions_Sort_Order := REWARDASC;
            end if;
         when others =>
            null;
      end case;
      if Missions_Sort_Order = NONE then
         return TCL_OK;
      end if;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Fill_Local_Missions_Loop :
      for I in 1 .. Get_Accepted_Missions_Amount loop
         Accepted_Mission := Get_Accepted_Mission(Mission_Index => I);
         Local_Missions(I) :=
           (M_Type => Accepted_Mission.M_Type,
            Distance =>
              Count_Distance
                (Destination_X => Accepted_Mission.Target_X,
                 Destination_Y => Accepted_Mission.Target_Y),
            Coords =>
              To_Unbounded_String
                (Source =>
                   "X:" & Natural'Image(Accepted_Mission.Target_X) & " Y:" &
                   Natural'Image(Accepted_Mission.Target_Y)),
            Details =>
              (case Accepted_Mission.M_Type is
                 when DELIVER =>
                   To_String
                     (Source =>
                        Get_Proto_Item(Index => Accepted_Mission.Item_Index)
                          .Name) &
                   To_Unbounded_String(Source => " to ") &
                   To_String
                     (Source =>
                        Sky_Bases
                          (Sky_Map
                             (Accepted_Mission.Target_X,
                              Accepted_Mission.Target_Y)
                             .Base_Index)
                          .Name),
                 when PATROL =>
                   To_Unbounded_String
                     (Source =>
                        "X:" & Natural'Image(Accepted_Mission.Target_X) &
                        " Y:" & Natural'Image(Accepted_Mission.Target_Y)),
                 when DESTROY =>
                   To_Unbounded_String
                     (Source =>
                        To_String
                          (Source =>
                             Get_Proto_Ship
                               (Proto_Index => Accepted_Mission.Ship_Index)
                               .Name)),
                 when EXPLORE =>
                   To_Unbounded_String
                     (Source =>
                        "X:" & Natural'Image(Accepted_Mission.Target_X) &
                        " Y:" & Natural'Image(Accepted_Mission.Target_Y)),
                 when PASSENGER =>
                   To_Unbounded_String(Source => "To ") &
                   Tiny_String.To_String
                     (Source =>
                        Sky_Bases
                          (Sky_Map
                             (Accepted_Mission.Target_X,
                              Accepted_Mission.Target_Y)
                             .Base_Index)
                          .Name)),
            Time => Accepted_Mission.Time, Reward => Accepted_Mission.Reward,
            Id => I);
      end loop Fill_Local_Missions_Loop;
      Sort_Missions(Container => Local_Missions);
      Missions_Indexes.Clear;
      Fill_Missions_Indexes_Loop :
      for Event of Local_Missions loop
         Missions_Indexes.Append(New_Item => Event.Id);
      end loop Fill_Missions_Indexes_Loop;
      Update_Missions_List;
      return TCL_OK;
   end Sort_Missions_Command;

   procedure Add_Knowledge_Missions_Commands is
   begin
      Add_Command
        (Name => "ShowMissionMenu",
         Ada_Command => Show_Missions_Menu_Command'Access);
      Add_Command
        (Name => "ShowMissions", Ada_Command => Show_Missions_Command'Access);
      Add_Command
        (Name => "SortAccepted_Missions",
         Ada_Command => Sort_Missions_Command'Access);
   end Add_Knowledge_Missions_Commands;

   procedure Update_Missions_List(Page: Positive := 1) is
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.Canvas;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use CoreUI;

      Missions_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Main_Paned & ".knowledgeframe.missions.canvas");
      Missions_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Missions_Canvas & ".frame");
      procedure Update_Ada_Missions_List(P: Positive) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaMissionsList";
   begin
      Update_Ada_Missions_List(P => Page);
      if Get_Accepted_Missions_Amount > 0 then
         Missions_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Missions_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Distance"),
                 3 => To_Unbounded_String(Source => "Coordinates"),
                 4 => To_Unbounded_String(Source => "Details"),
                 5 => To_Unbounded_String(Source => "Time limit"),
                 6 => To_Unbounded_String(Source => "Base reward")),
              Scrollbar =>
                Get_Widget
                  (pathName =>
                     ".gameframe.paned.knowledgeframe.missions.scrolly"),
              Command => "SortAccepted_Missions",
              Tooltip_Text => "Press mouse button to sort the missions.");
         if Natural(Missions_Indexes.Length) /=
           Get_Accepted_Missions_Amount then
            Missions_Indexes.Clear;
            Fill_Missions_Indexes_Loop :
            for I in 1 .. Get_Accepted_Missions_Amount loop
               Missions_Indexes.Append(New_Item => I);
            end loop Fill_Missions_Indexes_Loop;
         end if;
      end if;
   end Update_Missions_List;

end Knowledge.Missions;
