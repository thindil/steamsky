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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Generic_Array_Sort;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Config; use Config;
with CoreUI; use CoreUI;
with Dialogs; use Dialogs;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Items; use Items;
with Maps; use Maps;
with Missions; use Missions;
with Ships; use Ships;
with Table; use Table;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Knowledge.Missions is

   -- ****if* KMissions/KMissions.Show_Missions_Menu_Command
   -- FUNCTION
   -- Show the menu with available the selected mission options
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMissionsMenu missionindex
   -- MissionIndex is the index of the mission's menu to show
   -- SOURCE
   function Show_Missions_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Missions_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MissionIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Mission_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".missionslistmenu",
           Title =>
             (case AcceptedMissions(MissionIndex).MType is
                when Deliver => "Deliver item",
                when Destroy => "Destroy enemy", when Patrol => "Patrol area",
                when Explore => "Explore area",
                when Passenger => "Transport passenger") &
             " mission actions",
           Parent_Name => ".");
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Mission_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Mission_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Mission_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Mission_Menu & ".show;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Add_Button
        (Name => ".show", Label => "Show the mission on map",
         Command =>
           "ShowOnMap" &
           Map_X_Range'Image(AcceptedMissions(MissionIndex).TargetX) &
           Map_Y_Range'Image(AcceptedMissions(MissionIndex).TargetY));
      Add_Button
        (Name => ".destination",
         Label => "Set the mission as destination for the ship",
         Command =>
           "SetDestination2 " &
           Map_X_Range'Image(AcceptedMissions(MissionIndex).TargetX) &
           Map_Y_Range'Image(AcceptedMissions(MissionIndex).TargetY));
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Mission_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Missions_Menu_Command;

   -- ****o* KMissions/KMissions.Show_Missions_Command
   -- FUNCTION
   -- Show the list of known missions to the player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMissions ?startindex?
   -- Page parameter is a page number which will be show
   -- SOURCE
   function Show_Missions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Missions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
   begin
      if Argc = 2 then
         UpdateMissionsList(Positive'Value(CArgv.Arg(Argv, 1)));
      else
         UpdateMissionsList;
      end if;
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Missions_Command;

   -- ****iv* KMissions/KMissions.MissionsTable
   -- FUNCTION
   -- Table with info about the known Missions
   -- SOURCE
   MissionsTable: Table_Widget (5);
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
   -- NONE         - No sorting missions (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Missions_Sort_Orders is
     (TYPEASC, TYPEDESC, DISTANCEASC, DISTANCEDESC, DETAILSASC, DETAILSDESC,
      TIMEASC, TIMEDESC, REWARDASC, REWARDDESC, NONE) with
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

   -- ****iv* KMissions/KMissions.Missions_Sort_Order
   -- FUNCTION
   -- The current sorting order for accepted missions list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Missions_Sort_Order: Missions_Sort_Orders := Default_Missions_Sort_Order;
   -- ****

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
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortAcceptedMissions x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Missions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Missions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Column: constant Positive :=
        Get_Column_Number(MissionsTable, Natural'Value(CArgv.Arg(Argv, 1)));
      type Local_Mission_Data is record
         MType: Missions_Types;
         Distance: Natural;
         Details: Unbounded_String;
         Time: Natural;
         Reward: Natural;
         Id: Positive;
      end record;
      type Missions_Array is array(Positive range <>) of Local_Mission_Data;
      Local_Missions: Missions_Array(1 .. Positive(AcceptedMissions.Length));
      function "<"(Left, Right: Local_Mission_Data) return Boolean is
      begin
         if Missions_Sort_Order = TYPEASC
           and then Left.MType < Right.MType then
            return True;
         end if;
         if Missions_Sort_Order = TYPEDESC
           and then Left.MType > Right.MType then
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
            if Missions_Sort_Order = DETAILSASC then
               Missions_Sort_Order := DETAILSDESC;
            else
               Missions_Sort_Order := DETAILSASC;
            end if;
         when 4 =>
            if Missions_Sort_Order = TIMEASC then
               Missions_Sort_Order := TIMEDESC;
            else
               Missions_Sort_Order := TIMEASC;
            end if;
         when 5 =>
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
      for I in AcceptedMissions.Iterate loop
         Local_Missions(Mission_Container.To_Index(I)) :=
           (MType => AcceptedMissions(I).MType,
            Distance =>
              Count_Distance
                (AcceptedMissions(I).TargetX, AcceptedMissions(I).TargetY),
            Details =>
              (case AcceptedMissions(I).MType is
                 when Deliver =>
                   Items_List(AcceptedMissions(I).ItemIndex).Name & " to " &
                   Sky_Bases
                     (Sky_Map
                        (AcceptedMissions(I).TargetX,
                         AcceptedMissions(I).TargetY)
                        .Base_Index)
                     .Name,
                 when Patrol =>
                   To_Unbounded_String
                     ("X:" & Natural'Image(AcceptedMissions(I).TargetX) &
                      " Y:" & Natural'Image(AcceptedMissions(I).TargetY)),
                 when Destroy =>
                   Proto_Ships_List(AcceptedMissions(I).ShipIndex).Name,
                 when Explore =>
                   To_Unbounded_String
                     ("X:" & Natural'Image(AcceptedMissions(I).TargetX) &
                      " Y:" & Natural'Image(AcceptedMissions(I).TargetY)),
                 when Passenger =>
                   "To " &
                   Sky_Bases
                     (Sky_Map
                        (AcceptedMissions(I).TargetX,
                         AcceptedMissions(I).TargetY)
                        .Base_Index)
                     .Name),
            Time => AcceptedMissions(I).Time,
            Reward => AcceptedMissions(I).Reward,
            Id => Mission_Container.To_Index(I));
      end loop;
      Sort_Missions(Local_Missions);
      Missions_Indexes.Clear;
      for Event of Local_Missions loop
         Missions_Indexes.Append(Event.Id);
      end loop;
      UpdateMissionsList;
      return TCL_OK;
   end Sort_Missions_Command;

   procedure AddCommands is
   begin
      Add_Command("ShowMissionMenu", Show_Missions_Menu_Command'Access);
      Add_Command("ShowMissions", Show_Missions_Command'Access);
      Add_Command("SortAcceptedMissions", Sort_Missions_Command'Access);
   end AddCommands;

   procedure UpdateMissionsList(Page: Positive := 1) is
      MissionsCanvas: constant Tk_Canvas :=
        Get_Widget(Main_Paned & ".knowledgeframe.missions.canvas");
      MissionsFrame: constant Ttk_Frame :=
        Get_Widget(MissionsCanvas & ".frame");
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Label: Ttk_Label;
      Row: Positive;
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      Mission_Time: Unbounded_String;
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(MissionsFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      if MissionsTable.Row > 1 then
         ClearTable(MissionsTable);
      end if;
      Delete_Widgets(1, Rows - 1, MissionsFrame);
      if AcceptedMissions.Length = 0 then
         Label :=
           Create
             (MissionsFrame & ".nomissions",
              "-text {You didn't accept any mission yet. You may ask for missions in bases. When your ship is docked to base, check Missions from ship orders menu.} -wraplength 350");
         Tcl.Tk.Ada.Grid.Grid(Label, "-padx 10");
         Bind
           (MissionsCanvas, "<Configure>",
            "{" & Label & " configure -wraplength [expr [winfo width " &
            MissionsCanvas & "] - 15]}");
      else
         Unbind(MissionsCanvas, "<Configure>");
         Row := 2;
         MissionsTable :=
           CreateTable
             (Widget_Image(MissionsFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Distance"),
               To_Unbounded_String("Details"),
               To_Unbounded_String("Time limit"),
               To_Unbounded_String("Base reward")),
              Get_Widget(".gameframe.paned.knowledgeframe.missions.scrolly"),
              "SortAcceptedMissions",
              "Press mouse button to sort the missions.");
         if Missions_Indexes.Length /= AcceptedMissions.Length then
            Missions_Indexes.Clear;
            for I in AcceptedMissions.Iterate loop
               Missions_Indexes.Append(Mission_Container.To_Index(I));
            end loop;
         end if;
         Rows := 0;
         Load_Accepted_Missions_Loop :
         for I of Missions_Indexes loop
            if Current_Row < Start_Row then
               Current_Row := Current_Row + 1;
               goto End_Of_Loop;
            end if;
            AddButton
              (MissionsTable, Get_Mission_Type(AcceptedMissions(I).MType),
               "Show available mission's options",
               "ShowMissionMenu" & Positive'Image(Row - 1), 1);
            case AcceptedMissions(I).MType is
               when Deliver =>
                  AddButton
                    (MissionsTable,
                     To_String
                       (Items_List(AcceptedMissions(I).ItemIndex).Name) &
                     " to " &
                     To_String
                       (Sky_Bases
                          (Sky_Map
                             (AcceptedMissions(I).TargetX,
                              AcceptedMissions(I).TargetY)
                             .Base_Index)
                          .Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Patrol =>
                  AddButton
                    (MissionsTable,
                     "X:" & Natural'Image(AcceptedMissions(I).TargetX) &
                     " Y:" & Natural'Image(AcceptedMissions(I).TargetY),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Destroy =>
                  AddButton
                    (MissionsTable,
                     To_String
                       (Proto_Ships_List(AcceptedMissions(I).ShipIndex).Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Explore =>
                  AddButton
                    (MissionsTable,
                     "X:" & Natural'Image(AcceptedMissions(I).TargetX) &
                     " Y:" & Natural'Image(AcceptedMissions(I).TargetY),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Passenger =>
                  AddButton
                    (MissionsTable,
                     "To " &
                     To_String
                       (Sky_Bases
                          (Sky_Map
                             (AcceptedMissions(I).TargetX,
                              AcceptedMissions(I).TargetY)
                             .Base_Index)
                          .Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
            end case;
            AddButton
              (MissionsTable,
               Natural'Image
                 (Count_Distance
                    (AcceptedMissions(I).TargetX,
                     AcceptedMissions(I).TargetY)),
               "The distance to the mission",
               "ShowMissionMenu" & Positive'Image(Row - 1), 2);
            Mission_Time := Null_Unbounded_String;
            Minutes_To_Date(AcceptedMissions(I).Time, Mission_Time);
            AddButton
              (MissionsTable, To_String(Mission_Time),
               "The time limit for finish and return the mission",
               "ShowMissionMenu" & Positive'Image(Row - 1), 4);
            AddButton
              (MissionsTable,
               Natural'Image
                 (Natural
                    (Float(AcceptedMissions(I).Reward) *
                     Float(AcceptedMissions(I).Multiplier))) &
               " " & To_String(Money_Name),
               "The base money reward for the mission",
               "ShowMissionMenu" & Positive'Image(Row - 1), 5, True);
            Row := Row + 1;
            Rows := Rows + 1;
            exit Load_Accepted_Missions_Loop when Rows =
              Game_Settings.Lists_Limit and
              I /= AcceptedMissions.Last_Index;
            <<End_Of_Loop>>
         end loop Load_Accepted_Missions_Loop;
         if Page > 1 then
            if Rows < Game_Settings.Lists_Limit then
               AddPagination
                 (MissionsTable, "ShowMissions" & Positive'Image(Page - 1),
                  "");
            else
               AddPagination
                 (MissionsTable, "ShowMissions" & Positive'Image(Page - 1),
                  "ShowMissions" & Positive'Image(Page + 1));
            end if;
         elsif Rows > Game_Settings.Lists_Limit - 1 then
            AddPagination
              (MissionsTable, "", "ShowMissions" & Positive'Image(Page + 1));
         end if;
         UpdateTable(MissionsTable);
      end if;
      Tcl_Eval(Get_Context, "update");
      configure
        (MissionsCanvas,
         "-scrollregion [list " & BBox(MissionsCanvas, "all") & "]");
      Xview_Move_To(MissionsCanvas, "0.0");
      Yview_Move_To(MissionsCanvas, "0.0");
   end UpdateMissionsList;

end Knowledge.Missions;
