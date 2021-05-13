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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with CoreUI; use CoreUI;
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
   -- Interp     - Tcl interpreter in which command was executed.
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
      pragma Unreferenced(ClientData, Argc);
      EventMenu: Tk_Menu := Get_Widget(".missionslistmenu", Interp);
      MissionIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      if Winfo_Get(EventMenu, "exists") = "0" then
         EventMenu := Create(".missionslistmenu", "-tearoff false");
      end if;
      Delete(EventMenu, "0", "end");
      Menu.Add
        (EventMenu, "command",
         "-label {Show the mission on map} -command {ShowOnMap " &
         Map_X_Range'Image(AcceptedMissions(MissionIndex).TargetX) &
         Map_Y_Range'Image(AcceptedMissions(MissionIndex).TargetY) & "}");
      Menu.Add
        (EventMenu, "command",
         "-label {Set the mission as destination for the ship} -command {SetDestination2 " &
         Map_X_Range'Image(AcceptedMissions(MissionIndex).TargetX) &
         Map_Y_Range'Image(AcceptedMissions(MissionIndex).TargetY) & "}");
      Tk_Popup
        (EventMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
         Winfo_Get(Get_Main_Window(Interp), "pointery"));
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

   procedure AddCommands is
   begin
      AddCommand("ShowMissionMenu", Show_Missions_Menu_Command'Access);
      AddCommand("ShowMissions", Show_Missions_Command'Access);
   end AddCommands;

   -- ****iv* KMissions/KMissions.MissionsTable
   -- FUNCTION
   -- Table with info about the known Missions
   -- SOURCE
   MissionsTable: Table_Widget (5);
   -- ****

   procedure UpdateMissionsList(Page: Positive := 1) is
      MissionsCanvas: constant Tk_Canvas :=
        Get_Widget(Main_Paned & ".knowledgeframe.missions.canvas");
      MissionsFrame: constant Ttk_Frame :=
        Get_Widget(MissionsCanvas & ".frame");
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Label: Ttk_Label;
      Row: Positive;
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
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
              "-text {You didn't accept any mission yet. You may ask for missions in bases. When your ship is docked to base, check Missions from ship orders menu.} -wraplength 400");
         Tcl.Tk.Ada.Grid.Grid(Label, "-padx 10");
      else
         Row := 2;
         MissionsTable :=
           CreateTable
             (Widget_Image(MissionsFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Distance"),
               To_Unbounded_String("Details"),
               To_Unbounded_String("Time limit"),
               To_Unbounded_String("Base reward")),
              Get_Widget(".gameframe.paned.knowledgeframe.missions.scrolly"));
         Rows := 0;
         Load_Accepted_Missions_Loop :
         for I in
           AcceptedMissions.First_Index .. AcceptedMissions.Last_Index loop
            if Current_Row < Start_Row then
               Current_Row := Current_Row + 1;
               goto End_Of_Loop;
            end if;
            case AcceptedMissions(I).MType is
               when Deliver =>
                  AddButton
                    (MissionsTable, "Deliver item to base",
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 1);
                  AddButton
                    (MissionsTable,
                     To_String
                       (Items_List(AcceptedMissions(I).ItemIndex).Name) &
                     " to " &
                     To_String
                       (SkyBases
                          (SkyMap
                             (AcceptedMissions(I).TargetX,
                              AcceptedMissions(I).TargetY)
                             .BaseIndex)
                          .Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Patrol =>
                  AddButton
                    (MissionsTable, "Patrol area",
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 1);
                  AddButton
                    (MissionsTable,
                     "X:" & Natural'Image(AcceptedMissions(I).TargetX) &
                     " Y:" & Natural'Image(AcceptedMissions(I).TargetY),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Destroy =>
                  AddButton
                    (MissionsTable, "Destroy ship",
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 1);
                  AddButton
                    (MissionsTable,
                     To_String
                       (ProtoShips_List(AcceptedMissions(I).ShipIndex).Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Explore =>
                  AddButton
                    (MissionsTable, "Explore area",
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 1);
                  AddButton
                    (MissionsTable,
                     "X:" & Natural'Image(AcceptedMissions(I).TargetX) &
                     " Y:" & Natural'Image(AcceptedMissions(I).TargetY),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Passenger =>
                  AddButton
                    (MissionsTable, "Transport passenger to base",
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 1);
                  AddButton
                    (MissionsTable,
                     "To " &
                     To_String
                       (SkyBases
                          (SkyMap
                             (AcceptedMissions(I).TargetX,
                              AcceptedMissions(I).TargetY)
                             .BaseIndex)
                          .Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
            end case;
            AddButton
              (MissionsTable,
               Natural'Image
                 (CountDistance
                    (AcceptedMissions(I).TargetX,
                     AcceptedMissions(I).TargetY)),
               "The distance to the mission",
               "ShowMissionMenu" & Positive'Image(Row - 1), 2);
            Mission_Time := Null_Unbounded_String;
            MinutesToDate(AcceptedMissions(I).Time, Mission_Time);
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
            exit Load_Accepted_Missions_Loop when Rows = 25 and
              I /= AcceptedMissions.Last_Index;
            <<End_Of_Loop>>
         end loop Load_Accepted_Missions_Loop;
         if Page > 1 then
            if Rows < 25 then
               AddPagination
                 (MissionsTable, "ShowMissions" & Positive'Image(Page - 1),
                  "");
            else
               AddPagination
                 (MissionsTable, "ShowMissions" & Positive'Image(Page - 1),
                  "ShowMissions" & Positive'Image(Page + 1));
            end if;
         elsif Rows > 24 then
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
