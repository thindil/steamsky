-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Missions; use Missions;
with Ships; use Ships;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Knowledge.Missions is

   -- ****if* KMissions/Show_Missions_Menu_Command
   -- FUNCTION
   -- Show the menu with available the selected event options
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Missions_Menu_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      EventMenu: Tk_Menu := Get_Widget(".missionslistmenu", Interp);
   begin
      if Winfo_Get(EventMenu, "exists") = "0" then
         EventMenu := Create(".missionslistmenu", "-tearoff false");
      end if;
      Delete(EventMenu, "0", "end");
      Menu.Add
        (EventMenu, "command",
         "-label {Show the mission on map} -command {ShowMission2 " &
         CArgv.Arg(Argv, 1) & "}");
      Menu.Add
        (EventMenu, "command",
         "-label {Set the mission as destination for the ship} -command {SetMission2 " &
         CArgv.Arg(Argv, 1) & "}");
      Menu.Add
        (EventMenu, "command",
         "-label {Show more information about the mission} -command {ShowMissionInfo2 " &
         CArgv.Arg(Argv, 1) & "}");
      Tk_Popup
        (EventMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
         Winfo_Get(Get_Main_Window(Interp), "pointery"));
      return TCL_OK;
   end Show_Missions_Menu_Command;

   -- ****if* KMissions/Show_Mission_Command
   -- FUNCTION
   -- Show the selected mission on map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMission missionindex
   -- Missionindex is the index of the event to show
   -- SOURCE
   function Show_Mission_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Mission_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MissionIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      CenterX := AcceptedMissions(MissionIndex).TargetX;
      CenterY := AcceptedMissions(MissionIndex).TargetY;
      ShowSkyMap(True);
      return TCL_OK;
   end Show_Mission_Command;

   -- ****if* KMissions/Set_Mission_Command
   -- FUNCTION
   -- Set the selected mission as the player's ship destination
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetMission missionindex
   -- Missionindex is the index of the mission to show
   -- SOURCE
   function Set_Mission_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Mission_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MissionIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      if AcceptedMissions(MissionIndex).TargetX = PlayerShip.SkyX and
        AcceptedMissions(MissionIndex).TargetY = PlayerShip.SkyY then
         ShowMessage("You are at this mission now.");
         return TCL_OK;
      end if;
      PlayerShip.DestinationX := AcceptedMissions(MissionIndex).TargetX;
      PlayerShip.DestinationY := AcceptedMissions(MissionIndex).TargetY;
      AddMessage
        ("You set the travel destination for your ship.", OrderMessage);
      ShowSkyMap(True);
      return TCL_OK;
   end Set_Mission_Command;

   -- ****o* KMissions/Show_Mission_Info_Command
   -- FUNCTION
   -- Show information about the selected mission
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMissionInfo missionindex
   -- Missionindex is the index of the mission to show
   -- SOURCE
   function Show_Mission_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Mission_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MissionIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      MissionInfo: Unbounded_String;
      Mission: constant Mission_Data := AcceptedMissions(MissionIndex);
   begin
      case Mission.MType is
         when Deliver =>
            MissionInfo :=
              To_Unbounded_String
                ("Item: " & To_String(Items_List(Mission.ItemIndex).Name) &
                 LF & "Weight:" &
                 Positive'Image(Items_List(Mission.ItemIndex).Weight) & " kg" &
                 LF & "To base: " &
                 To_String
                   (SkyBases
                      (SkyMap(Mission.TargetX, Mission.TargetY).BaseIndex)
                      .Name));
         when Patrol =>
            MissionInfo := To_Unbounded_String("Patrol selected area");
         when Destroy =>
            MissionInfo :=
              To_Unbounded_String
                ("Target: " &
                 To_String(ProtoShips_List(Mission.ShipIndex).Name));
         when Explore =>
            MissionInfo := To_Unbounded_String("Explore selected area");
         when Passenger =>
            MissionInfo :=
              To_Unbounded_String
                ("Needed quality of cabin:  GetCabinQuality(Mission.Data)" &
                 LF & "To base: " &
                 To_String
                   (SkyBases
                      (SkyMap(Mission.TargetX, Mission.TargetY).BaseIndex)
                      .Name));
      end case;
      Append(MissionInfo, LF & "Time limit:");
      MinutesToDate(Mission.Time, MissionInfo);
      Append
        (MissionInfo,
         LF & "Base reward:" &
         Natural'Image
           (Natural(Float(Mission.Reward) * Float(Mission.Multiplier))) &
         " " & To_String(MoneyName));
      if MessageBox
          ("-message {" & To_String(MissionInfo) &
           "} -type ok -parent .  -title {Mission Info}") =
        "ok" then
         return TCL_OK;
      end if;
      return TCL_OK;
   end Show_Mission_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowMissionMenu", Show_Missions_Menu_Command'Access);
      AddCommand("ShowMission2", Show_Mission_Command'Access);
      AddCommand("SetMission2", Set_Mission_Command'Access);
      AddCommand("ShowMissionInfo2", Show_Mission_Info_Command'Access);
   end AddCommands;

end Knowledge.Missions;
