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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Game; use Game;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships; use Ships;
with Utils.UI; use Utils.UI;

package body Missions.UI is

   -- ****if* MUI3/Show_Mission_Info_Command
   -- FUNCTION
   -- Show information about the selected mission
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      pragma Unreferenced(ClientData, Argc, Argv);
      MissionsView: Ttk_Tree_View;
      MissionIndex: Positive;
      MissionInfo: Unbounded_String;
      Label: Ttk_Label;
      Mission: Mission_Data;
   begin
      MissionsView.Interp := Interp;
      MissionsView.Name :=
        New_String(".paned.missionsframe.canvas.missions.missionsview");
      MissionIndex := Positive'Value(Selection(MissionsView));
      Mission := AcceptedMissions(MissionIndex);
      case Mission.MType is
         when Deliver =>
            MissionInfo :=
              To_Unbounded_String("Item: ") &
              Items_List(Mission.ItemIndex).Name;
            Append
              (MissionInfo,
               LF & "Weight:" &
               Positive'Image(Items_List(Mission.ItemIndex).Weight) & " kg");
            Append
              (MissionInfo,
               LF & "To base: " &
               To_String
                 (SkyBases(SkyMap(Mission.TargetX, Mission.TargetY).BaseIndex)
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
            MissionInfo := To_Unbounded_String("Needed quality of cabin: ");
            Append(MissionInfo, GetCabinQuality(Mission.Data));
            Append
              (MissionInfo,
               LF & "To base: " &
               To_String
                 (SkyBases(SkyMap(Mission.TargetX, Mission.TargetY).BaseIndex)
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
      declare
         Distance: Positive;
      begin
         if Mission.MType = Deliver or Mission.MType = Passenger then
            Distance := Positive'Value(Set(MissionsView, "distance"));
         else
            Distance := Positive'Value(Set(MissionsView, "distance")) * 2;
         end if;
         TravelInfo(MissionInfo, Distance, True);
      end;
      Label.Interp := Get_Context;
      Label.Name :=
        New_String(".paned.missionsframe.canvas.missions.info.info.label");
      configure(Label, "-text {" & To_String(MissionInfo) & "}");
      return TCL_OK;
   end Show_Mission_Info_Command;

   -- ****if* MUI3/Show_Mission_Command
   -- FUNCTION
   -- Show mission on map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      pragma Unreferenced(ClientData, Argc, Argv);
      MissionsView: Ttk_Tree_View;
      MissionIndex: Positive;
   begin
      MissionsView.Interp := Interp;
      MissionsView.Name :=
        New_String(".paned.missionsframe.canvas.missions.missionsview");
      MissionIndex := Positive'Value(Selection(MissionsView));
      CenterX := AcceptedMissions(MissionIndex).TargetX;
      CenterY := AcceptedMissions(MissionIndex).TargetY;
      ShowSkyMap(True);
      return TCL_OK;
   end Show_Mission_Command;

   -- ****if* MUI3/Set_Mission_Command
   -- FUNCTION
   -- Set mission as the player's ship destination
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      pragma Unreferenced(ClientData, Argc, Argv);
      MissionsView: Ttk_Tree_View;
      MissionIndex: Positive;
   begin
      MissionsView.Interp := Interp;
      MissionsView.Name :=
        New_String(".paned.missionsframe.canvas.missions.missionsview");
      MissionIndex := Positive'Value(Selection(MissionsView));
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

   procedure ShowMissionsList is
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      MissionsCanvas: Tk_Canvas;
      MissionsFrame: Ttk_Frame;
      MissionsView: Ttk_Tree_View;
   begin
      Paned.Interp := Get_Context;
      Paned.Name := New_String(".paned");
      MissionsFrame.Interp := Get_Context;
      MissionsFrame.Name := New_String(Widget_Image(Paned) & ".missionsframe");
      MissionsCanvas.Interp := Get_Context;
      MissionsCanvas.Name :=
        New_String(Widget_Image(MissionsFrame) & ".canvas");
      Label.Interp := Get_Context;
      Label.Name :=
        New_String(Widget_Image(MissionsCanvas) & ".missions.info.info.label");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator &
            "missions.tcl");
         Bind(MissionsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         AddCommand("ShowMissionInfo", Show_Mission_Info_Command'Access);
         AddCommand("ShowMission", Show_Mission_Command'Access);
         AddCommand("SetMission", Set_Mission_Command'Access);
      elsif Winfo_Get(Label, "ismapped") = "1" then
         ShowSkyMap(True);
         return;
      end if;
      MissionsView.Interp := Get_Context;
      MissionsView.Name :=
        New_String(Widget_Image(MissionsCanvas) & ".missions.missionsview");
      Delete(MissionsView, "[list " & Children(MissionsView, "{}") & "]");
      for I in AcceptedMissions.First_Index .. AcceptedMissions.Last_Index loop
         case AcceptedMissions(I).MType is
            when Deliver =>
               Insert
                 (MissionsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Deliver item to base}" &
                  Positive'Image
                    (CountDistance
                       (AcceptedMissions(I).TargetX,
                        AcceptedMissions(I).TargetY)) &
                  "]");
            when Patrol =>
               Insert
                 (MissionsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Patrol area}" &
                  Positive'Image
                    (CountDistance
                       (AcceptedMissions(I).TargetX,
                        AcceptedMissions(I).TargetY)) &
                  "]");
            when Destroy =>
               Insert
                 (MissionsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Destroy ship}" &
                  Positive'Image
                    (CountDistance
                       (AcceptedMissions(I).TargetX,
                        AcceptedMissions(I).TargetY)) &
                  "]");
            when Explore =>
               Insert
                 (MissionsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Explore area}" &
                  Positive'Image
                    (CountDistance
                       (AcceptedMissions(I).TargetX,
                        AcceptedMissions(I).TargetY)) &
                  "]");
            when Passenger =>
               Insert
                 (MissionsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Transport passenger to base}" &
                  Positive'Image
                    (CountDistance
                       (AcceptedMissions(I).TargetX,
                        AcceptedMissions(I).TargetY)) &
                  "]");
         end case;
      end loop;
      Selection_Set(MissionsView, "[list 1]");
      configure
        (MissionsCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      MissionsFrame.Name :=
        New_String(Widget_Image(MissionsCanvas) & ".missions");
      Canvas_Create
        (MissionsCanvas, "window",
         "[expr " & Winfo_Get(MissionsFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(MissionsFrame, "reqheight") & " / 2] -window " &
         Widget_Image(MissionsFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (MissionsCanvas,
         "-scrollregion [list " & BBox(MissionsCanvas, "all") & "]");
      ShowScreen("missionsframe");
   end ShowMissionsList;

end Missions.UI;
