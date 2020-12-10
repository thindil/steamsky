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
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScale; use Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships; use Ships;
with Utils.UI; use Utils.UI;

package body Missions.UI is

   -- ****iv* MUI3/BaseIndex
   -- FUNCTION
   -- Index of the base in which available missions will be show
   -- SOURCE
   BaseIndex: Natural;
   -- ****

   -- ****o* MUI3/Show_Mission_Info_Command
   -- FUNCTION
   -- Show information about the selected mission
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMissionInfo
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
      MissionsView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.missionsframe.canvas.missions.missionsview",
           Interp);
      MissionInfo: Unbounded_String;
      MissionText: constant Tk_Text :=
        Get_Widget
          (".gameframe.paned.missionsframe.canvas.missions.info.info.text",
           Interp);
      MissionLabel: constant Ttk_Label :=
        Get_Widget
          (".gameframe.paned.missionsframe.canvas.missions.info.missioninfo",
           Interp);
      Button: constant Ttk_Button :=
        Get_Widget
          (".gameframe.paned.missionsframe.canvas.missions.info.set", Interp);
      CanAccept: Boolean := True;
      CabinTaken: Boolean := False;
      MissionScale: constant Ttk_Scale :=
        Get_Widget
          (".gameframe.paned.missionsframe.canvas.missions.info.reward",
           Interp);
      MissionIndex: constant Positive :=
        Positive'Value(Selection(MissionsView));
      Mission: Mission_Data :=
        (if BaseIndex = 0 then AcceptedMissions(MissionIndex)
         else SkyBases(BaseIndex).Missions(MissionIndex));
   begin
      configure(MissionText, "-state normal");
      Delete(MissionText, "1.0", "end");
      case Mission.MType is
         when Deliver =>
            Insert
              (MissionText, "end",
               "{Item: " & To_String(Items_List(Mission.ItemIndex).Name) & LF &
               "Weight:" &
               Positive'Image(Items_List(Mission.ItemIndex).Weight) & " kg" &
               LF & "To base: " &
               To_String
                 (SkyBases(SkyMap(Mission.TargetX, Mission.TargetY).BaseIndex)
                    .Name) &
               "}");
         when Patrol =>
            Insert(MissionText, "end", "{Patrol selected area}");
         when Destroy =>
            Insert
              (MissionText, "end",
               "{Target: " &
               To_String(ProtoShips_List(Mission.ShipIndex).Name) & "}");
         when Explore =>
            Insert(MissionText, "end", "{Explore selected area}");
         when Passenger =>
            CanAccept := False;
            Modules_Loop :
            for Module of PlayerShip.Modules loop
               if (Module.MType = CABIN and not CanAccept)
                 and then Module.Quality >= Mission.Data then
                  CanAccept := True;
                  CabinTaken := False;
                  for Owner of Module.Owner loop
                     if Owner > 0 then
                        CabinTaken := True;
                        CanAccept := False;
                        exit;
                     end if;
                  end loop;
                  exit when CanAccept;
               end if;
            end loop Modules_Loop;
            if BaseIndex = 0 then
               CanAccept := True;
            end if;
            Insert(MissionText, "end", "{Needed quality of cabin: }");
            if CanAccept then
               Insert
                 (MissionText, "end",
                  "{" & GetCabinQuality(Mission.Data) & "}");
            elsif CabinTaken then
               Insert
                 (MissionText, "end",
                  "{" & GetCabinQuality(Mission.Data) & "} [list yellow]");
            else
               Insert
                 (MissionText, "end",
                  "{" & GetCabinQuality(Mission.Data) & "} [list red]");
            end if;
            Insert
              (MissionText, "end",
               "{" & LF & "To base: " &
               To_String
                 (SkyBases(SkyMap(Mission.TargetX, Mission.TargetY).BaseIndex)
                    .Name) &
               "}");
      end case;
      Append(MissionInfo, LF & "Time limit:");
      MinutesToDate(Mission.Time, MissionInfo);
      Mission.Multiplier :=
        RewardMultiplier'Value(Tcl_GetVar(Get_Context, "reward"));
      Append
        (MissionInfo,
         LF & "Base reward:" &
         Natural'Image
           (Natural(Float(Mission.Reward) * Float(Mission.Multiplier))) &
         " " & To_String(MoneyName));
      State(Button, "!disabled");
      if BaseIndex > 0 then
         declare
            Distance: constant Positive :=
              (if Mission.MType in Deliver | Passenger then
                 Positive'Value
                   (Set(MissionsView, Selection(MissionsView), "distance"))
               else Positive'Value
                   (Set(MissionsView, Selection(MissionsView), "distance")) *
                 2);
         begin
            TravelInfo(MissionInfo, Distance, True);
         end;
         configure(Button, "-text {Accept mission}");
         Tcl.Tk.Ada.Grid.Grid(MissionScale);
      else
         configure(Button, "-text {Set mission as destination for ship}");
         Tcl.Tk.Ada.Grid.Grid_Remove(MissionScale);
      end if;
      Insert(MissionText, "end", "{" & To_String(MissionInfo) & "}");
      configure(MissionText, "-state disabled");
      if Mission.Finished then
         configure(MissionLabel, "-text {The mission is ready to return}");
         Tcl.Tk.Ada.Grid.Grid(MissionLabel);
      elsif BaseIndex > 0 then
         declare
            MissionsLimit: Natural;
         begin
            case SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
              .Reputation
              (1) is
               when 0 .. 25 =>
                  MissionsLimit := 1;
               when 26 .. 50 =>
                  MissionsLimit := 3;
               when 51 .. 75 =>
                  MissionsLimit := 5;
               when 76 .. 100 =>
                  MissionsLimit := 10;
               when others =>
                  MissionsLimit := 0;
            end case;
            for Mission of AcceptedMissions loop
               if Mission.StartBase =
                 SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex then
                  MissionsLimit := MissionsLimit - 1;
                  exit when MissionsLimit = 0;
               end if;
            end loop;
            if CanAccept then
               State(Button, "!disabled");
            else
               State(Button, "disabled");
            end if;
            if MissionsLimit > 0 then
               configure
                 (MissionLabel,
                  "-text {You can take" & Natural'Image(MissionsLimit) &
                  " more missions in from base.}");
            else
               configure
                 (MissionLabel,
                  "-text {You can't take any more missions from this base.}");
               State(Button, "disabled");
            end if;
         end;
         Tcl.Tk.Ada.Grid.Grid(MissionLabel);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(MissionLabel);
      end if;
      return TCL_OK;
   end Show_Mission_Info_Command;

   -- ****o* MUI3/Show_Mission_Command
   -- FUNCTION
   -- Show mission on map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMission
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
      MissionsView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.missionsframe.canvas.missions.missionsview",
           Interp);
      MissionIndex: constant Positive :=
        Positive'Value(Selection(MissionsView));
   begin
      if BaseIndex = 0 then
         CenterX := AcceptedMissions(MissionIndex).TargetX;
         CenterY := AcceptedMissions(MissionIndex).TargetY;
      else
         CenterX := SkyBases(BaseIndex).Missions(MissionIndex).TargetX;
         CenterY := SkyBases(BaseIndex).Missions(MissionIndex).TargetY;
      end if;
      ShowSkyMap(True);
      return TCL_OK;
   end Show_Mission_Command;

   procedure RefreshMissionsList(List: Mission_Container.Vector) is
      MissionsView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.missionsframe.canvas.missions.missionsview");
   begin
      Delete(MissionsView, "[list " & Children(MissionsView, "{}") & "]");
      for I in List.First_Index .. List.Last_Index loop
         case List(I).MType is
            when Deliver =>
               Insert
                 (MissionsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Deliver item to base}" &
                  Positive'Image
                    (CountDistance(List(I).TargetX, List(I).TargetY)) &
                  "]");
            when Patrol =>
               Insert
                 (MissionsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Patrol area}" &
                  Positive'Image
                    (CountDistance(List(I).TargetX, List(I).TargetY)) &
                  "]");
            when Destroy =>
               Insert
                 (MissionsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Destroy ship}" &
                  Positive'Image
                    (CountDistance(List(I).TargetX, List(I).TargetY)) &
                  "]");
            when Explore =>
               Insert
                 (MissionsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Explore area}" &
                  Positive'Image
                    (CountDistance(List(I).TargetX, List(I).TargetY)) &
                  "]");
            when Passenger =>
               Insert
                 (MissionsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Transport passenger to base}" &
                  Positive'Image
                    (CountDistance(List(I).TargetX, List(I).TargetY)) &
                  "]");
         end case;
      end loop;
      Selection_Set(MissionsView, "[list 1]");
   end RefreshMissionsList;

   -- ****o* MUI3/Set_Mission_Command
   -- FUNCTION
   -- Set mission as the player's ship destination or accept it in a base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetMission
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
      MissionsView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.missionsframe.canvas.missions.missionsview",
           Interp);
      MissionIndex: constant Positive :=
        Positive'Value(Selection(MissionsView));
   begin
      if BaseIndex = 0 then
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
      else
         AcceptMission(MissionIndex);
         RefreshMissionsList(SkyBases(BaseIndex).Missions);
         UpdateMessages;
      end if;
      return TCL_OK;
   end Set_Mission_Command;

   -- ****o* MUI3/Show_Base_Missions_Command
   -- FUNCTION
   -- Show the list of available missions in the base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBaseMissions
   -- SOURCE
   function Show_Base_Missions_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Missions_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      MissionsFrame: Ttk_Frame := Get_Widget(Paned & ".missionsframe", Interp);
      MissionsCanvas: constant Tk_Canvas :=
        Get_Widget(MissionsFrame & ".canvas", Interp);
      Label: constant Ttk_Label :=
        Get_Widget(MissionsCanvas & ".missions.info.missioninfo", Interp);
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
   begin
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "missions.tcl");
         Bind(MissionsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         AddCommand("ShowMissionInfo", Show_Mission_Info_Command'Access);
         AddCommand("ShowMission", Show_Mission_Command'Access);
         AddCommand("SetMission", Set_Mission_Command'Access);
      elsif Winfo_Get(Label, "ismapped") = "1" then
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp missions}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      BaseIndex := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      if SkyBases(BaseIndex).Missions.Length = 0 then
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      RefreshMissionsList(SkyBases(BaseIndex).Missions);
      configure
        (MissionsCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      MissionsFrame.Name :=
        New_String(Widget_Image(MissionsCanvas) & ".missions");
      Canvas_Create
        (MissionsCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(MissionsFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (MissionsCanvas,
         "-scrollregion [list " & BBox(MissionsCanvas, "all") & "]");
      ShowScreen("missionsframe");
      return TCL_OK;
   end Show_Base_Missions_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowBaseMissions", Show_Base_Missions_Command'Access);
   end AddCommands;

end Missions.UI;
