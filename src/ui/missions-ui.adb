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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScale; use Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with CoreUI; use CoreUI;
with Dialogs; use Dialogs;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Missions.UI is

   -- ****iv* MUI3/MIU3.BaseIndex
   -- FUNCTION
   -- Index of the base in which available missions will be show
   -- SOURCE
   BaseIndex: Natural;
   -- ****

   -- ****o* MUI3/MIU3.Show_Mission_Info_Command
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Mission_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      FrameName: constant String :=
        Main_Paned & ".missionsframe.canvas.missions";
      MissionsView: constant Ttk_Tree_View :=
        Get_Widget(FrameName & ".missions.missionsview", Interp);
      MissionInfo: Unbounded_String;
      MissionText: constant Tk_Text :=
        Get_Widget(FrameName & ".info.info.text", Interp);
      MissionLabel: constant Ttk_Label :=
        Get_Widget(FrameName & ".info.missioninfo", Interp);
      Button: constant Ttk_Button :=
        Get_Widget(FrameName & ".info.set", Interp);
      CanAccept: Boolean := True;
      CabinTaken: Boolean := False;
      MissionScale: constant Ttk_Scale :=
        Get_Widget(FrameName & ".info.reward.amount", Interp);
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
               To_String(Proto_Ships_List(Mission.ShipIndex).Name) & "}");
         when Explore =>
            Insert(MissionText, "end", "{Explore selected area}");
         when Passenger =>
            CanAccept := False;
            Modules_Loop :
            for Module of Player_Ship.Modules loop
               if (Module.M_Type = CABIN and not CanAccept)
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
                  exit Modules_Loop when CanAccept;
               end if;
            end loop Modules_Loop;
            if BaseIndex = 0 then
               CanAccept := True;
            end if;
            Insert(MissionText, "end", "{Needed quality of cabin: }");
            Insert
              (MissionText, "end",
               "{" & Get_Cabin_Quality(Mission.Data) & "}" &
               (if CanAccept then "" elsif CabinTaken then " [list yellow]"
                else " [list red]"));
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
         " " & To_String(Money_Name));
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
            MissionsLimit :=
              (case SkyBases
                 (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex)
                 .Reputation
                 (1) is
                 when 0 .. 25 => 1, when 26 .. 50 => 3, when 51 .. 75 => 5,
                 when 76 .. 100 => 10, when others => 0);
            Count_Missions_Limit_Loop :
            for Mission of AcceptedMissions loop
               if Mission.StartBase =
                 SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex then
                  MissionsLimit := MissionsLimit - 1;
                  exit Count_Missions_Limit_Loop when MissionsLimit = 0;
               end if;
            end loop Count_Missions_Limit_Loop;
            State(Button, (if CanAccept then "!disabled" else "disabled"));
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

   -- ****o* MUI3/MIU3.Show_Mission_Command
   -- FUNCTION
   -- Show mission on map
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMission
   -- SOURCE
   function Show_Mission_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Mission_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      MissionsView: constant Ttk_Tree_View :=
        Get_Widget
          (Main_Paned & ".missionsframe.canvas.missions.missions.missionsview",
           Interp);
      MissionIndex: constant Positive :=
        Positive'Value(Selection(MissionsView));
   begin
      return
        Show_On_Map_Command
          (ClientData, Interp, 3,
           CArgv.Empty & CArgv.Arg(Argv, 0) &
           Map_X_Range'Image
             (SkyBases(BaseIndex).Missions(MissionIndex).TargetX) &
           Map_Y_Range'Image
             (SkyBases(BaseIndex).Missions(MissionIndex).TargetY));
   end Show_Mission_Command;

   -- ****iv* MUI3/MUI3.MissionsTable
   -- FUNCTION
   -- Table with info about the known Missions
   -- SOURCE
   MissionsTable: Table_Widget (5);
   -- ****

   -- ****if* MUI3/MUI3.Show_Base_Missions_Menu_Command
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
   -- ShowBaseMissionsMenu missionindex
   -- MissionIndex is the index of the mission's menu to show
   -- SOURCE
   function Show_Base_Missions_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Missions_Menu_Command
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
         Map_X_Range'Image
           (SkyBases(BaseIndex).Missions(MissionIndex).TargetX) &
         Map_Y_Range'Image
           (SkyBases(BaseIndex).Missions(MissionIndex).TargetY) &
         "}");
      Menu.Add
        (EventMenu, "command",
         "-label {Accept the mission} -command {AcceptMission " &
         CArgv.Arg(Argv, 1) & "}");
      Tk_Popup
        (EventMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
         Winfo_Get(Get_Main_Window(Interp), "pointery"));
      return TCL_OK;
   end Show_Base_Missions_Menu_Command;

   -- ****if* MUI3/MUI3.RefreshMissionsList
   -- FUNCTION
   -- Refresh the list of available missions
   -- PARAMETERS
   -- List - The list of available missions in the selected base
   -- SOURCE
   procedure RefreshMissionsList
     (List: Mission_Container.Vector; Page: Positive := 1) is
      -- ****
      MissionsView: constant Ttk_Tree_View :=
        Get_Widget
          (Main_Paned &
           ".missionsframe.canvas.missions.missions.missionsview");
      Row: Positive := 2;
      Rows: Natural := 0;
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
      Current_Row: Positive := 1;
      Mission_Time: Unbounded_String;
   begin
      if List.Length = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         ShowSkyMap(True);
         return;
      end if;
      if MissionsTable.Row > 1 then
         ClearTable(MissionsTable);
      end if;
      Delete(MissionsView, "[list " & Children(MissionsView, "{}") & "]");
      Show_Missions_List_Loop :
      for I in List.First_Index .. List.Last_Index loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         case List(I).MType is
            when Deliver =>
               AddButton
                 (MissionsTable, "Deliver item to base",
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 1);
               AddButton
                 (MissionsTable,
                  To_String(Items_List(List(I).ItemIndex).Name) & " to " &
                  To_String
                    (SkyBases
                       (SkyMap(List(I).TargetX, List(I).TargetY).BaseIndex)
                       .Name),
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 3);
            when Patrol =>
               AddButton
                 (MissionsTable, "Patrol area",
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 1);
               AddButton
                 (MissionsTable,
                  "X:" & Natural'Image(List(I).TargetX) & " Y:" &
                  Natural'Image(List(I).TargetY),
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 3);
            when Destroy =>
               AddButton
                 (MissionsTable, "Destroy ship",
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 1);
               AddButton
                 (MissionsTable,
                  To_String(Proto_Ships_List(List(I).ShipIndex).Name),
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 3);
            when Explore =>
               AddButton
                 (MissionsTable, "Explore area",
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 1);
               AddButton
                 (MissionsTable,
                  "X:" & Natural'Image(List(I).TargetX) & " Y:" &
                  Natural'Image(List(I).TargetY),
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 3);
            when Passenger =>
               AddButton
                 (MissionsTable, "Transport passenger to base",
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 1);
               AddButton
                 (MissionsTable,
                  "To " &
                  To_String
                    (SkyBases
                       (SkyMap(List(I).TargetX, List(I).TargetY).BaseIndex)
                       .Name),
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 3);
         end case;
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
         AddButton
           (MissionsTable,
            Natural'Image(CountDistance(List(I).TargetX, List(I).TargetY)),
            "The distance to the mission",
            "ShowBaseMissionMenu" & Positive'Image(Row - 1), 2);
         Mission_Time := Null_Unbounded_String;
         MinutesToDate(List(I).Time, Mission_Time);
         AddButton
           (MissionsTable, To_String(Mission_Time),
            "The time limit for finish and return the mission",
            "ShowBaseMissionMenu" & Positive'Image(Row - 1), 4);
         AddButton
           (MissionsTable,
            Natural'Image
              (Natural(Float(List(I).Reward) * Float(List(I).Multiplier))) &
            " " & To_String(Money_Name),
            "The base money reward for the mission",
            "ShowBaseMissionMenu" & Positive'Image(Row - 1), 5, True);
         Row := Row + 1;
         Rows := Rows + 1;
         exit Show_Missions_List_Loop when Rows = 25 and I /= List.Last_Index;
         <<End_Of_Loop>>
      end loop Show_Missions_List_Loop;
      Selection_Set(MissionsView, "[list 1]");
   end RefreshMissionsList;

   -- ****o* MUI3/MIU3.Set_Mission_Command
   -- FUNCTION
   -- Accept the mission in a base
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Mission_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MissionsView: constant Ttk_Tree_View :=
        Get_Widget
          (Main_Paned & ".missionsframe.canvas.missions.missions.missionsview",
           Interp);
      MissionIndex: constant Positive :=
        Positive'Value(Selection(MissionsView));
   begin
      AcceptMission(MissionIndex);
      RefreshMissionsList(SkyBases(BaseIndex).Missions);
      UpdateMessages;
      return TCL_OK;
   exception
      when An_Exception : Missions_Accepting_Error =>
         ShowMessage
           (Text => Exception_Message(An_Exception),
            Title => "Can't accept mission");
         return TCL_OK;
   end Set_Mission_Command;

   -- ****o* MUI3/MIU3.Show_Base_Missions_Command
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Missions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MissionsFrame: Ttk_Frame :=
        Get_Widget(Main_Paned & ".missionsframe", Interp);
      MissionsCanvas: constant Tk_Canvas :=
        Get_Widget(MissionsFrame & ".canvas", Interp);
      Label: constant Ttk_Label :=
        Get_Widget(MissionsCanvas & ".missions.info.missioninfo", Interp);
   begin
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "missions.tcl");
         Bind(MissionsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         AddCommand("ShowMissionInfo", Show_Mission_Info_Command'Access);
         AddCommand("ShowMission", Show_Mission_Command'Access);
         AddCommand("SetMission", Set_Mission_Command'Access);
         MissionsTable :=
           CreateTable
             (MissionsCanvas & ".missions",
              (To_Unbounded_String("Name"), To_Unbounded_String("Distance"),
               To_Unbounded_String("Details"),
               To_Unbounded_String("Time limit"),
               To_Unbounded_String("Base reward")),
              Get_Widget(Main_Paned & ".missionsframe.scrolly"));
      elsif Winfo_Get(Label, "ismapped") = "1" then
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp missions}");
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      BaseIndex := SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      if SkyBases(BaseIndex).Missions.Length = 0 then
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      RefreshMissionsList(SkyBases(BaseIndex).Missions);
      UpdateTable(MissionsTable);
      configure
        (MissionsCanvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
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
      AddCommand
        ("ShowBaseMissionMenu", Show_Base_Missions_Menu_Command'Access);
   end AddCommands;

end Missions.UI;
