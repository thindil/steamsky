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
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScale; use Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
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
   -- ShowMission missionindex
   -- MissionIndex is the index of the mission to show on map
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
      MissionIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
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

   -- ****if* Missions.UI/Count_Missions_Amount
   -- FUNCTION
   -- Count the amount of missions which the player can get from the selected
   -- base
   -- RESULT
   -- The amount of missions which the player can get from the base
   -- SOURCE
   function Count_Missions_Amount return Natural is
      -- ****
      MissionsLimit: Natural;
   begin
      MissionsLimit :=
        (case SkyBases(SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex)
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
      return MissionsLimit;
   end Count_Missions_Amount;

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
      CanAccept: Boolean := True;
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
      if SkyBases(BaseIndex).Missions(MissionIndex).MType = Passenger then
         CanAccept := False;
         Modules_Loop :
         for Module of Player_Ship.Modules loop
            if (Module.M_Type = CABIN and not CanAccept)
              and then Module.Quality >=
                SkyBases(BaseIndex).Missions(MissionIndex).Data then
               CanAccept := True;
               for Owner of Module.Owner loop
                  if Owner > 0 then
                     CanAccept := False;
                     exit;
                  end if;
               end loop;
               exit Modules_Loop when CanAccept;
            end if;
         end loop Modules_Loop;
      end if;
      if Count_Missions_Amount = 0 then
         CanAccept := False;
      end if;
      if CanAccept then
         Menu.Add
           (EventMenu, "command",
            "-label {Accept the mission} -command {AcceptMission " &
            CArgv.Arg(Argv, 1) & "}");
      end if;
      Menu.Add
        (EventMenu, "command",
         "-label {Show more info} -command {MissionMoreInfo " &
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
   -- Page - The current page of the list to show. Can be empty. Default value
   --        is 1.
   -- SOURCE
   procedure RefreshMissionsList
     (List: Mission_Container.Vector; Page: Positive := 1) is
      -- ****
      Row: Positive := 2;
      Rows: Natural := 0;
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
      Current_Row: Positive := 1;
      Mission_Time: Unbounded_String;
      CanAccept: Boolean := True;
      CabinTaken: Boolean := False;
   begin
      if List.Length = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         ShowSkyMap(True);
         return;
      end if;
      declare
         MissionsLimit: constant Natural := Count_Missions_Amount;
         MissionLabel: constant Ttk_Label :=
           Get_Widget
             (Main_Paned & ".missionsframe.canvas.missions.missionslabel");
      begin
         if MissionsLimit > 0 then
            configure
              (MissionLabel,
               "-text {You can take" & Natural'Image(MissionsLimit) &
               " more missions in from base.}");
         else
            configure
              (MissionLabel,
               "-text {You can't take any more missions from this base.}");
         end if;
      end;
      if MissionsTable.Row > 1 then
         ClearTable(MissionsTable);
      end if;
      Show_Missions_List_Loop :
      for I in List.First_Index .. List.Last_Index loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         if List(I).MType = Passenger then
            CanAccept := False;
            Modules_Loop :
            for Module of Player_Ship.Modules loop
               if (Module.M_Type = CABIN and not CanAccept)
                 and then Module.Quality >= List(I).Data then
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
         end if;
         AddButton
           (Table => MissionsTable, Text => Get_Mission_Type(List(I).MType),
            Tooltip => "Show available mission's options",
            Command => "ShowBaseMissionMenu" & Positive'Image(Row - 1),
            Column => 1,
            Color =>
              (if not CanAccept then "red" elsif CabinTaken then "yellow"
               else ""));
         CanAccept := True;
         CabinTaken := False;
         case List(I).MType is
            when Deliver =>
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
                 (MissionsTable,
                  "X:" & Natural'Image(List(I).TargetX) & " Y:" &
                  Natural'Image(List(I).TargetY),
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 3);
            when Destroy =>
               AddButton
                 (MissionsTable,
                  To_String(Proto_Ships_List(List(I).ShipIndex).Name),
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 3);
            when Explore =>
               AddButton
                 (MissionsTable,
                  "X:" & Natural'Image(List(I).TargetX) & " Y:" &
                  Natural'Image(List(I).TargetY),
                  "Show available mission's options",
                  "ShowBaseMissionMenu" & Positive'Image(Row - 1), 3);
            when Passenger =>
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
      if Page > 1 then
         if Rows < 25 then
            AddPagination
              (MissionsTable, "ShowBaseMissions" & Positive'Image(Page - 1),
               "");
         else
            AddPagination
              (MissionsTable, "ShowBaseMissions" & Positive'Image(Page - 1),
               "ShowBaseMissions" & Positive'Image(Page + 1));
         end if;
      elsif Rows > 24 then
         AddPagination
           (MissionsTable, "", "ShowBaseMissions" & Positive'Image(Page + 1));
      end if;
   end RefreshMissionsList;

   -- ****o* MUI3/MIU3.Set_Mission_Command
   -- FUNCTION
   -- Accept the mission in a base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetMission missionindex
   -- MissionIndex is the index of the mission to accept
   -- SOURCE
   function Set_Mission_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Mission_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MissionIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      SkyBases(BaseIndex).Missions(MissionIndex).Multiplier :=
        RewardMultiplier'Value(Tcl_GetVar(Get_Context, "reward"));
      AcceptMission(MissionIndex);
      RefreshMissionsList(SkyBases(BaseIndex).Missions);
      UpdateTable(MissionsTable);
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
   -- ShowBaseMissions ?page?
   -- Page is the number of page of the missions list to show. If not
   -- set then it is 1
   -- SOURCE
   function Show_Base_Missions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Missions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      use Interfaces.C;

      MissionsFrame: Ttk_Frame :=
        Get_Widget(Main_Paned & ".missionsframe", Interp);
      MissionsCanvas: constant Tk_Canvas :=
        Get_Widget(MissionsFrame & ".canvas", Interp);
      Label: constant Ttk_Label :=
        Get_Widget(MissionsCanvas & ".missions.missionslabel", Interp);
   begin
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "missions.tcl");
         Bind(MissionsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
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
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
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
      RefreshMissionsList
        (SkyBases(BaseIndex).Missions,
         (if Argc > 1 then Positive'Value(CArgv.Arg(Argv, 1)) else 1));
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

   -- ****o* MUI3/MIU3.Mission_More_Info_Command
   -- FUNCTION
   -- Show more info about the selected mission
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MissionMoreInfo missionindex
   -- MissionIndex is the index of the mission's info to show
   -- SOURCE
   function Mission_More_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Mission_More_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      MissionIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Mission: constant Mission_Data :=
        SkyBases(BaseIndex).Missions(MissionIndex);
      MissionDialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".missiondialog",
           Title => "More info about " & Get_Mission_Type(Mission.MType));
      CanAccept: Boolean := True;
      CabinTaken: Boolean := False;
      Label: constant Ttk_Label :=
        Create(MissionDialog & ".infolabel", "-wraplength 400");
      MissionInfo: Unbounded_String := Null_Unbounded_String;
   begin
      TravelInfo
        (MissionInfo,
         (if Mission.MType in Deliver | Passenger then
            CountDistance(Mission.TargetX, Mission.TargetY)
          else CountDistance(Mission.TargetX, Mission.TargetY) * 2),
         True);
      case Mission.MType is
         when Deliver =>
            configure
              (Label,
               "-text {Item: " &
               To_String(Items_List(Mission.ItemIndex).Name) & LF & "Weight:" &
               Positive'Image(Items_List(Mission.ItemIndex).Weight) & " kg" &
               LF & "To base: " &
               To_String
                 (SkyBases(SkyMap(Mission.TargetX, Mission.TargetY).BaseIndex)
                    .Name) &
               To_String(MissionInfo) & "}");
         when Patrol =>
            configure
              (Label,
               "-text {Patrol selected area" & To_String(MissionInfo) & "}");
         when Destroy =>
            configure
              (Label,
               "-text {Target: " &
               To_String(Proto_Ships_List(Mission.ShipIndex).Name) &
               To_String(MissionInfo) & "}");
         when Explore =>
            configure
              (Label,
               "-text {Explore selected area" & To_String(MissionInfo) & "}");
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
            configure
              (Label,
               "-text {Needed quality of cabin: " &
               Get_Cabin_Quality(Mission.Data) &
               (if CanAccept then "" elsif CabinTaken then " (taken)"
                else " (no cabin)") &
               LF & "To base: " &
               To_String
                 (SkyBases(SkyMap(Mission.TargetX, Mission.TargetY).BaseIndex)
                    .Name) &
               To_String(MissionInfo) & "}");
      end case;
      Tcl.Tk.Ada.Grid.Grid(Label, "-padx 5");
      Add_Close_Button
        (MissionDialog & ".button", "Close", "CloseDialog " & MissionDialog);
      Show_Dialog(MissionDialog);
      return TCL_OK;
   end Mission_More_Info_Command;

   -- ****o* MUI3/MIU3.Accept_Mission_Command
   -- FUNCTION
   -- Accept the mission in a base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- AcceptMission missionindex
   -- MissionIndex is the index of the mission to accept
   -- SOURCE
   function Accept_Mission_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Accept_Mission_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      MissionIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Mission: constant Mission_Data :=
        SkyBases(BaseIndex).Missions(MissionIndex);
      MissionDialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".missiondialog",
           Title => "Accept " & Get_Mission_Type(Mission.MType), Columns => 2);
      Button: Ttk_Button :=
        Create
          (MissionDialog & ".accept",
           "-text {Accept} -command {CloseDialog " & MissionDialog &
           ";SetMission " & CArgv.Arg(Argv, 1) & "}");
      RewardLabel: constant Ttk_Label :=
        Create
          (MissionDialog & ".rewardlbl",
           "-text {Reward:" &
           Natural'Image
             (Natural(Float(Mission.Reward) * Float(Mission.Multiplier))) &
           " " & To_String(Money_Name) & "}");
      RewardScale: constant Ttk_Scale :=
        Create
          (MissionDialog & ".reward",
           "-from 0.0 -to 2.0 -variable reward -command {UpdateMissionReward " &
           CArgv.Arg(Argv, 1) & "} -length 300");
   begin
      Tcl_SetVar(Interp, "reward", "1.0");
      Add
        (RewardScale,
         "Move left - more reputation from mission but less money,\nmove right - more money from mission but less reputation.");
      Tcl.Tk.Ada.Grid.Grid(RewardLabel, "-columnspan 2 -padx 5");
      Tcl.Tk.Ada.Grid.Grid(RewardScale, "-columnspan 2 -padx 5");
      Tcl.Tk.Ada.Grid.Grid(Button, "-pady 5");
      Button :=
        Create
          (MissionDialog & ".cancel",
           "-text {Cancel} -command {CloseDialog " & MissionDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-row 3 -column 1 -pady 5");
      Bind(Button, "<Tab>", "{focus .missiondialog.accept;break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      Show_Dialog(MissionDialog);
      Focus(Button);
      return TCL_OK;
   end Accept_Mission_Command;

   -- ****o* MUI3/MIU3.Update_Mission_Reward_Command
   -- FUNCTION
   -- Update the information about the selected mission reward
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateMissionReward missionindex
   -- MissionIndex is the index of the mission to update info
   -- SOURCE
   function Update_Mission_Reward_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Mission_Reward_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      MissionIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      RewardLabel: constant Ttk_Label :=
        Get_Widget(".missiondialog.rewardlbl", Interp);
      Mission: constant Mission_Data :=
        SkyBases(BaseIndex).Missions(MissionIndex);
   begin
      configure
        (RewardLabel,
         "-text {Reward:" &
         Natural'Image
           (Natural
              (Float(Mission.Reward) *
               Float'Value(Tcl_GetVar(Interp, "reward")))) &
         " " & To_String(Money_Name) & "}");
      return TCL_OK;
   end Update_Mission_Reward_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowBaseMissions", Show_Base_Missions_Command'Access);
      AddCommand
        ("ShowBaseMissionMenu", Show_Base_Missions_Menu_Command'Access);
      AddCommand("MissionMoreInfo", Mission_More_Info_Command'Access);
      AddCommand("AcceptMission", Accept_Mission_Command'Access);
      AddCommand("UpdateMissionReward", Update_Mission_Reward_Command'Access);
   end AddCommands;

end Missions.UI;
