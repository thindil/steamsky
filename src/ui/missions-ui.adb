-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Containers.Generic_Array_Sort;
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
with Events; use Events;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Table; use Table;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Missions.UI is

   -- ****iv* MUI3/MIU3.Base_Index
   -- FUNCTION
   -- Index of the base in which available missions will be show
   -- SOURCE
   Base_Index: Natural;
   -- ****

   -- ****o* MUI3/MIU3.Show_Mission_Command
   -- FUNCTION
   -- Show mission on map
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMission missionindex
   -- MissionIndex is the index of the mission to show on map
   -- SOURCE
   function Show_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      Mission_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
   begin
      return
        Show_On_Map_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 3,
           Argv =>
             CArgv.Empty & CArgv.Arg(Argv => Argv, N => 0) &
             Map_X_Range'Image
               (Sky_Bases(Base_Index).Missions(Mission_Index).Target_X) &
             Map_Y_Range'Image
               (Sky_Bases(Base_Index).Missions(Mission_Index).Target_Y));
   end Show_Mission_Command;

   -- ****iv* MUI3/MUI3.Missions_Table
   -- FUNCTION
   -- Table with info about the known Missions
   -- SOURCE
   Missions_Table: Table_Widget (Amount => 5);
   -- ****

   -- ****iv* MUI3/MUI3.Missions_Indexes
   -- FUNCTION
   -- Indexes of the available missions in base
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Missions_Indexes: Positive_Container.Vector;
   -- ****

   -- ****if* MUI3/MUI3.Count_Missions_Amount
   -- FUNCTION
   -- Count the amount of missions which the player can get from the selected
   -- base
   -- RESULT
   -- The amount of missions which the player can get from the base
   -- SOURCE
   function Count_Missions_Amount return Natural is
      -- ****
      Missions_Limit: Natural;
   begin
      Missions_Limit :=
        (case Sky_Bases
           (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index)
           .Reputation
           .Level is
           when 0 .. 25 => 1, when 26 .. 50 => 3, when 51 .. 75 => 5,
           when 76 .. 100 => 10, when others => 0);
      Count_Missions_Limit_Loop :
      for Mission of Accepted_Missions loop
         if Mission.Start_Base =
           Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index then
            Missions_Limit := Missions_Limit - 1;
            exit Count_Missions_Limit_Loop when Missions_Limit = 0;
         end if;
      end loop Count_Missions_Limit_Loop;
      return Missions_Limit;
   end Count_Missions_Amount;

   -- ****if* MUI3/MUI3.Show_Base_Missions_Menu_Command
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
   -- ShowBaseMissionsMenu missionindex
   -- MissionIndex is the index of the mission's menu to show
   -- SOURCE
   function Show_Base_Missions_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Missions_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Mission_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Mission_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".missionlistmenu",
           Title =>
             (case Sky_Bases(Base_Index).Missions(Mission_Index).M_Type is
                when DELIVER => "Deliver item",
                when DESTROY => "Destroy enemy", when PATROL => "Patrol area",
                when EXPLORE => "Explore area",
                when PASSENGER => "Transport passenger") &
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
      Can_Accept: Boolean := True;
   begin
      Add_Button
        (Name => ".show", Label => "Show the mission on map",
         Command =>
           "ShowOnMap " &
           Map_X_Range'Image
             (Sky_Bases(Base_Index).Missions(Mission_Index).Target_X) &
           Map_Y_Range'Image
             (Sky_Bases(Base_Index).Missions(Mission_Index).Target_Y));
      if Sky_Bases(Base_Index).Missions(Mission_Index).M_Type = PASSENGER then
         Can_Accept := False;
         Modules_Loop :
         for Module of Player_Ship.Modules loop
            if (Module.M_Type = CABIN and not Can_Accept)
              and then Module.Quality >=
                Sky_Bases(Base_Index).Missions(Mission_Index).Data then
               Can_Accept := False;
               Can_Accept_Loop :
               for Owner of Module.Owner loop
                  if Owner = 0 then
                     Can_Accept := True;
                     exit Can_Accept_Loop;
                  end if;
               end loop Can_Accept_Loop;
               exit Modules_Loop when Can_Accept;
            end if;
         end loop Modules_Loop;
      end if;
      if Count_Missions_Amount = 0 then
         Can_Accept := False;
      end if;
      if Can_Accept then
         Add_Button
           (Name => ".accept", Label => "Accept the mission",
            Command => "AcceptMission " & CArgv.Arg(Argv => Argv, N => 1));
      end if;
      Add_Button
        (Name => ".info", Label => "Show more info",
         Command => "MissionMoreInfo " & CArgv.Arg(Argv => Argv, N => 1));
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Mission_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Base_Missions_Menu_Command;

   -- ****if* MUI3/MUI3.Refresh_Missions_List
   -- FUNCTION
   -- Refresh the list of available missions
   -- PARAMETERS
   -- List - The list of available missions in the selected base
   -- Page - The current page of the list to show. Can be empty. Default value
   --        is 1.
   -- SOURCE
   procedure Refresh_Missions_List
     (List: in out Mission_Container.Vector; Page: Positive := 1) is
      -- ****
      use Tiny_String;

      Row: Positive := 2;
      Rows: Natural := 0;
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
      Current_Row: Positive := 1;
      Mission_Time: Unbounded_String;
      Can_Accept: Boolean := True;
      Cabin_Taken: Boolean := False;
   begin
      if List.Length = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         Show_Sky_Map(Clear => True);
         return;
      end if;
      Show_Missions_Limit_Block :
      declare
         Missions_Limit: constant Natural := Count_Missions_Amount;
         Mission_Label: constant Ttk_Label :=
           Get_Widget
             (pathName =>
                Main_Paned & ".missionsframe.canvas.missions.missionslabel");
      begin
         if Missions_Limit > 0 then
            configure
              (Widgt => Mission_Label,
               options =>
                 "-text {You can take" & Natural'Image(Missions_Limit) &
                 " more missions in from base.}");
         else
            configure
              (Widgt => Mission_Label,
               options =>
                 "-text {You can't take any more missions from this base.}");
         end if;
      end Show_Missions_Limit_Block;
      if Missions_Table.Row > 1 then
         Clear_Table(Table => Missions_Table);
      end if;
      if Missions_Indexes.Length /= List.Length then
         Missions_Indexes.Clear;
         Set_Missions_Indexes_Loop :
         for I in List.Iterate loop
            Missions_Indexes.Append
              (New_Item => Mission_Container.To_Index(Position => I));
         end loop Set_Missions_Indexes_Loop;
      end if;
      Show_Missions_List_Loop :
      for I of Missions_Indexes loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         if List(I).M_Type = PASSENGER then
            Can_Accept := False;
            Modules_Loop :
            for Module of Player_Ship.Modules loop
               if (Module.M_Type = CABIN and not Can_Accept)
                 and then Module.Quality >= List(I).Data then
                  Can_Accept := False;
                  Cabin_Taken := True;
                  Can_Accept_Loop :
                  for Owner of Module.Owner loop
                     if Owner = 0 then
                        Cabin_Taken := False;
                        Can_Accept := True;
                        exit Can_Accept_Loop;
                     end if;
                  end loop Can_Accept_Loop;
                  exit Modules_Loop when Can_Accept;
               end if;
            end loop Modules_Loop;
         end if;
         Add_Button
           (Table => Missions_Table,
            Text => Get_Mission_Type(M_Type => List(I).M_Type),
            Tooltip => "Show available mission's options",
            Command => "ShowBaseMissionMenu" & Positive'Image(I), Column => 1,
            Color =>
              (if Can_Accept then "" elsif Cabin_Taken then "yellow"
               else "red"));
         Can_Accept := True;
         Cabin_Taken := False;
         case List(I).M_Type is
            when DELIVER =>
               Add_Button
                 (Table => Missions_Table,
                  Text => To_String
                    (Source => Objects_Container.Element
                       (Container => Items_List, Index => List(I).Item_Index)
                       .Name) &
                  " to " &
                  To_String
                    (Source => Sky_Bases
                       (Sky_Map(List(I).Target_X, List(I).Target_Y).Base_Index)
                       .Name),
                  Tooltip => "Show available mission's options",
                  Command => "ShowBaseMissionMenu" & Positive'Image(I), Column => 3);
            when PATROL =>
               Add_Button
                 (Table => Missions_Table,
                  Text => "X:" & Natural'Image(List(I).Target_X) & " Y:" &
                  Natural'Image(List(I).Target_Y),
                  Tooltip => "Show available mission's options",
                  Command => "ShowBaseMissionMenu" & Positive'Image(I), Column => 3);
            when DESTROY =>
               if List(I).Ship_Index = 0 then
                  Get_Enemy_Name_Block:
                  declare
                     Enemies: Positive_Container.Vector;
                  begin
                     Generate_Enemies
                       (Enemies => Enemies, With_Traders => False);
                     List(I).Ship_Index :=
                       Enemies
                         (Get_Random
                            (Min => Enemies.First_Index,
                             Max => Enemies.Last_Index));
                  end Get_Enemy_Name_Block;
               end if;
               Add_Button
                 (Table => Missions_Table,
                  Text => To_String(Source => Proto_Ships_List(List(I).Ship_Index).Name),
                  Tooltip => "Show available mission's options",
                  Command => "ShowBaseMissionMenu" & Positive'Image(I), Column => 3);
            when EXPLORE =>
               Add_Button
                 (Table => Missions_Table,
                  Text => "X:" & Natural'Image(List(I).Target_X) & " Y:" &
                  Natural'Image(List(I).Target_Y),
                  Tooltip => "Show available mission's options",
                  Command => "ShowBaseMissionMenu" & Positive'Image(I), Column => 3);
            when PASSENGER =>
               Add_Button
                 (Table => Missions_Table,
                  Text => "To " &
                  To_String
                    (Source => Sky_Bases
                       (Sky_Map(List(I).Target_X, List(I).Target_Y).Base_Index)
                       .Name),
                  Tooltip => "Show available mission's options",
                  Command => "ShowBaseMissionMenu" & Positive'Image(I), Column => 3);
         end case;
         Add_Button
           (Table => Missions_Table,
            Text => Natural'Image(Count_Distance(Destination_X => List(I).Target_X, Destination_Y => List(I).Target_Y)),
            Tooltip => "The distance to the mission",
            Command => "ShowBaseMissionMenu" & Positive'Image(I), Column => 2);
         Mission_Time := Null_Unbounded_String;
         Minutes_To_Date(Minutes => List(I).Time, Info_Text => Mission_Time);
         Add_Button
           (Table => Missions_Table, Text => To_String(Source => Mission_Time),
            Tooltip => "The time limit for finish and return the mission",
            Command => "ShowBaseMissionMenu" & Positive'Image(I), Column => 4);
         Add_Button
           (Table => Missions_Table,
            Text => Natural'Image
              (Natural(Float(List(I).Reward) * Float(List(I).Multiplier))) &
            " " & To_String(Source => Money_Name),
            Tooltip => "The base money reward for the mission",
            Command => "ShowBaseMissionMenu" & Positive'Image(I), Column => 5, New_Row => True);
         Row := Row + 1;
         Rows := Rows + 1;
         exit Show_Missions_List_Loop when Rows = 25 and I /= List.Last_Index;
         <<End_Of_Loop>>
      end loop Show_Missions_List_Loop;
      if Page > 1 then
         if Rows < 25 then
            Add_Pagination
              (Table => Missions_Table, Previous_Command => "ShowBaseMissions" & Positive'Image(Page - 1),
               Next_Command => "");
         else
            Add_Pagination
              (Table => Missions_Table, Previous_Command => "ShowBaseMissions" & Positive'Image(Page - 1),
               Next_Command => "ShowBaseMissions" & Positive'Image(Page + 1));
         end if;
      elsif Rows > 24 then
         Add_Pagination
           (Table => Missions_Table, Previous_Command => "", Next_Command => "ShowBaseMissions" & Positive'Image(Page + 1));
      end if;
   end Refresh_Missions_List;

   -- ****o* MUI3/MIU3.Set_Mission_Command
   -- FUNCTION
   -- Accept the mission in a base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetMission missionindex
   -- MissionIndex is the index of the mission to accept
   -- SOURCE
   function Set_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Mission_Index: constant Positive := Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
   begin
      Sky_Bases(Base_Index).Missions(Mission_Index).Multiplier :=
        Reward_Multiplier'Value(Tcl_GetVar(interp => Get_Context, varName => "reward"));
      Accept_Mission(Mission_Index => Mission_Index);
      Refresh_Missions_List(List => Sky_Bases(Base_Index).Missions);
      Update_Table(Table => Missions_Table);
      Update_Messages;
      return TCL_OK;
   exception
      when An_Exception : Missions_Accepting_Error =>
         Show_Message
           (Text => Exception_Message(X => An_Exception),
            Title => "Can't accept mission");
         return TCL_OK;
   end Set_Mission_Command;

   -- ****o* MUI3/MIU3.Show_Base_Missions_Command
   -- FUNCTION
   -- Show the list of available missions in the base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBaseMissions ?page?
   -- Page is the number of page of the missions list to show. If not
   -- set then it is 1
   -- SOURCE
   function Show_Base_Missions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Missions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
      use Interfaces.C;

      Missions_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".missionsframe", Interp => Interp);
      MissionsCanvas: constant Tk_Canvas :=
        Get_Widget(Missions_Frame & ".canvas", Interp);
      Label: constant Ttk_Label :=
        Get_Widget(MissionsCanvas & ".missions.missionslabel", Interp);
   begin
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "missions.tcl");
         Bind(Missions_Frame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         Add_Command("ShowMission", Show_Mission_Command'Access);
         Add_Command("SetMission", Set_Mission_Command'Access);
         Missions_Table :=
           Create_Table
             (MissionsCanvas & ".missions",
              (To_Unbounded_String("Name"), To_Unbounded_String("Distance"),
               To_Unbounded_String("Details"),
               To_Unbounded_String("Time limit"),
               To_Unbounded_String("Base reward")),
              Get_Widget(Main_Paned & ".missionsframe.scrolly"),
              "SortAvailableMissions",
              "Press mouse button to sort the missions.");
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
         Show_Sky_Map(True);
         return TCL_OK;
      end if;
      Tcl_SetVar(Interp, "gamestate", "missions");
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      Base_Index := Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      if Sky_Bases(Base_Index).Missions.Length = 0 then
         Show_Sky_Map(True);
         return TCL_OK;
      end if;
      Refresh_Missions_List
        (Sky_Bases(Base_Index).Missions,
         (if Argc > 1 then Positive'Value(CArgv.Arg(Argv, 1)) else 1));
      Update_Table(Missions_Table);
      configure
        (MissionsCanvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Missions_Frame.Name :=
        New_String(Widget_Image(MissionsCanvas) & ".missions");
      Canvas_Create
        (MissionsCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(Missions_Frame));
      Tcl_Eval(Get_Context, "update");
      configure
        (MissionsCanvas,
         "-scrollregion [list " & BBox(MissionsCanvas, "all") & "]");
      Show_Screen("missionsframe");
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
      use Tiny_String;

      MissionIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Mission: constant Mission_Data :=
        Sky_Bases(Base_Index).Missions(MissionIndex);
      MissionDialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".missiondialog",
           Title => "More info about " & Get_Mission_Type(Mission.M_Type));
      CanAccept: Boolean := True;
      CabinTaken: Boolean := False;
      Label: constant Ttk_Label :=
        Create(MissionDialog & ".infolabel", "-wraplength 400");
      MissionInfo: Unbounded_String := Null_Unbounded_String;
      Buttons_Frame: constant Ttk_Frame :=
        Create(pathName => MissionDialog & ".buttons");
      Button: Ttk_Button;
   begin
      Travel_Info
        (MissionInfo,
         (if Mission.M_Type in DELIVER | PASSENGER then
            Count_Distance(Mission.Target_X, Mission.Target_Y)
          else Count_Distance(Mission.Target_X, Mission.Target_Y) * 2),
         True);
      case Mission.M_Type is
         when DELIVER =>
            configure
              (Label,
               "-text {Item: " &
               To_String
                 (Objects_Container.Element
                    (Container => Items_List, Index => Mission.Item_Index)
                    .Name) &
               LF & "Weight:" &
               Positive'Image
                 (Objects_Container.Element
                    (Container => Items_List, Index => Mission.Item_Index)
                    .Weight) &
               " kg" & LF & "To base: " &
               To_String
                 (Sky_Bases
                    (Sky_Map(Mission.Target_X, Mission.Target_Y).Base_Index)
                    .Name) &
               To_String(MissionInfo) & "}");
         when PATROL =>
            configure
              (Label,
               "-text {Patrol selected area" & To_String(MissionInfo) & "}");
         when DESTROY =>
            configure
              (Label,
               "-text {Target: " &
               To_String(Proto_Ships_List(Mission.Ship_Index).Name) &
               To_String(MissionInfo) & "}");
         when EXPLORE =>
            configure
              (Label,
               "-text {Explore selected area" & To_String(MissionInfo) & "}");
         when PASSENGER =>
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
            if Base_Index = 0 then
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
                 (Sky_Bases
                    (Sky_Map(Mission.Target_X, Mission.Target_Y).Base_Index)
                    .Name) &
               To_String(MissionInfo) & "}");
      end case;
      Tcl.Tk.Ada.Grid.Grid(Label, "-padx 5");
      Button :=
        Create
          (pathName => Buttons_Frame & ".button1",
           options =>
             "-text Show -image showicon -command {CloseDialog " &
             MissionDialog & ";ShowOnMap " &
             Map_X_Range'Image
               (Sky_Bases(Base_Index).Missions(MissionIndex).Target_X) &
             Map_Y_Range'Image
               (Sky_Bases(Base_Index).Missions(MissionIndex).Target_Y) &
             "} -style Dialog.TButton");
      Add(Widget => Button, Message => "Show the mission on the map");
      Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-padx 5");
      Bind
        (Widgt => Button, Sequence => "<Tab>",
         Script => "{focus " & Buttons_Frame & ".button;break}");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Buttons_Frame & ".button invoke;break}");
      Add_Close_Button
        (Name => Buttons_Frame & ".button", Text => "Close",
         Command => "CloseDialog " & MissionDialog, Column => 1);
      if CanAccept then
         Button := Get_Widget(pathName => Buttons_Frame & ".button");
         Bind
           (Widgt => Button, Sequence => "<Tab>",
            Script => "{focus " & Buttons_Frame & ".button2;break}");
         Button :=
           Create
             (pathName => Buttons_Frame & ".button2",
              options =>
                "-text Accept -image negotiateicon -command {CloseDialog " &
                MissionDialog & ";AcceptMission " &
                CArgv.Arg(Argv => Argv, N => 1) & "} -style Dialog.TButton");
         Add
           (Widget => Button,
            Message => "Start negiotiating accepting the mission");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button, Options => "-row 0 -column 2 -padx 5");
         Bind
           (Widgt => Button, Sequence => "<Tab>",
            Script => "{focus " & Buttons_Frame & ".button1;break}");
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{" & Buttons_Frame & ".button invoke;break}");
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Buttons_Frame, Options => "-padx 5 -pady 5");
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
        Sky_Bases(Base_Index).Missions(MissionIndex);
      MissionDialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".missiondialog",
           Title => "Accept " & Get_Mission_Type(Mission.M_Type),
           Columns => 2);
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
           "-text Cancel -command {CloseDialog " & MissionDialog &
           "} -image cancelicon -style Dialog.TButton");
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
        Sky_Bases(Base_Index).Missions(MissionIndex);
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

   -- ****it* MUI3/MUI3.Missions_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the list of available missions
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
   -- NONE       - No sorting missions (default)
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   type Missions_Sort_Orders is
     (TYPEASC, TYPEDESC, DISTANCEASC, DISTANCEDESC, DETAILSASC, DETAILSDESC,
      TIMEASC, TIMEDESC, REWARDASC, REWARDDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* MUI3/MUI3.Default_Missions_Sort_Order
      -- FUNCTION
      -- Default sorting order for the list of available missions
      -- HISTORY
      -- 6.5 - Added
      -- SOURCE
   Default_Missions_Sort_Order: constant Missions_Sort_Orders := NONE;
   -- ****

   -- ****iv* MUI3/MUI3.Missions_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of available missions
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Missions_Sort_Order: Missions_Sort_Orders := Default_Missions_Sort_Order;
   -- ****

   -- ****o* MUI3/MUI3.Sort_Available_Missions_Command
   -- FUNCTION
   -- Sort the list of available missions
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- HISTORY
   -- 6.5 - Added
   -- COMMANDS
   -- SortAvailableMissions x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Available_Missions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Available_Missions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number(Missions_Table, Natural'Value(CArgv.Arg(Argv, 1)));
      type Local_Mission_Data is record
         MType: Missions_Types;
         Distance: Natural;
         Details: Unbounded_String;
         Time: Natural;
         Reward: Natural;
         Id: Positive;
      end record;
      type Missions_Array is array(Positive range <>) of Local_Mission_Data;
      Local_Missions: Missions_Array
        (1 .. Positive(Sky_Bases(Base_Index).Missions.Length));
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
      for I in Sky_Bases(Base_Index).Missions.Iterate loop
         Local_Missions(Mission_Container.To_Index(I)) :=
           (MType => Sky_Bases(Base_Index).Missions(I).M_Type,
            Distance =>
              Count_Distance
                (Sky_Bases(Base_Index).Missions(I).Target_X,
                 Sky_Bases(Base_Index).Missions(I).Target_Y),
            Details =>
              (case Sky_Bases(Base_Index).Missions(I).M_Type is
                 when DELIVER =>
                   To_String
                     (Source =>
                        Objects_Container.Element
                          (Container => Items_List,
                           Index =>
                             Sky_Bases(Base_Index).Missions(I).Item_Index)
                          .Name) &
                   To_Unbounded_String(Source => " to ") &
                   To_String
                     (Source =>
                        Sky_Bases
                          (Sky_Map
                             (Sky_Bases(Base_Index).Missions(I).Target_X,
                              Sky_Bases(Base_Index).Missions(I).Target_Y)
                             .Base_Index)
                          .Name),
                 when PATROL =>
                   To_Unbounded_String
                     ("X:" &
                      Natural'Image
                        (Sky_Bases(Base_Index).Missions(I).Target_X) &
                      " Y:" &
                      Natural'Image
                        (Sky_Bases(Base_Index).Missions(I).Target_Y)),
                 when DESTROY =>
                   To_Unbounded_String
                     (Source =>
                        To_String
                          (Source =>
                             Proto_Ships_List
                               (Sky_Bases(Base_Index).Missions(I).Ship_Index)
                               .Name)),
                 when EXPLORE =>
                   To_Unbounded_String
                     ("X:" &
                      Natural'Image
                        (Sky_Bases(Base_Index).Missions(I).Target_X) &
                      " Y:" &
                      Natural'Image
                        (Sky_Bases(Base_Index).Missions(I).Target_Y)),
                 when PASSENGER =>
                   To_Unbounded_String(Source => "To ") &
                   To_String
                     (Source =>
                        Sky_Bases
                          (Sky_Map
                             (Sky_Bases(Base_Index).Missions(I).Target_X,
                              Sky_Bases(Base_Index).Missions(I).Target_Y)
                             .Base_Index)
                          .Name)),
            Time => Sky_Bases(Base_Index).Missions(I).Time,
            Reward => Sky_Bases(Base_Index).Missions(I).Reward,
            Id => Mission_Container.To_Index(I));
      end loop;
      Sort_Missions(Local_Missions);
      Missions_Indexes.Clear;
      for Mission of Local_Missions loop
         Missions_Indexes.Append(Mission.Id);
      end loop;
      Refresh_Missions_List(Sky_Bases(Base_Index).Missions, 1);
      Update_Table(Missions_Table);
      return TCL_OK;
   end Sort_Available_Missions_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowBaseMissions", Show_Base_Missions_Command'Access);
      Add_Command
        ("ShowBaseMissionMenu", Show_Base_Missions_Menu_Command'Access);
      Add_Command("MissionMoreInfo", Mission_More_Info_Command'Access);
      Add_Command("AcceptMission", Accept_Mission_Command'Access);
      Add_Command("UpdateMissionReward", Update_Mission_Reward_Command'Access);
      Add_Command
        ("SortAvailableMissions", Sort_Available_Missions_Command'Access);
   end Add_Commands;

end Missions.UI;
