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

with Ada.Characters.Latin_1;
with Ada.Containers.Generic_Array_Sort;
with Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with CoreUI; use CoreUI;
with Config;
with Dialogs; use Dialogs;
with Events;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Table; use Table;
with Utils;
with Utils.UI; use Utils.UI;

package body Missions.UI is

   -- ****iv* MUI3/MIU3.Base_Index
   -- FUNCTION
   -- Index of the base in which available missions will be show
   -- SOURCE
   Base_Index: Natural;
   -- ****

   -- ****if* MUI3/MUI3.Get_Base_Index
   -- FUNCTION
   -- Get the base index
   -- RESULT
   -- The current base index in the list of bases
   -- SOURCE
   function Get_Base_Index return Natural is
      -- ****
   begin
      return Base_Index;
   end Get_Base_Index;

   -- ****if* MUI3/MIU3.Show_Mission_Command
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
               (Sky_Bases(Get_Base_Index).Missions(Mission_Index).Target_X) &
             Map_Y_Range'Image
               (Sky_Bases(Get_Base_Index).Missions(Mission_Index).Target_Y));
   end Show_Mission_Command;

   -- ****iv* MUI3/MUI3.Missions_Table
   -- FUNCTION
   -- Table with info about the known Missions
   -- SOURCE
   Missions_Table: Table_Widget (Amount => 6);
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
      Mission: Mission_Data := Empty_Mission;
   begin
      Missions_Limit :=
        (case Sky_Bases
           (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index)
           .Reputation
           .Level is
           when 0 .. 25 => 1, when 26 .. 50 => 3, when 51 .. 75 => 5,
           when 76 .. 100 => 10, when others => 0);
      Count_Missions_Limit_Loop :
      for I in 1 .. Get_Accepted_Missions_Amount loop
         Mission := Get_Accepted_Mission(Mission_Index => I);
         if Mission.Start_Base =
           Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index then
            Missions_Limit := Missions_Limit - 1;
            exit Count_Missions_Limit_Loop when Missions_Limit = 0;
         end if;
      end loop Count_Missions_Limit_Loop;
      return Missions_Limit;
   end Count_Missions_Amount;

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
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      Current_Row: Positive := 1;
      Mission_Time: Unbounded_String := Null_Unbounded_String;
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
                Main_Paned &
                ".missionsframe.canvas.missions.missionslabel.missionslbl2");
      begin
         configure
           (Widgt => Mission_Label,
            options => "-text {" & Trim(Source => Natural'Image(Missions_Limit), Side => Left) & "}");
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
            Tooltip => "Show more info about the mission",
            Command => "MissionMoreInfo" & Positive'Image(I), Column => 1,
            Color =>
              (if Can_Accept then "" elsif Cabin_Taken then "yellow"
               else "red"));
         Can_Accept := True;
         Cabin_Taken := False;
         case List(I).M_Type is
            when DELIVER =>
               Add_Button
                 (Table => Missions_Table,
                  Text =>
                    To_String
                      (Source =>
                         Get_Proto_Item(Index => List(I).Item_Index).Name) &
                    " to " &
                    To_String
                      (Source =>
                         Sky_Bases
                           (Sky_Map(List(I).Target_X, List(I).Target_Y)
                              .Base_Index)
                           .Name),
                  Tooltip => "Show more info about the mission",
                  Command => "MissionMoreInfo" & Positive'Image(I),
                  Column => 4);
            when PATROL =>
               Add_Button
                 (Table => Missions_Table,
                  Text =>
                    "X:" & Natural'Image(List(I).Target_X) & " Y:" &
                    Natural'Image(List(I).Target_Y),
                  Tooltip => "Show more info about the mission",
                  Command => "MissionMoreInfo" & Positive'Image(I),
                  Column => 4);
            when DESTROY =>
               if List(I).Ship_Index = 0 then
                  --## rule off IMPROPER_INITIALIZATION
                  Get_Enemy_Name_Block :
                  declare
                     use Events;
                     use Utils;

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
                  --## rule on IMPROPER_INITIALIZATION
               end if;
               Add_Button
                 (Table => Missions_Table,
                  Text =>
                    To_String
                      (Source =>
                         Get_Proto_Ship(Proto_Index => List(I).Ship_Index)
                           .Name),
                  Tooltip => "Show more info about the mission",
                  Command => "MissionMoreInfo" & Positive'Image(I),
                  Column => 4);
            when EXPLORE =>
               Add_Button
                 (Table => Missions_Table,
                  Text =>
                    "X:" & Natural'Image(List(I).Target_X) & " Y:" &
                    Natural'Image(List(I).Target_Y),
                  Tooltip => "Show more info about the mission",
                  Command => "MissionMoreInfo" & Positive'Image(I),
                  Column => 4);
            when PASSENGER =>
               Add_Button
                 (Table => Missions_Table,
                  Text =>
                    "To " &
                    To_String
                      (Source =>
                         Sky_Bases
                           (Sky_Map(List(I).Target_X, List(I).Target_Y)
                              .Base_Index)
                           .Name),
                  Tooltip => "Show more info about the mission",
                  Command => "MissionMoreInfo" & Positive'Image(I),
                  Column => 4);
         end case;
         Add_Button
           (Table => Missions_Table,
            Text =>
              Natural'Image
                (Count_Distance
                   (Destination_X => List(I).Target_X,
                    Destination_Y => List(I).Target_Y)),
            Tooltip => "The distance to the mission",
            Command => "MissionMoreInfo" & Positive'Image(I), Column => 2);
         Add_Button
           (Table => Missions_Table,
            Text =>
              "X:" & Natural'Image(List(I).Target_X) & " Y:" &
              Natural'Image(List(I).Target_Y),
            Tooltip => "Show more info about the mission",
            Command => "MissionMoreInfo" & Positive'Image(I), Column => 3);
         Mission_Time := Null_Unbounded_String;
         Minutes_To_Date(Minutes => List(I).Time, Info_Text => Mission_Time);
         Add_Button
           (Table => Missions_Table, Text => To_String(Source => Mission_Time),
            Tooltip => "The time limit for finish and return the mission",
            Command => "MissionMoreInfo" & Positive'Image(I), Column => 5);
         Add_Button
           (Table => Missions_Table,
            Text =>
              Natural'Image
                (Natural(Float(List(I).Reward) * Float(List(I).Multiplier))) &
              " " & To_String(Source => Money_Name),
            Tooltip => "The base money reward for the mission",
            Command => "MissionMoreInfo" & Positive'Image(I), Column => 6,
            New_Row => True);
         Row := Row + 1;
         Rows := Rows + 1;
         exit Show_Missions_List_Loop when Rows = 25 and I /= List.Last_Index;
         <<End_Of_Loop>>
      end loop Show_Missions_List_Loop;
      if Page > 1 then
         if Rows < 25 then
            Add_Pagination
              (Table => Missions_Table,
               Previous_Command =>
                 "ShowBaseMissions" & Positive'Image(Page - 1),
               Next_Command => "");
         else
            Add_Pagination
              (Table => Missions_Table,
               Previous_Command =>
                 "ShowBaseMissions" & Positive'Image(Page - 1),
               Next_Command => "ShowBaseMissions" & Positive'Image(Page + 1));
         end if;
      elsif Rows > 24 then
         Add_Pagination
           (Table => Missions_Table, Previous_Command => "",
            Next_Command => "ShowBaseMissions" & Positive'Image(Page + 1));
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
      use Ada.Exceptions;

      Mission_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
   begin
      Sky_Bases(Get_Base_Index).Missions(Mission_Index).Multiplier :=
        Reward_Multiplier
          (Float'Value
             (Tcl_GetVar(interp => Get_Context, varName => "reward")) /
           100.0);
      Accept_Mission(Mission_Index => Mission_Index);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      if Count_Missions_Amount > 0 then
         Refresh_Missions_List(List => Sky_Bases(Get_Base_Index).Missions);
         Update_Table(Table => Missions_Table);
         Update_Messages;
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         Show_Sky_Map(Clear => True);
      end if;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
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
      use Interfaces.C.Strings;
      use GNAT.Directory_Operations;
      use Tcl.Tk.Ada.Widgets.Canvas;
      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use Tcl.Tk.Ada.Winfo;

      Missions_Frame: Ttk_Frame :=
        Get_Widget
          (pathName => Main_Paned & ".missionsframe", Interp => Interp);
      Missions_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Missions_Frame & ".canvas", Interp => Interp);
      Label: constant Ttk_Label :=
        Get_Widget
          (pathName => Missions_Canvas & ".missions.missionslabel",
           Interp => Interp);
   begin
      if Winfo_Get(Widgt => Label, Info => "exists") = "0" then
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "missions.tcl");
         Bind
           (Widgt => Missions_Frame, Sequence => "<Configure>",
            Script => "{ResizeCanvas %W.canvas %w %h}");
         Add_Command
           (Name => "ShowMission", Ada_Command => Show_Mission_Command'Access);
         Add_Command
           (Name => "SetMission", Ada_Command => Set_Mission_Command'Access);
         --## rule off DIRECTLY_ACCESSED_GLOBALS
         Missions_Table :=
           Create_Table
             (Parent => Missions_Canvas & ".missions",
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Distance"),
                 3 => To_Unbounded_String(Source => "Coordinates"),
                 4 => To_Unbounded_String(Source => "Details"),
                 5 => To_Unbounded_String(Source => "Time limit"),
                 6 => To_Unbounded_String(Source => "Base reward")),
              Scrollbar =>
                Get_Widget(pathName => Main_Paned & ".missionsframe.scrolly"),
              Command => "SortAvailableMissions",
              Tooltip_Text => "Press mouse button to sort the missions.");
         --## rule on DIRECTLY_ACCESSED_GLOBALS
      elsif Winfo_Get(Widgt => Label, Info => "ismapped") = "1" and
        Argc = 1 then
         Show_Sky_Map(Clear => True);
         return TCL_OK;
      end if;
      Tcl_SetVar
        (interp => Interp, varName => "gamestate", newValue => "missions");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Button, Options => "-row 0 -column 1");
      Set_Ada_Ship(Ship => Player_Ship);
      Base_Index := Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Get_Base_From_Nim(Base_Index => Get_Base_Index);
      if Sky_Bases(Get_Base_Index).Missions.Length = 0 then
         Show_Sky_Map(Clear => True);
         return TCL_OK;
      end if;
      Refresh_Missions_List
        (List => Sky_Bases(Get_Base_Index).Missions,
         Page =>
           (if Argc > 1 then Positive'Value(CArgv.Arg(Argv => Argv, N => 1))
            else 1));
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Update_Table(Table => Missions_Table);
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      configure
        (Widgt => Missions_Canvas,
         options =>
           "-height [expr " & SashPos(Paned => Main_Paned, Index => "0") &
           " - 20] -width " & cget(Widgt => Main_Paned, option => "-width"));
      Tcl_Eval(interp => Get_Context, strng => "update");
      Missions_Frame.Name :=
        New_String(Str => Widget_Image(Win => Missions_Canvas) & ".missions");
      Canvas_Create
        (Parent => Missions_Canvas, Child_Type => "window",
         Options =>
           "0 0 -anchor nw -window " & Widget_Image(Win => Missions_Frame));
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Missions_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Missions_Canvas, TagOrId => "all") & "]");
      Show_Screen(New_Screen_Name => "missionsframe");
      return TCL_OK;
   end Show_Base_Missions_Command;

   -- ****o* MUI3/MIU3.Mission_More_Info_Command
   -- FUNCTION
   -- Show more info about the selected mission
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MissionMoreInfo missionindex
   -- MissionIndex is the index of the mission's info to show
   -- SOURCE
   function Mission_More_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Mission_More_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Ada.Characters.Latin_1;
      use Tcl.Tk.Ada.Widgets.Text;
      use Config;
      use Tiny_String;

      Mission_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Mission: constant Mission_Data :=
        Sky_Bases(Get_Base_Index).Missions(Mission_Index);
      Mission_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".missiondialog",
           Title =>
             "More info about " & Get_Mission_Type(M_Type => Mission.M_Type));
      Can_Accept: Boolean := True;
      Cabin_Taken: Boolean := False;
      Label: constant Tk_Text :=
        Create
          (pathName => Mission_Dialog & ".infolabel",
           options => "-height 5 -width 30");
      Mission_Info: Unbounded_String := Null_Unbounded_String;
      Buttons_Frame: constant Ttk_Frame :=
        Create(pathName => Mission_Dialog & ".buttons");
      Button: Ttk_Button; --## rule line off IMPROPER_INITIALIZATION
      Travel_Values: constant Travel_Array :=
        Travel_Info
          (Distance =>
             (if Mission.M_Type in DELIVER | PASSENGER then
                Count_Distance
                  (Destination_X => Mission.Target_X,
                   Destination_Y => Mission.Target_Y)
              else Count_Distance
                  (Destination_X => Mission.Target_X,
                   Destination_Y => Mission.Target_Y) *
                2));
   begin
      Tag_Configure
        (TextWidget => Label, TagName => "gold",
         Options =>
           "-foreground " &
           Tcl_GetVar
             (interp => Interp,
              varName =>
                "ttk::theme::" & To_String(Source => Get_Interface_Theme) &
                "::colors(-goldenyellow)"));
      Tag_Configure
        (TextWidget => Label, TagName => "red",
         Options =>
           "-foreground " &
           Tcl_GetVar
             (interp => Interp,
              varName =>
                "ttk::theme::" & To_String(Source => Get_Interface_Theme) &
                "::colors(-red)"));
      Tag_Configure
        (TextWidget => Label, TagName => "green",
         Options =>
           "-foreground " &
           Tcl_GetVar
             (interp => Interp,
              varName =>
                "ttk::theme::" & To_String(Source => Get_Interface_Theme) &
                "::colors(-green)"));
      case Mission.M_Type is
         when DELIVER =>
            Insert(TextWidget => Label, Index => "end", Text => "{Item: }");
            Insert
              (TextWidget => Label, Index => "end",
               Text =>
                 "{" &
                 To_String
                   (Source =>
                      Get_Proto_Item(Index => Mission.Item_Index).Name) &
                 "} [list gold]");
            Insert
              (TextWidget => Label, Index => "end",
               Text => "{" & LF & "Weight:}");
            Insert
              (TextWidget => Label, Index => "end",
               Text =>
                 "{" &
                 Positive'Image
                   (Get_Proto_Item(Index => Mission.Item_Index).Weight) &
                 " kg} [list gold]");
            Insert
              (TextWidget => Label, Index => "end",
               Text => "{" & LF & "To base: }");
            Insert
              (TextWidget => Label, Index => "end",
               Text =>
                 "{" &
                 To_String
                   (Source =>
                      Sky_Bases
                        (Sky_Map(Mission.Target_X, Mission.Target_Y)
                           .Base_Index)
                        .Name) &
                 "} [list gold]");
         when PATROL =>
            Insert
              (TextWidget => Label, Index => "end",
               Text => "{Patrol selected area" & "} [list gold]");
         when DESTROY =>
            Insert(TextWidget => Label, Index => "end", Text => "{Target: }");
            Insert
              (TextWidget => Label, Index => "end",
               Text =>
                 "{" &
                 To_String
                   (Source =>
                      Get_Proto_Ship(Proto_Index => Mission.Ship_Index).Name) &
                 "} [list gold]");
         when EXPLORE =>
            Insert
              (TextWidget => Label, Index => "end",
               Text =>
                 "{Explore selected area" & To_String(Source => Mission_Info) &
                 "} [list gold]");
         when PASSENGER =>
            Can_Accept := False;
            Modules_Loop :
            for Module of Player_Ship.Modules loop
               if (Module.M_Type = CABIN and not Can_Accept)
                 and then Module.Quality >= Mission.Data then
                  Can_Accept := True;
                  Cabin_Taken := False;
                  Owners_Loop :
                  for Owner of Module.Owner loop
                     if Owner > 0 then
                        Cabin_Taken := True;
                        Can_Accept := False;
                        exit Owners_Loop;
                     end if;
                  end loop Owners_Loop;
                  exit Modules_Loop when Can_Accept;
               end if;
            end loop Modules_Loop;
            if Get_Base_Index = 0 then
               Can_Accept := True;
            end if;
            Insert
              (TextWidget => Label, Index => "end",
               Text => "{Needed quality of cabin: }");
            Insert
              (TextWidget => Label, Index => "end",
               Text =>
                 "{" & Get_Cabin_Quality(Quality => Mission.Data) &
                 (if Can_Accept then " [list green]"
                  elsif Cabin_Taken then " (taken)} [list gold]"
                  else " (no cabin)} [list red]"));
            Insert
              (TextWidget => Label, Index => "end",
               Text => "{" & LF & "To base: }");
            Insert
              (TextWidget => Label, Index => "end",
               Text =>
                 "{" &
                 To_String
                   (Source =>
                      Sky_Bases
                        (Sky_Map(Mission.Target_X, Mission.Target_Y)
                           .Base_Index)
                        .Name) &
                 "} [list gold]");
      end case;
      if Travel_Values(1) > 0 then
         Minutes_To_Date
           (Minutes => Travel_Values(1), Info_Text => Mission_Info);
         Insert
           (TextWidget => Label, Index => "end", Text => "{" & LF & "ETA:}");
         Insert
           (TextWidget => Label, Index => "end",
            Text => "{" & To_String(Source => Mission_Info) & "} [list gold]");
         Insert
           (TextWidget => Label, Index => "end",
            Text => "{" & LF & "Approx fuel usage:}");
         Insert
           (TextWidget => Label, Index => "end",
            Text => "{" & Positive'Image(Travel_Values(2)) & " } [list gold]");
         Insert
           (TextWidget => Label, Index => "end",
            Text =>
              "{" &
              To_String
                (Source =>
                   Get_Proto_Item
                     (Index => Find_Proto_Item(Item_Type => Fuel_Type))
                     .Name) &
              "}");
      end if;
      configure(Widgt => Label, options => "-state disabled");
      Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-padx 5");
      Button :=
        Create
          (pathName => Buttons_Frame & ".button1",
           options =>
             "-text Show -image show2icon -command {CloseDialog " &
             Mission_Dialog & ";set mappreview 1;ShowOnMap " &
             Map_X_Range'Image
               (Sky_Bases(Get_Base_Index).Missions(Mission_Index).Target_X) &
             Map_Y_Range'Image
               (Sky_Bases(Get_Base_Index).Missions(Mission_Index).Target_Y) &
             "} -style Dialoggreen.TButton");
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
         Command => "CloseDialog " & Mission_Dialog, Column => 1);
      if Can_Accept then
         Button := Get_Widget(pathName => Buttons_Frame & ".button");
         Bind
           (Widgt => Button, Sequence => "<Tab>",
            Script => "{focus " & Buttons_Frame & ".button2;break}");
         Button :=
           Create
             (pathName => Buttons_Frame & ".button2",
              options =>
                "-text Accept -image negotiateicon -command {CloseDialog " &
                Mission_Dialog & ";AcceptMission " &
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
      Show_Dialog(Dialog => Mission_Dialog);
      return TCL_OK;
   end Mission_More_Info_Command;

   -- ****o* MUI3/MIU3.Accept_Mission_Command
   -- FUNCTION
   -- Accept the mission in a base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- AcceptMission missionindex
   -- MissionIndex is the index of the mission to accept
   -- SOURCE
   function Accept_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Accept_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
      use Tcl.Tk.Ada.Widgets.TtkScale;

      Mission_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Mission: constant Mission_Data :=
        Sky_Bases(Get_Base_Index).Missions(Mission_Index);
      Mission_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".missiondialog",
           Title => "Accept " & Get_Mission_Type(M_Type => Mission.M_Type),
           Columns => 2);
      Buttons_Box: constant Ttk_Frame :=
        Create(pathName => Mission_Dialog & ".buttons");
      Button: Ttk_Button :=
        Create
          (pathName => Buttons_Box & ".accept",
           options =>
             "-text Accept -command {CloseDialog " & Mission_Dialog &
             ";SetMission " & CArgv.Arg(Argv => Argv, N => 1) &
             "} -image negotiate2icon -style Dialoggreen.TButton");
      Reward_Box: constant Ttk_Frame :=
        Create(pathName => Mission_Dialog & ".rewardbox");
      Reward_Label: Ttk_Label :=
        Create
          (pathName => Reward_Box & ".rewardlbl",
           options => "-text {Reward:}");
      Reward_Scale: constant Ttk_Scale :=
        Create
          (pathName => Mission_Dialog & ".reward",
           options =>
             "-from 0 -to 200 -variable reward -command {UpdateMissionReward " &
             CArgv.Arg(Argv => Argv, N => 1) & "} -length 300");
      Reward_Field: constant Ttk_SpinBox :=
        Create
          (pathName => Mission_Dialog & ".rewardfield",
           options =>
             "-from 0 -to 200 -textvariable reward -validate key -validatecommand {ValidateSpinbox %W %P " &
             Button & "} -width 3");
   begin
      Add
        (Widget => Reward_Scale,
         Message =>
           "Move left - more reputation from mission but less money,\nmove right - more money from mission but less reputation.");
      Add
        (Widget => Reward_Field,
         Message =>
           "Lower value - more reputation from mission but less money,\nhigher value - more money from mission but less reputation.");
      Tcl.Tk.Ada.Grid.Grid(Slave => Reward_Label, Options => "-stick w");
      Reward_Label :=
        Create
          (pathName => Reward_Box & ".rewardlbl2",
           options =>
             "-text {" &
             Natural'Image
               (Natural(Float(Mission.Reward) * Float(Mission.Multiplier))) &
             " " & To_String(Source => Money_Name) & "} -style Golden.TLabel");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Reward_Label, Options => "-row 0 -column 1 -stick w");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Reward_Box, Options => "-columnspan 2 -padx 5 -stick w");
      Reward_Label :=
        Create
          (pathName => Mission_Dialog & ".rewardinfo",
           options =>
             "-text {Percent of " & To_String(Source => Money_Name) &
             " as reward:}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Reward_Label, Options => "-columnspan 2 -padx 5 -stick w");
      Tcl_SetVar(interp => Interp, varName => "reward", newValue => "100");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Reward_Scale, Options => "-padx {5 0} -stick w");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Reward_Field,
         Options => "-row 3 -column 1 -padx {0 5} -stick w");
      Bind
        (Widgt => Reward_Field, Sequence => "<Tab>",
         Script => "{focus " & Button & ";break}");
      Bind
        (Widgt => Reward_Field, Sequence => "<Escape>",
         Script => "{" & Mission_Dialog & ".buttons.cancel invoke;break}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-pady 5");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Mission_Dialog & ".buttons.cancel invoke;break}");
      Button :=
        Create
          (pathName => Buttons_Box & ".cancel",
           options =>
             "-text Cancel -command {CloseDialog " & Mission_Dialog &
             "} -image cancelicon -style Dialogred.TButton");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button, Options => "-row 0 -column 1 -pady 5 -padx 5");
      Bind
        (Widgt => Button, Sequence => "<Tab>",
         Script => "{focus " & Reward_Scale & ";break}");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Button & " invoke;break}");
      Bind
        (Widgt => Reward_Scale, Sequence => "<Escape>",
         Script => "{" & Button & " invoke;break}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Buttons_Box, Options => "-columnspan 2 -pady 5");
      Show_Dialog(Dialog => Mission_Dialog);
      Focus(Widgt => Button);
      return TCL_OK;
   end Accept_Mission_Command;

   -- ****o* MUI3/MIU3.Update_Mission_Reward_Command
   -- FUNCTION
   -- Update the information about the selected mission reward
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateMissionReward missionindex
   -- MissionIndex is the index of the mission to update info
   -- SOURCE
   function Update_Mission_Reward_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Mission_Reward_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);

      Mission_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Reward_Label: constant Ttk_Label :=
        Get_Widget
          (pathName => ".missiondialog.rewardbox.rewardlbl2",
           Interp => Interp);
      Mission: constant Mission_Data :=
        Sky_Bases(Get_Base_Index).Missions(Mission_Index);
      Value: constant Natural :=
        Natural
          (Float'Value(Tcl_GetVar(interp => Interp, varName => "reward")));
   begin
      Tcl_SetVar
        (interp => Interp, varName => "reward",
         newValue => Trim(Source => Natural'Image(Value), Side => Left));
      configure
        (Widgt => Reward_Label,
         options =>
           "-text {" &
           Natural'Image
             (Natural(Float(Mission.Reward) * (Float(Value) / 100.0))) &
           " " & To_String(Source => Money_Name) & "}");
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
   -- COORDASC     - Sort missions by coordinates ascending
   -- COORDDESC    - Sort missions by coordinates descending
   -- NONE         - No sorting missions (default)
   -- HISTORY
   -- 6.5 - Added
   -- 8.4 - Added sorting by coordinates
   -- SOURCE
   type Missions_Sort_Orders is
     (TYPEASC, TYPEDESC, DISTANCEASC, DISTANCEDESC, DETAILSASC, DETAILSDESC,
      TIMEASC, TIMEDESC, REWARDASC, REWARDDESC, COORDASC, COORDDESC, NONE) with
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

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* MUI3/MUI3.Missions_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of available missions
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Missions_Sort_Order: Missions_Sort_Orders := Default_Missions_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* MUI3/MUI3.Sort_Available_Missions_Command
   -- FUNCTION
   -- Sort the list of available missions
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- HISTORY
   -- 6.5 - Added
   -- COMMANDS
   -- SortAvailableMissions x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Available_Missions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Available_Missions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Column: constant Positive :=
        Get_Column_Number
          (Table => Missions_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
      --## rule on DIRECTLY_ACCESSED_GLOBALS
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
      Local_Missions: Missions_Array
        (1 .. Positive(Sky_Bases(Get_Base_Index).Missions.Length));
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
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      procedure Sort_Missions is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Mission_Data,
         Array_Type => Missions_Array);
   begin
      --## rule off DIRECTLY_ACCESSED_GLOBALS
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
      for I in Sky_Bases(Get_Base_Index).Missions.Iterate loop
         Local_Missions(Mission_Container.To_Index(Position => I)) :=
           (M_Type => Sky_Bases(Get_Base_Index).Missions(I).M_Type,
            Distance =>
              Count_Distance
                (Destination_X =>
                   Sky_Bases(Get_Base_Index).Missions(I).Target_X,
                 Destination_Y =>
                   Sky_Bases(Get_Base_Index).Missions(I).Target_Y),
            Coords =>
              To_Unbounded_String
                (Source =>
                   "X:" &
                   Natural'Image
                     (Sky_Bases(Get_Base_Index).Missions(I).Target_X) &
                   " Y:" &
                   Natural'Image
                     (Sky_Bases(Get_Base_Index).Missions(I).Target_Y)),
            Details =>
              (case Sky_Bases(Get_Base_Index).Missions(I).M_Type is
                 when DELIVER =>
                   To_String
                     (Source =>
                        Get_Proto_Item
                          (Index =>
                             Sky_Bases(Get_Base_Index).Missions(I).Item_Index)
                          .Name) &
                   To_Unbounded_String(Source => " to ") &
                   To_String
                     (Source =>
                        Sky_Bases
                          (Sky_Map
                             (Sky_Bases(Get_Base_Index).Missions(I).Target_X,
                              Sky_Bases(Get_Base_Index).Missions(I).Target_Y)
                             .Base_Index)
                          .Name),
                 when PATROL =>
                   To_Unbounded_String
                     (Source =>
                        "X:" &
                        Natural'Image
                          (Sky_Bases(Get_Base_Index).Missions(I).Target_X) &
                        " Y:" &
                        Natural'Image
                          (Sky_Bases(Get_Base_Index).Missions(I).Target_Y)),
                 when DESTROY =>
                   To_Unbounded_String
                     (Source =>
                        To_String
                          (Source =>
                             Get_Proto_Ship
                               (Proto_Index =>
                                  Sky_Bases(Get_Base_Index).Missions(I)
                                    .Ship_Index)
                               .Name)),
                 when EXPLORE =>
                   To_Unbounded_String
                     (Source =>
                        "X:" &
                        Natural'Image
                          (Sky_Bases(Get_Base_Index).Missions(I).Target_X) &
                        " Y:" &
                        Natural'Image
                          (Sky_Bases(Get_Base_Index).Missions(I).Target_Y)),
                 when PASSENGER =>
                   To_Unbounded_String(Source => "To ") &
                   To_String
                     (Source =>
                        Sky_Bases
                          (Sky_Map
                             (Sky_Bases(Get_Base_Index).Missions(I).Target_X,
                              Sky_Bases(Get_Base_Index).Missions(I).Target_Y)
                             .Base_Index)
                          .Name)),
            Time => Sky_Bases(Get_Base_Index).Missions(I).Time,
            Reward => Sky_Bases(Get_Base_Index).Missions(I).Reward,
            Id => Mission_Container.To_Index(Position => I));
      end loop Fill_Local_Missions_Loop;
      Sort_Missions(Container => Local_Missions);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Missions_Indexes.Clear;
      Fill_Missions_Indexes_Loop :
      for Mission of Local_Missions loop
         Missions_Indexes.Append(New_Item => Mission.Id);
      end loop Fill_Missions_Indexes_Loop;
      Refresh_Missions_List
        (List => Sky_Bases(Get_Base_Index).Missions, Page => 1);
      Update_Table(Table => Missions_Table);
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      return TCL_OK;
   end Sort_Available_Missions_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowBaseMissions",
         Ada_Command => Show_Base_Missions_Command'Access);
      Add_Command
        (Name => "MissionMoreInfo",
         Ada_Command => Mission_More_Info_Command'Access);
      Add_Command
        (Name => "AcceptMission",
         Ada_Command => Accept_Mission_Command'Access);
      Add_Command
        (Name => "UpdateMissionReward",
         Ada_Command => Update_Mission_Reward_Command'Access);
      Add_Command
        (Name => "SortAvailableMissions",
         Ada_Command => Sort_Available_Missions_Command'Access);
   end Add_Commands;

end Missions.UI;
