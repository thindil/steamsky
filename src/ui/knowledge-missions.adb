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

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Generic_Array_Sort;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl; use Tcl;
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
      Convention => C;
      -- ****

   function Show_Missions_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Mission_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Mission_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".missionslistmenu",
           Title =>
             (case Accepted_Missions(Mission_Index).M_Type is
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
   begin
      Add_Button
        (Name => ".show", Label => "Show the mission on map",
         Command =>
           "ShowOnMap" &
           Map_X_Range'Image(Accepted_Missions(Mission_Index).Target_X) &
           Map_Y_Range'Image(Accepted_Missions(Mission_Index).Target_Y));
      Add_Button
        (Name => ".destination",
         Label => "Set the mission as destination for the ship",
         Command =>
           "SetDestination2 " &
           Map_X_Range'Image(Accepted_Missions(Mission_Index).Target_X) &
           Map_Y_Range'Image(Accepted_Missions(Mission_Index).Target_Y));
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Mission_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Missions_Menu_Command;

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
   Missions_Table: Table_Widget (Amount => 5);
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
      type Local_Mission_Data is record
         M_Type: Missions_Types;
         Distance: Natural;
         Details: Unbounded_String;
         Time: Natural;
         Reward: Natural;
         Id: Positive;
      end record;
      type Missions_Array is array(Positive range <>) of Local_Mission_Data;
      Local_Missions: Missions_Array(1 .. Positive(Accepted_Missions.Length));
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
      Fill_Local_Missions_Loop :
      for I in Accepted_Missions.Iterate loop
         Local_Missions(Mission_Container.To_Index(Position => I)) :=
           (M_Type => Accepted_Missions(I).M_Type,
            Distance =>
              Count_Distance
                (Destination_X => Accepted_Missions(I).Target_X,
                 Destination_Y => Accepted_Missions(I).Target_Y),
            Details =>
              (case Accepted_Missions(I).M_Type is
                 when DELIVER =>
                   To_String
                     (Source =>
                        Objects_Container.Element
                          (Container => Items_List,
                           Index => Accepted_Missions(I).Item_Index)
                          .Name) &
                   To_Unbounded_String(Source => " to ") &
                   To_String
                     (Source =>
                        Sky_Bases
                          (Sky_Map
                             (Accepted_Missions(I).Target_X,
                              Accepted_Missions(I).Target_Y)
                             .Base_Index)
                          .Name),
                 when PATROL =>
                   To_Unbounded_String
                     (Source =>
                        "X:" & Natural'Image(Accepted_Missions(I).Target_X) &
                        " Y:" & Natural'Image(Accepted_Missions(I).Target_Y)),
                 when DESTROY =>
                   To_Unbounded_String
                     (Source =>
                        (To_String
                           (Source =>
                              Proto_Ships_List(Accepted_Missions(I).Ship_Index)
                                .Name))),
                 when EXPLORE =>
                   To_Unbounded_String
                     (Source =>
                        "X:" & Natural'Image(Accepted_Missions(I).Target_X) &
                        " Y:" & Natural'Image(Accepted_Missions(I).Target_Y)),
                 when PASSENGER =>
                   To_Unbounded_String(Source => "To ") &
                   Tiny_String.To_String
                     (Source =>
                        Sky_Bases
                          (Sky_Map
                             (Accepted_Missions(I).Target_X,
                              Accepted_Missions(I).Target_Y)
                             .Base_Index)
                          .Name)),
            Time => Accepted_Missions(I).Time,
            Reward => Accepted_Missions(I).Reward,
            Id => Mission_Container.To_Index(Position => I));
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

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowMissionMenu",
         Ada_Command => Show_Missions_Menu_Command'Access);
      Add_Command
        (Name => "ShowMissions", Ada_Command => Show_Missions_Command'Access);
      Add_Command
        (Name => "SortAccepted_Missions",
         Ada_Command => Sort_Missions_Command'Access);
   end Add_Commands;

   procedure Update_Missions_List(Page: Positive := 1) is
      use Tiny_String;

      Missions_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Main_Paned & ".knowledgeframe.missions.canvas");
      Missions_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Missions_Canvas & ".frame");
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Label: Ttk_Label;
      Row: Positive;
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      Mission_Time: Unbounded_String;
   begin
      Create
        (S => Tokens,
         From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Missions_Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      if Missions_Table.Row > 1 then
         Clear_Table(Table => Missions_Table);
      end if;
      Delete_Widgets
        (Start_Index => 1, End_Index => Rows - 1, Frame => Missions_Frame);
      if Accepted_Missions.Length = 0 then
         Label :=
           Create
             (pathName => Missions_Frame & ".nomissions",
              options =>
                "-text {You didn't accept any mission yet. You may ask for missions in bases. When your ship is docked to base, check Missions from ship orders menu.} -wraplength 350");
         Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-padx 10");
         Bind
           (Widgt => Missions_Canvas, Sequence => "<Configure>",
            Script =>
              "{" & Label & " configure -wraplength [expr [winfo width " &
              Missions_Canvas & "] - 15]}");
      else
         Unbind(Widgt => Missions_Canvas, Sequence => "<Configure>");
         Row := 2;
         Missions_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Missions_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Distance"),
                 3 => To_Unbounded_String(Source => "Details"),
                 4 => To_Unbounded_String(Source => "Time limit"),
                 5 => To_Unbounded_String(Source => "Base reward")),
              Scrollbar =>
                Get_Widget(".gameframe.paned.knowledgeframe.missions.scrolly"),
              Command => "SortAccepted_Missions",
              Tooltip => "Press mouse button to sort the missions.");
         if Missions_Indexes.Length /= Accepted_Missions.Length then
            Missions_Indexes.Clear;
            Fill_Missions_Indexes_Loop :
            for I in Accepted_Missions.Iterate loop
               Missions_Indexes.Append
                 (New_Item => Mission_Container.To_Index(Position => I));
            end loop Fill_Missions_Indexes_Loop;
         end if;
         Rows := 0;
         Load_Accepted_Missions_Loop :
         for I of Missions_Indexes loop
            if Current_Row < Start_Row then
               Current_Row := Current_Row + 1;
               goto End_Of_Loop;
            end if;
            Add_Button
              (Missions_Table, Get_Mission_Type(Accepted_Missions(I).M_Type),
               "Show available mission's options",
               "ShowMissionMenu" & Positive'Image(Row - 1), 1);
            case Accepted_Missions(I).M_Type is
               when DELIVER =>
                  Add_Button
                    (Missions_Table,
                     To_String
                       (Objects_Container.Element
                          (Container => Items_List,
                           Index => Accepted_Missions(I).Item_Index)
                          .Name) &
                     " to " &
                     Tiny_String.To_String
                       (Sky_Bases
                          (Sky_Map
                             (Accepted_Missions(I).Target_X,
                              Accepted_Missions(I).Target_Y)
                             .Base_Index)
                          .Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when PATROL =>
                  Add_Button
                    (Missions_Table,
                     "X:" & Natural'Image(Accepted_Missions(I).Target_X) &
                     " Y:" & Natural'Image(Accepted_Missions(I).Target_Y),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when DESTROY =>
                  Add_Button
                    (Missions_Table,
                     To_String
                       (Proto_Ships_List(Accepted_Missions(I).Ship_Index)
                          .Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when EXPLORE =>
                  Add_Button
                    (Missions_Table,
                     "X:" & Natural'Image(Accepted_Missions(I).Target_X) &
                     " Y:" & Natural'Image(Accepted_Missions(I).Target_Y),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when PASSENGER =>
                  Add_Button
                    (Missions_Table,
                     "To " &
                     Tiny_String.To_String
                       (Sky_Bases
                          (Sky_Map
                             (Accepted_Missions(I).Target_X,
                              Accepted_Missions(I).Target_Y)
                             .Base_Index)
                          .Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
            end case;
            Add_Button
              (Missions_Table,
               Natural'Image
                 (Count_Distance
                    (Accepted_Missions(I).Target_X,
                     Accepted_Missions(I).Target_Y)),
               "The distance to the mission",
               "ShowMissionMenu" & Positive'Image(Row - 1), 2);
            Mission_Time := Null_Unbounded_String;
            Minutes_To_Date(Accepted_Missions(I).Time, Mission_Time);
            Add_Button
              (Missions_Table, To_String(Mission_Time),
               "The time limit for finish and return the mission",
               "ShowMissionMenu" & Positive'Image(Row - 1), 4);
            Add_Button
              (Missions_Table,
               Natural'Image
                 (Natural
                    (Float(Accepted_Missions(I).Reward) *
                     Float(Accepted_Missions(I).Multiplier))) &
               " " & To_String(Money_Name),
               "The base money reward for the mission",
               "ShowMissionMenu" & Positive'Image(Row - 1), 5, True);
            Row := Row + 1;
            Rows := Rows + 1;
            exit Load_Accepted_Missions_Loop when Rows =
              Game_Settings.Lists_Limit and
              I /= Accepted_Missions.Last_Index;
            <<End_Of_Loop>>
         end loop Load_Accepted_Missions_Loop;
         if Page > 1 then
            if Rows < Game_Settings.Lists_Limit then
               Add_Pagination
                 (Missions_Table, "ShowMissions" & Positive'Image(Page - 1),
                  "");
            else
               Add_Pagination
                 (Missions_Table, "ShowMissions" & Positive'Image(Page - 1),
                  "ShowMissions" & Positive'Image(Page + 1));
            end if;
         elsif Rows > Game_Settings.Lists_Limit - 1 then
            Add_Pagination
              (Missions_Table, "", "ShowMissions" & Positive'Image(Page + 1));
         end if;
         Update_Table(Missions_Table);
      end if;
      Tcl_Eval(Get_Context, "update");
      configure
        (Missions_Canvas,
         "-scrollregion [list " & BBox(Missions_Canvas, "all") & "]");
      Xview_Move_To(Missions_Canvas, "0.0");
      Yview_Move_To(Missions_Canvas, "0.0");
   end Update_Missions_List;

end Knowledge.Missions;
