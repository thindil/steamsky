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
with Ada.Containers; use Ada.Containers;
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
with Ships; use Ships;
with Table; use Table;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Knowledge.Events is

   -- ****iv* KEvents/KEvents.Events_Table
   -- FUNCTION
   -- Table with info about the known events
   -- SOURCE
   Events_Table: Table_Widget (Amount => 3);
   -- ****

   -- ****if* KEvents/KEvents.Show_Events_Menu_Command
   -- FUNCTION
   -- Show the menu with available the selected event options
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowEventMenu eventindex
   -- EventIndex is the index of the event's menu to show
   -- SOURCE
   function Show_Events_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Events_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Event_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Event_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".eventslistmenu",
           Title =>
             (case Events_List(Event_Index).E_Type is
                when ENEMYSHIP | ENEMYPATROL => "Enemy",
                when ATTACKONBASE => "Defend", when DISEASE => "Disease",
                when DOUBLEPRICE => "Price", when FULLDOCKS => "Full docks",
                when TRADER | FRIENDLYSHIP => "Friendly ship",
                when others => "") &
             " event actions",
           Parent_Name => ".");
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Event_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Event_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Event_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Event_Menu & ".show;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Add_Button
        (Name => ".show", Label => "Show the event on map",
         Command =>
           "ShowOnMap" & Map_X_Range'Image(Events_List(Event_Index).Sky_X) &
           Map_Y_Range'Image(Events_List(Event_Index).Sky_Y));
      Add_Button
        (Name => ".destination",
         Label => "Set the event as destination for the ship",
         Command =>
           "SetDestination2" &
           Map_X_Range'Image(Events_List(Event_Index).Sky_X) &
           Map_Y_Range'Image(Events_List(Event_Index).Sky_Y));
      Add_Button
        (Name => ".info", Label => "Show more information about the event",
         Command => "ShowEventInfo " & CArgv.Arg(Argv => Argv, N => 1));
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Event_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Events_Menu_Command;

   -- ****o* KEvents/KEvents.Show_Event_Info_Command
   -- FUNCTION
   -- Show information about the selected event
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowEventInfo eventindex
   -- EventIndex is the index of the event to show
   -- SOURCE
   function Show_Event_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Event_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Event_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Event_Info: Unbounded_String;
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Events_List(Event_Index).Sky_X, Events_List(Event_Index).Sky_Y)
          .Base_Index;
   begin
      Event_Info :=
        To_Unbounded_String
          (Source =>
             "X:" & Positive'Image(Events_List(Event_Index).Sky_X) & " Y:" &
             Positive'Image(Events_List(Event_Index).Sky_Y));
      case Events_List(Event_Index).E_Type is
         when ENEMYSHIP | ENEMYPATROL | TRADER | FRIENDLYSHIP =>
            Append
              (Source => Event_Info,
               New_Item =>
                 LF & "Ship type: " &
                 To_String
                   (Source =>
                      Proto_Ships_List(Events_List(Event_Index).Ship_Index)
                        .Name));
         when FULLDOCKS | ATTACKONBASE | DISEASE =>
            Append
              (Source => Event_Info,
               New_Item =>
                 LF & "Base name: " &
                 To_String(Source => Sky_Bases(Base_Index).Name));
         when DOUBLEPRICE =>
            Append
              (Source => Event_Info,
               New_Item =>
                 LF & "Base name: " &
                 To_String(Source => Sky_Bases(Base_Index).Name));
            Append
              (Source => Event_Info,
               New_Item =>
                 LF & "Item: " &
                 To_String
                   (Source =>
                      Objects_Container.Element
                        (Container => Items_List,
                         Index => Events_List(Event_Index).Item_Index)
                        .Name));
         when NONE | BASERECOVERY =>
            null;
      end case;
      Show_Info
        (Text => To_String(Source => Event_Info),
         Title => "Event information");
      return TCL_OK;
   end Show_Event_Info_Command;

   -- ****o* KEvents/KEvents.Show_Events_Command
   -- FUNCTION
   -- Show the list of known events to the player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowEvents ?startindex?
   -- Page parameter is a page number which will be show
   -- SOURCE
   function Show_Events_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Events_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
   begin
      if Argc = 2 then
         Update_Events_List
           (Page => Positive'Value(CArgv.Arg(Argv => Argv, N => 1)));
      else
         Update_Events_List;
      end if;
      Tcl_SetResult(interp => Interp, str => "1");
      return TCL_OK;
   end Show_Events_Command;

   -- ****it* KEvents/KEvents.Events_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the known events list
   -- OPTIONS
   -- TYPEASC      - Sort events by type ascending
   -- TYPEDESC     - Sort events by type descending
   -- DISTANCEASC  - Sort events by distance ascending
   -- DISTANCEDESC - Sort events by distance descending
   -- DETAILSASC   - Sort events by details ascending
   -- DETAILSDESC  - Sort events by details descending
   -- NONE         - No sorting events (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Events_Sort_Orders is
     (TYPEASC, TYPEDESC, DISTANCEASC, DISTANCEDESC, DETAILSASC, DETAILSDESC,
      NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* KEvents/KEvents.Default_Events_Sort_Order
      -- FUNCTION
      -- Default sorting order for the known events
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Events_Sort_Order: constant Events_Sort_Orders := NONE;
   -- ****

   -- ****iv* KEvents/KEvents.Events_Sort_Order
   -- FUNCTION
   -- The current sorting order for known events list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Events_Sort_Order: Events_Sort_Orders := Default_Events_Sort_Order;
   -- ****

   -- ****iv* KEvents/KEvents.Events_Indexes
   -- FUNCTION
   -- Indexes of the known events
   -- SOURCE
   Events_Indexes: Positive_Container.Vector;
   -- ****

   -- ****o* KEvents/KEvents.Sort_Events_Command
   -- FUNCTION
   -- Sort the known events list
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortKnownEvents x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Events_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Events_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number
          (Table => Events_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
      type Local_Event_Data is record
         E_Type: Events_Types;
         Distance: Natural;
         Details: Unbounded_String;
         Id: Positive;
      end record;
      type Events_Array is array(Positive range <>) of Local_Event_Data;
      Local_Events: Events_Array(1 .. Positive(Events_List.Length));
      function "<"(Left, Right: Local_Event_Data) return Boolean is
      begin
         if Events_Sort_Order = TYPEASC
           and then Left.E_Type < Right.E_Type then
            return True;
         end if;
         if Events_Sort_Order = TYPEDESC
           and then Left.E_Type > Right.E_Type then
            return True;
         end if;
         if Events_Sort_Order = DISTANCEASC
           and then Left.Distance < Right.Distance then
            return True;
         end if;
         if Events_Sort_Order = DISTANCEDESC
           and then Left.Distance > Right.Distance then
            return True;
         end if;
         if Events_Sort_Order = DETAILSASC
           and then Left.Details < Right.Details then
            return True;
         end if;
         if Events_Sort_Order = DETAILSDESC
           and then Left.Details > Right.Details then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Events is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Event_Data,
         Array_Type => Events_Array);
   begin
      case Column is
         when 1 =>
            if Events_Sort_Order = TYPEASC then
               Events_Sort_Order := TYPEDESC;
            else
               Events_Sort_Order := TYPEASC;
            end if;
         when 2 =>
            if Events_Sort_Order = DISTANCEASC then
               Events_Sort_Order := DISTANCEDESC;
            else
               Events_Sort_Order := DISTANCEASC;
            end if;
         when 3 =>
            if Events_Sort_Order = DETAILSASC then
               Events_Sort_Order := DETAILSDESC;
            else
               Events_Sort_Order := DETAILSASC;
            end if;
         when others =>
            null;
      end case;
      if Events_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Events_Loop :
      for I in Events_List.Iterate loop
         Local_Events(Events_Container.To_Index(Position => I)) :=
           (E_Type => Events_List(I).E_Type,
            Distance =>
              Count_Distance
                (Destination_X => Events_List(I).Sky_X,
                 Destination_Y => Events_List(I).Sky_Y),
            Details =>
              (case Events_List(I).E_Type is
                 when DOUBLEPRICE =>
                   To_String
                     (Source =>
                        Objects_Container.Element
                          (Container => Items_List,
                           Index => Events_List(I).Item_Index)
                          .Name) &
                   To_Unbounded_String(Source => " in ") &
                   To_String
                     (Source =>
                        Sky_Bases
                          (Sky_Map(Events_List(I).Sky_X, Events_List(I).Sky_Y)
                             .Base_Index)
                          .Name),
                 when ATTACKONBASE | DISEASE | FULLDOCKS | ENEMYPATROL =>
                   To_Unbounded_String
                     (Source =>
                        To_String
                          (Source =>
                             Sky_Bases
                               (Sky_Map
                                  (Events_List(I).Sky_X, Events_List(I).Sky_Y)
                                  .Base_Index)
                               .Name)),
                 when ENEMYSHIP | TRADER | FRIENDLYSHIP =>
                   To_Unbounded_String
                     (Source =>
                        To_String
                          (Source =>
                             Proto_Ships_List(Events_List(I).Ship_Index)
                               .Name)),
                 when NONE | BASERECOVERY => Null_Unbounded_String),
            Id => Events_Container.To_Index(Position => I));
      end loop Fill_Local_Events_Loop;
      Sort_Events(Container => Local_Events);
      Events_Indexes.Clear;
      Fill_Events_Indexes_Loop :
      for Event of Local_Events loop
         Events_Indexes.Append(New_Item => Event.Id);
      end loop Fill_Events_Indexes_Loop;
      Update_Events_List;
      return TCL_OK;
   end Sort_Events_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowEventMenu",
         Ada_Command => Show_Events_Menu_Command'Access);
      Add_Command
        (Name => "ShowEventInfo",
         Ada_Command => Show_Event_Info_Command'Access);
      Add_Command
        (Name => "ShowEvents", Ada_Command => Show_Events_Command'Access);
      Add_Command
        (Name => "SortKnownEvents", Ada_Command => Sort_Events_Command'Access);
   end Add_Commands;

   procedure Update_Events_List(Page: Positive := 1) is
      use Tiny_String;

      Events_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Main_Paned & ".knowledgeframe.events.canvas");
      Events_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Events_Canvas & ".frame");
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Label: Ttk_Label;
      Row: Positive;
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
   begin
      Create
        (S => Tokens,
         From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Events_Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      if Events_Table.Row > 1 then
         Clear_Table(Table => Events_Table);
      end if;
      Delete_Widgets
        (Start_Index => 1, End_Index => Rows - 1, Frame => Events_Frame);
      if Events_List.Length = 0 then
         Label :=
           Create
             (pathName => Events_Frame & ".noevents",
              options =>
                "-text {You don't know any event yet. You may ask for events in bases. When your ship is docked to base, select Ask for Events from ship orders menu.} -wraplength 350");
         Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-padx 10");
         Bind
           (Widgt => Events_Canvas, Sequence => "<Configure>",
            Script =>
              "{" & Label & " configure -wraplength [expr [winfo width " &
              Events_Canvas & "] - 10]}");
      else
         Unbind(Widgt => Events_Canvas, Sequence => "<Configure>");
         Row := 2;
         Events_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Events_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Distance"),
                 3 => To_Unbounded_String(Source => "Details")),
              Scrollbar =>
                Get_Widget(Main_Paned & ".knowledgeframe.events.scrolly"),
              Command => "SortKnownEvents",
              Tooltip => "Press mouse button to sort the events.");
         if Events_Indexes.Length /= Events_List.Length then
            Events_Indexes.Clear;
            Fill_Event_Indexes_Loop :
            for I in Events_List.Iterate loop
               Events_Indexes.Append
                 (New_Item => Events_Container.To_Index(Position => I));
            end loop Fill_Event_Indexes_Loop;
         end if;
         Load_Known_Events_Loop :
         for Event of Events_Indexes loop
            if Current_Row < Start_Row then
               Current_Row := Current_Row + 1;
               goto End_Of_Loop;
            end if;
            case Events_List(Event).E_Type is
               when ENEMYSHIP =>
                  Add_Button
                    (Table => Events_Table, Text => "Enemy ship spotted",
                     Tooltip => "Show available event's options",
                     Command => "ShowEventMenu" & Positive'Image(Row - 1),
                     Column => 1);
               when FULLDOCKS =>
                  Add_Button
                    (Table => Events_Table, Text => "Full docks in base",
                     Tooltip => "Show available event's options",
                     Command => "ShowEventMenu" & Positive'Image(Row - 1),
                     Column => 1);
               when ATTACKONBASE =>
                  Add_Button
                    (Table => Events_Table, Text => "Base is under attack",
                     Tooltip => "Show available event's options",
                     Command => "ShowEventMenu" & Positive'Image(Row - 1),
                     Column => 1);
               when DISEASE =>
                  Add_Button
                    (Table => Events_Table, Text => "Disease in base",
                     Tooltip => "Show available event's options",
                     Command => "ShowEventMenu" & Positive'Image(Row - 1),
                     Column => 1);
               when ENEMYPATROL =>
                  Add_Button
                    (Table => Events_Table, Text => "Enemy patrol",
                     Tooltip => "Show available event's options",
                     Command => "ShowEventMenu" & Positive'Image(Row - 1),
                     Column => 1);
               when DOUBLEPRICE =>
                  Add_Button
                    (Table => Events_Table, Text => "Double price in base",
                     Tooltip => "Show available event's options",
                     Command => "ShowEventMenu" & Positive'Image(Row - 1),
                     Column => 1);
               when TRADER =>
                  Add_Button
                    (Table => Events_Table, Text => "Friendly trader spotted",
                     Tooltip => "Show available event's options",
                     Command => "ShowEventMenu" & Positive'Image(Row - 1),
                     Column => 1);
               when FRIENDLYSHIP =>
                  Add_Button
                    (Table => Events_Table, Text => "Friendly ship spotted",
                     Tooltip => "Show available event's options",
                     Command => "ShowEventMenu" & Positive'Image(Row - 1),
                     Column => 1);
               when NONE | BASERECOVERY =>
                  null;
            end case;
            Add_Button
              (Table => Events_Table,
               Text =>
                 Natural'Image
                   (Count_Distance
                      (Destination_X => Events_List(Event).Sky_X,
                       Destination_Y => Events_List(Event).Sky_Y)),
               Tooltip => "The distance to the event",
               Command => "ShowEventMenu" & Positive'Image(Event),
               Column => 2);
            case Events_List(Event).E_Type is
               when DOUBLEPRICE =>
                  Add_Button
                    (Table => Events_Table,
                     Text =>
                       To_String
                         (Source =>
                            Objects_Container.Element
                              (Container => Items_List,
                               Index => Events_List(Event).Item_Index)
                              .Name) &
                       " in " &
                       To_String
                         (Source =>
                            Sky_Bases
                              (Sky_Map
                                 (Events_List(Event).Sky_X,
                                  Events_List(Event).Sky_Y)
                                 .Base_Index)
                              .Name),
                     Tooltip => "Show available event's options",
                     Command => "ShowEventMenu" & Positive'Image(Event),
                     Column => 3, New_Row => True);
               when ATTACKONBASE | DISEASE | FULLDOCKS | ENEMYPATROL =>
                  Add_Button
                    (Events_Table,
                     To_String
                       (Sky_Bases
                          (Sky_Map
                             (Events_List(Event).Sky_X,
                              Events_List(Event).Sky_Y)
                             .Base_Index)
                          .Name),
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Event), 3, True);
               when ENEMYSHIP | TRADER | FRIENDLYSHIP =>
                  Add_Button
                    (Events_Table,
                     To_String
                       (Proto_Ships_List(Events_List(Event).Ship_Index).Name),
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Event), 3, True);
               when NONE | BASERECOVERY =>
                  null;
            end case;
            Row := Row + 1;
            exit Load_Known_Events_Loop when Events_Table.Row =
              Game_Settings.Lists_Limit + 1;
            <<End_Of_Loop>>
         end loop Load_Known_Events_Loop;
         if Page > 1 then
            if Events_Table.Row < Game_Settings.Lists_Limit + 1 then
               Add_Pagination
                 (Events_Table, "ShowEvents" & Positive'Image(Page - 1), "");
            else
               Add_Pagination
                 (Events_Table, "ShowEvents" & Positive'Image(Page - 1),
                  "ShowEvents" & Positive'Image(Page + 1));
            end if;
         elsif Events_Table.Row > Game_Settings.Lists_Limit then
            Add_Pagination
              (Events_Table, "", "ShowEvents" & Positive'Image(Page + 1));
         end if;
         Update_Table(Events_Table);
      end if;
      Tcl_Eval(Get_Context, "update");
      configure
        (Events_Canvas,
         "-scrollregion [list " & BBox(Events_Canvas, "all") & "]");
      Xview_Move_To(Events_Canvas, "0.0");
      Yview_Move_To(Events_Canvas, "0.0");
   end Update_Events_List;

end Knowledge.Events;
