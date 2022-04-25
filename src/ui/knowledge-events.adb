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

   -- ****iv* KEvents/KEvents.EventsTable
   -- FUNCTION
   -- Table with info about the known events
   -- SOURCE
   EventsTable: Table_Widget (3);
   -- ****

   -- ****if* KEvents/KEvents.Show_Events_Menu_Command
   -- FUNCTION
   -- Show the menu with available the selected event options
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowEventMenu eventindex
   -- EventIndex is the index of the event's menu to show
   -- SOURCE
   function Show_Events_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Events_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      EventIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Event_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".eventslistmenu",
           Title =>
             (case Events_List(EventIndex).E_Type is
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
             (Event_Menu & Name,
              "-text {" & Label & "} -command {CloseDialog " & Event_Menu &
              " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Button,
            "-sticky we -padx 5" &
            (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind(Button, "<Escape>", "{CloseDialog " & Event_Menu & " .;break}");
         if Command'Length = 0 then
            Bind(Button, "<Tab>", "{focus " & Event_Menu & ".show;break}");
            Focus(Button);
         end if;
      end Add_Button;
   begin
      Add_Button
        (".show", "Show the event on map",
         "ShowOnMap" & Map_X_Range'Image(Events_List(EventIndex).Sky_X) &
         Map_Y_Range'Image(Events_List(EventIndex).Sky_Y));
      Add_Button
        (".destination", "Set the event as destination for the ship",
         "SetDestination2" & Map_X_Range'Image(Events_List(EventIndex).Sky_X) &
         Map_Y_Range'Image(Events_List(EventIndex).Sky_Y));
      Add_Button
        (".info", "Show more information about the event",
         "ShowEventInfo " & CArgv.Arg(Argv, 1));
      Add_Button(".close", "Close", "");
      Show_Dialog(Dialog => Event_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Events_Menu_Command;

   -- ****o* KEvents/KEvents.Show_Event_Info_Command
   -- FUNCTION
   -- Show information about the selected event
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowEventInfo eventindex
   -- EventIndex is the index of the event to show
   -- SOURCE
   function Show_Event_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Event_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      EventIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      EventInfo: Unbounded_String;
      BaseIndex: constant Extended_Base_Range :=
        Sky_Map(Events_List(EventIndex).Sky_X, Events_List(EventIndex).Sky_Y)
          .Base_Index;
   begin
      EventInfo :=
        To_Unbounded_String
          ("X:" & Positive'Image(Events_List(EventIndex).Sky_X) & " Y:" &
           Positive'Image(Events_List(EventIndex).Sky_Y));
      case Events_List(EventIndex).E_Type is
         when ENEMYSHIP | ENEMYPATROL | TRADER | FRIENDLYSHIP =>
            Append
              (EventInfo,
               LF & "Ship type: " &
               To_String
                 (Proto_Ships_List
                    (To_Bounded_String
                       (Source =>
                          To_String
                            (Source => Events_List(EventIndex).Ship_Index)))
                    .Name));
         when FULLDOCKS | ATTACKONBASE | DISEASE =>
            Append
              (EventInfo,
               LF & "Base name: " & To_String(Sky_Bases(BaseIndex).Name));
         when DOUBLEPRICE =>
            Append
              (EventInfo,
               LF & "Base name: " & To_String(Sky_Bases(BaseIndex).Name));
            Append
              (EventInfo,
               LF & "Item: " &
               To_String(Items_List(Events_List(EventIndex).Item_Index).Name));
         when NONE | BASERECOVERY =>
            null;
      end case;
      Show_Info(Text => To_String(EventInfo), Title => "Event information");
      return TCL_OK;
   end Show_Event_Info_Command;

   -- ****o* KEvents/KEvents.Show_Events_Command
   -- FUNCTION
   -- Show the list of known events to the player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowEvents ?startindex?
   -- Page parameter is a page number which will be show
   -- SOURCE
   function Show_Events_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Events_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
   begin
      if Argc = 2 then
         UpdateEventsList(Positive'Value(CArgv.Arg(Argv, 1)));
      else
         UpdateEventsList;
      end if;
      Tcl_SetResult(Interp, "1");
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
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortKnownEvents x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Events_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Events_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number(EventsTable, Natural'Value(CArgv.Arg(Argv, 1)));
      type Local_Event_Data is record
         EType: Events_Types;
         Distance: Natural;
         Details: Unbounded_String;
         Id: Positive;
      end record;
      type Events_Array is array(Positive range <>) of Local_Event_Data;
      Local_Events: Events_Array(1 .. Positive(Events_List.Length));
      function "<"(Left, Right: Local_Event_Data) return Boolean is
      begin
         if Events_Sort_Order = TYPEASC and then Left.EType < Right.EType then
            return True;
         end if;
         if Events_Sort_Order = TYPEDESC and then Left.EType > Right.EType then
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
      for I in Events_List.Iterate loop
         Local_Events(Events_Container.To_Index(I)) :=
           (EType => Events_List(I).E_Type,
            Distance =>
              Count_Distance(Events_List(I).Sky_X, Events_List(I).Sky_Y),
            Details =>
              (case Events_List(I).E_Type is
                 when DOUBLEPRICE =>
                   Items_List(Events_List(I).Item_Index).Name & " in " &
                   Tiny_String.To_String
                     (Source =>
                        Sky_Bases
                          (Sky_Map(Events_List(I).Sky_X, Events_List(I).Sky_Y)
                             .Base_Index)
                          .Name),
                 when ATTACKONBASE | DISEASE | FULLDOCKS | ENEMYPATROL =>
                   To_Unbounded_String
                     (Source =>
                        Tiny_String.To_String
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
                             Proto_Ships_List
                               (To_Bounded_String
                                  (Source =>
                                     To_String
                                       (Source => Events_List(I).Ship_Index)))
                               .Name)),
                 when NONE | BASERECOVERY => Null_Unbounded_String),
            Id => Events_Container.To_Index(I));
      end loop;
      Sort_Events(Local_Events);
      Events_Indexes.Clear;
      for Event of Local_Events loop
         Events_Indexes.Append(Event.Id);
      end loop;
      UpdateEventsList;
      return TCL_OK;
   end Sort_Events_Command;

   procedure AddCommands is
   begin
      Add_Command("ShowEventMenu", Show_Events_Menu_Command'Access);
      Add_Command("ShowEventInfo", Show_Event_Info_Command'Access);
      Add_Command("ShowEvents", Show_Events_Command'Access);
      Add_Command("SortKnownEvents", Sort_Events_Command'Access);
   end AddCommands;

   procedure UpdateEventsList(Page: Positive := 1) is
      use Tiny_String;

      EventsCanvas: constant Tk_Canvas :=
        Get_Widget(Main_Paned & ".knowledgeframe.events.canvas");
      EventsFrame: constant Ttk_Frame := Get_Widget(EventsCanvas & ".frame");
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Label: Ttk_Label;
      Row: Positive;
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(EventsFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      if EventsTable.Row > 1 then
         Clear_Table(EventsTable);
      end if;
      Delete_Widgets(1, Rows - 1, EventsFrame);
      if Events_List.Length = 0 then
         Label :=
           Create
             (EventsFrame & ".noevents",
              "-text {You don't know any event yet. You may ask for events in bases. When your ship is docked to base, select Ask for Events from ship orders menu.} -wraplength 350");
         Tcl.Tk.Ada.Grid.Grid(Label, "-padx 10");
         Bind
           (EventsCanvas, "<Configure>",
            "{" & Label & " configure -wraplength [expr [winfo width " &
            EventsCanvas & "] - 10]}");
      else
         Unbind(EventsCanvas, "<Configure>");
         Row := 2;
         EventsTable :=
           Create_Table
             (Widget_Image(EventsFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Distance"),
               To_Unbounded_String("Details")),
              Get_Widget(Main_Paned & ".knowledgeframe.events.scrolly"),
              "SortKnownEvents", "Press mouse button to sort the events.");
         if Events_Indexes.Length /= Events_List.Length then
            Events_Indexes.Clear;
            for I in Events_List.Iterate loop
               Events_Indexes.Append(Events_Container.To_Index(I));
            end loop;
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
                    (EventsTable, "Enemy ship spotted",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when FULLDOCKS =>
                  Add_Button
                    (EventsTable, "Full docks in base",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when ATTACKONBASE =>
                  Add_Button
                    (EventsTable, "Base is under attack",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when DISEASE =>
                  Add_Button
                    (EventsTable, "Disease in base",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when ENEMYPATROL =>
                  Add_Button
                    (EventsTable, "Enemy patrol",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when DOUBLEPRICE =>
                  Add_Button
                    (EventsTable, "Double price in base",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when TRADER =>
                  Add_Button
                    (EventsTable, "Friendly trader spotted",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when FRIENDLYSHIP =>
                  Add_Button
                    (EventsTable, "Friendly ship spotted",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when NONE | BASERECOVERY =>
                  null;
            end case;
            Add_Button
              (EventsTable,
               Natural'Image
                 (Count_Distance
                    (Events_List(Event).Sky_X, Events_List(Event).Sky_Y)),
               "The distance to the event",
               "ShowEventMenu" & Positive'Image(Event), 2);
            case Events_List(Event).E_Type is
               when DOUBLEPRICE =>
                  Add_Button
                    (EventsTable,
                     To_String
                       (Items_List(Events_List(Event).Item_Index).Name) &
                     " in " &
                     To_String
                       (Sky_Bases
                          (Sky_Map
                             (Events_List(Event).Sky_X,
                              Events_List(Event).Sky_Y)
                             .Base_Index)
                          .Name),
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Event), 3, True);
               when ATTACKONBASE | DISEASE | FULLDOCKS | ENEMYPATROL =>
                  Add_Button
                    (EventsTable,
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
                    (EventsTable,
                     To_String
                       (Proto_Ships_List
                          (To_Bounded_String
                             (Source =>
                                To_String
                                  (Source => Events_List(Event).Ship_Index)))
                          .Name),
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Event), 3, True);
               when NONE | BASERECOVERY =>
                  null;
            end case;
            Row := Row + 1;
            exit Load_Known_Events_Loop when EventsTable.Row =
              Game_Settings.Lists_Limit + 1;
            <<End_Of_Loop>>
         end loop Load_Known_Events_Loop;
         if Page > 1 then
            if EventsTable.Row < Game_Settings.Lists_Limit + 1 then
               Add_Pagination
                 (EventsTable, "ShowEvents" & Positive'Image(Page - 1), "");
            else
               Add_Pagination
                 (EventsTable, "ShowEvents" & Positive'Image(Page - 1),
                  "ShowEvents" & Positive'Image(Page + 1));
            end if;
         elsif EventsTable.Row > Game_Settings.Lists_Limit then
            Add_Pagination
              (EventsTable, "", "ShowEvents" & Positive'Image(Page + 1));
         end if;
         Update_Table(EventsTable);
      end if;
      Tcl_Eval(Get_Context, "update");
      configure
        (EventsCanvas,
         "-scrollregion [list " & BBox(EventsCanvas, "all") & "]");
      Xview_Move_To(EventsCanvas, "0.0");
      Yview_Move_To(EventsCanvas, "0.0");
   end UpdateEventsList;

end Knowledge.Events;
