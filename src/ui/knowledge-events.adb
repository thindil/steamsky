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

with Ada.Containers.Generic_Array_Sort;
with Ada.Containers;
with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Bases; use Bases;
with CoreUI;
with Events; use Events;
with Game; use Game;
with Items; use Items;
with Maps; use Maps;
with Ships; use Ships;
with Table; use Table;
with Utils;
with Utils.UI; use Utils.UI;

package body Knowledge.Events is

   -- ****iv* KEvents/KEvents.Events_Table
   -- FUNCTION
   -- Table with info about the known events
   -- SOURCE
   Events_Table: Table_Widget (Amount => 4);
   -- ****

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
      Convention => C,
      Import => True,
      External_Name => "showEventInfoCommand";
      -- ****

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
   -- COORDASC     - Sort events by coordinates ascending
   -- COORDDESC    - Sort events by coordinates descending
   -- NONE         - No sorting events (default)
   -- HISTORY
   -- 6.4 - Added
   -- 8.4 - Added sorting by coordinates
   -- SOURCE
   type Events_Sort_Orders is
     (TYPEASC, TYPEDESC, DISTANCEASC, DISTANCEDESC, DETAILSASC, DETAILSDESC,
      COORDASC, COORDDESC, NONE) with
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

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* KEvents/KEvents.Events_Sort_Order
   -- FUNCTION
   -- The current sorting order for known events list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Events_Sort_Order: Events_Sort_Orders := Default_Events_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

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
      --## rule off TYPE_INITIAL_VALUES
      type Local_Event_Data is record
         E_Type: Events_Types;
         Distance: Natural;
         Coords: Unbounded_String;
         Details: Unbounded_String;
         Id: Positive;
      end record;
      type Events_Array is array(Positive range <>) of Local_Event_Data;
      --## rule on TYPE_INITIAL_VALUES
      --## rule off IMPROPER_INITIALIZATION
      Local_Events: Events_Array(1 .. Get_Events_Amount);
      --## rule on IMPROPER_INITIALIZATION
      --## rule off DIRECTLY_ACCESSED_GLOBALS
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
         if Events_Sort_Order = COORDASC
           and then Left.Coords < Right.Coords then
            return True;
         end if;
         if Events_Sort_Order = COORDDESC
           and then Left.Coords > Right.Coords then
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
            if Events_Sort_Order = COORDASC then
               Events_Sort_Order := COORDDESC;
            else
               Events_Sort_Order := COORDASC;
            end if;
         when 4 =>
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
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Fill_Local_Events_Loop :
      for I in 1 .. Get_Events_Amount loop
         Local_Events(I) :=
           (E_Type => Get_Event(Index => I).E_Type,
            Distance =>
              Count_Distance
                (Destination_X => Get_Event(Index => I).Sky_X,
                 Destination_Y => Get_Event(Index => I).Sky_Y),
            Coords =>
              To_Unbounded_String
                (Source =>
                   "X:" & Natural'Image(Get_Event(Index => I).Sky_X) & " Y:" &
                   Natural'Image(Get_Event(Index => I).Sky_Y)),
            Details =>
              (case Get_Event(Index => I).E_Type is
                 when DOUBLEPRICE =>
                   To_String
                     (Source =>
                        Get_Proto_Item
                          (Index => Get_Event(Index => I).Item_Index)
                          .Name) &
                   To_Unbounded_String(Source => " in ") &
                   To_String
                     (Source =>
                        Sky_Bases
                          (Sky_Map
                             (Get_Event(Index => I).Sky_X,
                              Get_Event(Index => I).Sky_Y)
                             .Base_Index)
                          .Name),
                 when ATTACKONBASE | DISEASE | FULLDOCKS | ENEMYPATROL =>
                   To_Unbounded_String
                     (Source =>
                        To_String
                          (Source =>
                             Sky_Bases
                               (Sky_Map
                                  (Get_Event(Index => I).Sky_X,
                                   Get_Event(Index => I).Sky_Y)
                                  .Base_Index)
                               .Name)),
                 when ENEMYSHIP | TRADER | FRIENDLYSHIP =>
                   To_Unbounded_String
                     (Source =>
                        To_String
                          (Source =>
                             Get_Proto_Ship
                               (Proto_Index =>
                                  Get_Event(Index => I).Ship_Index)
                               .Name)),
                 when NONE | BASERECOVERY => Null_Unbounded_String),
            Id => I);
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

   procedure Add_Knowledge_Events_Commands is
   begin
      Add_Command
        (Name => "ShowEventInfo",
         Ada_Command => Show_Event_Info_Command'Access);
      Add_Command
        (Name => "ShowEvents", Ada_Command => Show_Events_Command'Access);
      Add_Command
        (Name => "SortKnownEvents", Ada_Command => Sort_Events_Command'Access);
   end Add_Knowledge_Events_Commands;

   procedure Update_Events_List(Page: Positive := 1) is
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.Canvas;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use CoreUI;

      Events_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Main_Paned & ".knowledgeframe.events.canvas");
      Events_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Events_Canvas & ".frame");
      procedure Update_Ada_Events_List(P: Positive) with
         Convention => C,
         Import => True,
         External_Name => "updateAdaEventsList";
   begin
      Update_Ada_Events_List(P => Page);
      if Get_Events_Amount > 0 then
         Events_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Events_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Distance"),
                 3 => To_Unbounded_String(Source => "Coordinates"),
                 4 => To_Unbounded_String(Source => "Details")),
              Scrollbar =>
                Get_Widget
                  (pathName => Main_Paned & ".knowledgeframe.events.scrolly"),
              Command => "SortKnownEvents",
              Tooltip_Text => "Press mouse button to sort the events.");
      end if;
   end Update_Events_List;

end Knowledge.Events;
