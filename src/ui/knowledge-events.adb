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
with Ada.Containers; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
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
   -- Interp     - Tcl interpreter in which command was executed.
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
      pragma Unreferenced(ClientData, Argc);
      EventMenu: Tk_Menu := Get_Widget(".eventslistmenu", Interp);
   begin
      if Winfo_Get(EventMenu, "exists") = "0" then
         EventMenu := Create(".eventslistmenu", "-tearoff false");
      end if;
      Delete(EventMenu, "0", "end");
      Menu.Add
        (EventMenu, "command",
         "-label {Show the event on map} -command {ShowEvent " &
         CArgv.Arg(Argv, 1) & "}");
      Menu.Add
        (EventMenu, "command",
         "-label {Set the event as destination for the ship} -command {SetEvent " &
         CArgv.Arg(Argv, 1) & "}");
      Menu.Add
        (EventMenu, "command",
         "-label {Show more information about the event} -command {ShowEventInfo " &
         CArgv.Arg(Argv, 1) & "}");
      Tk_Popup
        (EventMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
         Winfo_Get(Get_Main_Window(Interp), "pointery"));
      return TCL_OK;
   end Show_Events_Menu_Command;

   -- ****if* KEvents/KEvents.Show_Event_Command
   -- FUNCTION
   -- Show the selected event on map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowEvent eventidex
   -- EventIndex is the index of the event to show
   -- SOURCE
   function Show_Event_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Event_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      EventIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
   begin
      CenterX := Events_List(EventIndex).SkyX;
      CenterY := Events_List(EventIndex).SkyY;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      Tcl_Eval(Interp, "InvokeButton " & CloseButton);
      Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
      return TCL_OK;
   end Show_Event_Command;

   -- ****if* KEvents/KEvents.Set_Event_Command
   -- FUNCTION
   -- Set the selected event as the player's ship destination
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetEvent2 eventidex
   -- EventIndex is the index of the event to show
   -- SOURCE
   function Set_Event_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Event_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      EventIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
   begin
      if Events_List(EventIndex).SkyX = PlayerShip.SkyX and
        Events_List(EventIndex).SkyY = PlayerShip.SkyY then
         ShowMessage("You are at this event now.");
         return TCL_OK;
      end if;
      PlayerShip.DestinationX := Events_List(EventIndex).SkyX;
      PlayerShip.DestinationY := Events_List(EventIndex).SkyY;
      AddMessage
        ("You set the travel destination for your ship.", OrderMessage);
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      Tcl_Eval(Interp, "InvokeButton " & CloseButton);
      Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
      return TCL_OK;
   end Set_Event_Command;

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
      EventIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      EventInfo: Unbounded_String;
      BaseIndex: constant Extended_Base_Range :=
        SkyMap(Events_List(EventIndex).SkyX, Events_List(EventIndex).SkyY)
          .BaseIndex;
   begin
      EventInfo :=
        To_Unbounded_String
          ("X:" & Positive'Image(Events_List(EventIndex).SkyX) & " Y:" &
           Positive'Image(Events_List(EventIndex).SkyY));
      case Events_List(EventIndex).EType is
         when EnemyShip | EnemyPatrol | Trader | FriendlyShip =>
            Append
              (EventInfo,
               LF & "Ship type: " &
               To_String
                 (ProtoShips_List(Events_List(EventIndex).ShipIndex).Name));
         when FullDocks | AttackOnBase | Disease =>
            Append
              (EventInfo,
               LF & "Base name: " & To_String(SkyBases(BaseIndex).Name));
         when DoublePrice =>
            Append
              (EventInfo,
               LF & "Base name: " & To_String(SkyBases(BaseIndex).Name));
            Append
              (EventInfo,
               LF & "Item: " &
               To_String(Items_List(Events_List(EventIndex).ItemIndex).Name));
         when None | BaseRecovery =>
            null;
      end case;
      ShowInfo(To_String(EventInfo));
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

   procedure AddCommands is
   begin
      AddCommand("ShowEventMenu", Show_Events_Menu_Command'Access);
      AddCommand("ShowEvent", Show_Event_Command'Access);
      AddCommand("SetEvent", Set_Event_Command'Access);
      AddCommand("ShowEventInfo", Show_Event_Info_Command'Access);
      AddCommand("ShowEvents", Show_Events_Command'Access);
   end AddCommands;

   procedure UpdateEventsList(Page: Positive := 1) is
      EventsCanvas: constant Tk_Canvas :=
        Get_Widget(".gameframe.paned.knowledgeframe.events.canvas");
      EventsFrame: constant Ttk_Frame := Get_Widget(EventsCanvas & ".frame");
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Label: Ttk_Label;
      Row: Positive;
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
      Current_Row: Positive := 1;
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(EventsFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      if EventsTable.Row > 1 then
         ClearTable(EventsTable);
      end if;
      Delete_Widgets(1, Rows - 1, EventsFrame);
      if Events_List.Length = 0 then
         Label :=
           Create
             (EventsFrame & ".noevents",
              "-text {You don't know any event yet. You may ask for events in bases. When your ship is docked to base, select Ask for Events from ship orders menu.} -wraplength 400");
         Tcl.Tk.Ada.Grid.Grid(Label);
      else
         Row := 2;
         EventsTable :=
           CreateTable
             (Widget_Image(EventsFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Distance"),
               To_Unbounded_String("Details")),
              False);
         Rows := 0;
         Load_Known_Events_Loop :
         for Event of Events_List loop
            if Current_Row < Start_Row then
               Current_Row := Current_Row + 1;
               goto End_Of_Loop;
            end if;
            case Event.EType is
               when EnemyShip =>
                  AddButton
                    (EventsTable, "Enemy ship spotted",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when FullDocks =>
                  AddButton
                    (EventsTable, "Full docks in base",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when AttackOnBase =>
                  AddButton
                    (EventsTable, "Base is under attack",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when Disease =>
                  AddButton
                    (EventsTable, "Disease in base",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when EnemyPatrol =>
                  AddButton
                    (EventsTable, "Enemy patrol",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when DoublePrice =>
                  AddButton
                    (EventsTable, "Double price in base",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when Trader =>
                  AddButton
                    (EventsTable, "Friendly trader spotted",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when FriendlyShip =>
                  AddButton
                    (EventsTable, "Friendly ship spotted",
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 1);
               when None | BaseRecovery =>
                  null;
            end case;
            AddButton
              (EventsTable,
               Natural'Image(CountDistance(Event.SkyX, Event.SkyY)),
               "The distance to the event",
               "ShowEventMenu" & Positive'Image(Row - 1), 2);
            case Event.EType is
               when DoublePrice =>
                  AddButton
                    (EventsTable,
                     To_String(Items_List(Event.ItemIndex).Name) & " in " &
                     To_String
                       (SkyBases(SkyMap(Event.SkyX, Event.SkyY).BaseIndex)
                          .Name),
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 3, True);
               when AttackOnBase | Disease | FullDocks | EnemyPatrol =>
                  AddButton
                    (EventsTable,
                     To_String
                       (SkyBases(SkyMap(Event.SkyX, Event.SkyY).BaseIndex)
                          .Name),
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 3, True);
               when EnemyShip | Trader | FriendlyShip =>
                  AddButton
                    (EventsTable,
                     To_String(ProtoShips_List(Event.ShipIndex).Name),
                     "Show available event's options",
                     "ShowEventMenu" & Positive'Image(Row - 1), 3, True);
               when None | BaseRecovery =>
                  null;
            end case;
            Row := Row + 1;
            Rows := Rows + 1;
            exit Load_Known_Events_Loop when Rows = 25 and
              Event /= Events_List.Last_Element;
            <<End_Of_Loop>>
         end loop Load_Known_Events_Loop;
         if Page > 1 then
            if Rows < 25 then
               AddPagination
                 (EventsTable, "ShowEvents" & Positive'Image(Page - 1), "");
            else
               AddPagination
                 (EventsTable, "ShowEvents" & Positive'Image(Page - 1),
                  "ShowEvents" & Positive'Image(Page + 1));
            end if;
         elsif Rows > 24 then
            AddPagination
              (EventsTable, "", "ShowEvents" & Positive'Image(Page + 1));
         end if;
         UpdateTable(EventsTable);
      end if;
      Tcl_Eval(Get_Context, "update");
      configure
        (EventsCanvas,
         "-scrollregion [list " & BBox(EventsCanvas, "all") & "]");
      Xview_Move_To(EventsCanvas, "0.0");
      Yview_Move_To(EventsCanvas, "0.0");
   end UpdateEventsList;

end Knowledge.Events;
