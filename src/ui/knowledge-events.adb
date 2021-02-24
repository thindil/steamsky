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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
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
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Knowledge.Events is

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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Events_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Event_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      EventIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      CenterX := Events_List(EventIndex).SkyX;
      CenterY := Events_List(EventIndex).SkyY;
      ShowSkyMap(True);
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Event_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      EventIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
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
      ShowSkyMap(True);
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Event_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      EventIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      EventInfo: Unbounded_String;
      BaseIndex: constant Extended_BaseRange :=
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

   procedure AddCommands is
   begin
      AddCommand("ShowEventMenu", Show_Events_Menu_Command'Access);
      AddCommand("ShowEvent", Show_Event_Command'Access);
      AddCommand("SetEvent", Set_Event_Command'Access);
      AddCommand("ShowEventInfo", Show_Event_Info_Command'Access);
   end AddCommands;

end Knowledge.Events;
