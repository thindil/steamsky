-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships; use Ships;
with Utils.UI; use Utils.UI;

package body Events.UI is

   -- ****o* EUI/Show_Event_Info_Command
   -- FUNCTION
   -- Show information about the selected event
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowEventInfo
   -- SOURCE
   function Show_Event_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Event_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      EventsView: Ttk_Tree_View;
      EventIndex: Positive;
      EventInfo: Unbounded_String;
      BaseIndex: Integer;
      Label: Ttk_Label;
   begin
      EventsView.Interp := Interp;
      EventsView.Name :=
        New_String(".paned.eventsframe.canvas.events.eventsview");
      EventIndex := Positive'Value(Selection(EventsView));
      BaseIndex :=
        SkyMap(Events_List(EventIndex).SkyX, Events_List(EventIndex).SkyY)
          .BaseIndex;
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
      Label.Interp := Get_Context;
      Label.Name :=
        New_String(".paned.eventsframe.canvas.events.info.info.label");
      configure(Label, "-text {" & To_String(EventInfo) & "}");
      return TCL_OK;
   end Show_Event_Info_Command;

   -- ****o* EUI/Show_Event_Command
   -- FUNCTION
   -- Show event on map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowEvent
   -- SOURCE
   function Show_Event_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Event_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      EventsView: Ttk_Tree_View;
      EventIndex: Positive;
   begin
      EventsView.Interp := Interp;
      EventsView.Name :=
        New_String(".paned.eventsframe.canvas.events.eventsview");
      EventIndex := Positive'Value(Selection(EventsView));
      CenterX := Events_List(EventIndex).SkyX;
      CenterY := Events_List(EventIndex).SkyY;
      ShowSkyMap(True);
      return TCL_OK;
   end Show_Event_Command;

   -- ****o* EUI/Set_Event_Command
   -- FUNCTION
   -- Set event as the player's ship destination
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetEvent
   -- SOURCE
   function Set_Event_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Event_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      EventsView: Ttk_Tree_View;
      EventIndex: Positive;
   begin
      EventsView.Interp := Interp;
      EventsView.Name :=
        New_String(".paned.eventsframe.canvas.events.eventsview");
      EventIndex := Positive'Value(Selection(EventsView));
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

   procedure ShowEventsList is
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      EventsCanvas: Tk_Canvas;
      EventsFrame: Ttk_Frame;
      EventsView: Ttk_Tree_View;
   begin
      Paned.Interp := Get_Context;
      Paned.Name := New_String(".paned");
      EventsFrame.Interp := Get_Context;
      EventsFrame.Name := New_String(Widget_Image(Paned) & ".eventsframe");
      EventsCanvas.Interp := Get_Context;
      EventsCanvas.Name := New_String(Widget_Image(EventsFrame) & ".canvas");
      Label.Interp := Get_Context;
      Label.Name :=
        New_String(Widget_Image(EventsCanvas) & ".events.info.info.label");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "events.tcl");
         Bind(EventsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         AddCommand("ShowEventInfo", Show_Event_Info_Command'Access);
         AddCommand("ShowEvent", Show_Event_Command'Access);
         AddCommand("SetEvent", Set_Event_Command'Access);
      elsif Winfo_Get(Label, "ismapped") = "1" then
         ShowSkyMap(True);
         return;
      end if;
      EventsView.Interp := Get_Context;
      EventsView.Name :=
        New_String(Widget_Image(EventsCanvas) & ".events.eventsview");
      Delete(EventsView, "[list " & Children(EventsView, "{}") & "]");
      for I in Events_List.First_Index .. Events_List.Last_Index loop
         case Events_List(I).EType is
            when EnemyShip =>
               Insert
                 (EventsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Enemy ship spotted}" &
                  Positive'Image
                    (CountDistance(Events_List(I).SkyX, Events_List(I).SkyY)) &
                  "]");
            when FullDocks =>
               Insert
                 (EventsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Full docks in base}" &
                  Positive'Image
                    (CountDistance(Events_List(I).SkyX, Events_List(I).SkyY)) &
                  "]");
            when AttackOnBase =>
               Insert
                 (EventsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list  {Base is under attack}" &
                  Positive'Image
                    (CountDistance(Events_List(I).SkyX, Events_List(I).SkyY)) &
                  "]");
            when Disease =>
               Insert
                 (EventsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Disease in base}" &
                  Positive'Image
                    (CountDistance(Events_List(I).SkyX, Events_List(I).SkyY)) &
                  "]");
            when EnemyPatrol =>
               Insert
                 (EventsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Enemy patrol}" &
                  Positive'Image
                    (CountDistance(Events_List(I).SkyX, Events_List(I).SkyY)) &
                  "]");
            when DoublePrice =>
               Insert
                 (EventsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Double price in base}" &
                  Positive'Image
                    (CountDistance(Events_List(I).SkyX, Events_List(I).SkyY)) &
                  "]");
            when Trader =>
               Insert
                 (EventsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Friendly trader spotted}" &
                  Positive'Image
                    (CountDistance(Events_List(I).SkyX, Events_List(I).SkyY)) &
                  "]");
            when FriendlyShip =>
               Insert
                 (EventsView,
                  "{} end -id" & Positive'Image(I) &
                  " -values [list {Friendly ship spotted}" &
                  Positive'Image
                    (CountDistance(Events_List(I).SkyX, Events_List(I).SkyY)) &
                  "]");
            when None | BaseRecovery =>
               null;
         end case;
      end loop;
      Selection_Set(EventsView, "[list 1]");
      configure
        (EventsCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      EventsFrame.Name := New_String(Widget_Image(EventsCanvas) & ".events");
      Canvas_Create
        (EventsCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(EventsFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (EventsCanvas,
         "-scrollregion [list " & BBox(EventsCanvas, "all") & "]");
      ShowScreen("eventsframe");
   end ShowEventsList;

end Events.UI;
