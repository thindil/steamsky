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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Utils.UI; use Utils.UI;

package body Events.UI is

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
      elsif Winfo_Get(Label, "ismapped") = "1" then
         ShowSkyMap(True);
         return;
      end if;
      EventsView.Interp := Get_Context;
      EventsView.Name :=
        New_String(Widget_Image(EventsCanvas) & ".events.eventsview");
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
      configure
        (EventsCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      EventsFrame.Name := New_String(Widget_Image(EventsCanvas) & ".events");
      Canvas_Create
        (EventsCanvas, "window",
         "[expr " & Winfo_Get(EventsFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(EventsFrame, "reqheight") & " / 2] -window " &
         Widget_Image(EventsFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (EventsCanvas,
         "-scrollregion [list " & BBox(EventsCanvas, "all") & "]");
      ShowScreen("eventsframe");
   end ShowEventsList;

end Events.UI;
