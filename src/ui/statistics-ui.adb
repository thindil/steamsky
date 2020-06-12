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
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Config; use Config;
with Game; use Game;
with Utils.UI; use Utils.UI;

package body Statistics.UI is

   procedure ShowStatistics is
      TotalFinished, TotalDestroyed: Natural := 0;
      StatsText: Unbounded_String;
      ProtoIndex: Positive;
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      SubWindows: Unbounded_String;
      SubWindow: Ttk_Frame;
   begin
      Label.Interp := Get_Context;
      Label.Name := New_String(".paned.statsframe.left.crafts");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "stats.tcl");
      end if;
      StatsText :=
        To_Unbounded_String("Points:" & Natural'Image(GetGamePoints));
      Append(StatsText, LF & "Time passed:");
      declare
         MinutesDiff: constant Natural :=
           (GameDate.Minutes + (GameDate.Hour * 60) + (GameDate.Day * 1440) +
            (GameDate.Month * 43200) + (GameDate.Year * 518400)) -
           829571520;
      begin
         MinutesToDate(MinutesDiff, StatsText);
      end;
      declare
         type VisitedFactor is digits 4 range 0.0 .. 100.0;
         VisitedPercent: VisitedFactor;
         VisitedString: String(1 .. 5);
      begin
         VisitedPercent :=
           VisitedFactor((Float(GameStats.BasesVisited) / 1024.0) * 100.0);
         Put
           (To => VisitedString, Item => Float(VisitedPercent), Aft => 3,
            Exp => 0);
         Append
           (StatsText,
            LF & "Bases visited:" & Positive'Image(GameStats.BasesVisited) &
            " (" & VisitedString & "%)");
         VisitedPercent :=
           VisitedFactor(Float(GameStats.MapVisited) / (1024.0 * 1024.0)) *
           100.0;
         if VisitedPercent < 0.001 then
            VisitedPercent := 0.001;
         end if;
         Put
           (To => VisitedString, Item => Float(VisitedPercent), Aft => 3,
            Exp => 0);
         Append(StatsText, LF & "Map discovered: " & VisitedString & "%");
      end;
      Append
        (StatsText,
         LF & "Distance traveled:" &
         Natural'Image(GameStats.DistanceTraveled));
      Label.Name := New_String(".paned.statsframe.left.stats");
      configure(Label, "-text {" & To_String(StatsText) & "}");
      TotalFinished := 0;
      for CraftingOrder of GameStats.CraftingOrders loop
         TotalFinished := TotalFinished + CraftingOrder.Amount;
      end loop;
      configure
        (Label,
         "-text {Crafting orders finished:" & Natural'Image(TotalFinished) &
         "}");
      Paned.Interp := Get_Context;
      Paned.Name := New_String(".paned");
      SubWindow.Interp := Get_Context;
      SubWindows := To_Unbounded_String(Panes(Paned));
      if Index(SubWindows, " ") = 0 then
         SubWindow.Name := New_String(To_String(SubWindows));
      else
         SubWindow.Name :=
           New_String(Slice(SubWindows, 1, Index(SubWindows, " ")));
      end if;
      Forget(Paned, SubWindow);
      SubWindow.Name := New_String(".paned.statsframe");
      Insert(Paned, "0", SubWindow);
      SashPos(Paned, "0", Natural'Image(GameSettings.MessagesPosition));
   end ShowStatistics;

end Statistics.UI;
