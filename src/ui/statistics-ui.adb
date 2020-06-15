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
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Crafts; use Crafts;
with Game; use Game;
with Goals; use Goals;
with Items; use Items;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with Utils.UI; use Utils.UI;

package body Statistics.UI is

   procedure ShowStatistics is
      TotalFinished, TotalDestroyed: Natural := 0;
      StatsText: Unbounded_String;
      ProtoIndex: Positive;
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      StatsCanvas: Tk_Canvas;
      StatsFrame: Ttk_Frame;
      TreeView: Ttk_Tree_View;
   begin
      Paned.Interp := Get_Context;
      Paned.Name := New_String(".paned");
      StatsFrame.Interp := Get_Context;
      StatsFrame.Name := New_String(Widget_Image(Paned) & ".statsframe");
      StatsCanvas.Interp := Get_Context;
      StatsCanvas.Name := New_String(Widget_Image(StatsFrame) & ".canvas");
      Label.Interp := Get_Context;
      Label.Name :=
        New_String(Widget_Image(StatsCanvas) & ".stats.left.stats");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "stats.tcl");
         Bind(StatsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" then
         ShowSkyMap(True);
         return;
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
      configure(Label, "-text {" & To_String(StatsText) & "}");
      StatsFrame.Name := New_String(Widget_Image(StatsCanvas) & ".stats");
      TotalFinished := 0;
      for CraftingOrder of GameStats.CraftingOrders loop
         TotalFinished := TotalFinished + CraftingOrder.Amount;
      end loop;
      Label.Name := New_String(Widget_Image(StatsFrame) & ".left.crafts");
      configure
        (Label,
         "-text {Crafting orders finished:" & Natural'Image(TotalFinished) &
         "}");
      TreeView.Interp := Get_Context;
      TreeView.Name :=
        New_String(Widget_Image(StatsFrame) & ".left.craftsview");
      if Children(TreeView, "{}") /= "{}" then
         Delete(TreeView, "[list " & Children(TreeView, "{}") & "]");
      end if;
      if TotalFinished > 0 then
         for Order of GameStats.CraftingOrders loop
            Insert
              (TreeView,
               "{} end -values [list {" &
               To_String
                 (Items_List(Recipes_List(Order.Index).ResultIndex).Name) &
               "} {" & Positive'Image(Order.Amount) & "}]");
         end loop;
         Tcl.Tk.Ada.Grid.Grid(TreeView);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(TreeView);
      end if;
      TotalFinished := 0;
      for FinishedMission of GameStats.FinishedMissions loop
         TotalFinished := TotalFinished + FinishedMission.Amount;
      end loop;
      Label.Name := New_String(Widget_Image(StatsFrame) & ".left.missions");
      declare
         MissionsPercent: Natural := 0;
      begin
         if GameStats.AcceptedMissions > 0 then
            MissionsPercent :=
              Natural
                ((Float(TotalFinished) / Float(GameStats.AcceptedMissions)) *
                 100.0);
         end if;
         configure
           (Label,
            "-text {Missions completed:" & Natural'Image(TotalFinished) &
            " (" &
            To_String
              (Trim
                 (To_Unbounded_String(Natural'Image(MissionsPercent)),
                  Ada.Strings.Left)) &
            "%)" & "}");
      end;
      TreeView.Interp := Get_Context;
      TreeView.Name :=
        New_String(Widget_Image(StatsFrame) & ".left.missionsview");
      if Children(TreeView, "{}") /= "{}" then
         Delete(TreeView, "[list " & Children(TreeView, "{}") & "]");
      end if;
      if TotalFinished > 0 then
         for FinishedMission of GameStats.FinishedMissions loop
            case Missions_Types'Val
              (Integer'Value(To_String(FinishedMission.Index))) is
               when Deliver =>
                  Insert
                    (TreeView,
                     "{} end -values [list {Delivered items} {" &
                     Positive'Image(FinishedMission.Amount) & "}]");
               when Patrol =>
                  Insert
                    (TreeView,
                     "{} end -values [list {Patroled areas} {" &
                     Positive'Image(FinishedMission.Amount) & "}]");
               when Destroy =>
                  Insert
                    (TreeView,
                     "{} end -values [list {Destroyed ships} {" &
                     Positive'Image(FinishedMission.Amount) & "}]");
               when Explore =>
                  Insert
                    (TreeView,
                     "{} end -values [list {Explored areas} {" &
                     Positive'Image(FinishedMission.Amount) & "}]");
               when Passenger =>
                  Insert
                    (TreeView,
                     "{} end -values [list {Passengers transported} {" &
                     Positive'Image(FinishedMission.Amount) & "}]");
            end case;
         end loop;
         Tcl.Tk.Ada.Grid.Grid(TreeView);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(TreeView);
      end if;
      Label.Name := New_String(Widget_Image(StatsFrame) & ".left.goal");
      configure(Label, "-text {" & GoalText(0) & "}");
      TotalFinished := 0;
      for FinishedGoal of GameStats.FinishedGoals loop
         TotalFinished := TotalFinished + FinishedGoal.Amount;
      end loop;
      Label.Name := New_String(Widget_Image(StatsFrame) & ".left.goals");
      configure
        (Label, "-text {Finished goals:" & Natural'Image(TotalFinished) & "}");
      TreeView.Interp := Get_Context;
      TreeView.Name :=
        New_String(Widget_Image(StatsFrame) & ".left.goalsview");
      if Children(TreeView, "{}") /= "{}" then
         Delete(TreeView, "[list " & Children(TreeView, "{}") & "]");
      end if;
      if TotalFinished > 0 then
         for Goal of GameStats.FinishedGoals loop
            for J in Goals_List.Iterate loop
               if Goal.Index = Goals_List(J).Index then
                  ProtoIndex := Goals_Container.To_Index(J);
                  exit;
               end if;
            end loop;
            Insert
              (TreeView,
               "{} end -values [list {" & GoalText(ProtoIndex) & "} {" &
               Positive'Image(Goal.Amount) & "}]");
         end loop;
         Tcl.Tk.Ada.Grid.Grid(TreeView);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(TreeView);
      end if;
      configure
        (StatsCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (StatsCanvas, "window",
         "[expr " & Winfo_Get(StatsFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(StatsFrame, "reqheight") & " / 2] -window " &
         Widget_Image(StatsFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (StatsCanvas, "-scrollregion [list " & BBox(StatsCanvas, "all") & "]");
      ShowScreen("statsframe");
   end ShowStatistics;

end Statistics.UI;
