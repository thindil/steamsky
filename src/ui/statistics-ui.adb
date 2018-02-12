--    Copyright 2018 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Button; use Gtk.Button;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Goals; use Goals;

package body Statistics.UI is

   Builder: Gtkada_Builder;

   function HideStatistics
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      CreateSkyMap;
      return Hide_On_Delete
          (Gtk_Widget(Get_Object(Object, "statisticswindow")));
   end HideStatistics;

   procedure CreateStatsUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) &
           "ui" &
           Dir_Separator &
           "statistics.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Hide_Statistics", HideStatistics'Access);
      Do_Connect(Builder);
   end CreateStatsUI;

   procedure ShowStatsUI is
      MinutesDiff: Natural;
      TimePassed: Date_Record :=
        (Year => 0, Month => 0, Day => 0, Hour => 0, Minutes => 0);
      type VisitedFactor is digits 4 range 0.0 .. 100.0;
      VisitedPercent: VisitedFactor;
      VisitedString: String(1 .. 5);
      MissionsPercent, TotalFinished: Natural := 0;
      StatsText: Unbounded_String;
   begin
      MinutesDiff :=
        (GameDate.Minutes +
         (GameDate.Hour * 60) +
         (GameDate.Day * 1440) +
         (GameDate.Month * 43200) +
         (GameDate.Year * 518400)) -
        829571520;
      while MinutesDiff > 0 loop
         if MinutesDiff >= 518400 then
            TimePassed.Year := TimePassed.Year + 1;
            MinutesDiff := MinutesDiff - 518400;
         elsif MinutesDiff >= 43200 then
            TimePassed.Month := TimePassed.Month + 1;
            MinutesDiff := MinutesDiff - 43200;
         elsif MinutesDiff >= 1440 then
            TimePassed.Day := TimePassed.Day + 1;
            MinutesDiff := MinutesDiff - 1440;
         elsif MinutesDiff >= 60 then
            TimePassed.Hour := TimePassed.Hour + 1;
            MinutesDiff := MinutesDiff - 60;
         else
            TimePassed.Minutes := MinutesDiff;
            MinutesDiff := 0;
         end if;
      end loop;
      StatsText :=
        To_Unbounded_String
          ("Time passed:" &
           Natural'Image(TimePassed.Year) &
           "y," &
           Natural'Image(TimePassed.Month) &
           "m," &
           Natural'Image(TimePassed.Day) &
           "d," &
           Natural'Image(TimePassed.Hour) &
           "h," &
           Natural'Image(TimePassed.Minutes) &
           "mins");
      VisitedPercent :=
        (VisitedFactor(GameStats.BasesVisited) / 1024.0) * 100.0;
      Put
        (To => VisitedString,
         Item => Float(VisitedPercent),
         Aft => 3,
         Exp => 0);
      Append
        (StatsText,
         ASCII.LF &
         "Bases visited:" &
         Positive'Image(GameStats.BasesVisited) &
         " (" &
         VisitedString &
         "%)");
      VisitedPercent :=
        VisitedFactor(Float(GameStats.MapVisited) / (1024.0 * 1024.0)) * 100.0;
      if VisitedPercent < 0.001 then
         VisitedPercent := 0.001;
      end if;
      Put
        (To => VisitedString,
         Item => Float(VisitedPercent),
         Aft => 3,
         Exp => 0);
      Append(StatsText, ASCII.LF & "Map discovered: " & VisitedString & "%");
      Append
        (StatsText,
         ASCII.LF &
         "Distance traveled:" &
         Natural'Image(GameStats.DistanceTraveled));
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblstats")),
         To_String(StatsText));
      TotalFinished := 0;
      for CraftingOrder of GameStats.CraftingOrders loop
         TotalFinished := TotalFinished + CraftingOrder.Amount;
      end loop;
      Set_Label
        (Gtk_Button(Get_Object(Builder, "btncrafts")),
         "Crafting orders finished:" & Natural'Image(TotalFinished));
      TotalFinished := 0;
      for FinishedMission of GameStats.FinishedMissions loop
         TotalFinished := TotalFinished + FinishedMission.Amount;
      end loop;
      if GameStats.AcceptedMissions > 0 then
         MissionsPercent :=
           Natural
             ((Float(TotalFinished) / Float(GameStats.AcceptedMissions)) *
              100.0);
      end if;
      Set_Label
        (Gtk_Button(Get_Object(Builder, "btnmissions")),
         "Missions finished:" &
         Natural'Image(TotalFinished) &
         " (" &
         To_String
           (Trim
              (To_Unbounded_String(Natural'Image(MissionsPercent)),
               Ada.Strings.Left)) &
         "%)");
      Set_Label
        (Gtk_Button(Get_Object(Builder, "btngoals")),
         "Current goal: " & GoalText(0));
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblpoints")),
         "Points:" & Natural'Image(GameStats.Points));
      Show_All(Gtk_Widget(Get_Object(Builder, "statisticswindow")));
   end ShowStatsUI;

end Statistics.UI;
