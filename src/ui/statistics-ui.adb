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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Button; use Gtk.Button;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Stack; use Gtk.Stack;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Goals; use Goals;
with Goals.UI; use Goals.UI;
with Ships; use Ships;
with Missions; use Missions;
with Crafts; use Crafts;
with Items; use Items;
with MainMenu; use MainMenu;
with Utils.UI; use Utils.UI;

package body Statistics.UI is

   Builder: Gtkada_Builder;

   procedure HideStatistics is
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "btnclose")));
      if PreviousGameState = SkyMap_View then
         ShowSkyMap;
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Builder, "gamestack")), "skymap");
      else
         Hide(Gtk_Widget(Get_Object(Builder, "skymapwindow")));
         EndGame(False);
         ShowMainMenu;
      end if;
   end HideStatistics;

   procedure ShowGoals(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      ShowGoalsMenu(False);
   end ShowGoals;

   procedure UpdateGoalsButton(Message: String) is
   begin
      Set_Label(Gtk_Button(Get_Object(Builder, "btngoals")), Message);
   end UpdateGoalsButton;

   procedure CreateStatsUI(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Show_Goals", ShowGoals'Access);
   end CreateStatsUI;

   procedure ShowStatsUI is
      MinutesDiff: Natural;
      TimePassed: Date_Record :=
        (Year => 0, Month => 0, Day => 0, Hour => 0, Minutes => 0);
      type VisitedFactor is digits 4 range 0.0 .. 100.0;
      VisitedPercent: VisitedFactor;
      VisitedString: String(1 .. 5);
      MissionsPercent, TotalFinished, TotalDestroyed: Natural := 0;
      StatsText: Unbounded_String;
      Iter: Gtk_Tree_Iter;
      List: Gtk_List_Store;
      ProtoIndex: Positive;
      ItemIndex: Positive;
   begin
      MinutesDiff :=
        (GameDate.Minutes + (GameDate.Hour * 60) + (GameDate.Day * 1440) +
         (GameDate.Month * 43200) + (GameDate.Year * 518400)) -
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
        To_Unbounded_String("Points:" & Natural'Image(GameStats.Points));
      Append
        (StatsText,
         LF & "Time passed:" & Natural'Image(TimePassed.Year) & "y," &
         Natural'Image(TimePassed.Month) & "m," &
         Natural'Image(TimePassed.Day) & "d," &
         Natural'Image(TimePassed.Hour) & "h," &
         Natural'Image(TimePassed.Minutes) & "mins");
      VisitedPercent :=
        (VisitedFactor(GameStats.BasesVisited) / 1024.0) * 100.0;
      Put
        (To => VisitedString, Item => Float(VisitedPercent), Aft => 3,
         Exp => 0);
      Append
        (StatsText,
         LF & "Bases visited:" & Positive'Image(GameStats.BasesVisited) &
         " (" & VisitedString & "%)");
      VisitedPercent :=
        VisitedFactor(Float(GameStats.MapVisited) / (1024.0 * 1024.0)) * 100.0;
      if VisitedPercent < 0.001 then
         VisitedPercent := 0.001;
      end if;
      Put
        (To => VisitedString, Item => Float(VisitedPercent), Aft => 3,
         Exp => 0);
      Append(StatsText, LF & "Map discovered: " & VisitedString & "%");
      Append
        (StatsText,
         LF & "Distance traveled:" &
         Natural'Image(GameStats.DistanceTraveled));
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblstats")), To_String(StatsText));
      TotalFinished := 0;
      for CraftingOrder of GameStats.CraftingOrders loop
         TotalFinished := TotalFinished + CraftingOrder.Amount;
      end loop;
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblcrafts")),
         "_Crafting orders finished:" & Natural'Image(TotalFinished));
      List := Gtk_List_Store(Get_Object(Builder, "craftslist"));
      Clear(List);
      if TotalFinished > 0 then
         for I in GameStats.CraftingOrders.Iterate loop
            Append(List, Iter);
            ItemIndex :=
              Recipes_List(FindRecipe(GameStats.CraftingOrders(I).Index))
                .ResultIndex;
            Set(List, Iter, 0, To_String(Items_List(ItemIndex).Name));
            Set(List, Iter, 1, Gint(GameStats.CraftingOrders(I).Amount));
         end loop;
      end if;
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
        (Gtk_Label(Get_Object(Builder, "lblmissions")),
         "_Missions completed:" & Natural'Image(TotalFinished) & " (" &
         To_String
           (Trim
              (To_Unbounded_String(Natural'Image(MissionsPercent)),
               Ada.Strings.Left)) &
         "%)");
      List := Gtk_List_Store(Get_Object(Builder, "finishedmissionslist"));
      Clear(List);
      if TotalFinished > 0 then
         for I in GameStats.FinishedMissions.Iterate loop
            Append(List, Iter);
            case Missions_Types'Val
              (Integer'Value
                 (To_String(GameStats.FinishedMissions(I).Index))) is
               when Deliver =>
                  Set(List, Iter, 0, "Delivered items");
               when Patrol =>
                  Set(List, Iter, 0, "Patroled areas");
               when Destroy =>
                  Set(List, Iter, 0, "Destroyed ships");
               when Explore =>
                  Set(List, Iter, 0, "Explored areas");
               when Passenger =>
                  Set(List, Iter, 0, "Passengers transported");
            end case;
            Set(List, Iter, 1, Gint(GameStats.FinishedMissions(I).Amount));
         end loop;
      end if;
      Set_Label
        (Gtk_Button(Get_Object(Builder, "btngoals")),
         "Current _goal: " & GoalText(0));
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "gamestats");
      Hide(Gtk_Widget(Get_Object(Builder, "btnshowhelp")));
      if GameStats.DestroyedShips.Length > 0 then
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "expdestroyed")), True);
         List := Gtk_List_Store(Get_Object(Builder, "destroyedlist"));
         Clear(List);
         for I in GameStats.DestroyedShips.Iterate loop
            Append(List, Iter);
            for ProtoShip of ProtoShips_List loop
               if ProtoShip.Index = GameStats.DestroyedShips(I).Index then
                  Set(List, Iter, 0, To_String(ProtoShip.Name));
                  Set(List, Iter, 1, Gint(GameStats.DestroyedShips(I).Amount));
                  exit;
               end if;
            end loop;
            TotalDestroyed :=
              TotalDestroyed + GameStats.DestroyedShips(I).Amount;
         end loop;
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lbldestroyed")),
            "_Destroyed ships (Total:" & Natural'Image(TotalDestroyed) & ")");
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "expdestroyed")), False);
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lbldestroyed")),
            "Destroyed ships: none");
      end if;
      TotalFinished := 0;
      if GameStats.FinishedGoals.Length > 0 then
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "expgoals")), True);
         List := Gtk_List_Store(Get_Object(Builder, "goalslist"));
         Clear(List);
         for I in GameStats.FinishedGoals.Iterate loop
            Append(List, Iter);
            for J in Goals_List.Iterate loop
               if GameStats.FinishedGoals(I).Index = Goals_List(J).Index then
                  ProtoIndex := Goals_Container.To_Index(J);
                  exit;
               end if;
            end loop;
            Set(List, Iter, 0, GoalText(ProtoIndex));
            Set(List, Iter, 1, Gint(GameStats.FinishedGoals(I).Amount));
            TotalFinished := TotalFinished + GameStats.FinishedGoals(I).Amount;
         end loop;
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lblfinishedgoals")),
            "_Finished goals (Total:" & Natural'Image(TotalFinished) & ")");
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "expgoals")), False);
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lblfinishedgoals")),
            "Finished goals: none");
      end if;
      if GameStats.CraftingOrders.Length > 0 then
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "expcrafts")), True);
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "expcrafts")), False);
      end if;
      if GameStats.FinishedMissions.Length > 0 then
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "expmissions")), True);
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "expmissions")), False);
      end if;
      if PlayerShip.Crew(1).Health = 0 then
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btngoals")), False);
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btngoals")), True);
      end if;
      if GameStats.KilledMobs.Length > 0 then
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "expkilledmobs")), True);
         TotalDestroyed := 0;
         List := Gtk_List_Store(Get_Object(Builder, "killedlist"));
         Clear(List);
         for KilledMob of GameStats.KilledMobs loop
            Append(List, Iter);
            Set(List, Iter, 0, To_String(KilledMob.Index));
            Set(List, Iter, 1, Gint(KilledMob.Amount));
            TotalDestroyed := TotalDestroyed + KilledMob.Amount;
         end loop;
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lblkilledstat")),
            "_Killed enemies (Total:" & Natural'Image(TotalDestroyed) & ")");
      else
         Set_Sensitive
           (Gtk_Widget(Get_Object(Builder, "expkilledmobs")), False);
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lblkilledstat")),
            "Killed enemies: none");
      end if;
   end ShowStatsUI;

end Statistics.UI;
