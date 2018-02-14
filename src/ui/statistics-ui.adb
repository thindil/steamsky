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
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
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

package body Statistics.UI is

   Builder: Gtkada_Builder;
   GameState: GameStates;

   function HideStatistics
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      if GameState = SkyMap_View then
         CreateSkyMap;
      else
         EndGame(False);
         ShowMainMenu;
      end if;
      return Hide_On_Delete
          (Gtk_Widget(Get_Object(Object, "statisticswindow")));
   end HideStatistics;

   procedure ShowMore(User_Data: access GObject_Record'Class) is
      InfoIter: Gtk_Tree_Iter;
      InfoList: Gtk_List_Store;
      ItemIndex: Positive;
   begin
      InfoList := Gtk_List_Store(Get_Object(Builder, "infolist"));
      Clear(InfoList);
      if User_Data = Get_Object(Builder, "btnmissions") then
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lblinfo")),
            "Finished missions:");
         for I in GameStats.FinishedMissions.Iterate loop
            Append(InfoList, InfoIter);
            case Missions_Types'Val
              (Integer'Value
                 (To_String(GameStats.FinishedMissions(I).Index))) is
               when Deliver =>
                  Set(InfoList, InfoIter, 0, "Delivered items");
               when Patrol =>
                  Set(InfoList, InfoIter, 0, "Patroled areas");
               when Destroy =>
                  Set(InfoList, InfoIter, 0, "Destroyed ships");
               when Explore =>
                  Set(InfoList, InfoIter, 0, "Explored areas");
               when Passenger =>
                  Set(InfoList, InfoIter, 0, "Passengers transported");
            end case;
            Set
              (InfoList,
               InfoIter,
               1,
               Gint(GameStats.FinishedMissions(I).Amount));
         end loop;
      else
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lblinfo")),
            "Finished crafting orders:");
         for I in GameStats.CraftingOrders.Iterate loop
            Append(InfoList, InfoIter);
            ItemIndex :=
              Recipes_List(FindRecipe(GameStats.CraftingOrders(I).Index))
                .ResultIndex;
            Set(InfoList, InfoIter, 0, To_String(Items_List(ItemIndex).Name));
            Set
              (InfoList,
               InfoIter,
               1,
               Gint(GameStats.CraftingOrders(I).Amount));
         end loop;
      end if;
      Show_All(Gtk_Widget(Get_Object(Builder, "showmorewindow")));
   end ShowMore;

   procedure ShowGoals(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      ShowGoalsMenu(False);
   end ShowGoals;

   procedure UpdateGoalsButton(Message: String) is
   begin
      Set_Label(Gtk_Button(Get_Object(Builder, "btngoals")), Message);
   end UpdateGoalsButton;

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
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Register_Handler(Builder, "Hide_Statistics", HideStatistics'Access);
      Register_Handler(Builder, "Show_More", ShowMore'Access);
      Register_Handler(Builder, "Show_Goals", ShowGoals'Access);
      Do_Connect(Builder);
   end CreateStatsUI;

   procedure ShowStatsUI(OldState: GameStates) is
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
   begin
      GameState := OldState;
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
      if GameStats.DestroyedShips.Length > 0 then
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
            "Destroyed ships (Total:" & Natural'Image(TotalDestroyed) & ")");
         Show_All(Gtk_Widget(Get_Object(Builder, "scrolldestroyed")));
      else
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lbldestroyed")),
            "Destroyed ships: none");
         Hide(Gtk_Widget(Get_Object(Builder, "scrolldestroyed")));
      end if;
      TotalFinished := 0;
      if GameStats.FinishedGoals.Length > 0 then
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
            "Finished goals (Total:" & Natural'Image(TotalFinished) & ")");
         Show_All(Gtk_Widget(Get_Object(Builder, "scrollgoals")));
      else
         Set_Label
           (Gtk_Label(Get_Object(Builder, "lblfinishedgoals")),
            "Finished goals: none");
         Hide(Gtk_Widget(Get_Object(Builder, "scrollgoals")));
      end if;
      if GameStats.FinishedMissions.Length > 0 then
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btncrafts")), True);
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btncrafts")), False);
      end if;
      if GameStats.CraftingOrders.Length > 0 then
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnmissions")), True);
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btnmissions")), False);
      end if;
      if PlayerShip.Crew(1).Health = 0 then
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btngoals")), False);
      else
         Set_Sensitive(Gtk_Widget(Get_Object(Builder, "btngoals")), True);
      end if;
   end ShowStatsUI;

end Statistics.UI;
