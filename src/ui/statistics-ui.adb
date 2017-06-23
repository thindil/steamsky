--    Copyright 2017 Bartek thindil Jasicki
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

with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with UserInterface; use UserInterface;
with Ships; use Ships;
with MainMenu; use MainMenu;
with Goals; use Goals;
with Goals.UI; use Goals.UI;
with Help.UI; use Help.UI;
with Missions; use Missions;
with Crafts; use Crafts;
with Items; use Items;

package body Statistics.UI is

   DestroyedShipsPad, FinishedGoalsPad, DetailedStatsPad: Window;
   StartIndex,
   StartIndex2,
   EndIndex,
   EndIndex2,
   StartIndex3,
   EndIndex3: Integer :=
     0;

   procedure ShowGameStats(RefreshOnly: Boolean := False) is
      MinutesDiff: Natural;
      TimePassed: Date_Record :=
        (Year => 0, Month => 0, Day => 0, Hour => 0, Minutes => 0);
      type VisitedFactor is digits 4 range 0.0 .. 100.0;
      VisitedPercent: VisitedFactor;
      VisitedString: String(1 .. 5);
      MissionsPercent, TotalDestroyed, TotalFinished: Natural := 0;
      ProtoIndex: Positive;
   begin
      if not RefreshOnly then
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
         Move_Cursor(Line => 2, Column => 2);
         Add
           (Str =>
              "Time passed:" &
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
         if GameStats.DestroyedShips.Length > 0 then
            DestroyedShipsPad :=
              New_Pad
                (Line_Position(GameStats.DestroyedShips.Length + 2),
                 (Columns / 2));
            for I in GameStats.DestroyedShips.Iterate loop
               Move_Cursor
                 (Win => DestroyedShipsPad,
                  Line => Line_Position(Statistics_Container.To_Index(I)),
                  Column => 0);
               Add
                 (Win => DestroyedShipsPad,
                  Str =>
                    To_String(GameStats.DestroyedShips(I).Index) &
                    ":" &
                    Positive'Image(GameStats.DestroyedShips(I).Amount));
               TotalDestroyed :=
                 TotalDestroyed + GameStats.DestroyedShips(I).Amount;
            end loop;
            Move_Cursor(Win => DestroyedShipsPad, Line => 0, Column => 0);
            Add
              (Win => DestroyedShipsPad,
               Str =>
                 "Destroyed ships (Total:" &
                 Natural'Image(TotalDestroyed) &
                 ")");
            EndIndex :=
              Integer(GameStats.DestroyedShips.Length) - Integer(Lines - 2);
         else
            DestroyedShipsPad := New_Pad(2, (Columns / 2));
            Add(Win => DestroyedShipsPad, Str => "Destroyed ships:");
            Move_Cursor(Win => DestroyedShipsPad, Line => 1, Column => 0);
            Add(Win => DestroyedShipsPad, Str => "none");
         end if;
         Move_Cursor(Line => 3, Column => 2);
         VisitedPercent :=
           (VisitedFactor(GameStats.BasesVisited) / 1024.0) * 100.0;
         Put
           (To => VisitedString,
            Item => Float(VisitedPercent),
            Aft => 3,
            Exp => 0);
         Add
           (Str =>
              "Bases visited:" &
              Positive'Image(GameStats.BasesVisited) &
              " (" &
              VisitedString &
              "%)");
         Move_Cursor(Line => 4, Column => 2);
         VisitedPercent :=
           VisitedFactor(Float(GameStats.MapVisited) / (1024.0 * 1024.0)) *
           100.0;
         if VisitedPercent < 0.001 then
            VisitedPercent := 0.001;
         end if;
         Put
           (To => VisitedString,
            Item => Float(VisitedPercent),
            Aft => 3,
            Exp => 0);
         Add(Str => "Map discovered: " & VisitedString & "%");
         Move_Cursor(Line => 5, Column => 2);
         Add
           (Str =>
              "Distance traveled:" &
              Natural'Image(GameStats.DistanceTraveled));
         TotalFinished := 0;
         for CraftingOrder of GameStats.CraftingOrders loop
            TotalFinished := TotalFinished + CraftingOrder.Amount;
         end loop;
         Move_Cursor(Line => 6, Column => 2);
         Add
           (Str => "Crafting orders finished:" & Natural'Image(TotalFinished));
         Change_Attributes(Line => 6, Column => 3, Count => 1, Color => 1);
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
         Move_Cursor(Line => 7, Column => 2);
         Add
           (Str =>
              "Missions finished:" &
              Natural'Image(TotalFinished) &
              " (" &
              To_String
                (Trim
                   (To_Unbounded_String(Natural'Image(MissionsPercent)),
                    Ada.Strings.Left)) &
              "%)");
         Change_Attributes(Line => 7, Column => 2, Count => 1, Color => 1);
         Move_Cursor(Line => 8, Column => 2);
         Add(Str => "Current goal: " & GoalText(0));
         Change_Attributes(Line => 8, Column => 2, Count => 1, Color => 1);
         TotalFinished := 0;
         if GameStats.FinishedGoals.Length > 0 then
            FinishedGoalsPad :=
              New_Pad
                (Line_Position(GameStats.FinishedGoals.Length + 2),
                 (Columns / 2) - 2);
            for I in GameStats.FinishedGoals.Iterate loop
               Move_Cursor
                 (Win => FinishedGoalsPad,
                  Line => Line_Position(Statistics_Container.To_Index(I)),
                  Column => 0);
               for J in Goals_List.Iterate loop
                  if GameStats.FinishedGoals(I).Index =
                    Goals_List(J).Index then
                     ProtoIndex := Goals_Container.To_Index(J);
                     exit;
                  end if;
               end loop;
               Add
                 (Win => FinishedGoalsPad,
                  Str =>
                    GoalText(ProtoIndex) &
                    ":" &
                    Positive'Image(GameStats.FinishedGoals(I).Amount));
               TotalFinished :=
                 TotalFinished + GameStats.FinishedGoals(I).Amount;
            end loop;
            Move_Cursor(Win => FinishedGoalsPad, Line => 0, Column => 0);
            Add
              (Win => FinishedGoalsPad,
               Str =>
                 "Finished goals (Total:" &
                 Natural'Image(TotalFinished) &
                 ")");
            EndIndex2 :=
              Integer(GameStats.FinishedGoals.Length) - Integer(Lines - 2);
         else
            FinishedGoalsPad := New_Pad(2, (Columns / 2));
            Add(Win => FinishedGoalsPad, Str => "Finished goals: none");
         end if;
         Refresh;
      end if;
      Refresh
        (DestroyedShipsPad,
         Line_Position(StartIndex),
         0,
         2,
         (Columns / 2),
         (Lines - 1),
         Columns);
      Refresh
        (FinishedGoalsPad,
         Line_Position(StartIndex2),
         0,
         9,
         2,
         (Lines - 1),
         (Columns / 2) - 2);
   end ShowGameStats;

   procedure ShowDetailedStats(StatsType: String; RefreshOnly: Boolean) is
      BoxWindow: Window;
      WindowHeight: Line_Position := 3;
      ItemIndex: Positive;
   begin
      if not RefreshOnly then
         if StatsType = "missions" then
            if GameStats.FinishedMissions.Length > 0 then
               WindowHeight :=
                 Line_Position(GameStats.FinishedMissions.Length) + 2;
               EndIndex3 :=
                 Integer(GameStats.FinishedMissions.Length) -
                 Integer(Lines - 2);
               DetailedStatsPad :=
                 New_Pad((WindowHeight - 2), (Columns / 2) - 1);
               for I in GameStats.FinishedMissions.Iterate loop
                  Move_Cursor
                    (Win => DetailedStatsPad,
                     Line =>
                       Line_Position(Statistics_Container.To_Index(I)) - 1,
                     Column => 0);
                  case Missions_Types'Val
                    (Integer'Value
                       (To_String(GameStats.FinishedMissions(I).Index))) is
                     when Deliver =>
                        Add
                          (Win => DetailedStatsPad,
                           Str =>
                             "Delivered items:" &
                             Positive'Image
                               (GameStats.FinishedMissions(I).Amount));
                     when Patrol =>
                        Add
                          (Win => DetailedStatsPad,
                           Str =>
                             "Patroled areas:" &
                             Positive'Image
                               (GameStats.FinishedMissions(I).Amount));
                     when Destroy =>
                        Add
                          (Win => DetailedStatsPad,
                           Str =>
                             "Destroyed ships:" &
                             Positive'Image
                               (GameStats.FinishedMissions(I).Amount));
                     when Explore =>
                        Add
                          (Win => DetailedStatsPad,
                           Str =>
                             "Explored areas:" &
                             Positive'Image
                               (GameStats.FinishedMissions(I).Amount));
                     when Passenger =>
                        Add
                          (Win => DetailedStatsPad,
                           Str =>
                             "Passengers transported:" &
                             Positive'Image
                               (GameStats.FinishedMissions(I).Amount));
                  end case;
               end loop;
            else
               DetailedStatsPad := New_Pad(1, (Columns / 2) - 1);
               Add(Win => DetailedStatsPad, Str => "No finished missions yet");
            end if;
         elsif StatsType = "crafting orders" then
            if GameStats.CraftingOrders.Length > 0 then
               WindowHeight :=
                 Line_Position(GameStats.CraftingOrders.Length) + 2;
               EndIndex3 :=
                 Integer(GameStats.CraftingOrders.Length) - Integer(Lines - 2);
               DetailedStatsPad :=
                 New_Pad((WindowHeight - 2), (Columns / 2) - 1);
               for I in GameStats.CraftingOrders.Iterate loop
                  Move_Cursor
                    (Win => DetailedStatsPad,
                     Line =>
                       Line_Position(Statistics_Container.To_Index(I)) - 1,
                     Column => 0);
                  ItemIndex :=
                    Recipes_List(FindRecipe(GameStats.CraftingOrders(I).Index))
                      .ResultIndex;
                  Add
                    (Win => DetailedStatsPad,
                     Str =>
                       To_String(Items_List(ItemIndex).Name) &
                       ":" &
                       Positive'Image(GameStats.CraftingOrders(I).Amount));
               end loop;
            else
               DetailedStatsPad := New_Pad(1, (Columns / 2) - 1);
               Add
                 (Win => DetailedStatsPad,
                  Str => "No finished crafting orders yet");
            end if;
         end if;
         BoxWindow :=
           Create(WindowHeight, (Columns / 2), (Lines / 4), (Columns / 4));
         Box(BoxWindow);
         Move_Cursor(Win => BoxWindow, Line => 0, Column => 2);
         Add(Win => BoxWindow, Str => "[Finished " & StatsType & "]");
         Refresh;
         Refresh(BoxWindow);
         Delete(BoxWindow);
      end if;
      Refresh
        (DetailedStatsPad,
         Line_Position(StartIndex3),
         0,
         (Lines / 4) + 1,
         (Columns / 4) + 1,
         (Lines - 1),
         (Columns - (Columns / 4) - 3));
   end ShowDetailedStats;

   function ShowGameStatsKeys(Key: Key_Code) return GameStates is
   begin
      case Key is
         when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
            if PlayerShip.Crew(1).Health = 0 then -- Player is dead
               EndGame(False);
               Erase;
               Refresh;
               ShowMainMenu;
               return Main_Menu;
            end if;
            StartIndex := 0;
            StartIndex2 := 0;
            StartIndex3 := 0;
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when 56 | KEY_UP => -- Scroll destroyed ships list one line up
            StartIndex := StartIndex - 1;
         when 50 | KEY_DOWN => -- Scroll destroyed ships list one line down
            StartIndex := StartIndex + 1;
         when 51 | KEY_NPAGE => -- Scroll finished goals list one line down
            StartIndex2 := StartIndex2 + 1;
         when 57 | KEY_PPAGE => -- Scroll finished goals list one line up
            StartIndex2 := StartIndex2 - 1;
         when 55 | Key_Home => -- Scroll destroyed ship list to start
            StartIndex := 0;
         when 49 | Key_End => -- Scroll destroyed ship list to end
            StartIndex := EndIndex;
         when Character'Pos('c') |
           Character'Pos('C') => -- Set new current goal
            ShowGoalsTypes;
            return GoalsTypes_View;
         when Key_F1 => -- Show help
            Erase;
            ShowGameHeader(Help_Topic);
            ShowHelp(GameStats_View, 10);
            return Help_Topic;
         when Character'Pos('m') |
           Character'Pos('M') => -- Show details about finished missions
            ShowDetailedStats("missions", False);
            return DetailedStats_View;
         when Character'Pos('r') |
           Character'Pos('R') => -- Show details about finished crafting orders
            ShowDetailedStats("crafting orders", False);
            return DetailedStats_View;
         when others =>
            null;
      end case;
      if StartIndex < 0 then
         StartIndex := 0;
      end if;
      if StartIndex > EndIndex then
         StartIndex := EndIndex;
      end if;
      if StartIndex2 < 0 then
         StartIndex2 := 0;
      end if;
      if StartIndex2 > EndIndex2 then
         StartIndex2 := EndIndex2;
      end if;
      ShowGameStats(True);
      return GameStats_View;
   end ShowGameStatsKeys;

   function DetailedStatsKeys(Key: Key_Code) return GameStates is
   begin
      case Key is
         when 56 | KEY_UP => -- Scroll detailed stats list one line up
            StartIndex3 := StartIndex3 - 1;
         when 50 | KEY_DOWN => -- Scroll detailed stats list one line down
            StartIndex3 := StartIndex3 + 1;
         when others => -- Back to game statistics
            StartIndex3 := 0;
            DrawGame(GameStats_View);
            return GameStats_View;
      end case;
      if StartIndex3 < 0 then
         StartIndex3 := 0;
      end if;
      if StartIndex3 > EndIndex3 then
         StartIndex3 := EndIndex3;
      end if;
      ShowDetailedStats("", True);
      return DetailedStats_View;
   end DetailedStatsKeys;

end Statistics.UI;
