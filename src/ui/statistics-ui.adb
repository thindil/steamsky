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

with Ada.Directories; use Ada.Directories;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Events; use Events;
with Messages; use Messages;
with MainMenu; use MainMenu;
with Crafts; use Crafts;
with Goals; use Goals;
with Goals.UI; use Goals.UI;

package body Statistics.UI is

   DestroyedShipsPad, FinishedGoalsPad: Window;
   StartIndex, StartIndex2, EndIndex, EndIndex2: Integer := 0;

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
         Move_Cursor(Line => 6, Column => 2);
         Add
           (Str =>
              "Crafting orders finished:" &
              Natural'Image(GameStats.CraftingOrders));
         if GameStats.AcceptedMissions > 0 then
            MissionsPercent :=
              Natural
                ((Float(GameStats.FinishedMissions) /
                  Float(GameStats.AcceptedMissions)) *
                 100.0);
         end if;
         Move_Cursor(Line => 7, Column => 2);
         Add
           (Str =>
              "Missions finished:" &
              Natural'Image(GameStats.FinishedMissions) &
              " (" &
              To_String
                (Trim
                   (To_Unbounded_String(Natural'Image(MissionsPercent)),
                    Ada.Strings.Left)) &
              "%)");
         Move_Cursor(Line => 8, Column => 2);
         Add(Str => "Current goal: " & GoalText(0));
         Change_Attributes(Line => 8, Column => 2, Count => 1, Color => 1);
         if GameStats.FinishedGoals.Length > 0 then
            FinishedGoalsPad :=
              New_Pad
                (Line_Position(GameStats.FinishedGoals.Length + 2),
                 (Columns / 2));
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
         (Columns / 2));
   end ShowGameStats;

   function ShowGameStatsKeys(Key: Key_Code) return GameStates is
   begin
      case Key is
         when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
            if PlayerShip.Crew(1).Health = 0 then -- Player is dead
               if Exists("data/savegame.dat") then
                  Delete_File("data/savegame.dat");
               end if;
               ClearMessages;
               Events_List.Clear;
               ClearGameStats;
               Known_Recipes.Clear;
               ClearCurrentGoal;
               Erase;
               Refresh;
               ShowMainMenu;
               return Main_Menu;
            end if;
            StartIndex := 0;
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

end Statistics.UI;
