--    Copyright 2017-2018 Bartek thindil Jasicki
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ships; use Ships;
with Statistics; use Statistics;

package body DeathScreen is

   procedure ShowDeathScreen is
      StartLine: Line_Position := Lines / 5;
      StartColumn: Column_Position := Columns / 4;
      PlateText: String :=
        "|                                                <";
      Name: constant String :=
        To_Upper(To_String(PlayerShip.Crew(1).Name)) &
        " score:" &
        Natural'Image(GameStats.Points);
   begin
      if (Lines / 5) + 23 >= Lines then
         StartLine := 0;
      end if;
      if (Columns / 4) + 49 >= Columns then
         StartColumn := 0;
      end if;
      PlateText :=
        Overwrite(PlateText, (PlateText'Length / 2) - (Name'Length / 2), Name);
      Move_Cursor(Line => StartLine, Column => StartColumn + 18);
      Add(Str => "_____  _____");
      Move_Cursor(Line => StartLine + 1, Column => StartColumn + 17);
      Add(Str => "<     `/     |");
      Move_Cursor(Line => StartLine + 2, Column => StartColumn + 17);
      Add(Str => ">          (");
      Move_Cursor(Line => StartLine + 3, Column => StartColumn + 17);
      Add(Str => "|   _     _  |");
      Move_Cursor(Line => StartLine + 4, Column => StartColumn + 17);
      Add(Str => "|  |_) | |_) |");
      Move_Cursor(Line => StartLine + 5, Column => StartColumn + 17);
      Add(Str => "|  | \ | |   |");
      Move_Cursor(Line => StartLine + 6, Column => StartColumn + 17);
      Add(Str => "|            |");
      Move_Cursor(Line => StartLine + 7, Column => StartColumn + 2);
      Add(Str => "______.______%_|            |__________  _____");
      Move_Cursor(Line => StartLine + 8, Column => StartColumn);
      Add(Str => "_/                                       \|     |");
      Move_Cursor(Line => StartLine + 9, Column => StartColumn);
      Add(Str => PlateText);
      Move_Cursor(Line => StartLine + 10, Column => StartColumn);
      Add(Str => "|_____.-._________              ____/|___________|");
      Move_Cursor(Line => StartLine + 11, Column => StartColumn + 18);
      Add(Str => "|            |");
      Move_Cursor(Line => StartLine + 12, Column => StartColumn + 18);
      Add(Str => "|            |");
      Move_Cursor(Line => StartLine + 13, Column => StartColumn + 18);
      Add(Str => "|            |");
      Move_Cursor(Line => StartLine + 14, Column => StartColumn + 18);
      Add(Str => "|            |");
      Move_Cursor(Line => StartLine + 15, Column => StartColumn + 18);
      Add(Str => "|   _        <");
      Move_Cursor(Line => StartLine + 16, Column => StartColumn + 18);
      Add(Str => "|__/         |");
      Move_Cursor(Line => StartLine + 17, Column => StartColumn + 18);
      Add(Str => " / `--.      |");
      Move_Cursor(Line => StartLine + 18, Column => StartColumn + 17);
      Add(Str => "%|            |%");
      Move_Cursor(Line => StartLine + 19, Column => StartColumn + 13);
      Add(Str => "|/.%%|          -< @%%%");
      Move_Cursor(Line => StartLine + 20, Column => StartColumn + 13);
      Add(Str => "`\%`@|     v      |@@%@%%");
      Move_Cursor(Line => StartLine + 21, Column => StartColumn + 11);
      Add(Str => ".%%%@@@|%    |    % @@@%%@%%%%");
      Move_Cursor(Line => StartLine + 22, Column => StartColumn + 6);
      Add(Str => "_.%%%%%%@@@@@@%%_/%\_%@@%%@@@@@@@%%%%%%");
      Move_Cursor(Line => StartLine + 23, Column => StartColumn);
      Add(Str => "Would you like to see your game statistics? (Y/N)");
   end ShowDeathScreen;

end DeathScreen;
