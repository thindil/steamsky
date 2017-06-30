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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with GNAT.String_Split; use GNAT.String_Split;
with Game; use Game;
with Statistics; use Statistics;

package body HallOfFame is

   procedure LoadHallOfFame is
      HoFFile: File_Type;
      Index: Positive := 1;
      SubStrings: Slice_Set;
   begin
      if HallOfFame_Array(1).Name /= Null_Unbounded_String then
         return;
      end if;
      if not Exists(To_String(SaveDirectory) & "halloffame.dat") then
         return;
      end if;
      Open(HoFFile, In_File, To_String(SaveDirectory) & "halloffame.dat");
      while not End_Of_File(HoFFile) loop
         Create
           (S => SubStrings,
            From => Get_Line(HoFFile),
            Separators => ";",
            Mode => GNAT.String_Split.Single);
         HallOfFame_Array(Index) :=
           (Name => To_Unbounded_String(Slice(SubStrings, 1)),
            Points => Natural'Value(Slice(SubStrings, 2)),
            DeathReason => To_Unbounded_String(Slice(SubStrings, 3)));
         Index := Index + 1;
      end loop;
      Close(HoFFile);
   end LoadHallOfFame;

   procedure UpdateHallOfFame(PlayerName, DeathReason: Unbounded_String) is
      NewIndex: Natural := 0;
      HoFFile: File_Type;
   begin
      for I in HallOfFame_Array'Range loop
         if HallOfFame_Array(I).Points < GameStats.Points then
            NewIndex := I;
            exit;
         end if;
      end loop;
      if NewIndex = 0 then
         return;
      end if;
      for I in reverse NewIndex .. 9 loop
         HallOfFame_Array(I + 1) := HallOfFame_Array(I);
      end loop;
      HallOfFame_Array(NewIndex) :=
        (Name => PlayerName,
         Points => GameStats.Points,
         DeathReason => DeathReason);
      Create
        (HoFFile,
         Append_File,
         To_String(SaveDirectory) & "halloffame.dat");
      for I in HallOfFame_Array'Range loop
         if HallOfFame_Array(I).Name = Null_Unbounded_String then
            exit;
         end if;
         Put_Line
           (HoFFile,
            To_String(HallOfFame_Array(I).Name) &
            ";" &
            Natural'Image(HallOfFame_Array(I).Points) &
            ";" &
            To_String(HallOfFame_Array(I).DeathReason));
      end loop;
      Close(HoFFile);
   end UpdateHallOfFame;

end HallOfFame;
