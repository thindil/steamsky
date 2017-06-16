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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Game; use Game;
with Log; use Log;

package body Goals is

   procedure LoadGoals is
      GoalsFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex: Natural;
      TempRecord: Goal_Data;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
   begin
      if Goals_List.Length > 0 then
         return;
      end if;
      if not Exists
          (To_String(DataDirectory) & "goals" & Dir_Separator) then
         raise Goals_Directory_Not_Found;
      end if;
      Start_Search
        (Files,
         To_String(DataDirectory) & "goals" & Dir_Separator,
         "*.dat");
      if not More_Entries(Files) then
         raise Goals_Files_Not_Found;
      end if;
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         TempRecord :=
           (Index => Null_Unbounded_String,
           GType => RANDOM,
           Amount => 0,
           TargetIndex => Null_Unbounded_String);
         LogMessage
           ("Loading goals file: " & Full_Name(FoundFile),
            Everything);
         Open(GoalsFile, In_File, Full_Name(FoundFile));
         while not End_Of_File(GoalsFile) loop
            RawData := To_Unbounded_String(Get_Line(GoalsFile));
            if Element(RawData, 1) /= '[' then
               EqualIndex := Index(RawData, "=");
               FieldName := Head(RawData, EqualIndex - 2);
               Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
               if FieldName = To_Unbounded_String("Type") then
                  TempRecord.GType := GoalTypes'Value(To_String(Value));
               elsif FieldName = To_Unbounded_String("Amount") then
                  TempRecord.Amount := Natural'Value(To_String(Value));
               elsif FieldName = To_Unbounded_String("Target") then
                  TempRecord.TargetIndex := Value;
               end if;
            else
               if TempRecord.GType /= RANDOM then
                  LogMessage
                    ("Goal added: " & To_String(TempRecord.Index),
                     Everything);
                  Goals_List.Append(New_Item => TempRecord);
                  TempRecord :=
                     (Index => Null_Unbounded_String,
                     GType => RANDOM,
                     Amount => 0,
                     TargetIndex => Null_Unbounded_String);
               end if;
               if Length(RawData) > 2 then
                  TempRecord.Index :=
                    Unbounded_Slice(RawData, 2, (Length(RawData) - 1));
               end if;
            end if;
         end loop;
         Close(GoalsFile);
      end loop;
      End_Search(Files);
   end LoadGoals;

   procedure UpdateGoal(GType: GoalTypes; Index: Unbounded_String) is
   begin
      null;
   end UpdateGoal;

end Goals;
