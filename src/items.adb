--    Copyright 2016-2017 Bartek thindil Jasicki
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
with Log; use Log;

package body Items is

   procedure LoadItems is
      ItemsFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex, StartIndex, EndIndex: Natural;
      TempRecord: Object_Data;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
   begin
      if Items_List.Length > 0 then
         return;
      end if;
      if not Exists("data/items/") then
         raise Items_Directory_Not_Found;
      end if;
      Start_Search(Files, "data/items/", "*.dat");
      if not More_Entries(Files) then
         raise Items_Files_Not_Found;
      end if;
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         TempRecord :=
           (Name => Null_Unbounded_String,
            Weight => 1,
            IType => Null_Unbounded_String,
            Prices => (0, 0, 0, 0),
            Buyable => (False, False, False, False),
            Value => 0,
            ShowType => Null_Unbounded_String,
            Description => Null_Unbounded_String);
         LogMessage("Loading item file: " & Full_Name(FoundFile), Everything);
         Open(ItemsFile, In_File, Full_Name(FoundFile));
         while not End_Of_File(ItemsFile) loop
            RawData := To_Unbounded_String(Get_Line(ItemsFile));
            if Element(RawData, 1) /= '[' then
               EqualIndex := Index(RawData, "=");
               FieldName := Head(RawData, EqualIndex - 2);
               Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
               if FieldName = To_Unbounded_String("Name") then
                  TempRecord.Name := Value;
               elsif FieldName = To_Unbounded_String("Weight") then
                  TempRecord.Weight := Integer'Value(To_String(Value));
               elsif FieldName = To_Unbounded_String("Type") then
                  TempRecord.IType := Value;
               elsif FieldName = To_Unbounded_String("Prices") then
                  StartIndex := 1;
                  for I in TempRecord.Prices'Range loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     TempRecord.Prices(I) :=
                       Integer'Value(Slice(Value, StartIndex, EndIndex - 1));
                     StartIndex := EndIndex + 2;
                  end loop;
               elsif FieldName = To_Unbounded_String("Buyable") then
                  StartIndex := 1;
                  for I in TempRecord.Prices'Range loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     if Slice(Value, StartIndex, EndIndex - 1) = "Y" then
                        TempRecord.Buyable(I) := True;
                     else
                        TempRecord.Buyable(I) := False;
                     end if;
                     StartIndex := EndIndex + 2;
                  end loop;
               elsif FieldName = To_Unbounded_String("Value") then
                  TempRecord.Value := Integer'Value(To_String(Value));
               elsif FieldName = To_Unbounded_String("ShowType") then
                  TempRecord.ShowType := Value;
               elsif FieldName = To_Unbounded_String("Description") then
                  TempRecord.Description := Value;
               end if;
            elsif TempRecord.Name /= Null_Unbounded_String then
               Items_List.Append(New_Item => TempRecord);
               TempRecord :=
                 (Name => Null_Unbounded_String,
                  Weight => 1,
                  IType => Null_Unbounded_String,
                  Prices => (0, 0, 0, 0),
                  Buyable => (False, False, False, False),
                  Value => 0,
                  ShowType => Null_Unbounded_String,
                  Description => Null_Unbounded_String);
            end if;
         end loop;
         Close(ItemsFile);
      end loop;
      End_Search(Files);
   end LoadItems;

end Items;
