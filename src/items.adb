--    Copyright 2016 Bartek thindil Jasicki
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

package body Items is

    function LoadItems return Boolean is
        ItemsFile : File_Type;
        RawData, FieldName, Value : Unbounded_String;
        EqualIndex, StartIndex, EndIndex : Natural;
        TempRecord : Object_Data;
    begin
        if Items_List.Length > 0 then
            return True;
        end if;
        if not Exists("data/items.dat") then
            return False;
        end if;
        TempRecord := (Name => Null_Unbounded_String, Weight => 1,
            IType => Fuel, Prices => (0, 0, 0), Buyable => (False, False,
            False));
        Open(ItemsFile, In_File, "data/items.dat");
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
                    TempRecord.IType := Items_Types'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Prices") then
                    StartIndex := 1;
                    for I in 1..3 loop
                        EndIndex := Index(Value, ", ", StartIndex);
                        if EndIndex = 0 then
                            EndIndex := StartIndex + 1;
                        end if;
                        TempRecord.Prices(I) := Integer'Value(Slice(Value, StartIndex, EndIndex - 1));
                        StartIndex := EndIndex + 2;
                    end loop;
                elsif FieldName = To_Unbounded_String("Buyable") then
                    StartIndex := 1;
                    for I in 1..3 loop
                        EndIndex := Index(Value, ", ", StartIndex);
                        if EndIndex = 0 then
                            EndIndex := StartIndex + 1;
                        end if;
                        if Slice(Value, StartIndex, EndIndex - 1) = "Y" then
                            TempRecord.Buyable(I) := True;
                        else
                            TempRecord.Buyable(I) := False;
                        end if;
                        StartIndex := EndIndex + 2;
                    end loop;
                end if;
            elsif TempRecord.Name /= Null_Unbounded_String then
                Items_List.Append(New_Item => TempRecord);
                TempRecord := (Name => Null_Unbounded_String, Weight => 1,
                    IType => Fuel, Prices => (0, 0, 0), Buyable => (False, False,
                    False));
            end if;
        end loop;
        Close(ItemsFile);
        return True;
    end LoadItems;

end Items;
