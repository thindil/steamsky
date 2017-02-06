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
with Game; use Game;

package body ShipModules is

    function LoadShipModules return Boolean is
        ModulesFile : File_Type;
        RawData, FieldName, Value : Unbounded_String;
        EqualIndex : Natural;
        TempRecord : BaseModule_Data;
    begin
        if Modules_List.Length > 0 then
            return True;
        end if;
        if not Exists("data/shipmodules.dat") then
            return False;
        end if;
        TempRecord := (Name => Null_Unbounded_String, MType => ENGINE, 
            Weight => 0, Value => 0, MaxValue => 0, Durability => 0,
            RepairMaterial => Null_Unbounded_String, RepairSkill => 2, Price => 0, 
            InstallTime => 60, Unique => False, Size => 0, Description => Null_Unbounded_String);
        Open(ModulesFile, In_File, "data/shipmodules.dat");
        while not End_Of_File(ModulesFile) loop
            RawData := To_Unbounded_String(Get_Line(ModulesFile));
            if Element(RawData, 1) /= '[' then
                EqualIndex := Index(RawData, "=");
                FieldName := Head(RawData, EqualIndex - 2);
                Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
                if FieldName = To_Unbounded_String("Name") then
                    TempRecord.Name := Value;
                elsif FieldName = To_Unbounded_String("Type") then
                    TempRecord.MType := ModuleType'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Weight") then
                    TempRecord.Weight := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Value") then
                    TempRecord.Value := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("MaxValue") then
                    TempRecord.MaxValue := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Durability") then
                    TempRecord.Durability := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Material") then
                    TempRecord.RepairMaterial := Value;
                elsif FieldName = To_Unbounded_String("Skill") then
                    for I in Skills_Names.First_Index..Skills_Names.Last_Index loop
                        if Value = To_String(Skills_Names.Element(I)) then
                            TempRecord.RepairSkill := I;
                            exit;
                        end if;
                    end loop;
                elsif FieldName = To_Unbounded_String("Price") then
                    TempRecord.Price := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("InstallTime") then
                    TempRecord.InstallTime := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Unique") then
                    if Value = To_Unbounded_String("Yes") then
                        TempRecord.Unique := True;
                    else
                        TempRecord.Unique := False;
                    end if;
                elsif FieldName = To_Unbounded_String("Size") then
                    TempRecord.Size := Integer'Value(To_String(Value));
                elsif FieldName = To_Unbounded_String("Description") then
                    TempRecord.Description := Value;
                end if;
            elsif TempRecord.Name /= Null_Unbounded_String then
                Modules_List.Append(New_Item => TempRecord);
                TempRecord := (Name => Null_Unbounded_String, MType => ENGINE, 
                    Weight => 0, Value => 0, MaxValue => 0, Durability => 0,
                    RepairMaterial => Null_Unbounded_String, RepairSkill => 2, Price => 0,
                    InstallTime => 60, Unique => False, Size => 0, Description => Null_Unbounded_String);
            end if;
        end loop;
        Close(ModulesFile);
        return True;
    end LoadShipModules;

end ShipModules;
