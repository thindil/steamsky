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

package body ShipModules is

    procedure LoadModules is
        ModulesFile : File_Type;
        RawData, FieldName, Value : Unbounded_String;
        EqualIndex, StartIndex, EndIndex : Natural;
        TempRecord : BaseModule_Data;
    begin
        if Modules_List.Length > 0 then
            return;
        end if;
        TempRecord := (Name => Null_Unbounded_String, MType => ENGINE, 
            Weight => 0, Value => 0, MaxValue => 0, Durability => 0);
        Open(ModulesFile, In_File, "data/shipmodules.dat");
        while not End_Of_File(ModulesFile) loop
            RawData := To_Unbounded_String(Get_Line(ModulesFile));
            if Element(RawData, 1) /= '[' then
                null;
            elsif TempRecord.Name /= Null_Unbounded_String then
                Modules_List.Append(New_Item => TempRecord);
                TempRecord := (Name => Null_Unbounded_String, MType => ENGINE, 
                    Weight => 0, Value => 0, MaxValue => 0, Durability => 0);
            end if;
        end loop;
        Close(ModulesFile);

    end LoadModules;

end ShipModules;
