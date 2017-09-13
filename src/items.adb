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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Log; use Log;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Ships.Cargo; use Ships.Cargo;
with Crew; use Crew;
with Utils; use Utils;

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
      if not Exists(To_String(DataDirectory) & "items" & Dir_Separator) then
         raise Items_Directory_Not_Found;
      end if;
      Start_Search
        (Files,
         To_String(DataDirectory) & "items" & Dir_Separator,
         "*.dat");
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
            Description => Null_Unbounded_String,
            Index => Null_Unbounded_String);
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
                  if TempRecord.Index = MoneyIndex then
                     MoneyName := Value;
                  end if;
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
            else
               if TempRecord.Name /= Null_Unbounded_String then
                  Items_List.Append(New_Item => TempRecord);
                  LogMessage
                    ("Item added: " & To_String(TempRecord.Name),
                     Everything);
                  TempRecord :=
                    (Name => Null_Unbounded_String,
                     Weight => 1,
                     IType => Null_Unbounded_String,
                     Prices => (0, 0, 0, 0),
                     Buyable => (False, False, False, False),
                     Value => 0,
                     ShowType => Null_Unbounded_String,
                     Description => Null_Unbounded_String,
                     Index => Null_Unbounded_String);
               end if;
               if Length(RawData) > 2 then
                  TempRecord.Index :=
                    Unbounded_Slice(RawData, 2, (Length(RawData) - 1));
               end if;
            end if;
         end loop;
         Close(ItemsFile);
      end loop;
      End_Search(Files);
   end LoadItems;

   function FindProtoItem
     (Index,
      ItemType: Unbounded_String :=
        Null_Unbounded_String)
      return Natural is
   begin
      if Index /= Null_Unbounded_String then
         for I in Items_List.Iterate loop
            if Items_List(I).Index = Index then
               return Objects_Container.To_Index(I);
            end if;
         end loop;
      elsif ItemType /= Null_Unbounded_String then
         for I in Items_List.Iterate loop
            if Items_List(I).IType = ItemType then
               return Objects_Container.To_Index(I);
            end if;
         end loop;
      end if;
      return 0;
   end FindProtoItem;

   function GetItemName
     (ItemIndex: Positive;
      MemberIndex: Natural := 0) return String is
   begin
      if MemberIndex = 0 then
         if PlayerShip.Cargo(ItemIndex).Name /= Null_Unbounded_String then
            return To_String(PlayerShip.Cargo(ItemIndex).Name);
         else
            return To_String
                (Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Name);
         end if;
      else
         if PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Name /=
           Null_Unbounded_String then
            return To_String
                (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Name);
         else
            return To_String
                (Items_List
                   (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex)
                      .ProtoIndex)
                   .Name);
         end if;
      end if;
   end GetItemName;

   procedure DamageItem
     (CargoIndex: Positive;
      CrewIndex, SkillIndex: Natural := 0) is
      SelectedItem: InventoryData := PlayerShip.Cargo(CargoIndex);
      DamageChance: Integer := Items_List(SelectedItem.ProtoIndex).Value;
      I, LastIndex: Natural;
   begin
      if CrewIndex > 0 then
         if SkillIndex > 0 then
            DamageChance :=
              DamageChance - (GetSkillLevel(CrewIndex, SkillIndex) / 5);
            if DamageChance < 0 then
               DamageChance := 0;
            end if;
         end if;
         SelectedItem := PlayerShip.Crew(CrewIndex).Inventory(CargoIndex);
         I := PlayerShip.Crew(CrewIndex).Inventory.First_Index;
         LastIndex := PlayerShip.Crew(CrewIndex).Inventory.Last_Index;
      else
         I := PlayerShip.Cargo.First_Index;
         LastIndex := PlayerShip.Cargo.Last_Index;
      end if;
      if GetRandom(1, 100) > DamageChance then -- Cargo not damaged
         return;
      end if;
      if SelectedItem.Amount > 1 then
         if CrewIndex = 0 then
            PlayerShip.Cargo.Append
            (New_Item =>
               (ProtoIndex => SelectedItem.ProtoIndex,
                Amount => (SelectedItem.Amount - 1),
                Name => SelectedItem.Name,
                Durability => SelectedItem.Durability));
         else
            PlayerShip.Crew(CrewIndex).Inventory.Append
            (New_Item =>
               (ProtoIndex => SelectedItem.ProtoIndex,
                Amount => (SelectedItem.Amount - 1),
                Name => SelectedItem.Name,
                Durability => SelectedItem.Durability));
         end if;
         SelectedItem.Amount := 1;
      end if;
      SelectedItem.Durability := SelectedItem.Durability - 1;
      if SelectedItem.Durability = 0 then -- Item destroyed
         if CrewIndex = 0 then
            UpdateCargo
              (Ship => PlayerShip,
               CargoIndex => CargoIndex,
               Amount => -1);
         else
            UpdateInventory(CrewIndex, SelectedItem.ProtoIndex, -1);
         end if;
         return;
      end if;
      if CrewIndex = 0 then
         PlayerShip.Cargo(CargoIndex) := SelectedItem;
      else
         PlayerShip.Crew(CrewIndex).Inventory(CargoIndex) := SelectedItem;
      end if;
      while I <= LastIndex loop
         if CrewIndex = 0 then
            for J in
              PlayerShip.Cargo.First_Index .. PlayerShip.Cargo.Last_Index loop
               if PlayerShip.Cargo(I).ProtoIndex =
                 PlayerShip.Cargo(J).ProtoIndex and
                 PlayerShip.Cargo(I).Durability =
                   PlayerShip.Cargo(J).Durability and
                 I /= J then
                  UpdateCargo
                    (Ship => PlayerShip,
                     CargoIndex => J,
                     Amount => (0 - PlayerShip.Cargo.Element(J).Amount));
                  UpdateCargo
                    (Ship => PlayerShip,
                     CargoIndex => I,
                     Amount => PlayerShip.Cargo(J).Amount);
                  I := I - 1;
                  exit;
               end if;
            end loop;
         else
            for J in
              PlayerShip.Crew(CrewIndex).Inventory.First_Index ..
                  PlayerShip.Crew(CrewIndex).Inventory.Last_Index loop
               if PlayerShip.Crew(CrewIndex).Inventory(I).ProtoIndex =
                 PlayerShip.Crew(CrewIndex).Inventory(J).ProtoIndex and
                 PlayerShip.Crew(CrewIndex).Inventory(I).Durability =
                   PlayerShip.Crew(CrewIndex).Inventory(J).Durability and
                 I /= J then
                  UpdateInventory
                    (CrewIndex,
                     J,
                     (0 -
                      PlayerShip.Crew(CrewIndex).Inventory.Element(J).Amount));
                  UpdateInventory
                    (CrewIndex,
                     I,
                     PlayerShip.Crew(CrewIndex).Inventory(J).Amount);
                  I := I - 1;
                  exit;
               end if;
            end loop;
         end if;
         I := I + 1;
      end loop;
   end DamageItem;

end Items;
