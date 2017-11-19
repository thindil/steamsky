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
with Log; use Log;
with Game; use Game;
with Items; use Items;

package body Mobs is

   procedure LoadMobs is
      MobsFile: File_Type;
      RawData, FieldName, Value, EquipmentSlot: Unbounded_String;
      EqualIndex,
      StartIndex,
      EndIndex,
      Amount,
      XIndex,
      DotIndex,
      ItemIndex: Natural;
      TempRecord: ProtoMobRecord;
      TempSkills, TempInventory: Skills_Container.Vector;
      TempAttributes: Attributes_Container.Vector;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
      TempPriorities: Orders_Array := (others => 0);
      TempEquipment: Equipment_Array := (others => 0);
      OrdersNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("Piloting"),
         To_Unbounded_String("Engineering"),
         To_Unbounded_String("Operating guns"),
         To_Unbounded_String("Repair ship"),
         To_Unbounded_String("Manufacturing"),
         To_Unbounded_String("Upgrading ship"),
         To_Unbounded_String("Talking in bases"),
         To_Unbounded_String("Healing wounded"),
         To_Unbounded_String("Cleaning ship"));
   begin
      if ProtoMobs_List.Length > 0 then
         return;
      end if;
      if not Exists(To_String(DataDirectory) & "mobs" & Dir_Separator) then
         raise Mobs_Directory_Not_Found;
      end if;
      Start_Search
        (Files,
         To_String(DataDirectory) & "mobs" & Dir_Separator,
         "*.dat");
      if not More_Entries(Files) then
         raise Mobs_Files_Not_Found;
      end if;
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         TempRecord :=
           (Index => Null_Unbounded_String,
            Skills => TempSkills,
            Attributes => TempAttributes,
            Order => Rest,
            Priorities => TempPriorities,
            Inventory => TempInventory,
            Equipment => TempEquipment);
         LogMessage("Loading mobs file: " & Full_Name(FoundFile), Everything);
         Open(MobsFile, In_File, Full_Name(FoundFile));
         while not End_Of_File(MobsFile) loop
            RawData := To_Unbounded_String(Get_Line(MobsFile));
            if Element(RawData, 1) /= '[' then
               EqualIndex := Index(RawData, "=");
               FieldName := Head(RawData, EqualIndex - 2);
               Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
               if FieldName = To_Unbounded_String("Skills") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                  for I in 1 .. Amount loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     TempSkills.Append
                     (New_Item =>
                        (FindSkillIndex
                           (Unbounded_Slice(Value, StartIndex, EndIndex - 1)),
                         0,
                         0));
                     StartIndex := EndIndex + 2;
                  end loop;
               elsif FieldName = To_Unbounded_String("SkillsLevels") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                  for I in 1 .. Amount loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     DotIndex := Index(Value, "..", StartIndex);
                     if DotIndex = 0 or DotIndex > EndIndex then
                        TempSkills(I)(2) :=
                          Integer'Value
                            (Slice(Value, StartIndex, EndIndex - 1));
                        TempSkills(I)(3) := 0;
                     else
                        TempSkills(I)(2) :=
                          Integer'Value
                            (Slice(Value, StartIndex, DotIndex - 1));
                        TempSkills(I)(3) :=
                          Integer'Value
                            (Slice(Value, DotIndex + 2, EndIndex - 1));
                     end if;
                     StartIndex := EndIndex + 2;
                  end loop;
                  TempRecord.Skills := TempSkills;
                  TempSkills.Clear;
               elsif FieldName = To_Unbounded_String("Attributes") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                  for J in 1 .. Amount loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     DotIndex := Index(Value, "..", StartIndex);
                     if DotIndex = 0 or DotIndex > EndIndex then
                        TempAttributes.Append
                        (New_Item =>
                           (Integer'Value
                              (Slice(Value, StartIndex, EndIndex - 1)),
                            0));
                     else
                        TempAttributes.Append
                        (New_Item =>
                           (Integer'Value
                              (Slice(Value, StartIndex, DotIndex - 1)),
                            Integer'Value
                              (Slice(Value, DotIndex + 2, EndIndex - 1))));
                     end if;
                     StartIndex := EndIndex + 2;
                  end loop;
                  TempRecord.Attributes := TempAttributes;
                  TempAttributes.Clear;
                  StartIndex := EndIndex + 2;
               elsif FieldName = To_Unbounded_String("Order") then
                  TempRecord.Order := Crew_Orders'Value(To_String(Value));
               elsif FieldName = To_Unbounded_String("Priorities") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                  for J in 1 .. Amount loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     XIndex := Index(Value, ":", StartIndex);
                     for K in OrdersNames'Range loop
                        if OrdersNames(K) =
                          Unbounded_Slice(Value, StartIndex, XIndex - 1) then
                           if Slice(Value, XIndex + 1, EndIndex - 1) =
                             "Normal" then
                              TempPriorities(K) := 1;
                           else
                              TempPriorities(K) := 2;
                           end if;
                           exit;
                        end if;
                     end loop;
                     StartIndex := EndIndex + 2;
                  end loop;
                  TempRecord.Priorities := TempPriorities;
                  TempPriorities := (others => 0);
               elsif FieldName = To_Unbounded_String("Inventory") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                  for I in 1 .. Amount loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     XIndex := Index(Value, "x", StartIndex);
                     DotIndex := Index(Value, "..", StartIndex);
                     ItemIndex :=
                       FindProtoItem
                         (Unbounded_Slice(Value, XIndex + 1, EndIndex - 1));
                     if ItemIndex = 0 then
                        Close(MobsFile);
                        End_Search(Files);
                        raise Mobs_Invalid_Data
                          with "Invalid item index: |" &
                          Slice(Value, XIndex + 1, EndIndex - 1) &
                          "| in " &
                          To_String(TempRecord.Index) &
                          ".";
                     end if;
                     if DotIndex = 0 or DotIndex > EndIndex then
                        TempRecord.Inventory.Append
                        (New_Item =>
                           (ItemIndex,
                            Integer'Value
                              (Slice(Value, StartIndex, XIndex - 1)),
                            0));
                     else
                        TempRecord.Inventory.Append
                        (New_Item =>
                           (ItemIndex,
                            Integer'Value
                              (Slice(Value, StartIndex, DotIndex - 1)),
                            Integer'Value
                              (Slice(Value, DotIndex + 2, XIndex - 1))));
                     end if;
                     StartIndex := EndIndex + 2;
                  end loop;
               elsif FieldName = To_Unbounded_String("Equipment") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                  for J in 1 .. Amount loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     XIndex := Index(Value, ":", StartIndex);
                     EquipmentSlot :=
                       Unbounded_Slice(Value, StartIndex, XIndex - 1);
                     if EquipmentSlot = "Weapon" then
                        TempEquipment(1) :=
                          Positive'Value
                            (Slice(Value, XIndex + 1, EndIndex - 1));
                     elsif EquipmentSlot = "Shield" then
                        TempEquipment(2) :=
                          Positive'Value
                            (Slice(Value, XIndex + 1, EndIndex - 1));
                     elsif EquipmentSlot = "Head" then
                        TempEquipment(3) :=
                          Positive'Value
                            (Slice(Value, XIndex + 1, EndIndex - 1));
                     elsif EquipmentSlot = "Torso" then
                        TempEquipment(4) :=
                          Positive'Value
                            (Slice(Value, XIndex + 1, EndIndex - 1));
                     elsif EquipmentSlot = "Arms" then
                        TempEquipment(5) :=
                          Positive'Value
                            (Slice(Value, XIndex + 1, EndIndex - 1));
                     elsif EquipmentSlot = "Legs" then
                        TempEquipment(6) :=
                          Positive'Value
                            (Slice(Value, XIndex + 1, EndIndex - 1));
                     elsif EquipmentSlot = "Tool" then
                        TempEquipment(7) :=
                          Positive'Value
                            (Slice(Value, XIndex + 1, EndIndex - 1));
                     end if;
                     StartIndex := EndIndex + 2;
                  end loop;
                  TempRecord.Equipment := TempEquipment;
                  TempEquipment := (others => 0);
               end if;
            else
               if TempRecord.Index /= Null_Unbounded_String then
                  ProtoMobs_List.Append(New_Item => TempRecord);
                  LogMessage
                    ("Mob added: " & To_String(TempRecord.Index),
                     Everything);
                  TempRecord :=
                    (Index => Null_Unbounded_String,
                     Skills => TempSkills,
                     Attributes => TempAttributes,
                     Order => Rest,
                     Priorities => TempPriorities,
                     Inventory => TempInventory,
                     Equipment => TempEquipment);
               end if;
               if Length(RawData) > 2 then
                  TempRecord.Index :=
                    Unbounded_Slice(RawData, 2, (Length(RawData) - 1));
               end if;
            end if;
         end loop;
         Close(MobsFile);
      end loop;
      End_Search(Files);
   end LoadMobs;

   function FindProtoMob(Index: Unbounded_String) return Natural is
   begin
      for I in ProtoMobs_List.Iterate loop
         if ProtoMobs_List(I).Index = Index then
            return ProtoMobs_Container.To_Index(I);
         end if;
      end loop;
      return 0;
   end FindProtoMob;

end Mobs;
