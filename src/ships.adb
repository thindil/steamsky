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
with Messages; use Messages;
with Items; use Items;
with ShipModules; use ShipModules;
with Utils; use Utils;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Log; use Log;

package body Ships is

   procedure UpdateModule
     (Ship: in out ShipRecord;
      ModuleIndex: Positive;
      Field: String;
      Value: String) is
      NewDurability,
      NewValue,
      NewMaxDurability,
      NewMaxValue,
      NewUpgradeProgress: Integer;
      NewName: Unbounded_String;
      NewOwner, NewWeight: Natural;
      NewUpgradeAction: ShipUpgrade;
      procedure UpdateMod(Module: in out ModuleData) is
      begin
         if Field = "Durability" then
            Module.Durability := NewDurability;
         elsif Field = "Name" then
            Module.Name := NewName;
         elsif Field = "Current_Value" then
            Module.Current_Value := NewValue;
         elsif Field = "Owner" then
            Module.Owner := NewOwner;
         elsif Field = "MaxDurability" then
            Module.MaxDurability := NewMaxDurability;
         elsif Field = "Max_Value" then
            Module.Max_Value := NewMaxValue;
         elsif Field = "UpgradeProgress" then
            Module.UpgradeProgress := NewUpgradeProgress;
         elsif Field = "UpgradeAction" then
            Module.UpgradeAction := NewUpgradeAction;
         elsif Field = "Weight" then
            Module.Weight := NewWeight;
         end if;
      end UpdateMod;
   begin
      if ModuleIndex > Positive(Ship.Modules.Length) then
         return;
      end if;
      if Field = "Durability" then
         NewDurability :=
           Ship.Modules.Element(ModuleIndex).Durability + Integer'Value(Value);
         if NewDurability < 0 then
            NewDurability := 0;
         end if;
      elsif Field = "Name" then
         NewName := To_Unbounded_String(Value);
      elsif Field = "Current_Value" then
         NewValue := Integer'Value(Value);
      elsif Field = "Owner" then
         NewOwner := Natural'Value(Value);
      elsif Field = "MaxDurability" then
         NewMaxDurability :=
           Ship.Modules.Element(ModuleIndex).MaxDurability +
           Integer'Value(Value);
      elsif Field = "Max_Value" then
         NewMaxValue :=
           Ship.Modules.Element(ModuleIndex).Max_Value + Integer'Value(Value);
      elsif Field = "UpgradeProgress" then
         NewUpgradeProgress := Integer'Value(Value);
      elsif Field = "UpgradeAction" then
         NewUpgradeAction := ShipUpgrade'Value(Value);
      elsif Field = "Weight" then
         NewWeight :=
           Ship.Modules.Element(ModuleIndex).Weight + Natural'Value(Value);
      end if;
      Ship.Modules.Update_Element
      (Index => ModuleIndex, Process => UpdateMod'Access);
   end UpdateModule;

   function CreateShip
     (ProtoIndex: Positive;
      Name: Unbounded_String;
      X, Y: Integer;
      Speed: ShipSpeed) return ShipRecord is
      TmpShip: ShipRecord;
      ShipModules: Modules_Container.Vector;
      ShipCrew: Crew_Container.Vector;
      ShipMissions: Mission_Container.Vector;
      NewName: Unbounded_String;
      TurretIndex, GunIndex, HullIndex, Amount: Natural := 0;
      Gender: Character;
      MemberName: Unbounded_String;
      TmpSkills: Skills_Container.Vector;
      ProtoShip: constant ProtoShipData := ProtoShips_List.Element(ProtoIndex);
      ShipCargo: Cargo_Container.Vector;
   begin
      for Module of ProtoShip.Modules loop
         ShipModules.Append
         (New_Item =>
            (Name => Modules_List.Element(Module).Name,
             ProtoIndex => Module,
             Weight => Modules_List.Element(Module).Weight,
             Current_Value => Modules_List.Element(Module).Value,
             Max_Value => Modules_List.Element(Module).MaxValue,
             Durability => Modules_List.Element(Module).Durability,
             MaxDurability => Modules_List.Element(Module).Durability,
             Owner => 0,
             UpgradeProgress => 0,
             UpgradeAction => NONE));
      end loop;
      if Name = Null_Unbounded_String then
         NewName := ProtoShip.Name;
      else
         NewName := Name;
      end if;
      for Member of ProtoShip.Crew loop
         if GetRandom(1, 100) < 50 then
            Gender := 'M';
         else
            Gender := 'F';
         end if;
         MemberName := GenerateMemberName(Gender);
         for Skill of Member.Skills loop
            if Skill(3) = 0 then
               TmpSkills.Append(New_Item => Skill);
            else
               TmpSkills.Append
               (New_Item => (Skill(1), GetRandom(Skill(2), Skill(3)), 0));
            end if;
         end loop;
         ShipCrew.Append
         (New_Item =>
            (Name => MemberName,
             Gender => Gender,
             Health => 100,
             Tired => 0,
             Skills => TmpSkills,
             Hunger => 0,
             Thirst => 0,
             Order => Member.Order,
             PreviousOrder => Rest,
             OrderTime => 15,
             Orders => (others => 0)));
         TmpSkills.Clear;
         for Module of ShipModules loop
            if Modules_List.Element(Module.ProtoIndex).MType = CABIN and
              Module.Owner = 0 then
               Module.Name := MemberName & To_Unbounded_String("'s Cabin");
               Module.Owner := ShipCrew.Last_Index;
               exit;
            end if;
         end loop;
         for Module of ShipModules loop
            if Module.Owner = 0 then
               if Modules_List.Element(Module.ProtoIndex).MType = GUN and
                 Member.Order = Gunner then
                  Module.Owner := ShipCrew.Last_Index;
                  exit;
               end if;
            elsif Modules_List.Element(Module.ProtoIndex).MType = COCKPIT and
              Member.Order = Pilot then
               Module.Owner := ShipCrew.Last_Index;
               exit;
            end if;
         end loop;
      end loop;
      for Item of ProtoShip.Cargo loop
         if Item(3) > 0 then
            Amount := GetRandom(Item(2), Item(3));
         else
            Amount := Item(2);
         end if;
         ShipCargo.Append
         (New_Item =>
            (ProtoIndex => Item(1),
             Amount => Amount,
             Name => Null_Unbounded_String,
             Durability => 100));
      end loop;
      TmpShip :=
        (Name => NewName,
         SkyX => X,
         SkyY => Y,
         Speed => Speed,
         Modules => ShipModules,
         Cargo => ShipCargo,
         Crew => ShipCrew,
         UpgradeModule => 0,
         DestinationX => 0,
         DestinationY => 0,
         RepairModule => 0,
         Missions => ShipMissions,
         Description => ProtoShip.Description);
      Amount := 0;
      for I in TmpShip.Modules.First_Index .. TmpShip.Modules.Last_Index loop
         case Modules_List.Element(TmpShip.Modules.Element(I).ProtoIndex)
           .MType is
            when TURRET =>
               TurretIndex := I;
            when GUN =>
               GunIndex := I;
            when HULL =>
               HullIndex := I;
            when others =>
               null;
         end case;
         if TurretIndex > 0 and GunIndex > 0 then
            UpdateModule
              (TmpShip,
               TurretIndex,
               "Current_Value",
               Positive'Image(GunIndex));
            TurretIndex := 0;
            GunIndex := 0;
         end if;
         Amount :=
           Amount +
           Modules_List.Element(TmpShip.Modules.Element(I).ProtoIndex).Size;
      end loop;
      UpdateModule(TmpShip, HullIndex, "Current_Value", Natural'Image(Amount));
      return TmpShip;
   end CreateShip;

   procedure LoadShips is
      ShipsFile: File_Type;
      RawData, FieldName, Value, SkillsValue: Unbounded_String;
      EqualIndex,
      StartIndex,
      EndIndex,
      Amount,
      XIndex,
      CombatValue,
      DotIndex,
      EndIndex2,
      StartIndex2: Natural;
      TempRecord: ProtoShipData;
      TempModules: Positive_Container.Vector;
      TempCargo: Skills_Container.Vector;
      TempCrew: ProtoCrew_Container.Vector;
      TempSkills: Skills_Container.Vector;
      TempOrder: Crew_Orders;
      SkillsAmount, ModuleIndex: Positive;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
      procedure UpdateMember(Member: in out ProtoCrewData) is
      begin
         Member.Order := TempOrder;
      end UpdateMember;
   begin
      if ProtoShips_List.Length > 0 then
         return;
      end if;
      if not Exists("data/ships/") then
         raise Ships_Directory_Not_Found;
      end if;
      Start_Search(Files, "data/ships/", "*.dat");
      if not More_Entries(Files) then
         raise Ships_Files_Not_Found;
      end if;
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         TempRecord :=
           (Name => Null_Unbounded_String,
            Modules => TempModules,
            Accuracy => (0, 0),
            CombatAI => NONE,
            Evasion => (0, 0),
            Loot => (0, 0),
            Perception => (0, 0),
            Cargo => TempCargo,
            CombatValue => 1,
            Crew => TempCrew,
            Description => Null_Unbounded_String,
            Owner => Poleis);
         LogMessage("Loading ships file: " & Full_Name(FoundFile), Everything);
         Open(ShipsFile, In_File, Full_Name(FoundFile));
         while not End_Of_File(ShipsFile) loop
            RawData := To_Unbounded_String(Get_Line(ShipsFile));
            if Element(RawData, 1) /= '[' then
               EqualIndex := Index(RawData, "=");
               FieldName := Head(RawData, EqualIndex - 2);
               Value := Tail(RawData, (Length(RawData) - EqualIndex - 1));
               if FieldName = To_Unbounded_String("Name") then
                  TempRecord.Name := Value;
               elsif FieldName = To_Unbounded_String("Modules") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                  for I in 1 .. Amount loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     for I in Modules_List.Iterate loop
                        if Modules_List(I).Index =
                          Unbounded_Slice(Value, StartIndex, EndIndex - 1) then
                           ModuleIndex := BaseModules_Container.To_Index(I);
                           exit;
                        end if;
                     end loop;
                     TempRecord.Modules.Append(New_Item => ModuleIndex);
                     StartIndex := EndIndex + 2;
                  end loop;
               elsif FieldName = To_Unbounded_String("Accuracy") then
                  DotIndex := Index(Value, "..");
                  if DotIndex = 0 then
                     TempRecord.Accuracy :=
                       (Integer'Value(To_String(Value)), 0);
                  else
                     TempRecord.Accuracy :=
                       (Integer'Value(Slice(Value, 1, DotIndex - 1)),
                        Integer'
                          Value
                            (Slice(Value, DotIndex + 2, Length(Value))));
                  end if;
               elsif FieldName = To_Unbounded_String("CombatAI") then
                  TempRecord.CombatAI := ShipCombatAi'Value(To_String(Value));
               elsif FieldName = To_Unbounded_String("Evasion") then
                  DotIndex := Index(Value, "..");
                  if DotIndex = 0 then
                     TempRecord.Evasion :=
                       (Integer'Value(To_String(Value)), 0);
                  else
                     TempRecord.Evasion :=
                       (Integer'Value(Slice(Value, 1, DotIndex - 1)),
                        Integer'
                          Value
                            (Slice(Value, DotIndex + 2, Length(Value))));
                  end if;
               elsif FieldName = To_Unbounded_String("Loot") then
                  DotIndex := Index(Value, "..");
                  if DotIndex = 0 then
                     TempRecord.Loot := (Integer'Value(To_String(Value)), 0);
                  else
                     TempRecord.Loot :=
                       (Integer'Value(Slice(Value, 1, DotIndex - 1)),
                        Integer'
                          Value
                            (Slice(Value, DotIndex + 2, Length(Value))));
                  end if;
               elsif FieldName = To_Unbounded_String("Perception") then
                  DotIndex := Index(Value, "..");
                  if DotIndex = 0 then
                     TempRecord.Perception :=
                       (Integer'Value(To_String(Value)), 0);
                  else
                     TempRecord.Perception :=
                       (Integer'Value(Slice(Value, 1, DotIndex - 1)),
                        Integer'
                          Value
                            (Slice(Value, DotIndex + 2, Length(Value))));
                  end if;
               elsif FieldName = To_Unbounded_String("Cargo") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                  for I in 1 .. Amount loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     XIndex := Index(Value, "x", StartIndex);
                     DotIndex := Index(Value, "..", StartIndex);
                     if DotIndex = 0 or DotIndex > EndIndex then
                        TempRecord.Cargo.Append
                        (New_Item =>
                           (Integer'
                              Value
                                (Slice(Value, XIndex + 1, EndIndex - 1)),
                            Integer'
                              Value
                                (Slice(Value, StartIndex, XIndex - 1)),
                            0));
                     else
                        TempRecord.Cargo.Append
                        (New_Item =>
                           (Integer'
                              Value
                                (Slice(Value, XIndex + 1, EndIndex - 1)),
                            Integer'
                              Value
                                (Slice(Value, StartIndex, DotIndex - 1)),
                            Integer'
                              Value
                                (Slice(Value, DotIndex + 2, XIndex - 1))));
                     end if;
                     StartIndex := EndIndex + 2;
                  end loop;
               elsif FieldName = To_Unbounded_String("Skills") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, "; ") + 1;
                  for I in 1 .. Amount loop
                     EndIndex := Index(Value, "; ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     SkillsValue :=
                       To_Unbounded_String
                         (Slice(Value, StartIndex, EndIndex - 1));
                     StartIndex2 := 1;
                     SkillsAmount :=
                       Ada.Strings.Unbounded.Count(SkillsValue, ", ") + 1;
                     for J in 1 .. SkillsAmount loop
                        EndIndex2 := Index(SkillsValue, ", ", StartIndex2);
                        if EndIndex2 = 0 then
                           EndIndex2 := Length(SkillsValue) + 1;
                        end if;
                        XIndex := Index(SkillsValue, "x", StartIndex2);
                        DotIndex := Index(SkillsValue, "..", StartIndex2);
                        if DotIndex = 0 or DotIndex > EndIndex2 then
                           TempSkills.Append
                           (New_Item =>
                              (Integer'
                                 Value
                                   (Slice
                                      (SkillsValue,
                                       StartIndex2,
                                       XIndex - 1)),
                               Integer'
                                 Value
                                   (Slice
                                      (SkillsValue,
                                       XIndex + 1,
                                       EndIndex2 - 1)),
                               0));
                        else
                           TempSkills.Append
                           (New_Item =>
                              (Integer'
                                 Value
                                   (Slice
                                      (SkillsValue,
                                       StartIndex2,
                                       XIndex - 1)),
                               Integer'
                                 Value
                                   (Slice
                                      (SkillsValue,
                                       XIndex + 1,
                                       DotIndex - 1)),
                               Integer'
                                 Value
                                   (Slice
                                      (SkillsValue,
                                       DotIndex + 2,
                                       EndIndex2 - 1))));
                        end if;
                        StartIndex2 := EndIndex2 + 2;
                     end loop;
                     TempRecord.Crew.Append
                     (New_Item => (Skills => TempSkills, Order => Rest));
                     TempSkills.Clear;
                     StartIndex := EndIndex + 2;
                  end loop;
               elsif FieldName = To_Unbounded_String("Orders") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                  for I in 1 .. Amount loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     TempOrder :=
                       Crew_Orders'
                         Value
                           (Slice(Value, StartIndex, EndIndex - 1));
                     TempRecord.Crew.Update_Element
                     (Index => I, Process => UpdateMember'Access);
                     StartIndex := EndIndex + 2;
                  end loop;
                  TempOrder := Rest;
               elsif FieldName = To_Unbounded_String("Description") then
                  TempRecord.Description := Value;
               elsif FieldName = To_Unbounded_String("Owner") then
                  TempRecord.Owner := Bases_Owners'Value(To_String(Value));
               end if;
            elsif TempRecord.Name /= Null_Unbounded_String then
               CombatValue := 0;
               for I in
                 TempRecord.Modules.First_Index ..
                     TempRecord.Modules.Last_Index loop
                  case Modules_List.Element(TempRecord.Modules.Element(I))
                    .MType is
                     when HULL | GUN | BATTERING_RAM =>
                        CombatValue :=
                          CombatValue +
                          Modules_List.Element(TempRecord.Modules.Element(I))
                            .Durability +
                          (Modules_List.Element(TempRecord.Modules.Element(I))
                             .MaxValue *
                           10);
                     when ARMOR =>
                        CombatValue :=
                          CombatValue +
                          Modules_List.Element(TempRecord.Modules.Element(I))
                            .Durability;
                     when others =>
                        null;
                  end case;
               end loop;
               for I in
                 TempRecord.Cargo.First_Index ..
                     TempRecord.Cargo.Last_Index loop
                  if Length
                      (Items_List.Element(TempRecord.Cargo.Element(I)(1))
                         .IType) >=
                    4 then
                     if Slice
                         (Items_List.Element(TempRecord.Cargo.Element(I)(1))
                            .IType,
                          1,
                          4) =
                       "Ammo" then
                        CombatValue :=
                          CombatValue +
                          (Items_List.Element(TempRecord.Cargo.Element(I)(1))
                             .Value *
                           10);
                     end if;
                  end if;
               end loop;
               TempRecord.CombatValue := CombatValue;
               ProtoShips_List.Append(New_Item => TempRecord);
               LogMessage
                 ("Ship added: " & To_String(TempRecord.Name),
                  Everything);
               TempRecord :=
                 (Name => Null_Unbounded_String,
                  Modules => TempModules,
                  Accuracy => (0, 0),
                  CombatAI => NONE,
                  Evasion => (0, 0),
                  Loot => (0, 0),
                  Perception => (0, 0),
                  Cargo => TempCargo,
                  CombatValue => 1,
                  Crew => TempCrew,
                  Description => Null_Unbounded_String,
                  Owner => Poleis);
               TempRecord.Name := Null_Unbounded_String;
            end if;
         end loop;
         Close(ShipsFile);
      end loop;
      End_Search(Files);
   end LoadShips;

   function CountShipWeight(Ship: ShipRecord) return Positive is
      Weight: Natural := 0;
      CargoWeight: Positive;
   begin
      for Module of Ship.Modules loop
         Weight := Weight + Module.Weight;
      end loop;
      for Item of Ship.Cargo loop
         CargoWeight :=
           Item.Amount * Items_List.Element(Item.ProtoIndex).Weight;
         Weight := Weight + CargoWeight;
      end loop;
      return Weight;
   end CountShipWeight;

   procedure RepairShip(Minutes: Positive) is
      OrderTime, CurrentMinutes, RepairPoints: Integer;
      RepairNeeded, RepairStopped: Boolean := False;
      package Natural_Container is new Vectors(Positive, Natural);
      CrewRepairPoints: Natural_Container.Vector;
      procedure UpdatePoints(Points: in out Natural) is
      begin
         Points := RepairPoints;
      end UpdatePoints;
      procedure RepairModule(ModuleIndex: Positive) is
         PointsIndex, PointsBonus, RepairMaterial, ToolsIndex: Natural;
         ProtoIndex, RepairValue: Positive;
      begin
         PointsIndex := 0;
         RepairNeeded := True;
         RepairStopped := False;
         for J in
           PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(J).Order = Repair then
               PointsIndex := PointsIndex + 1;
               if CrewRepairPoints(PointsIndex) > 0 then
                  PointsBonus :=
                    (GetSkillLevel
                       (J,
                        Modules_List.Element
                        (PlayerShip.Modules.Element(ModuleIndex).ProtoIndex)
                          .RepairSkill) /
                     10) *
                    CrewRepairPoints(PointsIndex);
                  RepairPoints := CrewRepairPoints(PointsIndex) + PointsBonus;
                  RepairMaterial := 0;
                  ToolsIndex := 0;
                  for K in
                    PlayerShip.Cargo.First_Index ..
                        PlayerShip.Cargo.Last_Index loop
                     if Items_List.Element
                       (PlayerShip.Cargo.Element(K).ProtoIndex)
                         .IType =
                       Modules_List.Element
                       (PlayerShip.Modules.Element(ModuleIndex).ProtoIndex)
                         .RepairMaterial then
                        ProtoIndex := PlayerShip.Cargo.Element(K).ProtoIndex;
                        RepairMaterial := K;
                  -- Limit repair point depends on amount of repair materials
                        if PlayerShip.Cargo.Element(K).Amount <
                          RepairPoints then
                           RepairPoints := PlayerShip.Cargo.Element(K).Amount;
                        end if;
                     elsif Items_List.Element
                       (PlayerShip.Cargo.Element(K).ProtoIndex)
                         .IType =
                       To_Unbounded_String("RepairTools") then
                        ToolsIndex := K;
                     end if;
                     exit when RepairMaterial > 0 and ToolsIndex > 0;
                  end loop;
                  if RepairMaterial = 0 then
                     AddMessage
                       ("You don't have repair materials to continue repairs of " &
                        To_String
                          (PlayerShip.Modules.Element(ModuleIndex).Name) &
                        ".",
                        OrderMessage);
                     RepairStopped := True;
                     return;
                  end if;
                  if ToolsIndex = 0 then
                     if PointsIndex = 1 then
                        AddMessage
                          ("You don't have repair tools to continue repairs of " &
                           To_String
                             (PlayerShip.Modules.Element(ModuleIndex).Name) &
                           ".",
                           OrderMessage);
                     else
                        AddMessage
                          (To_String(PlayerShip.Crew.Element(J).Name) &
                           " can't continue repairs due to lack of repair tools.",
                           OrderMessage);
                     end if;
                     RepairStopped := True;
                     return;
                  end if;
                  -- Repair module
                  if PlayerShip.Modules.Element(ModuleIndex).Durability +
                    RepairPoints >=
                    PlayerShip.Modules.Element(ModuleIndex).MaxDurability then
                     RepairValue :=
                       PlayerShip.Modules.Element(ModuleIndex).MaxDurability -
                       PlayerShip.Modules.Element(ModuleIndex).Durability;
                     RepairNeeded := False;
                  else
                     RepairValue := RepairPoints;
                  end if;
                  UpdateCargo(PlayerShip, ProtoIndex, (0 - RepairValue));
                  UpdateModule
                    (PlayerShip,
                     ModuleIndex,
                     "Durability",
                     Integer'Image(RepairValue));
                  if RepairValue > CrewRepairPoints(PointsIndex) then
                     RepairValue := CrewRepairPoints(PointsIndex);
                     RepairPoints := 0;
                  else
                     RepairPoints :=
                       CrewRepairPoints(PointsIndex) - RepairValue;
                  end if;
                  GainExp
                    (RepairValue,
                     Modules_List.Element
                     (PlayerShip.Modules.Element(ModuleIndex).ProtoIndex)
                       .RepairSkill,
                     J);
                  CrewRepairPoints.Update_Element
                  (Index => PointsIndex, Process => UpdatePoints'Access);
                  DamageCargo
                    (ToolsIndex,
                     J,
                     Modules_List.Element
                     (PlayerShip.Modules.Element(ModuleIndex).ProtoIndex)
                       .RepairSkill);
                  exit when not RepairNeeded;
               end if;
            end if;
         end loop;
      end RepairModule;
   begin
      for Member of PlayerShip.Crew loop
         if Member.Order = Repair then
            CurrentMinutes := Minutes;
            OrderTime := Member.OrderTime;
            RepairPoints := 0;
            while CurrentMinutes > 0 loop
               if CurrentMinutes >= OrderTime then
                  CurrentMinutes := CurrentMinutes - OrderTime;
                  RepairPoints := RepairPoints + 1;
                  OrderTime := 15;
               else
                  OrderTime := OrderTime - CurrentMinutes;
                  CurrentMinutes := 0;
               end if;
            end loop;
            CrewRepairPoints.Append(New_Item => RepairPoints);
            Member.OrderTime := OrderTime;
         end if;
      end loop;
      if CrewRepairPoints.Length = 0 then
         return;
      end if;
      if PlayerShip.RepairModule > 0 then
         if PlayerShip.Modules.Element(PlayerShip.RepairModule).Durability <
           PlayerShip.Modules.Element(PlayerShip.RepairModule)
             .MaxDurability then
            RepairModule(PlayerShip.RepairModule);
         end if;
      end if;
      Repair_Loop:
      for I in
        PlayerShip.Modules.First_Index .. PlayerShip.Modules.Last_Index loop
         if PlayerShip.Modules.Element(I).Durability <
           PlayerShip.Modules.Element(I).MaxDurability then
            RepairModule(I);
         end if;
      end loop Repair_Loop;
      -- Send repair team on break if all is ok
      if not RepairNeeded or RepairStopped then
         if not RepairNeeded then
            AddMessage("All repairs are finished.", OrderMessage);
         end if;
         for I in
           PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
            if PlayerShip.Crew.Element(I).Order = Repair then
               GiveOrders(I, Rest);
            end if;
         end loop;
      end if;
   end RepairShip;

   function GenerateShipName
     (Owner: Bases_Owners :=
        Any)
     return Unbounded_String is -- based on name generator from libtcod
      NewName: Unbounded_String := Null_Unbounded_String;
      LettersAmount, NumbersAmount: Positive;
      subtype Letters is Character range 'A' .. 'Z';
      subtype Numbers is Character range '0' .. '9';
   begin
      case Owner is
         when Any =>
            NewName :=
              ShipSyllablesStart.Element
              (GetRandom
                 (ShipSyllablesStart.First_Index,
                  ShipSyllablesStart.Last_Index));
            if GetRandom(1, 100) < 51 then
               Append
                 (NewName,
                  ShipSyllablesMiddle.Element
                  (GetRandom
                     (ShipSyllablesMiddle.First_Index,
                      ShipSyllablesMiddle.Last_Index)));
            end if;
            Append
              (NewName,
               ShipSyllablesEnd.Element
               (GetRandom
                  (ShipSyllablesEnd.First_Index,
                   ShipSyllablesEnd.Last_Index)));
         when Drones =>
            LettersAmount := GetRandom(2, 5);
            for I in 1 .. LettersAmount loop
               Append
                 (NewName,
                  Letters'
                    Val
                      (GetRandom
                         (Letters'Pos(Letters'First),
                          Letters'Pos(Letters'Last))));
            end loop;
            Append(NewName, '-');
            NumbersAmount := GetRandom(2, 4);
            for I in 1 .. NumbersAmount loop
               Append
                 (NewName,
                  Numbers'
                    Val
                      (GetRandom
                         (Numbers'Pos(Numbers'First),
                          Numbers'Pos(Numbers'Last))));
            end loop;
         when others =>
            null;
      end case;
      return NewName;
   end GenerateShipName;

end Ships;
