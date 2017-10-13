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
with Messages; use Messages;
with ShipModules; use ShipModules;
with Utils; use Utils;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Log; use Log;
with Crafts; use Crafts;
with Events; use Events;
with Maps; use Maps;
with Mobs; use Mobs;

package body Ships is

   function CreateShip
     (ProtoIndex: Positive;
      Name: Unbounded_String;
      X, Y: Integer;
      Speed: ShipSpeed;
      RandomUpgrades: Boolean := True) return ShipRecord is
      TmpShip: ShipRecord;
      ShipModules: Modules_Container.Vector;
      ShipCrew: Crew_Container.Vector;
      ShipMissions: Mission_Container.Vector;
      NewName: Unbounded_String;
      TurretIndex,
      GunIndex,
      HullIndex,
      Amount,
      UpgradesAmount,
      WeightGain: Natural :=
        0;
      Gender: Character;
      MemberName: Unbounded_String;
      TmpSkills: Skills_Container.Vector;
      ProtoShip: constant ProtoShipData := ProtoShips_List(ProtoIndex);
      ShipCargo, TmpInventory: Inventory_Container.Vector;
      TempModule: BaseModule_Data;
      MaxValue, Roll: Positive;
      StartX, StartY, EndX, EndY: Integer;
      TmpAttributes: Attributes_Container.Vector;
      Member: ProtoMobRecord;
   begin
      if RandomUpgrades then
         UpgradesAmount := GetRandom(0, Positive(ProtoShip.Modules.Length));
      end if;
      for Module of ProtoShip.Modules loop
         TempModule := Modules_List(Module);
         if UpgradesAmount > 0 then
            WeightGain :=
              Modules_List(Module).Weight / Modules_List(Module).Durability;
            if WeightGain < 1 then
               WeightGain := 1;
            end if;
            if GetRandom(1, 100) > 50 then
               Roll := GetRandom(1, 100);
               case Roll is
                  when 1 .. 50 => -- Upgrade durability of module
                     MaxValue :=
                       Positive(Float(Modules_List(Module).Durability) * 1.5);
                     TempModule.Durability :=
                       GetRandom(Modules_List(Module).Durability, MaxValue);
                     TempModule.Weight :=
                       TempModule.Weight +
                       (WeightGain *
                        (TempModule.Durability -
                         Modules_List(Module).Durability));
                  when
                      51 ..
                        75 => -- Upgrade value (depends on module) of module
                     if Modules_List(Module).MType = ENGINE then
                        WeightGain := WeightGain * 10;
                        MaxValue :=
                          Positive(Float(Modules_List(Module).Value) / 2.0);
                        TempModule.Value :=
                          GetRandom(MaxValue, Modules_List(Module).Value);
                        TempModule.Weight :=
                          TempModule.Weight +
                          (WeightGain *
                           (Modules_List(Module).Value - TempModule.Value));
                     end if;
                  when
                      76 ..
                        100 => -- Upgrade max_value (depends on module) of module
                     case Modules_List(Module).MType is
                        when HULL =>
                           WeightGain := WeightGain * 10;
                        when ENGINE =>
                           WeightGain := 1;
                        when others =>
                           null;
                     end case;
                     if TempModule.MType = ENGINE or
                       TempModule.MType = CABIN or
                       TempModule.MType = GUN or
                       TempModule.MType = BATTERING_RAM or
                       TempModule.MType = HULL then
                        MaxValue :=
                          Positive(Float(Modules_List(Module).MaxValue) * 1.5);
                        TempModule.MaxValue :=
                          GetRandom(Modules_List(Module).MaxValue, MaxValue);
                        TempModule.Weight :=
                          TempModule.Weight +
                          (WeightGain *
                           (TempModule.MaxValue -
                            Modules_List(Module).MaxValue));
                     end if;
                  when others =>
                     null;
               end case;
               UpgradesAmount := UpgradesAmount - 1;
            end if;
         end if;
         ShipModules.Append
         (New_Item =>
            (Name => Modules_List(Module).Name,
             ProtoIndex => Module,
             Weight => TempModule.Weight,
             Durability => TempModule.Durability,
             MaxDurability => TempModule.Durability,
             Owner => 0,
             UpgradeProgress => 0,
             UpgradeAction => NONE,
             Data => (TempModule.Value, TempModule.MaxValue, 0)));
      end loop;
      if Name = Null_Unbounded_String then
         NewName := ProtoShip.Name;
      else
         NewName := Name;
      end if;
      for I in ProtoShip.Crew.First_Index .. ProtoShip.Crew.Last_Index loop
         if GetRandom(1, 100) < 50 then
            Gender := 'M';
         else
            Gender := 'F';
         end if;
         MemberName := GenerateMemberName(Gender);
         Member := ProtoMobs_List.Element(ProtoShip.Crew.Element(I));
         for Skill of Member.Skills loop
            if Skill(3) = 0 then
               TmpSkills.Append(New_Item => Skill);
            else
               TmpSkills.Append
               (New_Item => (Skill(1), GetRandom(Skill(2), Skill(3)), 0));
            end if;
         end loop;
         for Attribute of Member.Attributes loop
            if Attribute(2) = 0 then
               TmpAttributes.Append(New_Item => Attribute);
            else
               TmpAttributes.Append
               (New_Item => (GetRandom(Attribute(1), Attribute(2)), 0));
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
             Orders => Member.Priorities,
             Attributes => TmpAttributes,
             Inventory => TmpInventory));
         TmpSkills.Clear;
         TmpAttributes.Clear;
         for Module of ShipModules loop
            if Modules_List(Module.ProtoIndex).MType = CABIN and
              Module.Owner = 0 then
               Module.Name := MemberName & To_Unbounded_String("'s Cabin");
               Module.Owner := ShipCrew.Last_Index;
               exit;
            end if;
         end loop;
         for Module of ShipModules loop
            if Module.Owner = 0 then
               if Modules_List(Module.ProtoIndex).MType = GUN and
                 Member.Order = Gunner then
                  Module.Owner := ShipCrew.Last_Index;
                  exit;
               end if;
            elsif Modules_List(Module.ProtoIndex).MType = COCKPIT and
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
         Description => ProtoShip.Description,
         HomeBase => 0);
      Amount := 0;
      for I in TmpShip.Modules.Iterate loop
         case Modules_List(TmpShip.Modules(I).ProtoIndex).MType is
            when TURRET =>
               TurretIndex := Modules_Container.To_Index(I);
            when GUN =>
               GunIndex := Modules_Container.To_Index(I);
            when HULL =>
               HullIndex := Modules_Container.To_Index(I);
            when others =>
               null;
         end case;
         if TurretIndex > 0 and GunIndex > 0 then
            TmpShip.Modules(TurretIndex).Data(1) := GunIndex;
            TurretIndex := 0;
            GunIndex := 0;
         end if;
         Amount := Amount + Modules_List(TmpShip.Modules(I).ProtoIndex).Size;
      end loop;
      TmpShip.Modules(HullIndex).Data(1) := Amount;
      if ProtoShip.Index = PlayerShipIndex then
         for Recipe of ProtoShip.KnownRecipes loop
            Known_Recipes.Append(New_Item => Recipe);
         end loop;
      end if;
      -- Set home base for ship
      if SkyMap(X, Y).BaseIndex > 0 then
         TmpShip.HomeBase := SkyMap(X, Y).BaseIndex;
      else
         StartX := X - 100;
         if StartX < 1 then
            StartX := 1;
         end if;
         StartY := Y - 100;
         if StartY < 1 then
            StartY := 1;
         end if;
         EndX := X + 100;
         if EndX > 1024 then
            EndX := 1024;
         end if;
         EndY := Y + 100;
         if EndY > 1024 then
            EndY := 1024;
         end if;
         Bases_Loop:
         for SkyX in StartX .. EndX loop
            for SkyY in StartY .. EndY loop
               if SkyMap(SkyX, SkyY).BaseIndex > 0 then
                  if SkyBases(SkyMap(SkyX, SkyY).BaseIndex).Owner =
                    ProtoShip.Owner then
                     TmpShip.HomeBase := SkyMap(SkyX, SkyY).BaseIndex;
                     exit Bases_Loop;
                  end if;
               end if;
            end loop;
         end loop Bases_Loop;
         if TmpShip.HomeBase = 0 then
            for I in SkyBases'Range loop
               if SkyBases(I).Owner = ProtoShip.Owner then
                  TmpShip.HomeBase := I;
                  exit;
               end if;
            end loop;
         end if;
      end if;
      return TmpShip;
   end CreateShip;

   procedure LoadShips is
      ShipsFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex,
      StartIndex,
      EndIndex,
      Amount,
      XIndex,
      CombatValue,
      DotIndex,
      ModuleIndex,
      ItemIndex,
      RecipeIndex,
      MobIndex: Natural;
      TempRecord: ProtoShipData;
      TempModules, TempRecipes: Positive_Container.Vector;
      TempCargo: Skills_Container.Vector;
      TempCrew: Positive_Container.Vector;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
   begin
      if ProtoShips_List.Length > 0 then
         return;
      end if;
      if not Exists(To_String(DataDirectory) & "ships" & Dir_Separator) then
         raise Ships_Directory_Not_Found;
      end if;
      Start_Search
        (Files,
         To_String(DataDirectory) & "ships" & Dir_Separator,
         "*.dat");
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
            Owner => Poleis,
            Index => Null_Unbounded_String,
            KnownRecipes => TempRecipes);
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
                     ModuleIndex :=
                       FindProtoModule
                         (Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     if ModuleIndex = 0 then
                        Close(ShipsFile);
                        End_Search(Files);
                        raise Ships_Invalid_Data
                          with "Invalid module index: |" &
                          Slice(Value, StartIndex, EndIndex - 1) &
                          "| in " &
                          To_String(TempRecord.Name) &
                          ".";
                     end if;
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
                        Integer'Value
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
                        Integer'Value
                          (Slice(Value, DotIndex + 2, Length(Value))));
                  end if;
               elsif FieldName = To_Unbounded_String("Loot") then
                  DotIndex := Index(Value, "..");
                  if DotIndex = 0 then
                     TempRecord.Loot := (Integer'Value(To_String(Value)), 0);
                  else
                     TempRecord.Loot :=
                       (Integer'Value(Slice(Value, 1, DotIndex - 1)),
                        Integer'Value
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
                        Integer'Value
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
                     ItemIndex :=
                       FindProtoItem
                         (Unbounded_Slice(Value, XIndex + 1, EndIndex - 1));
                     if ItemIndex = 0 then
                        Close(ShipsFile);
                        End_Search(Files);
                        raise Ships_Invalid_Data
                          with "Invalid item index: |" &
                          Slice(Value, XIndex + 1, EndIndex - 1) &
                          "| in " &
                          To_String(TempRecord.Name) &
                          ".";
                     end if;
                     if DotIndex = 0 or DotIndex > EndIndex then
                        TempRecord.Cargo.Append
                        (New_Item =>
                           (ItemIndex,
                            Integer'Value
                              (Slice(Value, StartIndex, XIndex - 1)),
                            0));
                     else
                        TempRecord.Cargo.Append
                        (New_Item =>
                           (ItemIndex,
                            Integer'Value
                              (Slice(Value, StartIndex, DotIndex - 1)),
                            Integer'Value
                              (Slice(Value, DotIndex + 2, XIndex - 1))));
                     end if;
                     StartIndex := EndIndex + 2;
                  end loop;
               elsif FieldName = To_Unbounded_String("Description") then
                  TempRecord.Description := Value;
               elsif FieldName = To_Unbounded_String("Owner") then
                  TempRecord.Owner := Bases_Owners'Value(To_String(Value));
               elsif FieldName = To_Unbounded_String("Recipes") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                  for I in 1 .. Amount loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     RecipeIndex :=
                       FindRecipe
                         (Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     if RecipeIndex = 0 then
                        Close(ShipsFile);
                        End_Search(Files);
                        raise Ships_Invalid_Data
                          with "Invalid recipe index: |" &
                          Slice(Value, StartIndex, EndIndex - 1) &
                          "| in " &
                          To_String(TempRecord.Name) &
                          ".";
                     end if;
                     TempRecord.KnownRecipes.Append(New_Item => RecipeIndex);
                     StartIndex := EndIndex + 2;
                  end loop;
               elsif FieldName = To_Unbounded_String("Crew") then
                  StartIndex := 1;
                  Amount := Ada.Strings.Unbounded.Count(Value, ", ") + 1;
                  for I in 1 .. Amount loop
                     EndIndex := Index(Value, ", ", StartIndex);
                     if EndIndex = 0 then
                        EndIndex := Length(Value) + 1;
                     end if;
                     MobIndex :=
                       FindProtoMob
                         (Unbounded_Slice(Value, StartIndex, EndIndex - 1));
                     if MobIndex = 0 then
                        Close(ShipsFile);
                        End_Search(Files);
                        raise Ships_Invalid_Data
                          with "Invalid mob index: |" &
                          Slice(Value, StartIndex, EndIndex - 1) &
                          "| in " &
                          To_String(TempRecord.Name) &
                          ".";
                     end if;
                     TempRecord.Crew.Append(New_Item => MobIndex);
                     StartIndex := EndIndex + 2;
                  end loop;
               end if;
            else
               if TempRecord.Name /= Null_Unbounded_String then
                  CombatValue := 0;
                  for ModuleIndex of TempRecord.Modules loop
                     case Modules_List(ModuleIndex).MType is
                        when HULL | GUN | BATTERING_RAM =>
                           CombatValue :=
                             CombatValue +
                             Modules_List(ModuleIndex).Durability +
                             (Modules_List(ModuleIndex).MaxValue * 10);
                        when ARMOR =>
                           CombatValue :=
                             CombatValue +
                             Modules_List(ModuleIndex).Durability;
                        when HARPOON_GUN =>
                           CombatValue :=
                             CombatValue +
                             Modules_List(ModuleIndex).Durability +
                             (Modules_List(ModuleIndex).MaxValue * 5);
                        when others =>
                           null;
                     end case;
                  end loop;
                  for Item of TempRecord.Cargo loop
                     if Length(Items_List(Item(1)).IType) >= 4 then
                        if Slice(Items_List(Item(1)).IType, 1, 4) = "Ammo" then
                           CombatValue :=
                             CombatValue + (Items_List(Item(1)).Value * 10);
                        elsif Items_List(Item(1)).IType = "Harpoon" then
                           CombatValue :=
                             CombatValue + (Items_List(Item(1)).Value * 5);
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
                     Owner => Poleis,
                     Index => Null_Unbounded_String,
                     KnownRecipes => TempRecipes);
                  TempRecord.Name := Null_Unbounded_String;
               end if;
               if Length(RawData) > 2 then
                  TempRecord.Index :=
                    Unbounded_Slice(RawData, 2, (Length(RawData) - 1));
               end if;
            end if;
         end loop;
         Close(ShipsFile);
      end loop;
      End_Search(Files);
      GenerateTraders;
   end LoadShips;

   function CountShipWeight(Ship: ShipRecord) return Positive is
      Weight: Natural := 0;
      CargoWeight: Positive;
   begin
      for Module of Ship.Modules loop
         Weight := Weight + Module.Weight;
      end loop;
      for Item of Ship.Cargo loop
         CargoWeight := Item.Amount * Items_List(Item.ProtoIndex).Weight;
         Weight := Weight + CargoWeight;
      end loop;
      return Weight;
   end CountShipWeight;

   procedure RepairShip(Minutes: Positive) is
      OrderTime, CurrentMinutes, RepairPoints: Integer;
      RepairNeeded, RepairStopped: Boolean := False;
      package Natural_Container is new Vectors(Positive, Natural);
      CrewRepairPoints: Natural_Container.Vector;
      procedure RepairModule(ModuleIndex: Positive) is
         PointsIndex, PointsBonus, RepairMaterial, ToolsIndex: Natural;
         RepairValue: Positive;
      begin
         PointsIndex := 0;
         RepairNeeded := True;
         RepairStopped := False;
         for J in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(J).Order = Repair then
               PointsIndex := PointsIndex + 1;
               if CrewRepairPoints(PointsIndex) > 0 then
                  PointsBonus :=
                    (GetSkillLevel
                       (PlayerShip.Crew(J),
                        Modules_List
                          (PlayerShip.Modules(ModuleIndex).ProtoIndex)
                          .RepairSkill) /
                     10) *
                    CrewRepairPoints(PointsIndex);
                  RepairPoints := CrewRepairPoints(PointsIndex) + PointsBonus;
                  RepairMaterial :=
                    FindItem
                      (Inventory => PlayerShip.Cargo,
                       ItemType =>
                         Modules_List
                           (PlayerShip.Modules(ModuleIndex).ProtoIndex)
                           .RepairMaterial);
                  if RepairMaterial > 0 then
                     if PlayerShip.Cargo(RepairMaterial).Amount <
                       RepairPoints then
                        RepairPoints :=
                          PlayerShip.Cargo(RepairMaterial).Amount;
                     end if;
                  end if;
                  ToolsIndex :=
                    FindItem
                      (Inventory => PlayerShip.Crew(J).Inventory,
                       ItemType => RepairTools);
                  if ToolsIndex = 0 then
                     ToolsIndex :=
                       FindItem
                         (Inventory => PlayerShip.Cargo,
                          ItemType => RepairTools);
                     if ToolsIndex > 0 then
                        UpdateInventory
                          (Crew_Container.To_Index(J),
                           1,
                           PlayerShip.Cargo(ToolsIndex).ProtoIndex,
                           PlayerShip.Cargo(ToolsIndex).Durability);
                        UpdateCargo
                          (Ship => PlayerShip,
                           Amount => -1,
                           CargoIndex => ToolsIndex);
                        ToolsIndex :=
                          FindItem
                            (Inventory => PlayerShip.Crew(J).Inventory,
                             ItemType => RepairTools);
                     end if;
                  end if;
                  if RepairMaterial = 0 then
                     AddMessage
                       ("You don't have repair materials to continue repairs of " &
                        To_String(PlayerShip.Modules(ModuleIndex).Name) &
                        ".",
                        OrderMessage,
                        3);
                     RepairStopped := True;
                     return;
                  end if;
                  if ToolsIndex = 0 then
                     if PointsIndex = 1 then
                        AddMessage
                          ("You don't have repair tools to continue repairs of " &
                           To_String(PlayerShip.Modules(ModuleIndex).Name) &
                           ".",
                           OrderMessage,
                           3);
                     else
                        AddMessage
                          (To_String(PlayerShip.Crew(J).Name) &
                           " can't continue repairs due to lack of repair tools.",
                           OrderMessage,
                           3);
                     end if;
                     RepairStopped := True;
                     return;
                  end if;
                  -- Repair module
                  if PlayerShip.Modules(ModuleIndex).Durability +
                    RepairPoints >=
                    PlayerShip.Modules(ModuleIndex).MaxDurability then
                     RepairValue :=
                       PlayerShip.Modules(ModuleIndex).MaxDurability -
                       PlayerShip.Modules(ModuleIndex).Durability;
                     RepairNeeded := False;
                  else
                     RepairValue := RepairPoints;
                  end if;
                  if RepairValue = PlayerShip.Cargo(RepairMaterial).Amount and
                    ToolsIndex > RepairMaterial then
                     ToolsIndex := ToolsIndex - 1;
                  end if;
                  UpdateCargo
                    (Ship => PlayerShip,
                     CargoIndex => RepairMaterial,
                     Amount => (0 - RepairValue));
                  PlayerShip.Modules(ModuleIndex).Durability :=
                    PlayerShip.Modules(ModuleIndex).Durability + RepairValue;
                  if RepairValue > CrewRepairPoints(PointsIndex) then
                     RepairValue := CrewRepairPoints(PointsIndex);
                     RepairPoints := 0;
                  else
                     RepairPoints :=
                       CrewRepairPoints(PointsIndex) - RepairValue;
                  end if;
                  GainExp
                    (RepairValue,
                     Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                       .RepairSkill,
                     Crew_Container.To_Index(J));
                  CrewRepairPoints(PointsIndex) := RepairPoints;
                  DamageItem
                    (PlayerShip.Crew(J).Inventory,
                     ToolsIndex,
                     GetSkillLevel
                       (PlayerShip.Crew(J),
                        Modules_List
                          (PlayerShip.Modules(ModuleIndex).ProtoIndex)
                          .RepairSkill),
                     Crew_Container.To_Index(J));
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
         if PlayerShip.Modules(PlayerShip.RepairModule).Durability <
           PlayerShip.Modules(PlayerShip.RepairModule).MaxDurability then
            RepairModule(PlayerShip.RepairModule);
         end if;
      end if;
      Repair_Loop:
      for I in PlayerShip.Modules.Iterate loop
         if PlayerShip.Modules(I).Durability <
           PlayerShip.Modules(I).MaxDurability then
            RepairModule(Modules_Container.To_Index(I));
         end if;
      end loop Repair_Loop;
      -- Send repair team on break if all is ok
      if not RepairNeeded or RepairStopped then
         if not RepairNeeded then
            AddMessage("All repairs are finished.", OrderMessage, 2);
         end if;
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Order = Repair then
               GiveOrders(Crew_Container.To_Index(I), Rest);
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
              ShipSyllablesStart
                (GetRandom
                   (ShipSyllablesStart.First_Index,
                    ShipSyllablesStart.Last_Index));
            if GetRandom(1, 100) < 51 then
               Append
                 (NewName,
                  ShipSyllablesMiddle
                    (GetRandom
                       (ShipSyllablesMiddle.First_Index,
                        ShipSyllablesMiddle.Last_Index)));
            end if;
            Append
              (NewName,
               ShipSyllablesEnd
                 (GetRandom
                    (ShipSyllablesEnd.First_Index,
                     ShipSyllablesEnd.Last_Index)));
         when Drones =>
            LettersAmount := GetRandom(2, 5);
            for I in 1 .. LettersAmount loop
               Append
                 (NewName,
                  Letters'Val
                    (GetRandom
                       (Letters'Pos(Letters'First),
                        Letters'Pos(Letters'Last))));
            end loop;
            Append(NewName, '-');
            NumbersAmount := GetRandom(2, 4);
            for I in 1 .. NumbersAmount loop
               Append
                 (NewName,
                  Numbers'Val
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
