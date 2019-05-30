--    Copyright 2016-2018 Bartek thindil Jasicki
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with ShipModules; use ShipModules;
with Utils; use Utils;
with Log; use Log;
with Crafts; use Crafts;
with Maps; use Maps;
with Mobs; use Mobs;
with Factions; use Factions;
with Bases; use Bases;

package body Ships is

   function CreateShip
     (ProtoIndex: Positive; Name: Unbounded_String; X, Y: Integer;
      Speed: ShipSpeed; RandomUpgrades: Boolean := True) return ShipRecord is
      TmpShip: ShipRecord;
      ShipModules: Modules_Container.Vector;
      ShipCrew: Crew_Container.Vector;
      NewName: Unbounded_String;
      HullIndex, Amount, UpgradesAmount, WeightGain: Natural := 0;
      Gender: Character;
      MemberName: Unbounded_String;
      TmpSkills: Skills_Container.Vector;
      ProtoShip: constant ProtoShipData := ProtoShips_List(ProtoIndex);
      ShipCargo, TmpInventory: Inventory_Container.Vector;
      TempModule: BaseModule_Data;
      MaxValue, Roll, MemberFaction: Positive;
      StartX, StartY, EndX, EndY: Integer;
      TmpAttributes: Attributes_Container.Vector;
      Member: ProtoMobRecord;
      GunAssigned: Boolean;
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
                  when 51 ..
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
                  when 76 ..
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
                       TempModule.MType = CABIN or TempModule.MType = GUN or
                       TempModule.MType = BATTERING_RAM or
                       TempModule.MType = HULL or
                       TempModule.MType = HARPOON_GUN then
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
              (Name => Modules_List(Module).Name, ProtoIndex => Module,
               Weight => TempModule.Weight,
               Durability => TempModule.Durability,
               MaxDurability => TempModule.Durability, Owner => 0,
               UpgradeProgress => 0, UpgradeAction => NONE,
               Data => (TempModule.Value, TempModule.MaxValue, 0)));
      end loop;
      if Name = Null_Unbounded_String then
         NewName := ProtoShip.Name;
      else
         NewName := Name;
      end if;
      for ProtoMember of ProtoShip.Crew loop
         if ProtoMember(3) = 0 then
            Amount := ProtoMember(2);
         else
            Amount := GetRandom(ProtoMember(2), ProtoMember(3));
         end if;
         for I in 1 .. Amount loop
            if GetRandom(1, 100) < 99 then
               MemberFaction := ProtoShip.Owner;
            else
               MemberFaction :=
                 GetRandom
                   (Factions_List.First_Index, Factions_List.Last_Index);
            end if;
            if not Factions_List(MemberFaction).Flags.Contains
                (To_Unbounded_String("nogender")) then
               if GetRandom(1, 100) < 50 then
                  Gender := 'M';
               else
                  Gender := 'F';
               end if;
            else
               Gender := 'M';
            end if;
            MemberName :=
              GenerateMemberName(Gender, Factions_List(MemberFaction).Index);
            Member := ProtoMobs_List.Element(ProtoMember(1));
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
            for Item of Member.Inventory loop
               if Item(3) > 0 then
                  Amount := GetRandom(Item(2), Item(3));
               else
                  Amount := Item(2);
               end if;
               TmpInventory.Append
                 (New_Item =>
                    (ProtoIndex => Item(1), Amount => Amount,
                     Name => Null_Unbounded_String, Durability => 100));
            end loop;
            ShipCrew.Append
              (New_Item =>
                 (Name => MemberName, Gender => Gender, Health => 100,
                  Tired => 0, Skills => TmpSkills, Hunger => 0, Thirst => 0,
                  Order => Member.Order, PreviousOrder => Rest,
                  OrderTime => 15, Orders => Member.Priorities,
                  Attributes => TmpAttributes, Inventory => TmpInventory,
                  Equipment => Member.Equipment, Payment => (20, 0),
                  ContractLength => -1, Morale => (50, 0), Loyalty => 100,
                  HomeBase => 1, Faction => MemberFaction));
            TmpSkills.Clear;
            TmpAttributes.Clear;
            TmpInventory.Clear;
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
                  if
                    (Modules_List(Module.ProtoIndex).MType = GUN or
                     Modules_List(Module.ProtoIndex).MType = HARPOON_GUN) and
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
      end loop;
      for Item of ProtoShip.Cargo loop
         if Item(3) > 0 then
            Amount := GetRandom(Item(2), Item(3));
         else
            Amount := Item(2);
         end if;
         ShipCargo.Append
           (New_Item =>
              (ProtoIndex => Item(1), Amount => Amount,
               Name => Null_Unbounded_String, Durability => 100));
      end loop;
      TmpShip :=
        (Name => NewName, SkyX => X, SkyY => Y, Speed => Speed,
         Modules => ShipModules, Cargo => ShipCargo, Crew => ShipCrew,
         UpgradeModule => 0, DestinationX => 0, DestinationY => 0,
         RepairModule => 0, Description => ProtoShip.Description,
         HomeBase => 0);
      for I in TmpShip.Modules.Iterate loop
         if Modules_List(TmpShip.Modules(I).ProtoIndex).MType = TURRET then
            for J in TmpShip.Modules.Iterate loop
               if Modules_List(TmpShip.Modules(J).ProtoIndex).MType = GUN or
                 Modules_List(TmpShip.Modules(J).ProtoIndex).MType =
                   HARPOON_GUN then
                  GunAssigned := False;
                  for K in TmpShip.Modules.Iterate loop
                     if Modules_List(TmpShip.Modules(K).ProtoIndex).MType =
                       TURRET and
                       TmpShip.Modules(K).Data(1) =
                         Modules_Container.To_Index(J) then
                        GunAssigned := True;
                        exit;
                     end if;
                  end loop;
                  if not GunAssigned then
                     TmpShip.Modules(I).Data(1) :=
                       Modules_Container.To_Index(J);
                  end if;
               end if;
            end loop;
         end if;
      end loop;
      Amount := 0;
      for I in TmpShip.Modules.Iterate loop
         if Modules_List(TmpShip.Modules(I).ProtoIndex).MType = HULL then
            HullIndex := Modules_Container.To_Index(I);
         end if;
         Amount := Amount + Modules_List(TmpShip.Modules(I).ProtoIndex).Size;
      end loop;
      TmpShip.Modules(HullIndex).Data(1) := Amount;
      for Recipe of ProtoShip.KnownRecipes loop
         Known_Recipes.Append(New_Item => Recipe);
      end loop;
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
         Bases_Loop :
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
      -- Set home base for crew members
      for Member of TmpShip.Crew loop
         if GetRandom(1, 100) < 99 then
            Member.HomeBase := TmpShip.HomeBase;
         else
            Member.HomeBase := GetRandom(SkyBases'First, SkyBases'Last);
         end if;
      end loop;
      return TmpShip;
   end CreateShip;

   procedure LoadShips(Reader: Tree_Reader) is
      NodesList, ChildNodes: Node_List;
      ShipsData: Document;
      TempRecord: ProtoShipData;
      TempModules, TempRecipes: Positive_Container.Vector;
      TempCargo, TempCrew: Skills_Container.Vector;
      ModuleAmount: Positive;
      Index: Natural;
      RemoveIndex: Unbounded_String;
      procedure CountAmmoValue(ItemTypeIndex, Multiple: Positive) is
      begin
         for Item of TempRecord.Cargo loop
            if Items_List(Item(1)).IType = Items_Types(ItemTypeIndex) then
               TempRecord.CombatValue :=
                 TempRecord.CombatValue +
                 (Items_List(Item(1)).Value(1) * Multiple);
            end if;
         end loop;
      end CountAmmoValue;
   begin
      TempRecord :=
        (Name => Null_Unbounded_String, Modules => TempModules,
         Accuracy => (0, 0), CombatAI => NONE, Evasion => (0, 0),
         Loot => (0, 0), Perception => (0, 0), Cargo => TempCargo,
         CombatValue => 1, Crew => TempCrew,
         Description => Null_Unbounded_String, Owner => 1,
         Index => Null_Unbounded_String, KnownRecipes => TempRecipes);
      ShipsData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(ShipsData, "ship");
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord.Index :=
           To_Unbounded_String(Get_Attribute(Item(NodesList, I), "index"));
         TempRecord.Name :=
           To_Unbounded_String(Get_Attribute(Item(NodesList, I), "name"));
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "module");
         for J in 0 .. Length(ChildNodes) - 1 loop
            if Get_Attribute(Item(ChildNodes, J), "amount") /= "" then
               ModuleAmount :=
                 Positive'Value(Get_Attribute(Item(ChildNodes, J), "amount"));
            else
               ModuleAmount := 1;
            end if;
            Index :=
              FindProtoModule
                (To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "index")));
            if Index = 0 then
               raise Ships_Invalid_Data
                 with "Invalid module index: |" &
                 Get_Attribute(Item(ChildNodes, J), "index") & "| in " &
                 To_String(TempRecord.Name) & ".";
            end if;
            TempRecord.Modules.Append
              (New_Item => Index, Count => Count_Type(ModuleAmount));
         end loop;
         if Get_Attribute(Item(NodesList, I), "accuracy") /= "" then
            TempRecord.Accuracy(1) :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "accuracy"));
         elsif Get_Attribute(Item(NodesList, I), "minaccuracy") /= "" then
            TempRecord.Accuracy(1) :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "minaccuracy"));
            TempRecord.Accuracy(2) :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "maxaccuracy"));
         end if;
         if Get_Attribute(Item(NodesList, I), "combatai") /= "" then
            TempRecord.CombatAI :=
              ShipCombatAi'Value
                (Get_Attribute(Item(NodesList, I), "combatai"));
         end if;
         if Get_Attribute(Item(NodesList, I), "evasion") /= "" then
            TempRecord.Evasion(1) :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "evasion"));
         elsif Get_Attribute(Item(NodesList, I), "minevasion") /= "" then
            TempRecord.Evasion(1) :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "minevasion"));
            TempRecord.Evasion(2) :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "maxevasion"));
         end if;
         if Get_Attribute(Item(NodesList, I), "loot") /= "" then
            TempRecord.Loot(1) :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "loot"));
         elsif Get_Attribute(Item(NodesList, I), "minloot") /= "" then
            TempRecord.Loot(1) :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "minloot"));
            TempRecord.Loot(2) :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "maxloot"));
         end if;
         if Get_Attribute(Item(NodesList, I), "perception") /= "" then
            TempRecord.Perception(1) :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "perception"));
         elsif Get_Attribute(Item(NodesList, I), "minperception") /= "" then
            TempRecord.Perception(1) :=
              Integer'Value
                (Get_Attribute(Item(NodesList, I), "minperception"));
            TempRecord.Perception(2) :=
              Integer'Value
                (Get_Attribute(Item(NodesList, I), "maxperception"));
         end if;
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "cargo");
         for J in 0 .. Length(ChildNodes) - 1 loop
            Index :=
              FindProtoItem
                (To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "index")));
            if Index = 0 then
               raise Ships_Invalid_Data
                 with "Invalid item index: |" &
                 Get_Attribute(Item(ChildNodes, J), "index") & "| in " &
                 To_String(TempRecord.Name) & ".";
            end if;
            if Get_Attribute(Item(ChildNodes, J), "amount") /= "" then
               TempRecord.Cargo.Append
                 (New_Item =>
                    (Index,
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "amount")),
                     0));
            elsif Get_Attribute(Item(ChildNodes, J), "minamount") /= "" then
               TempRecord.Cargo.Append
                 (New_Item =>
                    (Index,
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "minamount")),
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "maxamount"))));
            end if;
         end loop;
         if Get_Attribute(Item(NodesList, I), "owner") /= "" then
            for J in Factions_List.Iterate loop
               if To_Lower(To_String(Factions_List(J).Index)) =
                 To_Lower(Get_Attribute(Item(NodesList, I), "owner")) then
                  TempRecord.Owner := Factions_Container.To_Index(J);
                  exit;
               end if;
            end loop;
         end if;
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "recipe");
         for J in 0 .. Length(ChildNodes) - 1 loop
            Index :=
              FindRecipe
                (To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "index")));
            if Index = 0 then
               raise Ships_Invalid_Data
                 with "Invalid recipe index: |" &
                 Get_Attribute(Item(ChildNodes, J), "index") & "| in " &
                 To_String(TempRecord.Name) & ".";
            end if;
            TempRecord.KnownRecipes.Append(New_Item => Index);
         end loop;
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "member");
         for J in 0 .. Length(ChildNodes) - 1 loop
            Index :=
              FindProtoMob
                (To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "index")));
            if Index = 0 then
               raise Ships_Invalid_Data
                 with "Invalid mob index: |" &
                 Get_Attribute(Item(ChildNodes, J), "index") & "| in " &
                 To_String(TempRecord.Name) & ".";
            end if;
            if Get_Attribute(Item(ChildNodes, J), "amount") /= "" then
               TempRecord.Crew.Append
                 (New_Item =>
                    (Index,
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "amount")),
                     0));
            elsif Get_Attribute(Item(ChildNodes, J), "minamount") /= "" then
               TempRecord.Crew.Append
                 (New_Item =>
                    (Index,
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "minamount")),
                     Integer'Value
                       (Get_Attribute(Item(ChildNodes, J), "maxamount"))));
            else
               TempRecord.Crew.Append(New_Item => (Index, 1, 0));
            end if;
         end loop;
         ChildNodes :=
           DOM.Core.Elements.Get_Elements_By_Tag_Name
             (Item(NodesList, I), "description");
         if Length(ChildNodes) > 0 then
            TempRecord.Description :=
              To_Unbounded_String
                (Node_Value(First_Child(Item(ChildNodes, 0))));
         end if;
         for ModuleIndex of TempRecord.Modules loop
            case Modules_List(ModuleIndex).MType is
               when HULL | GUN | BATTERING_RAM =>
                  TempRecord.CombatValue :=
                    TempRecord.CombatValue +
                    Modules_List(ModuleIndex).Durability +
                    (Modules_List(ModuleIndex).MaxValue * 10);
                  if Modules_List(ModuleIndex).MType = GUN then
                     CountAmmoValue(Modules_List(ModuleIndex).Value, 10);
                  end if;
               when ARMOR =>
                  TempRecord.CombatValue :=
                    TempRecord.CombatValue +
                    Modules_List(ModuleIndex).Durability;
               when HARPOON_GUN =>
                  TempRecord.CombatValue :=
                    TempRecord.CombatValue +
                    Modules_List(ModuleIndex).Durability +
                    (Modules_List(ModuleIndex).MaxValue * 5);
                  CountAmmoValue(Modules_List(ModuleIndex).Value, 5);
               when others =>
                  null;
            end case;
         end loop;
         TempRecord.CombatValue := TempRecord.CombatValue - 1;
         if Get_Attribute(Item(NodesList, I), "remove") = "" then
            ProtoShips_List.Append(New_Item => TempRecord);
            LogMessage
              ("Ship added: " & To_String(TempRecord.Name), Everything);
         else
            RemoveIndex :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "remove"));
            for J in ProtoShips_List.Iterate loop
               if ProtoShips_List(J).Index = RemoveIndex then
                  Index := ProtoShips_Container.To_Index(J);
                  exit;
               end if;
            end loop;
            ProtoShips_List.Delete(Index => Index);
            LogMessage("Ship removed: " & To_String(RemoveIndex), Everything);
         end if;
         TempRecord :=
           (Name => Null_Unbounded_String, Modules => TempModules,
            Accuracy => (0, 0), CombatAI => NONE, Evasion => (0, 0),
            Loot => (0, 0), Perception => (0, 0), Cargo => TempCargo,
            CombatValue => 1, Crew => TempCrew,
            Description => Null_Unbounded_String, Owner => 1,
            Index => Null_Unbounded_String, KnownRecipes => TempRecipes);
      end loop;
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

   function GenerateShipName
     (Owner: Unbounded_String)
      return Unbounded_String is -- based on name generator from libtcod
      NewName: Unbounded_String := Null_Unbounded_String;
      LettersAmount, NumbersAmount: Positive;
      subtype Letters is Character range 'A' .. 'Z';
      subtype Numbers is Character range '0' .. '9';
   begin
      for Faction of Factions_List loop
         if To_Lower(To_String(Faction.Index)) =
           To_Lower(To_String(Owner)) then
            if Faction.NamesType = ROBOTIC then
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
            else
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
            end if;
            exit;
         end if;
      end loop;
      return NewName;
   end GenerateShipName;

   function CountCombatValue return Natural is
      CombatValue: Natural := 0;
      procedure CountAmmoValue(ItemTypeIndex, Multiple: Positive) is
      begin
         for Item of PlayerShip.Cargo loop
            if Items_List(Item.ProtoIndex).IType =
              Items_Types(ItemTypeIndex) then
               CombatValue :=
                 CombatValue +
                 (Items_List(Item.ProtoIndex).Value(1) * Multiple);
            end if;
         end loop;
      end CountAmmoValue;
   begin
      for Module of PlayerShip.Modules loop
         case Modules_List(Module.ProtoIndex).MType is
            when HULL | GUN | BATTERING_RAM =>
               CombatValue :=
                 CombatValue + Module.MaxDurability + (Module.Data(2) * 10);
               if Modules_List(Module.ProtoIndex).MType = GUN then
                  CountAmmoValue(Modules_List(Module.ProtoIndex).Value, 10);
               end if;
            when ARMOR =>
               CombatValue := CombatValue + Module.MaxDurability;
            when HARPOON_GUN =>
               CombatValue :=
                 CombatValue + Module.MaxDurability + (Module.Data(2) * 5);
               CountAmmoValue(Modules_List(Module.ProtoIndex).Value, 5);
            when others =>
               null;
         end case;
      end loop;
      return CombatValue;
   end CountCombatValue;

end Ships;
