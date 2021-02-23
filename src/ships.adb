--    Copyright 2016-2021 Bartek thindil Jasicki
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
with Utils; use Utils;
with Log; use Log;
with Crafts; use Crafts;
with Maps; use Maps;
with Factions; use Factions;
with Bases; use Bases;
with ShipModules; use ShipModules;
with Ships.Crew; use Ships.Crew;

package body Ships is

   function CreateShip
     (ProtoIndex, Name: Unbounded_String; X: MapXRange; Y: MapYRange;
      Speed: ShipSpeed; RandomUpgrades: Boolean := True) return ShipRecord is
      TmpShip: ShipRecord;
      ShipModules: Modules_Container.Vector;
      ShipCrew: Crew_Container.Vector;
      NewName: Unbounded_String;
      HullIndex: Modules_Container.Extended_Index := 0;
      Amount: Natural := 0;
      ProtoShip: constant ProtoShipData := ProtoShips_List(ProtoIndex);
      ShipCargo: Inventory_Container.Vector;
      Owners: Natural_Container.Vector;
   begin
      -- Set ship modules
      declare
         WeightGain: Natural := 0;
         MaxValue: Positive;
         TempModule: BaseModule_Data;
         Roll: Positive range 1 .. 100;
         UpgradesAmount: Natural :=
           (if RandomUpgrades then
              GetRandom(0, Positive(ProtoShip.Modules.Length))
            else 0);
      begin
         Set_Modules_Loop :
         for Module of ProtoShip.Modules loop
            TempModule := Modules_List(Module);
            if UpgradesAmount = 0 or GetRandom(1, 100) < 51 then
               goto End_Of_Setting_Upgrades;
            end if;
            WeightGain :=
              Modules_List(Module).Weight / Modules_List(Module).Durability;
            if WeightGain < 1 then
               WeightGain := 1;
            end if;
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
               when 51 .. 75 => -- Upgrade value (depends on module) of module
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
                  if TempModule.MType in ENGINE | CABIN | GUN | BATTERING_RAM |
                        HULL | HARPOON_GUN then
                     MaxValue :=
                       Positive(Float(Modules_List(Module).MaxValue) * 1.5);
                     TempModule.MaxValue :=
                       GetRandom(Modules_List(Module).MaxValue, MaxValue);
                     TempModule.Weight :=
                       TempModule.Weight +
                       (WeightGain *
                        (TempModule.MaxValue - Modules_List(Module).MaxValue));
                  end if;
            end case;
            UpgradesAmount := UpgradesAmount - 1;
            <<End_Of_Setting_Upgrades>>
            Owners.Clear;
            if TempModule.MaxOwners > 0 then
               Set_Module_Owners_Loop :
               for I in 1 .. TempModule.MaxOwners loop
                  Owners.Append(0);
               end loop Set_Module_Owners_Loop;
            end if;
            case TempModule.MType is
               when ENGINE =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => ENGINE, Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE, FuelUsage => TempModule.Value,
                        Power => TempModule.MaxValue, Disabled => False));
               when CABIN =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => CABIN, Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE, Cleanliness => TempModule.Value,
                        Quality => TempModule.Value));
               when ALCHEMY_LAB .. GREENHOUSE =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => WORKSHOP, Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE,
                        CraftingIndex => Null_Unbounded_String,
                        CraftingTime => 0, CraftingAmount => 0));
               when MEDICAL_ROOM =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => MEDICAL_ROOM,
                        Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE));
               when COCKPIT =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => COCKPIT, Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE));
               when TRAINING_ROOM =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => TRAINING_ROOM,
                        Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE, TrainedSkill => 0));
               when TURRET =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => TURRET, Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE, GunIndex => 0));
               when GUN =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => GUN, Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE, Damage => TempModule.MaxValue,
                        AmmoIndex => 0));
               when CARGO =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => CARGO_ROOM, Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE));
               when HULL =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => HULL, Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE,
                        InstalledModules => TempModule.Value,
                        MaxModules => TempModule.MaxValue));
               when ARMOR =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => ARMOR, Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE));
               when BATTERING_RAM =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => BATTERING_RAM,
                        Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE, Damage2 => TempModule.MaxValue,
                        CoolingDown => False));
               when HARPOON_GUN =>
                  ShipModules.Append
                    (New_Item =>
                       (MType => HARPOON_GUN,
                        Name => Modules_List(Module).Name,
                        ProtoIndex => Module, Weight => TempModule.Weight,
                        Durability => TempModule.Durability,
                        MaxDurability => TempModule.Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE, Duration => TempModule.MaxValue,
                        HarpoonIndex => 0));
               when ANY =>
                  null;
            end case;
         end loop Set_Modules_Loop;
      end;
      -- Set ship name
      NewName :=
        (if Name = Null_Unbounded_String then ProtoShip.Name else Name);
      -- Set ship crew
      declare
         Member: Member_Data;
      begin
         Set_Crew_Loop :
         for ProtoMember of ProtoShip.Crew loop
            Amount :=
              (if ProtoMember.MaxAmount = 0 then ProtoMember.MinAmount
               else GetRandom(ProtoMember.MinAmount, ProtoMember.MaxAmount));
            Add_Crew_Member_Loop :
            for I in 1 .. Amount loop
               Member := GenerateMob(ProtoMember.ProtoIndex, ProtoShip.Owner);
               ShipCrew.Append(New_Item => Member);
               Modules_Loop :
               for Module of ShipModules loop
                  if Module.MType = CABIN then
                     Set_Cabin_Name_Loop :
                     for I in Module.Owner.Iterate loop
                        if Module.Owner(I) = 0 then
                           Module.Owner(I) := ShipCrew.Last_Index;
                           if Natural_Container.To_Index(I) = 1 then
                              Module.Name :=
                                Member.Name & To_Unbounded_String("'s Cabin");
                           end if;
                           exit Modules_Loop;
                        end if;
                     end loop Set_Cabin_Name_Loop;
                  end if;
               end loop Modules_Loop;
               Set_Module_Owner_Loop :
               for Module of ShipModules loop
                  if Module.Owner.Length > 0 then
                     if Module.Owner(1) = 0 and
                       ((Module.MType in GUN | HARPOON_GUN) and
                        Member.Order = Gunner) then
                        Module.Owner(1) := ShipCrew.Last_Index;
                        exit Set_Module_Owner_Loop;
                     elsif Module.MType = COCKPIT and Member.Order = Pilot then
                        Module.Owner(1) := ShipCrew.Last_Index;
                        exit Set_Module_Owner_Loop;
                     end if;
                  end if;
               end loop Set_Module_Owner_Loop;
            end loop Add_Crew_Member_Loop;
         end loop Set_Crew_Loop;
      end;
      -- Set ship cargo
      Set_Cargo_Loop :
      for I in ProtoShip.Cargo.Iterate loop
         Amount :=
           (if ProtoShip.Cargo(I).MaxAmount > 0 then
              GetRandom
                (ProtoShip.Cargo(I).MinAmount, ProtoShip.Cargo(I).MaxAmount)
            else ProtoShip.Cargo(I).MinAmount);
         ShipCargo.Append
           (New_Item =>
              (ProtoIndex => ProtoShip.Cargo(I).ProtoIndex, Amount => Amount,
               Name => Null_Unbounded_String, Durability => 100, Price => 0));
      end loop Set_Cargo_Loop;
      TmpShip :=
        (Name => NewName, SkyX => X, SkyY => Y, Speed => Speed,
         Modules => ShipModules, Cargo => ShipCargo, Crew => ShipCrew,
         UpgradeModule => 0, DestinationX => 0, DestinationY => 0,
         RepairModule => 0, Description => ProtoShip.Description,
         HomeBase => 0);
      declare
         GunAssigned: Boolean;
      begin
         Amount := 0;
         Count_Modules_Loop :
         for I in TmpShip.Modules.Iterate loop
            if TmpShip.Modules(I).MType = TURRET then
               Count_Guns_Loop :
               for J in TmpShip.Modules.Iterate loop
                  if TmpShip.Modules(J).MType in GUN | HARPOON_GUN then
                     GunAssigned := False;
                     Check_Assigned_Guns_Loop :
                     for K in TmpShip.Modules.Iterate loop
                        if TmpShip.Modules(K).MType = TURRET
                          and then TmpShip.Modules(K).GunIndex =
                            Modules_Container.To_Index(J) then
                           GunAssigned := True;
                           exit Check_Assigned_Guns_Loop;
                        end if;
                     end loop Check_Assigned_Guns_Loop;
                     if not GunAssigned then
                        TmpShip.Modules(I).GunIndex :=
                          Modules_Container.To_Index(J);
                     end if;
                  end if;
               end loop Count_Guns_Loop;
            elsif TmpShip.Modules(I).MType = HULL then
               HullIndex := Modules_Container.To_Index(I);
            end if;
            if Modules_List(TmpShip.Modules(I).ProtoIndex).MType not in GUN |
                  HARPOON_GUN | ARMOR | HULL then
               Amount :=
                 Amount + Modules_List(TmpShip.Modules(I).ProtoIndex).Size;
            end if;
         end loop Count_Modules_Loop;
         TmpShip.Modules(HullIndex).InstalledModules := Amount;
      end;
      -- Set known crafting recipes
      Set_Known_Recipes_Loop :
      for Recipe of ProtoShip.KnownRecipes loop
         Known_Recipes.Append(New_Item => Recipe);
      end loop Set_Known_Recipes_Loop;
      -- Set home base for ship
      if SkyMap(X, Y).BaseIndex > 0 then
         TmpShip.HomeBase := SkyMap(X, Y).BaseIndex;
      else
         declare
            StartX, StartY, EndX, EndY: Integer;
         begin
            StartX := X - 100;
            NormalizeCoord(StartX);
            StartY := Y - 100;
            NormalizeCoord(StartY, False);
            EndX := X + 100;
            NormalizeCoord(EndX);
            EndY := Y + 100;
            NormalizeCoord(EndY, False);
            Bases_X_Loop :
            for SkyX in StartX .. EndX loop
               Bases_Y_Loop :
               for SkyY in StartY .. EndY loop
                  if SkyMap(SkyX, SkyY).BaseIndex > 0 then
                     if SkyBases(SkyMap(SkyX, SkyY).BaseIndex).Owner =
                       ProtoShip.Owner then
                        TmpShip.HomeBase := SkyMap(SkyX, SkyY).BaseIndex;
                        exit Bases_X_Loop;
                     end if;
                  end if;
               end loop Bases_Y_Loop;
            end loop Bases_X_Loop;
            if TmpShip.HomeBase = 0 then
               Set_Home_Base_Loop :
               for I in SkyBases'Range loop
                  if SkyBases(I).Owner = ProtoShip.Owner then
                     TmpShip.HomeBase := I;
                     exit Set_Home_Base_Loop;
                  end if;
               end loop Set_Home_Base_Loop;
            end if;
         end;
      end if;
      -- Set home base for crew members
      Set_Home_For_Members_Loop :
      for Member of TmpShip.Crew loop
         Member.HomeBase :=
           (if GetRandom(1, 100) < 99 then TmpShip.HomeBase
            else GetRandom(SkyBases'First, SkyBases'Last));
      end loop Set_Home_For_Members_Loop;
      return TmpShip;
   end CreateShip;

   procedure LoadShips(Reader: Tree_Reader) is
      NodesList, ChildNodes: Node_List;
      ShipsData: Document;
      TempRecord: ProtoShipData;
      TempModules: UnboundedString_Container.Vector;
      TempCargo: MobInventory_Container.Vector;
      TempCrew: ProtoCrew_Container.Vector;
      ModuleAmount, DeleteIndex: Positive;
      Action, SubAction: DataAction;
      ShipNode, ChildNode: Node;
      ItemIndex, RecipeIndex, MobIndex, ModuleIndex,
      ShipIndex: Unbounded_String;
      TempRecipes: UnboundedString_Container.Vector;
      procedure CountAmmoValue(ItemTypeIndex, Multiple: Positive) is
      begin
         Count_Ammo_Value_Loop :
         for I in TempRecord.Cargo.Iterate loop
            if Items_List(TempRecord.Cargo(I).ProtoIndex).IType =
              Items_Types(ItemTypeIndex) then
               TempRecord.CombatValue :=
                 TempRecord.CombatValue +
                 (Items_List(TempRecord.Cargo(I).ProtoIndex).Value(1) *
                  Multiple);
            end if;
         end loop Count_Ammo_Value_Loop;
      end CountAmmoValue;
   begin
      ShipsData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(ShipsData, "ship");
      Load_Proto_Ships_Loop :
      for I in 0 .. Length(NodesList) - 1 loop
         TempRecord :=
           (Name => Null_Unbounded_String, Modules => TempModules,
            Accuracy => (0, 0), CombatAI => NONE, Evasion => (0, 0),
            Loot => (0, 0), Perception => (0, 0), Cargo => TempCargo,
            CombatValue => 1, Crew => TempCrew,
            Description => Null_Unbounded_String,
            Owner => Factions_Container.Key(Factions_List.First),
            KnownRecipes => TempRecipes);
         ShipNode := Item(NodesList, I);
         ShipIndex := To_Unbounded_String(Get_Attribute(ShipNode, "index"));
         Action :=
           (if Get_Attribute(ShipNode, "action")'Length > 0 then
              DataAction'Value(Get_Attribute(ShipNode, "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not ProtoShips_Container.Contains
                (ProtoShips_List, ShipIndex) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(DataAction'Image(Action)) &
                 " ship '" & To_String(ShipIndex) &
                 "', there is no ship with that index.";
            end if;
         elsif ProtoShips_Container.Contains(ProtoShips_List, ShipIndex) then
            raise Data_Loading_Error
              with "Can't add ship '" & To_String(ShipIndex) &
              "', there is already a ship with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := ProtoShips_List(ShipIndex);
            end if;
            if Get_Attribute(ShipNode, "name")'Length > 0 then
               TempRecord.Name :=
                 To_Unbounded_String(Get_Attribute(ShipNode, "name"));
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(ShipNode, "module");
            Load_Modules_Loop :
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               ModuleAmount :=
                 (if Get_Attribute(ChildNode, "amount") /= "" then
                    Positive'Value(Get_Attribute(ChildNode, "amount"))
                  else 1);
               ModuleIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               if not BaseModules_Container.Contains
                   (Modules_List, ModuleIndex) then
                  raise Ships_Invalid_Data
                    with "Invalid module index: |" &
                    Get_Attribute(ChildNode, "index") & "| in " &
                    To_String(TempRecord.Name) & ".";
               end if;
               SubAction :=
                 (if Get_Attribute(ChildNode, "action")'Length > 0 then
                    DataAction'Value(Get_Attribute(ChildNode, "action"))
                  else ADD);
               if SubAction = ADD then
                  TempRecord.Modules.Append
                    (New_Item => ModuleIndex,
                     Count => Count_Type(ModuleAmount));
               else
                  Find_Delete_Module_Loop :
                  for K in TempRecord.Modules.Iterate loop
                     if TempRecord.Modules(K) = ModuleIndex then
                        DeleteIndex := UnboundedString_Container.To_Index(K);
                        exit Find_Delete_Module_Loop;
                     end if;
                  end loop Find_Delete_Module_Loop;
                  TempRecord.Modules.Delete
                    (Index => DeleteIndex, Count => Count_Type(ModuleAmount));
               end if;
            end loop Load_Modules_Loop;
            if Get_Attribute(ShipNode, "accuracy") /= "" then
               TempRecord.Accuracy(1) :=
                 Integer'Value(Get_Attribute(ShipNode, "accuracy"));
               TempRecord.Accuracy(2) := 0;
            elsif Get_Attribute(ShipNode, "minaccuracy") /= "" then
               TempRecord.Accuracy(1) :=
                 Integer'Value(Get_Attribute(ShipNode, "minaccuracy"));
               TempRecord.Accuracy(2) :=
                 Integer'Value(Get_Attribute(ShipNode, "maxaccuracy"));
               if TempRecord.Accuracy(2) < TempRecord.Accuracy(1) then
                  raise Ships_Invalid_Data
                    with "Can't add ship '" & To_String(ShipIndex) &
                    "', invalid range for accuracy.";
               end if;
            end if;
            if Get_Attribute(ShipNode, "combatai") /= "" then
               TempRecord.CombatAI :=
                 ShipCombatAi'Value(Get_Attribute(ShipNode, "combatai"));
            end if;
            if Get_Attribute(ShipNode, "evasion") /= "" then
               TempRecord.Evasion(1) :=
                 Integer'Value(Get_Attribute(ShipNode, "evasion"));
               TempRecord.Evasion(2) := 0;
            elsif Get_Attribute(ShipNode, "minevasion") /= "" then
               TempRecord.Evasion(1) :=
                 Integer'Value(Get_Attribute(ShipNode, "minevasion"));
               TempRecord.Evasion(2) :=
                 Integer'Value(Get_Attribute(ShipNode, "maxevasion"));
               if TempRecord.Evasion(2) < TempRecord.Evasion(1) then
                  raise Ships_Invalid_Data
                    with "Can't add ship '" & To_String(ShipIndex) &
                    "', invalid range for evasion.";
               end if;
            end if;
            if Get_Attribute(ShipNode, "loot") /= "" then
               TempRecord.Loot(1) :=
                 Integer'Value(Get_Attribute(ShipNode, "loot"));
               TempRecord.Loot(2) := 0;
            elsif Get_Attribute(ShipNode, "minloot") /= "" then
               TempRecord.Loot(1) :=
                 Integer'Value(Get_Attribute(ShipNode, "minloot"));
               TempRecord.Loot(2) :=
                 Integer'Value(Get_Attribute(ShipNode, "maxloot"));
               if TempRecord.Loot(2) < TempRecord.Loot(1) then
                  raise Ships_Invalid_Data
                    with "Can't add ship '" & To_String(ShipIndex) &
                    "', invalid range for loot.";
               end if;
            end if;
            if Get_Attribute(ShipNode, "perception") /= "" then
               TempRecord.Perception(1) :=
                 Integer'Value(Get_Attribute(ShipNode, "perception"));
               TempRecord.Perception(2) := 0;
            elsif Get_Attribute(ShipNode, "minperception") /= "" then
               TempRecord.Perception(1) :=
                 Integer'Value(Get_Attribute(ShipNode, "minperception"));
               TempRecord.Perception(2) :=
                 Integer'Value(Get_Attribute(ShipNode, "maxperception"));
               if TempRecord.Perception(2) < TempRecord.Perception(1) then
                  raise Ships_Invalid_Data
                    with "Can't add ship '" & To_String(ShipIndex) &
                    "', invalid range for perception.";
               end if;
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(ShipNode, "cargo");
            Load_Cargo_Loop:
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               ItemIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               if not Objects_Container.Contains(Items_List, ItemIndex) then
                  raise Ships_Invalid_Data
                    with "Invalid item index: |" &
                    Get_Attribute(ChildNode, "index") & "| in " &
                    To_String(TempRecord.Name) & ".";
               end if;
               SubAction :=
                 (if Get_Attribute(ChildNode, "action")'Length > 0 then
                    DataAction'Value(Get_Attribute(ChildNode, "action"))
                  else ADD);
               case SubAction is
                  when ADD =>
                     if Get_Attribute(ChildNode, "amount")'Length /= 0 then
                        TempRecord.Cargo.Append
                          (New_Item =>
                             (ItemIndex,
                              Integer'Value
                                (Get_Attribute(ChildNode, "amount")),
                              0));
                     else
                        if Integer'Value
                            (Get_Attribute(ChildNode, "maxamount")) <
                          Integer'Value
                            (Get_Attribute(ChildNode, "minamount")) then
                           raise Ships_Invalid_Data
                             with "Invalid amount range for item : |" &
                             Get_Attribute(ChildNode, "index") & "| in " &
                             To_String(TempRecord.Name) & ".";
                        end if;
                        TempRecord.Cargo.Append
                          (New_Item =>
                             (ItemIndex,
                              Integer'Value
                                (Get_Attribute(ChildNode, "minamount")),
                              Integer'Value
                                (Get_Attribute(ChildNode, "maxamount"))));
                     end if;
                  when UPDATE =>
                     Update_Cargo_Loop:
                     for Item of TempRecord.Cargo loop
                        if Item.ProtoIndex = ItemIndex then
                           if Get_Attribute(ChildNode, "amount")'Length /=
                             0 then
                              Item :=
                                (ItemIndex,
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "amount")),
                                 0);
                           else
                              if Integer'Value
                                  (Get_Attribute(ChildNode, "maxamount")) <
                                Integer'Value
                                  (Get_Attribute(ChildNode, "minamount")) then
                                 raise Ships_Invalid_Data
                                   with "Invalid amount range for item : |" &
                                   Get_Attribute(ChildNode, "index") &
                                   "| in " & To_String(TempRecord.Name) & ".";
                              end if;
                              Item :=
                                (ItemIndex,
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "minamount")),
                                 Integer'Value
                                   (Get_Attribute(ChildNode, "maxamount")));
                           end if;
                           exit Update_Cargo_Loop;
                        end if;
                     end loop Update_Cargo_Loop;
                  when REMOVE =>
                     declare
                        DeleteIndex: Natural := 0;
                     begin
                        Find_Delete_Cargo_Loop:
                        for K in
                          TempRecord.Cargo.First_Index ..
                            TempRecord.Cargo.Last_Index loop
                           if TempRecord.Cargo(K).ProtoIndex = ItemIndex then
                              DeleteIndex := K;
                              exit Find_Delete_Cargo_Loop;
                           end if;
                        end loop Find_Delete_Cargo_Loop;
                        if DeleteIndex > 0 then
                           TempRecord.Cargo.Delete(DeleteIndex);
                        end if;
                     end;
               end case;
            end loop Load_Cargo_Loop;
            if Get_Attribute(ShipNode, "owner") /= "" then
               TempRecord.Owner :=
                 To_Unbounded_String(Get_Attribute(ShipNode, "owner"));
            end if;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(ShipNode, "recipe");
            Load_Known_Recipes_Loop:
            for J in 0 .. Length(ChildNodes) - 1 loop
               RecipeIndex :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "index"));
               if not Recipes_Container.Contains
                   (Recipes_List, RecipeIndex) then
                  raise Ships_Invalid_Data
                    with "Invalid recipe index: |" &
                    Get_Attribute(Item(ChildNodes, J), "index") & "| in " &
                    To_String(TempRecord.Name) & ".";
               end if;
               SubAction :=
                 (if Get_Attribute(ChildNode, "action")'Length > 0 then
                    DataAction'Value(Get_Attribute(ChildNode, "action"))
                  else ADD);
               if SubAction = ADD then
                  TempRecord.KnownRecipes.Append(New_Item => RecipeIndex);
               else
                  Find_Delete_Recipe_Loop:
                  for K in TempRecord.KnownRecipes.Iterate loop
                     if TempRecord.KnownRecipes(K) = RecipeIndex then
                        DeleteIndex := UnboundedString_Container.To_Index(K);
                        exit Find_Delete_Recipe_Loop;
                     end if;
                  end loop Find_Delete_Recipe_Loop;
                  TempRecord.KnownRecipes.Delete(Index => DeleteIndex);
               end if;
            end loop Load_Known_Recipes_Loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(ShipNode, "member");
            Load_Crew_Loop:
            for J in 0 .. Length(ChildNodes) - 1 loop
               ChildNode := Item(ChildNodes, J);
               MobIndex :=
                 To_Unbounded_String(Get_Attribute(ChildNode, "index"));
               if not ProtoMobs_Container.Contains
                   (ProtoMobs_List, MobIndex) then
                  raise Ships_Invalid_Data
                    with "Invalid mob index: |" &
                    Get_Attribute(ChildNode, "index") & "| in " &
                    To_String(TempRecord.Name) & ".";
               end if;
               SubAction :=
                 (if Get_Attribute(ChildNode, "action")'Length > 0 then
                    DataAction'Value(Get_Attribute(ChildNode, "action"))
                  else ADD);
               case SubAction is
                  when ADD =>
                     if Get_Attribute(ChildNode, "amount") /= "" then
                        TempRecord.Crew.Append
                          (New_Item =>
                             (MobIndex,
                              Integer'Value
                                (Get_Attribute(ChildNode, "amount")),
                              0));
                     elsif Get_Attribute(ChildNode, "minamount") /= "" then
                        if Integer'Value
                            (Get_Attribute(ChildNode, "maxamount")) <
                          Integer'Value
                            (Get_Attribute(ChildNode, "minamount")) then
                           raise Ships_Invalid_Data
                             with "Invalid amount range for member : |" &
                             Get_Attribute(ChildNode, "index") & "| in " &
                             To_String(TempRecord.Name) & ".";
                        end if;
                        TempRecord.Crew.Append
                          (New_Item =>
                             (MobIndex,
                              Integer'Value
                                (Get_Attribute(ChildNode, "minamount")),
                              Integer'Value
                                (Get_Attribute(ChildNode, "maxamount"))));
                     else
                        TempRecord.Crew.Append(New_Item => (MobIndex, 1, 0));
                     end if;
                  when UPDATE =>
                     Update_Crew_Loop:
                     for Member of TempRecord.Crew loop
                        if Member.ProtoIndex = MobIndex then
                           if Get_Attribute(ChildNode, "amount") /= "" then
                              Member.MinAmount :=
                                Integer'Value
                                  (Get_Attribute(ChildNode, "amount"));
                              Member.MaxAmount := 0;
                           elsif Get_Attribute(ChildNode, "minamount") /=
                             "" then
                              if Integer'Value
                                  (Get_Attribute(ChildNode, "maxamount")) <
                                Integer'Value
                                  (Get_Attribute(ChildNode, "minamount")) then
                                 raise Ships_Invalid_Data
                                   with "Invalid amount range for member : |" &
                                   Get_Attribute(ChildNode, "index") &
                                   "| in " & To_String(TempRecord.Name) & ".";
                              end if;
                              Member.MinAmount :=
                                Integer'Value
                                  (Get_Attribute(ChildNode, "minamount"));
                              Member.MaxAmount :=
                                Integer'Value
                                  (Get_Attribute(ChildNode, "maxamount"));
                           else
                              Member.MinAmount := 1;
                              Member.MaxAmount := 0;
                           end if;
                           exit Update_Crew_Loop;
                        end if;
                     end loop Update_Crew_Loop;
                  when REMOVE =>
                     Find_Delete_Crew_Loop:
                     for K in TempRecord.Crew.Iterate loop
                        if TempRecord.Crew(K).ProtoIndex = MobIndex then
                           DeleteIndex := ProtoCrew_Container.To_Index(K);
                           exit Find_Delete_Crew_Loop;
                        end if;
                     end loop Find_Delete_Crew_Loop;
                     TempRecord.Crew.Delete(Index => DeleteIndex);
               end case;
            end loop Load_Crew_Loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (ShipNode, "description");
            if Length(ChildNodes) > 0 then
               TempRecord.Description :=
                 To_Unbounded_String
                   (Node_Value(First_Child(Item(ChildNodes, 0))));
            end if;
            Count_Combat_Value_Loop:
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
            end loop Count_Combat_Value_Loop;
            TempRecord.CombatValue := TempRecord.CombatValue - 1;
            if Action /= UPDATE then
               ProtoShips_Container.Include
                 (ProtoShips_List, ShipIndex, TempRecord);
               LogMessage
                 ("Ship added: " & To_String(TempRecord.Name), Everything);
            else
               ProtoShips_List(ShipIndex) := TempRecord;
            end if;
         else
            ProtoShips_Container.Exclude(ProtoShips_List, ShipIndex);
            LogMessage("Ship removed: " & To_String(ShipIndex), Everything);
         end if;
      end loop Load_Proto_Ships_Loop;
   end LoadShips;

   function CountShipWeight(Ship: ShipRecord) return Positive is
      Weight: Natural := 0;
      CargoWeight: Positive;
   begin
      Count_Ship_Weight_Loop:
      for Module of Ship.Modules loop
         Weight := Weight + Module.Weight;
      end loop Count_Ship_Weight_Loop;
      Count_Cargo_Weight_Loop:
      for Item of Ship.Cargo loop
         CargoWeight := Item.Amount * Items_List(Item.ProtoIndex).Weight;
         Weight := Weight + CargoWeight;
      end loop Count_Cargo_Weight_Loop;
      return Weight;
   end CountShipWeight;

   function GenerateShipName
     (Owner: Unbounded_String) return Unbounded_String is
      NewName: Unbounded_String := Null_Unbounded_String;
   begin
      Generate_Ship_Name_Loop:
      for I in Factions_List.Iterate loop
         if Factions_Container.Key(I) = Owner then
            if Factions_List(I).NamesType = ROBOTIC then
               NewName := GenerateRoboticName;
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
            exit Generate_Ship_Name_Loop;
         end if;
      end loop Generate_Ship_Name_Loop;
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
            when BATTERING_RAM =>
               CombatValue :=
                 CombatValue + Module.MaxDurability + (Module.Damage2 * 10);
            when GUN =>
               CombatValue :=
                 CombatValue + Module.MaxDurability + (Module.Damage * 10);
               CountAmmoValue(Modules_List(Module.ProtoIndex).Value, 10);
            when ARMOR =>
               CombatValue := CombatValue + Module.MaxDurability;
            when HARPOON_GUN =>
               CombatValue :=
                 CombatValue + Module.MaxDurability + (Module.Duration * 5);
               CountAmmoValue(Modules_List(Module.ProtoIndex).Value, 5);
            when HULL =>
               CombatValue :=
                 CombatValue + Module.MaxDurability + (Module.MaxModules * 10);
            when others =>
               null;
         end case;
      end loop;
      return CombatValue;
   end CountCombatValue;

   function GetCabinQuality(Quality: Natural) return String is
   begin
      case Quality is
         when 0 .. 10 =>
            return "Empty room";
         when 11 .. 20 =>
            return "Minimal quality";
         when 21 .. 30 =>
            return "Basic quality";
         when 31 .. 40 =>
            return "Second class";
         when 41 .. 50 =>
            return "Medium quality";
         when 51 .. 60 =>
            return "First class";
         when 61 .. 70 =>
            return "Extended quality";
         when 71 .. 80 =>
            return "Encrusted room";
         when 81 .. 90 =>
            return "Luxury quality";
         when others =>
            return "Palace room";
      end case;
   end GetCabinQuality;

   procedure DamageModule
     (Ship: in out ShipRecord; ModuleIndex: Modules_Container.Extended_Index;
      Damage: Positive; DeathReason: String) is
      RealDamage: Natural := Damage;
      WeaponIndex: Natural;
      procedure RemoveGun(ModuleIndex2: Positive) is
      begin
         if Ship.Modules(ModuleIndex2).Owner(1) > 0 then
            Death
              (Ship.Modules(ModuleIndex2).Owner(1),
               To_Unbounded_String(DeathReason), Ship);
         end if;
      end RemoveGun;
   begin
      if Damage > Ship.Modules(ModuleIndex).Durability then
         RealDamage := Ship.Modules(ModuleIndex).Durability;
      end if;
      Ship.Modules(ModuleIndex).Durability :=
        Ship.Modules(ModuleIndex).Durability - RealDamage;
      if Ship.Modules(ModuleIndex).Durability = 0 then
         case Modules_List(Ship.Modules(ModuleIndex).ProtoIndex).MType is
            when HULL | ENGINE =>
               if Ship = PlayerShip then
                  Death(1, To_Unbounded_String(DeathReason), PlayerShip);
               end if;
            when TURRET =>
               WeaponIndex := Ship.Modules(ModuleIndex).GunIndex;
               if WeaponIndex > 0 then
                  Ship.Modules(WeaponIndex).Durability := 0;
                  RemoveGun(WeaponIndex);
               end if;
            when GUN =>
               RemoveGun(ModuleIndex);
            when CABIN =>
               for Owner of Ship.Modules(ModuleIndex).Owner loop
                  if Owner > 0 and then Ship.Crew(Owner).Order = Rest then
                     Death(Owner, To_Unbounded_String(DeathReason), Ship);
                  end if;
               end loop;
            when others =>
               if Ship.Modules(ModuleIndex).Owner.Length > 0 then
                  if Ship.Modules(ModuleIndex).Owner(1) > 0
                    and then
                      Ship.Crew(Ship.Modules(ModuleIndex).Owner(1)).Order /=
                      Rest then
                     Death
                       (Ship.Modules(ModuleIndex).Owner(1),
                        To_Unbounded_String(DeathReason), Ship);
                  end if;
               end if;
         end case;
      end if;
   end DamageModule;

end Ships;
