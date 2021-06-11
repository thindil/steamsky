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

   function Create_Ship
     (Proto_Index, Name: Unbounded_String; X: Map_X_Range; Y: Map_Y_Range;
      Speed: Ship_Speed; Random_Upgrades: Boolean := True)
      return Ship_Record is
      Tmp_Ship: Ship_Record := Empty_Ship;
      Ship_Modules: Modules_Container.Vector := Modules_Container.Empty_Vector;
      Ship_Crew: Crew_Container.Vector := Crew_Container.Empty_Vector;
      New_Name: Unbounded_String := Null_Unbounded_String;
      Hull_Index: Modules_Container.Extended_Index := 0;
      Amount: Natural := 0;
      Proto_Ship: constant Proto_Ship_Data := Proto_Ships_List(Proto_Index);
      Ship_Cargo: Inventory_Container.Vector :=
        Inventory_Container.Empty_Vector;
      Owners: Natural_Container.Vector := Natural_Container.Empty_Vector;
   begin
      -- Set ship modules
      Set_Modules_Block :
      declare
         Weight_Gain: Natural := 0;
         Max_Upgrade_Value: Positive := 1;
         Temp_Module: BaseModule_Data := (others => <>);
         Roll: Positive range 1 .. 100 := 1;
         Upgrades_Amount: Natural :=
           (if Random_Upgrades then
              GetRandom(Min => 0, Max => Positive(Proto_Ship.Modules.Length))
            else 0);
      begin
         Set_Modules_Loop :
         for Module of Proto_Ship.Modules loop
            Temp_Module := Modules_List(Module);
            if Upgrades_Amount = 0 or GetRandom(Min => 1, Max => 100) < 51 then
               goto End_Of_Setting_Upgrades;
            end if;
            Weight_Gain :=
              Modules_List(Module).Weight / Modules_List(Module).Durability;
            if Weight_Gain < 1 then
               Weight_Gain := 1;
            end if;
            Roll := GetRandom(Min => 1, Max => 100);
            case Roll is
               when 1 .. 50 => -- Upgrade durability of module
                  Max_Upgrade_Value :=
                    Positive(Float(Modules_List(Module).Durability) * 1.5);
                  Temp_Module.Durability :=
                    GetRandom
                      (Min => Modules_List(Module).Durability,
                       Max => Max_Upgrade_Value);
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Temp_Module.Weight :=
                    Temp_Module.Weight +
                    (Weight_Gain *
                     (Temp_Module.Durability -
                      Modules_List(Module).Durability));
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
               when 51 .. 75 => -- Upgrade value (depends on module) of module
                  if Modules_List(Module).MType = ENGINE then
                     Weight_Gain := Weight_Gain * 10;
                     Max_Upgrade_Value :=
                       Positive(Float(Modules_List(Module).Value) / 2.0);
                     Temp_Module.Value :=
                       GetRandom
                         (Min => Max_Upgrade_Value,
                          Max => Modules_List(Module).Value);
                     --## rule off SIMPLIFIABLE_EXPRESSIONS
                     Temp_Module.Weight :=
                       Temp_Module.Weight +
                       (Weight_Gain *
                        (Modules_List(Module).Value - Temp_Module.Value));
                     --## rule on SIMPLIFIABLE_EXPRESSIONS
                  end if;
               when 76 ..
                     100 => -- Upgrade max_value (depends on module) of module
                  case Modules_List(Module).MType is
                     when HULL =>
                        Weight_Gain := Weight_Gain * 10;
                     when ENGINE =>
                        Weight_Gain := 1;
                     when others =>
                        null;
                  end case;
                  if Temp_Module.MType in ENGINE | CABIN | GUN |
                        BATTERING_RAM | HULL | HARPOON_GUN then
                     Max_Upgrade_Value :=
                       Positive(Float(Modules_List(Module).MaxValue) * 1.5);
                     Temp_Module.MaxValue :=
                       GetRandom
                         (Min => Modules_List(Module).MaxValue,
                          Max => Max_Upgrade_Value);
                     --## rule off SIMPLIFIABLE_EXPRESSIONS
                     Temp_Module.Weight :=
                       Temp_Module.Weight +
                       (Weight_Gain *
                        (Temp_Module.MaxValue -
                         Modules_List(Module).MaxValue));
                     --## rule on SIMPLIFIABLE_EXPRESSIONS
                  end if;
            end case;
            Upgrades_Amount := Upgrades_Amount - 1;
            <<End_Of_Setting_Upgrades>>
            Owners.Clear;
            if Temp_Module.MaxOwners > 0 then
               Set_Module_Owners_Loop :
               for I in 1 .. Temp_Module.MaxOwners loop
                  Owners.Append(New_Item => 0);
               end loop Set_Module_Owners_Loop;
            end if;
            case Temp_Module.MType is
               when ENGINE =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => ENGINE, Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Fuel_Usage => Temp_Module.Value,
                        Power => Temp_Module.MaxValue, Disabled => False));
               when CABIN =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => CABIN, Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Cleanliness => Temp_Module.Value,
                        Quality => Temp_Module.Value));
               when ALCHEMY_LAB .. GREENHOUSE =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => WORKSHOP, Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Crafting_Index => Null_Unbounded_String,
                        Crafting_Time => 0, Crafting_Amount => 0));
               when MEDICAL_ROOM =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => MEDICAL_ROOM,
                        Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when COCKPIT =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => COCKPIT, Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when TRAINING_ROOM =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => TRAINING_ROOM,
                        Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE, Trained_Skill => 0));
               when TURRET =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => TURRET, Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE, Gun_Index => 0));
               when GUN =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => GUN, Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE, Damage => Temp_Module.MaxValue,
                        Ammo_Index => 0));
               when CARGO =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => CARGO_ROOM,
                        Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when HULL =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => HULL, Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Installed_Modules => Temp_Module.Value,
                        Max_Modules => Temp_Module.MaxValue));
               when ARMOR =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => ARMOR, Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE));
               when BATTERING_RAM =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => BATTERING_RAM,
                        Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Damage2 => Temp_Module.MaxValue,
                        Cooling_Down => False));
               when HARPOON_GUN =>
                  Ship_Modules.Append
                    (New_Item =>
                       (M_Type => HARPOON_GUN,
                        Name => Modules_List(Module).Name,
                        Proto_Index => Module, Weight => Temp_Module.Weight,
                        Durability => Temp_Module.Durability,
                        Max_Durability => Temp_Module.Durability,
                        Owner => Owners, Upgrade_Progress => 0,
                        Upgrade_Action => NONE,
                        Duration => Temp_Module.MaxValue, Harpoon_Index => 0));
               when ANY =>
                  null;
            end case;
         end loop Set_Modules_Loop;
      end Set_Modules_Block;
      -- Set ship name
      New_Name :=
        (if Name = Null_Unbounded_String then Proto_Ship.Name else Name);
      -- Set ship crew
      Set_Ship_Crew_Block :
      declare
         Member: Member_Data := Member_Data'(others => <>);
      begin
         Set_Crew_Loop :
         for ProtoMember of Proto_Ship.Crew loop
            Amount :=
              (if ProtoMember.Max_Amount = 0 then ProtoMember.Min_Amount
               else GetRandom
                   (Min => ProtoMember.Min_Amount,
                    Max => ProtoMember.Max_Amount));
            Add_Crew_Member_Loop :
            for I in 1 .. Amount loop
               Member :=
                 GenerateMob
                   (MobIndex => ProtoMember.Proto_Index,
                    FactionIndex => Proto_Ship.Owner);
               Ship_Crew.Append(New_Item => Member);
               Modules_Loop :
               for Module of Ship_Modules loop
                  if Module.M_Type = CABIN then
                     Set_Cabin_Name_Loop :
                     for J in Module.Owner.Iterate loop
                        if Module.Owner(J) = 0 then
                           Module.Owner(J) := Ship_Crew.Last_Index;
                           if Natural_Container.To_Index(J) = 1 then
                              Module.Name :=
                                Member.Name & To_Unbounded_String("'s Cabin");
                           end if;
                           exit Modules_Loop;
                        end if;
                     end loop Set_Cabin_Name_Loop;
                  end if;
               end loop Modules_Loop;
               Set_Module_Owner_Loop :
               for Module of Ship_Modules loop
                  if Module.Owner.Length > 0 then
                     if Module.Owner(1) = 0 and
                       (Module.M_Type in GUN | HARPOON_GUN and
                        Member.Order = Gunner) then
                        Module.Owner(1) := Ship_Crew.Last_Index;
                        exit Set_Module_Owner_Loop;
                     elsif Module.M_Type = COCKPIT and
                       Member.Order = Pilot then
                        Module.Owner(1) := Ship_Crew.Last_Index;
                        exit Set_Module_Owner_Loop;
                     end if;
                  end if;
               end loop Set_Module_Owner_Loop;
            end loop Add_Crew_Member_Loop;
         end loop Set_Crew_Loop;
      end Set_Ship_Crew_Block;
      -- Set ship cargo
      Set_Cargo_Loop :
      for I in Proto_Ship.Cargo.Iterate loop
         Amount :=
           (if Proto_Ship.Cargo(I).MaxAmount > 0 then
              GetRandom
                (Proto_Ship.Cargo(I).MinAmount, Proto_Ship.Cargo(I).MaxAmount)
            else Proto_Ship.Cargo(I).MinAmount);
         Ship_Cargo.Append
           (New_Item =>
              (ProtoIndex => Proto_Ship.Cargo(I).ProtoIndex, Amount => Amount,
               Name => Null_Unbounded_String, Durability => 100, Price => 0));
      end loop Set_Cargo_Loop;
      Tmp_Ship :=
        (Name => New_Name, Sky_X => X, Sky_Y => Y, Speed => Speed,
         Modules => Ship_Modules, Cargo => Ship_Cargo, Crew => Ship_Crew,
         Upgrade_Module => 0, Destination_X => 0, Destination_Y => 0,
         Repair_Module => 0, Description => Proto_Ship.Description,
         Home_Base => 0);
      Assing_Gun_Block :
      declare
         Gun_Assigned: Boolean := False;
      begin
         Amount := 0;
         Count_Modules_Loop :
         for I in Tmp_Ship.Modules.Iterate loop
            if Tmp_Ship.Modules(I).M_Type = TURRET then
               Count_Guns_Loop :
               for J in Tmp_Ship.Modules.Iterate loop
                  if Tmp_Ship.Modules(J).M_Type in GUN | HARPOON_GUN then
                     Gun_Assigned := False;
                     Check_Assigned_Guns_Loop :
                     for K in Tmp_Ship.Modules.Iterate loop
                        if Tmp_Ship.Modules(K).M_Type = TURRET
                          and then Tmp_Ship.Modules(K).Gun_Index =
                            Modules_Container.To_Index(J) then
                           Gun_Assigned := True;
                           exit Check_Assigned_Guns_Loop;
                        end if;
                     end loop Check_Assigned_Guns_Loop;
                     if not Gun_Assigned then
                        Tmp_Ship.Modules(I).Gun_Index :=
                          Modules_Container.To_Index(J);
                     end if;
                  end if;
               end loop Count_Guns_Loop;
            elsif Tmp_Ship.Modules(I).M_Type = HULL then
               Hull_Index := Modules_Container.To_Index(I);
            end if;
            if Modules_List(Tmp_Ship.Modules(I).Proto_Index).MType not in GUN |
                  HARPOON_GUN | ARMOR | HULL then
               Amount :=
                 Amount + Modules_List(Tmp_Ship.Modules(I).Proto_Index).Size;
            end if;
         end loop Count_Modules_Loop;
         Tmp_Ship.Modules(Hull_Index).Installed_Modules := Amount;
      end Assing_Gun_Block;
      -- Set known crafting recipes
      Set_Known_Recipes_Loop :
      for Recipe of Proto_Ship.Known_Recipes loop
         Known_Recipes.Append(New_Item => Recipe);
      end loop Set_Known_Recipes_Loop;
      -- Set home base for ship
      if SkyMap(X, Y).BaseIndex > 0 then
         Tmp_Ship.Home_Base := SkyMap(X, Y).BaseIndex;
      else
         Find_Home_Base_Block :
         declare
            Start_X, Start_Y, End_X, End_Y: Integer;
         begin
            Start_X := X - 100;
            NormalizeCoord(Start_X);
            Start_Y := Y - 100;
            NormalizeCoord(Start_Y, False);
            End_X := X + 100;
            NormalizeCoord(End_X);
            End_Y := Y + 100;
            NormalizeCoord(End_Y, False);
            Bases_X_Loop :
            for SkyX in Start_X .. End_X loop
               Bases_Y_Loop :
               for SkyY in Start_Y .. End_Y loop
                  if SkyMap(SkyX, SkyY).BaseIndex > 0 then
                     if SkyBases(SkyMap(SkyX, SkyY).BaseIndex).Owner =
                       Proto_Ship.Owner then
                        Tmp_Ship.Home_Base := SkyMap(SkyX, SkyY).BaseIndex;
                        exit Bases_X_Loop;
                     end if;
                  end if;
               end loop Bases_Y_Loop;
            end loop Bases_X_Loop;
            if Tmp_Ship.Home_Base = 0 then
               Set_Home_Base_Loop :
               for I in SkyBases'Range loop
                  if SkyBases(I).Owner = Proto_Ship.Owner then
                     Tmp_Ship.Home_Base := I;
                     exit Set_Home_Base_Loop;
                  end if;
               end loop Set_Home_Base_Loop;
            end if;
         end Find_Home_Base_Block;
      end if;
      -- Set home base for crew members
      Set_Home_For_Members_Loop :
      for Member of Tmp_Ship.Crew loop
         Member.HomeBase :=
           (if GetRandom(1, 100) < 99 then Tmp_Ship.Home_Base
            else GetRandom(SkyBases'First, SkyBases'Last));
      end loop Set_Home_For_Members_Loop;
      return Tmp_Ship;
   end Create_Ship;

   procedure Load_Ships(Reader: Tree_Reader) is
      NodesList, ChildNodes: Node_List;
      ShipsData: Document;
      TempRecord: Proto_Ship_Data;
      Temp_Modules: UnboundedString_Container.Vector;
      TempCargo: MobInventory_Container.Vector;
      TempCrew: Proto_Crew_Container.Vector;
      ModuleAmount, DeleteIndex: Positive;
      Action, SubAction: Data_Action;
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
               TempRecord.Combat_Value :=
                 TempRecord.Combat_Value +
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
           (Name => Null_Unbounded_String, Modules => Temp_Modules,
            Accuracy => (0, 0), Combat_Ai => NONE, Evasion => (0, 0),
            Loot => (0, 0), Perception => (0, 0), Cargo => TempCargo,
            Combat_Value => 1, Crew => TempCrew,
            Description => Null_Unbounded_String,
            Owner => Factions_Container.Key(Factions_List.First),
            Known_Recipes => TempRecipes);
         ShipNode := Item(NodesList, I);
         ShipIndex := To_Unbounded_String(Get_Attribute(ShipNode, "index"));
         Action :=
           (if Get_Attribute(ShipNode, "action")'Length > 0 then
              Data_Action'Value(Get_Attribute(ShipNode, "action"))
            else ADD);
         if Action in UPDATE | REMOVE then
            if not Proto_Ships_Container.Contains
                (Proto_Ships_List, ShipIndex) then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(Data_Action'Image(Action)) &
                 " ship '" & To_String(ShipIndex) &
                 "', there is no ship with that index.";
            end if;
         elsif Proto_Ships_Container.Contains(Proto_Ships_List, ShipIndex) then
            raise Data_Loading_Error
              with "Can't add ship '" & To_String(ShipIndex) &
              "', there is already a ship with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := Proto_Ships_List(ShipIndex);
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
                    Data_Action'Value(Get_Attribute(ChildNode, "action"))
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
               TempRecord.Combat_Ai :=
                 Ship_Combat_Ai'Value(Get_Attribute(ShipNode, "combatai"));
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
            Load_Cargo_Loop :
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
                    Data_Action'Value(Get_Attribute(ChildNode, "action"))
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
                     Update_Cargo_Loop :
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
                        Find_Delete_Cargo_Loop :
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
            Load_Known_Recipes_Loop :
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
                    Data_Action'Value(Get_Attribute(ChildNode, "action"))
                  else ADD);
               if SubAction = ADD then
                  TempRecord.Known_Recipes.Append(New_Item => RecipeIndex);
               else
                  Find_Delete_Recipe_Loop :
                  for K in TempRecord.Known_Recipes.Iterate loop
                     if TempRecord.Known_Recipes(K) = RecipeIndex then
                        DeleteIndex := UnboundedString_Container.To_Index(K);
                        exit Find_Delete_Recipe_Loop;
                     end if;
                  end loop Find_Delete_Recipe_Loop;
                  TempRecord.Known_Recipes.Delete(Index => DeleteIndex);
               end if;
            end loop Load_Known_Recipes_Loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name(ShipNode, "member");
            Load_Crew_Loop :
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
                    Data_Action'Value(Get_Attribute(ChildNode, "action"))
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
                     Update_Crew_Loop :
                     for Member of TempRecord.Crew loop
                        if Member.Proto_Index = MobIndex then
                           if Get_Attribute(ChildNode, "amount") /= "" then
                              Member.Min_Amount :=
                                Integer'Value
                                  (Get_Attribute(ChildNode, "amount"));
                              Member.Max_Amount := 0;
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
                              Member.Min_Amount :=
                                Integer'Value
                                  (Get_Attribute(ChildNode, "minamount"));
                              Member.Max_Amount :=
                                Integer'Value
                                  (Get_Attribute(ChildNode, "maxamount"));
                           else
                              Member.Min_Amount := 1;
                              Member.Max_Amount := 0;
                           end if;
                           exit Update_Crew_Loop;
                        end if;
                     end loop Update_Crew_Loop;
                  when REMOVE =>
                     Find_Delete_Crew_Loop :
                     for K in TempRecord.Crew.Iterate loop
                        if TempRecord.Crew(K).Proto_Index = MobIndex then
                           DeleteIndex := Proto_Crew_Container.To_Index(K);
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
            Count_Combat_Value_Loop :
            for ModuleIndex of TempRecord.Modules loop
               case Modules_List(ModuleIndex).MType is
                  when HULL | GUN | BATTERING_RAM =>
                     TempRecord.Combat_Value :=
                       TempRecord.Combat_Value +
                       Modules_List(ModuleIndex).Durability +
                       (Modules_List(ModuleIndex).MaxValue * 10);
                     if Modules_List(ModuleIndex).MType = GUN then
                        CountAmmoValue(Modules_List(ModuleIndex).Value, 10);
                     end if;
                  when ARMOR =>
                     TempRecord.Combat_Value :=
                       TempRecord.Combat_Value +
                       Modules_List(ModuleIndex).Durability;
                  when HARPOON_GUN =>
                     TempRecord.Combat_Value :=
                       TempRecord.Combat_Value +
                       Modules_List(ModuleIndex).Durability +
                       (Modules_List(ModuleIndex).MaxValue * 5);
                     CountAmmoValue(Modules_List(ModuleIndex).Value, 5);
                  when others =>
                     null;
               end case;
            end loop Count_Combat_Value_Loop;
            TempRecord.Combat_Value := TempRecord.Combat_Value - 1;
            if Action /= UPDATE then
               Proto_Ships_Container.Include
                 (Proto_Ships_List, ShipIndex, TempRecord);
               Log_Message
                 ("Ship added: " & To_String(TempRecord.Name), EVERYTHING);
            else
               Proto_Ships_List(ShipIndex) := TempRecord;
            end if;
         else
            Proto_Ships_Container.Exclude(Proto_Ships_List, ShipIndex);
            Log_Message("Ship removed: " & To_String(ShipIndex), EVERYTHING);
         end if;
      end loop Load_Proto_Ships_Loop;
   end Load_Ships;

   function Count_Ship_Weight(Ship: Ship_Record) return Positive is
      Weight: Natural := 0;
      CargoWeight: Positive;
   begin
      Count_Ship_Weight_Loop :
      for Module of Ship.Modules loop
         Weight := Weight + Module.Weight;
      end loop Count_Ship_Weight_Loop;
      Count_Cargo_Weight_Loop :
      for Item of Ship.Cargo loop
         CargoWeight := Item.Amount * Items_List(Item.ProtoIndex).Weight;
         Weight := Weight + CargoWeight;
      end loop Count_Cargo_Weight_Loop;
      return Weight;
   end Count_Ship_Weight;

   function Generate_Ship_Name
     (Owner: Unbounded_String) return Unbounded_String is
      NewName: Unbounded_String := Null_Unbounded_String;
   begin
      Generate_Ship_Name_Loop :
      for I in Factions_List.Iterate loop
         if Factions_Container.Key(I) /= Owner then
            goto End_Of_Generate_Name_Loop;
         end if;
         if Factions_List(I).NamesType = ROBOTIC then
            NewName := GenerateRoboticName;
         else
            NewName :=
              Ship_Syllables_Start
                (GetRandom
                   (Ship_Syllables_Start.First_Index,
                    Ship_Syllables_Start.Last_Index));
            if GetRandom(1, 100) < 51 then
               Append
                 (NewName,
                  Ship_Syllables_Middle
                    (GetRandom
                       (Ship_Syllables_Middle.First_Index,
                        Ship_Syllables_Middle.Last_Index)));
            end if;
            Append
              (NewName,
               Ship_Syllables_End
                 (GetRandom
                    (Ship_Syllables_End.First_Index,
                     Ship_Syllables_End.Last_Index)));
         end if;
         exit Generate_Ship_Name_Loop;
         <<End_Of_Generate_Name_Loop>>
      end loop Generate_Ship_Name_Loop;
      return NewName;
   end Generate_Ship_Name;

   function Count_Combat_Value return Natural is
      CombatValue: Natural := 0;
      procedure CountAmmoValue(ItemTypeIndex, Multiple: Positive) is
      begin
         Count_Ammo_Value_Loop :
         for Item of Player_Ship.Cargo loop
            if Items_List(Item.ProtoIndex).IType =
              Items_Types(ItemTypeIndex) then
               CombatValue :=
                 CombatValue +
                 (Items_List(Item.ProtoIndex).Value(1) * Multiple);
            end if;
         end loop Count_Ammo_Value_Loop;
      end CountAmmoValue;
   begin
      Count_Combat_Value_Loop :
      for Module of Player_Ship.Modules loop
         case Modules_List(Module.Proto_Index).MType is
            when BATTERING_RAM =>
               CombatValue :=
                 CombatValue + Module.Max_Durability + (Module.Damage2 * 10);
            when GUN =>
               CombatValue :=
                 CombatValue + Module.Max_Durability + (Module.Damage * 10);
               CountAmmoValue(Modules_List(Module.Proto_Index).Value, 10);
            when ARMOR =>
               CombatValue := CombatValue + Module.Max_Durability;
            when HARPOON_GUN =>
               CombatValue :=
                 CombatValue + Module.Max_Durability + (Module.Duration * 5);
               CountAmmoValue(Modules_List(Module.Proto_Index).Value, 5);
            when HULL =>
               CombatValue :=
                 CombatValue + Module.Max_Durability +
                 (Module.Max_Modules * 10);
            when others =>
               null;
         end case;
      end loop Count_Combat_Value_Loop;
      return CombatValue;
   end Count_Combat_Value;

   function Get_Cabin_Quality(Quality: Natural) return String is
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
   end Get_Cabin_Quality;

   procedure Damage_Module
     (Ship: in out Ship_Record; Module_Index: Modules_Container.Extended_Index;
      Damage: Positive; Death_Reason: String) is
      RealDamage: Natural := Damage;
      WeaponIndex: Natural;
      procedure RemoveGun(ModuleIndex2: Positive) is
      begin
         if Ship.Modules(ModuleIndex2).Owner(1) > 0 then
            Death
              (Ship.Modules(ModuleIndex2).Owner(1),
               To_Unbounded_String(Death_Reason), Ship);
         end if;
      end RemoveGun;
   begin
      if Damage > Ship.Modules(Module_Index).Durability then
         RealDamage := Ship.Modules(Module_Index).Durability;
      end if;
      Ship.Modules(Module_Index).Durability :=
        Ship.Modules(Module_Index).Durability - RealDamage;
      if Ship.Modules(Module_Index).Durability = 0 then
         case Modules_List(Ship.Modules(Module_Index).Proto_Index).MType is
            when HULL | ENGINE =>
               if Ship = Player_Ship then
                  Death(1, To_Unbounded_String(Death_Reason), Player_Ship);
               end if;
            when TURRET =>
               WeaponIndex := Ship.Modules(Module_Index).Gun_Index;
               if WeaponIndex > 0 then
                  Ship.Modules(WeaponIndex).Durability := 0;
                  RemoveGun(WeaponIndex);
               end if;
            when GUN =>
               RemoveGun(Module_Index);
            when CABIN =>
               Kill_Owners_Loop :
               for Owner of Ship.Modules(Module_Index).Owner loop
                  if Owner > 0 and then Ship.Crew(Owner).Order = Rest then
                     Death(Owner, To_Unbounded_String(Death_Reason), Ship);
                  end if;
               end loop Kill_Owners_Loop;
            when others =>
               if Ship.Modules(Module_Index).Owner.Length > 0 then
                  if Ship.Modules(Module_Index).Owner(1) > 0
                    and then
                      Ship.Crew(Ship.Modules(Module_Index).Owner(1)).Order /=
                      Rest then
                     Death
                       (Ship.Modules(Module_Index).Owner(1),
                        To_Unbounded_String(Death_Reason), Ship);
                  end if;
               end if;
         end case;
      end if;
   end Damage_Module;

end Ships;
