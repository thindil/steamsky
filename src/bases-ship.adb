--    Copyright 2017-2021 Bartek thindil Jasicki
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

with Messages; use Messages;
with ShipModules; use ShipModules;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Trades; use Trades;
with Bases.Cargo; use Bases.Cargo;
with Config; use Config;
with BasesTypes; use BasesTypes;
with Maps; use Maps;

package body Bases.Ship is

   procedure RepairShip(ModuleIndex: Integer) is
      Cost, Time: Natural := 0;
      MoneyIndex2: constant Inventory_Container.Extended_Index :=
        FindItem(PlayerShip.Cargo, Money_Index);
      TraderIndex: constant Crew_Container.Extended_Index := FindMember(Talk);
   begin
      RepairCost(Cost, Time, ModuleIndex);
      if Cost = 0 then
         raise BasesShip_Nothing_To_Repair;
      end if;
      CountPrice(Cost, TraderIndex);
      if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
         raise Trade_Not_Enough_Money;
      end if;
      Give_Rest_Order_Loop :
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Order = Repair then
            GiveOrders(PlayerShip, Crew_Container.To_Index(I), Rest);
         end if;
      end loop Give_Rest_Order_Loop;
      if ModuleIndex > 0 then
         PlayerShip.Modules(ModuleIndex).Durability :=
           PlayerShip.Modules(ModuleIndex).MaxDurability;
         AddMessage
           ("You bought " & To_String(PlayerShip.Modules(ModuleIndex).Name) &
            " repair for" & Positive'Image(Cost) & " " &
            To_String(Money_Name) & ".",
            TradeMessage);
      else
         Repair_Whole_Ship_Loop :
         for Module of PlayerShip.Modules loop
            if Module.Durability < Module.MaxDurability then
               Module.Durability := Module.MaxDurability;
            end if;
         end loop Repair_Whole_Ship_Loop;
         AddMessage
           ("You bought an entire ship repair for" & Positive'Image(Cost) &
            " " & To_String(Money_Name) & ".",
            TradeMessage);
      end if;
      UpdateCargo
        (Ship => PlayerShip, CargoIndex => MoneyIndex2, Amount => -(Cost));
      UpdateBaseCargo(Money_Index, Cost);
      GainExp(1, Talking_Skill, TraderIndex);
      GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
      Update_Game(Time);
   end RepairShip;

   procedure UpgradeShip(Install: Boolean; ModuleIndex: Unbounded_String) is
      MoneyIndex2: constant Inventory_Container.Extended_Index :=
        FindItem(PlayerShip.Cargo, Money_Index);
      TraderIndex: constant Crew_Container.Extended_Index := FindMember(Talk);
      HullIndex, ShipModuleIndex: Modules_Container.Extended_Index;
      FreeTurretIndex: Modules_Container.Extended_Index := 0;
      ModulesAmount: Positive;
      Price: Natural := 0;
      BaseIndex: constant Bases_Range :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Owners: Natural_Container.Vector;
   begin
      if MoneyIndex2 = 0 then
         raise Trade_No_Money;
      end if;
      Find_Hull_And_Turrets_Loop :
      for C in PlayerShip.Modules.Iterate loop
         case PlayerShip.Modules(C).MType is
            when HULL =>
               HullIndex := Modules_Container.To_Index(C);
               ModulesAmount := PlayerShip.Modules(C).InstalledModules;
            when TURRET =>
               if (PlayerShip.Modules(C).GunIndex = 0 and Install)
                 and then
                   Modules_List(PlayerShip.Modules(C).ProtoIndex).Size >=
                   Modules_List(ModuleIndex).Size then
                  FreeTurretIndex := Modules_Container.To_Index(C);
               end if;
            when others =>
               null;
         end case;
      end loop Find_Hull_And_Turrets_Loop;
      if Install then
         Price := Modules_List(ModuleIndex).Price;
         CountPrice(Price, TraderIndex);
         if PlayerShip.Cargo(MoneyIndex2).Amount < Price then
            raise Trade_Not_Enough_Money
              with To_String(Modules_List(ModuleIndex).Name);
         end if;
         Check_Unique_Module_Loop :
         for Module of PlayerShip.Modules loop
            if Modules_List(Module.ProtoIndex).MType =
              Modules_List(ModuleIndex).MType and
              Modules_List(ModuleIndex).Unique then
               raise BasesShip_Unique_Module
                 with To_String(Modules_List(ModuleIndex).Name);
            end if;
         end loop Check_Unique_Module_Loop;
         if Modules_List(ModuleIndex).MType /= HULL then
            if Modules_List(ModuleIndex).Size >
              Modules_List(PlayerShip.Modules(HullIndex).ProtoIndex).Value then
               raise BasesShip_Installation_Error
                 with "You can't install this module because it is too big for this hull.";
            end if;
            ModulesAmount := ModulesAmount + Modules_List(ModuleIndex).Size;
            if ModulesAmount > PlayerShip.Modules(HullIndex).MaxModules and
              (Modules_List(ModuleIndex).MType /= GUN and
               Modules_List(ModuleIndex).MType /= HARPOON_GUN) then
               raise BasesShip_Installation_Error
                 with "You don't have free modules space for more modules.";
            end if;
            if
              (Modules_List(ModuleIndex).MType = GUN or
               Modules_List(ModuleIndex).MType = HARPOON_GUN) and
              FreeTurretIndex = 0 then
               raise BasesShip_Installation_Error
                 with "You don't have free turret with proprer size for this gun. Install new turret or remove old gun first.";
            end if;
         else
            Check_Module_Size_Loop :
            for Module of PlayerShip.Modules loop
               if Modules_List(Module.ProtoIndex).Size >
                 Modules_List(ModuleIndex).Value then
                  raise BasesShip_Installation_Error
                    with "This hull don't allow to have installed that big modules what you currently have.";
               end if;
            end loop Check_Module_Size_Loop;
            if Modules_List(ModuleIndex).MaxValue < ModulesAmount then
               raise BasesShip_Installation_Error
                 with "This hull is too small for your ship. Remove some modules first.";
            end if;
            PlayerShip.Modules.Delete(HullIndex);
         end if;
         UpdateCargo
           (Ship => PlayerShip, CargoIndex => MoneyIndex2, Amount => -(Price));
         UpdateBaseCargo(Money_Index, Price);
         GainExp(1, Talking_Skill, TraderIndex);
         GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
         Update_Game(Modules_List(ModuleIndex).InstallTime);
         if Modules_List(ModuleIndex).MType /= HULL then
            Set_Empty_Owners_Loop :
            for I in 1 .. Modules_List(ModuleIndex).MaxOwners loop
               Owners.Append(0);
            end loop Set_Empty_Owners_Loop;
            case Modules_List(ModuleIndex).MType is
               when ALCHEMY_LAB .. GREENHOUSE =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => WORKSHOP,
                        Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE,
                        CraftingIndex => Null_Unbounded_String,
                        CraftingTime => 0, CraftingAmount => 0));
               when MEDICAL_ROOM =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => MEDICAL_ROOM,
                        Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE));
               when TRAINING_ROOM =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => TRAINING_ROOM,
                        Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE, TrainedSkill => 0));
               when COCKPIT =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => COCKPIT,
                        Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE));
               when TURRET =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => TURRET,
                        Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE, GunIndex => 0));
               when CABIN =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => CABIN, Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE,
                        Cleanliness => Modules_List(ModuleIndex).Value,
                        Quality => Modules_List(ModuleIndex).MaxValue));
               when ShipModules.CARGO =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => CARGO_ROOM,
                        Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE));
               when ENGINE =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => ENGINE,
                        Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE,
                        FuelUsage => Modules_List(ModuleIndex).Value,
                        Power => Modules_List(ModuleIndex).MaxValue,
                        Disabled => False));
               when ARMOR =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => ARMOR, Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE));
               when BATTERING_RAM =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => BATTERING_RAM,
                        Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE,
                        Damage2 => Modules_List(ModuleIndex).MaxValue,
                        CoolingDown => False));
               when GUN =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => GUN, Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE,
                        Damage => Modules_List(ModuleIndex).MaxValue,
                        AmmoIndex => 0));
               when HARPOON_GUN =>
                  PlayerShip.Modules.Append
                    (New_Item =>
                       (MType => HARPOON_GUN,
                        Name => Modules_List(ModuleIndex).Name,
                        ProtoIndex => ModuleIndex,
                        Weight => Modules_List(ModuleIndex).Weight,
                        Durability => Modules_List(ModuleIndex).Durability,
                        MaxDurability => Modules_List(ModuleIndex).Durability,
                        Owner => Owners, UpgradeProgress => 0,
                        UpgradeAction => NONE,
                        Duration => Modules_List(ModuleIndex).MaxValue,
                        HarpoonIndex => 0));
               when ANY | HULL =>
                  null;
            end case;
         else
            PlayerShip.Modules.Insert
              (Before => HullIndex,
               New_Item =>
                 (MType => HULL, Name => Modules_List(ModuleIndex).Name,
                  ProtoIndex => ModuleIndex,
                  Weight => Modules_List(ModuleIndex).Weight,
                  Durability => Modules_List(ModuleIndex).Durability,
                  MaxDurability => Modules_List(ModuleIndex).Durability,
                  Owner => Owners, UpgradeProgress => 0, UpgradeAction => NONE,
                  InstalledModules => Modules_List(ModuleIndex).Value,
                  MaxModules => Modules_List(ModuleIndex).MaxValue));
         end if;
         case Modules_List(ModuleIndex).MType is
            when GUN | HARPOON_GUN =>
               PlayerShip.Modules(FreeTurretIndex).GunIndex :=
                 PlayerShip.Modules.Last_Index;
            when others =>
               PlayerShip.Modules(HullIndex).InstalledModules := ModulesAmount;
         end case;
         AddMessage
           ("You installed " & To_String(Modules_List(ModuleIndex).Name) &
            " on your ship for" & Positive'Image(Price) & " " &
            To_String(Money_Name) & ".",
            TradeMessage);
      else
         ShipModuleIndex := Integer'Value(To_String(ModuleIndex));
         declare
            Damage: Damage_Factor := 0.0;
         begin
            Damage :=
              1.0 -
              Damage_Factor
                (Float(PlayerShip.Modules(ShipModuleIndex).Durability) /
                 Float(PlayerShip.Modules(ShipModuleIndex).MaxDurability));
            Price :=
              Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                .Price -
              Integer
                (Float
                   (Modules_List
                      (PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                      .Price) *
                 Float(Damage));
         end;
         CountPrice(Price, TraderIndex, False);
         if FreeCargo(-(Price)) < 0 then
            raise Trade_No_Free_Cargo;
         end if;
         if Price > SkyBases(BaseIndex).Cargo(1).Amount then
            raise Trade_No_Money_In_Base;
         end if;
         case Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
           .MType is
            when TURRET =>
               if PlayerShip.Modules(ShipModuleIndex).GunIndex > 0 then
                  raise BasesShip_Removing_Error
                    with "You have installed gun in this turret, remove it before you remove this turret.";
               end if;
            when GUN | HARPOON_GUN =>
               for Module of PlayerShip.Modules loop
                  if Module.MType = TURRET
                    and then Module.GunIndex = ShipModuleIndex then
                     Module.GunIndex := 0;
                     exit;
                  end if;
               end loop;
            when ShipModules.CARGO =>
               if FreeCargo
                   (0 -
                    Modules_List
                      (PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                      .MaxValue) <
                 0 then
                  raise BasesShip_Removing_Error
                    with "You can't sell this cargo bay, because you have items in it.";
               end if;
            when others =>
               null;
         end case;
         if Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
             .MType not in
             HULL | ARMOR | GUN | HARPOON_GUN then
            ModulesAmount :=
              ModulesAmount -
              Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                .Size;
            PlayerShip.Modules(HullIndex).InstalledModules := ModulesAmount;
         end if;
         if PlayerShip.UpgradeModule = ShipModuleIndex then
            PlayerShip.UpgradeModule := 0;
            Remove_Upgrade_Order_Loop :
            for C in PlayerShip.Crew.Iterate loop
               if PlayerShip.Crew(C).Order = Upgrading then
                  GiveOrders(PlayerShip, Crew_Container.To_Index(C), Rest);
                  exit Remove_Upgrade_Order_Loop;
               end if;
            end loop Remove_Upgrade_Order_Loop;
         end if;
         if PlayerShip.Modules(ShipModuleIndex).MType /= CABIN then
            Give_Rest_Order_Loop :
            for Owner of PlayerShip.Modules(ShipModuleIndex).Owner loop
               if Owner > 0 then
                  GiveOrders
                    (Ship => PlayerShip, MemberIndex => Owner,
                     GivenOrder => Rest, CheckPriorities => False);
               end if;
            end loop Give_Rest_Order_Loop;
         end if;
         UpdateCargo
           (Ship => PlayerShip, CargoIndex => MoneyIndex2, Amount => Price);
         UpdateBaseCargo(Money_Index, Price);
         GainExp(1, Talking_Skill, TraderIndex);
         GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
         Update_Game
           (Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
              .InstallTime);
         AddMessage
           ("You removed " &
            To_String(PlayerShip.Modules(ShipModuleIndex).Name) &
            " from your ship and received" & Positive'Image(Price) & " " &
            To_String(Money_Name) & ".",
            TradeMessage);
         PlayerShip.Modules.Delete(ShipModuleIndex);
         if PlayerShip.RepairModule > ShipModuleIndex then
            PlayerShip.RepairModule := PlayerShip.RepairModule - 1;
         elsif PlayerShip.RepairModule = ShipModuleIndex then
            PlayerShip.RepairModule := 0;
         end if;
         if PlayerShip.UpgradeModule > ShipModuleIndex then
            PlayerShip.UpgradeModule := PlayerShip.UpgradeModule - 1;
         end if;
         for Module of PlayerShip.Modules loop
            if Module.MType = TURRET
              and then Module.GunIndex > ShipModuleIndex then
               Module.GunIndex := Module.GunIndex - 1;
            end if;
         end loop;
      end if;
   end UpgradeShip;

   procedure PayForDock is
      BaseIndex: constant Extended_Base_Range :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MoneyIndex2: constant Inventory_Container.Extended_Index :=
        FindItem(PlayerShip.Cargo, Money_Index);
      DockingCost: Natural;
      TraderIndex: constant Crew_Container.Extended_Index := FindMember(Talk);
   begin
      if SkyBases(BaseIndex).Population = 0 then
         return;
      end if;
      if MoneyIndex2 = 0 then
         GainRep(BaseIndex, -10);
         AddMessage
           ("You don't have " & To_String(Money_Name) &
            " for pay for docking!",
            OtherMessage, RED);
         return;
      end if;
      Count_Docking_Cost_Loop :
      for Module of PlayerShip.Modules loop
         if Module.MType = HULL then
            DockingCost := Module.MaxModules;
            exit Count_Docking_Cost_Loop;
         end if;
      end loop Count_Docking_Cost_Loop;
      DockingCost :=
        Natural(Float(DockingCost) * Float(New_Game_Settings.Prices_Bonus));
      if DockingCost = 0 then
         DockingCost := 1;
      end if;
      CountPrice(DockingCost, TraderIndex);
      if DockingCost > PlayerShip.Cargo(MoneyIndex2).Amount then
         DockingCost := PlayerShip.Cargo(MoneyIndex2).Amount;
      end if;
      UpdateCargo
        (Ship => PlayerShip, CargoIndex => MoneyIndex2,
         Amount => -(DockingCost));
      UpdateBaseCargo(Money_Index, DockingCost);
      AddMessage
        ("You pay" & Positive'Image(DockingCost) & " " &
         To_String(Money_Name) & " docking fee.",
         OtherMessage);
      if TraderIndex > 0 then
         GainExp(1, Talking_Skill, TraderIndex);
      end if;
   end PayForDock;

   procedure RepairCost(Cost, Time: in out Natural; ModuleIndex: Integer) is
      ProtoIndex: Unbounded_String;
      BaseIndex: constant Bases_Range :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      if ModuleIndex > 0 then
         Time :=
           PlayerShip.Modules(ModuleIndex).MaxDurability -
           PlayerShip.Modules(ModuleIndex).Durability;
         ProtoIndex :=
           FindProtoItem
             (ItemType =>
                Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                  .RepairMaterial);
         Cost := Time * Get_Price(SkyBases(BaseIndex).BaseType, ProtoIndex);
      else
         Count_Repair_Time_And_Cost_Loop :
         for Module of PlayerShip.Modules loop
            if Module.Durability < Module.MaxDurability then
               Time := Time + Module.MaxDurability - Module.Durability;
               ProtoIndex :=
                 FindProtoItem
                   (ItemType =>
                      Modules_List(Module.ProtoIndex).RepairMaterial);
               Cost :=
                 Cost +
                 ((Module.MaxDurability - Module.Durability) *
                  Get_Price(SkyBases(BaseIndex).BaseType, ProtoIndex));
            end if;
         end loop Count_Repair_Time_And_Cost_Loop;
         if ModuleIndex = -1 then
            Cost := Cost * 2;
            Time := Time / 2;
         elsif ModuleIndex = -2 then
            Cost := Cost * 4;
            Time := Time / 4;
         end if;
      end if;
      if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
          (To_Unbounded_String("shipyard")) then
         Cost := Cost / 2;
      end if;
      Cost := Natural(Float(Cost) * Float(New_Game_Settings.Prices_Bonus));
      if Cost = 0 then
         Cost := 1;
      end if;
      if Time = 0 then
         Time := 1;
      end if;
   end RepairCost;

end Bases.Ship;
