--    Copyright 2017-2018 Bartek thindil Jasicki
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

with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with ShipModules; use ShipModules;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Trades; use Trades;
with Bases.Cargo; use Bases.Cargo;

package body Bases.Ship is

   procedure RepairShip(ModuleIndex: Integer) is
      Cost, Time, MoneyIndex2: Natural := 0;
      TraderIndex, ProtoMoneyIndex: Positive;
   begin
      RepairCost(Cost, Time, ModuleIndex);
      if Cost = 0 then
         raise BasesShip_Nothing_To_Repair;
      end if;
      ProtoMoneyIndex := FindProtoItem(MoneyIndex);
      MoneyIndex2 := FindItem(PlayerShip.Cargo, ProtoMoneyIndex);
      if MoneyIndex2 = 0 then
         raise Trade_No_Money;
      end if;
      TraderIndex := FindMember(Talk);
      CountPrice(Cost, TraderIndex);
      if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
         raise Trade_Not_Enough_Money;
      end if;
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Order = Repair then
            GiveOrders(PlayerShip, Crew_Container.To_Index(I), Rest);
         end if;
      end loop;
      if ModuleIndex > 0 then
         PlayerShip.Modules(ModuleIndex).Durability :=
           PlayerShip.Modules(ModuleIndex).MaxDurability;
         AddMessage
           ("You bought " & To_String(PlayerShip.Modules(ModuleIndex).Name) &
            " repair for" & Positive'Image(Cost) & " " & To_String(MoneyName) &
            ".",
            TradeMessage);
      else
         for Module of PlayerShip.Modules loop
            if Module.Durability < Module.MaxDurability then
               Module.Durability := Module.MaxDurability;
            end if;
         end loop;
         AddMessage
           ("You bought whole ship repair for" & Positive'Image(Cost) & " " &
            To_String(MoneyName) & ".",
            TradeMessage);
      end if;
      UpdateCargo
        (Ship => PlayerShip, CargoIndex => MoneyIndex2, Amount => (0 - Cost));
      UpdateBaseCargo(ProtoMoneyIndex, Cost);
      GainExp(1, TalkingSkill, TraderIndex);
      GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
      UpdateGame(Time);
   end RepairShip;

   procedure UpgradeShip(Install: Boolean; ModuleIndex: Positive) is
      ProtoMoneyIndex: constant Positive := FindProtoItem(MoneyIndex);
      MoneyIndex2: constant Natural :=
        FindItem(PlayerShip.Cargo, ProtoMoneyIndex);
      HullIndex, ModulesAmount, TraderIndex: Positive;
      FreeTurretIndex, Price: Natural := 0;
      type DamageFactor is digits 2 range 0.0 .. 1.0;
      Damage: DamageFactor := 0.0;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      if MoneyIndex2 = 0 then
         raise Trade_No_Money;
      end if;
      for C in PlayerShip.Modules.Iterate loop
         case Modules_List(PlayerShip.Modules(C).ProtoIndex).MType is
            when HULL =>
               HullIndex := Modules_Container.To_Index(C);
               ModulesAmount := PlayerShip.Modules(C).Data(1);
            when TURRET =>
               if PlayerShip.Modules(C).Data(1) = 0 then
                  FreeTurretIndex := Modules_Container.To_Index(C);
               end if;
            when others =>
               null;
         end case;
      end loop;
      TraderIndex := FindMember(Talk);
      if Install then
         Price := Modules_List(ModuleIndex).Price;
         CountPrice(Price, TraderIndex);
         if PlayerShip.Cargo(MoneyIndex2).Amount < Price then
            raise Trade_Not_Enough_Money
              with To_String(Modules_List(ModuleIndex).Name);
         end if;
         for Module of PlayerShip.Modules loop
            if Modules_List(Module.ProtoIndex).MType =
              Modules_List(ModuleIndex).MType and
              Modules_List(ModuleIndex).Unique then
               raise BasesShip_Unique_Module
                 with To_String(Modules_List(ModuleIndex).Name);
            end if;
         end loop;
         if Modules_List(ModuleIndex).MType /= HULL then
            ModulesAmount := ModulesAmount + Modules_List(ModuleIndex).Size;
            if ModulesAmount > PlayerShip.Modules(HullIndex).Data(2) and
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
                 with "You don't have free turret for next gun. Install new turret or remove old gun first.";
            end if;
         else
            if Modules_List(ModuleIndex).MaxValue < ModulesAmount then
               raise BasesShip_Installation_Error
                 with "This hull is too small for your ship. Remove some modules first.";
            end if;
            PlayerShip.Modules.Delete(HullIndex);
         end if;
         UpdateGame(Modules_List(ModuleIndex).InstallTime);
         UpdateCargo
           (Ship => PlayerShip, CargoIndex => MoneyIndex2,
            Amount => (0 - Price));
         UpdateBaseCargo(ProtoMoneyIndex, Price);
         GainExp(1, TalkingSkill, TraderIndex);
         GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
         if Modules_List(ModuleIndex).MType /= HULL then
            PlayerShip.Modules.Append
              (New_Item =>
                 (Name => Modules_List(ModuleIndex).Name,
                  ProtoIndex => ModuleIndex,
                  Weight => Modules_List(ModuleIndex).Weight,
                  Durability => Modules_List(ModuleIndex).Durability,
                  MaxDurability => Modules_List(ModuleIndex).Durability,
                  Owner => 0, UpgradeProgress => 0, UpgradeAction => NONE,
                  Data =>
                    (Modules_List(ModuleIndex).Value,
                     Modules_List(ModuleIndex).MaxValue, 0)));
         else
            PlayerShip.Modules.Insert
              (Before => HullIndex,
               New_Item =>
                 (Name => Modules_List(ModuleIndex).Name,
                  ProtoIndex => ModuleIndex,
                  Weight => Modules_List(ModuleIndex).Weight,
                  Durability => Modules_List(ModuleIndex).Durability,
                  MaxDurability => Modules_List(ModuleIndex).Durability,
                  Owner => 0, UpgradeProgress => 0, UpgradeAction => NONE,
                  Data =>
                    (Modules_List(ModuleIndex).Value,
                     Modules_List(ModuleIndex).MaxValue, 0)));
         end if;
         case Modules_List(ModuleIndex).MType is
            when GUN | HARPOON_GUN =>
               PlayerShip.Modules(FreeTurretIndex).Data(1) :=
                 PlayerShip.Modules.Last_Index;
            when others =>
               PlayerShip.Modules(HullIndex).Data(1) := ModulesAmount;
         end case;
         AddMessage
           ("You installed " & To_String(Modules_List(ModuleIndex).Name) &
            " on your ship for" & Positive'Image(Price) & " " &
            To_String(MoneyName) & ".",
            TradeMessage);
      else
         Damage :=
           1.0 -
           DamageFactor
             (Float(PlayerShip.Modules(ModuleIndex).Durability) /
              Float(PlayerShip.Modules(ModuleIndex).MaxDurability));
         Price :=
           Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).Price -
           Integer
             (Float
                (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                   .Price) *
              Float(Damage));
         CountPrice(Price, TraderIndex, False);
         if FreeCargo((0 - Price)) < 0 then
            raise Trade_No_Free_Cargo;
         end if;
         if Price > SkyBases(BaseIndex).Cargo(1).Amount then
            raise Trade_No_Money_In_Base;
         end if;
         case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).MType is
            when TURRET =>
               if PlayerShip.Modules(ModuleIndex).Data(1) > 0 then
                  raise BasesShip_Removing_Error
                    with "You have installed gun in this turret, remove it before you remove this turret.";
               end if;
            when GUN | HARPOON_GUN =>
               for Module of PlayerShip.Modules loop
                  if Modules_List(Module.ProtoIndex).MType = TURRET and
                    Module.Data(1) = ModuleIndex then
                     Module.Data(1) := 0;
                     exit;
                  end if;
               end loop;
            when ShipModules.CARGO =>
               if FreeCargo((0 - PlayerShip.Modules(ModuleIndex).Data(2))) <
                 0 then
                  raise BasesShip_Removing_Error
                    with "You can't sell this cargo bay, because you have items in it.";
               end if;
            when others =>
               null;
         end case;
         ModulesAmount :=
           ModulesAmount -
           Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).Size;
         PlayerShip.Modules(HullIndex).Data(1) := ModulesAmount;
         if PlayerShip.UpgradeModule = ModuleIndex then
            PlayerShip.UpgradeModule := 0;
            for C in PlayerShip.Crew.Iterate loop
               if PlayerShip.Crew(C).Order = Upgrading then
                  GiveOrders(PlayerShip, Crew_Container.To_Index(C), Rest);
                  exit;
               end if;
            end loop;
         end if;
         UpdateGame
           (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
              .InstallTime);
         if PlayerShip.Modules(ModuleIndex).Owner > 0 and
           Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).MType /=
             CABIN then
            GiveOrders
              (Ship => PlayerShip,
               MemberIndex => PlayerShip.Modules(ModuleIndex).Owner,
               GivenOrder => Rest, CheckPriorities => False);
         end if;
         UpdateCargo
           (Ship => PlayerShip, CargoIndex => MoneyIndex2, Amount => Price);
         UpdateBaseCargo(ProtoMoneyIndex, Price);
         GainExp(1, TalkingSkill, TraderIndex);
         GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
         AddMessage
           ("You removed " & To_String(PlayerShip.Modules(ModuleIndex).Name) &
            " from your ship and earned" & Positive'Image(Price) & " " &
            To_String(MoneyName) & ".",
            TradeMessage);
         PlayerShip.Modules.Delete(ModuleIndex);
         if PlayerShip.RepairModule > ModuleIndex then
            PlayerShip.RepairModule := PlayerShip.RepairModule - 1;
         elsif PlayerShip.RepairModule = ModuleIndex then
            PlayerShip.RepairModule := 0;
         end if;
         if PlayerShip.UpgradeModule > ModuleIndex then
            PlayerShip.UpgradeModule := PlayerShip.UpgradeModule - 1;
         end if;
         for Module of PlayerShip.Modules loop
            if Modules_List(Module.ProtoIndex).MType = TURRET
              and then Module.Data(1) > ModuleIndex then
               Module.Data(1) := Module.Data(1) - 1;
            end if;
         end loop;
      end if;
   end UpgradeShip;

   procedure PayForDock is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      ProtoMoneyIndex: constant Positive := FindProtoItem(MoneyIndex);
      MoneyIndex2: constant Natural :=
        FindItem(PlayerShip.Cargo, ProtoMoneyIndex);
      DockingCost: Positive;
      TraderIndex: constant Natural := FindMember(Talk);
   begin
      if SkyBases(BaseIndex).Population = 0 then
         return;
      end if;
      if MoneyIndex2 = 0 then
         GainRep(BaseIndex, -10);
         AddMessage
           ("You don't have " & To_String(MoneyName) & " for pay for docking!",
            OtherMessage, 3);
         return;
      end if;
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = HULL then
            DockingCost := Module.Data(2);
            exit;
         end if;
      end loop;
      CountPrice(DockingCost, TraderIndex);
      if DockingCost > PlayerShip.Cargo(MoneyIndex2).Amount then
         DockingCost := PlayerShip.Cargo(MoneyIndex2).Amount;
      end if;
      UpdateCargo
        (Ship => PlayerShip, CargoIndex => MoneyIndex2,
         Amount => (0 - DockingCost));
      UpdateBaseCargo(ProtoMoneyIndex, DockingCost);
      AddMessage
        ("You pay" & Positive'Image(DockingCost) & " " & To_String(MoneyName) &
         " docking fee.",
         OtherMessage);
      if TraderIndex > 0 then
         GainExp(1, TalkingSkill, TraderIndex);
      end if;
   end PayForDock;

   procedure RepairCost(Cost, Time: in out Natural; ModuleIndex: Integer) is
      BaseType: constant Positive :=
        Bases_Types'Pos
          (SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
             .BaseType) +
        1;
      ProtoIndex: Positive;
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
         Cost := Time * Items_List(ProtoIndex).Prices(BaseType);
      else
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
                  Items_List(ProtoIndex).Prices(BaseType));
            end if;
         end loop;
         if ModuleIndex = -1 then
            Cost := Cost * 2;
            Time := Time / 2;
         elsif ModuleIndex = -2 then
            Cost := Cost * 4;
            Time := Time / 4;
         end if;
      end if;
      if Bases_Types'Val(BaseType - 1) = Shipyard then
         Cost := Cost / 2;
      end if;
      if Time = 0 then
         Time := 1;
      end if;
   end RepairCost;

end Bases.Ship;
