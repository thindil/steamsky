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

with Maps; use Maps;
with Messages; use Messages;
with Items; use Items;
with UserInterface; use UserInterface;
with Bases.UI.Repair; use Bases.UI.Repair;
with ShipModules; use ShipModules;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;

package body Bases.Ship is

   procedure RepairShip is
      Cost, Time, ModuleIndex, MoneyIndex2: Natural := 0;
      TraderIndex, ProtoMoneyIndex: Positive;
   begin
      RepairCost(Cost, Time, ModuleIndex);
      if Cost = 0 then
         return;
      end if;
      ProtoMoneyIndex := FindProtoItem(MoneyIndex);
      MoneyIndex2 := FindCargo(ProtoMoneyIndex);
      if MoneyIndex2 = 0 then
         ShowDialog
           ("You don't have " & To_String(MoneyName) & " to pay for repairs.");
         return;
      end if;
      TraderIndex := FindMember(Talk);
      CountPrice(Cost, TraderIndex);
      if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
         ShowDialog
           ("You don't have enough " &
            To_String(MoneyName) &
            " to pay for repairs.");
         return;
      end if;
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Order = Repair then
            GiveOrders(Crew_Container.To_Index(I), Rest);
         end if;
      end loop;
      if ModuleIndex > 0 then
         PlayerShip.Modules(ModuleIndex).Durability :=
           PlayerShip.Modules(ModuleIndex).MaxDurability;
         AddMessage
           ("You bought " &
            To_String(PlayerShip.Modules(ModuleIndex).Name) &
            " repair for" &
            Positive'Image(Cost) &
            " " &
            To_String(MoneyName) &
            ".",
            TradeMessage);
      else
         for Module of PlayerShip.Modules loop
            if Module.Durability < Module.MaxDurability then
               Module.Durability := Module.MaxDurability;
            end if;
         end loop;
         AddMessage
           ("You bought whole ship repair for" &
            Positive'Image(Cost) &
            " " &
            To_String(MoneyName) &
            ".",
            TradeMessage);
      end if;
      UpdateCargo(PlayerShip, ProtoMoneyIndex, (0 - Cost));
      GainExp(1, 4, TraderIndex);
      GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
      UpdateGame(Time);
   end RepairShip;

   procedure UpgradeShip(Install: Boolean; ModuleIndex: Positive) is
      ProtoMoneyIndex: constant Positive := FindProtoItem(MoneyIndex);
      MoneyIndex2: constant Natural := FindCargo(ProtoMoneyIndex);
      HullIndex, ModulesAmount, TraderIndex: Positive;
      FreeTurretIndex, Price: Natural := 0;
      type DamageFactor is digits 2 range 0.0 .. 1.0;
      Damage: DamageFactor := 0.0;
   begin
      if MoneyIndex2 = 0 then
         ShowDialog
           ("You don't have " & To_String(MoneyName) & " to pay for modules.");
         return;
      end if;
      for C in PlayerShip.Modules.Iterate loop
         case Modules_List(PlayerShip.Modules(C).ProtoIndex).MType is
            when HULL =>
               HullIndex := Modules_Container.To_Index(C);
               ModulesAmount := PlayerShip.Modules(C).Current_Value;
            when TURRET =>
               if PlayerShip.Modules(C).Current_Value = 0 then
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
            ShowDialog
              ("You don't have enough " &
               To_String(MoneyName) &
               " to pay for " &
               To_String(Modules_List(ModuleIndex).Name) &
               ".");
            return;
         end if;
         for Module of PlayerShip.Modules loop
            if Modules_List(Module.ProtoIndex).MType =
              Modules_List(ModuleIndex).MType and
              Modules_List(ModuleIndex).Unique then
               ShowDialog
                 ("You can't install another " &
                  To_String(Modules_List(ModuleIndex).Name) &
                  " because you have installed one module that type. Remove old first.");
               return;
            end if;
         end loop;
         if Modules_List(ModuleIndex).MType /= HULL then
            ModulesAmount := ModulesAmount + Modules_List(ModuleIndex).Size;
            if ModulesAmount > PlayerShip.Modules(HullIndex).Max_Value and
              Modules_List(ModuleIndex).MType /= GUN then
               ShowDialog
                 ("You don't have free modules space for more modules.");
               return;
            end if;
            if Modules_List(ModuleIndex).MType = GUN and
              FreeTurretIndex = 0 then
               ShowDialog
                 ("You don't have free turret for next gun. Install new turret or remove old gun first.");
               return;
            end if;
         else
            if Modules_List(ModuleIndex).MaxValue < ModulesAmount then
               ShowDialog
                 ("This hull is too small for your ship. Remove some modules first.");
               return;
            end if;
            PlayerShip.Modules.Delete(HullIndex, 1);
         end if;
         UpdateGame(Modules_List(ModuleIndex).InstallTime);
         UpdateCargo(PlayerShip, ProtoMoneyIndex, (0 - Price));
         GainExp(1, 4, TraderIndex);
         GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
         if Modules_List(ModuleIndex).MType /= HULL then
            PlayerShip.Modules.Append
            (New_Item =>
               (Name => Modules_List(ModuleIndex).Name,
                ProtoIndex => ModuleIndex,
                Weight => Modules_List(ModuleIndex).Weight,
                Current_Value => Modules_List(ModuleIndex).Value,
                Max_Value => Modules_List(ModuleIndex).MaxValue,
                Durability => Modules_List(ModuleIndex).Durability,
                MaxDurability => Modules_List(ModuleIndex).Durability,
                Owner => 0,
                UpgradeProgress => 0,
                UpgradeAction => NONE));
         else
            PlayerShip.Modules.Insert
            (Before =>
               HullIndex, New_Item =>
               (Name => Modules_List(ModuleIndex).Name,
                ProtoIndex => ModuleIndex,
                Weight => Modules_List(ModuleIndex).Weight,
                Current_Value => Modules_List(ModuleIndex).Value,
                Max_Value => Modules_List(ModuleIndex).MaxValue,
                Durability => Modules_List(ModuleIndex).Durability,
                MaxDurability => Modules_List(ModuleIndex).Durability,
                Owner => 0,
                UpgradeProgress => 0,
                UpgradeAction => NONE));
         end if;
         case Modules_List(ModuleIndex).MType is
            when GUN =>
               PlayerShip.Modules(FreeTurretIndex).Current_Value :=
                 PlayerShip.Modules.Last_Index;
            when others =>
               PlayerShip.Modules(HullIndex).Current_Value := ModulesAmount;
         end case;
         AddMessage
           ("You installed " &
            To_String(Modules_List(ModuleIndex).Name) &
            " on your ship for" &
            Positive'Image(Price) &
            " " &
            To_String(MoneyName) &
            ".",
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
            ShowDialog
              ("You don't have enough free space for " &
               To_String(MoneyName) &
               " in ship cargo.");
            return;
         end if;
         case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).MType is
            when TURRET =>
               if PlayerShip.Modules(ModuleIndex).Current_Value > 0 then
                  ShowDialog
                    ("You have installed gun in this turret, remove it before you remove this turret.");
                  return;
               end if;
            when GUN =>
               for Module of PlayerShip.Modules loop
                  if Modules_List(Module.ProtoIndex).MType = TURRET and
                    Module.Current_Value = ModuleIndex then
                     Module.Current_Value := 0;
                     exit;
                  end if;
               end loop;
            when ShipModules.CARGO =>
               if FreeCargo((0 - PlayerShip.Modules(ModuleIndex).Max_Value)) <
                 0 then
                  ShowDialog
                    ("You can't sell this cargo bay, because you have items in it.");
                  return;
               end if;
            when others =>
               null;
         end case;
         ModulesAmount :=
           ModulesAmount -
           Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex).Size;
         PlayerShip.Modules(HullIndex).Current_Value := ModulesAmount;
         if PlayerShip.UpgradeModule = ModuleIndex then
            PlayerShip.UpgradeModule := 0;
            for C in PlayerShip.Crew.Iterate loop
               if PlayerShip.Crew(C).Order = Upgrading then
                  GiveOrders(Crew_Container.To_Index(C), Rest);
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
              (MemberIndex => PlayerShip.Modules(ModuleIndex).Owner,
               GivenOrder => Rest,
               CheckPriorities => False);
         end if;
         UpdateCargo(PlayerShip, ProtoMoneyIndex, Price);
         GainExp(1, 4, TraderIndex);
         GainRep(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex, 1);
         AddMessage
           ("You removed " &
            To_String(PlayerShip.Modules(ModuleIndex).Name) &
            " from your ship and earned" &
            Positive'Image(Price) &
            " " &
            To_String(MoneyName) &
            ".",
            TradeMessage);
         PlayerShip.Modules.Delete(ModuleIndex, 1);
         if PlayerShip.RepairModule > ModuleIndex then
            PlayerShip.RepairModule := PlayerShip.RepairModule - 1;
         elsif PlayerShip.RepairModule = ModuleIndex then
            PlayerShip.RepairModule := 0;
         end if;
         if PlayerShip.UpgradeModule > ModuleIndex then
            PlayerShip.UpgradeModule := PlayerShip.UpgradeModule - 1;
         end if;
         for Module of PlayerShip.Modules loop
            if Modules_List(Module.ProtoIndex).MType = TURRET then
               if Module.Current_Value > ModuleIndex then
                  Module.Current_Value := Module.Current_Value - 1;
               end if;
            end if;
         end loop;
      end if;
   end UpgradeShip;

   procedure PayForDock is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      ProtoMoneyIndex: constant Positive := FindProtoItem(MoneyIndex);
      MoneyIndex2: constant Natural := FindCargo(ProtoMoneyIndex);
      DockingCost: Positive;
      TraderIndex: constant Natural := FindMember(Talk);
   begin
      if SkyBases(BaseIndex).Owner = Abandoned then
         return;
      end if;
      if MoneyIndex2 = 0 then
         GainRep(BaseIndex, -10);
         AddMessage
           ("You don't have " & To_String(MoneyName) & " for pay for docking!",
            OtherMessage);
         return;
      end if;
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = HULL then
            DockingCost := Module.Max_Value;
            exit;
         end if;
      end loop;
      CountPrice(DockingCost, TraderIndex);
      if DockingCost > PlayerShip.Cargo(MoneyIndex2).Amount then
         DockingCost := PlayerShip.Cargo(MoneyIndex2).Amount;
      end if;
      UpdateCargo(PlayerShip, ProtoMoneyIndex, (0 - DockingCost));
      AddMessage
        ("You pay" &
         Positive'Image(DockingCost) &
         " " &
         To_String(MoneyName) &
         " docking fee.",
         OtherMessage);
      if TraderIndex > 0 then
         GainExp(1, 4, TraderIndex);
      end if;
   end PayForDock;

end Bases.Ship;
