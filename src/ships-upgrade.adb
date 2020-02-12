--    Copyright 2017-2019 Bartek thindil Jasicki
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
with Crew.Inventory; use Crew.Inventory;
with Config; use Config;

package body Ships.Upgrade is

   procedure StartUpgrading(ModuleIndex, UpgradeType: Positive) is
      MaxValue: Natural;
      UpgradeProgress: Positive;
      UpgradeAction: ShipUpgrade;
   begin
      if PlayerShip.Modules(ModuleIndex).Durability = 0 and
        UpgradeType /= 3 then
         raise Ship_Upgrade_Error
           with "You can't upgrade " &
           To_String(PlayerShip.Modules(ModuleIndex).Name) &
           " because is destroyed.";
      end if;
      case UpgradeType is
         when 1 => -- Upgrade durability
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .Durability) *
                 1.5);
            if PlayerShip.Modules(ModuleIndex).MaxDurability = MaxValue then
               raise Ship_Upgrade_Error
                 with "You can't further improve durability of " &
                 To_String(PlayerShip.Modules(ModuleIndex).Name) & ".";
            end if;
            UpgradeAction := DURABILITY;
            UpgradeProgress :=
              Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .Durability;
         when 2 => -- Upgrade various max value of selected module
            MaxValue :=
              Natural
                (Float
                   (Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
              .MType is
               when ENGINE =>
                  if PlayerShip.Modules(ModuleIndex).Power = MaxValue then
                     raise Ship_Upgrade_Error
                       with "You can't further improve power of " &
                       To_String(PlayerShip.Modules(ModuleIndex).Name) & ".";
                  end if;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (PlayerShip.Modules(ModuleIndex).ProtoIndex)
                            .MaxValue /
                          20) *
                       NewGameSettings.UpgradeCostBonus);
               when CABIN =>
                  if PlayerShip.Modules(ModuleIndex).Quality = MaxValue then
                     raise Ship_Upgrade_Error
                       with "You can't further improve quality of " &
                       To_String(PlayerShip.Modules(ModuleIndex).Name) & ".";
                  end if;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (PlayerShip.Modules(ModuleIndex).ProtoIndex)
                            .MaxValue) *
                       NewGameSettings.UpgradeCostBonus);
               when GUN | BATTERING_RAM =>
                  declare
                     Damage: Positive;
                  begin
                     if PlayerShip.Modules(ModuleIndex).MType = GUN then
                        Damage := PlayerShip.Modules(ModuleIndex).Damage;
                     else
                        Damage := PlayerShip.Modules(ModuleIndex).Damage2;
                     end if;
                     if Damage = MaxValue then
                        raise Ship_Upgrade_Error
                          with "You can't further improve damage of " &
                          To_String(PlayerShip.Modules(ModuleIndex).Name) &
                          ".";
                     end if;
                  end;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (PlayerShip.Modules(ModuleIndex).ProtoIndex)
                            .MaxValue *
                          2) *
                       NewGameSettings.UpgradeCostBonus);
               when HULL =>
                  if PlayerShip.Modules(ModuleIndex).MaxModules = MaxValue then
                     raise Ship_Upgrade_Error
                       with "You can't further enlarge " &
                       To_String(PlayerShip.Modules(ModuleIndex).Name) & ".";
                  end if;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (PlayerShip.Modules(ModuleIndex).ProtoIndex)
                            .MaxValue *
                          40) *
                       NewGameSettings.UpgradeCostBonus);
               when HARPOON_GUN =>
                  if PlayerShip.Modules(ModuleIndex).Duration = MaxValue then
                     raise Ship_Upgrade_Error
                       with "You can't further improve strength of " &
                       To_String(PlayerShip.Modules(ModuleIndex).Name) & ".";
                  end if;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (PlayerShip.Modules(ModuleIndex).ProtoIndex)
                            .MaxValue *
                          10) *
                       NewGameSettings.UpgradeCostBonus);
               when others =>
                  raise Ship_Upgrade_Error
                    with To_String(PlayerShip.Modules(ModuleIndex).Name) &
                    " can't be upgraded in that way.";
            end case;
            UpgradeAction := MAX_VALUE;
         when 3 => -- Upgrade various value of selected module
            case Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
              .MType is
               when ENGINE =>
                  MaxValue :=
                    Natural
                      (Float
                         (Modules_List
                            (PlayerShip.Modules(ModuleIndex).ProtoIndex)
                            .Value) /
                       2.0);
                  if MaxValue < 1 then
                     MaxValue := 1;
                  end if;
                  if PlayerShip.Modules(ModuleIndex).FuelUsage = MaxValue then
                     raise Ship_Upgrade_Error
                       with "You can't further reduce fuel usage of " &
                       To_String(PlayerShip.Modules(ModuleIndex).Name) & ".";
                  end if;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (PlayerShip.Modules(ModuleIndex).ProtoIndex)
                            .Value *
                          20) *
                       NewGameSettings.UpgradeCostBonus);
               when others =>
                  raise Ship_Upgrade_Error
                    with To_String(PlayerShip.Modules(ModuleIndex).Name) &
                    " can't be upgraded in that way.";
            end case;
            UpgradeAction := VALUE;
         when 4 => -- Continue previous upgrade
            if PlayerShip.Modules(ModuleIndex).UpgradeAction = NONE then
               raise Ship_Upgrade_Error
                 with To_String(PlayerShip.Modules(ModuleIndex).Name) &
                 " don't have set any upgrade yet.";
            end if;
            UpgradeAction := PlayerShip.Modules(ModuleIndex).UpgradeAction;
         when others =>
            return;
      end case;
      declare
         MaterialIndex: constant Natural :=
           FindItem
             (Inventory => PlayerShip.Cargo,
              ItemType =>
                Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                  .RepairMaterial);
      begin
         if MaterialIndex = 0 then
            for Item of Items_List loop
               if Item.IType =
                 Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                   .RepairMaterial then
                  raise Ship_Upgrade_Error
                    with "You don't have " & To_String(Item.Name) &
                    " to upgrade " &
                    To_String(PlayerShip.Modules(ModuleIndex).Name) & ".";
               end if;
            end loop;
         end if;
      end;
      PlayerShip.UpgradeModule := ModuleIndex;
      if PlayerShip.Modules(ModuleIndex).UpgradeAction /= UpgradeAction then
         PlayerShip.Modules(ModuleIndex).UpgradeProgress :=
           Integer(Float(UpgradeProgress) * NewGameSettings.UpgradeCostBonus);
         if PlayerShip.Modules(ModuleIndex).UpgradeProgress = 0 then
            PlayerShip.Modules(ModuleIndex).UpgradeProgress := 1;
         end if;
         PlayerShip.Modules(ModuleIndex).UpgradeAction := UpgradeAction;
      end if;
      AddMessage
        ("You set the " & To_String(PlayerShip.Modules(ModuleIndex).Name) &
         " to upgrade.",
         OrderMessage);
   end StartUpgrading;

   procedure UpgradeShip(Minutes: Positive) is
      ResultAmount, UpgradePoints, WorkerIndex, UpgradeMaterial,
      UpgradeProgress, UpgradeTools, MaterialCost, MaxValue: Natural := 0;
      UpgradeValue: Positive;
      WeightGain: Natural;
      Times: Natural := 0;
      OrderTime, CurrentMinutes: Integer;
      procedure FindMatsAndTools is
      begin
         UpgradeMaterial :=
           FindItem
             (Inventory => PlayerShip.Cargo,
              ItemType =>
                Modules_List
                  (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                  .RepairMaterial);
         UpgradeTools := FindTools(WorkerIndex, RepairTools, Upgrading);
      end FindMatsAndTools;
      procedure MaxUpgradeReached(MessageText: String) is
      begin
         AddMessage
           (MessageText &
            To_String(PlayerShip.Modules(PlayerShip.UpgradeModule).Name) & ".",
            OrderMessage, YELLOW);
         PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeProgress := 0;
         PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeAction := NONE;
         PlayerShip.UpgradeModule := 0;
         GiveOrders(PlayerShip, WorkerIndex, Rest);
      end MaxUpgradeReached;
   begin
      WorkerIndex := FindMember(Upgrading);
      if WorkerIndex = 0 then
         return;
      end if;
      CurrentMinutes := Minutes;
      OrderTime := PlayerShip.Crew(WorkerIndex).OrderTime;
      if PlayerShip.Modules(PlayerShip.UpgradeModule).Durability = 0 then
         AddMessage
           (To_String(PlayerShip.Crew(WorkerIndex).Name) &
            " stops upgrading " &
            To_String(PlayerShip.Modules(PlayerShip.UpgradeModule).Name) &
            " because it was destroyed.",
            OrderMessage, RED);
         GiveOrders(PlayerShip, WorkerIndex, Rest);
         return;
      end if;
      while CurrentMinutes > 0 loop
         if CurrentMinutes >= OrderTime then
            CurrentMinutes := CurrentMinutes - OrderTime;
            Times := Times + 1;
            OrderTime := 15;
         else
            OrderTime := OrderTime - CurrentMinutes;
            CurrentMinutes := 0;
         end if;
      end loop;
      PlayerShip.Crew(WorkerIndex).OrderTime := OrderTime;
      if Times = 0 then
         return;
      end if;
      UpgradePoints :=
        ((GetSkillLevel
            (PlayerShip.Crew(WorkerIndex),
             Modules_List
               (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
               .RepairSkill) /
          10) *
         Times) +
        Times;
      while UpgradePoints > 0 and
        PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeProgress > 0 loop
         ResultAmount := UpgradePoints;
         if ResultAmount >
           PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeProgress then
            ResultAmount :=
              PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeProgress;
         end if;
         FindMatsAndTools;
         if UpgradeMaterial = 0 then
            AddMessage
              ("You don't have enough materials to upgrade " &
               To_String(PlayerShip.Modules(PlayerShip.UpgradeModule).Name),
               OrderMessage, RED);
            GiveOrders(PlayerShip, WorkerIndex, Rest);
            exit;
         end if;
         if UpgradeTools = 0 then
            AddMessage
              ("You don't have repair tools to upgrade " &
               To_String(PlayerShip.Modules(PlayerShip.UpgradeModule).Name),
               OrderMessage, RED);
            GiveOrders(PlayerShip, WorkerIndex, Rest);
            exit;
         end if;
         if PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeAction =
           MAX_VALUE then
            case PlayerShip.Modules(PlayerShip.UpgradeModule).MType is
               when ENGINE =>
                  if ResultAmount >
                    PlayerShip.Cargo(UpgradeMaterial).Amount * 200 then
                     ResultAmount :=
                       PlayerShip.Cargo(UpgradeMaterial).Amount * 200;
                  end if;
                  MaterialCost := ResultAmount / 200;
               when CABIN =>
                  if ResultAmount >
                    PlayerShip.Cargo(UpgradeMaterial).Amount * 20 then
                     ResultAmount :=
                       PlayerShip.Cargo(UpgradeMaterial).Amount * 20;
                  end if;
                  MaterialCost := ResultAmount / 20;
               when GUN | BATTERING_RAM | HARPOON_GUN =>
                  if ResultAmount >
                    PlayerShip.Cargo(UpgradeMaterial).Amount * 10 then
                     ResultAmount :=
                       PlayerShip.Cargo(UpgradeMaterial).Amount * 10;
                  end if;
                  MaterialCost := ResultAmount / 10;
               when HULL =>
                  if ResultAmount >
                    PlayerShip.Cargo(UpgradeMaterial).Amount * 2 then
                     ResultAmount :=
                       PlayerShip.Cargo(UpgradeMaterial).Amount * 2;
                  end if;
                  MaterialCost := ResultAmount / 2;
               when others =>
                  if ResultAmount >
                    PlayerShip.Cargo(UpgradeMaterial).Amount then
                     ResultAmount := PlayerShip.Cargo(UpgradeMaterial).Amount;
                  end if;
                  MaterialCost := ResultAmount;
            end case;
         elsif PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeAction =
           DURABILITY then
            if ResultAmount >
              PlayerShip.Cargo(UpgradeMaterial).Amount * 10 then
               ResultAmount := PlayerShip.Cargo(UpgradeMaterial).Amount * 10;
            end if;
            MaterialCost := ResultAmount / 10;
         else
            if ResultAmount > PlayerShip.Cargo(UpgradeMaterial).Amount then
               ResultAmount := PlayerShip.Cargo(UpgradeMaterial).Amount;
            end if;
            MaterialCost := ResultAmount;
         end if;
         if MaterialCost < Times then
            MaterialCost := Times;
         end if;
         if MaterialCost > PlayerShip.Cargo(UpgradeMaterial).Amount then
            MaterialCost := PlayerShip.Cargo(UpgradeMaterial).Amount;
         end if;
         GainExp
           (ResultAmount,
            Modules_List
              (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
              .RepairSkill,
            WorkerIndex);
         DamageItem
           (PlayerShip.Crew(WorkerIndex).Inventory, UpgradeTools,
            GetSkillLevel
              (PlayerShip.Crew(WorkerIndex),
               Modules_List
                 (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                 .RepairSkill),
            WorkerIndex);
         FindMatsAndTools;
         UpgradeProgress :=
           PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeProgress -
           ResultAmount;
         UpgradePoints := UpgradePoints - ResultAmount;
         UpdateCargo
           (PlayerShip, PlayerShip.Cargo.Element(UpgradeMaterial).ProtoIndex,
            (0 - MaterialCost));
         if UpgradeProgress = 0 then
            WeightGain :=
              Modules_List
                (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                .Weight /
              Modules_List
                (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                .Durability;
            if WeightGain < 1 then
               WeightGain := 1;
            end if;
            case PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeAction is
               when DURABILITY =>
                  if
                    (Modules_List
                       (PlayerShip.Modules(PlayerShip.UpgradeModule)
                          .ProtoIndex)
                       .Durability /
                     20) >
                    0 then
                     PlayerShip.Modules(PlayerShip.UpgradeModule)
                       .MaxDurability :=
                       PlayerShip.Modules(PlayerShip.UpgradeModule)
                         .MaxDurability +
                       (Modules_List
                          (PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .ProtoIndex)
                          .Durability /
                        20);
                     PlayerShip.Modules(PlayerShip.UpgradeModule).Weight :=
                       PlayerShip.Modules(PlayerShip.UpgradeModule).Weight +
                       (WeightGain *
                        (Modules_List
                           (PlayerShip.Modules(PlayerShip.UpgradeModule)
                              .ProtoIndex)
                           .Durability /
                         20));
                  else
                     PlayerShip.Modules(PlayerShip.UpgradeModule)
                       .MaxDurability :=
                       PlayerShip.Modules(PlayerShip.UpgradeModule)
                         .MaxDurability +
                       1;
                     PlayerShip.Modules(PlayerShip.UpgradeModule).Weight :=
                       PlayerShip.Modules(PlayerShip.UpgradeModule).Weight +
                       WeightGain;
                  end if;
                  AddMessage
                    (To_String(PlayerShip.Crew(WorkerIndex).Name) &
                     " has upgraded the durability of " &
                     To_String
                       (PlayerShip.Modules(PlayerShip.UpgradeModule).Name) &
                     ".",
                     OrderMessage, GREEN);
                  MaxValue :=
                    Positive
                      (Float
                         (Modules_List
                            (PlayerShip.Modules(PlayerShip.UpgradeModule)
                               .ProtoIndex)
                            .Durability) *
                       1.5);
                  if PlayerShip.Modules(PlayerShip.UpgradeModule)
                      .MaxDurability =
                    MaxValue then
                     MaxUpgradeReached
                       ("You've reached the maximum durability for ");
                     return;
                  else
                     PlayerShip.Modules(PlayerShip.UpgradeModule)
                       .UpgradeProgress :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .Durability;
                  end if;
               when MAX_VALUE =>
                  case PlayerShip.Modules(PlayerShip.UpgradeModule).MType is
                     when HULL =>
                        WeightGain := WeightGain * 10;
                        PlayerShip.Modules(PlayerShip.UpgradeModule)
                          .MaxModules :=
                          PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .MaxModules +
                          1;
                        UpgradeValue :=
                          PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .MaxModules;
                     when ENGINE =>
                        WeightGain :=
                          (Modules_List
                             (PlayerShip.Modules(PlayerShip.UpgradeModule)
                                .ProtoIndex)
                             .MaxValue /
                           40);
                        PlayerShip.Modules(PlayerShip.UpgradeModule).Power :=
                          PlayerShip.Modules(PlayerShip.UpgradeModule).Power +
                          (Modules_List
                             (PlayerShip.Modules(PlayerShip.UpgradeModule)
                                .ProtoIndex)
                             .MaxValue /
                           20);
                        UpgradeValue :=
                          PlayerShip.Modules(PlayerShip.UpgradeModule).Power;
                     when CABIN =>
                        PlayerShip.Modules(PlayerShip.UpgradeModule).Quality :=
                          PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .Quality +
                          (Modules_List
                             (PlayerShip.Modules(PlayerShip.UpgradeModule)
                                .ProtoIndex)
                             .MaxValue /
                           20);
                        UpgradeValue :=
                          PlayerShip.Modules(PlayerShip.UpgradeModule).Quality;
                     when GUN =>
                        if
                          (Modules_List
                             (PlayerShip.Modules(PlayerShip.UpgradeModule)
                                .ProtoIndex)
                             .MaxValue /
                           20) >
                          0 then
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .Damage :=
                             PlayerShip.Modules(PlayerShip.UpgradeModule)
                               .Damage +
                             (Modules_List
                                (PlayerShip.Modules(PlayerShip.UpgradeModule)
                                   .ProtoIndex)
                                .MaxValue /
                              20);
                        else
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .Damage :=
                             PlayerShip.Modules(PlayerShip.UpgradeModule)
                               .Damage +
                             1;
                        end if;
                        UpgradeValue :=
                          PlayerShip.Modules(PlayerShip.UpgradeModule).Damage;
                     when BATTERING_RAM =>
                        if
                          (Modules_List
                             (PlayerShip.Modules(PlayerShip.UpgradeModule)
                                .ProtoIndex)
                             .MaxValue /
                           20) >
                          0 then
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .Damage2 :=
                             PlayerShip.Modules(PlayerShip.UpgradeModule)
                               .Damage2 +
                             (Modules_List
                                (PlayerShip.Modules(PlayerShip.UpgradeModule)
                                   .ProtoIndex)
                                .MaxValue /
                              20);
                        else
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .Damage2 :=
                             PlayerShip.Modules(PlayerShip.UpgradeModule)
                               .Damage2 +
                             1;
                        end if;
                        UpgradeValue :=
                          PlayerShip.Modules(PlayerShip.UpgradeModule).Damage2;
                     when HARPOON_GUN =>
                        if
                          (Modules_List
                             (PlayerShip.Modules(PlayerShip.UpgradeModule)
                                .ProtoIndex)
                             .MaxValue /
                           20) >
                          0 then
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .Duration :=
                             PlayerShip.Modules(PlayerShip.UpgradeModule)
                               .Duration +
                             (Modules_List
                                (PlayerShip.Modules(PlayerShip.UpgradeModule)
                                   .ProtoIndex)
                                .MaxValue /
                              20);
                        else
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .Duration :=
                             PlayerShip.Modules(PlayerShip.UpgradeModule)
                               .Duration +
                             1;
                        end if;
                        UpgradeValue :=
                          PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .Duration;
                     when others =>
                        null;
                  end case;
                  PlayerShip.Modules(PlayerShip.UpgradeModule).Weight :=
                    PlayerShip.Modules(PlayerShip.UpgradeModule).Weight +
                    WeightGain;
                  AddMessage
                    (To_String(PlayerShip.Crew(WorkerIndex).Name) &
                     "has upgraded " &
                     To_String
                       (PlayerShip.Modules(PlayerShip.UpgradeModule).Name) &
                     ".",
                     OrderMessage, GREEN);
                  MaxValue :=
                    Positive
                      (Float
                         (Modules_List
                            (PlayerShip.Modules(PlayerShip.UpgradeModule)
                               .ProtoIndex)
                            .MaxValue) *
                       1.5);
                  if UpgradeValue >= MaxValue then
                     MaxUpgradeReached("You've reached the maximum upgrade for ");
                     return;
                  else
                     case Modules_List
                       (PlayerShip.Modules(PlayerShip.UpgradeModule)
                          .ProtoIndex)
                       .MType is
                        when ENGINE =>
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .UpgradeProgress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (PlayerShip.Modules
                                        (PlayerShip.UpgradeModule)
                                        .ProtoIndex)
                                     .MaxValue /
                                   20) *
                                NewGameSettings.UpgradeCostBonus);
                        when HARPOON_GUN =>
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .UpgradeProgress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (PlayerShip.Modules
                                        (PlayerShip.UpgradeModule)
                                        .ProtoIndex)
                                     .MaxValue *
                                   10) *
                                NewGameSettings.UpgradeCostBonus);
                        when GUN | BATTERING_RAM =>
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .UpgradeProgress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (PlayerShip.Modules
                                        (PlayerShip.UpgradeModule)
                                        .ProtoIndex)
                                     .MaxValue *
                                   2) *
                                NewGameSettings.UpgradeCostBonus);
                        when CABIN =>
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .UpgradeProgress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (PlayerShip.Modules
                                        (PlayerShip.UpgradeModule)
                                        .ProtoIndex)
                                     .MaxValue) *
                                NewGameSettings.UpgradeCostBonus);
                        when HULL =>
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .UpgradeProgress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (PlayerShip.Modules
                                        (PlayerShip.UpgradeModule)
                                        .ProtoIndex)
                                     .MaxValue *
                                   40) *
                                NewGameSettings.UpgradeCostBonus);
                        when others =>
                           null;
                     end case;
                     if PlayerShip.Modules(PlayerShip.UpgradeModule)
                         .UpgradeProgress =
                       0 then
                        PlayerShip.Modules(PlayerShip.UpgradeModule)
                          .UpgradeProgress :=
                          1;
                     end if;
                  end if;
               when VALUE =>
                  if PlayerShip.Modules(PlayerShip.UpgradeModule).MType =
                    ENGINE then
                     WeightGain := WeightGain * 10;
                     PlayerShip.Modules(PlayerShip.UpgradeModule).FuelUsage :=
                       PlayerShip.Modules(PlayerShip.UpgradeModule).FuelUsage -
                       1;
                     UpgradeValue :=
                       PlayerShip.Modules(PlayerShip.UpgradeModule).FuelUsage;
                  end if;
                  PlayerShip.Modules(PlayerShip.UpgradeModule).Weight :=
                    PlayerShip.Modules(PlayerShip.UpgradeModule).Weight +
                    WeightGain;
                  AddMessage
                    (To_String(PlayerShip.Crew(WorkerIndex).Name) &
                     " has upgraded " &
                     To_String
                       (PlayerShip.Modules(PlayerShip.UpgradeModule).Name) &
                     ".",
                     OrderMessage, GREEN);
                  MaxValue :=
                    Natural
                      (Float
                         (Modules_List
                            (PlayerShip.Modules(PlayerShip.UpgradeModule)
                               .ProtoIndex)
                            .Value) /
                       2.0);
                  if MaxValue < 1 then
                     MaxValue := 1;
                  end if;
                  if UpgradeValue = MaxValue then
                     MaxUpgradeReached("You've reached the maximum upgrade for ");
                     return;
                  else
                     case Modules_List
                       (PlayerShip.Modules(PlayerShip.UpgradeModule)
                          .ProtoIndex)
                       .MType is
                        when ENGINE =>
                           PlayerShip.Modules(PlayerShip.UpgradeModule)
                             .UpgradeProgress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (PlayerShip.Modules
                                        (PlayerShip.UpgradeModule)
                                        .ProtoIndex)
                                     .Value *
                                   20) *
                                NewGameSettings.UpgradeCostBonus);
                           if PlayerShip.Modules(PlayerShip.UpgradeModule)
                               .UpgradeProgress =
                             0 then
                              PlayerShip.Modules(PlayerShip.UpgradeModule)
                                .UpgradeProgress :=
                                1;
                           end if;
                        when others =>
                           null;
                     end case;
                  end if;
               when others =>
                  null;
            end case;
         else
            PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeProgress :=
              UpgradeProgress;
         end if;
      end loop;
   end UpgradeShip;

end Ships.Upgrade;
