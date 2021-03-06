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
with Crew.Inventory; use Crew.Inventory;
with Config; use Config;

package body Ships.Upgrade is

   procedure StartUpgrading
     (ModuleIndex: Modules_Container.Extended_Index; UpgradeType: Positive) is
      MaxValue, UpgradeProgress: Natural;
      UpgradeAction: Ship_Upgrade;
   begin
      if Player_Ship.Modules(ModuleIndex).Durability = 0 and
        UpgradeType /= 3 then
         raise Ship_Upgrade_Error
           with "You can't upgrade " &
           To_String(Player_Ship.Modules(ModuleIndex).Name) &
           " because it's destroyed.";
      end if;
      case UpgradeType is
         when 1 => -- Upgrade durability
            MaxValue :=
              Natural
                (Float
                   (Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index)
                      .Durability) *
                 1.5);
            if Player_Ship.Modules(ModuleIndex).Max_Durability = MaxValue then
               raise Ship_Upgrade_Error
                 with "You can't further improve the durability of " &
                 To_String(Player_Ship.Modules(ModuleIndex).Name) & ".";
            end if;
            UpgradeAction := DURABILITY;
            UpgradeProgress :=
              Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index)
                .Durability;
         when 2 => -- Upgrade various max value of selected module
            MaxValue :=
              Natural
                (Float
                   (Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index)
                      .MaxValue) *
                 1.5);
            case Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index)
              .MType is
               when ENGINE =>
                  if Player_Ship.Modules(ModuleIndex).Power = MaxValue then
                     raise Ship_Upgrade_Error
                       with "You can't further improve the power of " &
                       To_String(Player_Ship.Modules(ModuleIndex).Name) & ".";
                  end if;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (Player_Ship.Modules(ModuleIndex).Proto_Index)
                            .MaxValue /
                          20) *
                       Float(New_Game_Settings.Upgrade_Cost_Bonus));
               when CABIN =>
                  if Player_Ship.Modules(ModuleIndex).Quality = MaxValue then
                     raise Ship_Upgrade_Error
                       with "You can't further improve the quality of " &
                       To_String(Player_Ship.Modules(ModuleIndex).Name) & ".";
                  end if;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (Player_Ship.Modules(ModuleIndex).Proto_Index)
                            .MaxValue) *
                       Float(New_Game_Settings.Upgrade_Cost_Bonus));
               when GUN | BATTERING_RAM =>
                  declare
                     Damage: constant Positive :=
                       (if Player_Ship.Modules(ModuleIndex).M_Type = GUN then
                          Player_Ship.Modules(ModuleIndex).Damage
                        else Player_Ship.Modules(ModuleIndex).Damage2);
                  begin
                     if Damage = MaxValue then
                        raise Ship_Upgrade_Error
                          with "You can't further improve the damage of " &
                          To_String(Player_Ship.Modules(ModuleIndex).Name) &
                          ".";
                     end if;
                  end;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (Player_Ship.Modules(ModuleIndex).Proto_Index)
                            .MaxValue *
                          2) *
                       Float(New_Game_Settings.Upgrade_Cost_Bonus));
               when HULL =>
                  if Player_Ship.Modules(ModuleIndex).Max_Modules =
                    MaxValue then
                     raise Ship_Upgrade_Error
                       with "You can't further enlarge the size of" &
                       To_String(Player_Ship.Modules(ModuleIndex).Name) & ".";
                  end if;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (Player_Ship.Modules(ModuleIndex).Proto_Index)
                            .MaxValue *
                          40) *
                       Float(New_Game_Settings.Upgrade_Cost_Bonus));
               when HARPOON_GUN =>
                  if Player_Ship.Modules(ModuleIndex).Duration = MaxValue then
                     raise Ship_Upgrade_Error
                       with "You can't further improve the strength of " &
                       To_String(Player_Ship.Modules(ModuleIndex).Name) & ".";
                  end if;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (Player_Ship.Modules(ModuleIndex).Proto_Index)
                            .MaxValue *
                          10) *
                       Float(New_Game_Settings.Upgrade_Cost_Bonus));
               when others =>
                  raise Ship_Upgrade_Error
                    with To_String(Player_Ship.Modules(ModuleIndex).Name) &
                    " can't be upgraded in that way.";
            end case;
            UpgradeAction := MAX_VALUE;
         when 3 => -- Upgrade various value of selected module
            case Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index)
              .MType is
               when ENGINE =>
                  MaxValue :=
                    Natural
                      (Float
                         (Modules_List
                            (Player_Ship.Modules(ModuleIndex).Proto_Index)
                            .Value) /
                       2.0);
                  if MaxValue < 1 then
                     MaxValue := 1;
                  end if;
                  if Player_Ship.Modules(ModuleIndex).Fuel_Usage =
                    MaxValue then
                     raise Ship_Upgrade_Error
                       with "You can't further reduce the fuel usage of " &
                       To_String(Player_Ship.Modules(ModuleIndex).Name) & ".";
                  end if;
                  UpgradeProgress :=
                    Integer
                      (Float
                         (Modules_List
                            (Player_Ship.Modules(ModuleIndex).Proto_Index)
                            .Value *
                          20) *
                       Float(New_Game_Settings.Upgrade_Cost_Bonus));
               when others =>
                  raise Ship_Upgrade_Error
                    with To_String(Player_Ship.Modules(ModuleIndex).Name) &
                    " can't be upgraded in that way.";
            end case;
            UpgradeAction := VALUE;
         when 4 => -- Continue previous upgrade
            if Player_Ship.Modules(ModuleIndex).Upgrade_Action = NONE then
               raise Ship_Upgrade_Error
                 with To_String(Player_Ship.Modules(ModuleIndex).Name) &
                 " doesn't have any upgrade set yet.";
            end if;
            UpgradeAction := Player_Ship.Modules(ModuleIndex).Upgrade_Action;
         when others =>
            return;
      end case;
      declare
         MaterialIndex: constant Inventory_Container.Extended_Index :=
           FindItem
             (Inventory => Player_Ship.Cargo,
              ItemType =>
                Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index)
                  .RepairMaterial);
      begin
         if MaterialIndex = 0 then
            Materials_Loop :
            for Item of Items_List loop
               if Item.IType =
                 Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index)
                   .RepairMaterial then
                  raise Ship_Upgrade_Error
                    with "You don't have the " & To_String(Item.Name) &
                    " to upgrade " &
                    To_String(Player_Ship.Modules(ModuleIndex).Name) & ".";
               end if;
            end loop Materials_Loop;
         end if;
      end;
      Player_Ship.Upgrade_Module := ModuleIndex;
      if Player_Ship.Modules(ModuleIndex).Upgrade_Action /= UpgradeAction then
         Player_Ship.Modules(ModuleIndex).Upgrade_Progress :=
           Integer
             (Float(UpgradeProgress) *
              Float(New_Game_Settings.Upgrade_Cost_Bonus));
         if Player_Ship.Modules(ModuleIndex).Upgrade_Progress = 0 then
            Player_Ship.Modules(ModuleIndex).Upgrade_Progress := 1;
         end if;
         Player_Ship.Modules(ModuleIndex).Upgrade_Action := UpgradeAction;
      end if;
      AddMessage
        ("You set the " & To_String(Player_Ship.Modules(ModuleIndex).Name) &
         " to upgrade.",
         OrderMessage);
   end StartUpgrading;

   procedure UpgradeShip(Minutes: Positive) is
      ResultAmount, UpgradePoints, UpgradeProgress, MaterialCost,
      MaxValue: Natural := 0;
      UpgradeValue: Positive;
      WeightGain: Natural;
      Times: Natural := 0;
      OrderTime, CurrentMinutes: Integer;
      UpgradedModule: Module_Data;
      UpgradeMaterial, UpgradeTools: Inventory_Container.Extended_Index;
      WorkerIndex: Crew_Container.Extended_Index;
      procedure FindMatsAndTools is
      begin
         UpgradeTools := FindTools(WorkerIndex, Repair_Tools, Upgrading);
         UpgradeMaterial :=
           FindItem
             (Inventory => Player_Ship.Cargo,
              ItemType =>
                Modules_List(UpgradedModule.Proto_Index).RepairMaterial);
      end FindMatsAndTools;
      procedure MaxUpgradeReached(MessageText: String) is
      begin
         AddMessage
           (MessageText & To_String(UpgradedModule.Name) & ".", OrderMessage,
            YELLOW);
         UpgradedModule.Upgrade_Progress := 0;
         UpgradedModule.Upgrade_Action := NONE;
         Player_Ship.Modules(Player_Ship.Upgrade_Module) := UpgradedModule;
         Player_Ship.Upgrade_Module := 0;
         GiveOrders(Player_Ship, WorkerIndex, Rest);
      end MaxUpgradeReached;
   begin
      if Player_Ship.Upgrade_Module = 0 then
         return;
      end if;
      WorkerIndex := FindMember(Upgrading);
      if WorkerIndex = 0 then
         return;
      end if;
      UpgradedModule := Player_Ship.Modules(Player_Ship.Upgrade_Module);
      CurrentMinutes := Minutes;
      OrderTime := Player_Ship.Crew(WorkerIndex).OrderTime;
      if UpgradedModule.Durability = 0 then
         AddMessage
           (To_String(Player_Ship.Crew(WorkerIndex).Name) &
            " stops upgrading " & To_String(UpgradedModule.Name) &
            " because it's destroyed.",
            OrderMessage, RED);
         GiveOrders(Player_Ship, WorkerIndex, Rest);
         return;
      end if;
      Count_Time_Loop :
      while CurrentMinutes > 0 loop
         if CurrentMinutes >= OrderTime then
            CurrentMinutes := CurrentMinutes - OrderTime;
            Times := Times + 1;
            OrderTime := 15;
         else
            OrderTime := OrderTime - CurrentMinutes;
            CurrentMinutes := 0;
         end if;
      end loop Count_Time_Loop;
      Player_Ship.Crew(WorkerIndex).OrderTime := OrderTime;
      if Times = 0 then
         return;
      end if;
      UpgradePoints :=
        ((GetSkillLevel
            (Player_Ship.Crew(WorkerIndex),
             Modules_List(UpgradedModule.Proto_Index).RepairSkill) /
          10) *
         Times) +
        Times;
      Upgrade_Loop :
      while UpgradePoints > 0 and UpgradedModule.Upgrade_Progress > 0 loop
         ResultAmount := UpgradePoints;
         if ResultAmount > UpgradedModule.Upgrade_Progress then
            ResultAmount := UpgradedModule.Upgrade_Progress;
         end if;
         FindMatsAndTools;
         if UpgradeMaterial = 0 then
            AddMessage
              ("You don't have enough materials to upgrade " &
               To_String(UpgradedModule.Name),
               OrderMessage, RED);
            GiveOrders(Player_Ship, WorkerIndex, Rest);
            exit Upgrade_Loop;
         end if;
         if UpgradeTools = 0 then
            AddMessage
              ("You don't have the repair tool to upgrade " &
               To_String(UpgradedModule.Name),
               OrderMessage, RED);
            GiveOrders(Player_Ship, WorkerIndex, Rest);
            exit Upgrade_Loop;
         end if;
         if UpgradedModule.Upgrade_Action = MAX_VALUE then
            case UpgradedModule.M_Type is
               when ENGINE =>
                  if ResultAmount >
                    Player_Ship.Cargo(UpgradeMaterial).Amount * 200 then
                     ResultAmount :=
                       Player_Ship.Cargo(UpgradeMaterial).Amount * 200;
                  end if;
                  MaterialCost := ResultAmount / 200;
               when CABIN =>
                  if ResultAmount >
                    Player_Ship.Cargo(UpgradeMaterial).Amount * 20 then
                     ResultAmount :=
                       Player_Ship.Cargo(UpgradeMaterial).Amount * 20;
                  end if;
                  MaterialCost := ResultAmount / 20;
               when GUN | BATTERING_RAM | HARPOON_GUN =>
                  if ResultAmount >
                    Player_Ship.Cargo(UpgradeMaterial).Amount * 10 then
                     ResultAmount :=
                       Player_Ship.Cargo(UpgradeMaterial).Amount * 10;
                  end if;
                  MaterialCost := ResultAmount / 10;
               when HULL =>
                  if ResultAmount >
                    Player_Ship.Cargo(UpgradeMaterial).Amount * 2 then
                     ResultAmount :=
                       Player_Ship.Cargo(UpgradeMaterial).Amount * 2;
                  end if;
                  MaterialCost := ResultAmount / 2;
               when others =>
                  if ResultAmount >
                    Player_Ship.Cargo(UpgradeMaterial).Amount then
                     ResultAmount := Player_Ship.Cargo(UpgradeMaterial).Amount;
                  end if;
                  MaterialCost := ResultAmount;
            end case;
         elsif UpgradedModule.Upgrade_Action = DURABILITY then
            if ResultAmount >
              Player_Ship.Cargo(UpgradeMaterial).Amount * 10 then
               ResultAmount := Player_Ship.Cargo(UpgradeMaterial).Amount * 10;
            end if;
            MaterialCost := ResultAmount / 10;
         else
            if ResultAmount > Player_Ship.Cargo(UpgradeMaterial).Amount then
               ResultAmount := Player_Ship.Cargo(UpgradeMaterial).Amount;
            end if;
            MaterialCost := ResultAmount;
         end if;
         if MaterialCost < Times then
            MaterialCost := Times;
         end if;
         if MaterialCost > Player_Ship.Cargo(UpgradeMaterial).Amount then
            MaterialCost := Player_Ship.Cargo(UpgradeMaterial).Amount;
         end if;
         GainExp
           (ResultAmount, Modules_List(UpgradedModule.Proto_Index).RepairSkill,
            WorkerIndex);
         DamageItem
           (Player_Ship.Crew(WorkerIndex).Inventory, UpgradeTools,
            GetSkillLevel
              (Player_Ship.Crew(WorkerIndex),
               Modules_List(UpgradedModule.Proto_Index).RepairSkill),
            WorkerIndex);
         FindMatsAndTools;
         UpgradeProgress := UpgradedModule.Upgrade_Progress - ResultAmount;
         UpgradePoints := UpgradePoints - ResultAmount;
         UpdateCargo
           (Player_Ship, Player_Ship.Cargo.Element(UpgradeMaterial).ProtoIndex,
            (0 - MaterialCost));
         if UpgradeProgress = 0 then
            WeightGain :=
              Modules_List(UpgradedModule.Proto_Index).Weight /
              Modules_List(UpgradedModule.Proto_Index).Durability;
            if WeightGain < 1 then
               WeightGain := 1;
            end if;
            case UpgradedModule.Upgrade_Action is
               when DURABILITY =>
                  if
                    (Modules_List(UpgradedModule.Proto_Index).Durability /
                     20) >
                    0 then
                     UpgradedModule.Max_Durability :=
                       UpgradedModule.Max_Durability +
                       (Modules_List(UpgradedModule.Proto_Index).Durability /
                        20);
                     UpgradedModule.Weight :=
                       UpgradedModule.Weight +
                       (WeightGain *
                        (Modules_List(UpgradedModule.Proto_Index).Durability /
                         20));
                  else
                     UpgradedModule.Max_Durability :=
                       UpgradedModule.Max_Durability + 1;
                     UpgradedModule.Weight :=
                       UpgradedModule.Weight + WeightGain;
                  end if;
                  AddMessage
                    (To_String(Player_Ship.Crew(WorkerIndex).Name) &
                     " has upgraded the durability of " &
                     To_String(UpgradedModule.Name) & ".",
                     OrderMessage, GREEN);
                  MaxValue :=
                    Positive
                      (Float
                         (Modules_List(UpgradedModule.Proto_Index)
                            .Durability) *
                       1.5);
                  if UpgradedModule.Max_Durability = MaxValue then
                     MaxUpgradeReached
                       ("You've reached the maximum durability for ");
                     return;
                  else
                     UpgradedModule.Upgrade_Progress :=
                       Modules_List(UpgradedModule.Proto_Index).Durability;
                  end if;
               when MAX_VALUE =>
                  case UpgradedModule.M_Type is
                     when HULL =>
                        WeightGain := WeightGain * 10;
                        UpgradedModule.Max_Modules :=
                          UpgradedModule.Max_Modules + 1;
                        UpgradeValue := UpgradedModule.Max_Modules;
                     when ENGINE =>
                        WeightGain :=
                          (Modules_List(UpgradedModule.Proto_Index).MaxValue /
                           40);
                        UpgradedModule.Power :=
                          UpgradedModule.Power +
                          (Modules_List(UpgradedModule.Proto_Index).MaxValue /
                           20);
                        UpgradeValue := UpgradedModule.Power;
                     when CABIN =>
                        UpgradedModule.Quality :=
                          UpgradedModule.Quality +
                          (Modules_List(UpgradedModule.Proto_Index).MaxValue /
                           20);
                        UpgradeValue := UpgradedModule.Quality;
                     when GUN =>
                        if
                          (Modules_List(UpgradedModule.Proto_Index).MaxValue /
                           20) >
                          0 then
                           UpgradedModule.Damage :=
                             UpgradedModule.Damage +
                             (Modules_List(UpgradedModule.Proto_Index)
                                .MaxValue /
                              20);
                        else
                           UpgradedModule.Damage := UpgradedModule.Damage + 1;
                        end if;
                        UpgradeValue := UpgradedModule.Damage;
                     when BATTERING_RAM =>
                        if
                          (Modules_List(UpgradedModule.Proto_Index).MaxValue /
                           20) >
                          0 then
                           UpgradedModule.Damage2 :=
                             UpgradedModule.Damage2 +
                             (Modules_List(UpgradedModule.Proto_Index)
                                .MaxValue /
                              20);
                        else
                           UpgradedModule.Damage2 :=
                             UpgradedModule.Damage2 + 1;
                        end if;
                        UpgradeValue := UpgradedModule.Damage2;
                     when HARPOON_GUN =>
                        if
                          (Modules_List(UpgradedModule.Proto_Index).MaxValue /
                           20) >
                          0 then
                           UpgradedModule.Duration :=
                             UpgradedModule.Duration +
                             (Modules_List(UpgradedModule.Proto_Index)
                                .MaxValue /
                              20);
                        else
                           UpgradedModule.Duration :=
                             UpgradedModule.Duration + 1;
                        end if;
                        UpgradeValue := UpgradedModule.Duration;
                     when others =>
                        null;
                  end case;
                  UpgradedModule.Weight := UpgradedModule.Weight + WeightGain;
                  AddMessage
                    (To_String(Player_Ship.Crew(WorkerIndex).Name) &
                     " has upgraded " & To_String(UpgradedModule.Name) & ".",
                     OrderMessage, GREEN);
                  MaxValue :=
                    Positive
                      (Float
                         (Modules_List(UpgradedModule.Proto_Index).MaxValue) *
                       1.5);
                  if UpgradeValue >= MaxValue then
                     MaxUpgradeReached
                       ("You've reached the maximum upgrade for ");
                     return;
                  else
                     case Modules_List(UpgradedModule.Proto_Index).MType is
                        when ENGINE =>
                           UpgradedModule.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (Player_Ship.Modules
                                        (Player_Ship.Upgrade_Module)
                                        .Proto_Index)
                                     .MaxValue /
                                   20) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                        when HARPOON_GUN =>
                           UpgradedModule.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (Player_Ship.Modules
                                        (Player_Ship.Upgrade_Module)
                                        .Proto_Index)
                                     .MaxValue *
                                   10) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                        when GUN | BATTERING_RAM =>
                           UpgradedModule.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (Player_Ship.Modules
                                        (Player_Ship.Upgrade_Module)
                                        .Proto_Index)
                                     .MaxValue *
                                   2) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                        when CABIN =>
                           UpgradedModule.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (Player_Ship.Modules
                                        (Player_Ship.Upgrade_Module)
                                        .Proto_Index)
                                     .MaxValue) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                        when HULL =>
                           UpgradedModule.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (Player_Ship.Modules
                                        (Player_Ship.Upgrade_Module)
                                        .Proto_Index)
                                     .MaxValue *
                                   40) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                        when others =>
                           null;
                     end case;
                     if UpgradedModule.Upgrade_Progress = 0 then
                        UpgradedModule.Upgrade_Progress := 1;
                     end if;
                  end if;
               when VALUE =>
                  if UpgradedModule.M_Type = ENGINE then
                     WeightGain := WeightGain * 10;
                     UpgradedModule.Fuel_Usage :=
                       UpgradedModule.Fuel_Usage - 1;
                     UpgradeValue := UpgradedModule.Fuel_Usage;
                  end if;
                  UpgradedModule.Weight := UpgradedModule.Weight + WeightGain;
                  AddMessage
                    (To_String(Player_Ship.Crew(WorkerIndex).Name) &
                     " has upgraded " & To_String(UpgradedModule.Name) & ".",
                     OrderMessage, GREEN);
                  MaxValue :=
                    Natural
                      (Float(Modules_List(UpgradedModule.Proto_Index).Value) /
                       2.0);
                  if MaxValue < 1 then
                     MaxValue := 1;
                  end if;
                  if UpgradeValue = MaxValue then
                     MaxUpgradeReached
                       ("You've reached the maximum upgrade for ");
                     return;
                  else
                     case Modules_List(UpgradedModule.Proto_Index).MType is
                        when ENGINE =>
                           UpgradedModule.Upgrade_Progress :=
                             Integer
                               (Float
                                  (Modules_List
                                     (Player_Ship.Modules
                                        (Player_Ship.Upgrade_Module)
                                        .Proto_Index)
                                     .Value *
                                   20) *
                                Float(New_Game_Settings.Upgrade_Cost_Bonus));
                           if UpgradedModule.Upgrade_Progress = 0 then
                              UpgradedModule.Upgrade_Progress := 1;
                           end if;
                        when others =>
                           null;
                     end case;
                  end if;
               when others =>
                  null;
            end case;
         else
            UpgradedModule.Upgrade_Progress := UpgradeProgress;
         end if;
      end loop Upgrade_Loop;
      Player_Ship.Modules(Player_Ship.Upgrade_Module) := UpgradedModule;
   end UpgradeShip;

end Ships.Upgrade;
