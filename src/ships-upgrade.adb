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

with Messages; use Messages;
with Items; use Items;
with UserInterface; use UserInterface;
with ShipModules; use ShipModules;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;

package body Ships.Upgrade is

   procedure StartUpgrading(ModuleIndex, UpgradeType: Positive) is
      MaxValue: Natural;
      HaveMaterials, HaveTools: Boolean := False;
      UpgradeProgress: Positive;
      UpgradeAction: ShipUpgrade;
   begin
      if PlayerShip.Modules.Element(ModuleIndex).Durability = 0 and
        UpgradeType /= 3 then
         ShowDialog
           ("You can't upgrade " &
            To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
            " because is destroyed.");
         return;
      end if;
      case UpgradeType is
         when 1 => -- Upgrade durability
            MaxValue :=
              Natural
                (Float
                   (Modules_List.Element
                    (PlayerShip.Modules.Element(ModuleIndex).ProtoIndex)
                      .Durability) *
                 1.5);
            if PlayerShip.Modules.Element(ModuleIndex).MaxDurability =
              MaxValue then
               ShowDialog
                 ("You can't improve more durability of " &
                  To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
                  ".");
               return;
            end if;
            UpgradeAction := DURABILITY;
            UpgradeProgress := 10;
         when 2 => -- Upgrade various max value of selected module
            MaxValue :=
              Natural
                (Float
                   (Modules_List.Element
                    (PlayerShip.Modules.Element(ModuleIndex).ProtoIndex)
                      .MaxValue) *
                 1.5);
            case Modules_List.Element
            (PlayerShip.Modules.Element(ModuleIndex).ProtoIndex)
              .MType is
               when ENGINE =>
                  if PlayerShip.Modules.Element(ModuleIndex).Max_Value =
                    MaxValue then
                     ShowDialog
                       ("You can't improve more power of " &
                        To_String
                          (PlayerShip.Modules.Element(ModuleIndex).Name) &
                        ".");
                     return;
                  end if;
                  UpgradeProgress := 10;
               when CABIN =>
                  if PlayerShip.Modules.Element(ModuleIndex).Max_Value =
                    MaxValue then
                     ShowDialog
                       ("You can't improve more quality of " &
                        To_String
                          (PlayerShip.Modules.Element(ModuleIndex).Name) &
                        ".");
                     return;
                  end if;
                  UpgradeProgress := 100;
               when GUN | BATTERING_RAM =>
                  if PlayerShip.Modules.Element(ModuleIndex).Max_Value =
                    MaxValue then
                     ShowDialog
                       ("You can't improve more damage of " &
                        To_String
                          (PlayerShip.Modules.Element(ModuleIndex).Name) &
                        ".");
                     return;
                  end if;
                  UpgradeProgress := 100;
               when HULL =>
                  if PlayerShip.Modules.Element(ModuleIndex).Max_Value =
                    MaxValue then
                     ShowDialog
                       ("You can't enlarge more " &
                        To_String
                          (PlayerShip.Modules.Element(ModuleIndex).Name) &
                        ".");
                     return;
                  end if;
                  UpgradeProgress := 500;
               when others =>
                  ShowDialog
                    (To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
                     " can't be upgraded in that way.");
                  return;
            end case;
            UpgradeAction := MAX_VALUE;
         when 3 => -- Upgrade various value of selected module
            MaxValue :=
              Natural
                (Float
                   (Modules_List.Element
                    (PlayerShip.Modules.Element(ModuleIndex).ProtoIndex)
                      .Value) *
                 1.5);
            case Modules_List.Element
            (PlayerShip.Modules.Element(ModuleIndex).ProtoIndex)
              .MType is
               when ENGINE =>
                  if PlayerShip.Modules.Element(ModuleIndex).Current_Value =
                    MaxValue then
                     ShowDialog
                       ("You can't reduce more fuel usage of " &
                        To_String
                          (PlayerShip.Modules.Element(ModuleIndex).Name) &
                        ".");
                     return;
                  end if;
                  UpgradeProgress := 100;
               when others =>
                  ShowDialog
                    (To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
                     " can't be upgraded in that way.");
                  return;
            end case;
            UpgradeAction := VALUE;
         when 4 => -- Continue previous upgrade
            if PlayerShip.Modules.Element(ModuleIndex).UpgradeAction =
              NONE then
               ShowDialog
                 (To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
                  " don't have set any upgrade yet.");
               return;
            end if;
            UpgradeAction :=
              PlayerShip.Modules.Element(ModuleIndex).UpgradeAction;
         when others =>
            return;
      end case;
      for Item of PlayerShip.Cargo loop
         if Items_List(Item.ProtoIndex).IType =
           Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
             .RepairMaterial then
            HaveMaterials := True;
         elsif Items_List.Element(Item.ProtoIndex).IType =
           To_Unbounded_String("RepairTools") then
            HaveTools := True;
         end if;
         exit when HaveMaterials and HaveTools;
      end loop;
      if not HaveMaterials then
         for Item of Items_List loop
            if Item.IType =
              Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                .RepairMaterial then
               ShowDialog
                 ("You don't have " &
                  To_String(Item.Name) &
                  " for upgrading " &
                  To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
                  ".");
               return;
            end if;
         end loop;
      end if;
      if not HaveTools then
         ShowDialog
           ("You don't have repair tools for upgrading " &
            To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
            ".");
         return;
      end if;
      PlayerShip.UpgradeModule := ModuleIndex;
      if PlayerShip.Modules.Element(ModuleIndex).UpgradeAction /=
        UpgradeAction then
         UpdateModule
           (PlayerShip,
            ModuleIndex,
            "UpgradeProgress",
            Integer'Image(UpgradeProgress));
         UpdateModule
           (PlayerShip,
            ModuleIndex,
            "UpgradeAction",
            ShipUpgrade'Image(UpgradeAction));
      end if;
      AddMessage
        ("You set " &
         To_String(PlayerShip.Modules.Element(ModuleIndex).Name) &
         " to upgrade.",
         OrderMessage);
   end StartUpgrading;

   procedure UpgradeShip(Minutes: Positive) is
      ResultAmount,
      UpgradePoints,
      WorkerIndex,
      UpgradeMaterial,
      UpgradeProgress,
      UpgradeTools: Natural :=
        0;
      MaxValue: Positive;
      WeightGain: Natural;
      Times: Natural := 0;
      OrderTime, CurrentMinutes: Integer;
      procedure UpdateMember(Member: in out Member_Data) is
      begin
         Member.OrderTime := OrderTime;
      end UpdateMember;
   begin
      WorkerIndex := FindMember(Upgrading);
      if WorkerIndex = 0 then
         return;
      end if;
      CurrentMinutes := Minutes;
      OrderTime := PlayerShip.Crew.Element(WorkerIndex).OrderTime;
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
      PlayerShip.Crew.Update_Element
      (Index => WorkerIndex, Process => UpdateMember'Access);
      if Times = 0 then
         return;
      end if;
      UpgradePoints :=
        ((GetSkillLevel
            (WorkerIndex,
             Modules_List.Element
             (PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex)
               .RepairSkill) /
          10) *
         Times) +
        Times;
      while UpgradePoints > 0 and
        PlayerShip.Modules.Element(PlayerShip.UpgradeModule).UpgradeProgress >
          0 loop
         ResultAmount := UpgradePoints;
         if ResultAmount >
           PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
             .UpgradeProgress then
            ResultAmount :=
              PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                .UpgradeProgress;
         end if;
         UpgradeMaterial := 0;
         for I in
           PlayerShip.Cargo.First_Index .. PlayerShip.Cargo.Last_Index loop
            if Items_List(PlayerShip.Cargo.Element(I).ProtoIndex).IType =
              Modules_List
                (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                .RepairMaterial then
               UpgradeMaterial := I;
            elsif Items_List.Element(PlayerShip.Cargo.Element(I).ProtoIndex)
                .IType =
              To_Unbounded_String("RepairTools") then
               UpgradeTools := I;
            end if;
            exit when UpgradeMaterial > 0 and UpgradeTools > 0;
         end loop;
         if UpgradeMaterial = 0 then
            AddMessage
              ("You don't have enough materials to upgrade " &
               To_String
                 (PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name),
               OrderMessage);
            GiveOrders(WorkerIndex, Rest);
            exit;
         end if;
         if UpgradeTools = 0 then
            AddMessage
              ("You don't have repair tools to upgrade " &
               To_String
                 (PlayerShip.Modules.Element(PlayerShip.UpgradeModule).Name),
               OrderMessage);
            GiveOrders(WorkerIndex, Rest);
            exit;
         end if;
         if ResultAmount >
           PlayerShip.Cargo.Element(UpgradeMaterial).Amount then
            ResultAmount := PlayerShip.Cargo.Element(UpgradeMaterial).Amount;
         end if;
         GainExp
           (ResultAmount,
            Modules_List.Element
            (PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex)
              .RepairSkill,
            WorkerIndex);
         DamageCargo
           (UpgradeTools,
            WorkerIndex,
            Modules_List.Element
            (PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex)
              .RepairSkill);
         UpgradeProgress :=
           PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
             .UpgradeProgress -
           ResultAmount;
         UpgradePoints := UpgradePoints - ResultAmount;
         UpdateCargo
           (PlayerShip,
            PlayerShip.Cargo.Element(UpgradeMaterial).ProtoIndex,
            (0 - ResultAmount));
         if UpgradeProgress = 0 then
            WeightGain :=
              Modules_List.Element
              (PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex)
                .Weight /
              Modules_List.Element
              (PlayerShip.Modules.Element(PlayerShip.UpgradeModule).ProtoIndex)
                .Durability;
            if WeightGain < 1 then
               WeightGain := 1;
            end if;
            case PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
              .UpgradeAction is
               when DURABILITY =>
                  UpdateModule
                    (PlayerShip,
                     PlayerShip.UpgradeModule,
                     "MaxDurability",
                     "1");
                  UpdateModule
                    (PlayerShip,
                     PlayerShip.UpgradeModule,
                     "Weight",
                     Natural'Image(WeightGain));
                  AddMessage
                    (To_String(PlayerShip.Crew.Element(WorkerIndex).Name) &
                     " was upgraded durability of " &
                     To_String
                       (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                          .Name) &
                     ".",
                     OrderMessage);
                  MaxValue :=
                    Positive
                      (Float
                         (Modules_List.Element
                          (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                             .ProtoIndex)
                            .Durability) *
                       1.5);
                  if PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                      .MaxDurability =
                    MaxValue then
                     AddMessage
                       ("You reached maximum durability for " &
                        To_String
                          (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                             .Name) &
                        ".",
                        OrderMessage);
                     UpdateModule
                       (PlayerShip,
                        PlayerShip.UpgradeModule,
                        "UpgradeProgress",
                        "0");
                     UpdateModule
                       (PlayerShip,
                        PlayerShip.UpgradeModule,
                        "UpgradeAction",
                        "NONE");
                     PlayerShip.UpgradeModule := 0;
                     GiveOrders(WorkerIndex, Rest);
                     return;
                  else
                     UpdateModule
                       (PlayerShip,
                        PlayerShip.UpgradeModule,
                        "UpgradeProgress",
                        "10");
                  end if;
               when MAX_VALUE =>
                  UpdateModule
                    (PlayerShip,
                     PlayerShip.UpgradeModule,
                     "Max_Value",
                     "1");
                  case Modules_List.Element
                  (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                     .ProtoIndex)
                    .MType is
                     when HULL =>
                        WeightGain := WeightGain * 10;
                     when ENGINE =>
                        WeightGain := 1;
                     when others =>
                        null;
                  end case;
                  UpdateModule
                    (PlayerShip,
                     PlayerShip.UpgradeModule,
                     "Weight",
                     Natural'Image(WeightGain));
                  AddMessage
                    (To_String(PlayerShip.Crew.Element(WorkerIndex).Name) &
                     " was upgraded " &
                     To_String
                       (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                          .Name) &
                     ".",
                     OrderMessage);
                  MaxValue :=
                    Positive
                      (Float
                         (Modules_List.Element
                          (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                             .ProtoIndex)
                            .MaxValue) *
                       1.5);
                  if PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                      .Max_Value =
                    MaxValue then
                     AddMessage
                       ("You reached maximum upgrade for " &
                        To_String
                          (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                             .Name) &
                        ".",
                        OrderMessage);
                     UpdateModule
                       (PlayerShip,
                        PlayerShip.UpgradeModule,
                        "UpgradeProgress",
                        "0");
                     UpdateModule
                       (PlayerShip,
                        PlayerShip.UpgradeModule,
                        "UpgradeAction",
                        "NONE");
                     PlayerShip.UpgradeModule := 0;
                     GiveOrders(WorkerIndex, Rest);
                     return;
                  else
                     case Modules_List.Element
                     (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                        .ProtoIndex)
                       .MType is
                        when ENGINE =>
                           UpdateModule
                             (PlayerShip,
                              PlayerShip.UpgradeModule,
                              "UpgradeProgress",
                              "10");
                        when CABIN =>
                           UpdateModule
                             (PlayerShip,
                              PlayerShip.UpgradeModule,
                              "UpgradeProgress",
                              "100");
                        when GUN | BATTERING_RAM =>
                           UpdateModule
                             (PlayerShip,
                              PlayerShip.UpgradeModule,
                              "UpgradeProgress",
                              "100");
                        when HULL =>
                           UpdateModule
                             (PlayerShip,
                              PlayerShip.UpgradeModule,
                              "UpgradeProgress",
                              "500");
                        when others =>
                           null;
                     end case;
                  end if;
               when VALUE =>
                  UpdateModule
                    (PlayerShip,
                     PlayerShip.UpgradeModule,
                     "Current_Value",
                     Integer'
                       Image
                         (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                            .Current_Value -
                          1));
                  case Modules_List.Element
                  (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                     .ProtoIndex)
                    .MType is
                     when ENGINE =>
                        WeightGain := WeightGain * 10;
                     when others =>
                        null;
                  end case;
                  UpdateModule
                    (PlayerShip,
                     PlayerShip.UpgradeModule,
                     "Weight",
                     Natural'Image(WeightGain));
                  AddMessage
                    (To_String(PlayerShip.Crew.Element(WorkerIndex).Name) &
                     " was upgraded " &
                     To_String
                       (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                          .Name) &
                     ".",
                     OrderMessage);
                  MaxValue :=
                    Positive
                      (Float
                         (Modules_List.Element
                          (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                             .ProtoIndex)
                            .Value) /
                       2.0);
                  if PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                      .Current_Value =
                    MaxValue then
                     AddMessage
                       ("You reached maximum upgrade for " &
                        To_String
                          (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                             .Name) &
                        ".",
                        OrderMessage);
                     UpdateModule
                       (PlayerShip,
                        PlayerShip.UpgradeModule,
                        "UpgradeProgress",
                        "0");
                     UpdateModule
                       (PlayerShip,
                        PlayerShip.UpgradeModule,
                        "UpgradeAction",
                        "NONE");
                     PlayerShip.UpgradeModule := 0;
                     GiveOrders(WorkerIndex, Rest);
                     return;
                  else
                     case Modules_List.Element
                     (PlayerShip.Modules.Element(PlayerShip.UpgradeModule)
                        .ProtoIndex)
                       .MType is
                        when ENGINE =>
                           UpdateModule
                             (PlayerShip,
                              PlayerShip.UpgradeModule,
                              "UpgradeProgress",
                              "100");
                        when others =>
                           null;
                     end case;
                  end if;
               when others =>
                  null;
            end case;
         else
            UpdateModule
              (PlayerShip,
               PlayerShip.UpgradeModule,
               "UpgradeProgress",
               Integer'Image(UpgradeProgress));
         end if;
      end loop;
   end UpgradeShip;

end Ships.Upgrade;
