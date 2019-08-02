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

with Messages; use Messages;
with ShipModules; use ShipModules;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Crew.Inventory; use Crew.Inventory;

package body Ships.Repairs is

   procedure RepairShip(Minutes: Positive) is
      OrderTime, CurrentMinutes, RepairPoints: Integer;
      RepairNeeded, RepairStopped: Boolean := False;
      CrewRepairPoints: Natural_Container.Vector;
      procedure RepairModule(ModuleIndex: Positive) is
         PointsIndex, PointsBonus, RepairMaterial, ToolsIndex,
         RepairValue: Natural;
      begin
         PointsIndex := 0;
         RepairNeeded := True;
         RepairStopped := False;
         for J in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(J).Order /= Repair then
               goto End_Of_Loop;
            end if;
            PointsIndex := PointsIndex + 1;
            if CrewRepairPoints(PointsIndex) > 0 then
               PointsBonus :=
                 (GetSkillLevel
                    (PlayerShip.Crew(J),
                     Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                       .RepairSkill) /
                  10) *
                 CrewRepairPoints(PointsIndex);
               RepairPoints := CrewRepairPoints(PointsIndex) + PointsBonus;
               RepairMaterial :=
                 FindItem
                   (Inventory => PlayerShip.Cargo,
                    ItemType =>
                      Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                        .RepairMaterial);
               if RepairMaterial > 0
                 and then PlayerShip.Cargo(RepairMaterial).Amount <
                   RepairPoints then
                  RepairPoints := PlayerShip.Cargo(RepairMaterial).Amount;
               end if;
               if RepairMaterial = 0 then
                  AddMessage
                    ("You don't have repair materials to continue repairs of " &
                     To_String(PlayerShip.Modules(ModuleIndex).Name) & ".",
                     OrderMessage, RED);
                  RepairStopped := True;
                  return;
               end if;
               ToolsIndex :=
                 FindTools(Crew_Container.To_Index(J), RepairTools, Repair);
               if ToolsIndex = 0 then
                  if PointsIndex = 1 then
                     AddMessage
                       ("You don't have repair tools to continue repairs of " &
                        To_String(PlayerShip.Modules(ModuleIndex).Name) & ".",
                        OrderMessage, RED);
                  else
                     AddMessage
                       (To_String(PlayerShip.Crew(J).Name) &
                        " can't continue repairs due to lack of repair tools.",
                        OrderMessage, RED);
                  end if;
                  RepairStopped := True;
                  return;
               end if;
               -- Repair module
               if PlayerShip.Modules(ModuleIndex).Durability + RepairPoints >=
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
                 (Ship => PlayerShip, CargoIndex => RepairMaterial,
                  Amount => (0 - RepairValue));
               PlayerShip.Modules(ModuleIndex).Durability :=
                 PlayerShip.Modules(ModuleIndex).Durability + RepairValue;
               if RepairValue > CrewRepairPoints(PointsIndex) then
                  RepairValue := CrewRepairPoints(PointsIndex);
                  RepairPoints := 0;
               else
                  RepairPoints := CrewRepairPoints(PointsIndex) - RepairValue;
               end if;
               GainExp
                 (RepairValue,
                  Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                    .RepairSkill,
                  Crew_Container.To_Index(J));
               CrewRepairPoints(PointsIndex) := RepairPoints;
               DamageItem
                 (PlayerShip.Crew(J).Inventory, ToolsIndex,
                  GetSkillLevel
                    (PlayerShip.Crew(J),
                     Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                       .RepairSkill),
                  Crew_Container.To_Index(J));
               exit when not RepairNeeded;
            end if;
            <<End_Of_Loop>>
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
      if PlayerShip.RepairModule > 0
        and then PlayerShip.Modules(PlayerShip.RepairModule).Durability <
          PlayerShip.Modules(PlayerShip.RepairModule).MaxDurability then
         RepairModule(PlayerShip.RepairModule);
      end if;
      Repair_Loop :
      for I in PlayerShip.Modules.Iterate loop
         if PlayerShip.Modules(I).Durability <
           PlayerShip.Modules(I).MaxDurability then
            RepairModule(Modules_Container.To_Index(I));
         end if;
      end loop Repair_Loop;
      -- Send repair team on break if all is ok
      if not RepairNeeded or RepairStopped then
         if not RepairNeeded then
            AddMessage("All repairs are finished.", OrderMessage, GREEN);
         end if;
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Order = Repair then
               GiveOrders(PlayerShip, Crew_Container.To_Index(I), Rest);
            end if;
         end loop;
      end if;
   end RepairShip;

end Ships.Repairs;
