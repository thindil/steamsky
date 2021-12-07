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

package body Ships.Repairs is

   procedure RepairShip(Minutes: Positive) is
      OrderTime, CurrentMinutes, RepairPoints: Integer;
      RepairNeeded, RepairStopped: Boolean := False;
      CrewRepairPoints: Natural_Container.Vector;
      procedure RepairModule(ModuleIndex: Positive) is
         PointsIndex, PointsBonus, RepairValue: Natural;
         RepairMaterial, ToolsIndex: Inventory_Container.Extended_Index;
      begin
         PointsIndex := 0;
         RepairNeeded := True;
         RepairStopped := False;
         Repair_Module_Loop :
         for J in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(J).Order /= REPAIR then
               goto End_Of_Loop;
            end if;
            PointsIndex := PointsIndex + 1;
            if CrewRepairPoints(PointsIndex) > 0 then
               PointsBonus :=
                 (GetSkillLevel
                    (Player_Ship.Crew(J),
                     Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index)
                       .RepairSkill) /
                  10) *
                 CrewRepairPoints(PointsIndex);
               RepairPoints := CrewRepairPoints(PointsIndex) + PointsBonus;
               ToolsIndex :=
                 FindTools(Crew_Container.To_Index(J), Repair_Tools, REPAIR);
               if ToolsIndex = 0 then
                  if PointsIndex = 1 then
                     Add_Message
                       ("You don't have the proper repair tools to continue repairs of " &
                        To_String(Player_Ship.Modules(ModuleIndex).Name) & ".",
                        ORDERMESSAGE, RED);
                  else
                     Add_Message
                       (To_String(Player_Ship.Crew(J).Name) &
                        " can't continue repairs due to a lack of repair tools.",
                        ORDERMESSAGE, RED);
                  end if;
                  RepairStopped := True;
                  return;
               end if;
               RepairMaterial :=
                 Find_Item
                   (Inventory => Player_Ship.Cargo,
                    Item_Type =>
                      Modules_List
                        (Player_Ship.Modules(ModuleIndex).Proto_Index)
                        .RepairMaterial);
               if RepairMaterial > 0
                 and then Player_Ship.Cargo(RepairMaterial).Amount <
                   RepairPoints then
                  RepairPoints := Player_Ship.Cargo(RepairMaterial).Amount;
               end if;
               if RepairMaterial = 0 then
                  Add_Message
                    ("You don't have the proper repair materials to continue repairs of " &
                     To_String(Player_Ship.Modules(ModuleIndex).Name) & ".",
                     ORDERMESSAGE, RED);
                  RepairStopped := True;
                  return;
               end if;
               -- Repair module
               if Player_Ship.Modules(ModuleIndex).Durability + RepairPoints >=
                 Player_Ship.Modules(ModuleIndex).Max_Durability then
                  RepairValue :=
                    Player_Ship.Modules(ModuleIndex).Max_Durability -
                    Player_Ship.Modules(ModuleIndex).Durability;
                  RepairNeeded := False;
               else
                  RepairValue := RepairPoints;
               end if;
               if RepairValue = Player_Ship.Cargo(RepairMaterial).Amount and
                 ToolsIndex > RepairMaterial then
                  ToolsIndex := ToolsIndex - 1;
               end if;
               UpdateCargo
                 (Ship => Player_Ship, CargoIndex => RepairMaterial,
                  Amount => (0 - RepairValue));
               Player_Ship.Modules(ModuleIndex).Durability :=
                 Player_Ship.Modules(ModuleIndex).Durability + RepairValue;
               if RepairValue > CrewRepairPoints(PointsIndex) then
                  RepairValue := CrewRepairPoints(PointsIndex);
                  RepairPoints := 0;
               else
                  RepairPoints := CrewRepairPoints(PointsIndex) - RepairValue;
               end if;
               Gain_Exp
                 (RepairValue,
                  Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index)
                    .RepairSkill,
                  Crew_Container.To_Index(J));
               CrewRepairPoints(PointsIndex) := RepairPoints;
               Damage_Item
                 (Player_Ship.Crew(J).Inventory, ToolsIndex,
                  GetSkillLevel
                    (Player_Ship.Crew(J),
                     Modules_List(Player_Ship.Modules(ModuleIndex).Proto_Index)
                       .RepairSkill),
                  Crew_Container.To_Index(J));
               exit Repair_Module_Loop when not RepairNeeded;
            end if;
            <<End_Of_Loop>>
         end loop Repair_Module_Loop;
      end RepairModule;
   begin
      Count_Repair_Workers_Loop :
      for Member of Player_Ship.Crew loop
         if Member.Order = REPAIR then
            CurrentMinutes := Minutes;
            OrderTime := Member.Order_Time;
            RepairPoints := 0;
            Count_Repair_Points_Loop :
            while CurrentMinutes > 0 loop
               if CurrentMinutes >= OrderTime then
                  CurrentMinutes := CurrentMinutes - OrderTime;
                  RepairPoints := RepairPoints + 1;
                  OrderTime := 15;
               else
                  OrderTime := OrderTime - CurrentMinutes;
                  CurrentMinutes := 0;
               end if;
            end loop Count_Repair_Points_Loop;
            CrewRepairPoints.Append(New_Item => RepairPoints);
            Member.Order_Time := OrderTime;
         end if;
      end loop Count_Repair_Workers_Loop;
      if CrewRepairPoints.Length = 0 then
         return;
      end if;
      if Player_Ship.Repair_Module > 0
        and then Player_Ship.Modules(Player_Ship.Repair_Module).Durability <
          Player_Ship.Modules(Player_Ship.Repair_Module).Max_Durability then
         RepairModule(Player_Ship.Repair_Module);
      end if;
      Repair_Loop :
      for I in Player_Ship.Modules.Iterate loop
         if Player_Ship.Modules(I).Durability <
           Player_Ship.Modules(I).Max_Durability then
            RepairModule(Modules_Container.To_Index(I));
         end if;
      end loop Repair_Loop;
      -- Send repair team on break if all is ok
      if not RepairNeeded or RepairStopped then
         if not RepairNeeded then
            Add_Message("All repairs have been finished.", ORDERMESSAGE, GREEN);
         end if;
         Give_Orders_Loop :
         for I in Player_Ship.Crew.Iterate loop
            if Player_Ship.Crew(I).Order = REPAIR then
               GiveOrders(Player_Ship, Crew_Container.To_Index(I), REST);
            end if;
         end loop Give_Orders_Loop;
      end if;
   end RepairShip;

end Ships.Repairs;
