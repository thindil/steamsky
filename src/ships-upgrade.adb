--    Copyright 2017-2023 Bartek thindil Jasicki
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

with Config; use Config;
with Messages; use Messages;
with ShipModules; use ShipModules;

package body Ships.Upgrade is

   procedure Start_Upgrading
     (Module_Index: Modules_Container.Extended_Index;
      Upgrade_Type: Positive) is
      use Tiny_String;

      Local_Max_Value, Upgrade_Progress: Natural := 0;
      Upgrade_Action: Ship_Upgrade := NONE;
   begin
      if Player_Ship.Modules(Module_Index).Durability = 0 and
        Upgrade_Type /= 3 then
         raise Ship_Upgrade_Error
           with "You can't upgrade " &
           To_String(Source => Player_Ship.Modules(Module_Index).Name) &
           " because it's destroyed.";
      end if;
      case Upgrade_Type is
         when 1 => -- Upgrade durability
            Local_Max_Value :=
              Natural
                (Float
                   (Get_Module
                      (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Durability) *
                 1.5);
            if Player_Ship.Modules(Module_Index).Max_Durability =
              Local_Max_Value then
               raise Ship_Upgrade_Error
                 with "You can't further improve the durability of " &
                 To_String(Source => Player_Ship.Modules(Module_Index).Name) &
                 ".";
            end if;
            Upgrade_Action := DURABILITY;
            Upgrade_Progress :=
              Integer
                (Float
                   (Get_Module
                      (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Durability) *
                 New_Game_Settings.Upgrade_Cost_Bonus);
         when 2 => -- Upgrade various max value of selected module
            Local_Max_Value :=
              Natural
                (Float
                   (Get_Module
                      (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                      .Max_Value) *
                 1.5);
            case Get_Module
              (Index => Player_Ship.Modules(Module_Index).Proto_Index)
              .M_Type is
               when ENGINE =>
                  if Player_Ship.Modules(Module_Index).Power =
                    Local_Max_Value then
                     raise Ship_Upgrade_Error
                       with "You can't further improve the power of " &
                       To_String
                         (Source => Player_Ship.Modules(Module_Index).Name) &
                       ".";
                  end if;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Max_Value /
                          20) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when CABIN =>
                  if Player_Ship.Modules(Module_Index).Quality =
                    Local_Max_Value then
                     raise Ship_Upgrade_Error
                       with "You can't further improve the quality of " &
                       To_String
                         (Source => Player_Ship.Modules(Module_Index).Name) &
                       ".";
                  end if;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Max_Value) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when GUN | BATTERING_RAM =>
                  Update_Damage_Block :
                  declare
                     Damage: constant Positive :=
                       (if Player_Ship.Modules(Module_Index).M_Type = GUN then
                          Player_Ship.Modules(Module_Index).Damage
                        else Player_Ship.Modules(Module_Index).Damage2);
                  begin
                     if Damage = Local_Max_Value then
                        raise Ship_Upgrade_Error
                          with "You can't further improve the damage of " &
                          To_String
                            (Source =>
                               Player_Ship.Modules(Module_Index).Name) &
                          ".";
                     end if;
                  end Update_Damage_Block;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Max_Value *
                          2) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when HULL =>
                  if Player_Ship.Modules(Module_Index).Max_Modules =
                    Local_Max_Value then
                     raise Ship_Upgrade_Error
                       with "You can't further enlarge the size of" &
                       To_String
                         (Source => Player_Ship.Modules(Module_Index).Name) &
                       ".";
                  end if;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Max_Value *
                          40) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when HARPOON_GUN =>
                  if Player_Ship.Modules(Module_Index).Duration =
                    Local_Max_Value then
                     raise Ship_Upgrade_Error
                       with "You can't further improve the strength of " &
                       To_String
                         (Source => Player_Ship.Modules(Module_Index).Name) &
                       ".";
                  end if;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Max_Value *
                          10) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when others =>
                  raise Ship_Upgrade_Error
                    with To_String
                      (Source => Player_Ship.Modules(Module_Index).Name) &
                    " can't be upgraded in that way.";
            end case;
            Upgrade_Action := MAX_VALUE;
         when 3 => -- Upgrade various value of selected module
            case Get_Module
              (Index => Player_Ship.Modules(Module_Index).Proto_Index)
              .M_Type is
               when ENGINE =>
                  Local_Max_Value :=
                    Natural
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Value) /
                       2.0);
                  if Local_Max_Value < 1 then
                     Local_Max_Value := 1;
                  end if;
                  if Player_Ship.Modules(Module_Index).Fuel_Usage =
                    Local_Max_Value then
                     raise Ship_Upgrade_Error
                       with "You can't further reduce the fuel usage of " &
                       To_String
                         (Source => Player_Ship.Modules(Module_Index).Name) &
                       ".";
                  end if;
                  Upgrade_Progress :=
                    Integer
                      (Float
                         (Get_Module
                            (Index =>
                               Player_Ship.Modules(Module_Index).Proto_Index)
                            .Value *
                          20) *
                       New_Game_Settings.Upgrade_Cost_Bonus);
               when others =>
                  raise Ship_Upgrade_Error
                    with To_String
                      (Source => Player_Ship.Modules(Module_Index).Name) &
                    " can't be upgraded in that way.";
            end case;
            Upgrade_Action := VALUE;
         when 4 => -- Continue previous upgrade
            if Player_Ship.Modules(Module_Index).Upgrade_Action = NONE then
               raise Ship_Upgrade_Error
                 with To_String
                   (Source => Player_Ship.Modules(Module_Index).Name) &
                 " doesn't have any upgrade set yet.";
            end if;
            Upgrade_Action := Player_Ship.Modules(Module_Index).Upgrade_Action;
         when others =>
            return;
      end case;
      Find_Materials_Block :
      declare
         Material_Index: constant Inventory_Container.Extended_Index :=
           Find_Item
             (Inventory => Player_Ship.Cargo,
              Item_Type =>
                Get_Module
                  (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                  .Repair_Material);
      begin
         if Material_Index = 0 then
            Materials_Loop :
            for I in 1 .. Get_Proto_Amount loop
               if To_String(Source => Get_Proto_Item(Index => I).I_Type) =
                 To_String
                   (Source =>
                      Get_Module
                        (Index =>
                           Player_Ship.Modules(Module_Index).Proto_Index)
                        .Repair_Material) then
                  raise Ship_Upgrade_Error
                    with "You don't have the " &
                    To_String(Source => Get_Proto_Item(Index => I).Name) &
                    " to upgrade " &
                    To_String
                      (Source => Player_Ship.Modules(Module_Index).Name) &
                    ".";
               end if;
            end loop Materials_Loop;
         end if;
      end Find_Materials_Block;
      Player_Ship.Upgrade_Module := Module_Index;
      if Player_Ship.Modules(Module_Index).Upgrade_Action /=
        Upgrade_Action then
         Player_Ship.Modules(Module_Index).Upgrade_Progress :=
           Integer
             (Float(Upgrade_Progress) * New_Game_Settings.Upgrade_Cost_Bonus);
         if Player_Ship.Modules(Module_Index).Upgrade_Progress = 0 then
            Player_Ship.Modules(Module_Index).Upgrade_Progress := 1;
         end if;
         Player_Ship.Modules(Module_Index).Upgrade_Action := Upgrade_Action;
      end if;
      Add_Message
        (Message =>
           "You set the " &
           To_String(Source => Player_Ship.Modules(Module_Index).Name) &
           " to upgrade.",
         M_Type => ORDERMESSAGE);
   end Start_Upgrading;

   procedure Upgrade_Ship(Minutes: Positive) is
      procedure Upgrade_Ada_Ship(M: Integer) with
         Import => True,
         Convention => C,
         External_Name => "upgradeAdaShip";
   begin
      Set_Ship_In_Nim;
      Upgrade_Ada_Ship(M => Minutes);
      Get_Ship_From_Nim(Ship => Player_Ship);
   end Upgrade_Ship;

end Ships.Upgrade;
