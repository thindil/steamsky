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

with Ada.Exceptions; use Ada.Exceptions;
with ShipModules; use ShipModules;
with UserInterface; use UserInterface;
with Messages; use Messages;
with Help.UI; use Help.UI;
with Ships.Upgrade; use Ships.Upgrade;
with Ships.Crew; use Ships.Crew;
with Header; use Header;
with Config; use Config;

package body Ships.UI.Ship.Keys is

   function ShipInfoKeys
     (Key: Key_Code;
      OldState: GameStates) return GameStates is
      Result: Menus.Driver_Result;
   begin
      if Key = Key_Code(GameSettings.Keys(33)) then -- Show help
         Erase;
         ShowGameHeader(Help_Topic);
         ShowHelp(Ship_Info, 6);
         return Help_Topic;
      end if;
      case Key is
         when 27 => -- Back to sky map or combat screen
            CurrentMenuIndex := 1;
            DrawGame(OldState);
            return OldState;
         when 56 | KEY_UP => -- Select previous module
            Result := Driver(ShipsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(ShipsMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next module
            Result := Driver(ShipsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(ShipsMenu, M_First_Item);
            end if;
         when 10 => -- Show module options menu
            ShowModuleOptions;
            return Module_Options;
         when Character'Pos('n') | Character'Pos('N') => -- Rename ship
            ShowShipForm("New name for ship:");
            return Rename_Ship;
         when others =>
            Result := Driver(ShipsMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(ShipsMenu, M_Clear_Pattern);
               Result := Driver(ShipsMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         ShowModuleInfo;
      end if;
      CurrentMenuIndex := Get_Index(Current(ShipsMenu));
      return Ship_Info;
   end ShipInfoKeys;

   function ModuleOptionsKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      OptionIndex: constant Positive :=
        Positive'Value(Description(Current(OptionsMenu)));
      ModuleName: constant String :=
        To_String(PlayerShip.Modules(CurrentMenuIndex).Name);
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous item
            Result := Driver(OptionsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next item
            Result := Driver(OptionsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_First_Item);
            end if;
         when 10 => -- Select option from menu
            Post(OptionsMenu, False);
            if OptionIndex /= 5 and OptionIndex < 7 then
               if OptionIndex < 5 then
                  StartUpgrading(CurrentMenuIndex, OptionIndex);
                  UpdateOrders(PlayerShip);
               end if;
               DrawGame(Ship_Info);
               return Ship_Info;
            elsif OptionIndex = 5 then
               DrawGame(Ship_Info);
               ShowShipForm("New name for " & ModuleName & ":");
               return Rename_Module;
            elsif OptionIndex = 7 then
               DrawGame(Ship_Info);
               ShowAssignMenu;
               return Assign_Owner;
            elsif OptionIndex = 8 then
               PlayerShip.UpgradeModule := 0;
               for I in
                 PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
                  if PlayerShip.Crew(I).Order = Upgrading then
                     GiveOrders(PlayerShip, I, Rest);
                     exit;
                  end if;
               end loop;
               AddMessage("You stopped current upgrade.", OrderMessage);
               DrawGame(Ship_Info);
               return Ship_Info;
            elsif OptionIndex = 9 then
               DrawGame(Ship_Info);
               return ShowAssignAmmoMenu;
            elsif OptionIndex = 10 then
               PlayerShip.RepairModule := CurrentMenuIndex;
               AddMessage
                 ("You assigned " & ModuleName & " as repair priority.",
                  OrderMessage);
               DrawGame(Ship_Info);
               return Ship_Info;
            elsif OptionIndex = 11 then
               PlayerShip.RepairModule := 0;
               AddMessage("You removed repair priority.", OrderMessage);
               DrawGame(Ship_Info);
               return Ship_Info;
            end if;
         when 27 => -- Esc select close option, used second time, close menu
            if OptionIndex = 6 then
               DrawGame(Ship_Info);
               return Ship_Info;
            else
               Result := Driver(OptionsMenu, M_Last_Item);
            end if;
         when others =>
            Result := Driver(OptionsMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(OptionsMenu, M_Clear_Pattern);
               Result := Driver(OptionsMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return Module_Options;
   exception
      when An_Exception : Ship_Upgrade_Error =>
         ShowDialog(Exception_Message(An_Exception));
         DrawGame(Ship_Info);
         return Ship_Info;
   end ModuleOptionsKeys;

   function AssignOwnerKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      OptionIndex: constant Natural :=
        Positive'Value(Description(Current(OptionsMenu)));
      ModuleName: constant String :=
        To_String(PlayerShip.Modules(CurrentMenuIndex).Name);
      Message: Unbounded_String;
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous item
            Result := Driver(OptionsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next item
            Result := Driver(OptionsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_First_Item);
            end if;
         when 10 => -- Select new module owner
            Post(OptionsMenu, False);
            if OptionIndex /= 0 then
               case Modules_List
                 (PlayerShip.Modules(CurrentMenuIndex).ProtoIndex)
                 .MType is
                  when CABIN =>
                     for I in PlayerShip.Modules.Iterate loop
                        if PlayerShip.Modules(I).Owner = OptionIndex and
                          Modules_List(PlayerShip.Modules(I).ProtoIndex)
                              .MType =
                            CABIN then
                           PlayerShip.Modules(I).Owner := 0;
                        end if;
                     end loop;
                     PlayerShip.Modules(CurrentMenuIndex).Owner := OptionIndex;
                     AddMessage
                       ("You assigned " &
                        ModuleName &
                        " to " &
                        To_String(PlayerShip.Crew(OptionIndex).Name) &
                        ".",
                        OrderMessage);
                  when GUN =>
                     GiveOrders
                       (PlayerShip,
                        OptionIndex,
                        Gunner,
                        CurrentMenuIndex);
                  when ALCHEMY_LAB .. GREENHOUSE =>
                     GiveOrders
                       (PlayerShip,
                        OptionIndex,
                        Craft,
                        CurrentMenuIndex);
                  when MEDICAL_ROOM =>
                     GiveOrders
                       (PlayerShip,
                        OptionIndex,
                        Heal,
                        CurrentMenuIndex);
                  when others =>
                     null;
               end case;
            end if;
            if Length(Message) > 0 then
               ShowDialog(To_String(Message));
            end if;
            DrawGame(Ship_Info);
            return Ship_Info;
         when 27 => -- Esc select close option, used second time, close menu
            if OptionIndex = 0 then
               DrawGame(Ship_Info);
               return Ship_Info;
            else
               Result := Driver(OptionsMenu, M_Last_Item);
            end if;
         when others =>
            Result := Driver(OptionsMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(OptionsMenu, M_Clear_Pattern);
               Result := Driver(OptionsMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return Assign_Owner;
   exception
      when An_Exception : Crew_Order_Error =>
         ShowDialog(Exception_Message(An_Exception));
         DrawGame(Ship_Info);
         return Ship_Info;
   end AssignOwnerKeys;

   function AssignAmmoKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      OptionIndex: constant Natural :=
        Positive'Value(Description(Current(OptionsMenu)));
      GunName: constant String :=
        To_String(PlayerShip.Modules(CurrentMenuIndex).Name);
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous item
            Result := Driver(OptionsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next item
            Result := Driver(OptionsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_First_Item);
            end if;
         when 10 => -- Select new ammo for gun
            Post(OptionsMenu, False);
            if OptionIndex /= 0 then
               PlayerShip.Modules(CurrentMenuIndex).Data(1) := OptionIndex;
               AddMessage
                 ("You assigned " &
                  To_String
                    (Items_List(PlayerShip.Cargo(OptionIndex).ProtoIndex)
                       .Name) &
                  " to " &
                  GunName &
                  ".",
                  OrderMessage);
            end if;
            DrawGame(Ship_Info);
            return Ship_Info;
         when 27 => -- Esc select close option, used second time, close menu
            if OptionIndex = 0 then
               DrawGame(Ship_Info);
               return Ship_Info;
            else
               Result := Driver(OptionsMenu, M_Last_Item);
            end if;
         when others =>
            Result := Driver(OptionsMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(OptionsMenu, M_Clear_Pattern);
               Result := Driver(OptionsMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return Assign_Ammo;
   end AssignAmmoKeys;

end Ships.UI.Ship.Keys;
