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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ships; use Ships;
with Messages; use Messages;
with Items; use Items;
with Config; use Config;
with ShipModules; use ShipModules;
with Crew; use Crew;
with Maps; use Maps;
with Utils.UI; use Utils.UI;
with Events; use Events;

package body Header is

   procedure ShowGameHeader(CurrentState: GameStates) is
      Speed: Unbounded_String;
      HavePilot,
      HaveEngineer,
      HaveRepair,
      HaveUpgrade,
      HaveTrader,
      HaveCleaner,
      NeedClean: Boolean :=
        False;
      GunnersCheck,
      CraftersCheck,
      ItemIndex,
      FuelAmount,
      FoodAmount,
      DrinksAmount: Natural :=
        0;
      CurrentColumn, EndColumn, StartColumn: Column_Position;
      CurrentLine: Line_Position;
   begin
      case CurrentState is
         when Sky_Map_View | Control_Speed | Wait_Order =>
            Add(Str => "[Menu]");
            Get_Cursor_Position(Line => CurrentLine, Column => CurrentColumn);
            Change_Attributes
              (Line => 0,
               Column => 2,
               Count => 1,
               Color => 1,
               Attr => BoldCharacters);
         when Ship_Info =>
            Add(Str => "Ship Informations");
         when Crew_Info | Giving_Orders | Dismiss_Confirm =>
            Add(Str => "Crew Informations");
         when Messages_View =>
            Add(Str => "Last Messages [Escape closes]");
            Get_Cursor_Position(Line => CurrentLine, Column => CurrentColumn);
            Change_Attributes
              (Line => 0,
               Column => 15,
               Count => 6,
               Color => 1,
               Attr => BoldCharacters);
         when Clear_Confirm =>
            Add(Str => "Last Messages");
         when Trade_View =>
            if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
               Add(Str => "Trade with base");
            else
               Add(Str => "Trade with ship");
            end if;
         when Help_View =>
            Add(Str => "Help Index [Escape closes]");
            Change_Attributes
              (Line => 0,
               Column => 12,
               Count => 6,
               Color => 1,
               Attr => BoldCharacters);
            return;
         when Craft_View =>
            Add(Str => "Manufacturing");
         when Cargo_Info =>
            Add(Str => "Ship Cargo");
         when Help_Topic =>
            Add(Str => "Help [Menu] [Escape closes]");
            Change_Attributes
              (Line => 0,
               Column => 6,
               Count => 1,
               Color => 1,
               Attr => BoldCharacters);
            Change_Attributes
              (Line => 0,
               Column => 13,
               Count => 6,
               Color => 1,
               Attr => BoldCharacters);
            return;
         when Repairs_View =>
            Add(Str => "Ship repairs");
         when Shipyard_View =>
            Add(Str => "Shipyard");
         when Recruits_View =>
            Add(Str => "Recruit new crew members");
         when Bases_List =>
            Add(Str => "List of know bases");
         when Events_View =>
            Add(Str => "List of know events");
         when GameStats_View =>
            Add(Str => "Game statistics [Escape closes] [F1 Help]");
            Change_Attributes
              (Line => 0,
               Column => 17,
               Count => 6,
               Color => 1,
               Attr => BoldCharacters);
            Change_Attributes
              (Line => 0,
               Column => 33,
               Count => 2,
               Color => 1,
               Attr => BoldCharacters);
            return;
         when TradeRecipes_View =>
            Add(Str => "Buy crafting recipes");
         when BaseMissions_View =>
            Add(Str => "Available missions");
         when Missions_View =>
            Add(Str => "Accepted missions");
         when GameOptions_View =>
            Add(Str => "Game options [Escape closes]");
            Change_Attributes
              (Line => 0,
               Column => 14,
               Count => 6,
               Color => 1,
               Attr => BoldCharacters);
            return;
         when Heal_View =>
            Add(Str => "Heal wounded crew members");
         when Loot_View =>
            Add(Str => "Loot base");
         when Inventory_View =>
            Add(Str => "Inventory");
         when Death_Confirm =>
            return;
         when School_View =>
            Add(Str => "Train skills");
         when others =>
            null;
      end case;
      if CurrentState /= Sky_Map_View and
        CurrentState /= Control_Speed and
        CurrentState /= Wait_Order and
        CurrentState /= Messages_View then
         Get_Cursor_Position(Line => CurrentLine, Column => CurrentColumn);
      end if;
      case PlayerShip.Speed is
         when DOCKED =>
            Speed := To_Unbounded_String("Docked");
         when FULL_STOP =>
            Speed := To_Unbounded_String("Stopped");
         when QUARTER_SPEED =>
            Speed := To_Unbounded_String("Quarter Speed");
         when HALF_SPEED =>
            Speed := To_Unbounded_String("Half Speed");
         when FULL_SPEED =>
            Speed := To_Unbounded_String("Full Speed");
      end case;
      EndColumn :=
        (Columns / 3) +
        Column_Position(FormatedTime'Length + 9 + Length(Speed));
      ItemIndex :=
        FindItem(Inventory => PlayerShip.Cargo, ItemType => FuelType);
      if ItemIndex > 0 then
         for Item of PlayerShip.Cargo loop
            if Items_List(Item.ProtoIndex).IType = FuelType then
               FuelAmount := FuelAmount + Item.Amount;
            end if;
            exit when FuelAmount > GameSettings.LowFuel;
         end loop;
         if FuelAmount < GameSettings.LowFuel then
            EndColumn := EndColumn + 10;
         end if;
      else
         EndColumn := EndColumn + 9;
      end if;
      ItemIndex :=
        FindItem(Inventory => PlayerShip.Cargo, ItemType => DrinksType);
      if ItemIndex = 0 then
         EndColumn := EndColumn + 11;
      else
         for Item of PlayerShip.Cargo loop
            if Items_List(Item.ProtoIndex).IType = DrinksType then
               DrinksAmount := DrinksAmount + Item.Amount;
            end if;
            exit when DrinksAmount > GameSettings.LowDrinks;
         end loop;
         if DrinksAmount < GameSettings.LowDrinks then
            EndColumn := EndColumn + 12;
         end if;
      end if;
      for FoodType of FoodTypes loop
         ItemIndex :=
           FindItem(Inventory => PlayerShip.Cargo, ItemType => FoodType);
         exit when ItemIndex > 0;
      end loop;
      if ItemIndex = 0 then
         EndColumn := EndColumn + 9;
      else
         for Item of PlayerShip.Cargo loop
            if FoodTypes.Find_Index
              (Item => Items_List(Item.ProtoIndex).IType) /=
              UnboundedString_Container.No_Index then
               FoodAmount := FoodAmount + Item.Amount;
            end if;
            exit when FoodAmount > GameSettings.LowFood;
         end loop;
         if FoodAmount < GameSettings.LowFood then
            EndColumn := EndColumn + 10;
         end if;
      end if;
      StartColumn := (Columns / 3);
      if EndColumn > (Columns - 25) then
         StartColumn := StartColumn - (EndColumn - (Columns - 25));
      end if;
      if StartColumn < CurrentColumn + 1 then
         StartColumn := CurrentColumn + 1;
      end if;
      Move_Cursor(Line => 0, Column => StartColumn);
      Add(Str => FormatedTime & " Speed: " & To_String(Speed));
      Get_Cursor_Position(Line => CurrentLine, Column => CurrentColumn);
      CurrentColumn := CurrentColumn + 1;
      if FuelAmount = 0 then
         Move_Cursor(Line => 0, Column => CurrentColumn);
         Add(Str => "[No Fuel]");
         Change_Attributes
           (Line => 0,
            Column => CurrentColumn,
            Count => 10,
            Color => 3);
         CurrentColumn := CurrentColumn + 9;
      elsif FuelAmount < GameSettings.LowFuel then
         Move_Cursor(Line => 0, Column => CurrentColumn);
         Add(Str => "[Low Fuel]");
         Change_Attributes
           (Line => 0,
            Column => CurrentColumn,
            Count => 11,
            Color => 1);
         CurrentColumn := CurrentColumn + 10;
      end if;
      if DrinksAmount = 0 then
         Move_Cursor(Line => 0, Column => CurrentColumn);
         Add(Str => "[No Drinks]");
         Change_Attributes
           (Line => 0,
            Column => CurrentColumn,
            Count => 11,
            Color => 3);
         CurrentColumn := CurrentColumn + 11;
      elsif DrinksAmount < GameSettings.LowDrinks then
         Move_Cursor(Line => 0, Column => CurrentColumn);
         Add(Str => "[Low Drinks]");
         Change_Attributes
           (Line => 0,
            Column => CurrentColumn,
            Count => 12,
            Color => 1);
         CurrentColumn := CurrentColumn + 12;
      end if;
      if CurrentColumn < (Columns - 26) then
         if FoodAmount = 0 then
            Move_Cursor(Line => 0, Column => CurrentColumn);
            Add(Str => "[No Food]");
            Change_Attributes
              (Line => 0,
               Column => CurrentColumn,
               Count => 10,
               Color => 3);
         elsif FoodAmount < GameSettings.LowFood then
            Move_Cursor(Line => 0, Column => CurrentColumn);
            Add(Str => "[Low Food]");
            Change_Attributes
              (Line => 0,
               Column => CurrentColumn,
               Count => 11,
               Color => 1);
         end if;
      end if;
      Move_Cursor(Line => 0, Column => (Columns - 25));
      Add(Str => "[P][E][G][R][M][U][T][C]");
      for Module of PlayerShip.Modules loop
         case Modules_List(Module.ProtoIndex).MType is
            when GUN | HARPOON_GUN =>
               GunnersCheck := GunnersCheck + 1;
            when ALCHEMY_LAB .. GREENHOUSE =>
               if Module.Data(1) /= 0 then
                  if Module.Owner > 0 and CraftersCheck < 2 then
                     CraftersCheck := 1;
                  else
                     CraftersCheck := 2;
                  end if;
               end if;
            when CABIN =>
               if Module.Data(1) < Module.Data(2) then
                  NeedClean := True;
               end if;
            when others =>
               null;
         end case;
      end loop;
      for Member of PlayerShip.Crew loop
         case Member.Order is
            when Pilot =>
               HavePilot := True;
               Change_Attributes
                 (Line => 0,
                  Column => (Columns - 24),
                  Count => 1,
                  Color => 2);
            when Engineer =>
               HaveEngineer := True;
               Change_Attributes
                 (Line => 0,
                  Column => (Columns - 21),
                  Count => 1,
                  Color => 2);
            when Repair =>
               HaveRepair := True;
               Change_Attributes
                 (Line => 0,
                  Column => (Columns - 15),
                  Count => 1,
                  Color => 2);
            when Upgrading =>
               HaveUpgrade := True;
               Change_Attributes
                 (Line => 0,
                  Column => (Columns - 9),
                  Count => 1,
                  Color => 2);
            when Talk =>
               HaveTrader := True;
               Change_Attributes
                 (Line => 0,
                  Column => (Columns - 6),
                  Count => 1,
                  Color => 2);
            when Clean =>
               HaveCleaner := True;
               Change_Attributes
                 (Line => 0,
                  Column => (Columns - 3),
                  Count => 1,
                  Color => 2);
            when Gunner =>
               GunnersCheck := GunnersCheck - 1;
            when others =>
               null;
         end case;
      end loop;
      if not HavePilot then
         Change_Attributes
           (Line => 0,
            Column => (Columns - 24),
            Count => 1,
            Color => 3);
      end if;
      if not HaveEngineer then
         Change_Attributes
           (Line => 0,
            Column => (Columns - 21),
            Count => 1,
            Color => 3);
      end if;
      if GunnersCheck = 0 then
         Change_Attributes
           (Line => 0,
            Column => (Columns - 18),
            Count => 1,
            Color => 2);
      else
         Change_Attributes
           (Line => 0,
            Column => (Columns - 18),
            Count => 1,
            Color => 3);
      end if;
      if not HaveRepair then
         for Module of PlayerShip.Modules loop
            if Module.Durability < Module.MaxDurability then
               Change_Attributes
                 (Line => 0,
                  Column => (Columns - 15),
                  Count => 1,
                  Color => 3);
               exit;
            end if;
         end loop;
      end if;
      if CraftersCheck = 1 then
         Change_Attributes
           (Line => 0,
            Column => (Columns - 12),
            Count => 1,
            Color => 2);
      elsif CraftersCheck = 2 then
         Change_Attributes
           (Line => 0,
            Column => (Columns - 12),
            Count => 1,
            Color => 3);
      end if;
      if not HaveUpgrade and PlayerShip.UpgradeModule > 0 then
         Change_Attributes
           (Line => 0,
            Column => (Columns - 9),
            Count => 1,
            Color => 3);
      end if;
      if not HaveTrader then
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
            Change_Attributes
               (Line => 0,
               Column => (Columns - 6),
               Count => 1,
               Color => 3);
         elsif SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
            if Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                .EType =
              FriendlyShip then
               Change_Attributes
                 (Line => 0,
                  Column => (Columns - 6),
                  Count => 1,
                  Color => 3);
            end if;
         end if;
      end if;
      if not HaveCleaner and NeedClean then
         Change_Attributes
           (Line => 0,
            Column => (Columns - 3),
            Count => 1,
            Color => 3);
      end if;
   end ShowGameHeader;

end Header;
