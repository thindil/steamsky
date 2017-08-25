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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Messages; use Messages;
with Items; use Items;
with Config; use Config;
with ShipModules; use ShipModules;
with Crew; use Crew;
with Maps; use Maps;
with Utils.UI; use Utils.UI;

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
      GunnersCheck, CraftersCheck, ItemIndex, ItemAmount: Natural := 0;
      CurrentColumn: Column_Position;
      CurrentLine: Line_Position;
   begin
      case CurrentState is
         when Sky_Map_View | Control_Speed | Wait_Order =>
            Add(Str => "[Menu]");
            Change_Attributes(Line => 0, Column => 2, Count => 1, Color => 1);
         when Ship_Info =>
            Add(Str => "Ship Informations [Quit] [F1 Help]");
            Change_Attributes(Line => 0, Column => 19, Count => 1, Color => 1);
            Change_Attributes(Line => 0, Column => 26, Count => 2, Color => 1);
         when Crew_Info | Giving_Orders | Dismiss_Confirm =>
            Add(Str => "Crew Informations");
         when Messages_View =>
            Add(Str => "Last Messages [Escape closes]");
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
               Add(Str => "Trade with base [Quit] [F1 Help]");
            else
               Add(Str => "Trade with ship [Quit] [F1 Help]");
            end if;
            Change_Attributes(Line => 0, Column => 17, Count => 1, Color => 1);
            Change_Attributes(Line => 0, Column => 24, Count => 2, Color => 1);
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
            Add(Str => "Game statistics [Quit] [F1 Help]");
            Change_Attributes(Line => 0, Column => 17, Count => 1, Color => 1);
            Change_Attributes(Line => 0, Column => 24, Count => 2, Color => 1);
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
         when Death_Confirm =>
            return;
         when others =>
            null;
      end case;
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
      Move_Cursor(Line => 0, Column => (Columns / 3));
      Add(Str => FormatedTime & " Speed: " & To_String(Speed));
      Get_Cursor_Position(Line => CurrentLine, Column => CurrentColumn);
      CurrentColumn := CurrentColumn + 1;
      ItemIndex := FindCargo(ItemType => FuelType);
      if ItemIndex = 0 then
         Move_Cursor(Line => 0, Column => CurrentColumn);
         Add(Str => "[No Fuel]");
         Change_Attributes
           (Line => 0,
            Column => CurrentColumn,
            Count => 10,
            Color => 3);
         CurrentColumn := CurrentColumn + 9;
      else
         for Item of PlayerShip.Cargo loop
            if Items_List(Item.ProtoIndex).IType = FuelType then
               ItemAmount := ItemAmount + Item.Amount;
            end if;
            exit when ItemAmount > 99;
         end loop;
         if ItemAmount < GameSettings.LowFuel then
            Move_Cursor(Line => 0, Column => CurrentColumn);
            Add(Str => "[Low Fuel]");
            Change_Attributes
              (Line => 0,
               Column => CurrentColumn,
               Count => 11,
               Color => 1);
            CurrentColumn := CurrentColumn + 10;
         end if;
      end if;
      ItemIndex := FindCargo(ItemType => DrinksType);
      if ItemIndex = 0 then
         Move_Cursor(Line => 0, Column => CurrentColumn);
         Add(Str => "[No Drinks]");
         Change_Attributes
           (Line => 0,
            Column => CurrentColumn,
            Count => 11,
            Color => 3);
         CurrentColumn := CurrentColumn + 11;
      else
         ItemAmount := 0;
         for Item of PlayerShip.Cargo loop
            if Items_List(Item.ProtoIndex).IType = DrinksType then
               ItemAmount := ItemAmount + Item.Amount;
            end if;
            exit when ItemAmount > 49;
         end loop;
         if ItemAmount < GameSettings.LowDrinks then
            Move_Cursor(Line => 0, Column => CurrentColumn);
            Add(Str => "[Low Drinks]");
            Change_Attributes
              (Line => 0,
               Column => CurrentColumn,
               Count => 12,
               Color => 1);
            CurrentColumn := CurrentColumn + 12;
         end if;
      end if;
      ItemIndex := FindCargo(ItemType => FoodTypes(1));
      if ItemIndex = 0 then
         ItemIndex := FindCargo(ItemType => FoodTypes(2));
      end if;
      if ItemIndex = 0 then
         Move_Cursor(Line => 0, Column => CurrentColumn);
         Add(Str => "[No Food]");
         Change_Attributes
           (Line => 0,
            Column => CurrentColumn,
            Count => 10,
            Color => 3);
      else
         ItemAmount := 0;
         for Item of PlayerShip.Cargo loop
            if Items_List(Item.ProtoIndex).IType = FoodTypes(1) or
              Items_List(Item.ProtoIndex).IType = FoodTypes(2) then
               ItemAmount := ItemAmount + Item.Amount;
            end if;
            exit when ItemAmount > 49;
         end loop;
         if ItemAmount < GameSettings.LowFood then
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
            when GUN =>
               GunnersCheck := GunnersCheck + 1;
            when ALCHEMY_LAB .. GREENHOUSE =>
               if Module.Current_Value /= 0 then
                  if Module.Owner > 0 and CraftersCheck < 2 then
                     CraftersCheck := 1;
                  else
                     CraftersCheck := 2;
                  end if;
               end if;
            when CABIN =>
               if Module.Current_Value < Module.Max_Value then
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
      if not HaveTrader and
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
         Change_Attributes
           (Line => 0,
            Column => (Columns - 6),
            Count => 1,
            Color => 3);
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
