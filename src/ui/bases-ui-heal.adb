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
with Items; use Items;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;

package body Bases.UI.Heal is

   procedure HealCost(Cost, Time, MemberIndex: in out Natural) is
      BaseType: constant Positive :=
        Bases_Types'
          Pos
            (SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
               .BaseType) +
        1;
   begin
      MemberIndex := Integer'Value(Description(Current(TradeMenu)));
      if MemberIndex > 0 then
         Time := 5 * (100 - PlayerShip.Crew(MemberIndex).Health);
      else
         for Member of PlayerShip.Crew loop
            if Member.Health < 100 then
               Time := Time + (5 * (100 - Member.Health));
            end if;
         end loop;
      end if;
      Cost :=
        Time *
        Items_List(FindProtoItem(ItemType => HealingTools)).Prices(BaseType);
      if Time = 0 then
         Time := 1;
      end if;
   end HealCost;

   procedure ShowHealInfo is
      Cost, Time, ModuleIndex: Natural := 0;
      InfoWindow: Window;
   begin
      HealCost(Cost, Time, ModuleIndex);
      InfoWindow := Create(5, (Columns / 2), 3, (Columns / 2));
      Add
        (Win => InfoWindow,
         Str =>
           "Heal cost:" & Natural'Image(Cost) & " " & To_String(MoneyName));
      Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
      Add
        (Win => InfoWindow,
         Str => "Heal time:" & Natural'Image(Time) & " minutes");
      Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
      Add(Win => InfoWindow, Str => "Press Enter to start healing");
      Change_Attributes
        (Win => InfoWindow,
         Line => 3,
         Column => 6,
         Count => 5,
         Color => 1);
      Refresh;
      Refresh(InfoWindow);
      Delete(InfoWindow);
   end ShowHealInfo;

   procedure ShowHeal is
      Heal_Items: constant Item_Array_Access :=
        new Item_Array
        (PlayerShip.Crew.First_Index .. (PlayerShip.Crew.Last_Index + 2));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Integer := 1;
      MoneyIndex2: Natural := 0;
   begin
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Health < 100 then
            Heal_Items.all(MenuIndex) :=
              New_Item
                (To_String(PlayerShip.Crew(I).Name),
                 Positive'Image(Crew_Container.To_Index(I)));
            MenuIndex := MenuIndex + 1;
         end if;
      end loop;
      if MenuIndex = 1 then
         if TradeMenu /= Null_Menu then
            Post(TradeMenu, False);
            Delete(TradeMenu);
         end if;
         Move_Cursor(Line => (Lines / 3), Column => (Columns / 3));
         Add(Str => "You have no one to heal.");
         Refresh;
         return;
      end if;
      Heal_Items.all(MenuIndex) :=
        New_Item("Heal all wounded crew members", "0");
      MenuIndex := MenuIndex + 1;
      for I in MenuIndex .. Heal_Items'Last loop
         Heal_Items.all(I) := Null_Item;
      end loop;
      Heal_Items.all(MenuIndex + 1) := Null_Item;
      TradeMenu := New_Menu(Heal_Items);
      Set_Options(TradeMenu, (Show_Descriptions => False, others => True));
      Set_Format(TradeMenu, Lines - 10, 1);
      Set_Mark(TradeMenu, "");
      Scale(TradeMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
      Set_Window(TradeMenu, MenuWindow);
      Set_Sub_Window
        (TradeMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(TradeMenu);
      if Heal_Items.all(CurrentMenuIndex) = Null_Item then
         CurrentMenuIndex := 1;
      end if;
      Set_Current(TradeMenu, Heal_Items.all(CurrentMenuIndex));
      MoneyIndex2 := FindCargo(FindProtoItem(MoneyIndex));
      Move_Cursor(Line => (MenuHeight + 4), Column => 2);
      if MoneyIndex2 > 0 then
         Add
           (Str =>
              "You have" &
              Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) &
              " " &
              To_String(MoneyName) &
              ".");
      else
         Add
           (Str =>
              "You don't have any " &
              To_String(MoneyName) &
              " to heal anyone.");
      end if;
      ShowHealInfo;
      Refresh(MenuWindow);
   end ShowHeal;

   function HealKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
   begin
      if TradeMenu /= Null_Menu then
         case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
               CurrentMenuIndex := 1;
               if TradeMenu /= Null_Menu then
                  Post(TradeMenu, False);
                  Delete(TradeMenu);
               end if;
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            when 56 | KEY_UP => -- Select previous wounded crew member
               Result := Driver(TradeMenu, M_Up_Item);
               if Result = Request_Denied then
                  Result := Driver(TradeMenu, M_Last_Item);
               end if;
               if Result = Menu_Ok then
                  ShowHealInfo;
                  Refresh(MenuWindow);
               end if;
            when 50 | KEY_DOWN => -- Select next wounded crew member
               Result := Driver(TradeMenu, M_Down_Item);
               if Result = Request_Denied then
                  Result := Driver(TradeMenu, M_First_Item);
               end if;
               if Result = Menu_Ok then
                  ShowHealInfo;
                  Refresh(MenuWindow);
               end if;
            when 10 => -- Heal wounded crew member(s)
               HealWounded;
               DrawGame(Heal_View);
               return Heal_View;
            when others =>
               Result := Driver(TradeMenu, Key);
               if Result = Menu_Ok then
                  ShowHealInfo;
                  Refresh(MenuWindow);
               else
                  Result := Driver(TradeMenu, M_Clear_Pattern);
                  Result := Driver(TradeMenu, Key);
                  if Result = Menu_Ok then
                     ShowHealInfo;
                     Refresh(MenuWindow);
                  end if;
               end if;
         end case;
         CurrentMenuIndex := Menus.Get_Index(Current(TradeMenu));
      else
         case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            when others =>
               null;
         end case;
      end if;
      return Heal_View;
   end HealKeys;

end Bases.UI.Heal;
