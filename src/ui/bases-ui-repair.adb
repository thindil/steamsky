--    Copyright 2016-2017 Bartek thindil Jasicki
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
with ShipModules; use ShipModules;

package body Bases.UI.Repair is

   procedure RepairCost(Cost, Time, ModuleIndex: in out Natural) is
      BaseType: constant Positive :=
        Bases_Types'
          Pos
            (SkyBases(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex)
               .BaseType) +
        1;
      ProtoIndex: Positive;
   begin
      ModuleIndex := Integer'Value(Description(Current(TradeMenu)));
      if ModuleIndex > 0 then
         Time :=
           PlayerShip.Modules(ModuleIndex).MaxDurability -
           PlayerShip.Modules(ModuleIndex).Durability;
         ProtoIndex :=
           FindProtoItem
             (ItemType =>
                Modules_List(PlayerShip.Modules(ModuleIndex).ProtoIndex)
                  .RepairMaterial);
         Cost := Time * Items_List(ProtoIndex).Prices(BaseType);
      else
         for Module of PlayerShip.Modules loop
            if Module.Durability < Module.MaxDurability then
               Time := Time + Module.MaxDurability - Module.Durability;
               ProtoIndex :=
                 FindProtoItem
                   (ItemType =>
                      Modules_List(Module.ProtoIndex).RepairMaterial);
               Cost :=
                 Cost +
                 ((Module.MaxDurability - Module.Durability) *
                  Items_List(ProtoIndex).Prices(BaseType));
            end if;
         end loop;
         if Name(Current(TradeMenu))(1) = 'R' then
            Cost := Cost * 2;
            Time := Time / 2;
         elsif Name(Current(TradeMenu))(1) = 'F' then
            Cost := Cost * 4;
            Time := Time / 4;
         end if;
      end if;
      if Bases_Types'Val(BaseType - 1) = Shipyard then
         Cost := Cost / 2;
      end if;
      if Time = 0 then
         Time := 1;
      end if;
   end RepairCost;

   procedure ShowRepairInfo is
      Cost, Time, ModuleIndex: Natural := 0;
      InfoWindow: Window;
   begin
      RepairCost(Cost, Time, ModuleIndex);
      InfoWindow := Create(5, (Columns / 2), 3, (Columns / 2));
      Add
        (Win => InfoWindow,
         Str =>
           "Repair cost:" & Natural'Image(Cost) & " " & To_String(MoneyName));
      Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
      Add
        (Win => InfoWindow,
         Str => "Repair time:" & Natural'Image(Time) & " minutes");
      Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
      Add(Win => InfoWindow, Str => "Press Enter to start repairing");
      Change_Attributes
        (Win => InfoWindow,
         Line => 3,
         Column => 6,
         Count => 5,
         Color => 1);
      Refresh;
      Refresh(InfoWindow);
      Delete(InfoWindow);
   end ShowRepairInfo;

   procedure ShowRepair is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Repair_Items: constant Item_Array_Access :=
        new Item_Array
        (PlayerShip.Modules.First_Index ..
             (PlayerShip.Modules.Last_Index + 4));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Integer := 1;
      MoneyIndex2: Natural := 0;
   begin
      for I in PlayerShip.Modules.Iterate loop
         if PlayerShip.Modules(I).Durability <
           PlayerShip.Modules(I).MaxDurability then
            Repair_Items.all(MenuIndex) :=
              New_Item
                (To_String(PlayerShip.Modules(I).Name),
                 Positive'Image(Modules_Container.To_Index(I)));
            MenuIndex := MenuIndex + 1;
         end if;
      end loop;
      if MenuIndex = 1 then
         if TradeMenu /= Null_Menu then
            Post(TradeMenu, False);
            Delete(TradeMenu);
         end if;
         Move_Cursor(Line => (Lines / 3), Column => (Columns / 3));
         Add(Str => "You have nothing to repair.");
         Refresh;
         return;
      end if;
      Repair_Items.all(MenuIndex) := New_Item("Slowly repair whole ship", "0");
      if SkyBases(BaseIndex).Population > 149 then
         MenuIndex := MenuIndex + 1;
         Repair_Items.all(MenuIndex) := New_Item("Repair whole ship", "0");
      end if;
      if SkyBases(BaseIndex).Population > 299 then
         MenuIndex := MenuIndex + 1;
         Repair_Items.all(MenuIndex) :=
           New_Item("Fast repair whole ship", "0");
      end if;
      MenuIndex := MenuIndex + 1;
      for I in MenuIndex .. Repair_Items'Last loop
         Repair_Items.all(I) := Null_Item;
      end loop;
      Repair_Items.all(MenuIndex + 1) := Null_Item;
      TradeMenu := New_Menu(Repair_Items);
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
      if Repair_Items.all(CurrentMenuIndex) = Null_Item then
         CurrentMenuIndex := 1;
      end if;
      Set_Current(TradeMenu, Repair_Items.all(CurrentMenuIndex));
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
              " to repair anything.");
      end if;
      ShowRepairInfo;
      Refresh(MenuWindow);
   end ShowRepair;

   function RepairKeys(Key: Key_Code) return GameStates is
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
            when 56 | KEY_UP => -- Select previous repair option
               Result := Driver(TradeMenu, M_Up_Item);
               if Result = Request_Denied then
                  Result := Driver(TradeMenu, M_Last_Item);
               end if;
               if Result = Menu_Ok then
                  ShowRepairInfo;
                  Refresh(MenuWindow);
               end if;
            when 50 | KEY_DOWN => -- Select next repair option
               Result := Driver(TradeMenu, M_Down_Item);
               if Result = Request_Denied then
                  Result := Driver(TradeMenu, M_First_Item);
               end if;
               if Result = Menu_Ok then
                  ShowRepairInfo;
                  Refresh(MenuWindow);
               end if;
            when 10 => -- Repair ship
               RepairShip;
               DrawGame(Repairs_View);
               return Repairs_View;
            when others =>
               Result := Driver(TradeMenu, Key);
               if Result = Menu_Ok then
                  ShowRepairInfo;
                  Refresh(MenuWindow);
               else
                  Result := Driver(TradeMenu, M_Clear_Pattern);
                  Result := Driver(TradeMenu, Key);
                  if Result = Menu_Ok then
                     ShowRepairInfo;
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
      return Repairs_View;
   end RepairKeys;

end Bases.UI.Repair;
