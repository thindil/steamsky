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
with Bases.Ship; use Bases.Ship;
with Utils.UI; use Utils.UI;

package body Bases.UI.Repair is

   procedure ShowRepairInfo is
      Cost, Time: Natural := 0;
      InfoWindow, ClearWindow: Window;
      CostInfo, TimeInfo: Unbounded_String;
      WindowWidth: Column_Position;
      ModuleIndex: constant Integer :=
        Integer'Value(Description(Current(TradeMenu)));
   begin
      ClearWindow := Create(4, (Columns / 2), 3, (Columns / 2));
      Refresh_Without_Update(ClearWindow);
      Delete(ClearWindow);
      RepairCost(Cost, Time, ModuleIndex);
      CostInfo :=
        To_Unbounded_String
          ("Repair cost:" & Natural'Image(Cost) & " " & To_String(MoneyName));
      TimeInfo :=
        To_Unbounded_String("Repair time:" & Natural'Image(Time) & " minutes");
      if Length(CostInfo) > Length(TimeInfo) then
         WindowWidth := Column_Position(Length(CostInfo)) + 4;
      else
         WindowWidth := Column_Position(Length(TimeInfo)) + 4;
      end if;
      if WindowWidth > Columns / 2 then
         WindowWidth := Columns / 2;
      end if;
      InfoWindow := Create(4, WindowWidth, 3, (Columns / 2));
      WindowFrame(InfoWindow, 2, "Info");
      Move_Cursor(Win => InfoWindow, Line => 1, Column => 2);
      Add(Win => InfoWindow, Str => To_String(CostInfo));
      Move_Cursor(Win => InfoWindow, Line => 2, Column => 2);
      Add(Win => InfoWindow, Str => To_String(TimeInfo));
      Refresh_Without_Update;
      Refresh_Without_Update(InfoWindow);
      Delete(InfoWindow);
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
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
         Repair_Items.all(MenuIndex) := New_Item("Repair whole ship", "-1");
      end if;
      if SkyBases(BaseIndex).Population > 299 then
         MenuIndex := MenuIndex + 1;
         Repair_Items.all(MenuIndex) :=
           New_Item("Fast repair whole ship", "-2");
      end if;
      MenuIndex := MenuIndex + 1;
      for I in MenuIndex .. Repair_Items'Last loop
         Repair_Items.all(I) := Null_Item;
      end loop;
      Repair_Items.all(MenuIndex + 1) := Null_Item;
      TradeMenu := New_Menu(Repair_Items);
      Set_Options(TradeMenu, (Show_Descriptions => False, others => True));
      Set_Format(TradeMenu, Lines - 10, 1);
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
      Move_Cursor(Line => 7, Column => (Columns / 2));
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
      Move_Cursor(Line => 8, Column => (Columns / 2));
      Add(Str => "Press Enter to start repairing");
      Change_Attributes
        (Line => 8,
         Column => (Columns / 2) + 6,
         Count => 5,
         Color => 1);
      ShowRepairInfo;
   end ShowRepair;

   function RepairKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
      Message: Unbounded_String;
      ModuleIndex: Integer;
   begin
      if TradeMenu /= Null_Menu then
         ModuleIndex := Integer'Value(Description(Current(TradeMenu)));
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
            when 50 | KEY_DOWN => -- Select next repair option
               Result := Driver(TradeMenu, M_Down_Item);
               if Result = Request_Denied then
                  Result := Driver(TradeMenu, M_First_Item);
               end if;
            when 10 => -- Repair ship
               Message :=
                 To_Unbounded_String(Bases.Ship.RepairShip(ModuleIndex));
               if Length(Message) > 0 then
                  ShowDialog(To_String(Message));
               end if;
               DrawGame(Repairs_View);
               return Repairs_View;
            when others =>
               Result := Driver(TradeMenu, Key);
               if Result /= Menu_Ok then
                  Result := Driver(TradeMenu, M_Clear_Pattern);
                  Result := Driver(TradeMenu, Key);
               end if;
         end case;
         if Result = Menu_Ok then
            ShowRepairInfo;
         end if;
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
