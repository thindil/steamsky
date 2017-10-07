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

with Items; use Items;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Utils.UI; use Utils.UI;
with Trades; use Trades;

package body Bases.UI.School is

   procedure ShowSchoolInfo is
      InfoWindow, ClearWindow: Window;
      WindowWidth: Column_Position := (Columns / 2);
      MemberIndex: constant Natural :=
        Natural'Value(Description(Current(TradeMenu)));
      WindowHeight: constant Line_Position :=
        Line_Position(Skills_List.Length) + 1;
      Cost: Positive;
      CurrentLine: Line_Position := 1;
      NewWindowWidth: Column_Position;
      MoneyIndex2: Natural := 0;
   begin
      ClearWindow := Create((Lines - 4), (Columns / 2), 3, (Columns / 2));
      Refresh_Without_Update(ClearWindow);
      Delete(ClearWindow);
      InfoWindow := Create(WindowHeight, WindowWidth, 3, (Columns / 2));
      WindowWidth := 1;
      for I in Skills_List.Iterate loop
         Cost := 100;
         for Skill of PlayerShip.Crew(MemberIndex).Skills loop
            if Skill(1) = SkillsData_Container.To_Index(I) then
               Cost := (Skill(2) + 1) * 100;
               exit;
            end if;
         end loop;
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 1);
         Add
           (Win => InfoWindow,
            Str =>
              To_String(Skills_List(I).Name) &
              ":" &
              Positive'Image(Cost) &
              " " &
              To_String(MoneyName));
         Get_Cursor_Position
           (Win => InfoWindow,
            Line => CurrentLine,
            Column => NewWindowWidth);
         CurrentLine := CurrentLine + 1;
         if NewWindowWidth + 2 > WindowWidth then
            WindowWidth := NewWindowWidth + 2;
         end if;
      end loop;
      if WindowWidth > Columns / 2 then
         WindowWidth := Columns / 2;
      end if;
      Resize(InfoWindow, WindowHeight + 1, WindowWidth);
      WindowFrame(InfoWindow, 2, "Train cost");
      MoneyIndex2 := FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
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
              " to pay for learning.");
      end if;
      Move_Cursor(Line => WindowHeight + 4, Column => (Columns / 2));
      Add(Str => "Press Enter to select skill to train");
      Change_Attributes
        (Line => WindowHeight + 4,
         Column => (Columns / 2) + 6,
         Count => 5,
         Attr => BoldCharacters,
         Color => 1);
      Move_Cursor(Line => WindowHeight + 5, Column => (Columns / 2));
      Add(Str => "Press Escape to back to sky map");
      Change_Attributes
        (Line => WindowHeight + 5,
         Column => (Columns / 2) + 6,
         Count => 6,
         Attr => BoldCharacters,
         Color => 1);
      Refresh_Without_Update;
      Refresh_Without_Update(InfoWindow);
      Delete(InfoWindow);
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
   end ShowSchoolInfo;

   procedure ShowSchool is
      School_Items: constant Item_Array_Access :=
        new Item_Array
        (PlayerShip.Crew.First_Index .. (PlayerShip.Crew.Last_Index + 1));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Integer := 1;
   begin
      for I in PlayerShip.Crew.Iterate loop
         School_Items.all(MenuIndex) :=
           New_Item
             (To_String(PlayerShip.Crew(I).Name),
              Positive'Image(Crew_Container.To_Index(I)));
         MenuIndex := MenuIndex + 1;
      end loop;
      School_Items.all(MenuIndex) := Null_Item;
      TradeMenu := New_Menu(School_Items);
      Set_Options(TradeMenu, (Show_Descriptions => False, others => True));
      Set_Format(TradeMenu, Lines - 10, 1);
      Scale(TradeMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 3, 2);
      Set_Window(TradeMenu, MenuWindow);
      Set_Sub_Window
        (TradeMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(TradeMenu);
      if School_Items.all(CurrentMenuIndex) = Null_Item then
         CurrentMenuIndex := 1;
      end if;
      Set_Current(TradeMenu, School_Items.all(CurrentMenuIndex));
      ShowSchoolInfo;
   end ShowSchool;

   function SchoolKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
   begin
      case Key is
         when 27 => -- Back to sky map
            CurrentMenuIndex := 1;
            if TradeMenu /= Null_Menu then
               Post(TradeMenu, False);
               Delete(TradeMenu);
            end if;
            DrawGame(Sky_Map_View);
            return Sky_Map_View;
         when 56 | KEY_UP => -- Select previous crew member
            Result := Driver(TradeMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next crew member
            Result := Driver(TradeMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(TradeMenu, M_First_Item);
            end if;
         when 10 => -- Start setting what to learn
            DrawGame(School_View);
            return School_View;
         when others =>
            Result := Driver(TradeMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(TradeMenu, M_Clear_Pattern);
               Result := Driver(TradeMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         ShowSchoolInfo;
      end if;
      CurrentMenuIndex := Menus.Get_Index(Current(TradeMenu));
      return School_View;
   exception
      when Trade_No_Money =>
         ShowDialog
           ("You don't have any " &
            To_String(MoneyName) &
            " to pay for learning.");
         DrawGame(School_View);
         return School_View;
      when Trade_Not_Enough_Money =>
         ShowDialog
           ("You don't have enough " &
            To_String(MoneyName) &
            " to pay for learning this skill.");
         DrawGame(School_View);
         return School_View;
   end SchoolKeys;

end Bases.UI.School;
