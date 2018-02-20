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

with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Items; use Items;
with UserInterface; use UserInterface;
with Ships; use Ships;
with Utils.UI; use Utils.UI;
with Trades; use Trades;
with Bases.Trade; use Bases.Trade;

package body Bases.UI.School is

   SkillsMenu: Menu;
   MenuWindow2: Window;

   procedure ShowSchoolInfo is
      InfoWindow, ClearWindow: Window;
      WindowWidth: Column_Position := (Columns / 2);
      WindowHeight: Line_Position := Line_Position(Skills_List.Length) + 1;
      Cost: Natural;
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
         Cost := TrainCost(CurrentMenuIndex, SkillsData_Container.To_Index(I));
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 1);
         if Cost > 0 then
            Add
              (Win => InfoWindow,
               Str =>
                 To_String(Skills_List(I).Name) &
                 ":" &
                 Natural'Image(Cost) &
                 " " &
                 To_String(MoneyName));
         else
            Add
              (Win => InfoWindow,
               Str => To_String(Skills_List(I).Name) & ": can't train");
         end if;
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
      if (WindowHeight + 6) > (Lines - 1) then
         WindowHeight := Lines - 7;
      end if;
      Resize(InfoWindow, WindowHeight + 1, WindowWidth);
      WindowFrame(InfoWindow, 2, "Train cost");
      MoneyIndex2 := FindItem(PlayerShip.Cargo, FindProtoItem(MoneyIndex));
      Move_Cursor(Line => WindowHeight + 4, Column => (Columns / 2));
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
      Move_Cursor(Line => WindowHeight + 5, Column => (Columns / 2));
      Add(Str => "Press Enter to select skill to train");
      Change_Attributes
        (Line => WindowHeight + 5,
         Column => (Columns / 2) + 6,
         Count => 5,
         Attr => BoldCharacters,
         Color => 1);
      Move_Cursor(Line => WindowHeight + 6, Column => (Columns / 2));
      Add(Str => "Press Escape to back to sky map");
      Change_Attributes
        (Line => WindowHeight + 6,
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
   begin
      for I in PlayerShip.Crew.Iterate loop
         School_Items.all(Crew_Container.To_Index(I)) :=
           New_Item(To_String(PlayerShip.Crew(I).Name));
      end loop;
      School_Items.all(PlayerShip.Crew.Last_Index + 1) := Null_Item;
      TradeMenu := New_Menu(School_Items);
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

   procedure ShowSchoolSkillsMenu is
      Skills_Items: constant Item_Array_Access :=
        new Item_Array
        (Skills_List.First_Index .. (Skills_List.Last_Index + 2));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Positive := 1;
      function CanTrain(SkillIndex: Positive) return Boolean is
      begin
         for Skill of PlayerShip.Crew(CurrentMenuIndex).Skills loop
            if Skill(1) = SkillIndex and Skill(2) = 100 then
               return False;
            end if;
         end loop;
         return True;
      end CanTrain;
   begin
      for I in Skills_List.Iterate loop
         if CanTrain(SkillsData_Container.To_Index(I)) then
            Skills_Items.all(MenuIndex) :=
              New_Item
                (To_String(Skills_List(I).Name),
                 Positive'Image(SkillsData_Container.To_Index(I)));
            MenuIndex := MenuIndex + 1;
         end if;
      end loop;
      Skills_Items.all(MenuIndex) := New_Item("Close");
      MenuIndex := MenuIndex + 1;
      for I in MenuIndex .. Skills_Items'Last loop
         Skills_Items.all(I) := Null_Item;
      end loop;
      SkillsMenu := New_Menu(Skills_Items);
      Set_Options(SkillsMenu, (Show_Descriptions => False, others => True));
      Scale(SkillsMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow2, 5, "Train");
      Set_Window(SkillsMenu, MenuWindow2);
      Set_Sub_Window
        (SkillsMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(SkillsMenu);
      Refresh(MenuWindow2);
      Refresh;
   end ShowSchoolSkillsMenu;

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
            ShowSchoolSkillsMenu;
            return SchoolSkills_Menu;
         when others =>
            Result := Driver(TradeMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(TradeMenu, M_Clear_Pattern);
               Result := Driver(TradeMenu, Key);
            end if;
      end case;
      CurrentMenuIndex := Menus.Get_Index(Current(TradeMenu));
      if Result = Menu_Ok then
         ShowSchoolInfo;
      end if;
      return School_View;
   end SchoolKeys;

   function SchoolSkillsMenuKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
   begin
      case Key is
         when 27 => -- Esc select close option, used second time, close menu
            if Name(Current(SkillsMenu)) = "Close" then
               if SkillsMenu /= Null_Menu then
                  Post(SkillsMenu, False);
                  Delete(SkillsMenu);
               end if;
               DrawGame(School_View);
               return School_View;
            else
               Result := Driver(SkillsMenu, M_Last_Item);
            end if;
         when 56 | KEY_UP => -- Select previous skill
            Result := Driver(SkillsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(SkillsMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next skill
            Result := Driver(SkillsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(SkillsMenu, M_First_Item);
            end if;
         when 10 => -- Start training skill or close menu
            if Name(Current(SkillsMenu)) /= "Close" then
               TrainSkill(CurrentMenuIndex, Get_Index(Current(SkillsMenu)));
            end if;
            if SkillsMenu /= Null_Menu then
               Post(SkillsMenu, False);
               Delete(SkillsMenu);
            end if;
            DrawGame(School_View);
            return School_View;
         when others =>
            Result := Driver(SkillsMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(SkillsMenu, M_Clear_Pattern);
               Result := Driver(SkillsMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return SchoolSkills_Menu;
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
      when Trade_Cant_Train =>
         ShowDialog("You can't train this skill any more.");
         DrawGame(School_View);
         return School_View;
   end SchoolSkillsMenuKeys;

end Bases.UI.School;
