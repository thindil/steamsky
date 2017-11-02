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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with MainMenu; use MainMenu;
with UserInterface; use UserInterface;
with Utils.UI; use Utils.UI;

package body Goals.UI is

   GoalsMenu: Menu;
   MenuWindow: Window;

   procedure ShowGoalsList(GType: GoalTypes) is
      Goals_Items: constant Item_Array_Access :=
        new Item_Array(Goals_List.First_Index .. Goals_List.Last_Index);
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Integer := 1;
      MenuOptions: Menu_Option_Set;
      Visibility: Cursor_Visibility := Invisible;
   begin
      Set_Cursor_Visibility(Visibility);
      for I in Goals_List.Iterate loop
         if Goals_List(I).GType = GType then
            Goals_Items.all(MenuIndex) :=
              New_Item
                (GoalText(Goals_Container.To_Index(I)),
                 Positive'Image(Goals_Container.To_Index(I)));
            MenuIndex := MenuIndex + 1;
         end if;
      end loop;
      Goals_Items.all(MenuIndex) := New_Item("Close", "0");
      MenuIndex := MenuIndex + 1;
      for I in MenuIndex .. Goals_Items'Last loop
         Goals_Items.all(I) := Null_Item;
      end loop;
      GoalsMenu := New_Menu(Goals_Items);
      MenuOptions := Get_Options(GoalsMenu);
      MenuOptions.Show_Descriptions := False;
      Set_Options(GoalsMenu, MenuOptions);
      Set_Format(GoalsMenu, Lines - (Lines / 2), 1);
      Scale(GoalsMenu, MenuHeight, MenuLength);
      MenuWindow :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow, 5, "Select goal");
      Set_Window(GoalsMenu, MenuWindow);
      Set_Sub_Window
        (GoalsMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 1, 1));
      Post(GoalsMenu);
      Refresh_Without_Update;
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
   end ShowGoalsList;

   procedure ShowGoalsTypes is
      GoalsTypes_Items: constant Item_Array_Access := new Item_Array(1 .. 8);
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuOptions: Menu_Option_Set;
   begin
      for I in 1 .. 6 loop
         GoalsTypes_Items.all(I) :=
           New_Item
             (GoalTypes'Image(GoalTypes'Val(I))(1) &
              To_Lower
                (GoalTypes'Image(GoalTypes'Val(I))
                   (2 .. GoalTypes'Image(GoalTypes'Val(I))'Last)),
              Positive'Image(I));
      end loop;
      GoalsTypes_Items.all(7) := New_Item("Close", "0");
      GoalsTypes_Items.all(8) := Null_Item;
      GoalsMenu := New_Menu(GoalsTypes_Items);
      MenuOptions := Get_Options(GoalsMenu);
      MenuOptions.Show_Descriptions := False;
      Set_Options(GoalsMenu, MenuOptions);
      Set_Format(GoalsMenu, Lines - 4, 1);
      Scale(GoalsMenu, MenuHeight, MenuLength);
      if MenuLength < 15 then
         MenuLength := 15;
      end if;
      MenuWindow :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow, 5, "Select type");
      Set_Window(GoalsMenu, MenuWindow);
      Set_Sub_Window
        (GoalsMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 1, 1));
      Post(GoalsMenu);
      Refresh_Without_Update;
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
   end ShowGoalsTypes;

   function GoalsMenuKeys
     (Key: Key_Code;
      CurrentState, OldState: GameStates) return GameStates is
      Result: Driver_Result;
      GoalIndex: constant Natural :=
        Natural'Value(Description(Current(GoalsMenu)));
   begin
      case Key is
         when 56 | KEY_UP => -- Select previous goal
            Result := Driver(GoalsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(GoalsMenu, M_Last_Item);
            end if;
         when 50 | KEY_DOWN => -- Select next goal
            Result := Driver(GoalsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(GoalsMenu, M_First_Item);
            end if;
         when 10 => -- Select goal/goal type or quit
            if CurrentState = GoalsList_View then
               if GoalIndex > 0 then
                  CurrentGoal := Goals_List(GoalIndex);
               end if;
               Post(GoalsMenu, False);
               Delete(GoalsMenu);
               if OldState = New_Game then
                  Erase;
                  ShowMainMenu;
                  ShowNewGameForm(8);
               else
                  DrawGame(GameStats_View);
               end if;
               return OldState;
            else
               Post(GoalsMenu, False);
               Delete(GoalsMenu);
               if OldState = New_Game then
                  Erase;
                  ShowMainMenu;
                  ShowNewGameForm(8);
               else
                  DrawGame(GameStats_View);
               end if;
               if GoalIndex > 0 then
                  ShowGoalsList(GoalTypes'Val(GoalIndex));
                  Refresh;
               end if;
               if GoalIndex > 0 then
                  return GoalsList_View;
               else
                  return OldState;
               end if;
            end if;
         when 27 => -- Esc select close option, used second time, close menu
            if Name(Current(GoalsMenu)) = "Close" then
               Post(GoalsMenu, False);
               Delete(GoalsMenu);
               if OldState = New_Game then
                  Erase;
                  ShowMainMenu;
                  ShowNewGameForm(8);
               else
                  DrawGame(OldState);
               end if;
               return OldState;
            else
               Result := Driver(GoalsMenu, M_Last_Item);
            end if;
         when others =>
            Result := Driver(GoalsMenu, Key);
            if Result /= Menu_Ok then
               Result := Driver(GoalsMenu, M_Clear_Pattern);
               Result := Driver(GoalsMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow);
      end if;
      return CurrentState;
   end GoalsMenuKeys;

end Goals.UI;
