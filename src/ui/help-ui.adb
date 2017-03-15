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

with Ada.Strings.Maps; use Ada.Strings.Maps;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with UserInterface; use UserInterface;

package body Help.UI is

   StartIndex, EndIndex: Integer := 0;
   TopicIndex: Integer := 1;
   HelpPad: Window;
   HelpMenu: Menu;
   MenuWindow: Window;
   PreviousState: GameStates := Sky_Map_View;

   procedure ShowHelpMenu(NewHelp: Boolean := False) is
      Help_Items: constant Item_Array_Access :=
        new Item_Array(Help_List.First_Index .. (Help_List.Last_Index + 1));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      if NewHelp then
         PreviousState := Sky_Map_View;
      end if;
      for I in Help_List.First_Index .. Help_List.Last_Index loop
         Help_Items.all(I) := New_Item(To_String(Help_List.Element(I).Title));
      end loop;
      Help_Items.all(Help_Items'Last) := Null_Item;
      HelpMenu := New_Menu(Help_Items);
      Set_Format(HelpMenu, Lines - 5, 1);
      Set_Mark(HelpMenu, "");
      Scale(HelpMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 2, (Columns / 3));
      Set_Window(HelpMenu, MenuWindow);
      Set_Sub_Window
        (HelpMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(HelpMenu);
      Set_Current(HelpMenu, Help_Items.all(TopicIndex));
      Refresh;
      Refresh(MenuWindow);
   end ShowHelpMenu;

   procedure ShowHelp
     (OldState: GameStates := Help_Topic;
      HelpIndex: Natural := 0) is
      LinesAmount: Line_Position;
      TextPosition, OldTextPosition: Natural := 1;
   begin
      if HelpIndex > 0 then
         TopicIndex := HelpIndex;
      end if;
      if OldState /= Help_Topic then
         PreviousState := OldState;
         LinesAmount :=
           Line_Position
             (Ada.Strings.Unbounded.Count
                (Help_List.Element(TopicIndex).Text,
                 To_Set(ASCII.LF)));
         while TextPosition > 0 loop
            TextPosition :=
              Index
                (Help_List.Element(TopicIndex).Text,
                 To_Set(ASCII.LF),
                 OldTextPosition);
            if TextPosition > 0 and
              Column_Position(TextPosition - OldTextPosition) > Columns then
               LinesAmount :=
                 LinesAmount +
                 (Line_Position((TextPosition - OldTextPosition)) /
                  Line_Position(Columns));
            end if;
            OldTextPosition := TextPosition + 1;
         end loop;
         if LinesAmount < 1 then
            LinesAmount := 1;
         end if;
         HelpPad := New_Pad(LinesAmount + 1, Columns);
         Add
           (Win => HelpPad,
            Str => To_String(Help_List.Element(TopicIndex).Text));
         EndIndex := Integer(LinesAmount - (Lines - 2));
         if EndIndex < 0 then
            EndIndex := 0;
         end if;
         Refresh;
      end if;
      Refresh
        (HelpPad,
         Line_Position(StartIndex),
         0,
         2,
         0,
         (Lines - 1),
         Columns);
   end ShowHelp;

   function HelpMenuKeys(Key: Key_Code) return GameStates is
      Result: Menus.Driver_Result;
   begin
      TopicIndex := Menus.Get_Index(Current(HelpMenu));
      case Key is
         when Character'Pos('q') |
           Character'Pos('Q') => -- Back to previous screen
            TopicIndex := 1;
            DrawGame(PreviousState);
            return PreviousState;
         when 56 | KEY_UP => -- Select previous help topic
            Result := Driver(HelpMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(HelpMenu, M_Last_Item);
            end if;
            if Result = Menu_Ok then
               Refresh(MenuWindow);
            end if;
         when 50 | KEY_DOWN => -- Select next help topic
            Result := Driver(HelpMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(HelpMenu, M_First_Item);
            end if;
            if Result = Menu_Ok then
               Refresh(MenuWindow);
            end if;
         when 10 => -- Select topic to read
            DrawGame(Help_Topic);
            return Help_Topic;
         when others =>
            Result := Driver(HelpMenu, Key);
            if Result = Menu_Ok then
               Refresh(MenuWindow);
            else
               Result := Driver(HelpMenu, M_Clear_Pattern);
               Result := Driver(HelpMenu, Key);
               if Result = Menu_Ok then
                  Refresh(MenuWindow);
               end if;
            end if;
      end case;
      return Help_View;
   end HelpMenuKeys;

   function HelpKeys(Key: Key_Code) return GameStates is
   begin
      case Key is
         when Character'Pos('q') |
           Character'Pos('Q') => -- Back to previous screen
            StartIndex := 0;
            DrawGame(PreviousState);
            return PreviousState;
         when Character'Pos('m') | Character'Pos('M') => -- Back to help menu
            StartIndex := 0;
            Erase;
            ShowGameHeader(Help_View);
            ShowHelpMenu;
            return Help_View;
         when 56 | KEY_UP => -- Scroll help one line up
            StartIndex := StartIndex - 1;
         when 50 | KEY_DOWN => -- Scroll help one line down
            StartIndex := StartIndex + 1;
         when 51 | KEY_NPAGE => -- Scroll help one screen down
            StartIndex := StartIndex + Integer(Lines - 2);
         when 57 | KEY_PPAGE => -- Scroll help one screen up
            StartIndex := StartIndex - Integer(Lines - 2);
         when 55 | Key_Home => -- Scroll help to start
            StartIndex := 0;
         when 49 | Key_End => -- Scroll help to end
            StartIndex := EndIndex;
         when others =>
            null;
      end case;
      if StartIndex < 0 then
         StartIndex := 0;
      end if;
      if StartIndex > EndIndex then
         StartIndex := EndIndex;
      end if;
      ShowHelp;
      return Help_Topic;
   end HelpKeys;

end Help.UI;
