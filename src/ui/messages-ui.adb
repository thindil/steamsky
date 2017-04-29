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

with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with UserInterface; use UserInterface;

package body Messages.UI is

   StartIndex, EndIndex: Integer := 0;
   MessagesType: Message_Type := Default;
   MessagesPad: Window := Null_Window;
   MessagesMenu: Menu;
   MenuWindow: Window;

   procedure ShowMessages is
      LinesAmount: Line_Position := 0;
      TextMessages: Unbounded_String;
      Messages_Items: constant Item_Array_Access := new Item_Array(1 .. 9);
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      if Messages_List.Length = 0 then
         Move_Cursor(Line => (Lines / 2), Column => (Columns / 2) - 8);
         Add(Str => "No messages yet.");
         return;
      end if;
      Messages_Items.all :=
        (New_Item("All"),
         New_Item("Combat"),
         New_Item("Trade"),
         New_Item("Orders"),
         New_Item("Crafts"),
         New_Item("Missions"),
         New_Item("Others"),
         New_Item("Delete all"),
         Null_Item);
      MessagesMenu := New_Menu(Messages_Items);
      Set_Format(MessagesMenu, 1, 8);
      Set_Mark(MessagesMenu, "");
      Scale(MessagesMenu, MenuHeight, MenuLength);
      MenuWindow := Create(MenuHeight, MenuLength, 2, 2);
      Set_Window(MessagesMenu, MenuWindow);
      Set_Sub_Window
        (MessagesMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
      Post(MessagesMenu);
      Set_Current
        (MessagesMenu,
         Messages_Items.all(Message_Type'Pos(MessagesType) + 1));
      if MessagesPad = Null_Window then
         for Message of reverse Messages_List loop
            if Message.MType = MessagesType or MessagesType = Default then
               Append(TextMessages, Message.Message);
               Append(TextMessages, ASCII.LF);
               LinesAmount := LinesAmount + 1;
               if Length(Message.Message) > Positive(Columns - 2) then
                  LinesAmount :=
                    LinesAmount +
                    (Line_Position(Length(Message.Message)) /
                     Line_Position(Columns - 2));
               end if;
            end if;
         end loop;
         if LinesAmount < 1 then
            LinesAmount := 1;
            TextMessages :=
              To_Unbounded_String("There no messages of that type.");
         end if;
         MessagesPad := New_Pad(LinesAmount + 1, Columns - 2);
         Add(Win => MessagesPad, Str => To_String(TextMessages));
         EndIndex := Integer(LinesAmount - (Lines - 4));
         if EndIndex < 0 then
            EndIndex := 0;
         end if;
      end if;
      Refresh;
      Refresh(MenuWindow);
      Refresh
        (MessagesPad,
         Line_Position(StartIndex),
         0,
         4,
         2,
         (Lines - 1),
         (Columns - 2));
   end ShowMessages;

   function MessagesKeys
     (Key: Key_Code;
      OldState: GameStates) return GameStates is
      Result: Driver_Result;
      function SetMessagesType return GameStates is
      begin
         if MessagesPad /= Null_Window then
            Delete(MessagesPad);
         end if;
         if Get_Index(Current(MessagesMenu)) < 8 then
            MessagesType :=
              Message_Type'Val(Get_Index(Current(MessagesMenu)) - 1);
            DrawGame(Messages_View);
            return Messages_View;
         else
            DrawGame(Clear_Confirm);
            return Clear_Confirm;
         end if;
      end SetMessagesType;
   begin
      case Key is
         when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
            StartIndex := 0;
            MessagesType := Default;
            if MessagesPad /= Null_Window then
               Delete(MessagesPad);
            end if;
            DrawGame(OldState);
            return OldState;
         when 56 | KEY_UP => -- Scroll messages one line up
            StartIndex := StartIndex - 1;
            if StartIndex < 0 then
               StartIndex := 0;
            end if;
            ShowMessages;
         when 50 | KEY_DOWN => -- Scroll messages one line down
            StartIndex := StartIndex + 1;
            if StartIndex > EndIndex then
               StartIndex := EndIndex;
            end if;
            ShowMessages;
         when 51 | KEY_NPAGE => -- Scroll messages one screen down
            StartIndex := StartIndex + Integer(Lines - 4);
            if StartIndex > EndIndex then
               StartIndex := EndIndex;
            end if;
            ShowMessages;
         when 57 | KEY_PPAGE => -- Scroll messages one screen up
            StartIndex := StartIndex - Integer(Lines - 4);
            if StartIndex < 0 then
               StartIndex := 0;
            end if;
            ShowMessages;
         when 55 | Key_Home => -- Scroll messages to start
            StartIndex := 0;
            ShowMessages;
         when 49 | Key_End => -- Scroll messages to end
            StartIndex := EndIndex;
            ShowMessages;
         when 52 | KEY_LEFT => -- Select previous messages types
            Result := Driver(MessagesMenu, M_Left_Item);
            if Result = Request_Denied then
               Result := Driver(MessagesMenu, M_Last_Item);
            end if;
            return SetMessagesType;
         when 54 | KEY_RIGHT => -- Select next messages types
            Result := Driver(MessagesMenu, M_Right_Item);
            if Result = Request_Denied then
               Result := Driver(MessagesMenu, M_First_Item);
            end if;
            return SetMessagesType;
         when others =>
            Result := Driver(MessagesMenu, Key);
            if Result = Menu_Ok then
               return SetMessagesType;
            else
               Result := Driver(MessagesMenu, M_Clear_Pattern);
               Result := Driver(MessagesMenu, Key);
               if Result = Menu_Ok then
                  return SetMessagesType;
               end if;
            end if;
      end case;
      return Messages_View;
   end MessagesKeys;

end Messages.UI;
