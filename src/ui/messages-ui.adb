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

with UserInterface; use UserInterface;

package body Messages.UI is

   StartIndex, EndIndex: Integer := 0;
   MessagesType: Message_Type := Default;
   MessagesPad: Window := Null_Window;

   procedure ShowMessages is
      LinesAmount: Line_Position := 0;
      TextMessages: Unbounded_String;
   begin
      if Messages_List.Length = 0 then
         Move_Cursor(Line => (Lines / 2), Column => (Columns / 2) - 8);
         Add(Str => "No messages yet.");
         return;
      end if;
      Move_Cursor(Line => 2, Column => 2);
      Add
        (Str =>
           "[All] [Combat] [Trade] [Orders] [Crafts] [Missions] [Others] [Delete all]");
      Change_Attributes(Line => 2, Column => 3, Count => 1, Color => 1);
      Change_Attributes(Line => 2, Column => 9, Count => 1, Color => 1);
      Change_Attributes(Line => 2, Column => 18, Count => 1, Color => 1);
      Change_Attributes(Line => 2, Column => 26, Count => 1, Color => 1);
      Change_Attributes(Line => 2, Column => 36, Count => 1, Color => 1);
      Change_Attributes(Line => 2, Column => 44, Count => 1, Color => 1);
      Change_Attributes(Line => 2, Column => 58, Count => 1, Color => 1);
      Change_Attributes(Line => 2, Column => 64, Count => 1, Color => 1);
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
         when Character'Pos('a') => -- Show all messages
            StartIndex := 0;
            MessagesType := Default;
            if MessagesPad /= Null_Window then
               Delete(MessagesPad);
            end if;
            DrawGame(Messages_View);
         when Character'Pos('c') |
           Character'Pos('C') => -- Show combat messages
            StartIndex := 0;
            MessagesType := CombatMessage;
            if MessagesPad /= Null_Window then
               Delete(MessagesPad);
            end if;
            DrawGame(Messages_View);
         when Character'Pos('t') | Character'Pos('T') => -- Show trade messages
            StartIndex := 0;
            MessagesType := TradeMessage;
            if MessagesPad /= Null_Window then
               Delete(MessagesPad);
            end if;
            DrawGame(Messages_View);
         when Character'Pos('o') |
           Character'Pos('O') => -- Show orders messages
            StartIndex := 0;
            MessagesType := OrderMessage;
            if MessagesPad /= Null_Window then
               Delete(MessagesPad);
            end if;
            DrawGame(Messages_View);
         when Character'Pos('r') | Character'Pos('R') => -- Show craft messages
            StartIndex := 0;
            MessagesType := CraftMessage;
            if MessagesPad /= Null_Window then
               Delete(MessagesPad);
            end if;
            DrawGame(Messages_View);
         when Character'Pos('m') |
           Character'Pos('M') => -- Show missions messages
            StartIndex := 0;
            MessagesType := MissionMessage;
            if MessagesPad /= Null_Window then
               Delete(MessagesPad);
            end if;
            DrawGame(Messages_View);
         when Character'Pos('e') |
           Character'Pos('E') => -- Show others messages
            StartIndex := 0;
            MessagesType := OtherMessage;
            if MessagesPad /= Null_Window then
               Delete(MessagesPad);
            end if;
            DrawGame(Messages_View);
         when Character'Pos('d') | Character'Pos('D') => -- Clear messages
            StartIndex := 0;
            MessagesType := Default;
            if MessagesPad /= Null_Window then
               Delete(MessagesPad);
            end if;
            DrawGame(Clear_Confirm);
            return Clear_Confirm;
         when others =>
            null;
      end case;
      return Messages_View;
   end MessagesKeys;

end Messages.UI;
