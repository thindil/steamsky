--    Copyright 2016 Bartek thindil Jasicki
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

with Ada.Containers.Vectors; use Ada.Containers;
with UserInterface; use UserInterface;

package body Messages is

    package Messages_Container is new Vectors(Positive, Message_Data);
    Messages_List : Messages_Container.Vector;
    StartIndex, EndIndex, LastIndex : Integer := 0;
    MessagesType : Message_Type := Default;
    MessagesPad : Window := Null_Window;

    function FormatedTime(Time : Date_Record := GameDate) return String is
        Result : Unbounded_String := To_Unbounded_String("");
        RawImage : Unbounded_String;
        TimeArray : constant array(1..5) of Natural := (Time.Year, Time.Month,
        Time.Day, Time.Hour, Time.Minutes);
    begin
        for I in TimeArray'Range loop
            RawImage := To_Unbounded_String(Natural'Image(TimeArray(I)));
            case I is
                when 1 =>
                    Result := Result & Trim(RawImage, Ada.Strings.Left);
                when 2 | 3 =>
                    Result := Result & To_Unbounded_String("-") & Trim(RawImage, Ada.Strings.Left);
                when 4 =>
                    Result := Result & RawImage;
                when 5 =>
                    if TimeArray(5) < 10 then
                        Result := Result & ":0" & Trim(RawImage, Ada.Strings.Left);
                    else
                        Result := Result & ":" & Trim(RawImage, Ada.Strings.Left);
                    end if;
            end case;
        end loop;
        return To_String(Result);
    end FormatedTime;

    procedure AddMessage(Message : String; MType : Message_Type) is
    begin
        if Messages_List.Length = 500 then
            Messages_List.Delete(Index => 1, Count => 1);
        end if;
        LastIndex := LastIndex + 1;
        Messages_List.Append(New_Item => (Message => To_Unbounded_String(FormatedTime) & ": " &
            To_Unbounded_String(Message), MType => MType, MessageIndex => LastIndex));
        LastMessage := To_Unbounded_String(Message);
    end AddMessage;

    function GetMessage(MessageIndex : Integer; MType : Message_Type := Default) return Message_Data is
        Index : Integer;
    begin
        if MessageIndex > Integer(Messages_List.Length) then
            return (Message => Null_Unbounded_String, MType => Default, MessageIndex => 1);
        end if;
        if MessageIndex < 1 then
            Index := 1;
            if Integer(Messages_List.Length) + MessageIndex > 0 then
                for I in reverse Messages_List.First_Index..Messages_List.Last_Index loop
                    if Messages_List.Element(I).MType = MType or MType = Default then
                        Index := Index - 1;
                    end if;
                    if Index = MessageIndex then
                        return Messages_List.Element(I);
                    end if;
                end loop;
            end if;
            return (Message => Null_Unbounded_String, MType => Default, MessageIndex => 1);
        end if;
        Index := 0;
        for I in Messages_List.First_Index..Messages_List.Last_Index loop
            if Messages_List.Element(I).MType = MType or MType = Default then
                Index := Index + 1;
            end if;
            if Index = MessageIndex then
                return Messages_List.Element(I);
            end if;
        end loop;
        return (Message => Null_Unbounded_String, MType => Default, MessageIndex => 1);
    end GetMessage;

    procedure ClearMessages is
    begin
        LastIndex := 0;
        Messages_List.Clear;
    end ClearMessages;

    function MessagesAmount(MType : Message_Type := Default) return Natural is
        Amount : Natural := 0;
    begin
        if MType = Default then
            return Natural(Messages_List.Length);
        else
            for I in Messages_List.First_Index..Messages_List.Last_Index loop
                if Messages_List.Element(I).MType = MType then
                    Amount := Amount + 1;
                end if;
            end loop;
            return Amount;
        end if;
    end MessagesAmount;

    procedure RestoreMessage(Message : Unbounded_String; MType : Message_Type := Default) is
    begin
        LastIndex := LastIndex + 1;
        Messages_List.Append(New_Item => (Message => Message, MType => MType, MessageIndex => LastIndex));
    end RestoreMessage;

    function GetLastMessageIndex return Natural is
    begin
        return LastIndex;
    end GetLastMessageIndex;

    procedure ShowMessages is
        LinesAmount : Line_Position := 0;
        TextMessages : Unbounded_String;
    begin
        if Messages_List.Length = 0 then
            Move_Cursor(Line => (Lines / 2), Column => (Columns / 2) - 8);
            Add(Str => "No messages yet.");
            return;
        end if;
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "[All] [Combat] [Trade] [Orders] [Crafts] [Others] [Delete all]");
        Change_Attributes(Line => 2, Column => 3, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 9, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 18, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 26, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 36, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 47, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 53, Count => 1, Color => 1);
        if MessagesPad = Null_Window then
            for I in reverse Messages_List.First_Index..Messages_List.Last_Index loop
                if Messages_List.Element(I).MType = MessagesType or MessagesType = Default then
                    Append(TextMessages, Messages_List.Element(I).Message);
                    Append(TextMessages, ASCII.LF);
                    LinesAmount := LinesAmount + 1;
                    if Length(Messages_List.Element(I).Message) > Positive(Columns - 2) then
                        LinesAmount := LinesAmount + (Line_Position(Length(Messages_List.Element(I).Message)) / 
                            Line_Position(Columns - 2));
                    end if;
                end if;
            end loop;
            if LinesAmount < 1 then
                LinesAmount := 1;
                TextMessages := To_Unbounded_String("There no messages of that type.");
            end if;
            MessagesPad := New_Pad(LinesAmount + 1, Columns - 2);
            Add(Win => MessagesPad, Str => To_String(TextMessages));
            EndIndex := Integer(LinesAmount - (Lines - 4));
            if EndIndex < 0 then
                EndIndex := 0;
            end if;
        end if;
        Refresh;
        Refresh(MessagesPad, Line_Position(StartIndex), 0, 4, 2, (Lines - 1), (Columns - 2));
    end ShowMessages;

    function MessagesKeys(Key : Key_Code; OldState : GameStates) return GameStates is
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
            when 55 | KEY_HOME => -- Scroll messages to start
                StartIndex := 0;
                ShowMessages;
            when 49 | KEY_END => -- Scroll messages to end
                StartIndex := EndIndex;
                ShowMessages;
            when Character'Pos('a') => -- Show all messages
                StartIndex := 0;
                MessagesType := Default;
                if MessagesPad /= Null_Window then
                    Delete(MessagesPad);
                end if;
                DrawGame(Messages_View);
            when Character'Pos('c') | Character'Pos('C') => -- Show combat messages
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
            when Character'Pos('o') | Character'Pos('O') => -- Show orders messages
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
            when Character'Pos('e') | Character'Pos('E') => -- Show others messages
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

end Messages;
