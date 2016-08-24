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

    type Message_Data is -- Data structure for messages
        record
            Message : Unbounded_String;
            MType : Message_Type;
        end record;
    package Messages_Container is new Vectors(Positive, Message_Data);
    Messages_List : Messages_Container.Vector;
    StartIndex : Integer := 0;
    MessagesType : Message_Type := Default;

    function FormatedTime return String is
        Result : Unbounded_String := To_Unbounded_String("");
        RawImage : Unbounded_String;
        TimeArray : constant array(1..5) of Natural := (GameDate.Year,
            GameDate.Month, GameDate.Day, GameDate.Hour, GameDate.Minutes);
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
        Messages_List.Append(New_Item => (Message => To_Unbounded_String(FormatedTime) & ": " &
            To_Unbounded_String(Message), MType => MType));
        LastMessage := To_Unbounded_String(Message);
    end AddMessage;

    function GetMessage(MessageIndex : Integer; MType : Message_Type := Default) return String is
        Index : Integer;
    begin
        if MessageIndex > Integer(Messages_List.Length) then
            return "";
        end if;
        if MessageIndex < 1 then
            Index := 1;
            if Integer(Messages_List.Length) + MessageIndex > 0 then
                for I in reverse Messages_List.First_Index..Messages_List.Last_Index loop
                    if Messages_List.Element(I).MType = MType or MType = Default then
                        Index := Index - 1;
                    end if;
                    if Index = MessageIndex then
                        return To_String(Messages_List.Element(I).Message);
                    end if;
                end loop;
            end if;
            return "";
        end if;
        Index := 0;
        for I in Messages_List.First_Index..Messages_List.Last_Index loop
            if Messages_List.Element(I).MType = MType or MType = Default then
                Index := Index + 1;
            end if;
            if Index = MessageIndex then
                return To_String(Messages_List.Element(I).Message);
            end if;
        end loop;
        return "";
    end GetMessage;

    procedure ClearMessages is
    begin
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
        Messages_List.Append(New_Item => (Message => Message, MType => MType));
    end RestoreMessage;

    function GetMessageType(MessageIndex : Integer) return Message_Type is
    begin
        if MessageIndex > Integer(Messages_List.Length) then
            return Default;
        end if;
        if MessageIndex < 1 then
            if Integer(Messages_List.Length) + MessageIndex < 1 then
                return Default;
            else
                return Messages_List.Element(Integer(Messages_List.Length) + MessageIndex).MType;
            end if;
        end if;
        return Messages_List.Element(MessageIndex).MType;
    end GetMessageType;

    procedure ShowMessages is
        Index : Integer;
    begin
        if Messages_List.Length = 0 then
            Move_Cursor(Line => (Lines / 2), Column => (Columns / 2) - 8);
            Add(Str => "No messages yet.");
            return;
        end if;
        Index := StartIndex;
        Move_Cursor(Line => 2, Column => 2);
        Add(Str => "[All] [Combat] [Trade] [Orders] [Crafts] [Others]");
        Change_Attributes(Line => 2, Column => 3, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 9, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 18, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 26, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 36, Count => 1, Color => 1);
        Change_Attributes(Line => 2, Column => 47, Count => 1, Color => 1);
        if Index > 0 then
            Index := 0;
        end if;
        for I in 4..(Lines - 6) loop
            Move_Cursor(Line => I, Column => 2);
            Add(Str => GetMessage(Index, MessagesType));
            Index := Index - 1;
            exit when Index > Integer(Messages_List.Length);
        end loop;
    end ShowMessages;

    function MessagesKeys(Key : Key_Code) return GameStates is
    begin
        case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
                StartIndex := 0;
                MessagesType := Default;
                DrawGame(Sky_Map_View);
                return Sky_Map_View;
            when 56 | 65 => -- Scroll messages up
                StartIndex := StartIndex + 1;
                if StartIndex > 0 then
                    StartIndex := 0;
                end if;
                DrawGame(Messages_View);
                return Messages_View;
            when 50 | 66 => -- Scroll messages down
                StartIndex := StartIndex - 1;
                if abs StartIndex > MessagesAmount(MessagesType) then
                    StartIndex := 0 - (MessagesAmount(MessagesType) - Natural(Lines) + 6);
                    if StartIndex > 0 then
                        StartIndex := 0;
                    end if;
                end if;
                DrawGame(Messages_View);
                return Messages_View;
            when Character'Pos('a') => -- Show all messages
                StartIndex := 0;
                MessagesType := Default;
                DrawGame(Messages_View);
                return Messages_View;
            when Character'Pos('c') | Character'Pos('C') => -- Show combat messages
                StartIndex := 0;
                MessagesType := CombatMessage;
                DrawGame(Messages_View);
                return Messages_View;
            when Character'Pos('t') | Character'Pos('T') => -- Show trade messages
                StartIndex := 0;
                MessagesType := TradeMessage;
                DrawGame(Messages_View);
                return Messages_View;
            when Character'Pos('o') | Character'Pos('O') => -- Show orders messages
                StartIndex := 0;
                MessagesType := OrderMessage;
                DrawGame(Messages_View);
                return Messages_View;
            when Character'Pos('r') | Character'Pos('R') => -- Show craft messages
                StartIndex := 0;
                MessagesType := CraftMessage;
                DrawGame(Messages_View);
                return Messages_View;
            when Character'Pos('e') | Character'Pos('E') => -- Show others messages
                StartIndex := 0;
                MessagesType := OtherMessage;
                DrawGame(Messages_View);
                return Messages_View;
            when others =>
                return Messages_View;
        end case;
    end MessagesKeys;

end Messages;
