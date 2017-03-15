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

package body Messages is

   LastIndex: Integer := 0;

   function FormatedTime(Time: Date_Record := GameDate) return String is
      Result: Unbounded_String := To_Unbounded_String("");
      RawImage: Unbounded_String;
      TimeArray: constant array(1 .. 5) of Natural :=
        (Time.Year, Time.Month, Time.Day, Time.Hour, Time.Minutes);
   begin
      for I in TimeArray'Range loop
         RawImage := To_Unbounded_String(Natural'Image(TimeArray(I)));
         case I is
            when 1 =>
               Result := Result & Trim(RawImage, Ada.Strings.Left);
            when 2 | 3 =>
               Result :=
                 Result &
                 To_Unbounded_String("-") &
                 Trim(RawImage, Ada.Strings.Left);
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

   procedure AddMessage(Message: String; MType: Message_Type) is
   begin
      if Messages_List.Length = 500 then
         Messages_List.Delete(Index => 1, Count => 1);
      end if;
      LastIndex := LastIndex + 1;
      Messages_List.Append
      (New_Item =>
         (Message =>
            To_Unbounded_String(FormatedTime) &
            ": " &
            To_Unbounded_String(Message),
          MType => MType,
          MessageIndex => LastIndex));
      LastMessage := To_Unbounded_String(Message);
   end AddMessage;

   function GetMessage
     (MessageIndex: Integer;
      MType: Message_Type := Default) return Message_Data is
      Index: Integer;
   begin
      if MessageIndex > Integer(Messages_List.Length) then
         return
           (Message => Null_Unbounded_String,
            MType => Default,
            MessageIndex => 1);
      end if;
      if MessageIndex < 1 then
         Index := 1;
         if Integer(Messages_List.Length) + MessageIndex > 0 then
            for Message of reverse Messages_List loop
               if Message.MType = MType or MType = Default then
                  Index := Index - 1;
               end if;
               if Index = MessageIndex then
                  return Message;
               end if;
            end loop;
         end if;
         return
           (Message => Null_Unbounded_String,
            MType => Default,
            MessageIndex => 1);
      end if;
      Index := 0;
      for Message of Messages_List loop
         if Message.MType = MType or MType = Default then
            Index := Index + 1;
         end if;
         if Index = MessageIndex then
            return Message;
         end if;
      end loop;
      return
        (Message => Null_Unbounded_String,
         MType => Default,
         MessageIndex => 1);
   end GetMessage;

   procedure ClearMessages is
   begin
      LastIndex := 0;
      Messages_List.Clear;
   end ClearMessages;

   function MessagesAmount(MType: Message_Type := Default) return Natural is
      Amount: Natural := 0;
   begin
      if MType = Default then
         return Natural(Messages_List.Length);
      else
         for Message of Messages_List loop
            if Message.MType = MType then
               Amount := Amount + 1;
            end if;
         end loop;
         return Amount;
      end if;
   end MessagesAmount;

   procedure RestoreMessage
     (Message: Unbounded_String;
      MType: Message_Type := Default) is
   begin
      LastIndex := LastIndex + 1;
      Messages_List.Append
      (New_Item =>
         (Message => Message, MType => MType, MessageIndex => LastIndex));
   end RestoreMessage;

   function GetLastMessageIndex return Natural is
   begin
      return LastIndex;
   end GetLastMessageIndex;

end Messages;
