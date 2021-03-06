--    Copyright 2016-2021 Bartek thindil Jasicki
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

with Config; use Config;

package body Messages is

   function FormatedTime(Time: Date_Record := Game_Date) return String is
      Result: Unbounded_String := To_Unbounded_String("");
      RawImage: Unbounded_String;
      TimeArray: constant Natural_Array(1 .. 5) :=
        (Time.Year, Time.Month, Time.Day, Time.Hour, Time.Minutes);
   begin
      Format_Time_Loop :
      for I in TimeArray'Range loop
         RawImage := To_Unbounded_String(Natural'Image(TimeArray(I)));
         case I is
            when 1 =>
               Result := Result & Trim(RawImage, Ada.Strings.Left);
            when 2 | 3 =>
               Result :=
                 Result & To_Unbounded_String("-") &
                 Trim(RawImage, Ada.Strings.Left);
            when 4 =>
               Result := Result & RawImage;
            when 5 =>
               Result :=
                 (if TimeArray(5) < 10 then
                    Result & ":0" & Trim(RawImage, Ada.Strings.Left)
                  else Result & ":" & Trim(RawImage, Ada.Strings.Left));
         end case;
      end loop Format_Time_Loop;
      return To_String(Result);
   end FormatedTime;

   procedure AddMessage
     (Message: String; MType: Message_Type; Color: Message_Color := WHITE) is
   begin
      if Natural(Messages_List.Length) = Game_Settings.Messages_Limit then
         Messages_List.Delete_First;
      end if;
      Messages_List.Append
        (New_Item =>
           (Message =>
              To_Unbounded_String(FormatedTime) & ": " &
              To_Unbounded_String(Message),
            MType => MType, Color => Color));
      LastMessageIndex := GetLastMessageIndex;
   end AddMessage;

   function GetMessage
     (MessageIndex: Integer; MType: Message_Type := Default)
      return Message_Data is
      Index: Integer;
   begin
      if MessageIndex > Integer(Messages_List.Length) then
         return
           (Message => Null_Unbounded_String, MType => Default,
            Color => WHITE);
      end if;
      if MessageIndex < 1 then
         Index := 1;
         if Integer(Messages_List.Length) + MessageIndex > 0 then
            Get_Reverse_Message_Loop :
            for Message of reverse Messages_List loop
               if Message.MType = MType or MType = Default then
                  Index := Index - 1;
               end if;
               if Index = MessageIndex then
                  return Message;
               end if;
            end loop Get_Reverse_Message_Loop;
         end if;
         return
           (Message => Null_Unbounded_String, MType => Default,
            Color => WHITE);
      end if;
      Index := 0;
      Get_Message_Loop :
      for Message of Messages_List loop
         if Message.MType = MType or MType = Default then
            Index := Index + 1;
         end if;
         if Index = MessageIndex then
            return Message;
         end if;
      end loop Get_Message_Loop;
      return
        (Message => Null_Unbounded_String, MType => Default, Color => WHITE);
   end GetMessage;

   procedure ClearMessages is
   begin
      Messages_List.Clear;
   end ClearMessages;

   function MessagesAmount(MType: Message_Type := Default) return Natural is
      Amount: Natural := 0;
   begin
      if MType = Default then
         return Natural(Messages_List.Length);
      else
         Count_Messages_Loop :
         for Message of Messages_List loop
            if Message.MType = MType then
               Amount := Amount + 1;
            end if;
         end loop Count_Messages_Loop;
         return Amount;
      end if;
   end MessagesAmount;

   procedure RestoreMessage
     (Message: Unbounded_String; MType: Message_Type := Default;
      Color: Message_Color := WHITE) is
   begin
      Messages_List.Append
        (New_Item => (Message => Message, MType => MType, Color => Color));
   end RestoreMessage;

   function GetLastMessageIndex return Natural is
   begin
      return Messages_List.Last_Index;
   end GetLastMessageIndex;

end Messages;
