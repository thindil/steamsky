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

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Messages is

   function Formated_Time(Time: Date_Record := Game_Date) return String is

      function Nim_Formatted_Time
        (Year, Month, Day, Hour, Minutes: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "formattedTime";
   begin
      return
        Value
          (Item =>
             Nim_Formatted_Time
               (Year => Time.Year, Month => Time.Month, Day => Time.Day,
                Hour => Time.Hour, Minutes => Time.Minutes));
   end Formated_Time;

   procedure Add_Message
     (Message: String; M_Type: Message_Type; Color: Message_Color := WHITE) is
      procedure Nim_Add_Message
        (Msg: chars_ptr; Mtype: Integer; Mcolor: Integer) with
         Import => True,
         Convention => C,
         External_Name => "addMessage";
   begin
      Nim_Add_Message
        (Msg => New_String(Str => Message), Mtype => Message_Type'Pos(M_Type),
         Mcolor => Message_Color'Pos(Color));
   end Add_Message;

   function Get_Message
     (Message_Index: Integer; M_Type: Message_Type := DEFAULT)
      return Message_Data is
      Index: Integer;
   begin
      if Message_Index > Integer(Messages_List.Length) then
         return
           (Message => Null_Unbounded_String, M_Type => DEFAULT,
            Color => WHITE);
      end if;
      if Message_Index < 1 then
         Index := 1;
         if Integer(Messages_List.Length) + Message_Index > 0 then
            Get_Reverse_Message_Loop :
            for Message of reverse Messages_List loop
               if Message.M_Type = M_Type or M_Type = DEFAULT then
                  Index := Index - 1;
               end if;
               if Index = Message_Index then
                  return Message;
               end if;
            end loop Get_Reverse_Message_Loop;
         end if;
         return
           (Message => Null_Unbounded_String, M_Type => DEFAULT,
            Color => WHITE);
      end if;
      Index := 0;
      Get_Message_Loop :
      for Message of Messages_List loop
         if Message.M_Type = M_Type or M_Type = DEFAULT then
            Index := Index + 1;
         end if;
         if Index = Message_Index then
            return Message;
         end if;
      end loop Get_Message_Loop;
      return
        (Message => Null_Unbounded_String, M_Type => DEFAULT, Color => WHITE);
   end Get_Message;

   procedure Clear_Messages is
   begin
      Messages_List.Clear;
   end Clear_Messages;

   function Messages_Amount(M_Type: Message_Type := DEFAULT) return Natural is
      Amount: Natural := 0;
   begin
      if M_Type = DEFAULT then
         return Natural(Messages_List.Length);
      else
         Count_Messages_Loop :
         for Message of Messages_List loop
            if Message.M_Type = M_Type then
               Amount := Amount + 1;
            end if;
         end loop Count_Messages_Loop;
         return Amount;
      end if;
   end Messages_Amount;

   procedure Restore_Message
     (Message: Unbounded_String; M_Type: Message_Type := DEFAULT;
      Color: Message_Color := WHITE) is
   begin
      Messages_List.Append
        (New_Item => (Message => Message, M_Type => M_Type, Color => Color));
   end Restore_Message;

   function Get_Last_Message_Index return Natural is
   begin
      return Messages_List.Last_Index;
   end Get_Last_Message_Index;

end Messages;
