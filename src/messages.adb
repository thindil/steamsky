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
      type Message_Data_C is record
         Msg: chars_ptr;
         Kind: Integer;
         Color: Integer;
      end record;
      function Nim_Get_Message
        (Mindex, Mtype: Integer) return Message_Data_C with
         Import => True,
         Convention => C,
         External_Name => "getMessage";
      Temp_Message: constant Message_Data_C :=
        Nim_Get_Message
          (Mindex => Message_Index, Mtype => Message_Type'Pos(M_Type));
   begin
      return
        (Message =>
           To_Unbounded_String(Source => Value(Item => Temp_Message.Msg)),
         M_Type => Message_Type'Val(Temp_Message.Kind),
         Color => Message_Color'Val(Temp_Message.Color));
   end Get_Message;

   function Messages_Amount(M_Type: Message_Type := DEFAULT) return Natural is
      function Nim_Messages_Amount(Kind: Integer) return Natural with
         Import => True,
         Convention => C,
         External_Name => "messagesAmount";
   begin
      return Nim_Messages_Amount(Kind => Message_Type'Pos(M_Type));
   end Messages_Amount;

   procedure Restore_Message
     (Message: Unbounded_String; M_Type: Message_Type := DEFAULT;
      Color: Message_Color := WHITE) is
   begin
      Messages_List.Append
        (New_Item => (Message => Message, M_Type => M_Type, Color => Color));
   end Restore_Message;

   function Get_Last_Message_Index return Natural is
      function Nim_Get_Last_Message_Index return Integer with
         Import => True,
         Convention => C,
         External_Name => "getLastMessageIndex";
   begin
      return Nim_Get_Last_Message_Index + 1;
   end Get_Last_Message_Index;

end Messages;
