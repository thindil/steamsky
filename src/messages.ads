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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with Game; use Game;

package Messages is

   type Message_Type is
     (Default,
      CombatMessage,
      TradeMessage,
      OrderMessage,
      CraftMessage,
      OtherMessage,
      MissionMessage); -- Types of messages
   type Message_Data is -- Data structure for messages
   record
      Message: Unbounded_String;
      MType: Message_Type;
      MessageIndex: Positive;
   end record;
   package Messages_Container is new Vectors(Positive, Message_Data);
   Messages_List: Messages_Container.Vector; -- List of all messages

   LastMessage: Unbounded_String :=
     To_Unbounded_String(""); -- Last message received
   function FormatedTime
     (Time: Date_Record := GameDate) return String; -- Format game time
   procedure AddMessage
     (Message: String;
      MType: Message_Type); -- Add new message to list
   function GetMessage
     (MessageIndex: Integer;
      MType: Message_Type :=
        Default)
     return Message_Data; -- Return selected message
   procedure ClearMessages; -- Remove all messages;
   function MessagesAmount
     (MType: Message_Type :=
        Default)
     return Natural; -- Return amount of selected type messages
   procedure RestoreMessage
     (Message: Unbounded_String;
      MType: Message_Type := Default); -- Restore message from save file
   function GetLastMessageIndex return Natural; -- Return last message index

end Messages;
