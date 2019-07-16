--    Copyright 2016-2019 Bartek thindil Jasicki
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

   -- Types of messages
   type Message_Type is
     (Default, CombatMessage, TradeMessage, OrderMessage, CraftMessage,
      OtherMessage, MissionMessage);
   -- Colors of messages
   type Message_Color is (WHITE, YELLOW, GREEN, RED, BLUE, CYAN);
   -- Data structure for messages
   type Message_Data is record
      Message: Unbounded_String; -- Text of message
      MType: Message_Type; -- Type of message
      Color: Message_Color; -- Color used for show message
   end record;
   package Messages_Container is new Vectors(Positive, Message_Data);
   -- List of all messages
   Messages_List: Messages_Container.Vector;
   -- Index of last message to show
   LastMessageIndex: Natural := 0;

   -- Format game time
   function FormatedTime(Time: Date_Record := GameDate) return String;
   -- Add new message to list
   procedure AddMessage
     (Message: String; MType: Message_Type; Color: Message_Color := WHITE);
   -- Return selected message
   function GetMessage
     (MessageIndex: Integer; MType: Message_Type := Default)
      return Message_Data;
   -- Remove all messages
   procedure ClearMessages;
   -- Return amount of selected type messages
   function MessagesAmount(MType: Message_Type := Default) return Natural;
   -- Restore message from save file
   procedure RestoreMessage
     (Message: Unbounded_String; MType: Message_Type := Default;
      Color: Message_Color := WHITE);
   -- Return last message index
   function GetLastMessageIndex return Natural;

end Messages;
