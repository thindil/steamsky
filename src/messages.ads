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

-- ****t* Messages/Message_Type
-- FUNCTION
-- Types of messages
-- SOURCE
   type Message_Type is
     (Default, CombatMessage, TradeMessage, OrderMessage, CraftMessage,
      OtherMessage, MissionMessage);
-- ****
-- ****t* Messages/Message_Color
-- FUNCTION
-- Colors of messages
-- SOURCE
   type Message_Color is (WHITE, YELLOW, GREEN, RED, BLUE, CYAN);
-- ****
-- ****t* Messages/Message_Data
-- FUNCTION
-- Data structure for messages
-- SOURCE
   type Message_Data is record
      Message: Unbounded_String; -- Text of message
      MType: Message_Type; -- Type of message
      Color: Message_Color; -- Color used for show message
   end record;
-- ****

-- ****t* Messages/Messages_Container
-- SOURCE
   package Messages_Container is new Vectors(Positive, Message_Data);
-- ****

-- ****v* Messages/Messages_List
-- FUNCTION
-- List of all messages
-- SOURCE
   Messages_List: Messages_Container.Vector;
-- ****
-- ****v* Messages/LastMessageIndex
-- FUNCTION
-- Index of last message to show
-- SOURCE
   LastMessageIndex: Natural := 0;
-- ****

-- ****f* Messages/FormatedTime
-- FUNCTION
-- Format game time
-- SOURCE
   function FormatedTime(Time: Date_Record := GameDate) return String;
-- ****
-- ****f* Messages/AddMessage
-- FUNCTION
-- Add new message to list
-- SOURCE
   procedure AddMessage
     (Message: String; MType: Message_Type; Color: Message_Color := WHITE);
-- ****
-- ****f* Messages/GetMessage
-- FUNCTION
-- Return selected message
-- SOURCE
   function GetMessage
     (MessageIndex: Integer; MType: Message_Type := Default)
      return Message_Data;
-- ****
-- ****f* Messages/ClearMessages;
-- FUNCTION
-- Remove all messages
-- SOURCE
   procedure ClearMessages;
-- ****
-- ****f* Messages/MessagesAmount
-- FUNCTION
-- Return amount of selected type messages
-- SOURCE
   function MessagesAmount(MType: Message_Type := Default) return Natural;
-- ****
-- ****f* Messages/RestoreMessage
-- FUNCTION
-- Restore message from save file
-- SOURCE
   procedure RestoreMessage
     (Message: Unbounded_String; MType: Message_Type := Default;
      Color: Message_Color := WHITE);
-- ****
-- ****f* Messages/GetLastMessageIndex
-- FUNCTION
-- Return last message index
-- SOURCE
   function GetLastMessageIndex return Natural;
-- ****

end Messages;
