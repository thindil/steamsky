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

-- ****h* Steamsky/Messages
-- FUNCTION
-- Provides code for manipulate in game messages
-- SOURCE
package Messages is
-- ****

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
   -- PARAMETERS
   -- Message - Text of message
   -- MType   - Type of message
   -- Color   - Color used for show message
   -- SOURCE
   type Message_Data is record
      Message: Unbounded_String;
      MType: Message_Type;
      Color: Message_Color;
   end record;
   -- ****

   -- ****t* Messages/Messages_Container
   -- FUNCTION
   -- Used to store messages data
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
   -- PARAMETERS
   -- Time - Game time to format. Default is current game time
   -- RESULT
   -- Formatted in YYYY-MM-DD HH:MM style in game time
   -- SOURCE
   function FormatedTime(Time: Date_Record := GameDate) return String with
      Test_Case => ("Test_FormattedTime", Robustness);
      -- ****

      -- ****f* Messages/AddMessage
      -- FUNCTION
      -- Add new message to list
      -- PARAMETERS
      -- Message - Text of message to add
      -- MType   - Type of message to add
      -- Color   - Color of message to add
      -- SOURCE
   procedure AddMessage
     (Message: String; MType: Message_Type; Color: Message_Color := WHITE) with
      Pre => Message'Length > 0,
      Test_Case => ("Test_AddMessage", Nominal);
      -- ****

      -- ****f* Messages/GetMessage
      -- FUNCTION
      -- Get Nth message of selected type
      -- PARAMETERS
      -- MessageIndex - If positive, get Nth message from start of list if
      --                negative, get Nth message from the end of the messages
      --                list
      -- MType        - Type of messages to check. Default all messages
      -- RESULT
      -- Selected message or empty message if nothing found
      -- SOURCE
   function GetMessage
     (MessageIndex: Integer; MType: Message_Type := Default)
      return Message_Data with
      Test_Case => ("Test_GetMessage", Robustness);
      -- ****

      -- ****f* Messages/ClearMessages
      -- FUNCTION
      -- Remove all messages
      -- SOURCE
   procedure ClearMessages with
      Test_Case => ("Test_ClearMessages", Robustness);
      -- ****

      -- ****f* Messages/MessagesAmount
      -- FUNCTION
      -- Get amount of selected type messages
      -- PARAMETERS
      -- MType - Type of messages to search. Default is all messages
      -- RESULT
      -- Amount of messages of selected type
      -- SOURCE
   function MessagesAmount(MType: Message_Type := Default) return Natural;
   -- ****

   -- ****f* Messages/RestoreMessage
   -- FUNCTION
   -- Restore message from save file
   -- PARAMETERS
   -- Message - Text of message to restore
   -- MType   - Type of message to restore. Default is no type
   -- Color   - Color of message to restore. Default is white.
   -- SOURCE
   procedure RestoreMessage
     (Message: Unbounded_String; MType: Message_Type := Default;
      Color: Message_Color := WHITE) with
      Pre => Message /= Null_Unbounded_String;
      -- ****

      -- ****f* Messages/GetLastMessageIndex
      -- FUNCTION
      -- Get last message index
      -- RESULT
      -- List index of the last message
      -- SOURCE
   function GetLastMessageIndex return Natural;
   -- ****

end Messages;
