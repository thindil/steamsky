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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with Game; use Game;

-- ****h* Messages/Messages
-- FUNCTION
-- Provides code for manipulate in game messages
-- SOURCE
package Messages is
-- ****

   -- ****t* Messages/Messages.Message_Type
   -- FUNCTION
   -- Types of messages
   -- SOURCE
   type Message_Type is
     (DEFAULT, COMBATMESSAGE, TRADEMESSAGE, ORDERMESSAGE, CRAFTMESSAGE,
      OTHERMESSAGE, MISSIONMESSAGE) with
      Default_Value => DEFAULT;
   -- ****

   -- ****t* Messages/Messages.Message_Color
   -- FUNCTION
   -- Colors of messages
   -- SOURCE
   type Message_Color is (WHITE, YELLOW, GREEN, RED, BLUE, CYAN) with
      Default_Value => WHITE;
   -- ****

   -- ****s* Messages/Messages.Message_Data
   -- FUNCTION
   -- Data structure for messages
   -- PARAMETERS
   -- Message  - Text of message
   -- M_Type   - Type of message
   -- Color    - Color used for show message
   -- SOURCE
   type Message_Data is record
      Message: Unbounded_String;
      M_Type: Message_Type;
      Color: Message_Color;
   end record;
   -- ****

   -- ****t* Messages/Messages.Messages_Container
   -- FUNCTION
   -- Used to store messages data
   -- SOURCE
   package Messages_Container is new Vectors
     (Index_Type => Positive, Element_Type => Message_Data);
   -- ****

   -- ****v* Messages/Messages.Messages_List
   -- FUNCTION
   -- List of all messages
   -- SOURCE
   Messages_List: Messages_Container.Vector;
   -- ****

   -- ****v* Messages/Messages.Last_Message_Index
   -- FUNCTION
   -- Index of last message to show
   -- SOURCE
   Last_Message_Index: Natural := 0;
   -- ****

   -- ****f* Messages/Messages.Formated_Time
   -- FUNCTION
   -- Format game time
   -- PARAMETERS
   -- Time - Game time to format. Default is current game time
   -- RESULT
   -- Formatted in YYYY-MM-DD HH:MM style in game time
   -- SOURCE
   function Formated_Time(Time: Date_Record := Game_Date) return String with
      Post => Formated_Time'Result'Length > 0,
      Test_Case => (Name => "Test_FormattedTime", Mode => Nominal);
      -- ****

      -- ****f* Messages/Messages.Add_Message
      -- FUNCTION
      -- Add new message to list
      -- PARAMETERS
      -- Message  - Text of message to add
      -- M_Type   - Type of message to add
      -- Color    - Color of message to add
      -- SOURCE
   procedure Add_Message
     (Message: String; M_Type: Message_Type;
      Color: Message_Color := WHITE) with
      Pre => Message'Length > 0,
      Test_Case => (Name => "Test_AddMessage", Mode => Nominal);
      -- ****

      -- ****f* Messages/Messages.Get_Message
      -- FUNCTION
      -- Get Nth message of selected type
      -- PARAMETERS
      -- Message_Index - If positive, get Nth message from start of list if
      --                 negative, get Nth message from the end of the messages
      --                 list
      -- M_Type        - Type of messages to check. Default all messages
      -- RESULT
      -- Selected message or empty message if nothing found
      -- SOURCE
   function Get_Message
     (Message_Index: Integer; M_Type: Message_Type := DEFAULT)
      return Message_Data with
      Test_Case => (Name => "Test_GetMessage", Mode => Robustness);
      -- ****

      -- ****f* Messages/Messages.Clear_Messages
      -- FUNCTION
      -- Remove all messages
      -- SOURCE
   procedure Clear_Messages with
      Test_Case => (Name => "Test_ClearMessages", Mode => Robustness);
      -- ****

      -- ****f* Messages/Messages.Messages_Amount
      -- FUNCTION
      -- Get amount of selected type messages
      -- PARAMETERS
      -- M_Type - Type of messages to search. Default is all messages
      -- RESULT
      -- Amount of messages of selected type
      -- SOURCE
   function Messages_Amount
     (M_Type: Message_Type := DEFAULT) return Natural with
      Test_Case => (Name => "Test_MessagesAmount", Mode => Robustness);
      -- ****

      -- ****f* Messages/Messages.Restore_Message
      -- FUNCTION
      -- Restore message from save file
      -- PARAMETERS
      -- Message  - Text of message to restore
      -- M_Type   - Type of message to restore. Default is no type
      -- Color    - Color of message to restore. Default is white.
      -- SOURCE
   procedure Restore_Message
     (Message: Unbounded_String; M_Type: Message_Type := DEFAULT;
      Color: Message_Color := WHITE) with
      Pre => Message /= Null_Unbounded_String,
      Test_Case => (Name => "Test_RestoreMessage", Mode => Nominal);
      -- ****

      -- ****f* Messages/Messages.Get_Last_Message_Index
      -- FUNCTION
      -- Get last message index
      -- RESULT
      -- List index of the last message
      -- SOURCE
   function Get_Last_Message_Index return Natural with
      Test_Case => (Name => "Test_GetLastMessageIndex", Mode => Robustness);
      -- ****

end Messages;
