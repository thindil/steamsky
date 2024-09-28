--    Copyright 2016-2024 Bartek thindil Jasicki
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

      -- ****d* Messages/Messages.Default_Message_Type
      -- FUNCTION
      -- The default type of the in-game messages
      -- SOURCE
   Default_Message_Type: constant Message_Type := DEFAULT;
   -- ****

   -- ****t* Messages/Messages.Message_Color
   -- FUNCTION
   -- Colors of messages
   -- SOURCE
   type Message_Color is (WHITE, YELLOW, GREEN, RED, BLUE, CYAN) with
      Default_Value => WHITE;
      -- ****

      -- ****d* Messages/Messages.Default_Message_Color
      -- FUNCTION
      -- The default color of the in-game messages
      -- SOURCE
   Default_Message_Color: constant Message_Color := WHITE;
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

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Messages/Messages.Empty_Message
   -- FUNCTION
   -- The empty in-game message
   -- SOURCE
   Empty_Message: constant Message_Data :=
     (Message => Null_Unbounded_String, M_Type => Default_Message_Type,
      Color => Default_Message_Color);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****f* Messages/Messages.Formated_Time
   -- FUNCTION
   -- Format game time
   -- PARAMETERS
   -- Time - Game time to format. Default is current game time
   -- RESULT
   -- Formatted in YYYY-MM-DD HH:MM style in game time
   -- SOURCE
   function Formated_Time(Time: Date_Record := Game_Date) return String with
      Post => Formated_Time'Result'Length > 0;
      -- ****

      -- ****f* Messages/Messages.Get_Last_Message_Index
      -- FUNCTION
      -- Get last message index
      -- RESULT
      -- List index of the last message
      -- SOURCE
   function Get_Last_Message_Index return Natural;
   -- ****

end Messages;
