--    Copyright 2016-2023 Bartek thindil Jasicki
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
with Ada.Containers.Ordered_Maps; use Ada.Containers;

-- ****h* Help/Help
-- FUNCTION
-- Provide code for manipulate help system
-- SOURCE
package Help is
-- ****

   -- ****s* Help/Help.Help_Data
   -- FUNCTION
   -- Data structure for help topic
   -- PARAMETERS
   -- Index - Index of help topic
   -- Text  - Text of help
   -- SOURCE
   type Help_Data is record
      Index: Unbounded_String;
      Text: Unbounded_String;
   end record;
   -- ****

   -- ****d* Help/Help.Empty_Help
   -- FUNCTION
   -- Default value for Help_Data, an empty help entry
   -- SOURCE
   Empty_Help: constant Help_Data :=
     (Index => Null_Unbounded_String, Text => Null_Unbounded_String);
   -- ****

   -- ****t* Help/Help.Help_Container
   -- FUNCTION
   -- Used to store help data
   -- SOURCE
   package Help_Container is new Ordered_Maps
     (Key_Type => Unbounded_String, Element_Type => Help_Data);
   -- ****

   -- ****v* Help/Help.Help_List
   -- FUNCTION
   -- List of all help topics
   -- SOURCE
   Help_List: Help_Container.Map;
   -- ****

   -- ****f* Help/Help.Load_Help
   -- FUNCTION
   -- Load help text from file
   -- PARAMETERS
   -- File_Name - The full path to the factions file which will be read
   -- SOURCE
   procedure Load_Help(File_Name: String);
   -- ****

-- Temporary code to interact with Nim

   function Get_Help(Title: Unbounded_String) return Help_Data;

end Help;
