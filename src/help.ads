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

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Help/Help.Empty_Help
   -- FUNCTION
   -- Default value for Help_Data, an empty help entry
   -- SOURCE
   Empty_Help: constant Help_Data :=
     (Index => Null_Unbounded_String, Text => Null_Unbounded_String);
   -- ****
   --## rule on REDUCEABLE_SCOPE

end Help;
