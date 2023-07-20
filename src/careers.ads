--    Copyright 2018-2023 Bartek thindil Jasicki
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

-- ****h* Careers/Careers
-- FUNCTION
-- Provide code for characters careers
-- SOURCE
package Careers is
-- ****

   --## rule off TYPE_INITIAL_VALUES
   -- ****s* Careers/Careers.Career_Record
   -- FUNCTION
   -- Data structure for player career
   -- PARAMETERS
   -- Name   - Name of career, displayed to player
   -- Skills - List of skills which have bonuses to experience if player
   --          select this career
   -- SOURCE
   type Career_Record is record
      Name: Unbounded_String;
      Skills: UnboundedString_Container.Vector;
   end record;
   -- ****
   --## rule on TYPE_INITIAL_VALUES

   -- ****f* Careers/Careers.Load_Careers
   -- FUNCTION
   -- Load player careers from file
   -- SOURCE
   procedure Load_Careers;
   -- ****

-- Temporary code to interact with Nim

   function Get_Career(Career_Index: String) return Career_Record;

end Careers;
