--    Copyright 2018-2019 Bartek thindil Jasicki
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

with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DOM.Readers; use DOM.Readers;
with Game; use Game;

-- ****h* Steamsky/Careers
-- FUNCTION
-- Provide code for characters careers
-- SOURCE
package Careers is
-- ****

   -- ****t* Careers/CareerRecord
   -- FUNCTION
   -- Data structure for player career
   -- PARAMETERS
   -- Name   - Name of career, displayed to player
   -- Skills - List of skills which have bonuses to experience if player
   --          select this career
   -- SOURCE
   type CareerRecord is record
      Name: Unbounded_String;
      Skills: UnboundedString_Container.Vector;
   end record;
   -- ****

   -- ****t* Careers/Careers_Container
   -- FUNCTION
   -- Used to store all available careers
   -- SOURCE
   package Careers_Container is new Hashed_Maps(Unbounded_String, CareerRecord,
      Ada.Strings.Unbounded.Hash, "=");
   -- ****

   -- ****v* Careers/Careers_List
   -- FUNCTION
   -- List of all available careers for player
   -- SOURCE
   Careers_List: Careers_Container.Map;
   -- ****

   -- ****f* Careers/LoadCareers
   -- FUNCTION
   -- Load player careers from file
   -- PARAMETERS
   -- Reader - XML Reader from which careers will be read
   -- SOURCE
   procedure LoadCareers(Reader: Tree_Reader);
   -- ****

end Careers;
