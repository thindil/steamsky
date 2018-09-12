--    Copyright 2018 Bartek thindil Jasicki
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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DOM.Readers; use DOM.Readers;
with Game; use Game;

package Careers is

   type CareerRecord is -- Data structure for player career
   record
      Index: Unbounded_String; -- Index of career, used in code
      Name: Unbounded_String; -- Name of career, displayed to player
      Skills: UnboundedString_Container
        .Vector; -- List of skills which have bonuses to experience if player select this career
   end record;
   package Careers_Container is new Vectors(Positive, CareerRecord);
   Careers_List: Careers_Container
     .Vector; -- List of all available careers for player

   procedure LoadCareers(Reader: Tree_Reader); -- Load player careers from file

end Careers;
