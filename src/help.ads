--    Copyright 2016-2017 Bartek thindil Jasicki
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

package Help is

   type Help_Data is -- Data structure for help topic
   record
      Title: Unbounded_String; -- Title of help topic
      Text: Unbounded_String; -- Text of help
   end record;
   package Help_Container is new Vectors(Positive, Help_Data);
   Help_List: Help_Container.Vector; -- List of all help topics

   function LoadHelp
     return Boolean; -- Load help text from file, returns False if file not found

end Help;
