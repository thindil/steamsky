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
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Strings.Unbounded.Hash;
with DOM.Readers; use DOM.Readers;

package Help is

   type Help_Data is -- Data structure for help topic
   record
      Title: Unbounded_String; -- Title of help topic
      Text: Unbounded_String; -- Text of help
   end record;
   package Help_Container is new Hashed_Maps(Unbounded_String, Help_Data,
      Ada.Strings.Unbounded.Hash, "=");
   Help_List: Help_Container.Map; -- List of all help topics

   procedure LoadHelp(Reader: Tree_Reader); -- Load help text from file

end Help;
