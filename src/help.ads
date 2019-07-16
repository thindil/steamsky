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
with Ada.Containers.Ordered_Maps; use Ada.Containers;
with DOM.Readers; use DOM.Readers;

package Help is

   -- Data structure for help topic
   type Help_Data is record
      Index: Unbounded_String; -- Index of help topic
      Text: Unbounded_String; -- Text of help
   end record;
   package Help_Container is new Ordered_Maps(Unbounded_String, Help_Data);
   -- List of all help topics
   Help_List: Help_Container.Map;

   -- Load help text from file
   procedure LoadHelp(Reader: Tree_Reader);

end Help;
