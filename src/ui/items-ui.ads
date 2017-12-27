--    Copyright 2017 Bartek thindil Jasicki
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

with Terminal_Interface.Curses; use Terminal_Interface.Curses;

package Items.UI is

   procedure ShowItemStatus
     (Inventory: Inventory_Container.Vector;
      ItemIndex: Positive;
      InfoWindow: Window;
      Line: Line_Position); -- show damage level of item in character inventory or ship cargo
   function ShowItemInfo
     (Inventory: Inventory_Container.Vector;
      ItemIndex: Positive)
     return Line_Position; --Show informations about selected item
   function GetStatusLength
     (Inventory: Inventory_Container.Vector;
      ItemIndex: Positive)
     return Column_Position; -- Return minimal length of window needed for show item status

end Items.UI;
