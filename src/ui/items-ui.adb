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

package body Items.UI is

   procedure ShowItemStatus
     (Inventory: Inventory_Container.Vector;
      ItemIndex: Positive;
      InfoWindow: Window;
      Line: Line_Position) is
      DamagePercent: constant Natural :=
        100 -
        Natural((Float(Inventory(ItemIndex).Durability) / 100.0) * 100.0);
      TextLength: Positive;
      TextColor: Color_Pair;
   begin
      if DamagePercent > 0 and DamagePercent < 20 then
         Add(Win => InfoWindow, Str => "Slightly used");
         TextLength := 13;
         TextColor := 2;
      elsif DamagePercent > 19 and DamagePercent < 50 then
         Add(Win => InfoWindow, Str => "Damaged");
         TextLength := 7;
         TextColor := 1;
      elsif DamagePercent > 49 and DamagePercent < 80 then
         Add(Win => InfoWindow, Str => "Heavily damaged");
         TextLength := 15;
         TextColor := 3;
      elsif DamagePercent > 79 and DamagePercent < 100 then
         Add(Win => InfoWindow, Str => "Almost destroyed");
         TextLength := 16;
         TextColor := 4;
      end if;
      Change_Attributes
        (Win => InfoWindow,
         Line => Line,
         Column => 8,
         Count => TextLength,
         Color => TextColor);
   end ShowItemStatus;

end Items.UI;
