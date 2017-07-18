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

package body Utils.UI is

   procedure WindowFrame(Win: Window; Color: Color_Pair; Caption: String) is
   begin
      Set_Color(Win, Color);
      Box(Win);
      Move_Cursor(Win => Win, Line => 0, Column => 2);
      if Caption'Length > 0 then
         Add(Win => Win, Str => "[");
         Set_Color(Win, Color_Pair'First);
         Add(Win => Win, Str => Caption);
         Set_Color(Win, Color);
         Add(Win => Win, Str => "]");
      end if;
      Set_Color(Win, Color_Pair'First);
   end WindowFrame;

end Utils.UI;
