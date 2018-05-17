--    Copyright 2017-2018 Bartek thindil Jasicki
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

with Ada.Text_IO; use Ada.Text_IO;
with DOM.Core; use DOM.Core;

package Game.SaveLoad is

   SaveGame_Invalid_Data: exception; -- Raised when invalid data found in savegame

   procedure SaveGame; -- Save game to file
   procedure LoadGame; -- Load game from file
   function ReadData
     (SaveGame: File_Type)
     return Unbounded_String; -- Read saved data from file
   procedure AddData
     (NodeName, Value: String;
      ParentNode: DOM.Core.Element); -- Add xml data to save to file

end Game.SaveLoad;
