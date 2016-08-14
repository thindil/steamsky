--    Copyright 2016 Bartek thindil Jasicki
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

package Game is

    type GameStates is (Quit, Main_Menu, Sky_Map_View, Control_Speed,
        Ship_Info, Crew_Info, Giving_Orders, Messages_View, Trade_View,
        Help_View, Quit_Confirm, New_Game, Combat_Confirm, Combat_State,
        Combat_Orders, Craft_View, License_Info, License_Full, Wait_Order); -- Game states
    type Date_Record is -- Data for game date/time
        record
            Year : Natural;
            Month : Natural;
            Day : Natural;
            Hour : Natural;
            Minutes : Natural;
        end record;
    GameDate : Date_Record;
    
    procedure NewGame(CharName, ShipName : Unbounded_String); -- Start new game: create map, place ship, crew, etc
    procedure UpdateGame(Minutes : Positive); -- Game ticks (update time, crew, ship, etc)
    procedure SaveGame; -- Save game to file
    function LoadGame return Boolean; -- Load game from file, return false if save can't be loaded

end Game;
