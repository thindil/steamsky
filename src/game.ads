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

package Game is

    type GameStates is (Quit, Main_Menu, Sky_Map_View, Control_Speed,
        Ship_Info, Crew_Info, Giving_Orders); -- Game states
    type Time_Record is -- Data for game time
        record
            Hour : Natural;
            Minutes : Natural;
            Seconds : Natural;
        end record;
    type Date_Record is -- Data for game date
        record
            Year : Natural;
            Month : Natural;
            Day : Natural;
        end record;
    GameTime : Time_Record;
    GameDate : Date_Record;
    
    procedure NewGame; -- Start new game: create map, place ship, crew, etc

end Game;
