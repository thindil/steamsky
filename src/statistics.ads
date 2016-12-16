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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Game; use Game;

package Statistics is

    type DestroyedShips_Data is -- Data for destroyed ships
        record
            ProtoIndex : Positive; -- Prototype index of destroyed ship
            Amount : Positive; -- Amount of destroyed that ships
        end record;
    package DestroyedShips_Container is new Vectors(Positive, DestroyedShips_Data);
    type GameStats_Data is -- Data for game statistics
        record
            DestroyedShips : DestroyedShips_Container.Vector; -- Data for all destroyed ships by player
            BasesVisited : Positive; -- Amount of visited bases
            MapVisited : Positive; -- Amount of visited map fields
        end record;
    GameStats : GameStats_Data; -- Game statistics

    procedure UpdateDestroyedShips(ShipName : Unbounded_String); -- Add new destroyed ship do list
    procedure ClearGameStats; -- Clear game statistics
    procedure ShowGameStats; -- Show game statistics
    function ShowGameStatsKeys(Key : Key_Code) return GameStates; -- Handle keys on game statistics screen

end Statistics;
