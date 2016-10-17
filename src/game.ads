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
with Ada.Containers.Vectors; use Ada.Containers;

package Game is

    type GameStates is (Quit, Main_Menu, Sky_Map_View, Control_Speed,
        Ship_Info, Crew_Info, Giving_Orders, Messages_View, Trade_View,
        Help_View, Quit_Confirm, New_Game, Combat_Confirm, Combat_State,
        Combat_Orders, Craft_View, License_Info, License_Full, Wait_Order,
        News_View, Cargo_Info, Help_Topic, Repairs_View, Clear_Confirm,
        Upgrade_Module, Shipyard_View, Recruits_View, Dismiss_Confirm); -- Game states
    type Date_Record is -- Data for game date/time
        record
            Year : Natural;
            Month : Natural;
            Day : Natural;
            Hour : Natural;
            Minutes : Natural;
        end record;
    GameDate : Date_Record;
    GameVersion : constant String := "Version: 0.5";
    package UnboundedString_Container is new Vectors(Positive, Unbounded_String);
    package Positive_Container is new Vectors(Positive, Positive);
    BaseSyllablesPre : UnboundedString_Container.Vector;
    BaseSyllablesStart : UnboundedString_Container.Vector;
    BaseSyllablesEnd : UnboundedString_Container.Vector;
    BaseSyllablesPost : UnboundedString_Container.Vector;
    MaleSyllablesStart : UnboundedString_Container.Vector;
    MaleSyllablesMiddle : UnboundedString_Container.Vector;
    MaleSyllablesEnd : UnboundedString_Container.Vector;
    FemaleSyllablesEnd : UnboundedString_Container.Vector;
    Skills_Names : UnboundedString_Container.Vector;
    
    procedure NewGame(CharName, ShipName : Unbounded_String; Gender : Character); -- Start new game: create map, place ship, crew, etc
    procedure UpdateGame(Minutes : Positive); -- Game ticks (update time, crew, ship, etc)
    procedure SaveGame; -- Save game to file
    function LoadGame return Boolean; -- Load game from file, return false if save can't be loaded
    function LoadData return Boolean; -- Load game data from file, return false if file not found

end Game;
