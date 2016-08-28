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

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Game; use Game;

package Bases is

    type Bases_Types is (Industrial, Agricultural, Refinery);
    type BaseRecord is -- Data structure for bases
        record
            Name : Unbounded_String; -- Base name
            Visited : Boolean; -- Did player visited base ealier
            SkyX : Integer; -- X coordinate on sky map
            SkyY : Integer; -- Y coordinate on sky map
            BaseType : Bases_Types; -- Type of base
        end record;
    SkyBases : array (1..1024) of BaseRecord; -- List of sky bases
    procedure BuyItems(ItemIndex : Positive; Amount : String); -- Buy items from bases
    procedure SellItems(ItemIndex : Positive; Amount : String); -- Sell items from bases
    function GenerateBaseName return Unbounded_String; -- Generate random name for base
    procedure ShowTrade(Key : Key_Code); -- Show trade window
    procedure ShowTrade2; -- Show trade window
    function TradeKeys(Key : Key_Code) return GameStates; -- Handle keys in bases trades

end Bases;
