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

package Bases is

    type Bases_Types is (Industrial, Agricultural, Refinery);
    type GoodData is -- Data structure for goods in bases
        record
            Name : Unbounded_String; -- Name of good
            Weight : Positive; -- Weight of one unit of good
            Price : Positive; -- Selling price of good
            Buyable : Boolean; -- Did this item is buyable on this base
        end record;
    type Goods_Array is array(1..3) of GoodData;
    type BaseRecord is -- Data structure for bases
        record
            Name : Unbounded_String; -- Base name
            Visited : Boolean; -- Did player visited base ealier
            SkyX : Integer; -- X coordinate on sky map
            SkyY : Integer; -- Y coordinate on sky map
            BaseType : Bases_Types; -- Type of base
            Goods: Goods_Array; -- List of goods for sale in base
        end record;
    SkyBases : array (1..1024) of BaseRecord; -- List of sky bases
    procedure BuyItems(ItemIndex : Positive; Amount : String); -- Buy items from bases
    procedure SellItems(ItemIndex : Positive; Amount : String); -- Sell items from bases

end Bases;
