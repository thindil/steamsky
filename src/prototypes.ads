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

package Prototypes is

    type Items_Types is (Fuel, Food, Drink, Ammo); -- Types of items
    type Object_Prices is array(1..3) of Natural; -- Prices of item in bases
    type Object_Buyable is array(1..3) of Boolean; -- Did item is buyable in bases
    type Object_Data is -- Data structure for objects prototypes
        record
            Name : Unbounded_String; -- Name of item
            Weight : Positive; -- Weight of item
            IType : Items_Types; -- Type of item
            Prices : Object_Prices; -- Prices of item in bases
            Buyable : Object_Buyable; -- Did item is buyable in selected bases
        end record;
    Objects_Prototypes : constant array(1..4) of Object_Data := ((Name =>
        To_Unbounded_String("Charcollum"), Weight => 1, IType => Fuel, Prices =>
        (0, 0, 0), Buyable => (False, False, False)), (Name => To_Unbounded_String("Basic rations"), 
        Weight => 1, IType => Food, Prices => (2, 1, 2), Buyable => (False, True,
        False)), (Name => To_Unbounded_String("Water"), Weight => 1, IType =>
        Drink, Prices => (2, 1, 2), Buyable => (False, True, False)), (Name =>
        To_Unbounded_String("20mm ammo"), Weight => 1, IType => Ammo, Prices => (3,
        5, 5), Buyable => (True, False, False)));

end Prototypes;
