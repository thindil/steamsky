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

    type Object_Data is -- Data structure for objects prototypes
        record
            Name : Unbounded_String; -- Name of item
            Weight : Positive; -- Weight of item
        end record;
    Objects_Prototypes : constant array(1..4) of Object_Data := ((Name =>
    To_Unbounded_String("Charcollum"), Weight => 1), (Name =>
    To_Unbounded_String("Basic rations"), Weight => 1), (Name =>
    To_Unbounded_String("Water"), Weight => 1), (Name =>
    To_Unbounded_String("20mm ammo"), Weight => 1));

end Prototypes;
