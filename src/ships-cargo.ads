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

package Ships.Cargo is
    
    procedure UpdateCargo(Ship : in out ShipRecord; ProtoIndex : Positive; Amount : Integer); -- Update selected item in ship cargo
    function FreeCargo(Amount : Integer) return Integer; -- Return available space in cargo after adding/extracting Amount
    function FindMoney return Natural; -- Return index of moneys, 0 if no moneys on ship
    function GetCargoName(CargoIndex : Positive) return String; -- Get name of cargo

end Ships.Cargo;
