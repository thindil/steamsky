--    Copyright 2017-2019 Bartek thindil Jasicki
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

package Bases.Cargo is

   procedure GenerateCargo; -- Generate base cargo
   procedure UpdateBaseCargo
     (ProtoIndex: Unbounded_String := Null_Unbounded_String; Amount: Integer;
      Durability: Natural := 100;
      CargoIndex: Natural := 0); -- Update cargo in base
   function FindBaseCargo
     (ProtoIndex: Unbounded_String; Durability: Natural := 101)
      return Natural; -- Find index of item in base cargo, return 0 if no item found
end Bases.Cargo;
