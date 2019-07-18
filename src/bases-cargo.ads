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

-- ****h* Steamsky/Bases.Cargo
-- FUNCTION
-- Provide code for manipulate cargo of sky bases
-- SOURCE
package Bases.Cargo is
-- ****

   -- ****f* Bases.Cargo/GenerateCargo;
   -- FUNCTION
   -- Generate base cargo
   -- SOURCE
   procedure GenerateCargo;
   -- ****
   -- ****f* Bases.Cargo/UpdateBaseCargo
   -- FUNCTION
   -- Update cargo in base
   -- PARAMETERS
   -- ProtoIndex - Index of item prototype. Can be empty if CargoIndex is set
   -- Amount     - Amount of item to add or remove
   -- Durability - Durability of item to add or remove. Can be empty
   -- CargoIndex - Index of item in sky base cargo. Can be empty if ProtoIndex
   --              is set
   -- SOURCE
   procedure UpdateBaseCargo
     (ProtoIndex: Unbounded_String := Null_Unbounded_String; Amount: Integer;
      Durability: Natural := 100; CargoIndex: Natural := 0);
   -- ****
   -- ****f* Bases.Cargo/FindBaseCargo
   -- FUNCTION
   -- Find index of item in base cargo
   -- PARAMETERS
   -- ProtoIndex - Index of prototype of item to search
   -- Durability - Durability of item to search. Can be empty
   -- RESULT
   -- Index of item in sky base cargo or 0 if item not found
   -- SOURCE
   function FindBaseCargo
     (ProtoIndex: Unbounded_String; Durability: Natural := 101) return Natural;
     -- ****

end Bases.Cargo;
