--    Copyright 2019-2024 Bartek thindil Jasicki
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

with Game; use Game; --## rule line off REDUCEABLE_SCOPE

-- ****h* BasesTypes/BasesTypes
-- FUNCTION
-- Provide code for bases types
-- SOURCE
package BasesTypes is
-- ****

   -- ****f* BasesTypes/BasesTypes.Load_Bases_Types
   -- FUNCTION
   -- Load bases types from file
   -- SOURCE
   procedure Load_Bases_Types;
   -- ****

-- Temporary code to interact with Nim

   function Get_Base_Type_Name
     (Base_Type: Tiny_String.Bounded_String) return String;

end BasesTypes;
