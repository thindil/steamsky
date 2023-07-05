--    Copyright 2019-2023 Bartek thindil Jasicki
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

with Game; use Game;

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

   -- ****f* BasesTypes/BasesTypes.Is_Buyable
   -- FUNCTION
   -- Check if selected item is buyable in selected base type
   -- PARAMETERS
   -- Base_Type  - Base type to check
   -- Item_Index - Index of item prototype to check
   -- Check_Flag - Check if selected base type has blackmarket flag
   -- Base_Index - Index of the selected base to check. Default value
   --              is 0
   -- RESULT
   -- True if item is buyable in that type of bases otherwise false
   -- SOURCE
   function Is_Buyable
     (Base_Type: Tiny_String.Bounded_String; Item_Index: Positive;
      Check_Flag: Boolean := True; Base_Index: Extended_Base_Range := 0)
      return Boolean;
      -- ****

      -- ****f* BasesTypes/BasesTypes.Get_Price
      -- FUNCTION
      -- Get price of selected item in selected base type
      -- PARAMETERS
      -- Base_Type  - Base type to check
      -- Item_Index - Index of item prototype to check
      -- RESULT
      -- Price of selected item in selected base type
      -- SOURCE
   function Get_Price
     (Base_Type: Tiny_String.Bounded_String; Item_Index: Positive)
      return Natural;
      -- ****

-- Temporary code to interact with Nim

   Bases_Types: array(0 .. 15) of Tiny_String.Bounded_String;

   function Has_Flag
     (Base_Type: Tiny_String.Bounded_String; Flag: String) return Boolean;

   function Get_Base_Type_Name
     (Base_Type: Tiny_String.Bounded_String) return String;

   function Has_Recipe
     (Base_Type: Tiny_String.Bounded_String; Recipe: String) return Boolean;

   function Get_Base_Type_Color
     (Base_Type: Tiny_String.Bounded_String) return String;

   function Get_Base_Type_Description
     (Base_Type: Tiny_String.Bounded_String) return String;

end BasesTypes;
