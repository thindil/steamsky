--    Copyright 2019-2020 Bartek thindil Jasicki
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
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps; use Ada.Containers;
with DOM.Readers; use DOM.Readers;
with Game; use Game;
with Items; use Items;

-- ****h* Steamsky/BasesTypes
-- FUNCTION
-- Provide code for bases types
-- SOURCE
package BasesTypes is
-- ****

   -- ****t* BasesTypes/Prices_Array
   -- FUNCTION
   -- Buy and sell prices for the item in selected base type
   -- SOURCE
   type Prices_Array is array(1 .. 2) of Natural;
   -- ****

   -- ****t* BasesTypes/BasesTrade_Container
   -- FUNCTION
   -- Used to store base buy and sell prices for items in selected base type
   -- SOURCE
   package BasesTrade_Container is new Hashed_Maps(Unbounded_String,
      Prices_Array, Ada.Strings.Unbounded.Hash, "=");
   -- ****

   -- ****t* BasesTypes/BaseType_Data
   -- FUNCTION
   -- Data structure for bases types
   -- PARAMETERS
   -- Name        - Name of base type, will be presented to the player
   -- Color       - Hexadecimal number of color used to show that base type on
   --               the map
   -- Trades      - List of base items prices for buy and sale in that base
   --               type
   -- Recipes     - List of available crafting recipes in that base type
   -- Flags       - Special flags for selected base type (like shipyard, etc)
   -- Description - Description of the base type. Will be presented to the
   --               player, for example in new game menu
   -- SOURCE
   type BaseType_Data is record
      Name: Unbounded_String;
      Color: String(1 .. 6);
      Trades: BasesTrade_Container.Map;
      Recipes: UnboundedString_Container.Vector;
      Flags: UnboundedString_Container.Vector;
      Description: Unbounded_String;
   end record;
   -- ****

   -- ****t* BasesTypes/BasesTypes_Container
   -- FUNCTION
   -- Used to store information about all available bases types
   -- SOURCE
   package BasesTypes_Container is new Hashed_Maps(Unbounded_String,
      BaseType_Data, Ada.Strings.Unbounded.Hash, "=");
   -- ****

   -- ****v* BasesTypes/BasesTypes_List
   -- FUNCTION
   -- List of all available bases types
   -- SOURCE
   BasesTypes_List: BasesTypes_Container.Map;
   -- ****

   -- ****f* BasesTypes/LoadBasesTypes
   -- FUNCTION
   -- Load bases types from file
   -- PARAMETERS
   -- Reader - XML Reader from which bases types will be read
   -- SOURCE
   procedure LoadBasesTypes(Reader: Tree_Reader);
   -- ****

   -- ****f* BasesTypes/Is_Buyable
   -- FUNCTION
   -- Check if selected item is buyable in selected base type
   -- PARAMETERS
   -- BaseType  - Base type to check
   -- ItemIndex - Index of item prototype to check
   -- CheckFlag - Check if selected base type has blackmarket flag
   -- BaseIndex - Index of the selected base to check. Default value
   --             is 0
   -- RESULT
   -- True if item is buyable in that type of bases otherwise false
   -- SOURCE
   function Is_Buyable
     (BaseType, ItemIndex: Unbounded_String; CheckFlag: Boolean := True;
      BaseIndex: Natural := 0) return Boolean with
      Pre => BasesTypes_List.Contains(BaseType) and
      Items_List.Contains(ItemIndex),
      Test_Case => ("Test_Is_Buyable", Nominal);
      -- ****

      -- ****f* BasesTypes/Get_Price
      -- FUNCTION
      -- Get price of selected item in selected base type
      -- PARAMETERS
      -- BaseType  - Base type to check
      -- ItemIndex - Index of item prototype to check
      -- RESULT
      -- Price of selected item in selected base type
      -- SOURCE
   function Get_Price
     (BaseType, ItemIndex: Unbounded_String) return Natural with
      Pre => BasesTypes_List.Contains(BaseType) and
      Items_List.Contains(ItemIndex),
      Test_Case => ("Test_Get_Price", Nominal);
      -- ****

end BasesTypes;
