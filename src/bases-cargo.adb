--    Copyright 2017-2023 Bartek thindil Jasicki
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

with Maps; use Maps;

package body Bases.Cargo is

   procedure Generate_Cargo is
      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      procedure Generate_Ada_Cargo with
         Import => True,
         Convention => C,
         External_Name => "generateAdaCargo";
   begin
      Get_Ada_Base_Date
        (Base_Index => Base_Index, Year => Sky_Bases(Base_Index).Visited.Year,
         Month => Sky_Bases(Base_Index).Visited.Month,
         Day => Sky_Bases(Base_Index).Visited.Day,
         Hour => Sky_Bases(Base_Index).Visited.Hour,
         Minutes => Sky_Bases(Base_Index).Visited.Minutes, Date_Type => 0);
      Get_Base_Cargo(Base_Index => Base_Index);
      Get_Ada_Base_Population
        (Base_Index => Base_Index,
         Population => Sky_Bases(Base_Index).Population);
      Get_Base_Reputation(Base_Index => Base_Index);
      Get_Base_Type
        (Base_Index => Base_Index,
         Base_Type => Sky_Bases(Base_Index).Base_Type);
      Generate_Ada_Cargo;
      Set_Base_Cargo(Base_Index => Base_Index);
   end Generate_Cargo;

   procedure Update_Base_Cargo
     (Proto_Index: Natural := 0; Amount: Integer;
      Durability: Items_Durability := Default_Item_Durability;
      Cargo_Index: Inventory_Container.Extended_Index := 0) is
      Base_Index: constant Bases_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      procedure Update_Ada_Base_Cargo
        (P_Index: Natural; A, D, C_Index: Integer) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaBaseCargo";
   begin
      Set_Ship_In_Nim;
      Get_Base_Cargo(Base_Index => Base_Index);
      Update_Ada_Base_Cargo
        (P_Index => Proto_Index, A => Amount, D => Durability,
         C_Index => Cargo_Index);
      Set_Base_Cargo(Base_Index => Base_Index);
   end Update_Base_Cargo;

   function Find_Base_Cargo
     (Proto_Index: Natural;
      Durability: Items_Durability := Items_Durability'Last) return Natural is
      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      function Find_Ada_Base_Cargo
        (P_Index: Natural; D: Integer) return Integer with
         Import => True,
         Convention => C,
         External_Name => "findAdaBaseCargo";
   begin
      Set_Ship_In_Nim;
      Get_Base_Cargo(Base_Index => Base_Index);
      return Find_Ada_Base_Cargo(P_Index => Proto_Index, D => Durability);
   end Find_Base_Cargo;

end Bases.Cargo;
