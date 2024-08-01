--    Copyright 2017-2024 Bartek thindil Jasicki
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

package body Ships.Cargo is

   --## rule off LOCAL_HIDING
   procedure Update_Cargo
     (Ship: in out Ship_Record; Proto_Index: Natural := 0; Amount: Integer;
      Durability: Items_Durability := Default_Item_Durability;
      Cargo_Index, Price: Natural := 0) is
      Nim_Cargo: Nim_Inventory_Array :=
        Inventory_To_Nim(Inventory => Ship.Cargo);
      Old_Amount: constant Count_Type :=
        Inventory_Container.Length(Container => Ship.Cargo);
      procedure Update_Ada_Cargo
        (P_Index, A, Dur, C_Index, P, Get_Player_Ship: Integer) with
         Import => True,
         Convention => C,
         External_Name => "updateAdaCargo";
   begin
      Get_Ada_Modules(Ship => Ship);
      Get_Ada_Ship_Cargo
        (Cargo => Nim_Cargo,
         Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      Update_Ada_Cargo
        (P_Index => Proto_Index, A => Amount, Dur => Durability,
         C_Index => Cargo_Index, P => Price,
         Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      Set_Ada_Ship_Cargo
        (Cargo => Nim_Cargo,
         Get_Player_Ship => (if Ship = Player_Ship then 1 else 0));
      Inventory_Container.Assign
        (Target => Ship.Cargo,
         Source => Inventory_From_Nim(Inventory => Nim_Cargo, Size => 128));
      if Old_Amount > Inventory_Container.Length(Container => Ship.Cargo) then
         Update_Ammo_Loop :
         for Module of Ship.Modules loop
            if Module.M_Type = GUN then
               Module.Ammo_Index := 0;
            end if;
         end loop Update_Ammo_Loop;
      end if;
   end Update_Cargo;
   --## rule on LOCAL_HIDING

end Ships.Cargo;
