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

with Interfaces.C.Strings;
with Items;

package body Mobs is

   function Generate_Mob
     (Mob_Index: Positive; Faction_Index: Tiny_String.Bounded_String)
      return Member_Data is
      use Interfaces.C.Strings;
      use Items;
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Mob: Member_Data
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount);
      Nim_Mob: Nim_Member_Data;
      Nim_Inventory: Nim_Inventory_Array;
      procedure Ada_Generate_Mob
        (Index: Integer; F_Index: chars_ptr; N_Mob: out Nim_Member_Data;
         N_Inventory: out Nim_Inventory_Array) with
         Import => True,
         Convention => C,
         External_Name => "adaGenerateMob";
   begin
      Ada_Generate_Mob
        (Index => Mob_Index,
         F_Index => New_String(Str => To_String(Source => Faction_Index)),
         N_Mob => Nim_Mob, N_Inventory => Nim_Inventory);
      Member_From_Nim(Member => Nim_Mob, Ada_Member => Mob);
      Mob.Inventory :=
        Inventory_From_Nim(Inventory => Nim_Inventory, Size => 32);
      return Mob;
      --## rule on IMPROPER_INITIALIZATION
   end Generate_Mob;

   function Get_Proto_Mob(Index: Positive) return Proto_Mob_Record is
      --## rule off IMPROPER_INITIALIZATION
      Temp_Record: Proto_Mob_Record
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount);
      Temp_Skills: Skills_Container.Vector (Capacity => Skills_Amount);
      Temp_Inventory: MobInventory_Container.Vector (Capacity => 32);
      Temp_Priorities: constant Natural_Array(1 .. 12) := (others => 0);
      Temp_Equipment: constant Equipment_Array := (others => 0);
      --## rule off TYPE_INITIAL_VALUES
      type Nim_Proto_Attributes_Array is array(0 .. 5, 0 .. 1) of Integer;
      type Nim_Proto_Skills_Array is array(0 .. 5, 0 .. 2) of Integer;
      type Nim_Proto_Inventory_Array is array(0 .. 19, 0 .. 2) of Integer;
      type Nim_Proto_Mob is record
         Attributes: Nim_Proto_Attributes_Array;
         Skills: Nim_Proto_Skills_Array;
         Order: Integer;
         Priorities: Natural_Array(1 .. 12);
         Inventory: Nim_Proto_Inventory_Array;
         Equipment: Nim_Equipment_Array;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      Nim_Mob: Nim_Proto_Mob;
      procedure Get_Ada_Mob(M_Index: Integer; Ada_Mob: out Nim_Proto_Mob) with
         Import => True,
         Convention => C,
         External_Name => "getAdaMob";
   begin
      Get_Ada_Mob(M_Index => Index, Ada_Mob => Nim_Mob);
      Temp_Record :=
        (Amount_Of_Attributes => Attributes_Amount,
         Amount_Of_Skills => Skills_Amount, Skills => Temp_Skills,
         Attributes => (others => <>), Order => REST,
         Priorities => Temp_Priorities, Inventory => Temp_Inventory,
         Equipment => Temp_Equipment);
      --## rule on IMPROPER_INITIALIZATION
      if Nim_Mob.Attributes(0, 0) = 0 then
         return Temp_Record;
      end if;
      Load_Attributes_Loop :
      for J in 0 .. 5 loop
         exit Load_Attributes_Loop when Nim_Mob.Attributes(J, 0) = 0;
         Temp_Record.Attributes(J + 1) :=
           (Level => Nim_Mob.Attributes(J, 0),
            Experience => Nim_Mob.Attributes(J, 1));
      end loop Load_Attributes_Loop;
      Load_Skills_Loop :
      for J in 0 .. 5 loop
         exit Load_Skills_Loop when Nim_Mob.Skills(J, 0) = 0;
         Skills_Container.Append
           (Container => Temp_Record.Skills,
            New_Item =>
              (Index => Count_Type(Nim_Mob.Skills(J, 0)),
               Level => Nim_Mob.Skills(J, 1),
               Experience => Nim_Mob.Skills(J, 2)));
      end loop Load_Skills_Loop;
      Temp_Record.Order := Crew_Orders'Val(Nim_Mob.Order);
      Temp_Record.Priorities := Nim_Mob.Priorities;
      Load_Inventory_Loop :
      for J in 0 .. 19 loop
         exit Load_Inventory_Loop when Nim_Mob.Inventory(J, 0) = 0;
         MobInventory_Container.Append
           (Container => Temp_Record.Inventory,
            New_Item =>
              (Proto_Index => Nim_Mob.Inventory(J, 0),
               Min_Amount => Nim_Mob.Inventory(J, 1),
               Max_Amount => Nim_Mob.Inventory(J, 2)));
      end loop Load_Inventory_Loop;
      Load_Equipment_Loop :
      for J in 0 .. 6 loop
         Temp_Record.Equipment(Equipment_Locations'Val(J)) :=
           Nim_Mob.Equipment(J);
      end loop Load_Equipment_Loop;
      return Temp_Record;
   end Get_Proto_Mob;

end Mobs;
