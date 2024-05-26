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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Trades; use Trades;
with Maps; use Maps;

package body Bases.Ship is

   procedure Repair_Ship(Module_Index: Integer) is
      use Interfaces.C;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Result: chars_ptr;
      Ada_Result, Exception_Name: Unbounded_String := Null_Unbounded_String;
      Space_Index: Natural := 0;
      function Repair_Ada_Ship(M_Index: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "repairAdaShip2";
      Bases_Ship_Nothing_To_Repair: exception;
   begin
      Get_Base_Cargo(Base_Index => Base_Index);
      Get_Game_Date;
      Result := Repair_Ada_Ship(M_Index => Module_Index);
      if Strlen(Item => Result) > 0 then
         Ada_Result := To_Unbounded_String(Source => Value(Item => Result));
         Space_Index := Index(Source => Ada_Result, Pattern => " ");
         if Space_Index > 0 then
            Exception_Name :=
              Unbounded_Slice
                (Source => Ada_Result, Low => 1, High => Space_Index - 1);
         end if;
         if Exception_Name =
           To_Unbounded_String(Source => "NothingToRepairError") then
            raise Bases_Ship_Nothing_To_Repair;
         elsif Exception_Name =
           To_Unbounded_String(Source => "NotEnoughMoneyError") then
            raise Trade_Not_Enough_Money;
         end if;
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Base_Cargo(Base_Index => Base_Index);
      Set_Game_Date;
   end Repair_Ship;

   procedure Upgrade_Ship(Install: Boolean; Module_Index: Positive) is
      use Interfaces.C;

      Base_Index: constant Extended_Base_Range :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Result: chars_ptr;
      Ada_Result, Exception_Name: Unbounded_String := Null_Unbounded_String;
      Space_Index: Natural := 0;
      function Upgrade_Ada_Ship(I, M_Index: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "upgradeAdaShip2";
   begin
      Set_Ship_In_Nim;
      Get_Base_Cargo(Base_Index => Base_Index);
      Get_Game_Date;
      Result :=
        Upgrade_Ada_Ship
          (I => (if Install then 1 else 0), M_Index => Module_Index);
      if Strlen(Item => Result) > 0 then
         Ada_Result := To_Unbounded_String(Source => Value(Item => Result));
         Space_Index := Index(Source => Ada_Result, Pattern => " ");
         if Space_Index > 0 then
            Exception_Name :=
              Unbounded_Slice
                (Source => Ada_Result, Low => 1, High => Space_Index - 1);
         end if;
         if Exception_Name = To_Unbounded_String(Source => "NoMoneyError") then
            raise Trade_No_Money;
         elsif Exception_Name =
           To_Unbounded_String(Source => "NotEnoughMoneyError") then
            raise Trade_Not_Enough_Money;
         elsif Exception_Name =
           To_Unbounded_String(Source => "UniqueModuleError") then
            raise Bases_Ship_Unique_Module;
         elsif Exception_Name =
           To_Unbounded_String(Source => "InstallationError") then
            raise Bases_Ship_Installation_Error
              with Slice
                (Source => Ada_Result, Low => Space_Index + 1,
                 High => Length(Source => Ada_Result));
         elsif Exception_Name =
           To_Unbounded_String(Source => "NoFreeCargoError") then
            raise Trade_No_Free_Cargo;
         elsif Exception_Name =
           To_Unbounded_String(Source => "NoMoneyInBaseError") then
            raise Trade_No_Money_In_Base;
         elsif Exception_Name =
           To_Unbounded_String(Source => "RemovingError") then
            raise Bases_Ship_Removing_Error
              with Slice
                (Source => Ada_Result, Low => Space_Index + 1,
                 High => Length(Source => Ada_Result));
         end if;
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
      Set_Base_Cargo(Base_Index => Base_Index);
      Set_Game_Date;
   end Upgrade_Ship;

end Bases.Ship;
