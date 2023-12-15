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

package body Ships.Upgrade is

   procedure Start_Upgrading
     (Module_Index: Modules_Container.Extended_Index;
      Upgrade_Type: Positive) is
      use Interfaces.C;
      use Interfaces.C.Strings;

      Result: chars_ptr;
      Ship_Upgrade_Error: exception;
      function Start_Ada_Upgrading
        (M_Index, U_Type: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "startAdaUpgrading";
   begin
      Set_Ship_In_Nim;
      Result :=
        Start_Ada_Upgrading(M_Index => Module_Index, U_Type => Upgrade_Type);
      if Strlen(Item => Result) > 0 then
         raise Ship_Upgrade_Error with Value(Item => Result);
      end if;
      Get_Ship_From_Nim(Ship => Player_Ship);
   end Start_Upgrading;

end Ships.Upgrade;
