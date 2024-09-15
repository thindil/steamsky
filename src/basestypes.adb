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

--## rule off REDUCEABLE_SCOPE
with Interfaces.C.Strings; use Interfaces.C.Strings;
--## rule on REDUCEABLE_SCOPE

package body BasesTypes is

   procedure Load_Bases_Types is
   begin
      null;
   end Load_Bases_Types;

   function Get_Base_Type_Name
     (Base_Type: Tiny_String.Bounded_String) return String is
      function Get_Ada_Base_Type_Name(B_Type: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaBaseTypeName";
   begin
      return
        Value
          (Item =>
             Get_Ada_Base_Type_Name
               (B_Type =>
                  New_String
                    (Str => Tiny_String.To_String(Source => Base_Type))));
   end Get_Base_Type_Name;

end BasesTypes;
