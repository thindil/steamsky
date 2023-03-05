--    Copyright 2016-2023 Bartek thindil Jasicki
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

with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Interfaces.C.Strings;

package body ShipModules is

   procedure Load_Ship_Modules(File_Name: String) is
      use Interfaces.C;
      use Interfaces.C.Strings;

      Result: chars_ptr; --## rule line off IMPROPER_INITIALIZATION
      function Load_Ada_Items(Name: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "loadAdaModules";
   begin
      Result := Load_Ada_Items(Name => New_String(Str => File_Name));
      if Strlen(Item => Result) > 0 then
         raise Data_Loading_Error with Value(Item => Result);
      end if;
   end Load_Ship_Modules;

   function Get_Module_Type(Module_Index: Positive) return String is
      use Ada.Characters.Handling;
      use Ada.Strings.Maps;
      use Ada.Strings.Unbounded;

      Module_Type_Name: Unbounded_String :=
        To_Unbounded_String
          (Source =>
             To_Lower
               (Item =>
                  Module_Type'Image
                    (Get_Module(Index => Module_Index).M_Type)));
   begin
      Replace_Element
        (Source => Module_Type_Name, Index => 1,
         By =>
           To_Upper
             (Item =>
                Ada.Strings.Unbounded.Element
                  (Source => Module_Type_Name, Index => 1)));
      Translate
        (Source => Module_Type_Name,
         Mapping => To_Mapping(From => "_", To => " "));
      return To_String(Source => Module_Type_Name);
   end Get_Module_Type;

   function Get_Module(Index: Positive) return Base_Module_Data is
      use Ada.Containers;
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Tiny_String;

      Temp_Record: Base_Module_Data;
      --## rule off TYPE_INITIAL_VALUES
      type Nim_Module_Data is record
         Name: chars_ptr;
         Mtype: Integer;
         Weight: Integer;
         Value: Integer;
         Max_Value: Integer;
         Durability: Integer;
         Repair_Material: chars_ptr;
         Repair_Skill: Integer;
         Price: Integer;
         Install_Time: Integer;
         Unique: Integer;
         Size: Integer;
         Description: chars_ptr;
         Max_Owners: Integer;
         Speed: Integer;
         Reputation: Integer;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      Temp_Nim_Module: Nim_Module_Data;
      procedure Get_Ada_Module
        (I: Integer; Ada_Module: out Nim_Module_Data) with
         Import => True,
         Convention => C,
         External_Name => "getAdaModule";
   begin
      Get_Ada_Module(I => Index, Ada_Module => Temp_Nim_Module);
      Temp_Record :=
        (Name =>
           To_Bounded_String
             (Source => Strings.Value(Item => Temp_Nim_Module.Name)),
         M_Type => Module_Type'Val(Temp_Nim_Module.Mtype),
         Weight => Temp_Nim_Module.Weight, Value => Temp_Nim_Module.Value,
         Max_Value => Temp_Nim_Module.Max_Value,
         Durability => Temp_Nim_Module.Durability,
         Repair_Material =>
           To_Bounded_String
             (Source =>
                Strings.Value(Item => Temp_Nim_Module.Repair_Material)),
         Repair_Skill => Count_Type(Temp_Nim_Module.Repair_Skill),
         Price => Temp_Nim_Module.Price,
         Install_Time => Temp_Nim_Module.Install_Time,
         Unique => (if Temp_Nim_Module.Unique = 1 then True else False),
         Size => Temp_Nim_Module.Size,
         Description =>
           Short_String.To_Bounded_String
             (Source => Strings.Value(Item => Temp_Nim_Module.Description)),
         Max_Owners => Temp_Nim_Module.Max_Owners,
         Speed => Temp_Nim_Module.Speed,
         Reputation => Temp_Nim_Module.Reputation);
      return Temp_Record;
   end Get_Module;

end ShipModules;
