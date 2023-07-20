--    Copyright 2018-2022 Bartek thindil Jasicki
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

package body Careers is

   function Get_Career(Career_Index: String) return Career_Record is
      use Interfaces.C.Strings;
      --## rule off IMPROPER_INITIALIZATION
      Temp_Record: Career_Record;
      --## rule on IMPROPER_INITIALIZATION
      --## rule off TYPE_INITIAL_VALUES
      type Nim_Career_Array is array(0 .. 1) of chars_ptr;
      --## rule on TYPE_INITIAL_VALUES
      Temp_Nim_Career: Nim_Career_Array;
      Index2: Natural := 0;
      Skill_Name: Unbounded_String := Null_Unbounded_String;
      function Get_Ada_Career_Skill
        (C_Index: chars_ptr; Skill_Index: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaCareerSkill";
      function Get_Ada_Career_Name(C_Index: chars_ptr) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaCareerName";
   begin
      Temp_Record.Name :=
        To_Unbounded_String
          (Source =>
             Value
               (Item =>
                  Get_Ada_Career_Name
                    (C_Index => New_String(Str => Career_Index))));
      Load_Skills_Loop :
      loop
         Skill_Name :=
           To_Unbounded_String
             (Source =>
                Interfaces.C.Strings.Value
                  (Item =>
                     Get_Ada_Career_Skill
                       (C_Index => Temp_Nim_Career(0),
                        Skill_Index => Index2)));
         exit Load_Skills_Loop when Length(Source => Skill_Name) = 0;
         Temp_Record.Skills.Append(New_Item => Skill_Name);
         Index2 := Index2 + 1;
      end loop Load_Skills_Loop;
      return Temp_Record;
   end Get_Career;

end Careers;
