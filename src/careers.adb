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

   procedure Load_Careers(File_Name: String) is
      use Interfaces.C;
      use Interfaces.C.Strings;

      --## rule off IMPROPER_INITIALIZATION
      Temp_Record: Career_Record;
      --## rule on IMPROPER_INITIALIZATION
      Skill_Name, Career_Index: Unbounded_String := Null_Unbounded_String;
      type Nim_Career_Array is array(0 .. 1) of chars_ptr;
      Temp_Nim_Career: Nim_Career_Array;
      Index: Positive := 1;
      Index2: Natural := 0;
      procedure Load_Ada_Careers(Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "loadAdaCareers";
      procedure Get_Ada_Career
        (C_Index: Integer; Ada_Career: out Nim_Career_Array) with
         Import => True,
         Convention => C,
         External_Name => "getAdaCareer";
      function Get_Ada_Career_Skill
        (C_Index: chars_ptr; Skill_Index: Integer) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaCareerSkill";
   begin
      Load_Ada_Careers(Name => New_String(Str => File_Name));
      Load_Careers_Data_Loop :
      loop
         Get_Ada_Career(C_Index => Index, Ada_Career => Temp_Nim_Career);
         exit Load_Careers_Data_Loop when Strlen(Item => Temp_Nim_Career(0)) =
           0;
         Career_Index :=
           To_Unbounded_String(Source => Value(Item => Temp_Nim_Career(0)));
         Temp_Record.Name :=
           To_Unbounded_String(Source => Value(Item => Temp_Nim_Career(1)));
         Index2 := 0;
         Temp_Record.Skills.Clear;
         Load_Skills_Loop :
         loop
            Skill_Name :=
              To_Unbounded_String
                (Source =>
                   (Interfaces.C.Strings.Value
                      (Item =>
                         Get_Ada_Career_Skill
                           (C_Index => Temp_Nim_Career(0),
                            Skill_Index => Index2))));
            exit Load_Skills_Loop when Length(Source => Skill_Name) = 0;
            Temp_Record.Skills.Append(New_Item => Skill_Name);
            Index2 := Index2 + 1;
         end loop Load_Skills_Loop;
         Careers_List.Include(Key => Career_Index, New_Item => Temp_Record);
         Index := Index + 1;
      end loop Load_Careers_Data_Loop;
   end Load_Careers;

end Careers;
