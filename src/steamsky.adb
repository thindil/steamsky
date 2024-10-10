--    Copyright 2016-2024 Bartek thindil Jasicki
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Game; use Game;

procedure Steamsky is

   Parameters: Unbounded_String := Null_Unbounded_String;

   function Update_Path
     (Path: in out Unbounded_String; Path_Name: String) return Boolean is
      use Ada.Text_IO;

   begin
      if Element(Source => Path, Index => Length(Source => Path)) /=
        Dir_Separator then
         Append(Source => Path, New_Item => Dir_Separator);
      end if;
      if not Exists(Name => To_String(Source => Path))
        and then Path_Name not in "Save" | "Modifications" | "Themes" then
         Put_Line
           (Item =>
              "Directory " & To_String(Source => Path) &
              " does not exist. You must use an existing directory as " &
              To_Lower(Item => Path_Name) & " directory.");
         return False;
      end if;
      return True;
   end Update_Path;

   procedure Nim_Main with
      Import => True,
      Convention => C,
      External_Name => "NimMain";

   procedure Steam_Sky(Params: chars_ptr) with
      Import => True,
      Convention => C,
      External_Name => "steamsky";
begin
   Nim_Main;
   Set_Directory(Directory => Dir_Name(Path => Command_Name));
   -- Command line arguments
   Command_Line_Loop :
   for I in 1 .. Argument_Count loop
      if Argument(Number => I)'Length > 8 then
         Append(Source => Parameters, New_Item => Argument(Number => I) & " ");
         if Argument(Number => I)(1 .. 8) = "--datadi" then
            Data_Directory :=
              To_Unbounded_String
                (Source =>
                   Argument(Number => I)(11 .. Argument(Number => I)'Last));
            if not Update_Path
                (Path => Data_Directory, Path_Name => "Data") then
               return;
            end if;
         elsif Argument(Number => I)(1 .. 8) = "--savedi" then
            Save_Directory :=
              To_Unbounded_String
                (Source =>
                   Argument(Number => I)(11 .. Argument(Number => I)'Last));
            if not Update_Path
                (Path => Save_Directory, Path_Name => "Save") then
               return;
            end if;
         elsif Argument(Number => I)(1 .. 8) = "--docdir" then
            Doc_Directory :=
              To_Unbounded_String
                (Source =>
                   Argument(Number => I)(10 .. Argument(Number => I)'Last));
            if not Update_Path
                (Path => Doc_Directory, Path_Name => "Documentation") then
               return;
            end if;
         elsif Argument(Number => I)(1 .. 8) = "--modsdi" then
            Mods_Directory :=
              To_Unbounded_String
                (Source =>
                   Argument(Number => I)(11 .. Argument(Number => I)'Last));
            if not Update_Path
                (Path => Mods_Directory, Path_Name => "Modifications") then
               return;
            end if;
         elsif Argument(Number => I)(1 .. 8) = "--themes" then
            Themes_Directory :=
              To_Unbounded_String
                (Source =>
                   Argument(Number => I)(13 .. Argument(Number => I)'Last));
            if not Update_Path
                (Path => Themes_Directory, Path_Name => "Themes") then
               return;
            end if;
         end if;
      end if;
   end loop Command_Line_Loop;

   Steam_Sky(Params => New_String(Str => To_String(Source => Parameters)));

end Steamsky;
