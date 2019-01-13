--    Copyright 2018-2019 Bartek thindil Jasicki
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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Themes is

   type FontTypes is
     (HELPFONT, MAPFONT, INTERFACEFONT,
      ALLFONTS); -- Types of font available in game
   type ThemeRecord is -- Data structure for themes settings
   record
      Name: Unbounded_String; -- Name of theme
      FileName: Unbounded_String; -- Name of .css file of theme
   end record;
   package Themes_Container is new Ada.Containers.Indefinite_Hashed_Maps
     (String, ThemeRecord, Ada.Strings.Hash, "=");
   Themes_List: Themes_Container.Map; -- List of all available themes

   procedure SetFontSize
     (FontType: FontTypes); -- Set size of selected font (or all if FontName is empty string)
   procedure LoadTheme; -- Load selected UI theme
   procedure ResetFontsSizes; -- Reset size of fonts to theme default values
   procedure LoadThemes; -- Load data for all themes

end Themes;
