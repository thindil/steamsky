-- Copyright (c) 2020-2024 Bartek thindil Jasicki
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- ****h* Themes/Themes
-- FUNCTION
-- Provide code for read and set the game UI themes
-- SOURCE
package Themes is
-- ****

   -- ****t* Themes/Themes.FontTypes
   -- FUNCTION
   -- Types of font available in game
   -- SOURCE
   type Font_Types is (HELPFONT, MAPFONT, INTERFACEFONT, ALLFONTS);
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Themes/Themes.All_Fonts
   -- FUNCTION
   -- Default value for FontTypes, all fonts
   -- SOURCE
   All_Fonts: constant Font_Types := ALLFONTS;
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****f* Themes/Themes.Load_Themes
   -- FUNCTION
   -- Load data for all themes
   -- SOURCE
   procedure Load_Themes with
      Import => True,
      Convention => C,
      External_Name => "loadAdaThemes";
      -- ****

      -- ****f* Themes/Themes.Load_Theme_Images
      -- FUNCTION
      -- Load all images for the selected the game's theme
      -- HISTORY
      -- 7.1 - Added
      -- SOURCE
   procedure Load_Theme_Images with
      Import => True,
      Convention => C,
      External_Name => "loadAdaThemeImages";
      -- ****

-- Temporary code to interact with Nim

   function Get_Icon(Name: String) return String;

end Themes;
