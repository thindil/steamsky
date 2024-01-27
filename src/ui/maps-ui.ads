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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ships; use Ships;

-- ****h* Maps/MUI
-- FUNCTION
-- Provide code for showing and interacting with maps
-- SOURCE
package Maps.UI is
-- ****

   -- ****v* MUI/MUI.Default_Fonts_Sizes
   -- FUNCTION
   -- Default sizes of the game fonts
   -- SOURCE
   Default_Fonts_Sizes: array(1 .. 3) of Positive;
   -- ****

   -- ****f* MUI/MUI.Update_Header
   -- FUNCTION
   -- Update the game information on the UI header (time, crew, etc)
   -- SOURCE
   procedure Update_Header;
   -- ****

   -- ****f* MUI/MUI.Draw_Map
   -- FUNCTION
   -- Draw map on the screen
   -- SOURCE
   procedure Draw_Map with
      Import => True,
      Convention => C,
      External_Name => "drawAdaMap";
   -- ****

   -- ****f* MUI/MUI.Create_Game_Ui
   -- FUNCTION
   -- Create the game UI and show sky map to the player
   -- SOURCE
   procedure Create_Game_Ui;
   -- ****

   -- ****f* MUI/MUI.Show_Sky_Map
   -- FUNCTION
   -- Show sky map - draw map, update header, etc
   -- PARAMETERS
   -- Clear - If true (when back from other screen), remove old subwindow and
   -- add map
   -- SOURCE
   procedure Show_Sky_Map(Clear: Boolean := False);
   -- ****

   -- ****f* MUI/MUI.Set_Keys
   -- FUNCTION
   -- Set keyboard shortcuts
   -- SOURCE
   procedure Set_Keys;
   -- ****

   -- ****f* MUI/MUI.Finish_Story
   -- FUNCTION
   -- Finish the current story
   -- SOURCE
   procedure Finish_Story with
      Import => True,
      Convention => C,
      External_Name => "finishAdaStory";
   -- ****

-- Temporary code to interact with Nim

   function Get_General_Accelerator(Index: Positive) return String;
   procedure Set_General_Accelerator(Index: Positive; Value: String);
   function Get_Menu_Accelerator(Index: Positive) return String;
   procedure Set_Menu_Accelerator(Index: Positive; Value: String);
   function Get_Map_Accelerator(Index: Positive) return String;
   procedure Set_Map_Accelerator(Index: Positive; Value: String);
   function Get_Full_Screen_Accel return String;
   procedure Set_Full_Screen_Accel(Value: String);
   procedure Get_Center_Point(X, Y: out Positive) with
      Import => True,
      Convention => C,
      External_Name => "getAdaCenterPoint";
   procedure Set_Center_Point(X, Y: Positive) with
      Import => True,
      Convention => C,
      External_Name => "setAdaCenterPoint";

private

   -- ****f* MUI/MUI.Update_Map_Info_(private)
   -- FUNCTION
   -- Update information about map cell
   -- PARAMETERS
   -- X - X coordinate of the map cell
   -- Y - Y coordinate of the map cell
   -- SOURCE
   procedure Update_Map_Info
     (X: Positive := Player_Ship.Sky_X; Y: Positive := Player_Ship.Sky_Y);
   -- ****

   -- ****f* MUI/MUI.Update_Move_Buttons_(private)
   -- FUNCTION
   -- Updated the player ship movement buttons
   -- SOURCE
   procedure Update_Move_Buttons;
   -- ****

-- Temporary code to interact with Nim
   procedure Get_Start_Point(X, Y: out Integer) with
      Import => True,
      Convention => C,
      External_Name => "getAdaStartPoint";

end Maps.UI;
