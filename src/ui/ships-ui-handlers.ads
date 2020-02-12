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

with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gtkada.Builder; use Gtkada.Builder;

-- ****h* Steamsky/Ships.UI.Handlers
-- FUNCTION
-- Provided code for ship info UI handlers
-- SOURCE
package Ships.UI.Handlers is
-- ****

   -- ****f* Ships.UI.Handlers/ShowModuleInfo
   -- FUNCTION
   -- Show information about selected module
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowModuleInfo(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Ships.UI.Handlers/ChangeShipName
   -- FUNCTION
   -- Change name of player's ship
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ChangeShipName(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Ships.UI.Handlers/ChangeModuleName
   -- FUNCTION
   -- Change name of selected module
   -- PARAMETERS
   -- Self     - GtkTreeView cell with name of the module to change. Unused
   -- Path     - Full GtkTreeView path to selected module name
   -- New_Text - New name for selected module
   -- SOURCE
   procedure ChangeModuleName
     (Self: access Gtk_Cell_Renderer_Text_Record'Class; Path: UTF8_String;
      New_Text: UTF8_String);
   -- ****
   -- ****f* Ships.UI.Handlers/SetUpgrade
   -- FUNCTION
   -- Start upgrading selected module
   -- PARAMETERS
   -- User_Data - Button with upgrade type clicked
   -- SOURCE
   procedure SetUpgrade(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Ships.UI.Handlers/StopUpgrading
   -- FUNCTION
   -- Stop current module upgrade
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure StopUpgrading(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Ships.UI.Handlers/SetRepair
   -- FUNCTION
   -- Set repair priority for selected module
   -- PARAMETERS
   -- User_Data - Button clicked to set or remove repair priority
   -- SOURCE
   procedure SetRepair(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Ships.UI.Handlers/Assign
   -- FUNCTION
   -- Assign crew member or ammo to selected module
   -- PARAMETERS
   -- User_Data - Button clicked to assign member or ammo to module
   -- SOURCE
   procedure Assign(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Ships.UI.Handlers/DisableEngine
   -- FUNCTION
   -- Enable/disable selected engine
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure DisableEngine(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Ships.UI.Handlers/ToggleSearch
   -- FUNCTION
   -- Enable/disable search for ship module when start/end editing module name
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ToggleSearch(Object: access Gtkada_Builder_Record'Class);
   -- ****

end Ships.UI.Handlers;
