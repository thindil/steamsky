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

package Ships.UI.Handlers is

   -- Show informations about selected module
   procedure ShowModuleInfo(Object: access Gtkada_Builder_Record'Class);
   -- Change name of player's ship
   procedure ChangeShipName(Object: access Gtkada_Builder_Record'Class);
   -- Change name of selected module
   procedure ChangeModuleName
     (Self: access Gtk_Cell_Renderer_Text_Record'Class; Path: UTF8_String;
      New_Text: UTF8_String);
   -- Start upgrading selected module
   procedure SetUpgrade(User_Data: access GObject_Record'Class);
   -- Stop current module upgrade
   procedure StopUpgrading(Object: access Gtkada_Builder_Record'Class);
   -- Set repair priority for selected module
   procedure SetRepair(User_Data: access GObject_Record'Class);
   -- Assign crew member or ammo to selected module
   procedure Assign(User_Data: access GObject_Record'Class);
   -- Enable/disable selected engine
   procedure DisableEngine(Object: access Gtkada_Builder_Record'Class);
   -- Enable/disable search for ship module when start/end editing module name
   procedure ToggleSearch(Object: access Gtkada_Builder_Record'Class);

end Ships.UI.Handlers;
