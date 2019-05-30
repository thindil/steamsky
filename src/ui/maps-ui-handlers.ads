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

with Glib.Object; use Glib.Object;
with Gdk; use Gdk;
with Gdk.Event; use Gdk.Event;

package Maps.UI.Handlers is

   procedure QuitGameMenu
     (Object: access Gtkada_Builder_Record'Class); -- Quit from game
   procedure HideMapInfoWindow
     (User_Data: access GObject_Record'Class); -- Hide selected window
   procedure GetMapSize
     (Object: access Gtkada_Builder_Record'Class); -- Get size of map cell
   function SetDestination
     (Object: access Gtkada_Builder_Record'Class)
      return Boolean; -- Set ship destination
   procedure MoveMap
     (User_Data: access GObject_Record'Class); -- Move map to selected position
   procedure BtnDockClicked
     (Object: access Gtkada_Builder_Record'
        Class); -- Dock/undock ship from base
   procedure ChangeSpeed
     (Object: access Gtkada_Builder_Record'Class); -- Change current ship speed
   procedure MoveShip
     (User_Data: access GObject_Record'
        Class); -- Move ship in selected direction
   procedure ShowOrders
     (Object: access Gtkada_Builder_Record'
        Class); -- Show available ship orders
   procedure WaitOrder
     (User_Data: access GObject_Record'Class); -- Execute selected wait order
   procedure AttackOrder
     (Object: access Gtkada_Builder_Record'
        Class); -- Execute attack or show wait order
   procedure ShowHelp
     (Object: access Gtkada_Builder_Record'
        Class); -- Show help for selected topic
   procedure ShowInfo
     (User_Data: access GObject_Record'
        Class); -- Show selected info (ship/crew/cargo/etc)
   procedure ResignFromGame
     (Object: access Gtkada_Builder_Record'
        Class); -- Show confirmation for resign from game
   procedure ShowMissions
     (Object: access Gtkada_Builder_Record'
        Class); -- Show available missions in base
   procedure StartMission
     (Object: access Gtkada_Builder_Record'
        Class); -- Start mission if ship is in cell with it
   procedure CompleteMission
     (Object: access Gtkada_Builder_Record'
        Class); -- Finish mission if ship is at sky base
   procedure ExecuteOrder
     (User_Data: access GObject_Record'
        Class); -- Set home base/ask for bases/ask for events
   procedure DeliverMedicines
     (User_Data: access GObject_Record'Class); -- Deliver medicines to base
   procedure ShowWaitOrders
     (Object: access Gtkada_Builder_Record'
        Class); -- Show available wait orders
   function UpdateTooltip
     (Object: access Gtkada_Builder_Record'Class)
      return Boolean; -- Update map tooltip with information about cell on mouse movement
   function MapKeyReleased
     (Self: access Gtk_Widget_Record'Class; Event: Gdk.Event.Gdk_Event_Key)
      return Boolean; -- Center map on player ship
   function MapKeyPressed
     (Self: access Gtk_Widget_Record'Class; Event: Gdk.Event.Gdk_Event_Key)
      return Boolean; -- Move mouse cursor on map by keyboard
   function ZoomMap
     (Self: access Gtk_Widget_Record'Class; Event: Gdk.Event.Gdk_Event_Scroll)
      return Boolean; -- Resize sky map font with mouse wheel
   function DisableMenuShortcuts
     (Object: access Gtkada_Builder_Record'Class)
      return Boolean; -- Disable menu shortcuts keys
   function EnableMenuShortcuts
     (Object: access Gtkada_Builder_Record'Class)
      return Boolean; -- Enable menu shortcuts keys
   procedure DisableMenuShortcutsProc
     (Object: access Gtkada_Builder_Record'
        Class); -- Disable menu shortcuts keys
   procedure EnableMenuShortcutsProc
     (Object: access Gtkada_Builder_Record'
        Class); -- Enable menu shortcuts keys
   function ToggleCloseButton
     (User_Data: access GObject_Record'Class)
      return Boolean; -- Toggle sensitive of close button
   procedure ToggleCloseButtonProc
     (User_Data: access GObject_Record'
        Class); -- Toggle sensitive of close button
   -- Disable mouse clicking on selected UI element
   function DisableMouse
     (Object: access Gtkada_Builder_Record'Class) return Boolean;

end Maps.UI.Handlers;
