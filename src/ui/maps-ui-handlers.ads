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

-- ****h* Steamsky/Maps.UI.Handlers
-- FUNCTION
-- Provides code for handlers of maps UI
-- SOURCE
package Maps.UI.Handlers is
-- ****

   -- ****f* Maps.UI.Handlers/QuitGameMenu
   -- FUNCTION
   -- Quit from game
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure QuitGameMenu(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/HideMapInfoWindow
   -- FUNCTION
   -- Hide selected window
   -- PARAMETERS
   -- User_Data - Button which was clicked
   -- SOURCE
   procedure HideMapInfoWindow(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/GetMapSize
   -- FUNCTION
   -- Get size of map cell
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure GetMapSize(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/SetDestination
   -- FUNCTION
   -- Set ship destination
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   function SetDestination
     (Object: access Gtkada_Builder_Record'Class) return Boolean;
   -- ****
   -- ****f* Maps.UI.Handlers/MoveMap
   -- FUNCTION
   -- Move map to selected position
   -- PARAMETERS
   -- User_Data - Button which was clicked
   -- SOURCE
   procedure MoveMap(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/BtnDockClicked
   -- FUNCTION
   -- Dock/undock ship from base
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure BtnDockClicked(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/ChangeSpeed
   -- FUNCTION
   -- Change current ship speed
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ChangeSpeed(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/MoveShip
   -- FUNCTION
   -- Move ship in selected direction
   -- PARAMETERS
   -- User_Data - Button which was clicked
   -- SOURCE
   procedure MoveShip(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/ShowOrders
   -- FUNCTION
   -- Show available ship orders
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowOrders(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/WaitOrder
   -- FUNCTION
   -- Execute selected wait order
   -- PARAMETERS
   -- User_Data - Button which was clicked
   -- SOURCE
   procedure WaitOrder(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/AttackOrder
   -- FUNCTION
   -- Execute attack or show wait order
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure AttackOrder(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/ShowHelp
   -- FUNCTION
   -- Show help for selected topic
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowHelp(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/ShowInfo
   -- FUNCTION
   -- Show selected info (ship/crew/cargo/etc)
   -- PARAMETERS
   -- User_Data - Menu entry which was selected
   -- SOURCE
   procedure ShowInfo(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/ResignFromGame
   -- FUNCTION
   -- Show confirmation for resign from game
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ResignFromGame(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/ShowMissions
   -- FUNCTION
   -- Show available missions in base
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowMissions(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/StartMission
   -- FUNCTION
   -- Start mission if ship is in cell with it
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure StartMission(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/CompleteMission
   -- FUNCTION
   -- Finish mission if ship is at sky base
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure CompleteMission(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/ExecuteOrder
   -- FUNCTION
   -- Set home base/ask for bases/ask for events
   -- PARAMETERS
   -- User_Data - Button which was clicked
   -- SOURCE
   procedure ExecuteOrder(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/DeliverMedicines
   -- FUNCTION
   -- Deliver medicines to base
   -- PARAMETERS
   -- User_Data - Button which was clicked
   -- SOURCE
   procedure DeliverMedicines(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/ShowWaitOrders
   -- FUNCTION
   -- Show available wait orders
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowWaitOrders(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/UpdateTooltip
   -- FUNCTION
   -- Update map tooltip with information about cell on mouse movement
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   function UpdateTooltip
     (Object: access Gtkada_Builder_Record'Class) return Boolean;
   -- ****
   -- ****f* Maps.UI.Handlers/MapKeyReleased
   -- FUNCTION
   -- Center map on player ship
   -- PARAMETERS
   -- Self  - Main game window
   -- Event - Detailed info about which key was released
   -- SOURCE
   function MapKeyReleased
     (Self: access Gtk_Widget_Record'Class; Event: Gdk.Event.Gdk_Event_Key)
      return Boolean;
   -- ****
   -- ****f* Maps.UI.Handlers/MapKeyPressed
   -- FUNCTION
   -- Move mouse cursor on map by keyboard
   -- PARAMETERS
   -- Self  - Main game window
   -- Event - Detailed info about which key was pressed or where mouse was
   --         clicked
   -- SOURCE
   function MapKeyPressed
     (Self: access Gtk_Widget_Record'Class; Event: Gdk.Event.Gdk_Event_Key)
      return Boolean;
   -- ****
   -- ****f* Maps.UI.Handlers/ZoomMap
   -- FUNCTION
   -- Resize sky map font with mouse wheel
   -- PARAMETERS
   -- Self  - Main game window (unused)
   -- Event - Detailed info about mouse action
   -- SOURCE
   function ZoomMap
     (Self: access Gtk_Widget_Record'Class; Event: Gdk.Event.Gdk_Event_Scroll)
      return Boolean;
   -- ****
   -- ****f* Maps.UI.Handlers/DisableMenuShortcuts
   -- FUNCTION
   -- Disable menu shortcuts keys
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SEE ALSO
   -- DisableMenuShortcutsProc
   -- SOURCE
   function DisableMenuShortcuts
     (Object: access Gtkada_Builder_Record'Class) return Boolean;
   -- ****
   -- ****f* Maps.UI.Handlers/EnableMenuShortcuts
   -- FUNCTION
   -- Enable menu shortcuts keys
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SEE ALSO
   -- EnableMenuShortcutsProc
   -- SOURCE
   function EnableMenuShortcuts
     (Object: access Gtkada_Builder_Record'Class) return Boolean;
   -- ****
   -- ****f* Maps.UI.Handlers/DisableMenuShortcutsProc
   -- FUNCTION
   -- Disable menu shortcuts keys
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SEE ALSO
   -- DisableMenuShortcuts
   -- SOURCE
   procedure DisableMenuShortcutsProc
     (Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/EnableMenuShortcutsProc
   -- FUNCTION
   -- Enable menu shortcuts keys
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SEE ALSO
   -- EnableMenuShortcuts
   -- SOURCE
   procedure EnableMenuShortcutsProc
     (Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/ToggleCloseButton
   -- FUNCTION
   -- Toggle sensitive of close button
   -- PARAMETERS
   -- User_Data - Button to disable
   -- SEE ALSO
   -- ToggleCloseButtonProc
   -- SOURCE
   function ToggleCloseButton
     (User_Data: access GObject_Record'Class) return Boolean;
   -- ****
   -- ****f* Maps.UI.Handlers/ToggleCloseButtonProc
   -- FUNCTION
   -- Toggle sensitive of close button
   -- PARAMETERS
   -- User_Data - Button to disable
   -- SEE ALSO
   -- ToggleCloseButton
   -- SOURCE
   procedure ToggleCloseButtonProc(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/MoveMapInfo
   -- FUNCTION
   -- Move map info widget when mouse enters it
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   function MoveMapInfo
     (Object: access Gtkada_Builder_Record'Class) return Boolean;
   -- ****
   -- ****f* Maps.UI.Handlers/MoveMapButtons
   -- FUNCTION
   -- Move map buttons to selected position
   -- SOURCE
   procedure MoveMapButtons(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Maps.UI.Handlers/DisableMouse
   -- FUNCTION
   -- Disable mouse clicking on selected UI element
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   function DisableMouse
     (Object: access Gtkada_Builder_Record'Class) return Boolean;
   -- ****
   -- ****f* Maps.UI.Handlers/SetMessagesPosition
   -- FUNCTION
   -- Set new message window size setting on resize it
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   function SetMessagesPosition
     (Object: access Gtkada_Builder_Record'Class) return Boolean;
   -- ****

end Maps.UI.Handlers;
