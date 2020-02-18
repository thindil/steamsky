--    Copyright 2018-2020 Bartek thindil Jasicki
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
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Info_Bar; use Gtk.Info_Bar;
with Gtk.Label; use Gtk.Label;
with Gtk.Window; use Gtk.Window;
with Gtk.Widget; use Gtk.Widget;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gdk.Event; use Gdk.Event;
with Ships; use Ships;

-- ****h* Steamsky/Utils.UI
-- FUNCTION
-- Provides various code for UI
-- SOURCE
package Utils.UI is
-- ****

   -- ****t* Utils.UI/GameStates
   -- FUNCTION
   -- Game states
   -- SOURCE
   type GameStates is (SkyMap_View, Combat_View, Main_Menu);
   -- ****

   -- ****v* Utils.UI/PreviousGameState
   -- FUNCTION
   -- Current game state, needed for hide some windows
   -- SOURCE
   PreviousGameState: GameStates;
   -- ****

   -- ****v* Utils.UI/MessageBox
   -- FUNCTION
   -- Dialog used to show messages to the player
   -- SOURCE
   MessageBox: Gtk_Info_Bar;
   -- ****

   -- ****f* Utils.UI/HideDialog
   -- FUNCTION
   -- Close dialog window and stop auto close timer
   -- PARAMETERS
   -- Self        - Gtk_Info_Bar to hide
   -- Response_Id - Gtk response id. Unused.
   -- SOURCE
   procedure HideDialog
     (Self: access Gtk_Info_Bar_Record'Class; Response_Id: Glib.Gint);
   -- ****

   -- ****f* Utils.UI/ShowDialog
   -- FUNCTION
   -- Show dialog with info
   -- PARAMETERS
   -- Message - Text to show
   -- SOURCE
   procedure ShowDialog(Message: String) with
      Pre => Message'Length > 0;
      -- ****

      -- ****f* Utils.UI/HideWindow
      -- FUNCTION
      -- Hide window instead of destroying it
      -- PARAMETERS
      -- User_Data - Window to hide
      -- RESULT
      -- Always return true
      -- SOURCE
   function HideWindow(User_Data: access GObject_Record'Class) return Boolean;
   -- ****

   -- ****f* Utils.UI/ShowWindow
   -- FUNCTION
   -- Show selected window
   -- PARAMETERS
   -- User_Data - Window to show
   -- SOURCE
   procedure ShowWindow(User_Data: access GObject_Record'Class);
   -- ****

   -- ****f* Utils.UI/ShowConfirmDialog
   -- FUNCTION
   -- Show confirmation dialog to player
   -- PARAMETERS
   -- Message - Text to show to player
   -- Parent  - Gtk Window which will be parent for that dialog
   -- RESULT
   -- True if player select 'Yes' option, otherwise false
   -- SOURCE
   function ShowConfirmDialog
     (Message: String; Parent: Gtk_Window) return Boolean with
      Pre => Message'Length > 0;
      -- ****

      -- ****f* Utils.UI/QuitGame
      -- FUNCTION
      -- Save and quit from game
      -- PARAMETERS
      -- User_Data - the game window
      -- RESULT
      -- Always true
      -- SOURCE
   function QuitGame(User_Data: access GObject_Record'Class) return Boolean;
   -- ****

   -- ****f* Utils.UI/CloseWindow
   -- FUNCTION
   -- Close window on press Escape key
   -- PARAMETERS
   -- Self  - Game window to close
   -- Event - Detailed info about keyboard event
   -- RESULT
   -- False if window was closed otherwise true
   -- SOURCE
   function CloseWindow
     (Self: access Gtk_Widget_Record'Class; Event: Gdk_Event_Key)
      return Boolean;
   -- ****

   -- ****f* Utils.UI/CloseMessages
   -- FUNCTION
   -- Switch back to skymap or combat from info
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure CloseMessages(Object: access Gtkada_Builder_Record'Class);
   -- ****

   -- ****f* Utils.UI/SelectElement
   -- FUNCTION
   -- Select other element on press Return key
   -- PARAMETERS
   -- Self  - Gtk_Widget which will grab focus
   -- Event - Detailed info about keyboard event
   -- RESULT
   -- True if new element was selected, otherwise false
   -- SOURCE
   function SelectElement
     (Self: access GObject_Record'Class; Event: Gdk_Event_Key) return Boolean;
   -- ****

   -- ****f* Utils.UI/TravelInfo
   -- FUNCTION
   -- Add info about travel eta and approx fuel usage
   -- PARAMETERS
   -- InfoText     - Text to which info about travel will be added
   -- Distance     - Distance in map fields to destination point
   -- ShowFuelName - If true, add fuel name to info. Default is false
   -- RESULT
   -- Parameter InfoText
   -- SOURCE
   procedure TravelInfo
     (InfoText: in out Unbounded_String; Distance: Positive;
      ShowFuelName: Boolean := False);
   -- ****

   -- ****f* Utils.UI/MinutesToDate
   -- FUNCTION
   -- Convert minutes to game date and add it to text
   -- PARAMETERS
   -- Minutes  - Amount of minutes to convert
   -- InfoText - Text to which time info will be added
   -- RESULT
   -- Parameter InfoText
   -- SOURCE
   procedure MinutesToDate
     (Minutes: Natural; InfoText: in out Unbounded_String);
   -- ****

   -- ****f* Utils.UI/ShowInventoryItemInfo
   -- FUNCTION
   -- Show info about selected item in ship cargo or crew member inventory
   -- PARAMETERS
   -- Label       - Gtk_Label which text will be set
   -- ItemIndex   - Index of item (can be inventory or ship cargo)
   -- MemberIndex - If item is in crew member inventory, crew index of member,
   --               otherwise 0
   -- SOURCE
   procedure ShowInventoryItemInfo
     (Label: Gtk_Label; ItemIndex: Positive; MemberIndex: Natural) with
      Pre => MemberIndex <= PlayerShip.Crew.Last_Index;
      -- ****

      -- ****f* Utils.UI/HideItemInfo
      -- FUNCTION
      -- Hide or show detailed item info
      -- PARAMETERS
      -- User_Data - Gtk_Widget to hide
      -- SOURCE
   procedure HideItemInfo(User_Data: access GObject_Record'Class);
   -- ****

   -- ****f* Utils.UI/ShowPopupMenu
   -- FUNCTION
   -- Show popup menu for selected widget
   -- PARAMETERS
   -- User_Data - Gtk_Menu to show
   -- RESULT
   -- Always false
   -- SOURCE
   function ShowPopupMenu
     (User_Data: access GObject_Record'Class) return Boolean;
   -- ****

   -- ****f* Utils.UI/ShowPopupMenuButton
   -- FUNCTION
   -- Show popup menu on click of right mouse button
   -- PARAMETERS
   -- Self  - GtkTreeView to which menu will be show
   -- Event - Detailed info about mouse event
   -- RESULT
   -- Always false
   -- SOURCE
   function ShowPopupMenuButton
     (Self: access Gtk_Widget_Record'Class; Event: Gdk_Event_Button)
      return Boolean;
   -- ****

   -- ****f* Utils.UI/SetUtilsBuilder
   -- FUNCTION
   -- Set Gtk Builder for Utils package
   -- PARAMETERS
   -- NewBuilder - Gtkada_Builder used to create UI
   -- SOURCE
   procedure SetUtilsBuilder(NewBuilder: Gtkada_Builder);
   -- ****

   -- ****f* Utils.UI/UpdateMessages
   -- FUNCTION
   -- Update game messages and last message
   -- SOURCE
   procedure UpdateMessages;
   -- ****

-- ****f* Utils.UI/CheckAmount
-- FUNCTION
-- Check did entered amount in text field don't drop below low level warnings
-- PARAMETERS
-- User_Data - Text field to check
-- SOURCE
   procedure CheckAmount(User_Data: access GObject_Record'Class);
   -- ****

   -- ****f* Utils.UI/RemoveWidget
   -- FUNCTION
   -- Remove selected widget
   -- PARAMETERS
   -- Widget - Gtk_Widget to remove
   -- SOURCE
   procedure RemoveWidget(Widget: not null access Gtk_Widget_Record'Class);
   -- ****

end Utils.UI;
