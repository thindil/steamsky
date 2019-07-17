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
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Window; use Gtk.Window;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gdk.Event; use Gdk.Event;
with Ships; use Ships;

package Utils.UI is

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

-- ****f* Utils.UI/HideDialog
-- FUNCTION
-- Close dialog window and stop auto close timer
-- SOURCE
   procedure HideDialog(Object: access Gtkada_Builder_Record'Class);
-- ****
-- ****f* Utils.UI/ShowDialog
-- FUNCTION
-- Show dialog with info
-- SOURCE
   procedure ShowDialog(Message: String);
-- ****
-- ****f* Utils.UI/HideWindow
-- FUNCTION
-- Hide window instead of destroying it
-- SOURCE
   function HideWindow(User_Data: access GObject_Record'Class) return Boolean;
-- ****
-- ****f* Utils.UI/ShowWindow
-- FUNCTION
-- Show selected window
-- SOURCE
   procedure ShowWindow(User_Data: access GObject_Record'Class);
-- ****
-- ****f* Utils.UI/ShowConfirmDialog
-- FUNCTION
-- Show confirmation dialog to player, return True, if player choice 'Yes' option
-- SOURCE
   function ShowConfirmDialog
     (Message: String; Parent: Gtk_Window) return Boolean;
-- ****
-- ****f* Utils.UI/QuitGame
-- FUNCTION
-- Save and quit from game
-- SOURCE
   function QuitGame(User_Data: access GObject_Record'Class) return Boolean;
-- ****
-- ****f* Utils.UI/CloseWindow
-- FUNCTION
-- Close window on press Escape key
-- SOURCE
   function CloseWindow
     (Self: access Gtk_Widget_Record'Class; Event: Gdk_Event_Key)
      return Boolean;
-- ****
-- ****f* Utils.UI/CloseMessages
-- FUNCTION
-- Switch back to skymap or combat from info
-- SOURCE
   procedure CloseMessages(Object: access Gtkada_Builder_Record'Class);
-- ****
-- ****f* Utils.UI/SelectElement
-- FUNCTION
-- Select other element on press Return key
-- SOURCE
   function SelectElement
     (Self: access GObject_Record'Class; Event: Gdk_Event_Key) return Boolean;
-- ****
-- ****f* Utils.UI/TravelInfo
-- FUNCTION
-- Add info about travel eta and approx fuel usage
-- SOURCE
   procedure TravelInfo
     (InfoText: in out Unbounded_String; Distance: Positive;
      ShowFuelName: Boolean := False);
-- ****
-- ****f* Utils.UI/MinutesToDate
-- FUNCTION
-- Convert minutes to game date and add it to text
-- SOURCE
   procedure MinutesToDate
     (Minutes: Natural; InfoText: in out Unbounded_String);
-- ****
-- ****f* Utils.UI/ShowInventoryItemInfo
-- FUNCTION
-- Show info about selected item in ship cargo or crew member inventory
-- SOURCE
   procedure ShowInventoryItemInfo
     (Label: Gtk_Label; ItemIndex: Positive; MemberIndex: Natural) with
      Pre => MemberIndex <= PlayerShip.Crew.Last_Index;
-- ****
-- ****f* Utils.UI/HideItemInfo
-- FUNCTION
-- Hide or show detailed item info
-- SOURCE
   procedure HideItemInfo(User_Data: access GObject_Record'Class);
-- ****
-- ****f* Utils.UI/ShowPopupMenu
-- FUNCTION
-- Show popup menu for selected widget
-- SOURCE
   function ShowPopupMenu
     (User_Data: access GObject_Record'Class) return Boolean;
-- ****
-- ****f* Utils.UI/ShowPopupMenuButton
-- FUNCTION
-- Show popup menu on click of right mouse button
-- SOURCE
   function ShowPopupMenuButton
     (Self: access Gtk_Widget_Record'Class; Event: Gdk_Event_Button)
      return Boolean;
-- ****
-- ****f* Utils.UI/SetUtilsBuilder
-- FUNCTION
-- Set Gtk Builder for Utils package
-- SOURCE
   procedure SetUtilsBuilder(NewBuilder: Gtkada_Builder);
-- ****
-- ****f* Utils.UI/UpdateMessages;
-- FUNCTION
-- Update game messages and last message
-- SOURCE
   procedure UpdateMessages;
-- ****
-- ****f* Utils.UI/CheckAmount
-- FUNCTION
-- Check did entered amount in text field don't drop below low level warnings
-- SOURCE
   procedure CheckAmount(User_Data: access GObject_Record'Class);
-- ****

end Utils.UI;
