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

   -- Game states
   type GameStates is (SkyMap_View, Combat_View, Main_Menu);
   -- Current game state, needed for hide some windows
   PreviousGameState: GameStates;

   -- Close dialog window and stop auto close timer
   procedure HideDialog(Object: access Gtkada_Builder_Record'Class);
   -- Show dialog with info
   procedure ShowDialog(Message: String);
   -- Hide window instead of destroying it
   function HideWindow(User_Data: access GObject_Record'Class) return Boolean;
   -- Show selected window
   procedure ShowWindow(User_Data: access GObject_Record'Class);
   -- Show confirmation dialog to player, return True, if player choice 'Yes' option
   function ShowConfirmDialog
     (Message: String; Parent: Gtk_Window) return Boolean;
   -- Save and quit from game
   function QuitGame(User_Data: access GObject_Record'Class) return Boolean;
   -- Close window on press Escape key
   function CloseWindow
     (Self: access Gtk_Widget_Record'Class; Event: Gdk_Event_Key)
      return Boolean;
   -- Switch back to skymap or combat from info
   procedure CloseMessages(Object: access Gtkada_Builder_Record'Class);
   -- Select other element on press Return key
   function SelectElement
     (Self: access GObject_Record'Class; Event: Gdk_Event_Key) return Boolean;
   -- Add info about travel eta and approx fuel usage
   procedure TravelInfo
     (InfoText: in out Unbounded_String; Distance: Positive;
      ShowFuelName: Boolean := False);
   -- Convert minutes to game date and add it to text
   procedure MinutesToDate
     (Minutes: Natural; InfoText: in out Unbounded_String);
   -- Show info about selected item in ship cargo or crew member inventory
   procedure ShowInventoryItemInfo
     (Label: Gtk_Label; ItemIndex: Positive; MemberIndex: Natural) with
      Pre => MemberIndex <= PlayerShip.Crew.Last_Index;
      -- Hide or show detailed item info
   procedure HideItemInfo(User_Data: access GObject_Record'Class);
   -- Show popup menu for selected widget
   function ShowPopupMenu
     (User_Data: access GObject_Record'Class) return Boolean;
   -- Show popup menu on click of right mouse button
   function ShowPopupMenuButton
     (Self: access Gtk_Widget_Record'Class; Event: Gdk_Event_Button)
      return Boolean;
   -- Set Gtk Builder for Utils package
   procedure SetUtilsBuilder(NewBuilder: Gtkada_Builder);
   -- Update game messages and last message
   procedure UpdateMessages;
-- Check did entered amount in text field don't drop below low level warnings
   procedure CheckAmount(User_Data: access GObject_Record'Class);

end Utils.UI;
