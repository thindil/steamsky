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

   type GameStates is (SkyMap_View, Combat_View, Main_Menu); -- Game states
   PreviousGameState: GameStates; -- Current game state, needed for hide some windows

   procedure ShowDialog(Message: String;
      Parent: Gtk_Window); -- Show dialog with info
   function HideWindow
     (User_Data: access GObject_Record'Class)
     return Boolean; -- Hide window instead of destroying it
   procedure ShowWindow
     (User_Data: access GObject_Record'Class); -- Show selected window
   function ShowConfirmDialog(Message: String;
      Parent: Gtk_Window)
     return Boolean; -- Show confirmation dialog to player, return True, if player choice 'Yes' option
   function QuitGame
     (User_Data: access GObject_Record'Class)
     return Boolean; -- Save and quit from game
   function CloseWindow(Self: access Gtk_Widget_Record'Class;
      Event: Gdk_Event_Key) return Boolean; -- Close window on press Escape key
   procedure CloseMessages
     (Object: access Gtkada_Builder_Record'
        Class); -- Switch back to skymap or combat from info
   function SelectElement(Self: access GObject_Record'Class;
      Event: Gdk_Event_Key)
     return Boolean; -- Select other element on press Return key
   procedure TravelInfo(InfoText: in out Unbounded_String; Distance: Positive;
      ShowFuelName: Boolean :=
        False); -- Add info about travel eta and approx fuel usage
   procedure MinutesToDate(Minutes: Natural;
      InfoText: in out Unbounded_String); -- Convert minutes to game date and add it to text
   procedure ShowInventoryItemInfo(Label: Gtk_Label; ItemIndex: Positive;
      MemberIndex: Natural) with
      Pre => MemberIndex <=
      PlayerShip.Crew
        .Last_Index; -- Show info about selected item in ship cargo or crew member inventory
   procedure HideItemInfo
     (User_Data: access GObject_Record'
        Class); -- Hide or show detailed item info
   function ShowPopupMenu
     (User_Data: access GObject_Record'Class)
     return Boolean; -- Show popup menu for selected widget
   function ShowPopupMenuButton(Self: access Gtk_Widget_Record'Class;
      Event: Gdk_Event_Button)
     return Boolean; -- Show popup menu on click of right mouse button
   procedure SetUtilsBuilder
     (NewBuilder: Gtkada_Builder); -- Set Gtk Builder for Utils package

end Utils.UI;
