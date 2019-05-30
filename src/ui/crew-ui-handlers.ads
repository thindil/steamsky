--    Copyright 2018 Bartek thindil Jasicki
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

with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Cell_Renderer_Combo; use Gtk.Cell_Renderer_Combo;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Glib; use Glib;
with Glib.Object; use Glib.Object;

package Crew.UI.Handlers is

   procedure ShowMemberInfo
     (Object: access Gtkada_Builder_Record'Class); -- Show selected member info
   procedure GiveOrdersAll
     (User_Data: access GObject_Record'
        Class); -- Give orders to all crew members
   procedure ShowInventory
     (Object: access Gtkada_Builder_Record'
        Class); -- Refresh informations about selected member inventory
   procedure ShowItemInfo2
     (Object: access Gtkada_Builder_Record'
        Class); -- Show informations about selected item
   procedure UseItem
     (Self: access Gtk_Cell_Renderer_Toggle_Record'Class;
      Path: UTF8_String); -- Set selected item as used by crew member or take it down
   procedure MoveItem
     (Object: access Gtkada_Builder_Record'
        Class); -- Move item from inventory to ship cargo
   procedure GiveCrewOrders
     (Self: access Gtk_Cell_Renderer_Combo_Record'Class;
      Path_String: UTF8_String;
      New_Iter: Gtk.Tree_Model
        .Gtk_Tree_Iter); -- Show give orders for all crew members
   procedure SetPriority
     (Self: access Gtk_Cell_Renderer_Combo_Record'Class;
      Path_String: UTF8_String;
      New_Iter: Gtk.Tree_Model
        .Gtk_Tree_Iter); -- Set selected priority (and reduce others if needed)
   procedure DismissMember
     (Object: access Gtkada_Builder_Record'
        Class); -- Dismiss selected crew member

end Crew.UI.Handlers;
