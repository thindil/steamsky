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

with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Cell_Renderer_Combo; use Gtk.Cell_Renderer_Combo;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Gtkada.Builder; use Gtkada.Builder;

-- ****h* Steamsky/Crew.UI.Handlers
-- FUNCTION
-- Provides code for crew UI handers
-- SOURCE
package Crew.UI.Handlers is
-- ****

   -- ****f* Crew.UI.Handlers/ShowMemberInfo
   -- FUNCTION
   -- Show selected member info
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowMemberInfo(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Crew.UI.Handlers/GiveOrdersAll
   -- FUNCTION
   -- Give orders to all crew members
   -- PARAMETERS
   -- User_Data - Button which was clicked
   -- SOURCE
   procedure GiveOrdersAll(User_Data: access GObject_Record'Class);
   -- ****
   -- ****f* Crew.UI.Handlers/ShowInventory
   -- FUNCTION
   -- Refresh information about selected member inventory
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowInventory(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Crew.UI.Handlers/ShowItemInfo2
   -- FUNCTION
   -- Show information about selected item
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowItemInfo2(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Crew.UI.Handlers/UseItem
   -- FUNCTION
   -- Set selected item as used by crew member or take it down
   -- PARAMETERS
   -- Self - GtkTreeView cell which was clicked
   -- Path - Full path in GtkTreeView to selected cell
   -- SOURCE
   procedure UseItem
     (Self: access Gtk_Cell_Renderer_Toggle_Record'Class; Path: UTF8_String);
   -- ****
   -- ****f* Crew.UI.Handlers/MoveItem
   -- FUNCTION
   -- Move item from inventory to ship cargo
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure MoveItem(Object: access Gtkada_Builder_Record'Class);
   -- ****
   -- ****f* Crew.UI.Handlers/GiveCrewOrders
   -- FUNCTION
   -- Show give orders for all crew members
   -- PARAMETERS
   -- Self        - GtkTreeView cell which was clicked
   -- Path_String - Full path in GtkTreeView to selected cell
   -- New_Iter    - Gtk_Tree_Iter to selected GtkTreeView cell
   -- SOURCE
   procedure GiveCrewOrders
     (Self: access Gtk_Cell_Renderer_Combo_Record'Class;
      Path_String: UTF8_String; New_Iter: Gtk.Tree_Model.Gtk_Tree_Iter);
   -- ****
   -- ****f* Crew.UI.Handlers/SetPriority
   -- FUNCTION
   -- Set selected priority (and reduce others if needed)
   -- PARAMETERS
   -- Self        - GtkTreeView cell which was clicked
   -- Path_String - Full path in GtkTreeView to selected cell
   -- New_Iter    - Gtk_Tree_Iter to selected GtkTreeView cell
   -- SOURCE
   procedure SetPriority
     (Self: access Gtk_Cell_Renderer_Combo_Record'Class;
      Path_String: UTF8_String; New_Iter: Gtk.Tree_Model.Gtk_Tree_Iter);
   -- ****
   -- ****f* Crew.UI.Handlers/DismissMember
   -- FUNCTION
   -- Dismiss selected crew member
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure DismissMember(Object: access Gtkada_Builder_Record'Class);
   -- ****

end Crew.UI.Handlers;
