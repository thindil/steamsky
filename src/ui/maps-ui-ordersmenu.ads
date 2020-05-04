-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Gtk.Button; use Gtk.Button;

-- ****h* Maps.UI.OrdersMenu/OrdersMenu
-- FUNCTION
-- Provide code for manipulate orders menu
-- SOURCE
package Maps.UI.OrdersMenu is
-- ****

   -- ****f* OrdersMenu/ShowOrders
   -- FUNCTION
   -- Show available ship orders
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowOrders(Object: access Gtkada_Builder_Record'Class);
   -- ****

   -- ****if* Maps.UI.OrdersMenu/ExecuteStory
   -- FUNCTION
   -- Execute proper action which depends on current story progress
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused
   -- SOURCE
   procedure ExecuteStory(Self: access Gtk_Button_Record'Class);
   -- ****

   -- ****f* Maps.UI.OrdersMenu/HideOrders
   -- FUNCTION
   -- Hide orders menu
   -- SOURCE
   procedure HideOrders(Self: access Gtk_Button_Record'Class);
   -- ****

   -- ****f* Maps.UI.OrdersMenu/CreateOrdersMenu
   -- FUNCTION
   -- Create orders menu UI
   -- SOURCE
   procedure CreateOrdersMenu;
   -- ****

end Maps.UI.OrdersMenu;
