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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Label; use Gtk.Label;
with Gtk.Stack; use Gtk.Stack;
with Glib; use Glib;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Bases; use Bases;
with Items; use Items;
with Utils.UI; use Utils.UI;
with Messages; use Messages;

package body Events.UI is

-- ****iv* Events.UI/Builder
-- SOURCE
   Builder: Gtkada_Builder;
-- ****
-- ****iv* Events.UI/EventIndex
-- SOURCE
   EventIndex: Positive;
-- ****
-- ****iv* Events.UI/Cleaning
-- SOURCE
   Cleaning: Boolean;
-- ****

-- ****if* Events.UI/ShowEventInfo
-- SOURCE
   procedure ShowEventInfo(Object: access Gtkada_Builder_Record'Class) is
-- ****
      EventInfo: Unbounded_String;
      BaseIndex: Integer;
   begin
      if Cleaning then
         return;
      end if;
      declare
         EventsIter: Gtk_Tree_Iter;
         EventsModel: Gtk_Tree_Model;
      begin
         Get_Selected
           (Gtk.Tree_View.Get_Selection
              (Gtk_Tree_View(Get_Object(Object, "treeevents"))),
            EventsModel, EventsIter);
         if EventsIter = Null_Iter then
            return;
         end if;
         EventIndex := Positive(Get_Int(EventsModel, EventsIter, 1));
      end;
      BaseIndex :=
        SkyMap(Events_List(EventIndex).SkyX, Events_List(EventIndex).SkyY)
          .BaseIndex;
      EventInfo :=
        To_Unbounded_String
          ("X:" & Positive'Image(Events_List(EventIndex).SkyX) & " Y:" &
           Positive'Image(Events_List(EventIndex).SkyY));
      case Events_List(EventIndex).EType is
         when EnemyShip | EnemyPatrol | Trader | FriendlyShip =>
            Append
              (EventInfo,
               LF & "Ship type: " &
               To_String
                 (ProtoShips_List(Events_List(EventIndex).ShipIndex).Name));
         when FullDocks | AttackOnBase | Disease =>
            Append
              (EventInfo,
               LF & "Base name: " & To_String(SkyBases(BaseIndex).Name));
         when DoublePrice =>
            Append
              (EventInfo,
               LF & "Base name: " & To_String(SkyBases(BaseIndex).Name));
            Append
              (EventInfo,
               LF & "Item: " &
               To_String(Items_List(Events_List(EventIndex).ItemIndex).Name));
         when None | BaseRecovery =>
            null;
      end case;
      Set_Label
        (Gtk_Label(Get_Object(Object, "lbleventinfo")), To_String(EventInfo));
   end ShowEventInfo;

-- ****if* Events.UI/SetEventAsDestination
-- SOURCE
   procedure SetEventAsDestination
     (Object: access Gtkada_Builder_Record'Class) is
-- ****
   begin
      if Events_List(EventIndex).SkyX = PlayerShip.SkyX and
        Events_List(EventIndex).SkyY = PlayerShip.SkyY then
         ShowDialog("You are at this event now.");
         return;
      end if;
      PlayerShip.DestinationX := Events_List(EventIndex).SkyX;
      PlayerShip.DestinationY := Events_List(EventIndex).SkyY;
      AddMessage
        ("You set the travel destination for your ship.", OrderMessage);
      ShowSkyMap;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Object, "gamestack")), "skymap");
   end SetEventAsDestination;

-- ****if* Events.UI/ShowEvent
-- SOURCE
   procedure ShowEvent(Object: access Gtkada_Builder_Record'Class) is
-- ****
   begin
      ShowSkyMap(Events_List(EventIndex).SkyX, Events_List(EventIndex).SkyY);
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Object, "gamestack")), "skymap");
   end ShowEvent;

   procedure CreateEventsUI(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Show_Event_Info", ShowEventInfo'Access);
      Register_Handler
        (Builder, "Set_Event_As_Destination", SetEventAsDestination'Access);
      Register_Handler(Builder, "Show_Event", ShowEvent'Access);
      On_Button_Release_Event
        (Gtk_Widget(Get_Object(Builder, "treeevents")),
         ShowPopupMenuButton'Access);
   end CreateEventsUI;

   procedure ShowEventsUI is
      EventsIter: Gtk_Tree_Iter;
      EventsList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "eventslist"));
   begin
      Cleaning := True;
      Clear(EventsList);
      Cleaning := False;
      for I in Events_List.First_Index .. Events_List.Last_Index loop
         Append(EventsList, EventsIter);
         case Events_List(I).EType is
            when EnemyShip =>
               Set(EventsList, EventsIter, 0, "Enemy ship spotted");
            when FullDocks =>
               Set(EventsList, EventsIter, 0, "Full docks in base");
            when AttackOnBase =>
               Set(EventsList, EventsIter, 0, "Base is under attack");
            when Disease =>
               Set(EventsList, EventsIter, 0, "Disease in base");
            when EnemyPatrol =>
               Set(EventsList, EventsIter, 0, "Enemy patrol");
            when DoublePrice =>
               Set(EventsList, EventsIter, 0, "Double price in base");
            when Trader =>
               Set(EventsList, EventsIter, 0, "Friendly trader spotted");
            when FriendlyShip =>
               Set(EventsList, EventsIter, 0, "Friendly ship spotted");
            when None | BaseRecovery =>
               null;
         end case;
         Set(EventsList, EventsIter, 1, Gint(I));
         Set
           (EventsList, EventsIter, 2,
            Gint(CountDistance(Events_List(I).SkyX, Events_List(I).SkyY)));
      end loop;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "eventslist");
      if N_Children(EventsList, Null_Iter) > 0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treeevents")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
      end if;
   end ShowEventsUI;

end Events.UI;
