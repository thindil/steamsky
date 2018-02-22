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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Label; use Gtk.Label;
with Gtk.Window; use Gtk.Window;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Game; use Game;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Bases; use Bases;
with Items; use Items;
with Utils.UI; use Utils.UI;
with Messages; use Messages;

package body Events.UI is

   Builder: Gtkada_Builder;
   EventIndex: Positive;

   function HideEvents
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "eventswindow")));
      CreateSkyMap;
      return True;
   end HideEvents;

   procedure ShowEventInfo(Object: access Gtkada_Builder_Record'Class) is
      EventsIter: Gtk_Tree_Iter;
      EventsModel: Gtk_Tree_Model;
      EventInfo: Unbounded_String;
      BaseIndex: Natural;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treeevents"))),
         EventsModel,
         EventsIter);
      if EventsIter = Null_Iter then
         return;
      end if;
      EventIndex :=
        Natural'Value(To_String(Get_Path(EventsModel, EventsIter))) + 1;
      BaseIndex :=
        SkyMap(Events_List(EventIndex).SkyX, Events_List(EventIndex).SkyY)
          .BaseIndex;
      EventInfo :=
        To_Unbounded_String
          ("X:" &
           Positive'Image(Events_List(EventIndex).SkyX) &
           " Y:" &
           Positive'Image(Events_List(EventIndex).SkyY));
      case Events_List(EventIndex).EType is
         when EnemyShip | EnemyPatrol | Trader | FriendlyShip =>
            Append
              (EventInfo,
               ASCII.LF &
               "Ship type: " &
               To_String(ProtoShips_List(Events_List(EventIndex).Data).Name));
         when FullDocks | AttackOnBase | Disease =>
            Append
              (EventInfo,
               ASCII.LF & "Base name: " & To_String(SkyBases(BaseIndex).Name));
         when DoublePrice =>
            Append
              (EventInfo,
               ASCII.LF & "Base name: " & To_String(SkyBases(BaseIndex).Name));
            Append
              (EventInfo,
               ASCII.LF &
               "Item: " &
               To_String(Items_List(Events_List(EventIndex).Data).Name));
         when None | BaseRecovery =>
            null;
      end case;
      Append
        (EventInfo,
         ASCII.LF &
         "Distance:" &
         Integer'Image
           (CountDistance
              (Events_List(EventIndex).SkyX,
               Events_List(EventIndex).SkyY)));
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblinfo")),
         To_String(EventInfo));
   end ShowEventInfo;

   procedure SetDestination(Object: access Gtkada_Builder_Record'Class) is
   begin
      if Events_List(EventIndex).SkyX = PlayerShip.SkyX and
        Events_List(EventIndex).SkyY = PlayerShip.SkyY then
         ShowDialog
           ("You are at this event now.",
            Gtk_Window(Get_Object(Object, "eventswindow")));
         return;
      end if;
      PlayerShip.DestinationX := Events_List(EventIndex).SkyX;
      PlayerShip.DestinationY := Events_List(EventIndex).SkyY;
      AddMessage("You set travel destination for your ship.", OrderMessage);
      Hide(Gtk_Widget(Get_Object(Object, "eventswindow")));
      CreateSkyMap;
   end SetDestination;

   procedure ShowEvent(Object: access Gtkada_Builder_Record'Class) is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "eventswindow")));
      CreateSkyMap(Events_List(EventIndex).SkyX, Events_List(EventIndex).SkyY);
   end ShowEvent;

   procedure CreateEventsUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "events.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Hide_Events", HideEvents'Access);
      Register_Handler(Builder, "Show_Event_Info", ShowEventInfo'Access);
      Register_Handler(Builder, "Set_Destination", SetDestination'Access);
      Register_Handler(Builder, "Show_Event", ShowEvent'Access);
      Do_Connect(Builder);
   end CreateEventsUI;

   procedure ShowEventsUI is
      EventsIter: Gtk_Tree_Iter;
      EventsList: Gtk_List_Store;
   begin
      EventsList := Gtk_List_Store(Get_Object(Builder, "eventslist"));
      Clear(EventsList);
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
      end loop;
      Show_All(Gtk_Widget(Get_Object(Builder, "eventswindow")));
      if N_Children(EventsList, Null_Iter) > 0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treeevents")),
            Gtk_Tree_Path_New_From_String("0"),
            Gtk_Tree_View_Column(Get_Object(Builder, "columnnames")),
            False);
      end if;
   end ShowEventsUI;

end Events.UI;
