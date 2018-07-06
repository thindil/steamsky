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

with Gtk.Window; use Gtk.Window;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Utils.UI; use Utils.UI;
with Messages; use Messages;
with Maps.UI; use Maps.UI;

package body Stories.UI is

   Builder: Gtkada_Builder;

   procedure ShowStoryInfo(Object: access Gtkada_Builder_Record'Class) is
   begin
      null;
   end ShowStoryInfo;

   procedure SetStoryAsDestination
     (Object: access Gtkada_Builder_Record'Class) is
   begin
  --    if Events_List(EventIndex).SkyX = PlayerShip.SkyX and
  --      Events_List(EventIndex).SkyY = PlayerShip.SkyY then
  --       ShowDialog
  --         ("You are at this event now.",
  --          Gtk_Window(Get_Object(Object, "eventswindow")));
  --       return;
  --    end if;
  --    PlayerShip.DestinationX := Events_List(EventIndex).SkyX;
  --    PlayerShip.DestinationY := Events_List(EventIndex).SkyY;
      AddMessage
        ("You set the travel destination for your ship.",
         OrderMessage);
      ShowSkyMap;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Object, "gamestack")),
         "skymap");
      Set_Deletable(Gtk_Window(Get_Object(Object, "skymapwindow")), True);
   end SetStoryAsDestination;

   procedure ShowStory(Object: access Gtkada_Builder_Record'Class) is
   begin
      --ShowSkyMap(Events_List(EventIndex).SkyX, Events_List(EventIndex).SkyY);
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Object, "gamestack")),
         "skymap");
      Set_Deletable(Gtk_Window(Get_Object(Object, "skymapwindow")), True);
   end ShowStory;

   procedure CreateStoriesUI(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Show_Story_Info", ShowStoryInfo'Access);
      Register_Handler
        (Builder,
         "Set_Story_As_Destination",
         SetStoryAsDestination'Access);
      Register_Handler(Builder, "Show_Story", ShowStory'Access);
   end CreateStoriesUI;

   procedure ShowStoriesUI is
      StoriesIter: Gtk_Tree_Iter;
      StoriesList: Gtk_List_Store;
   begin
      if FinishedStories.Length = 0 then
         Show_All(Gtk_Widget(Get_Object(Builder, "btnmenu")));
         Hide(Gtk_Widget(Get_Object(Builder, "btnclose")));
         ShowDialog("You didn't discovered any story yet.", Gtk_Window(Get_Object(Builder, "skymapwindow")));
         return;
      end if;
      StoriesList := Gtk_List_Store(Get_Object(Builder, "storieslist"));
      Clear(StoriesList);
      for FinishedStory of FinishedStories loop
         Append(StoriesList, StoriesIter);
         Set(StoriesList, StoriesIter, 0, To_String(Stories_List(FinishedStory.Index).Name));
      end loop;
      Set_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbstories")), 0);
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")),
         "stories");
      Set_Deletable(Gtk_Window(Get_Object(Builder, "skymapwindow")), False);
      ShowLastMessage(Builder);
   end ShowStoriesUI;

end Stories.UI;
