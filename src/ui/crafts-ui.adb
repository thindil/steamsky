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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Gdk.RGBA; use Gdk.RGBA;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Help.UI; use Help.UI;
with Ships; use Ships;
with Items; use Items;

package body Crafts.UI is

   Builder: Gtkada_Builder;

   function HideCrafts
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "craftswindow")));
      CreateSkyMap;
      return True;
   end HideCrafts;

   procedure HideLastMessage(Object: access Gtkada_Builder_Record'Class) is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "infolastmessage")));
      LastMessage := Null_Unbounded_String;
   end HideLastMessage;

   procedure ShowLastMessage is
   begin
      if LastMessage = Null_Unbounded_String then
         HideLastMessage(Builder);
      else
         Set_Text
           (Gtk_Label(Get_Object(Builder, "lbllastmessage")),
            To_String(LastMessage));
         Show_All(Gtk_Widget(Get_Object(Builder, "infolastmessage")));
         LastMessage := Null_Unbounded_String;
      end if;
   end ShowLastMessage;

   procedure ShowHelp(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      ShowHelpUI(5);
   end ShowHelp;

   procedure CreateCraftsUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "crafts.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Override_Background_Color
        (Gtk_Widget(Get_Object(Builder, "lblinfo")),
         0,
         Black_RGBA);
      Override_Color
        (Gtk_Widget(Get_Object(Builder, "lblinfo")),
         0,
         White_RGBA);
      Register_Handler(Builder, "Hide_Crafts", HideCrafts'Access);
      Register_Handler(Builder, "Hide_Last_Message", HideLastMessage'Access);
      Register_Handler(Builder, "Show_Help", ShowHelp'Access);
      Do_Connect(Builder);
   end CreateCraftsUI;

   procedure ShowCraftsUI is
      Deconstructs: Positive_Container.Vector;
      RecipesIter: Gtk_Tree_Iter;
      RecipesList: Gtk_List_Store;
   begin
      for Item of PlayerShip.Cargo loop
         for J in Recipes_List.First_Index .. Recipes_List.Last_Index loop
            if Recipes_List(J).ResultIndex = Item.ProtoIndex then
               if Known_Recipes.Find_Index(Item => J) =
                 Positive_Container.No_Index and
                 Deconstructs.Find_Index(Item => Item.ProtoIndex) =
                   Positive_Container.No_Index then
                  Deconstructs.Append(New_Item => Item.ProtoIndex);
                  exit;
               end if;
            end if;
         end loop;
      end loop;
      RecipesList := Gtk_List_Store(Get_Object(Builder, "recipeslist"));
      Clear(RecipesList);
      for I in Known_Recipes.First_Index .. Known_Recipes.Last_Index loop
         Append(RecipesList, RecipesIter);
         Set
           (RecipesList,
            RecipesIter,
            0,
            To_String
              (Items_List(Recipes_List(Known_Recipes(I)).ResultIndex).Name));
         Set(RecipesList, RecipesIter, 1, Gint(Known_Recipes.Element(I)));
      end loop;
      for I in Deconstructs.First_Index .. Deconstructs.Last_Index loop
         Append(RecipesList, RecipesIter);
         Set
           (RecipesList,
            RecipesIter,
            0,
            "Deconstruct " & To_String(Items_List(Deconstructs(I)).Name));
         Set(RecipesList, RecipesIter, 1, Gint(0 - Deconstructs(I)));
      end loop;
      Show_All(Gtk_Widget(Get_Object(Builder, "craftswindow")));
      Set_Cursor
        (Gtk_Tree_View(Get_Object(Builder, "treerecipes")),
         Gtk_Tree_Path_New_From_String("0"),
         Gtk_Tree_View_Column(Get_Object(Builder, "columnname")),
         False);
      ShowLastMessage;
   end ShowCraftsUI;

end Crafts.UI;
