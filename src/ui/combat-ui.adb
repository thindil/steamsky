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
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Glib.Object; use Glib.Object;
with Game; use Game;
with Utils.UI; use Utils.UI;

package body Combat.UI is

   Builder: Gtkada_Builder;

   procedure ShowCombatUI is
      Iter: Gtk_Tree_Iter;
      List: Gtk_List_Store;
      DamagePercent: Gint;
      IsDamaged: Boolean := False;
   begin
      Show_All(Gtk_Widget(Get_Object(Builder, "combatwindow")));
      Hide(Gtk_Widget(Get_Object(Builder, "btnboard")));
      Set_Text(Gtk_Text_Buffer(Get_Object(Builder, "txtmessages")), "");
      Clear(Gtk_List_Store(Get_Object(Builder, "crewlist")));
      List := Gtk_List_Store(Get_Object(Builder, "damagelist"));
      Clear(List);
      for Module of PlayerShip.Modules loop
         if Module.Durability < Module.MaxDurability then
            Append(List, Iter);
            Set(List, Iter, 0, To_String(Module.Name));
            DamagePercent :=
              100 -
              Gint
                ((Float(Module.Durability) / Float(Module.MaxDurability)) *
                 100.0);
            Set(List, Iter, 1, DamagePercent);
            IsDamaged := True;
         end if;
      end loop;
      if not IsDamaged then
         Hide(Gtk_Widget(Get_Object(Builder, "scrolldamage")));
      end if;
      Clear(Gtk_List_Store(Get_Object(Builder, "enemylist")));
   end ShowCombatUI;

   procedure CreateCombatUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) & "ui" & Dir_Separator & "combat.glade",
           Error'Access) =
        Guint(0) then
         Put_Line("Error : " & Get_Message(Error));
         return;
      end if;
      Register_Handler(Builder, "Hide_Window", HideWindow'Access);
      Register_Handler(Builder, "Quit_Game", QuitGame'Access);
      Do_Connect(Builder);
   end CreateCombatUI;

end Combat.UI;
