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
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Label; use Gtk.Label;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Gdk.RGBA; use Gdk.RGBA;
with Maps.UI; use Maps.UI;
with Combat.UI; use Combat.UI;

package body Ships.Cargo.UI is

   Builder: Gtkada_Builder;
   GameState: GameStates;

   function HideCargoInfo
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "cargowindow")));
      case GameState is
         when SkyMap_View =>
            CreateSkyMap;
         when Combat_View =>
            ShowCombatUI;
      end case;
      return True;
   end HideCargoInfo;

   procedure RefreshCargoInfo is
      CargoIter: Gtk_Tree_Iter;
      CargoList: Gtk_List_Store;
   begin
      CargoList := Gtk_List_Store(Get_Object(Builder, "cargolist"));
      Clear(CargoList);
      for Item of PlayerShip.Cargo loop
         Append(CargoList, CargoIter);
         Set(CargoList, CargoIter, 0, GetItemName(Item));
      end loop;
      Set_Label
        (Gtk_Label(Get_Object(Builder, "lblfreespace")),
         "Free inventory space:" & Integer'Image(FreeCargo(0)) & " kg");
   end RefreshCargoInfo;

   procedure SetActiveItem is
   begin
      if PlayerShip.Cargo.Length > 0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treecargo")),
            Gtk_Tree_Path_New_From_String("0"),
            Gtk_Tree_View_Column(Get_Object(Builder, "columncargo")),
            False);
      end if;
   end SetActiveItem;

   procedure CreateCargoUI is
      Error: aliased GError;
   begin
      if Builder /= null then
         return;
      end if;
      Gtk_New(Builder);
      if Add_From_File
          (Builder,
           To_String(DataDirectory) &
           "ui" &
           Dir_Separator &
           "ships-cargo.glade",
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
      Register_Handler(Builder, "Hide_Cargo_Info", HideCargoInfo'Access);
      Do_Connect(Builder);
   end CreateCargoUI;

   procedure ShowCargoUI(OldState: GameStates) is
   begin
      RefreshCargoInfo;
      GameState := OldState;
      Show_All(Gtk_Widget(Get_Object(Builder, "cargowindow")));
      SetActiveItem;
   end ShowCargoUI;

end Ships.Cargo.UI;
