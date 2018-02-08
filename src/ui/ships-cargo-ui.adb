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
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Gdk.RGBA; use Gdk.RGBA;
with Maps.UI; use Maps.UI;
with Combat.UI; use Combat.UI;
with Messages; use Messages;

package body Ships.Cargo.UI is

   Builder: Gtkada_Builder;
   GameState: GameStates;
   ItemIndex: Positive;

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
         "Free cargo space:" & Integer'Image(FreeCargo(0)) & " kg");
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

   procedure ShowItemInfo(Object: access Gtkada_Builder_Record'Class) is
      CargoIter: Gtk_Tree_Iter;
      CargoModel: Gtk_Tree_Model;
      ItemInfo: Unbounded_String;
      ProtoIndex, ItemWeight: Positive;
      DamagePercent: Natural;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treecargo"))),
         CargoModel,
         CargoIter);
      if CargoIter = Null_Iter then
         return;
      end if;
      ItemIndex :=
        Natural'Value(To_String(Get_Path(CargoModel, CargoIter))) + 1;
      ProtoIndex :=
        PlayerShip.Cargo(ItemIndex).ProtoIndex;
      ItemWeight :=
        PlayerShip.Cargo(ItemIndex).Amount *
        Items_List(ProtoIndex).Weight;
      ItemInfo := To_Unbounded_String("Type: ");
      if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
         Append(ItemInfo, Items_List(ProtoIndex).IType);
      else
         Append(ItemInfo, Items_List(ProtoIndex).ShowType);
      end if;
      Append
        (ItemInfo,
         ASCII.LF &
         "Amount:" &
         Positive'Image
           (PlayerShip.Cargo(ItemIndex).Amount));
      Append
        (ItemInfo,
         ASCII.LF &
         "Weight:" &
         Positive'Image(Items_List(ProtoIndex).Weight) &
         " kg");
      Append
        (ItemInfo,
         ASCII.LF & "Total weight:" & Positive'Image(ItemWeight) & " kg");
      if Items_List(ProtoIndex).IType = WeaponType then
         Append
           (ItemInfo,
            ASCII.LF &
            "Skill: " &
            Skills_List(Items_List(ProtoIndex).Value(3)).Name &
            "/" &
            Attributes_Names
              (Skills_List(Items_List(ProtoIndex).Value(3)).Attribute));
      end if;
      if PlayerShip.Cargo(ItemIndex).Durability <
        100 then
         DamagePercent :=
           100 -
           Natural
             ((Float
                 (PlayerShip.Cargo(ItemIndex)
                    .Durability) /
               100.0) *
              100.0);
         Append(ItemInfo, ASCII.LF & "Status: ");
         case DamagePercent is
            when 1 .. 19 =>
               Append
                 (ItemInfo,
                  "<span foreground=""green"">Slightly used</span>");
            when 20 .. 49 =>
               Append(ItemInfo, "<span foreground=""yellow"">Damaged</span>");
            when 50 .. 79 =>
               Append
                 (ItemInfo,
                  "<span foreground=""red"">Heavily damaged</span>");
            when others =>
               Append
                 (ItemInfo,
                  "<span foreground=""blue"">Almost destroyed</span>");
         end case;
      end if;
      if Items_List(ProtoIndex).Description /= Null_Unbounded_String then
         Append
           (ItemInfo,
            ASCII.LF & ASCII.LF & Items_List(ProtoIndex).Description);
      end if;
      Set_Markup
        (Gtk_Label(Get_Object(Object, "lblinfo")),
         To_String(ItemInfo));
   end ShowItemInfo;

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
      Register_Handler(Builder, "Hide_Last_Message", HideLastMessage'Access);
      Register_Handler(Builder, "Show_Item_Info", ShowItemInfo'Access);
      Do_Connect(Builder);
   end CreateCargoUI;

   procedure ShowCargoUI(OldState: GameStates) is
   begin
      RefreshCargoInfo;
      GameState := OldState;
      Show_All(Gtk_Widget(Get_Object(Builder, "cargowindow")));
      ShowLastMessage;
      SetActiveItem;
   end ShowCargoUI;

end Ships.Cargo.UI;
