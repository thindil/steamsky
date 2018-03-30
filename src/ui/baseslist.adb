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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Label; use Gtk.Label;
with Gtk.Window; use Gtk.Window;
with Gtk.Stack; use Gtk.Stack;
with Glib; use Glib;
with Game; use Game;
with Bases; use Bases;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships; use Ships;
with Utils.UI; use Utils.UI;

package body BasesList is

   Builder: Gtkada_Builder;
   SettingTime: Boolean;
   BaseIndex: Positive;

   procedure RefreshBasesList(Object: access Gtkada_Builder_Record'Class) is
      BaseIter: Gtk_Tree_Iter;
      BaseList: Gtk_List_Store;
      AddBase: Boolean := False;
      BasesType: Bases_Types;
      BasesStatus: Natural;
      BasesOwner: Bases_Owners;
   begin
      if SettingTime then
         return;
      end if;
      BasesType :=
        Bases_Types'Val
          (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbtype"))));
      BasesStatus :=
        Natural(Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbstatus"))));
      BasesOwner :=
        Bases_Owners'Val
          (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbowner"))));
      BaseList := Gtk_List_Store(Get_Object(Builder, "baseslist"));
      Clear(BaseList);
      for I in SkyBases'Range loop
         if SkyBases(I).Known then
            case BasesStatus is
               when 0 => -- All bases
                  if
                    (BasesType = Any or
                     (BasesType /= Any and
                      SkyBases(I).Visited.Year > 0 and
                      SkyBases(I).BaseType = BasesType)) and
                    (BasesOwner = Any or
                     (BasesOwner /= Any and
                      SkyBases(I).Visited.Year > 0 and
                      SkyBases(I).Owner = BasesOwner)) then
                     AddBase := True;
                  end if;
               when 1 => -- Only visited bases
                  if
                    ((BasesType = Any or
                      (BasesType /= Any and
                       SkyBases(I).BaseType = BasesType)) and
                     (BasesOwner = Any or
                      (BasesOwner /= Any and
                       SkyBases(I).Owner = BasesOwner))) and
                    SkyBases(I).Visited.Year > 0 then
                     AddBase := True;
                  end if;
               when 2 => -- Only not visited bases
                  if SkyBases(I).Visited.Year = 0 then
                     AddBase := True;
                  end if;
               when others =>
                  null;
            end case;
            if AddBase then
               Append(BaseList, BaseIter);
               Set(BaseList, BaseIter, 0, To_String(SkyBases(I).Name));
               Set(BaseList, BaseIter, 1, Gint(I));
               Set
                 (BaseList,
                  BaseIter,
                  2,
                  Gint(CountDistance(SkyBases(I).SkyX, SkyBases(I).SkyY)));
               AddBase := False;
            end if;
         end if;
      end loop;
      if N_Children(BaseList, Null_Iter) > 0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treebases")),
            Gtk_Tree_Path_New_From_String("0"),
            Gtk_Tree_View_Column(Get_Object(Builder, "columnnames1")),
            False);
         Set_Visible(Gtk_Widget(Get_Object(Object, "baseinfoframe")), True);
         Set_Visible(Gtk_Widget(Get_Object(Object, "btnshowbase")), True);
         Set_Visible
           (Gtk_Widget(Get_Object(Object, "btndestinationbase")),
            True);
      else
         Set_Visible(Gtk_Widget(Get_Object(Object, "baseinfoframe")), False);
         Set_Visible(Gtk_Widget(Get_Object(Object, "btnshowbase")), False);
         Set_Visible
           (Gtk_Widget(Get_Object(Object, "btndestinationbase")),
            False);
      end if;
   end RefreshBasesList;

   procedure ShowBaseInfo(Object: access Gtkada_Builder_Record'Class) is
      BasesIter: Gtk_Tree_Iter;
      BasesModel: Gtk_Tree_Model;
      BaseInfo: Unbounded_String;
      TimeDiff: Integer;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treebases"))),
         BasesModel,
         BasesIter);
      if BasesIter = Null_Iter then
         return;
      end if;
      BaseIndex := Natural(Get_Int(BasesModel, BasesIter, 1));
      if SkyBases(BaseIndex).Visited.Year > 0 then
         BaseInfo :=
           To_Unbounded_String
             ("X:" &
              Positive'Image(SkyBases(BaseIndex).SkyX) &
              " Y:" &
              Positive'Image(SkyBases(BaseIndex).SkyY));
         Append
           (BaseInfo,
            ASCII.LF &
            "Type: " &
            To_Lower(Bases_Types'Image(SkyBases(BaseIndex).BaseType)));
         Append
           (BaseInfo,
            ASCII.LF &
            "Owner: " &
            To_Lower(Bases_Owners'Image(SkyBases(BaseIndex).Owner)));
         Append(BaseInfo, ASCII.LF & "Size: ");
         if SkyBases(BaseIndex).Population < 150 then
            Append(BaseInfo, "small");
         elsif SkyBases(BaseIndex).Population > 149 and
           SkyBases(BaseIndex).Population < 300 then
            Append(BaseInfo, "medium");
         else
            Append(BaseInfo, "large");
         end if;
         Append
           (BaseInfo,
            ASCII.LF &
            "Last visited: " &
            FormatedTime(SkyBases(BaseIndex).Visited));
         if SkyBases(BaseIndex).Owner /= Abandoned and
           SkyBases(BaseIndex).Reputation(1) > -25 then
            TimeDiff := 30 - DaysDifference(SkyBases(BaseIndex).RecruitDate);
            if TimeDiff > 0 then
               Append
                 (BaseInfo,
                  ASCII.LF &
                  "New recruits available in" &
                  Natural'Image(TimeDiff) &
                  " days.");
            else
               Append(BaseInfo, ASCII.LF & "New recruits available now.");
            end if;
         else
            Append
              (BaseInfo,
               ASCII.LF & "You can't recruit crew members on this base.");
         end if;
         if SkyBases(BaseIndex).Owner /= Abandoned and
           SkyBases(BaseIndex).Reputation(1) > -25 then
            TimeDiff := DaysDifference(SkyBases(BaseIndex).AskedForEvents);
            if TimeDiff < 7 then
               Append
                 (BaseInfo,
                  ASCII.LF &
                  "You asked for events" &
                  Natural'Image(TimeDiff) &
                  " days ago.");
            else
               Append(BaseInfo, ASCII.LF & "You can ask for events again.");
            end if;
         else
            Append
              (BaseInfo,
               ASCII.LF & "You can't ask for events in this base.");
         end if;
         if SkyBases(BaseIndex).Owner /= Abandoned and
           SkyBases(BaseIndex).Reputation(1) > -1 then
            TimeDiff := 7 - DaysDifference(SkyBases(BaseIndex).MissionsDate);
            if TimeDiff > 0 then
               Append
                 (BaseInfo,
                  ASCII.LF &
                  "New missions available in" &
                  Natural'Image(TimeDiff) &
                  " days.");
            else
               Append(BaseInfo, ASCII.LF & "New missions available now.");
            end if;
         else
            Append
              (BaseInfo,
               ASCII.LF & "You can't take missions in this base.");
         end if;
         Append(BaseInfo, ASCII.LF & "Reputation: ");
         case SkyBases(BaseIndex).Reputation(1) is
            when -100 .. -75 =>
               Append(BaseInfo, "Hated");
            when -74 .. -50 =>
               Append(BaseInfo, "Outlaw");
            when -49 .. -25 =>
               Append(BaseInfo, "Hostile");
            when -24 .. -1 =>
               Append(BaseInfo, "Unfriendly");
            when 0 =>
               Append(BaseInfo, "Unknown");
            when 1 .. 25 =>
               Append(BaseInfo, "Visitor");
            when 26 .. 50 =>
               Append(BaseInfo, "Trader");
            when 51 .. 75 =>
               Append(BaseInfo, "Friend");
            when 76 .. 100 =>
               Append(BaseInfo, "Well known");
            when others =>
               null;
         end case;
      else
         BaseInfo := To_Unbounded_String("Not visited yet.");
      end if;
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblbaseinfo")),
         To_String(BaseInfo));
   end ShowBaseInfo;

   procedure SetDestinationBase(Object: access Gtkada_Builder_Record'Class) is
   begin
      if SkyBases(BaseIndex).SkyX = PlayerShip.SkyX and
        SkyBases(BaseIndex).SkyY = PlayerShip.SkyY then
         ShowDialog
           ("You are at this base now.",
            Gtk_Window(Get_Object(Object, "skymapwindow")));
         return;
      end if;
      PlayerShip.DestinationX := SkyBases(BaseIndex).SkyX;
      PlayerShip.DestinationY := SkyBases(BaseIndex).SkyY;
      AddMessage
        ("You set base " &
         To_String(SkyBases(BaseIndex).Name) &
         " as a destination for your ship.",
         OrderMessage);
      CreateSkyMap;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")),
         "skymap");
      Set_Deletable(Gtk_Window(Get_Object(Builder, "skymapwindow")), True);
   end SetDestinationBase;

   procedure ShowBase(Object: access Gtkada_Builder_Record'Class) is
   begin
      CreateSkyMap(SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY);
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Object, "gamestack")),
         "skymap");
      Set_Deletable(Gtk_Window(Get_Object(Object, "skymapwindow")), True);
   end ShowBase;

   procedure CreateBasesListUI(NewBuilder: Gtkada_Builder) is
      Iter: Gtk_Tree_Iter;
      List: Gtk_List_Store;
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Hide_Bases_List", HideInfo'Access);
      Register_Handler(Builder, "Refresh_Bases_List", RefreshBasesList'Access);
      Register_Handler(Builder, "Show_Base_Info", ShowBaseInfo'Access);
      Register_Handler
        (Builder,
         "Set_Destination_Base",
         SetDestinationBase'Access);
      Register_Handler(Builder, "Show_Base", ShowBase'Access);
      List := Gtk_List_Store(Get_Object(Builder, "typeslist1"));
      for I in Bases_Types loop
         Append(List, Iter);
         Set
           (List,
            Iter,
            0,
            Bases_Types'Image(I)(1) &
            To_Lower(Bases_Types'Image(I)(2 .. Bases_Types'Image(I)'Last)));
      end loop;
      List := Gtk_List_Store(Get_Object(Builder, "ownerslist"));
      for I in Bases_Owners loop
         Append(List, Iter);
         Set
           (List,
            Iter,
            0,
            Bases_Owners'Image(I)(1) &
            To_Lower(Bases_Owners'Image(I)(2 .. Bases_Owners'Image(I)'Last)));
      end loop;
   end CreateBasesListUI;

   procedure ShowBasesListUI is
   begin
      SettingTime := True;
      Set_Active
        (Gtk_Combo_Box(Get_Object(Builder, "cmbtype")),
         Bases_Types'Pos(Bases_Types'Last));
      Set_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbstatus")), 0);
      Set_Active
        (Gtk_Combo_Box(Get_Object(Builder, "cmbowner")),
         Bases_Owners'Pos(Bases_Owners'Last));
      SettingTime := False;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")),
         "baseslist");
      RefreshBasesList(Builder);
      Set_Deletable(Gtk_Window(Get_Object(Builder, "skymapwindow")), False);
   end ShowBasesListUI;

end BasesList;
