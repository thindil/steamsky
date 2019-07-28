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
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Label; use Gtk.Label;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Tree_Model_Filter; use Gtk.Tree_Model_Filter;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Progress_Bar; use Gtk.Progress_Bar;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Bases; use Bases;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships; use Ships;
with Utils; use Utils;
with Utils.UI; use Utils.UI;
with Factions; use Factions;
with Config; use Config;

package body BasesList is

   -- ****iv* BasesList/Builder
   -- FUNCTION
   -- Gtkada_Builder used for creating UI
   -- SOURCE
   Builder: Gtkada_Builder;
   -- ****
   -- ****iv* BasesList/SettingTime
   -- FUNCTION
   -- If true, UI is currently in setting state
   -- SOURCE
   SettingTime: Boolean;
   -- ****
   -- ****iv* BasesList/BaseIndex
   -- FUNCTION
   -- Index of sky base to show
   -- SOURCE
   BaseIndex: Positive;
   -- ****

   -- ****if* BasesList/ShowBaseInfo
   -- FUNCTION
   -- Show info about selected base
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowBaseInfo(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      BaseInfo: Unbounded_String;
      procedure SetReputationText(ReputationText: String) is
      begin
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Object, "negativereputationbar")),
            ReputationText);
         Set_Tooltip_Text
           (Gtk_Widget(Get_Object(Object, "positivereputationbar")),
            ReputationText);
      end SetReputationText;
   begin
      declare
         BasesIter: Gtk_Tree_Iter;
         BasesModel: Gtk_Tree_Model;
      begin
         Get_Selected
           (Gtk.Tree_View.Get_Selection
              (Gtk_Tree_View(Get_Object(Object, "treebases"))),
            BasesModel, BasesIter);
         if BasesIter = Null_Iter then
            Set_Visible
              (Gtk_Widget(Get_Object(Object, "baseinfoframe")), False);
            Set_Visible(Gtk_Widget(Get_Object(Object, "btnshowbase")), False);
            Set_Visible
              (Gtk_Widget(Get_Object(Object, "btndestinationbase")), False);
            return;
         end if;
         Set_Visible(Gtk_Widget(Get_Object(Object, "baseinfoframe")), True);
         Set_Visible(Gtk_Widget(Get_Object(Object, "btnshowbase")), True);
         Set_Visible
           (Gtk_Widget(Get_Object(Object, "btndestinationbase")), True);
         BaseIndex := Natural(Get_Int(BasesModel, BasesIter, 1));
      end;
      if not Is_Visible(Gtk_Widget(Get_Object(Object, "baseinfoscroll"))) then
         return;
      end if;
      if SkyBases(BaseIndex).Visited.Year > 0 then
         BaseInfo :=
           To_Unbounded_String
             ("X:" & Positive'Image(SkyBases(BaseIndex).SkyX) & " Y:" &
              Positive'Image(SkyBases(BaseIndex).SkyY));
         Append
           (BaseInfo,
            LF & "Last visited: " & FormatedTime(SkyBases(BaseIndex).Visited));
         declare
            TimeDiff: Integer;
         begin
            if SkyBases(BaseIndex).Population > 0 and
              SkyBases(BaseIndex).Reputation(1) > -25 then
               TimeDiff :=
                 30 - DaysDifference(SkyBases(BaseIndex).RecruitDate);
               if TimeDiff > 0 then
                  Append
                    (BaseInfo,
                     LF & "New recruits available in" &
                     Natural'Image(TimeDiff) & " days.");
               else
                  Append(BaseInfo, LF & "New recruits available now.");
               end if;
            else
               Append
                 (BaseInfo,
                  LF & "You can't recruit crew members at this base.");
            end if;
            if SkyBases(BaseIndex).Population > 0 and
              SkyBases(BaseIndex).Reputation(1) > -25 then
               TimeDiff := DaysDifference(SkyBases(BaseIndex).AskedForEvents);
               if TimeDiff < 7 then
                  Append
                    (BaseInfo,
                     LF & "You asked for events" & Natural'Image(TimeDiff) &
                     " days ago.");
               else
                  Append(BaseInfo, LF & "You can ask for events again.");
               end if;
            else
               Append(BaseInfo, LF & "You can't ask for events at this base.");
            end if;
            if SkyBases(BaseIndex).Population > 0 and
              SkyBases(BaseIndex).Reputation(1) > -1 then
               TimeDiff :=
                 7 - DaysDifference(SkyBases(BaseIndex).MissionsDate);
               if TimeDiff > 0 then
                  Append
                    (BaseInfo,
                     LF & "New missions available in" &
                     Natural'Image(TimeDiff) & " days.");
               else
                  Append(BaseInfo, LF & "New missions available now.");
               end if;
            else
               Append(BaseInfo, LF & "You can't take missions at this base.");
            end if;
         end;
         Set_Visible
           (Gtk_Widget(Get_Object(Object, "basereputationbox")), True);
         Set_Fraction
           (Gtk_Progress_Bar(Get_Object(Object, "negativereputationbar")),
            0.0);
         Set_Fraction
           (Gtk_Progress_Bar(Get_Object(Object, "positivereputationbar")),
            0.0);
         if SkyBases(BaseIndex).Reputation(1) < 0 then
            Set_Fraction
              (Gtk_Progress_Bar(Get_Object(Object, "negativereputationbar")),
               Gdouble(abs (SkyBases(BaseIndex).Reputation(1))) / 100.0);
         elsif SkyBases(BaseIndex).Reputation(1) > 0 then
            Set_Fraction
              (Gtk_Progress_Bar(Get_Object(Object, "positivereputationbar")),
               Gdouble(abs (SkyBases(BaseIndex).Reputation(1))) / 100.0);
         end if;
         case SkyBases(BaseIndex).Reputation(1) is
            when -100 .. -75 =>
               SetReputationText("Hated");
            when -74 .. -50 =>
               SetReputationText("Outlaw");
            when -49 .. -25 =>
               SetReputationText("Hostile");
            when -24 .. -1 =>
               SetReputationText("Unfriendly");
            when 0 =>
               SetReputationText("Unknown");
            when 1 .. 25 =>
               SetReputationText("Visitor");
            when 26 .. 50 =>
               SetReputationText("Trader");
            when 51 .. 75 =>
               SetReputationText("Friend");
            when 76 .. 100 =>
               SetReputationText("Well known");
            when others =>
               null;
         end case;
         if BaseIndex = PlayerShip.HomeBase then
            Append(BaseInfo, LF & "It is your home base.");
         end if;
      else
         BaseInfo := To_Unbounded_String("Not visited yet.");
         Set_Visible(Gtk_Widget(Get_Object(Object, "baseinfoframe")), False);
      end if;
      Set_Label
        (Gtk_Label(Get_Object(Object, "lblbaseinfo")), To_String(BaseInfo));
   end ShowBaseInfo;

   -- ****if* BasesList/SetDestinationBase
   -- FUNCTION
   -- Set selected base as the destination for the player ship
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure SetDestinationBase(Object: access Gtkada_Builder_Record'Class) is
      -- ****
   begin
      if SkyBases(BaseIndex).SkyX = PlayerShip.SkyX and
        SkyBases(BaseIndex).SkyY = PlayerShip.SkyY then
         ShowDialog("You are at this base now.");
         return;
      end if;
      PlayerShip.DestinationX := SkyBases(BaseIndex).SkyX;
      PlayerShip.DestinationY := SkyBases(BaseIndex).SkyY;
      AddMessage
        ("You set base " & To_String(SkyBases(BaseIndex).Name) &
         " as a destination for your ship.",
         OrderMessage);
      ShowSkyMap;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Object, "gamestack")), "skymap");
   end SetDestinationBase;

   -- ****if* BasesList/ShowBase
   -- FUNCTION
   -- Show selected base on the map
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ShowBase(Object: access Gtkada_Builder_Record'Class) is
      -- ****
   begin
      ShowSkyMap(SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY);
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Object, "gamestack")), "skymap");
   end ShowBase;

   -- ****if* BasesList/SearchBases
   -- FUNCTION
   -- Search base by it name
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure SearchBases(Object: access Gtkada_Builder_Record'Class) is
      -- ****
   begin
      Refilter(Gtk_Tree_Model_Filter(Get_Object(Object, "basesfilter")));
      if N_Children
          (Gtk_List_Store(Get_Object(Builder, "baseslist")), Null_Iter) >
        0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treebases")),
            Gtk_Tree_Path_New_From_String("0"),
            Gtk_Tree_View_Column(Get_Object(Builder, "columnnames1")), False);
      end if;
   end SearchBases;

   -- ****if* BasesList/VisibleBases
   -- FUNCTION
   -- Check if selected base is visible on bases list
   -- PARAMETERS
   -- Model - Gtk_Tree_Model with bases which will be checked
   -- Iter  - Gtk_Tree_Iter of base which will be checked
   -- RESULT
   -- True if base should be visible, otherwise false
   -- SOURCE
   function VisibleBases
     (Model: Gtk_Tree_Model; Iter: Gtk_Tree_Iter) return Boolean is
     -- ****
      ShowBase: Boolean := False;
      BasesType: Bases_Types;
      BasesStatus: Natural;
      BaseIndex: Positive;
      BasesOwner: Unbounded_String;
   begin
      if SettingTime then
         return True;
      end if;
      BasesType :=
        Bases_Types'Val
          (Get_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbtype"))));
      BasesStatus :=
        Natural(Get_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbstatus"))));
      BasesOwner :=
        To_Unbounded_String
          (Get_Active_Id(Gtk_Combo_Box(Get_Object(Builder, "cmbowner"))));
      BaseIndex := Positive(Get_Int(Model, Iter, 1));
      case BasesStatus is
         when 0 => -- All bases
            if BasesType = Any then
               if
                 (Factions_Container.Contains(Factions_List, BasesOwner) and
                  SkyBases(BaseIndex).Visited.Year > 0)
                 and then SkyBases(BaseIndex).Owner = BasesOwner then
                  ShowBase := True;
               elsif not Factions_Container.Contains
                   (Factions_List, BasesOwner) then
                  ShowBase := True;
               end if;
            elsif SkyBases(BaseIndex).Visited.Year > 0 and
              SkyBases(BaseIndex).BaseType = BasesType then
               if Factions_Container.Contains(Factions_List, BasesOwner)
                 and then SkyBases(BaseIndex).Owner = BasesOwner then
                  ShowBase := True;
               else
                  ShowBase := True;
               end if;
            end if;
         when 1 => -- Only visited bases
            if
              (BasesType = Any or
               (BasesType /= Any and
                SkyBases(BaseIndex).BaseType = BasesType)) and
              SkyBases(BaseIndex).Visited.Year > 0 then
               if Factions_Container.Contains(Factions_List, BasesOwner)
                 and then SkyBases(BaseIndex).Owner = BasesOwner then
                  ShowBase := True;
               elsif not Factions_Container.Contains
                   (Factions_List, BasesOwner) then
                  ShowBase := True;
               end if;
            end if;
         when 2 => -- Only not visited bases
            if SkyBases(BaseIndex).Visited.Year = 0 then
               ShowBase := True;
            end if;
         when others =>
            null;
      end case;
      if not ShowBase then
         return False;
      end if;
      declare
         SearchEntry: constant Gtk_GEntry :=
           Gtk_GEntry(Get_Object(Builder, "entrysearchbases"));
      begin
         if Get_Text(SearchEntry) = "" then
            return True;
         end if;
         if Index
             (To_Lower(Get_String(Model, Iter, 0)),
              To_Lower(Get_Text(SearchEntry)), 1) >
           0 then
            return True;
         end if;
      end;
      return False;
   end VisibleBases;

   -- ****if* BasesList/ToggleBaseInfo
   -- FUNCTION
   -- Show or hide detailed info about selected base
   -- PARAMETERS
   -- Object - Gtkada_Builder used to create UI
   -- SOURCE
   procedure ToggleBaseInfo(Object: access Gtkada_Builder_Record'Class) is
      -- ****
      InfoWidget: constant Gtk_Widget :=
        Gtk_Widget(Get_Object(Object, "baseinfoscroll"));
      MenuItem: constant Gtk_Menu_Item :=
        Gtk_Menu_Item(Get_Object(Object, "hidebaseinfomenu"));
   begin
      if Is_Visible(InfoWidget) then
         Hide(InfoWidget);
         Set_Label(MenuItem, "S_how base info");
         GameSettings.ShowBaseInfo := False;
      else
         Show_All(InfoWidget);
         Set_Label(MenuItem, "_Hide base info");
         ShowBaseInfo(Object);
         GameSettings.ShowBaseInfo := True;
      end if;
   end ToggleBaseInfo;

   procedure CreateBasesListUI(NewBuilder: Gtkada_Builder) is
      ComboBox: Gtk_Combo_Box_Text;
   begin
      Builder := NewBuilder;
      Register_Handler(Builder, "Show_Base_Info", ShowBaseInfo'Access);
      Register_Handler
        (Builder, "Set_Destination_Base", SetDestinationBase'Access);
      Register_Handler(Builder, "Show_Base", ShowBase'Access);
      Register_Handler(Builder, "Search_Bases", SearchBases'Access);
      Register_Handler(Builder, "Show_Popup_Menu", ShowPopupMenu'Access);
      Register_Handler(Builder, "Toggle_Base_Info", ToggleBaseInfo'Access);
      ComboBox := Gtk_Combo_Box_Text(Get_Object(Builder, "cmbtype"));
      for I in Bases_Types loop
         Append_Text
           (ComboBox,
            Bases_Types'Image(I)(1) &
            To_Lower(Bases_Types'Image(I)(2 .. Bases_Types'Image(I)'Last)));
      end loop;
      ComboBox := Gtk_Combo_Box_Text(Get_Object(Builder, "cmbowner"));
      for I in Factions_List.Iterate loop
         Append
           (ComboBox, To_String(Factions_Container.Key(I)),
            To_String(Factions_List(I).Name));
      end loop;
      Append
        (Gtk_Combo_Box_Text(Get_Object(Builder, "cmbowner")), "Any", "Any");
      Set_Visible_Func
        (Gtk_Tree_Model_Filter(Get_Object(Builder, "basesfilter")),
         VisibleBases'Access);
      On_Key_Press_Event
        (Gtk_Widget(Get_Object(Builder, "entrysearchbases")),
         SelectElement'Access, Get_Object(Builder, "btnmenu"));
      Attach_To_Widget
        (Gtk_Menu(Get_Object(Builder, "baseslistmenu")),
         Gtk_Widget(Get_Object(Builder, "treebases")), null);
      On_Button_Release_Event
        (Gtk_Widget(Get_Object(Builder, "treebases")),
         ShowPopupMenuButton'Access);
   end CreateBasesListUI;

   procedure ShowBasesListUI is
      BaseIter: Gtk_Tree_Iter;
      BaseList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "baseslist"));
   begin
      Set_Active
        (Gtk_Combo_Box(Get_Object(Builder, "cmbtype")),
         Bases_Types'Pos(Bases_Types'Last));
      Set_Active(Gtk_Combo_Box(Get_Object(Builder, "cmbstatus")), 0);
      if not Set_Active_Id
          (Gtk_Combo_Box(Get_Object(Builder, "cmbowner")), "Any") then
         return;
      end if;
      Set_Visible_Child_Name
        (Gtk_Stack(Get_Object(Builder, "gamestack")), "baseslist");
      SettingTime := True;
      Clear(BaseList);
      for I in SkyBases'Range loop
         if SkyBases(I).Known then
            Append(BaseList, BaseIter);
            Set(BaseList, BaseIter, 0, To_String(SkyBases(I).Name));
            Set(BaseList, BaseIter, 1, Gint(I));
            Set
              (BaseList, BaseIter, 2,
               Gint(CountDistance(SkyBases(I).SkyX, SkyBases(I).SkyY)));
            if SkyBases(I).Visited.Year /= 0 then
               if SkyBases(I).Population = 0 then
                  Set(BaseList, BaseIter, 3, "empty");
               elsif SkyBases(I).Population < 150 then
                  Set(BaseList, BaseIter, 3, "small");
               elsif SkyBases(I).Population < 300 then
                  Set(BaseList, BaseIter, 3, "medium");
               else
                  Set(BaseList, BaseIter, 3, "large");
               end if;
               Set(BaseList, BaseIter, 4, Gint(SkyBases(I).Population));
               Set
                 (BaseList, BaseIter, 5,
                  To_Lower(Bases_Size'Image(SkyBases(I).Size)));
               Set
                 (BaseList, BaseIter, 6,
                  To_String(Factions_List(SkyBases(I).Owner).Name));
               Set
                 (BaseList, BaseIter, 7,
                  To_Lower(Bases_Types'Image(SkyBases(I).BaseType)));
            else
               Set(BaseList, BaseIter, 3, "not visited");
               Set(BaseList, BaseIter, 4, -1);
               Set(BaseList, BaseIter, 5, "not visited");
               Set(BaseList, BaseIter, 6, "not visited");
               Set(BaseList, BaseIter, 7, "not visited");
            end if;
         end if;
      end loop;
      if GameSettings.ShowBaseInfo then
         Show_All(Gtk_Widget(Get_Object(Builder, "baseinfoscroll")));
         Set_Label
           (Gtk_Menu_Item(Get_Object(Builder, "hidebaseinfomenu")),
            "_Hide base info");
      else
         Hide(Gtk_Widget(Get_Object(Builder, "baseinfoscroll")));
         Set_Label
           (Gtk_Menu_Item(Get_Object(Builder, "hidebaseinfomenu")),
            "S_how base info");
      end if;
      if N_Children(BaseList, Null_Iter) > 0 then
         Set_Cursor
           (Gtk_Tree_View(Get_Object(Builder, "treebases")),
            Gtk_Tree_Path_New_From_String("0"), null, False);
      end if;
      Set_Text(Gtk_GEntry(Get_Object(Builder, "entrysearchbases")), "");
      SettingTime := False;
   end ShowBasesListUI;

end BasesList;
