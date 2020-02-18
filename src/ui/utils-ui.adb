--    Copyright 2018-2020 Bartek thindil Jasicki
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Gtk.Message_Dialog; use Gtk.Message_Dialog;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Paned; use Gtk.Paned;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Mark; use Gtk.Text_Mark;
with Gtk.Text_Tag_Table; use Gtk.Text_Tag_Table;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gdk.Types; use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Glib.Main; use Glib.Main;
with MainMenu; use MainMenu;
with Game; use Game;
with Maps.UI; use Maps.UI;
with Maps.UI.Handlers; use Maps.UI.Handlers;
with Combat.UI; use Combat.UI;
with GameOptions; use GameOptions;
with Statistics.UI; use Statistics.UI;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Ships.Cargo; use Ships.Cargo;
with Items; use Items;
with Messages; use Messages;
with Config; use Config;
with Crew; use Crew;
with Factions; use Factions;

package body Utils.UI is

   -- ****iv* Utils.UI/Builder
   -- FUNCTION
   -- Gtkada_Builder used for creating UI
   -- SOURCE
   Builder: Gtkada_Builder;
   -- ****

   -- ****iv* Utils.UI/Source_Id
   -- FUNCTION
   -- Id of timer used to hide mesages. Default is No_Source_Id
   -- SOURCE
   Source_Id: G_Source_Id := No_Source_Id;
   -- ****

   -- ****if* Utils.UI/AutoHideDialog
   -- FUNCTION
   -- Auto hide message after some time
   -- SOURCE
   function AutoHideDialog return Boolean is
   -- ****
   begin
      Hide(MessageBox);
      Source_Id := No_Source_Id;
      if Get_Object(Builder, "btnclose") /= null then
         EnableMenuShortcutsProc(Builder);
         Set_Sensitive(Gtk_Button(Get_Object(Builder, "btnclose")), True);
      end if;
      return False;
   end AutoHideDialog;

   procedure HideDialog
     (Self: access Gtk_Info_Bar_Record'Class; Response_Id: Glib.Gint) is
      pragma Unreferenced(Response_Id);
   begin
      Hide(Self);
      Remove(Source_Id);
      Source_Id := No_Source_Id;
      if Get_Object(Builder, "btnclose") /= null then
         EnableMenuShortcutsProc(Builder);
         Set_Sensitive(Gtk_Button(Get_Object(Builder, "btnclose")), True);
      end if;
   end HideDialog;

   procedure ShowDialog(Message: String) is
   begin
      Set_Label
        (Gtk_Label(Get_Child(Gtk_Box(Get_Content_Area(MessageBox)), 0)),
         Message);
      Show_All(MessageBox);
      if Get_Object(Builder, "btnclose") /= null then
         DisableMenuShortcutsProc(Builder);
         Set_Sensitive(Gtk_Button(Get_Object(Builder, "btnclose")), False);
      end if;
      if Source_Id /= No_Source_Id then
         Remove(Source_Id);
         Source_Id := No_Source_Id;
      end if;
      Source_Id :=
        Timeout_Add
          (Guint(GameSettings.AutoCloseMessagesTime) * 1000,
           AutoHideDialog'Access);
   end ShowDialog;

   function HideWindow
     (User_Data: access GObject_Record'Class) return Boolean is
   begin
      return Hide_On_Delete(Gtk_Widget(User_Data));
   end HideWindow;

   procedure ShowWindow(User_Data: access GObject_Record'Class) is
   begin
      if not Get_Visible(Gtk_Widget(User_Data)) then
         Show_All(Gtk_Widget(User_Data));
         if User_Data = Get_Object(Builder, "moremovemapbox") then
            Hide(Gtk_Widget(Get_Object(Builder, "btnboxorders")));
            Hide(Gtk_Widget(Get_Object(Builder, "btnboxwait")));
            Grab_Focus(Gtk_Widget(Get_Object(Builder, "spinx")));
         end if;
      else
         Hide(Gtk_Widget(User_Data));
      end if;
   end ShowWindow;

   function ShowConfirmDialog
     (Message: String; Parent: Gtk_Window) return Boolean is
      MessageDialog: constant Gtk_Message_Dialog :=
        Gtk_Message_Dialog_New
          (Parent, Modal, Message_Question, Buttons_Yes_No, Message);
   begin
      if Run(MessageDialog) = Gtk_Response_Yes then
         Destroy(MessageDialog);
         return True;
      end if;
      Destroy(MessageDialog);
      return False;
   end ShowConfirmDialog;

   function QuitGame(User_Data: access GObject_Record'Class) return Boolean is
   begin
      if ShowConfirmDialog
          ("Are you sure want to quit?", Gtk_Window(User_Data)) then
         EndGame(True);
         ShowMainMenu;
         return Hide_On_Delete(Gtk_Widget(User_Data));
      end if;
      return True;
   end QuitGame;

   function CloseWindow
     (Self: access Gtk_Widget_Record'Class; Event: Gdk_Event_Key)
      return Boolean is
      KeyMods: constant Gdk_Modifier_Type :=
        Event.State and Get_Default_Mod_Mask;
   begin
      if KeyMods = 0 and Event.Keyval = GDK_Escape then
         Close(Gtk_Window(Self));
         return False;
      end if;
      return True;
   end CloseWindow;

   procedure CloseMessages(Object: access Gtkada_Builder_Record'Class) is
      VisibleChildName: constant String :=
        Get_Visible_Child_Name(Gtk_Stack(Get_Object(Object, "gamestack")));
      MenuArray: constant array(1 .. 10) of Unbounded_String :=
        (To_Unbounded_String("menuorders"),
         To_Unbounded_String("menucrafting"),
         To_Unbounded_String("menubaseslist"),
         To_Unbounded_String("menuevents"),
         To_Unbounded_String("menumissions"), To_Unbounded_String("menustory"),
         To_Unbounded_String("menuwait"), To_Unbounded_String("menustats"),
         To_Unbounded_String("menuhelp"), To_Unbounded_String("menuoptions"));
   begin
      if VisibleChildName = "options" or VisibleChildName = "lastmessages" then
         Set_Position
           (Gtk_Paned(Get_Object(Builder, "gamepaned")),
            Gint(GameSettings.MessagesPosition));
      end if;
      if VisibleChildName = "options" then
         CloseOptions(Object);
         return;
      end if;
      if VisibleChildName = "inventory" then
         Set_Visible_Child_Name
           (Gtk_Stack(Get_Object(Object, "gamestack")), "crew");
         return;
      end if;
      if VisibleChildName = "gamestats" or PlayerShip.Crew(1).Health = 0 then
         HideStatistics;
         return;
      end if;
      if VisibleChildName = "combat" then
         PreviousGameState := SkyMap_View;
         Set_Sensitive(Gtk_Widget(Get_Object(Object, "treecrew1")), True);
         for I in MenuArray'Range loop
            Show_All(Gtk_Widget(Get_Object(Object, To_String(MenuArray(I)))));
         end loop;
         UpdateOrders(PlayerShip);
      end if;
      Hide(Gtk_Widget(Get_Object(Object, "btnclose")));
      case PreviousGameState is
         when SkyMap_View =>
            ShowSkyMap;
            if PlayerShip.Speed = DOCKED then
               ShowOrders(Object);
            end if;
         when Combat_View =>
            ShowCombatUI(False);
         when Main_Menu =>
            null;
      end case;
   end CloseMessages;

   function SelectElement
     (Self: access GObject_Record'Class; Event: Gdk_Event_Key)
      return Boolean is
      KeyMods: constant Gdk_Modifier_Type :=
        Event.State and Get_Default_Mod_Mask;
   begin
      if KeyMods = 0 and
        (Event.Keyval = GDK_Return or Event.Keyval = GDK_Escape) then
         Grab_Focus(Gtk_Widget(Self));
         return True;
      end if;
      return False;
   end SelectElement;

   procedure TravelInfo
     (InfoText: in out Unbounded_String; Distance: Positive;
      ShowFuelName: Boolean := False) is
      type SpeedType is digits 2;
      Speed: constant SpeedType :=
        (SpeedType(RealSpeed(PlayerShip, True)) / 1000.0);
      MinutesDiff: Integer;
      Rests, CabinIndex, RestTime: Natural := 0;
      Damage: DamageFactor := 0.0;
      Tired, CabinBonus, TempTime: Natural;
   begin
      if Speed = 0.0 then
         Append(InfoText, LF & "ETA: Never");
         return;
      end if;
      MinutesDiff := Integer(100.0 / Speed);
      case PlayerShip.Speed is
         when QUARTER_SPEED =>
            if MinutesDiff < 60 then
               MinutesDiff := 60;
            end if;
         when HALF_SPEED =>
            if MinutesDiff < 30 then
               MinutesDiff := 30;
            end if;
         when FULL_SPEED =>
            if MinutesDiff < 15 then
               MinutesDiff := 15;
            end if;
         when others =>
            null;
      end case;
      Append(InfoText, LF & "ETA:");
      MinutesDiff := MinutesDiff * Distance;
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Order = Pilot or
           PlayerShip.Crew(I).Order = Engineer then
            Tired := (MinutesDiff / 15) + PlayerShip.Crew(I).Tired;
            if
              (Tired /
               (80 + PlayerShip.Crew(I).Attributes(ConditionIndex)(1))) >
              Rests then
               Rests :=
                 (Tired /
                  (80 + PlayerShip.Crew(I).Attributes(ConditionIndex)(1)));
            end if;
            if Rests > 0 then
               CabinIndex := FindCabin(Crew_Container.To_Index(I));
               if CabinIndex > 0 then
                  Damage :=
                    1.0 -
                    DamageFactor
                      (Float(PlayerShip.Modules(CabinIndex).Durability) /
                       Float(PlayerShip.Modules(CabinIndex).MaxDurability));
                  CabinBonus :=
                    PlayerShip.Modules(CabinIndex).Cleanliness -
                    Natural
                      (Float(PlayerShip.Modules(CabinIndex).Cleanliness) *
                       Float(Damage));
                  if CabinBonus = 0 then
                     CabinBonus := 1;
                  end if;
                  TempTime :=
                    ((80 + PlayerShip.Crew(I).Attributes(ConditionIndex)(1)) /
                     CabinBonus) *
                    15;
                  if TempTime = 0 then
                     TempTime := 15;
                  end if;
               else
                  TempTime :=
                    (80 + PlayerShip.Crew(I).Attributes(ConditionIndex)(1)) *
                    15;
               end if;
               TempTime := TempTime + 15;
               if TempTime > RestTime then
                  RestTime := TempTime;
               end if;
            end if;
         end if;
      end loop;
      MinutesDiff := MinutesDiff + (Rests * RestTime);
      MinutesToDate(MinutesDiff, InfoText);
      Append
        (InfoText,
         LF & "Approx fuel usage:" &
         Natural'Image
           (abs (Distance * CountFuelNeeded) + (Rests * (RestTime / 10))) &
         " ");
      if ShowFuelName then
         Append
           (InfoText, Items_List(FindProtoItem(ItemType => FuelType)).Name);
      end if;
   end TravelInfo;

   procedure MinutesToDate
     (Minutes: Natural; InfoText: in out Unbounded_String) is
      TravelTime: Date_Record := (others => 0);
      MinutesDiff: Integer := Minutes;
   begin
      while MinutesDiff > 0 loop
         if MinutesDiff >= 518400 then
            TravelTime.Year := TravelTime.Year + 1;
            MinutesDiff := MinutesDiff - 518400;
         elsif MinutesDiff >= 43200 then
            TravelTime.Month := TravelTime.Month + 1;
            MinutesDiff := MinutesDiff - 43200;
         elsif MinutesDiff >= 1440 then
            TravelTime.Day := TravelTime.Day + 1;
            MinutesDiff := MinutesDiff - 1440;
         elsif MinutesDiff >= 60 then
            TravelTime.Hour := TravelTime.Hour + 1;
            MinutesDiff := MinutesDiff - 60;
         else
            TravelTime.Minutes := MinutesDiff;
            MinutesDiff := 0;
         end if;
      end loop;
      if TravelTime.Year > 0 then
         Append(InfoText, Positive'Image(TravelTime.Year) & "y");
      end if;
      if TravelTime.Month > 0 then
         Append(InfoText, Positive'Image(TravelTime.Month) & "m");
      end if;
      if TravelTime.Day > 0 then
         Append(InfoText, Positive'Image(TravelTime.Day) & "d");
      end if;
      if TravelTime.Hour > 0 then
         Append(InfoText, Positive'Image(TravelTime.Hour) & "h");
      end if;
      if TravelTime.Minutes > 0 then
         Append(InfoText, Positive'Image(TravelTime.Minutes) & "mins");
      end if;
   end MinutesToDate;

   procedure ShowInventoryItemInfo
     (Label: Gtk_Label; ItemIndex: Positive; MemberIndex: Natural) is
      ProtoIndex: Unbounded_String;
      ItemInfo: Unbounded_String;
      ItemTypes: constant array(Positive range <>) of Unbounded_String :=
        (WeaponType, ChestArmor, HeadArmor, ArmsArmor, LegsArmor, ShieldType);
   begin
      if MemberIndex > 0 then
         ProtoIndex :=
           PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex;
      else
         ProtoIndex := PlayerShip.Cargo(ItemIndex).ProtoIndex;
      end if;
      Append
        (ItemInfo,
         "Weight:" & Positive'Image(Items_List(ProtoIndex).Weight) & " kg");
      if Items_List(ProtoIndex).IType = WeaponType then
         Append
           (ItemInfo,
            LF & "Skill: " &
            Skills_List(Items_List(ProtoIndex).Value(3)).Name & "/" &
            Attributes_List
              (Skills_List(Items_List(ProtoIndex).Value(3)).Attribute)
              .Name);
         if Items_List(ProtoIndex).Value(4) = 1 then
            Append(ItemInfo, LF & "Can be used with shield.");
         else
            Append
              (ItemInfo,
               LF & "Can't be used with shield (two-handed weapon).");
         end if;
         Append(ItemInfo, LF & "Damage type: ");
         case Items_List(ProtoIndex).Value(5) is
            when 1 =>
               Append(ItemInfo, "cutting");
            when 2 =>
               Append(ItemInfo, "impaling");
            when 3 =>
               Append(ItemInfo, "blunt");
            when others =>
               null;
         end case;
      end if;
      for ItemType of ItemTypes loop
         if Items_List(ProtoIndex).IType = ItemType then
            Append
              (ItemInfo,
               LF & "Damage chance: " &
               GetItemChanceToDamage(Items_List(ProtoIndex).Value(1)));
            Append
              (ItemInfo,
               LF & "Strength:" &
               Integer'Image(Items_List(ProtoIndex).Value(2)));
            exit;
         end if;
      end loop;
      if Tools_List.Contains(Items_List(ProtoIndex).IType) then
         Append
           (ItemInfo,
            LF & "Damage chance: " &
            GetItemChanceToDamage(Items_List(ProtoIndex).Value(1)));
      end if;
      if Length(Items_List(ProtoIndex).IType) > 4
        and then
        (Slice(Items_List(ProtoIndex).IType, 1, 4) = "Ammo" or
         Items_List(ProtoIndex).IType = To_Unbounded_String("Harpoon")) then
         Append
           (ItemInfo,
            LF & "Strength:" & Integer'Image(Items_List(ProtoIndex).Value(1)));
      end if;
      if Items_List(ProtoIndex).Description /= Null_Unbounded_String then
         Append
           (ItemInfo, LF & LF & To_String(Items_List(ProtoIndex).Description));
      end if;
      Set_Markup(Label, To_String(ItemInfo));
   end ShowInventoryItemInfo;

   procedure HideItemInfo(User_Data: access GObject_Record'Class) is
      ItemInfoBox: constant Gtk_Widget := Gtk_Widget(User_Data);
   begin
      Set_Visible(ItemInfoBox, not Get_Visible(ItemInfoBox));
      if User_Data = Get_Object(Builder, "boxcargoiteminfo") then
         GameSettings.ShowCargoInfo := not GameSettings.ShowCargoInfo;
      elsif User_Data = Get_Object(Builder, "boxinventoryiteminfo") then
         GameSettings.ShowInventoryInfo := not GameSettings.ShowInventoryInfo;
      end if;
   end HideItemInfo;

   function ShowPopupMenu
     (User_Data: access GObject_Record'Class) return Boolean is
   begin
      Popup(Gtk_Menu(User_Data));
      return False;
   end ShowPopupMenu;

   function ShowPopupMenuButton
     (Self: access Gtk_Widget_Record'Class; Event: Gdk_Event_Button)
      return Boolean is
   begin
      if Event.Button = 3 then
         if Self = Gtk_Widget(Get_Object(Builder, "treebases")) then
            return ShowPopupMenu(Get_Object(Builder, "baseslistmenu"));
         elsif Self = Gtk_Widget(Get_Object(Builder, "treemissions1")) then
            return ShowPopupMenu(Get_Object(Builder, "acceptedmissionsmenu"));
         elsif Self = Gtk_Widget(Get_Object(Builder, "treeevents")) then
            return ShowPopupMenu(Get_Object(Builder, "eventsmenu"));
         elsif Self = Gtk_Widget(Get_Object(Builder, "treemissions")) then
            return ShowPopupMenu(Get_Object(Builder, "availablemissionsmenu"));
         end if;
      end if;
      return False;
   end ShowPopupMenuButton;

   procedure SetUtilsBuilder(NewBuilder: Gtkada_Builder) is
   begin
      Builder := NewBuilder;
   end SetUtilsBuilder;

   procedure UpdateMessages is
      MessagesBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Builder, "txtmessages"));
      LoopStart: Integer := 0 - MessagesAmount;
      Message: Message_Data;
      Iter: Gtk_Text_Iter;
      TagNames: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String("yellow"), To_Unbounded_String("green"),
         To_Unbounded_String("red"), To_Unbounded_String("blue"),
         To_Unbounded_String("cyan"));
      procedure ShowMessage is
      begin
         if Message.Color = WHITE then
            Insert(MessagesBuffer, Iter, To_String(Message.Message));
         else
            Insert_With_Tags
              (MessagesBuffer, Iter, To_String(Message.Message),
               Lookup
                 (Get_Tag_Table(MessagesBuffer),
                  To_String(TagNames(Message_Color'Pos(Message.Color)))));
         end if;
      end ShowMessage;
   begin
      Set_Text(MessagesBuffer, "");
      Get_Start_Iter(MessagesBuffer, Iter);
      if LoopStart = 0 then
         return;
      end if;
      if LoopStart < -10 then
         LoopStart := -10;
      end if;
      if GameSettings.MessagesOrder = OLDER_FIRST then
         for I in LoopStart .. -1 loop
            Message := GetMessage(I + 1);
            ShowMessage;
            if I < -1 then
               Insert(MessagesBuffer, Iter, "" & LF);
            end if;
         end loop;
         declare
            Mark: Gtk_Text_Mark;
         begin
            Mark := Create_Mark(MessagesBuffer, "end", Iter);
            Scroll_Mark_Onscreen
              (Gtk_Text_View(Get_Object(Builder, "messagesview")), Mark);
         end;
      else
         for I in reverse LoopStart .. -1 loop
            Message := GetMessage(I + 1);
            ShowMessage;
            if I > LoopStart then
               Insert(MessagesBuffer, Iter, "" & LF);
            end if;
         end loop;
      end if;
   end UpdateMessages;

   procedure CheckAmount(User_Data: access GObject_Record'Class) is
      CargoIndex: Natural;
      TreeName, AdjustmentName, LabelName, WarningText: Unbounded_String;
      Amount: Integer;
   begin
      if User_Data = Get_Object(Builder, "spintradesell") then
         TreeName := To_Unbounded_String("treeitems1");
         AdjustmentName := To_Unbounded_String("amountadj");
         LabelName := To_Unbounded_String("lblsellwarning");
         WarningText :=
           To_Unbounded_String("You will sell amount below low level of ");
      elsif User_Data = Get_Object(Builder, "spincargodrop") then
         TreeName := To_Unbounded_String("treecargo");
         AdjustmentName := To_Unbounded_String("amountadj");
         LabelName := To_Unbounded_String("lbldropwarning");
         WarningText :=
           To_Unbounded_String("You will drop amount below low level of ");
      else
         TreeName := To_Unbounded_String("treecargo");
         AdjustmentName := To_Unbounded_String("amountadj1");
         LabelName := To_Unbounded_String("lblgivewarning");
         WarningText :=
           To_Unbounded_String("You will drop amount below low level of ");
      end if;
      declare
         ItemsIter: Gtk_Tree_Iter;
         ItemsModel: Gtk_Tree_Model;
      begin
         Get_Selected
           (Gtk.Tree_View.Get_Selection
              (Gtk_Tree_View(Get_Object(Builder, To_String(TreeName)))),
            ItemsModel, ItemsIter);
         if ItemsIter = Null_Iter then
            return;
         end if;
         CargoIndex := Natural(Get_Int(ItemsModel, ItemsIter, 1));
      end;
      if CargoIndex not in
          PlayerShip.Cargo.First_Index .. PlayerShip.Cargo.Last_Index then
         return;
      end if;
      if User_Data /= Get_Object(Builder, "spintradesell")
        and then Items_List(PlayerShip.Cargo(CargoIndex).ProtoIndex).IType =
          FuelType then
         Amount :=
           GetItemAmount(FuelType) -
           Natural
             (Get_Value
                (Gtk_Adjustment
                   (Get_Object(Builder, To_String(AdjustmentName)))));
         if Amount <= GameSettings.LowFuel then
            Set_Label
              (Gtk_Label(Get_Object(Builder, To_String(LabelName))),
               To_String(WarningText) & "fuel.");
            Show_All(Gtk_Widget(Get_Object(Builder, To_String(LabelName))));
            return;
         end if;
      end if;
      for Member of PlayerShip.Crew loop
         if Factions_List(Member.Faction).DrinksTypes.Contains
             (Items_List(PlayerShip.Cargo(CargoIndex).ProtoIndex).IType) then
            Amount :=
              GetItemsAmount("Drinks") -
              Natural
                (Get_Value
                   (Gtk_Adjustment
                      (Get_Object(Builder, To_String(AdjustmentName)))));
            if Amount <= GameSettings.LowDrinks then
               Set_Label
                 (Gtk_Label(Get_Object(Builder, To_String(LabelName))),
                  To_String(WarningText) & "drinks.");
               Show_All(Gtk_Widget(Get_Object(Builder, To_String(LabelName))));
               return;
            end if;
            exit;
         elsif Factions_List(Member.Faction).FoodTypes.Contains
             (Items_List(PlayerShip.Cargo(CargoIndex).ProtoIndex).IType) then
            Amount :=
              GetItemsAmount("Food") -
              Natural
                (Get_Value
                   (Gtk_Adjustment
                      (Get_Object(Builder, To_String(AdjustmentName)))));
            if Amount <= GameSettings.LowFood then
               Set_Label
                 (Gtk_Label(Get_Object(Builder, To_String(LabelName))),
                  To_String(WarningText) & "food.");
               Show_All(Gtk_Widget(Get_Object(Builder, To_String(LabelName))));
               return;
            end if;
            exit;
         end if;
      end loop;
      Hide(Gtk_Widget(Get_Object(Builder, To_String(LabelName))));
   end CheckAmount;

   procedure RemoveWidget(Widget: not null access Gtk_Widget_Record'Class) is
   begin
      Destroy(Widget);
   end RemoveWidget;

end Utils.UI;
