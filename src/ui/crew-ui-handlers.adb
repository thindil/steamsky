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

with Ada.Exceptions; use Ada.Exceptions;
with Gtkada.Builder; use Gtkada.Builder;
with Gtk.Widget; use Gtk.Widget;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Selection; use Gtk.Tree_Selection;
with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Label; use Gtk.Label;
with Gtk.Tree_View_Column; use Gtk.Tree_View_Column;
with Gtk.Button; use Gtk.Button;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Window; use Gtk.Window;
with Glib.Types; use Glib.Types;
with Glib.Properties; use Glib.Properties;
with Game; use Game;
with Maps; use Maps;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Ships.Cargo; use Ships.Cargo;
with Help.UI; use Help.UI;
with Messages; use Messages;
with Crew.Inventory; use Crew.Inventory;
with Bases; use Bases;

package body Crew.UI.Handlers is

   procedure ShowMemberInfo(Object: access Gtkada_Builder_Record'Class) is
      CrewIter: Gtk_Tree_Iter;
      CrewModel: Gtk_Tree_Model;
      Member: Member_Data;
      MemberInfo: Unbounded_String;
      TiredPoints: Integer;
      Iter: Gtk_Tree_Iter;
      List: Gtk_List_Store;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treecrew"))),
         CrewModel,
         CrewIter);
      if CrewIter = Null_Iter then
         return;
      end if;
      MemberIndex := Positive(Get_Int(CrewModel, CrewIter, 2));
      Member := PlayerShip.Crew(MemberIndex);
      if Member.Gender = 'M' then
         MemberInfo := To_Unbounded_String("Gender: Male");
      else
         MemberInfo := To_Unbounded_String("Gender: Female");
      end if;
      if Member.Skills.Length = 0 then
         Hide(Gtk_Widget(Get_Object(Object, "treestats")));
         Hide(Gtk_Widget(Get_Object(Object, "scrollskills")));
         Hide(Gtk_Widget(Get_Object(Object, "btnpriorities")));
         Hide(Gtk_Widget(Get_Object(Object, "btninventory")));
         Append(MemberInfo, ASCII.LF & "Passenger");
      else
         Show_All(Gtk_Widget(Get_Object(Object, "treestats")));
         Show_All(Gtk_Widget(Get_Object(Object, "scrollskills")));
         Show_All(Gtk_Widget(Get_Object(Object, "btnpriorities")));
         Show_All(Gtk_Widget(Get_Object(Object, "btninventory")));
      end if;
      if PlayerShip.Speed = DOCKED and MemberIndex > 1 then
         Show_All(Gtk_Widget(Get_Object(Object, "btndismiss")));
      else
         Hide(Gtk_Widget(Get_Object(Object, "btndismiss")));
      end if;
      if Member.Health < 100 and Member.Health > 80 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""green"">Slightly wounded</span>");
      elsif Member.Health < 81 and Member.Health > 50 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""yellow"">Wounded</span>");
      elsif Member.Health < 51 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""red"">Heavily Wounded</span>");
      end if;
      TiredPoints := Member.Tired - Member.Attributes(ConditionIndex)(1);
      if TiredPoints > 20 and TiredPoints < 41 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""green"">Bit tired</span>");
      elsif TiredPoints > 40 and TiredPoints < 81 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""yellow"">Tired</span>");
      elsif TiredPoints > 80 and TiredPoints < 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""red"">Very tired</span>");
      elsif TiredPoints = 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""blue"">Unconscious</span>");
      end if;
      if Member.Thirst > 20 and Member.Thirst < 41 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""green"">Bit thirsty</span>");
      elsif Member.Thirst > 40 and Member.Thirst < 81 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""yellow"">Thirsty</span>");
      elsif Member.Thirst > 80 and Member.Thirst < 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""red"">Very thirsty</span>");
      elsif Member.Thirst = 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""blue"">Dehydrated</span>");
      end if;
      if Member.Hunger > 20 and Member.Hunger < 41 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""green"">Bit hungry</span>");
      elsif Member.Hunger > 40 and Member.Hunger < 81 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""yellow"">Hungry</span>");
      elsif Member.Hunger > 80 and Member.Hunger < 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""red"">Very hungry</span>");
      elsif Member.Hunger = 100 then
         Append
           (MemberInfo,
            ASCII.LF & "<span foreground=""blue"">Starving</span>");
      end if;
      Set_Markup
        (Gtk_Label(Get_Object(Object, "lblinfo")),
         To_String(MemberInfo));
      if Member.Skills.Length > 0 then
         List := Gtk_List_Store(Get_Object(Builder, "statslist"));
         Clear(List);
         for I in Member.Attributes.Iterate loop
            Append(List, Iter);
            Set
              (List,
               Iter,
               0,
               To_String(Attributes_Names(Attributes_Container.To_Index(I))));
            Set(List, Iter, 1, Gint(Member.Attributes(I)(1) * 2));
         end loop;
         List := Gtk_List_Store(Get_Object(Builder, "skillslist"));
         Clear(List);
         for Skill of Member.Skills loop
            Append(List, Iter);
            Set(List, Iter, 0, To_String(Skills_List(Skill(1)).Name));
            Set(List, Iter, 1, Gint(Skill(2)));
         end loop;
      end if;
      SetOrdersList;
   end ShowMemberInfo;

   procedure ShowHelp(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
   begin
      ShowHelpUI(7);
   end ShowHelp;

   procedure GiveOrdersAll(User_Data: access GObject_Record'Class) is
      Order: Crew_Orders;
   begin
      if User_Data = Get_Object(Builder, "btnrepairall") then
         Order := Repair;
      else
         Order := Clean;
      end if;
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Crew(I).Skills.Length > 0 then
            begin
               GiveOrders(PlayerShip, I, Order);
            exception
               when An_Exception : Crew_Order_Error | Crew_No_Space_Error =>
                  AddMessage(Exception_Message(An_Exception), OrderMessage);
            end;
         end if;
      end loop;
      if LastMessage /= Null_Unbounded_String then
         Set_Text
           (Gtk_Label(Get_Object(Builder, "lbllastmessage")),
            To_String(LastMessage));
         Show_All(Gtk_Widget(Get_Object(Builder, "infolastmessage")));
         LastMessage := Null_Unbounded_String;
      end if;
   end GiveOrdersAll;

   procedure ShowInventory(Object: access Gtkada_Builder_Record'Class) is
   begin
      RefreshInventory;
      Show_All(Gtk_Widget(Get_Object(Object, "inventorywindow")));
      SetActiveItem;
   end ShowInventory;

   procedure ShowItemInfo(Object: access Gtkada_Builder_Record'Class) is
      InventoryIter: Gtk_Tree_Iter;
      InventoryModel: Gtk_Tree_Model;
      ItemInfo: Unbounded_String;
      ProtoIndex, ItemWeight: Positive;
      DamagePercent: Natural;
      ItemType: Unbounded_String;
   begin
      Get_Selected
        (Gtk.Tree_View.Get_Selection
           (Gtk_Tree_View(Get_Object(Object, "treeinventory"))),
         InventoryModel,
         InventoryIter);
      if InventoryIter = Null_Iter then
         return;
      end if;
      ItemIndex :=
        Natural'Value(To_String(Get_Path(InventoryModel, InventoryIter))) + 1;
      ProtoIndex :=
        PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex;
      ItemWeight :=
        PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Amount *
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
           (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Amount));
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
      if PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Durability <
        100 then
         DamagePercent :=
           100 -
           Natural
             ((Float
                 (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex)
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
        (Gtk_Label(Get_Object(Object, "lbliteminfo")),
         To_String(ItemInfo));
      if ItemIsUsed(MemberIndex, ItemIndex) then
         Set_Label
           (Gtk_Button(Get_Object(Object, "btnequip")),
            "Take off item");
      else
         ItemType :=
           Items_List
             (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
             .IType;
         if ItemType = WeaponType then
            Set_Label
              (Gtk_Button(Get_Object(Object, "btnequip")),
               "Use as weapon");
         elsif ItemType = ShieldType then
            Set_Label
              (Gtk_Button(Get_Object(Object, "btnequip")),
               "Use as shield");
         elsif ItemType = HeadArmor then
            Set_Label
              (Gtk_Button(Get_Object(Object, "btnequip")),
               "Use as helmet");
         elsif ItemType = ChestArmor then
            Set_Label
              (Gtk_Button(Get_Object(Object, "btnequip")),
               "Use as torso armor");
         elsif ItemType = ArmsArmor then
            Set_Label
              (Gtk_Button(Get_Object(Object, "btnequip")),
               "Use as arms armor");
         elsif ItemType = LegsArmor then
            Set_Label
              (Gtk_Button(Get_Object(Object, "btnequip")),
               "Use as legs armor");
         elsif Tools_List.Find_Index(Item => ItemType) /=
           UnboundedString_Container.No_Index then
            Set_Label
              (Gtk_Button(Get_Object(Object, "btnequip")),
               "Use as tool");
         end if;
      end if;
   end ShowItemInfo;

   procedure UseItem(Object: access Gtkada_Builder_Record'Class) is
      pragma Unreferenced(Object);
      ItemType: constant Unbounded_String :=
        Items_List
          (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
          .IType;
   begin
      if ItemIsUsed(MemberIndex, ItemIndex) then
         TakeOffItem(MemberIndex, ItemIndex);
      else
         if ItemType = WeaponType then
            PlayerShip.Crew(MemberIndex).Equipment(1) := ItemIndex;
         elsif ItemType = ShieldType then
            PlayerShip.Crew(MemberIndex).Equipment(2) := ItemIndex;
         elsif ItemType = HeadArmor then
            PlayerShip.Crew(MemberIndex).Equipment(3) := ItemIndex;
         elsif ItemType = ChestArmor then
            PlayerShip.Crew(MemberIndex).Equipment(4) := ItemIndex;
         elsif ItemType = ArmsArmor then
            PlayerShip.Crew(MemberIndex).Equipment(5) := ItemIndex;
         elsif ItemType = LegsArmor then
            PlayerShip.Crew(MemberIndex).Equipment(6) := ItemIndex;
         elsif Tools_List.Find_Index(Item => ItemType) /=
           UnboundedString_Container.No_Index then
            PlayerShip.Crew(MemberIndex).Equipment(7) := ItemIndex;
         end if;
      end if;
      RefreshInventory;
      SetActiveItem;
   end UseItem;

   procedure ShowMoveItem(Object: access Gtkada_Builder_Record'Class) is
      AmountAdj: constant Gtk_Adjustment :=
        Gtk_Adjustment(Get_Object(Object, "amountadj"));
   begin
      Set_Upper
        (AmountAdj,
         Gdouble(PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Amount));
      Set_Value(AmountAdj, 1.0);
      Show_All(Gtk_Widget(Get_Object(Builder, "moveitemwindow")));
   end ShowMoveItem;

   procedure MoveItem(Object: access Gtkada_Builder_Record'Class) is
      Amount: Positive;
      Item: constant InventoryData :=
        PlayerShip.Crew(MemberIndex).Inventory(ItemIndex);
   begin
      Amount :=
        Positive(Get_Value(Gtk_Adjustment(Get_Object(Object, "amountadj"))));
      if FreeCargo(0 - (Items_List(Item.ProtoIndex).Weight * Amount)) < 0 then
         ShowDialog
           ("No free space in ship cargo for that amount of " &
            GetItemName(Item),
            Gtk_Window(Get_Object(Object, "moveitemwindow")));
         return;
      end if;
      UpdateCargo(PlayerShip, Item.ProtoIndex, Amount, Item.Durability);
      UpdateInventory
        (MemberIndex => MemberIndex,
         Amount => (0 - Amount),
         InventoryIndex => ItemIndex);
      if
        (PlayerShip.Crew(MemberIndex).Order = Clean and
         FindItem
             (Inventory => PlayerShip.Crew(MemberIndex).Inventory,
              ItemType => CleaningTools) =
           0) or
        ((PlayerShip.Crew(MemberIndex).Order = Upgrading or
          PlayerShip.Crew(MemberIndex).Order = Repair) and
         FindItem
             (Inventory => PlayerShip.Crew(MemberIndex).Inventory,
              ItemType => RepairTools) =
           0) then
         GiveOrders(PlayerShip, MemberIndex, Rest);
      end if;
      Hide(Gtk_Widget(Get_Object(Builder, "moveitemwindow")));
      RefreshInventory;
      SetActiveItem;
   end MoveItem;

   procedure GiveCrewOrders
     (Self: access Gtk_Cell_Renderer_Combo_Record'Class;
      Path_String: UTF8_String;
      New_Iter: Gtk.Tree_Model.Gtk_Tree_Iter) is
      Model: Glib.Types.GType_Interface;
      List: Gtk_List_Store;
   begin
      Model := Get_Property(Self, Gtk.Cell_Renderer_Combo.Model_Property);
      List := -(Gtk_Tree_Model(Model));
      GiveOrders
        (PlayerShip,
         (Natural'Value(Path_String) + 1),
         Crew_Orders'Val(Get_Int(List, New_Iter, 1)),
         Natural(Get_Int(List, New_Iter, 2)));
      RefreshCrewInfo;
      ShowLastMessage(Builder);
      ShowOrdersForAll;
   end GiveCrewOrders;

   function UpdatePriorities
     (Model: Gtk_Tree_Model;
      Path: Gtk_Tree_Path;
      Iter: Gtk_Tree_Iter) return Boolean is
   begin
      case PlayerShip.Crew(MemberIndex).Orders
        (Positive'Value(To_String(Path)) + 1) is
         when 0 =>
            Set(-(Model), Iter, 1, "None");
         when 1 =>
            Set(-(Model), Iter, 1, "Normal");
         when 2 =>
            Set(-(Model), Iter, 1, "Highest");
         when others =>
            null;
      end case;
      return False;
   end UpdatePriorities;

   procedure ShowPriorities(Object: access Gtkada_Builder_Record'Class) is
   begin
      Foreach
        (Gtk_List_Store(Get_Object(Object, "prioritieslist")),
         UpdatePriorities'Access);
      Show_All(Gtk_Widget(Get_Object(Builder, "prioritieswindow")));
   end ShowPriorities;

   function ReducePriority
     (Model: Gtk_Tree_Model;
      Path: Gtk_Tree_Path;
      Iter: Gtk_Tree_Iter) return Boolean is
      pragma Unreferenced(Path);
   begin
      if Get_String(Model, Iter, 1) = "Highest" then
         Set(-(Model), Iter, 1, "Normal");
         return True;
      end if;
      return False;
   end ReducePriority;

   procedure SetPriority
     (Self: access Gtk_Cell_Renderer_Combo_Record'Class;
      Path_String: UTF8_String;
      New_Iter: Gtk.Tree_Model.Gtk_Tree_Iter) is
      Model: Glib.Types.GType_Interface;
      NewValue: Unbounded_String;
      PrioritiesList: constant Gtk_List_Store :=
        Gtk_List_Store(Get_Object(Builder, "prioritieslist"));
   begin
      Model := Get_Property(Self, Gtk.Cell_Renderer_Combo.Model_Property);
      NewValue :=
        To_Unbounded_String(Get_String(Gtk_Tree_Model(Model), New_Iter, 0));
      if NewValue = To_Unbounded_String("Highest") then
         Foreach(PrioritiesList, ReducePriority'Access);
      end if;
      Set
        (PrioritiesList,
         Get_Iter_From_String(PrioritiesList, Path_String),
         1,
         To_String(NewValue));
   end SetPriority;

   function SetEachPriority
     (Model: Gtk_Tree_Model;
      Path: Gtk_Tree_Path;
      Iter: Gtk_Tree_Iter) return Boolean is
      PriorityLevel: constant String := Get_String(Model, Iter, 1);
   begin
      if PriorityLevel = "Highest" then
         PlayerShip.Crew(MemberIndex).Orders
           (Positive'Value(To_String(Path)) + 1) :=
           2;
      elsif PriorityLevel = "Normal" then
         PlayerShip.Crew(MemberIndex).Orders
           (Positive'Value(To_String(Path)) + 1) :=
           1;
      else
         PlayerShip.Crew(MemberIndex).Orders
           (Positive'Value(To_String(Path)) + 1) :=
           0;
      end if;
      return False;
   end SetEachPriority;

   procedure SetPriorities(Object: access Gtkada_Builder_Record'Class) is
   begin
      Foreach
        (Gtk_List_Store(Get_Object(Object, "prioritieslist")),
         SetEachPriority'Access);
      UpdateOrders(PlayerShip);
      Hide(Gtk_Widget(Get_Object(Builder, "prioritieswindow")));
      RefreshCrewInfo;
      ShowLastMessage(Object);
      ShowOrdersForAll;
   end SetPriorities;

   procedure DismissMember(Object: access Gtkada_Builder_Record'Class) is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      if ShowConfirmDialog
          ("Are you sure want to dismiss this crew member?",
           Gtk_Window(Get_Object(Object, "crewwindow"))) then
         AddMessage
           ("You dismissed " &
            To_String(PlayerShip.Crew(MemberIndex).Name) &
            ".",
            OrderMessage);
         DeleteMember(MemberIndex, PlayerShip);
         SkyBases(BaseIndex).Population := SkyBases(BaseIndex).Population + 1;
         RefreshCrewInfo;
         ShowLastMessage(Object);
         ShowOrdersForAll;
         SetActiveMember;
      end if;
   end DismissMember;

end Crew.UI.Handlers;
