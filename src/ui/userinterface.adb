--    Copyright 2016-2017 Bartek thindil Jasicki
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
with Ada.Containers; use Ada.Containers;
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Forms.Field_Types.IntField;
use Terminal_Interface.Curses.Forms.Field_Types.IntField;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Ships.UI; use Ships.UI;
with Ships.UI.Cargo; use Ships.UI.Cargo;
with Ships.UI.Ship; use Ships.UI.Ship;
with Crew; use Crew;
with Crew.UI; use Crew.UI;
with Bases; use Bases;
with Bases.UI.Repair; use Bases.UI.Repair;
with Bases.UI.Shipyard; use Bases.UI.Shipyard;
with Bases.UI.Recruits; use Bases.UI.Recruits;
with Bases.UI.Recipes; use Bases.UI.Recipes;
with Bases.UI.Missions; use Bases.UI.Missions;
with Bases.UI.Heal; use Bases.UI.Heal;
with Bases.UI.Loot; use Bases.UI.Loot;
with Messages; use Messages;
with Messages.UI; use Messages.UI;
with Combat; use Combat;
with Combat.UI; use Combat.UI;
with Crafts; use Crafts;
with Crafts.UI; use Crafts.UI;
with Help.UI; use Help.UI;
with Events; use Events;
with Events.UI; use Events.UI;
with ShipModules; use ShipModules;
with BasesList; use BasesList;
with Items; use Items;
with Statistics.UI; use Statistics.UI;
with Missions; use Missions;
with Missions.UI; use Missions.UI;
with GameOptions; use GameOptions;
with Config; use Config;
with Header; use Header;
with Trades.UI; use Trades.UI;
with DeathScreen; use DeathScreen;
with Utils.UI; use Utils.UI;

package body UserInterface is

   DialogPanel: Panel := Null_Panel;

   procedure ShowOrdersMenu is
      Orders_Items: Item_Array_Access;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      Event: Events_Types := None;
      TimeDiff, BaseIndex, ItemIndex: Natural;
      MenuIndex, OrdersAmount: Positive;
      MissionsLimit: Integer;
      HaveTrader: Boolean := False;
   begin
      BaseIndex := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      if FindMember(Talk) > 0 then
         HaveTrader := True;
      end if;
      if PlayerShip.Speed = DOCKED then
         OrdersAmount := 4;
         MenuIndex := 2;
         if HaveTrader and SkyBases(BaseIndex).Owner /= Abandoned then
            case SkyBases(BaseIndex).Reputation(1) is
               when 0 .. 25 =>
                  MissionsLimit := 1;
               when 26 .. 50 =>
                  MissionsLimit := 3;
               when 51 .. 75 =>
                  MissionsLimit := 5;
               when 76 .. 100 =>
                  MissionsLimit := 10;
               when others =>
                  MissionsLimit := 0;
            end case;
            for Mission of PlayerShip.Missions loop
               if (Mission.Finished and Mission.StartBase = BaseIndex) or
                 (Mission.TargetX = PlayerShip.SkyX and
                  Mission.TargetY = PlayerShip.SkyY) then
                  OrdersAmount := OrdersAmount + 1;
               end if;
               if Mission.StartBase = BaseIndex then
                  MissionsLimit := MissionsLimit - 1;
               end if;
            end loop;
            if SkyBases(BaseIndex).Recruits.Length > 0 then
               OrdersAmount := OrdersAmount + 1;
            end if;
            TimeDiff := DaysDifference(SkyBases(BaseIndex).AskedForEvents);
            if TimeDiff > 6 then
               OrdersAmount := OrdersAmount + 1;
            end if;
            if not SkyBases(BaseIndex).AskedForBases then
               OrdersAmount := OrdersAmount + 1;
            end if;
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 then
                  OrdersAmount := OrdersAmount + 1;
                  exit;
               end if;
            end loop;
            for Module of PlayerShip.Modules loop
               if Module.Durability < Module.MaxDurability then
                  OrdersAmount := OrdersAmount + 1;
                  exit;
               end if;
            end loop;
            if SkyBases(BaseIndex).BaseType = Shipyard then
               OrdersAmount := OrdersAmount + 1;
            end if;
            for I in Recipes_List.First_Index .. Recipes_List.Last_Index loop
               if Known_Recipes.Find_Index(Item => I) =
                 Positive_Container.No_Index and
                 Recipes_List(I).BaseType =
                   Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1 then
                  OrdersAmount := OrdersAmount + 1;
                  exit;
               end if;
            end loop;
            if Integer(SkyBases(BaseIndex).Missions.Length) > 0 and
              MissionsLimit > 0 then
               OrdersAmount := OrdersAmount + 1;
            end if;
            if PlayerShip.HomeBase /= BaseIndex then
               OrdersAmount := OrdersAmount + 1;
            end if;
         end if;
         Orders_Items := new Item_Array(1 .. OrdersAmount);
         Orders_Items.all(1) := New_Item("Undock");
         if SkyBases(BaseIndex).Owner = Abandoned then
            Orders_Items.all(2) := New_Item("Loot");
            MenuIndex := MenuIndex + 1;
         end if;
         if HaveTrader and SkyBases(BaseIndex).Owner /= Abandoned then
            for Mission of PlayerShip.Missions loop
               if (Mission.Finished and Mission.StartBase = BaseIndex) or
                 (Mission.TargetX = PlayerShip.SkyX and
                  Mission.TargetY = PlayerShip.SkyY) then
                  case Mission.MType is
                     when Deliver =>
                        Orders_Items.all(MenuIndex) :=
                          New_Item
                            ("Complete delivery of " &
                             To_String(Items_List(Mission.Target).Name));
                        MenuIndex := MenuIndex + 1;
                     when Destroy =>
                        if Mission.Finished then
                           Orders_Items.all(MenuIndex) :=
                             New_Item
                               ("Complete destroy " &
                                To_String
                                  (ProtoShips_List(Mission.Target).Name));
                           MenuIndex := MenuIndex + 1;
                        end if;
                     when Patrol =>
                        if Mission.Finished then
                           Orders_Items.all(MenuIndex) :=
                             New_Item("Complete Patrol area mission");
                           MenuIndex := MenuIndex + 1;
                        end if;
                     when Explore =>
                        if Mission.Finished then
                           Orders_Items.all(MenuIndex) :=
                             New_Item("Complete Explore area mission");
                           MenuIndex := MenuIndex + 1;
                        end if;
                     when Passenger =>
                        if Mission.Finished then
                           Orders_Items.all(MenuIndex) :=
                             New_Item("Complete Transport passenger mission");
                           MenuIndex := MenuIndex + 1;
                        end if;
                  end case;
               end if;
            end loop;
            Orders_Items.all(MenuIndex) := New_Item("Trade");
            MenuIndex := MenuIndex + 1;
            if SkyBases(BaseIndex).Recruits.Length > 0 then
               Orders_Items.all(MenuIndex) := New_Item("Recruit");
               MenuIndex := MenuIndex + 1;
            end if;
            if not SkyBases(BaseIndex).AskedForBases then
               Orders_Items.all(MenuIndex) := New_Item("Ask for bases");
               MenuIndex := MenuIndex + 1;
            end if;
            if TimeDiff > 6 then
               Orders_Items.all(MenuIndex) := New_Item("Ask for events");
               MenuIndex := MenuIndex + 1;
            end if;
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 then
                  Orders_Items.all(MenuIndex) := New_Item("Heal wounded");
                  MenuIndex := MenuIndex + 1;
                  exit;
               end if;
            end loop;
            for Module of PlayerShip.Modules loop
               if Module.Durability < Module.MaxDurability then
                  Orders_Items.all(MenuIndex) := New_Item("Repair");
                  MenuIndex := MenuIndex + 1;
                  exit;
               end if;
            end loop;
            if SkyBases(BaseIndex).BaseType = Shipyard then
               Orders_Items.all(MenuIndex) := New_Item("Shipyard");
               MenuIndex := MenuIndex + 1;
            end if;
            for I in Recipes_List.First_Index .. Recipes_List.Last_Index loop
               if Known_Recipes.Find_Index(Item => I) =
                 Positive_Container.No_Index and
                 Recipes_List(I).BaseType =
                   Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1 then
                  Orders_Items.all(MenuIndex) := New_Item("Buy recipes");
                  MenuIndex := MenuIndex + 1;
                  exit;
               end if;
            end loop;
            if Integer(SkyBases(BaseIndex).Missions.Length) > 0 and
              MissionsLimit > 0 then
               Orders_Items.all(MenuIndex) := New_Item("Missions");
               MenuIndex := MenuIndex + 1;
            end if;
            if PlayerShip.HomeBase /= BaseIndex then
               Orders_Items.all(MenuIndex) := New_Item("Set as home");
               MenuIndex := MenuIndex + 1;
            end if;
         end if;
      else
         OrdersAmount := 6;
         MenuIndex := 1;
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
            Event :=
              Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                .EType;
         end if;
         if Event = None or Event = DoublePrice then
            if BaseIndex > 0 then
               if SkyBases(BaseIndex).Reputation(1) > -25 then
                  OrdersAmount := OrdersAmount + 1;
               end if;
               if HaveTrader then
                  for Mission of PlayerShip.Missions loop
                     if (Mission.Finished and Mission.StartBase = BaseIndex) or
                       (Mission.TargetX = PlayerShip.SkyX and
                        Mission.TargetY = PlayerShip.SkyY) then
                        OrdersAmount := OrdersAmount + 1;
                     end if;
                  end loop;
               end if;
            else
               for Mission of PlayerShip.Missions loop
                  if
                    (Mission.TargetX = PlayerShip.SkyX and
                     Mission.TargetY = PlayerShip.SkyY) and
                    Mission.MType /= Deliver and
                    not Mission.Finished then
                     OrdersAmount := OrdersAmount + 1;
                  end if;
               end loop;
            end if;
         elsif Event = Disease then
            if HaveTrader then
               ItemIndex :=
                 FindItem
                   (Inventory => PlayerShip.Cargo,
                    ItemType => HealingTools);
               if ItemIndex > 0 then
                  OrdersAmount := OrdersAmount + 2;
               end if;
            end if;
         elsif Event = Trader then
            if HaveTrader then
               OrdersAmount := OrdersAmount + 4;
            end if;
         elsif Event = FriendlyShip then
            if HaveTrader then
               if Index
                   (ProtoShips_List
                      (Events_List
                         (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                         .Data)
                      .Name,
                    To_String(TradersName)) >
                 0 then
                  OrdersAmount := OrdersAmount + 4;
               else
                  OrdersAmount := OrdersAmount + 2;
               end if;
            end if;
         else
            OrdersAmount := OrdersAmount + 1;
         end if;
         Orders_Items := new Item_Array(1 .. OrdersAmount);
         if BaseIndex > 0 and (Event = None or Event = DoublePrice) then
            if SkyBases(BaseIndex).Reputation(1) > -25 then
               Orders_Items.all(MenuIndex) := New_Item("Dock");
               MenuIndex := MenuIndex + 1;
            end if;
         end if;
         case Event is
            when EnemyShip | EnemyPatrol =>
               Orders_Items.all(MenuIndex) := New_Item("Attack");
               MenuIndex := MenuIndex + 1;
            when FullDocks =>
               Orders_Items.all(MenuIndex) := New_Item("Wait");
               MenuIndex := MenuIndex + 1;
            when AttackOnBase =>
               Orders_Items.all(MenuIndex) := New_Item("Defend");
               MenuIndex := MenuIndex + 1;
            when Disease =>
               if HaveTrader then
                  ItemIndex :=
                    FindItem
                      (Inventory => PlayerShip.Cargo,
                       ItemType => HealingTools);
                  if ItemIndex > 0 then
                     Orders_Items.all(MenuIndex) :=
                       New_Item("Deliver medicines for free");
                     MenuIndex := MenuIndex + 1;
                     Orders_Items.all(MenuIndex) :=
                       New_Item("Deliver medicines for price");
                     MenuIndex := MenuIndex + 1;
                  end if;
               end if;
            when None | DoublePrice | BaseRecovery =>
               if BaseIndex > 0 then
                  for Mission of PlayerShip.Missions loop
                     if HaveTrader then
                        case Mission.MType is
                           when Deliver =>
                              if Mission.TargetX = PlayerShip.SkyX and
                                Mission.TargetY = PlayerShip.SkyY then
                                 Orders_Items.all(MenuIndex) :=
                                   New_Item
                                     ("Complete delivery of " &
                                      To_String
                                        (Items_List(Mission.Target).Name));
                                 MenuIndex := MenuIndex + 1;
                              end if;
                           when Destroy =>
                              if Mission.Finished and
                                Mission.StartBase = BaseIndex then
                                 Orders_Items.all(MenuIndex) :=
                                   New_Item
                                     ("Complete destroy " &
                                      To_String
                                        (ProtoShips_List(Mission.Target)
                                           .Name));
                                 MenuIndex := MenuIndex + 1;
                              end if;
                           when Patrol =>
                              if Mission.Finished and
                                Mission.StartBase = BaseIndex then
                                 Orders_Items.all(MenuIndex) :=
                                   New_Item("Complete Patrol area mission");
                                 MenuIndex := MenuIndex + 1;
                              end if;
                           when Explore =>
                              if Mission.Finished and
                                Mission.StartBase = BaseIndex then
                                 Orders_Items.all(MenuIndex) :=
                                   New_Item("Complete Explore area mission");
                                 MenuIndex := MenuIndex + 1;
                              end if;
                           when Passenger =>
                              if Mission.Finished and
                                Mission.StartBase = BaseIndex then
                                 Orders_Items.all(MenuIndex) :=
                                   New_Item
                                     ("Complete Transport passenger mission");
                                 MenuIndex := MenuIndex + 1;
                              end if;
                        end case;
                     end if;
                  end loop;
               else
                  for Mission of PlayerShip.Missions loop
                     if Mission.TargetX = PlayerShip.SkyX and
                       Mission.TargetY = PlayerShip.SkyY and
                       not Mission.Finished then
                        case Mission.MType is
                           when Deliver | Passenger =>
                              null;
                           when Destroy =>
                              Orders_Items.all(MenuIndex) :=
                                New_Item
                                  ("Search for " &
                                   To_String
                                     (ProtoShips_List(Mission.Target).Name));
                              MenuIndex := MenuIndex + 1;
                           when Patrol =>
                              Orders_Items.all(MenuIndex) :=
                                New_Item("Patrol area");
                              MenuIndex := MenuIndex + 1;
                           when Explore =>
                              Orders_Items.all(MenuIndex) :=
                                New_Item("Explore area");
                              MenuIndex := MenuIndex + 1;
                        end case;
                     end if;
                  end loop;
               end if;
            when Trader =>
               if HaveTrader then
                  Orders_Items.all(MenuIndex) := New_Item("Trade");
                  MenuIndex := MenuIndex + 1;
                  Orders_Items.all(MenuIndex) := New_Item("Ask for bases");
                  MenuIndex := MenuIndex + 1;
                  Orders_Items.all(MenuIndex) := New_Item("Ask for events");
                  MenuIndex := MenuIndex + 1;
               end if;
               Orders_Items.all(MenuIndex) := New_Item("Attack");
               MenuIndex := MenuIndex + 1;
            when FriendlyShip =>
               if HaveTrader then
                  if Index
                      (ProtoShips_List
                         (Events_List
                            (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY)
                               .EventIndex)
                            .Data)
                         .Name,
                       To_String(TradersName)) >
                    0 then
                     Orders_Items.all(MenuIndex) := New_Item("Trade");
                     MenuIndex := MenuIndex + 1;
                     Orders_Items.all(MenuIndex) := New_Item("Ask for bases");
                     MenuIndex := MenuIndex + 1;
                  end if;
                  Orders_Items.all(MenuIndex) := New_Item("Ask for events");
                  MenuIndex := MenuIndex + 1;
               end if;
               Orders_Items.all(MenuIndex) := New_Item("Attack");
               MenuIndex := MenuIndex + 1;
         end case;
         Orders_Items.all(MenuIndex) := New_Item("All stop");
         MenuIndex := MenuIndex + 1;
         Orders_Items.all(MenuIndex) := New_Item("Quarter speed");
         MenuIndex := MenuIndex + 1;
         Orders_Items.all(MenuIndex) := New_Item("Half speed");
         MenuIndex := MenuIndex + 1;
         Orders_Items.all(MenuIndex) := New_Item("Full speed");
         MenuIndex := MenuIndex + 1;
      end if;
      Orders_Items.all(MenuIndex) := New_Item("Close");
      MenuIndex := MenuIndex + 1;
      Orders_Items.all(MenuIndex) := Null_Item;
      OrdersMenu := New_Menu(Orders_Items);
      Set_Format(OrdersMenu, Lines - 4, 1);
      Scale(OrdersMenu, MenuHeight, MenuLength);
      if MenuLength < 10 then
         MenuLength := 10;
      end if;
      MenuWindow :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow, 5, "Orders");
      Set_Window(OrdersMenu, MenuWindow);
      Set_Sub_Window
        (OrdersMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 1, 1));
      Post(OrdersMenu);
      Refresh_Without_Update;
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
   end ShowOrdersMenu;

   procedure ShowConfirm(Message: String) is
      ConfirmWindow: Window;
      Width: Positive;
      Height: Positive := 1;
   begin
      Width := Message'Length + 8;
      if Width >= Positive(Columns - 4) then
         Height := (Width / Positive(Columns - 4) + 2);
         Width := (Width / Height) + 2;
      end if;
      Height := Height + 2;
      ConfirmWindow :=
        New_Window
          (Line_Position(Height),
           Column_Position(Width),
           ((Lines / 2) - Line_Position(Height / 2)),
           ((Columns / 2) - Column_Position(Width / 2)));
      Set_Color(ConfirmWindow, 10);
      Box(ConfirmWindow);
      Add
        (Win => ConfirmWindow,
         Str => Message & " (Y/N)",
         Line => 1,
         Column => 1);
      Set_Color(ConfirmWindow, Color_Pair'First);
      Refresh(ConfirmWindow);
   end ShowConfirm;

   procedure ShowDialog(Message: String; DType: Dialog_Types := ERROR) is
      DialogWindow: Window;
      Width: Positive;
      Height, StartIndex, EndIndex: Positive := 1;
      CurrentLine: Line_Position := 1;
   begin
      Width := Message'Length + 2;
      if Width >= Positive(Columns - 4) then
         Height := (Width / Positive(Columns - 4)) + 1;
         Width := Positive(Columns - 4);
      end if;
      Height := Height + 2;
      DialogWindow :=
        New_Window
          (Line_Position(Height),
           Column_Position(Width),
           ((Lines / 2) - Line_Position(Height / 2)),
           ((Columns / 2) - Column_Position(Width / 2)));
      case DType is
         when ERROR =>
            Set_Color(DialogWindow, 8);
         when WARNING =>
            Set_Color(DialogWindow, 9);
         when INFO =>
            Set_Color(DialogWindow, 10);
      end case;
      Box(DialogWindow);
      if Height = 3 then
         Add(Win => DialogWindow, Str => Message, Line => 1, Column => 1);
      else
         while StartIndex < Message'Length loop
            EndIndex := StartIndex + Width - 3;
            if EndIndex > Message'Length then
               EndIndex := Message'Length;
            end if;
            Add
              (Win => DialogWindow,
               Str => Message(StartIndex .. EndIndex),
               Line => CurrentLine,
               Column => 1);
            CurrentLine := CurrentLine + 1;
            StartIndex := EndIndex;
         end loop;
      end if;
      Set_Color(DialogWindow, Color_Pair'First);
      if DialogPanel = Null_Panel then
         DialogPanel := New_Panel(DialogWindow);
      else
         Replace(DialogPanel, DialogWindow);
      end if;
      if Is_Hidden(DialogPanel) then
         Show(DialogPanel);
      end if;
   end ShowDialog;

   function HideDialog return Boolean is
   begin
      if not Is_Hidden(DialogPanel) then
         Hide(DialogPanel);
         return True;
      end if;
      return False;
   end HideDialog;

   procedure ShowWaitOrder is
      NeedHealing, NeedRest: Boolean := False;
      WaitOrders: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("1) Wait 1 minute"),
         To_Unbounded_String("2) Wait 5 minutes"),
         To_Unbounded_String("3) Wait 10 minutes"),
         To_Unbounded_String("4) Wait 15 minutes"),
         To_Unbounded_String("5) Wait 30 minutes"),
         To_Unbounded_String("6) Wait 1 hour"),
         To_Unbounded_String("7) Wait X minutes"));
      Wait_Items: Item_Array_Access;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      WaitAmount: Positive := WaitOrders'Length + 2;
      MenuIndex: Positive := WaitOrders'Length + 1;
   begin
      for I in PlayerShip.Crew.First_Index .. PlayerShip.Crew.Last_Index loop
         if PlayerShip.Crew(I).Tired > 0 and
           PlayerShip.Crew(I).Order = Rest then
            NeedRest := True;
         end if;
         if PlayerShip.Crew(I).Health < 100 and
           PlayerShip.Crew(I).Health > 0 and
           PlayerShip.Crew(I).Order = Rest then
            for Module of PlayerShip.Modules loop
               if Modules_List(Module.ProtoIndex).MType = CABIN and
                 Module.Owner = I then
                  NeedHealing := True;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
      if NeedRest then
         WaitAmount := WaitAmount + 1;
      end if;
      if NeedHealing then
         WaitAmount := WaitAmount + 1;
      end if;
      Wait_Items := new Item_Array(1 .. WaitAmount);
      for I in WaitOrders'Range loop
         Wait_Items.all(I) := New_Item(To_String(WaitOrders(I)));
      end loop;
      if NeedRest then
         Wait_Items.all(MenuIndex) :=
           New_Item
             (Positive'Image(MenuIndex)(2) & ") Wait until crew is rested");
         MenuIndex := MenuIndex + 1;
      end if;
      if NeedHealing then
         Wait_Items.all(MenuIndex) :=
           New_Item
             (Positive'Image(MenuIndex)(2) & ") Wait until crew is healed");
         MenuIndex := MenuIndex + 1;
      end if;
      Wait_Items.all(MenuIndex) := New_Item("C) Close");
      MenuIndex := MenuIndex + 1;
      Wait_Items.all(MenuIndex) := Null_Item;
      OrdersMenu := New_Menu(Wait_Items);
      Scale(OrdersMenu, MenuHeight, MenuLength);
      MenuWindow :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow, 5, "Wait orders");
      Set_Window(OrdersMenu, MenuWindow);
      Set_Sub_Window
        (OrdersMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 1, 1));
      Post(OrdersMenu);
      Refresh_Without_Update;
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
   end ShowWaitOrder;

   procedure ShowGameMenu is
      Menu_Items: constant Item_Array_Access := new Item_Array(1 .. 18);
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      Menu_Items.all :=
        (New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(21))) &
            ") Ship informations"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(22))) & ") Ship cargo"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(23))) &
            ") Crew informations"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(24))) & ") Ship orders"),
         New_Item(GetKeyName(Key_Code(GameSettings.Keys(25))) & ") Crafting"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(26))) & ") Last messages"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(27))) &
            ") List of known bases"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(28))) &
            ") List of known events"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(29))) &
            ") Accepted missions"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(30))) & ") Wait orders"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(31))) &
            ") Move map position"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(32))) & ") Game statistics"),
         New_Item(GetKeyName(Key_Code(GameSettings.Keys(33))) & ") Help"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(34))) & ") Game options"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(35))) & ") Quit from game"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(36))) &
            ") Resign from game"),
         New_Item
           (GetKeyName(Key_Code(GameSettings.Keys(37))) & ") Close menu"),
         Null_Item);
      OrdersMenu := New_Menu(Menu_Items);
      Set_Format(OrdersMenu, Lines - 4, 1);
      Scale(OrdersMenu, MenuHeight, MenuLength);
      MenuWindow :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      WindowFrame(MenuWindow, 5, "Main menu");
      Set_Window(OrdersMenu, MenuWindow);
      Set_Sub_Window
        (OrdersMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 1, 1));
      Post(OrdersMenu);
      Refresh_Without_Update;
      Refresh_Without_Update(MenuWindow);
      Update_Screen;
   end ShowGameMenu;

   procedure DrawGame(CurrentState: GameStates) is
   begin
      Erase;
      ShowGameHeader(CurrentState);
      case CurrentState is
         when Sky_Map_View =>
            ShowSkyMap;
         when Control_Speed =>
            ShowSkyMap;
            ShowOrdersMenu;
         when Ship_Info =>
            ShowShipInfo;
         when Crew_Info =>
            ShowCrewInfo;
         when Messages_View =>
            ShowMessages;
         when Trade_View =>
            ShowTrade;
         when Help_View =>
            ShowHelpMenu(True);
         when Quit_Confirm =>
            ShowSkyMap;
            Refresh_Without_Update;
            ShowConfirm("Are you sure want to quit game?");
         when Combat_State =>
            ShowCombat;
         when Craft_View =>
            ShowRecipes;
         when Wait_Order =>
            ShowSkyMap;
            ShowWaitOrder;
         when Cargo_Info =>
            ShowCargoInfo;
         when Help_Topic =>
            ShowHelp(Sky_Map_View);
         when Repairs_View =>
            ShowRepair;
         when Clear_Confirm =>
            ShowMessages;
            Refresh_Without_Update;
            ShowConfirm("Are you sure want to clear all messages?");
         when Shipyard_View =>
            ShowShipyard;
         when Recruits_View =>
            ShowRecruits;
         when Dismiss_Confirm =>
            ShowCrewInfo;
            Refresh_Without_Update;
            ShowConfirm("Are you sure want to dismiss this crew member?");
         when Bases_List =>
            ShowBasesList;
         when Events_View =>
            ShowEvents;
         when GameStats_View =>
            ShowGameStats;
         when Death_Confirm =>
            ShowDeathScreen;
            Refresh_Without_Update;
         when TradeRecipes_View =>
            ShowTradeRecipes;
         when BaseMissions_View =>
            ShowBaseMissions;
         when Missions_View =>
            ShowMissions;
         when PilotRest_Confirm =>
            ShowSkyMap;
            Refresh_Without_Update;
            ShowConfirm
              ("You don't have pilot on duty. Did you want to wait until your pilot rest?");
         when EngineerRest_Confirm =>
            ShowSkyMap;
            Refresh_Without_Update;
            ShowConfirm
              ("You don't have engineer on duty. Did you want to wait until your engineer rest?");
         when GameOptions_View =>
            ShowOptions;
         when Heal_View =>
            ShowHeal;
         when Resign_Confirm =>
            ShowSkyMap;
            Refresh_Without_Update;
            ShowConfirm("Are you sure want to resign from game?");
         when Loot_View =>
            ShowLoot;
         when ChangeHome_Confirm =>
            ShowSkyMap;
            Refresh_Without_Update;
            declare
               TraderIndex: constant Natural := FindMember(Talk);
               Price: Positive := 1000;
            begin
               CountPrice(Price, TraderIndex);
               ShowConfirm
                 ("Are you sure want to change your home base (it cost" &
                  Positive'Image(Price) &
                  " " &
                  To_String(MoneyName) &
                  ")?");
            end;
         when Inventory_View =>
            ShowInventory;
         when others =>
            null;
      end case;
      if LastMessage /= To_Unbounded_String("") then
         Move_Cursor(Line => 1, Column => 2);
         Add(Str => To_String(LastMessage));
         LastMessage := To_Unbounded_String("");
      end if;
      Update_Panels;
      Update_Screen;
   end DrawGame;

   procedure ShowWaitForm is
      Wait_Fields: constant Field_Array_Access := new Field_Array(1 .. 5);
      FieldOptions: Field_Option_Set;
      FormHeight: Line_Position;
      FormLength: Column_Position;
      Visibility: Cursor_Visibility := Normal;
      Result: Forms.Driver_Result;
   begin
      Set_Cursor_Visibility(Visibility);
      Wait_Fields.all(1) := New_Field(1, 8, 0, 0, 0, 0);
      FieldOptions := Get_Options(Wait_Fields.all(1));
      Set_Buffer(Wait_Fields.all(1), 0, "Minutes:");
      FieldOptions.Active := False;
      Set_Options(Wait_Fields.all(1), FieldOptions);
      Wait_Fields.all(2) := New_Field(1, 5, 0, 8, 0, 0);
      FieldOptions := Get_Options(Wait_Fields.all(2));
      Set_Buffer(Wait_Fields.all(2), 0, "1");
      FieldOptions.Auto_Skip := False;
      FieldOptions.Null_Ok := False;
      Set_Options(Wait_Fields.all(2), FieldOptions);
      Set_Foreground(Wait_Fields.all(2), BoldCharacters, 11);
      Set_Background(Wait_Fields.all(2), BoldCharacters, 11);
      Set_Field_Type(Wait_Fields.all(2), (0, 1, 1024));
      Wait_Fields.all(3) := New_Field(1, 8, 2, 1, 0, 0);
      Set_Buffer(Wait_Fields.all(3), 0, "[Cancel]");
      FieldOptions := Get_Options(Wait_Fields.all(3));
      FieldOptions.Edit := False;
      Set_Options(Wait_Fields.all(3), FieldOptions);
      Wait_Fields.all(4) := New_Field(1, 4, 2, 11, 0, 0);
      FieldOptions := Get_Options(Wait_Fields.all(4));
      FieldOptions.Edit := False;
      Set_Options(Wait_Fields.all(4), FieldOptions);
      Set_Buffer(Wait_Fields.all(4), 0, "[Ok]");
      Wait_Fields.all(5) := Null_Field;
      WaitForm := New_Form(Wait_Fields);
      Set_Options(WaitForm, (others => False));
      Scale(WaitForm, FormHeight, FormLength);
      MenuWindow :=
        Create
          (FormHeight + 2,
           FormLength + 2,
           ((Lines / 3) - (FormHeight / 2)),
           ((Columns / 2) - (FormLength / 2)));
      WindowFrame(MenuWindow, 5, "Wait");
      Set_Window(WaitForm, MenuWindow);
      Set_Sub_Window
        (WaitForm,
         Derived_Window(MenuWindow, FormHeight, FormLength, 1, 1));
      Post(WaitForm);
      Result := Driver(WaitForm, F_End_Line);
      if Result = Form_Ok then
         Refresh_Without_Update;
         Refresh_Without_Update(MenuWindow);
         Update_Screen;
      end if;
   end ShowWaitForm;

end UserInterface;
