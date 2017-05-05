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
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.UI; use Ships.UI;
with Ships.UI.Cargo; use Ships.UI.Cargo;
with Ships.UI.Ship; use Ships.UI.Ship;
with Crew; use Crew;
with Crew.UI; use Crew.UI;
with Bases; use Bases;
with Bases.UI.Trade; use Bases.UI.Trade;
with Bases.UI.Repair; use Bases.UI.Repair;
with Bases.UI.Shipyard; use Bases.UI.Shipyard;
with Bases.UI.Recruits; use Bases.UI.Recruits;
with Bases.UI.Recipes; use Bases.UI.Recipes;
with Bases.UI.Missions; use Bases.UI.Missions;
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

package body UserInterface is

   DialogPanel: Panel := Null_Panel;

   procedure ShowGameHeader(CurrentState: GameStates) is
      Speed: Unbounded_String;
      HavePilot,
      HaveEngineer,
      HaveRepair,
      HaveUpgrade,
      HaveTrader,
      HaveCleaner,
      NeedClean: Boolean :=
        False;
      GunnersCheck, CraftersCheck: Natural := 0;
   begin
      case CurrentState is
         when Sky_Map_View | Control_Speed | Wait_Order =>
            Add(Str => "[Menu]");
            Change_Attributes(Line => 0, Column => 2, Count => 1, Color => 1);
         when Ship_Info =>
            Add(Str => "Ship Informations [Quit] [F1 Help]");
            Change_Attributes(Line => 0, Column => 19, Count => 1, Color => 1);
            Change_Attributes(Line => 0, Column => 26, Count => 2, Color => 1);
         when Crew_Info | Giving_Orders =>
            Add(Str => "Crew Informations [Quit] [F1 Help]");
            Change_Attributes(Line => 0, Column => 19, Count => 1, Color => 1);
            Change_Attributes(Line => 0, Column => 26, Count => 2, Color => 1);
         when Dismiss_Confirm =>
            Add(Str => "Crew Informations");
         when Messages_View =>
            Add(Str => "Last Messages [Quit]");
            Change_Attributes(Line => 0, Column => 15, Count => 1, Color => 1);
            Change_Attributes(Line => 0, Column => 22, Count => 2, Color => 1);
         when Clear_Confirm =>
            Add(Str => "Last Messages");
         when Trade_View =>
            Add(Str => "Trade with base [Quit] [F1 Help]");
            Change_Attributes(Line => 0, Column => 17, Count => 1, Color => 1);
            Change_Attributes(Line => 0, Column => 24, Count => 2, Color => 1);
         when Help_View =>
            Add(Str => "Help Index [Quit]");
            Change_Attributes(Line => 0, Column => 12, Count => 1, Color => 1);
         when Craft_View =>
            Add(Str => "Manufacturing [Quit] [F1 Help]");
            Change_Attributes(Line => 0, Column => 15, Count => 1, Color => 1);
            Change_Attributes(Line => 0, Column => 22, Count => 2, Color => 1);
         when Cargo_Info =>
            Add(Str => "Ship Cargo [Quit]");
            Change_Attributes(Line => 0, Column => 12, Count => 1, Color => 1);
         when Help_Topic =>
            Add(Str => "Help [Menu] [Quit]");
            Change_Attributes(Line => 0, Column => 6, Count => 1, Color => 1);
            Change_Attributes(Line => 0, Column => 13, Count => 1, Color => 1);
         when Repairs_View =>
            Add(Str => "Ship repairs [Quit]");
            Change_Attributes(Line => 0, Column => 14, Count => 1, Color => 1);
         when Shipyard_View =>
            Add(Str => "Shipyard [Quit]");
            Change_Attributes(Line => 0, Column => 10, Count => 1, Color => 1);
         when Recruits_View =>
            Add(Str => "Recruit new crew members [Quit]");
            Change_Attributes(Line => 0, Column => 26, Count => 1, Color => 1);
         when Bases_List =>
            Add(Str => "List of know bases [Quit] [F1 Help]");
            Change_Attributes(Line => 0, Column => 20, Count => 1, Color => 1);
            Change_Attributes(Line => 0, Column => 27, Count => 2, Color => 1);
         when Events_View =>
            Add(Str => "List of know events [Quit]");
            Change_Attributes(Line => 0, Column => 21, Count => 1, Color => 1);
         when GameStats_View =>
            Add(Str => "Game statistics [Quit]");
            Change_Attributes(Line => 0, Column => 17, Count => 1, Color => 1);
         when TradeRecipes_View =>
            Add(Str => "Buy crafting recipes [Quit]");
            Change_Attributes(Line => 0, Column => 22, Count => 1, Color => 1);
         when BaseMissions_View =>
            Add(Str => "Available missions [Quit]");
            Change_Attributes(Line => 0, Column => 20, Count => 1, Color => 1);
         when Missions_View =>
            Add(Str => "Accepted missions [Quit]");
            Change_Attributes(Line => 0, Column => 19, Count => 1, Color => 1);
         when others =>
            null;
      end case;
      if CurrentState /= Help_View and CurrentState /= Help_Topic then
         case PlayerShip.Speed is
            when DOCKED =>
               Speed := To_Unbounded_String("Docked");
            when FULL_STOP =>
               Speed := To_Unbounded_String("Stopped");
            when QUARTER_SPEED =>
               Speed := To_Unbounded_String("Quarter Speed");
            when HALF_SPEED =>
               Speed := To_Unbounded_String("Half Speed");
            when FULL_SPEED =>
               Speed := To_Unbounded_String("Full Speed");
         end case;
         Move_Cursor(Line => 0, Column => (Columns / 3));
         Add(Str => FormatedTime & " Speed: " & To_String(Speed));
         Move_Cursor(Line => 0, Column => (Columns - 25));
         Add(Str => "[P][E][G][R][M][U][T][C]");
         for Module of PlayerShip.Modules loop
            case Modules_List(Module.ProtoIndex).MType is
               when GUN =>
                  if Module.Owner > 0 and GunnersCheck = 0 then
                     GunnersCheck := 1;
                  elsif Module.Owner = 0 and GunnersCheck = 1 then
                     GunnersCheck := 2;
                  end if;
               when ALCHEMY_LAB .. GREENHOUSE =>
                  if Module.Current_Value /= 0 then
                     if Module.Owner > 0 and CraftersCheck < 2 then
                        CraftersCheck := 1;
                     else
                        CraftersCheck := 2;
                     end if;
                  end if;
               when CABIN =>
                  if Module.Current_Value < Module.Max_Value then
                     NeedClean := True;
                  end if;
               when others =>
                  null;
            end case;
         end loop;
         for Member of PlayerShip.Crew loop
            case Member.Order is
               when Pilot =>
                  HavePilot := True;
                  Change_Attributes
                    (Line => 0,
                     Column => (Columns - 24),
                     Count => 1,
                     Color => 2);
               when Engineer =>
                  HaveEngineer := True;
                  Change_Attributes
                    (Line => 0,
                     Column => (Columns - 21),
                     Count => 1,
                     Color => 2);
               when Repair =>
                  HaveRepair := True;
                  Change_Attributes
                    (Line => 0,
                     Column => (Columns - 15),
                     Count => 1,
                     Color => 2);
               when Upgrading =>
                  HaveUpgrade := True;
                  Change_Attributes
                    (Line => 0,
                     Column => (Columns - 9),
                     Count => 1,
                     Color => 2);
               when Talk =>
                  HaveTrader := True;
                  Change_Attributes
                    (Line => 0,
                     Column => (Columns - 6),
                     Count => 1,
                     Color => 2);
               when Clean =>
                  HaveCleaner := True;
                  Change_Attributes
                    (Line => 0,
                     Column => (Columns - 3),
                     Count => 1,
                     Color => 2);
               when others =>
                  null;
            end case;
         end loop;
         if not HavePilot then
            Change_Attributes
              (Line => 0,
               Column => (Columns - 24),
               Count => 1,
               Color => 3);
         end if;
         if not HaveEngineer then
            Change_Attributes
              (Line => 0,
               Column => (Columns - 21),
               Count => 1,
               Color => 3);
         end if;
         if GunnersCheck = 1 then
            Change_Attributes
              (Line => 0,
               Column => (Columns - 18),
               Count => 1,
               Color => 2);
         elsif GunnersCheck = 2 then
            Change_Attributes
              (Line => 0,
               Column => (Columns - 18),
               Count => 1,
               Color => 3);
         end if;
         if not HaveRepair then
            for Module of PlayerShip.Modules loop
               if Module.Durability < Module.MaxDurability then
                  Change_Attributes
                    (Line => 0,
                     Column => (Columns - 15),
                     Count => 1,
                     Color => 3);
                  exit;
               end if;
            end loop;
         end if;
         if CraftersCheck = 1 then
            Change_Attributes
              (Line => 0,
               Column => (Columns - 12),
               Count => 1,
               Color => 2);
         elsif CraftersCheck = 2 then
            Change_Attributes
              (Line => 0,
               Column => (Columns - 12),
               Count => 1,
               Color => 3);
         end if;
         if not HaveUpgrade and PlayerShip.UpgradeModule > 0 then
            Change_Attributes
              (Line => 0,
               Column => (Columns - 9),
               Count => 1,
               Color => 3);
         end if;
         if not HaveTrader and
           SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
            Change_Attributes
              (Line => 0,
               Column => (Columns - 6),
               Count => 1,
               Color => 3);
         end if;
         if not HaveCleaner and NeedClean then
            Change_Attributes
              (Line => 0,
               Column => (Columns - 3),
               Count => 1,
               Color => 3);
         end if;
      end if;
   end ShowGameHeader;

   procedure ShowOrdersMenu is
      Orders_Items: Item_Array_Access;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      Event: Events_Types := None;
      TimeDiff, BaseIndex: Natural;
      MenuIndex, OrdersAmount: Positive;
      MissionsLimit: Integer;
      HaveTrader: Boolean := False;
   begin
      BaseIndex := SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      if FindMember(Talk) > 0 then
         HaveTrader := True;
      end if;
      if PlayerShip.Speed = DOCKED then
         OrdersAmount := 3;
         MenuIndex := 2;
         if HaveTrader and SkyBases(BaseIndex).Owner /= Abandoned then
            OrdersAmount := OrdersAmount + 1;
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
            TimeDiff :=
              (GameDate.Day + ((30 * GameDate.Month) * GameDate.Year)) -
              (SkyBases(BaseIndex).AskedForEvents.Day +
               ((30 * SkyBases(BaseIndex).AskedForEvents.Month) *
                SkyBases(BaseIndex).AskedForEvents.Year));
            if TimeDiff > 6 then
               OrdersAmount := OrdersAmount + 1;
            end if;
            if not SkyBases(BaseIndex).AskedForBases then
               OrdersAmount := OrdersAmount + 1;
            end if;
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
         end if;
         Orders_Items := new Item_Array(1 .. OrdersAmount);
         Orders_Items.all(1) := New_Item("Undock");
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
                     when Kill =>
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
         else
            OrdersAmount := OrdersAmount + 1;
         end if;
         Orders_Items := new Item_Array(1 .. OrdersAmount);
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
            Event :=
              Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                .EType;
         end if;
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
                  if FindCargo
                      (ItemType => To_Unbounded_String("Medical supplies")) >
                    0 then
                     Orders_Items.all(MenuIndex) :=
                       New_Item("Deliver medical supplies for free");
                     MenuIndex := MenuIndex + 1;
                     Orders_Items.all(MenuIndex) :=
                       New_Item("Deliver medical suppplies for price");
                     MenuIndex := MenuIndex + 1;
                  end if;
               end if;
            when None | DoublePrice =>
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
                           when Kill =>
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
                        end case;
                     end if;
                  end loop;
               else
                  for Mission of PlayerShip.Missions loop
                     if Mission.TargetX = PlayerShip.SkyX and
                       Mission.TargetY = PlayerShip.SkyY and
                       not Mission.Finished then
                        case Mission.MType is
                           when Deliver =>
                              null;
                           when Kill =>
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
      Orders_Items.all(MenuIndex) := New_Item("Quit");
      MenuIndex := MenuIndex + 1;
      Orders_Items.all(MenuIndex) := Null_Item;
      OrdersMenu := New_Menu(Orders_Items);
      Set_Format(OrdersMenu, Lines - 4, 1);
      Set_Mark(OrdersMenu, "");
      Scale(OrdersMenu, MenuHeight, MenuLength);
      MenuWindow :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      Box(MenuWindow);
      Set_Window(OrdersMenu, MenuWindow);
      Set_Sub_Window
        (OrdersMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 1, 1));
      Post(OrdersMenu);
      Refresh;
      Refresh(MenuWindow);
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
      Box(ConfirmWindow);
      Add
        (Win => ConfirmWindow,
         Str => Message & " (Y/N)",
         Line => 1,
         Column => 1);
      Refresh(ConfirmWindow);
   end ShowConfirm;

   procedure ShowDialog(Message: String) is
      DialogWindow: Window;
      Width: Positive;
      Height: Positive := 1;
   begin
      Width := Message'Length + 2;
      if Width >= Positive(Columns - 4) then
         Height := (Width / Positive(Columns - 4) + 2);
         Width := (Width / Height) + 2;
      end if;
      Height := Height + 2;
      DialogWindow :=
        New_Window
          (Line_Position(Height),
           Column_Position(Width),
           ((Lines / 2) - Line_Position(Height / 2)),
           ((Columns / 2) - Column_Position(Width / 2)));
      Box(DialogWindow);
      Add(Win => DialogWindow, Str => Message, Line => 1, Column => 1);
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
      Wait_Items.all(MenuIndex) := New_Item("Q) Quit");
      MenuIndex := MenuIndex + 1;
      Wait_Items.all(MenuIndex) := Null_Item;
      OrdersMenu := New_Menu(Wait_Items);
      Set_Mark(OrdersMenu, "");
      Scale(OrdersMenu, MenuHeight, MenuLength);
      MenuWindow :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      Box(MenuWindow);
      Set_Window(OrdersMenu, MenuWindow);
      Set_Sub_Window
        (OrdersMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 1, 1));
      Post(OrdersMenu);
      Refresh;
      Refresh(MenuWindow);
   end ShowWaitOrder;

   procedure ShowGameMenu is
      Menu_Items: constant Item_Array_Access := new Item_Array(1 .. 16);
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      Menu_Items.all :=
        (New_Item("s) Ship informations"),
         New_Item("a) Ship cargo"),
         New_Item("c) Crew informations"),
         New_Item("o) Ship orders"),
         New_Item("r) Crafting"),
         New_Item("m) Last messages"),
         New_Item("b) List of known bases"),
         New_Item("n) List of known events"),
         New_Item("i) Accepted missions"),
         New_Item("w) Wait orders"),
         New_Item("v) Move map position"),
         New_Item("g) Game statistics"),
         New_Item("h) Help"),
         New_Item("q) Quit from game"),
         New_Item("l) Close menu"),
         Null_Item);
      OrdersMenu := New_Menu(Menu_Items);
      Set_Format(OrdersMenu, Lines - 4, 1);
      Set_Mark(OrdersMenu, "");
      Scale(OrdersMenu, MenuHeight, MenuLength);
      MenuWindow :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      Box(MenuWindow);
      Set_Window(OrdersMenu, MenuWindow);
      Set_Sub_Window
        (OrdersMenu,
         Derived_Window(MenuWindow, MenuHeight, MenuLength, 1, 1));
      Post(OrdersMenu);
      Refresh;
      Refresh(MenuWindow);
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
            Refresh_Without_Update;
            ShowConfirm
              ("You are dead. Did you want to see your game statistics?");
         when TradeRecipes_View =>
            ShowTradeRecipes;
         when BaseMissions_View =>
            ShowBaseMissions;
         when Missions_View =>
            ShowMissions;
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
      Set_Background
        (Wait_Fields.all(2),
         (Reverse_Video => True, others => False));
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
      Box(MenuWindow);
      Set_Window(WaitForm, MenuWindow);
      Set_Sub_Window
        (WaitForm,
         Derived_Window(MenuWindow, FormHeight, FormLength, 1, 1));
      Post(WaitForm);
      Result := Driver(WaitForm, F_End_Line);
      if Result = Form_Ok then
         Refresh;
         Refresh(MenuWindow);
      end if;
   end ShowWaitForm;

end UserInterface;
