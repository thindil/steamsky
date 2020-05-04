-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Containers; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Gtk.Button_Box; use Gtk.Button_Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Combat; use Combat;
with Combat.UI; use Combat.UI;
with Crafts; use Crafts;
with Crew; use Crew;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Missions; use Missions;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Stories; use Stories;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Maps.UI.OrdersMenu is

   -- ****iv* Maps.UI.OrdersMenu/OrdersBox
   -- FUNCTION
   -- Orders menu
   -- SOURCE
   OrdersBox: Gtk_Button_Box;
   -- ****

   procedure ShowOrders(Object: access Gtkada_Builder_Record'Class) is
      HaveTrader: Boolean := False;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MissionsLimit: Integer;
      Event: Events_Types := None;
      ItemIndex: Natural;
   begin
      if Is_Visible(OrdersBox) then
         Hide(OrdersBox);
         return;
      end if;
      UpdateMapInfo(True);
      Foreach(OrdersBox, HideButtons'Access);
      if FindMember(Talk) > 0 then
         HaveTrader := True;
      end if;
      Set_No_Show_All(Get_Child(OrdersBox, 21), False);
      if CurrentStory.Index /= Null_Unbounded_String then
         declare
            Step: Step_Data;
         begin
            if CurrentStory.CurrentStep = 0 then
               Step := Stories_List(CurrentStory.Index).StartingStep;
            elsif CurrentStory.CurrentStep > 0 then
               Step :=
                 Stories_List(CurrentStory.Index).Steps
                   (CurrentStory.CurrentStep);
            else
               Step := Stories_List(CurrentStory.Index).FinalStep;
            end if;
            case Step.FinishCondition is
               when ASKINBASE =>
                  if BaseIndex > 0 then
                     if CurrentStory.Data = Null_Unbounded_String or
                       CurrentStory.Data = SkyBases(BaseIndex).Name then
                        Set_Label
                          (Gtk_Button(Get_Object(Builder, "btnstory")),
                           "Ask _for " &
                           To_String
                             (Items_List(GetStepData(Step.FinishData, "item"))
                                .Name));
                        Set_No_Show_All
                          (Gtk_Widget(Get_Object(Object, "btnstory")), False);
                     end if;
                  end if;
               when DESTROYSHIP =>
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create(Tokens, To_String(CurrentStory.Data), ";");
                     if PlayerShip.SkyX = Positive'Value(Slice(Tokens, 1)) and
                       PlayerShip.SkyY = Positive'Value(Slice(Tokens, 2)) then
                        Set_Label
                          (Gtk_Button(Get_Object(Builder, "btnstory")),
                           "_Search for " &
                           To_String
                             (ProtoShips_List
                                (To_Unbounded_String(Slice(Tokens, 3)))
                                .Name));
                        Set_No_Show_All
                          (Gtk_Widget(Get_Object(Object, "btnstory")), False);
                     end if;
                  end;
               when EXPLORE =>
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create(Tokens, To_String(CurrentStory.Data), ";");
                     if PlayerShip.SkyX = Positive'Value(Slice(Tokens, 1)) and
                       PlayerShip.SkyY = Positive'Value(Slice(Tokens, 2)) then
                        Set_Label
                          (Gtk_Button(Get_Object(Builder, "btnstory")),
                           "_Search area");
                        Set_No_Show_All
                          (Gtk_Widget(Get_Object(Object, "btnstory")), False);
                     end if;
                  end;
               when ANY | LOOT =>
                  null;
            end case;
         end;
      end if;
      if PlayerShip.Speed = DOCKED then
         Set_Label(Gtk_Button(Get_Object(Builder, "btndock")), "_Undock");
         Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btndock")), False);
         Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btnescape")), False);
         if HaveTrader and SkyBases(BaseIndex).Population > 0 then
            Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btntrade")), False);
            Set_No_Show_All
              (Gtk_Widget(Get_Object(Object, "btnschool")), False);
            if SkyBases(BaseIndex).Recruits.Length > 0 then
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnrecruit")), False);
            end if;
            if DaysDifference(SkyBases(BaseIndex).AskedForEvents) > 6 then
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnaskevents")), False);
            end if;
            if not SkyBases(BaseIndex).AskedForBases then
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnaskbases")), False);
            end if;
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 then
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnheal")), False);
                  exit;
               end if;
            end loop;
            for Module of PlayerShip.Modules loop
               if Module.Durability < Module.MaxDurability then
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnrepair")), False);
                  exit;
               end if;
            end loop;
            if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
                (To_Unbounded_String("shipyard")) then
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnshipyard")), False);
            end if;
            if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
                (To_Unbounded_String("temple")) then
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnpray")), False);
            end if;
            for I in Recipes_List.Iterate loop
               if Known_Recipes.Find_Index(Item => Recipes_Container.Key(I)) =
                 UnboundedString_Container.No_Index and
                 BasesTypes_List(SkyBases(BaseIndex).BaseType).Recipes.Contains
                   (Recipes_Container.Key(I)) and
                 Recipes_List(I).Reputation <=
                   SkyBases(BaseIndex).Reputation(1) then
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnrecipes")), False);
                  exit;
               end if;
            end loop;
            if SkyBases(BaseIndex).Missions.Length > 0 then
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
               for Mission of AcceptedMissions loop
                  if (Mission.Finished and Mission.StartBase = BaseIndex) or
                    (Mission.TargetX = PlayerShip.SkyX and
                     Mission.TargetY = PlayerShip.SkyY) then
                     case Mission.MType is
                        when Deliver =>
                           Set_Label
                             (Gtk_Button
                                (Get_Object(Object, "btnfinishmission")),
                              "_Complete delivery of " &
                              To_String(Items_List(Mission.ItemIndex).Name));
                        when Destroy =>
                           if Mission.Finished then
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btnfinishmission")),
                                 "_Complete destroy " &
                                 To_String
                                   (ProtoShips_List(Mission.ShipIndex).Name));
                           end if;
                        when Patrol =>
                           if Mission.Finished then
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btnfinishmission")),
                                 "_Complete Patrol area mission");
                           end if;
                        when Explore =>
                           if Mission.Finished then
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btnfinishmission")),
                                 "_Complete Explore area mission");
                           end if;
                        when Passenger =>
                           if Mission.Finished then
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btnfinishmission")),
                                 "_Complete Transport passenger mission");
                           end if;
                     end case;
                     Set_No_Show_All
                       (Gtk_Widget(Get_Object(Object, "btnfinishmission")),
                        False);
                  end if;
                  if Mission.StartBase = BaseIndex then
                     MissionsLimit := MissionsLimit - 1;
                  end if;
               end loop;
               if MissionsLimit > 0 then
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnmissions")), False);
               end if;
            end if;
            if PlayerShip.HomeBase /= BaseIndex then
               Set_No_Show_All(Get_Child(OrdersBox, 20), False);
            end if;
         end if;
         if SkyBases(BaseIndex).Population = 0 then
            Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btnloot")), False);
         end if;
      else
         Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btnescape")), True);
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
            Event :=
              Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                .EType;
            Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btnstory")), True);
         end if;
         case Event is
            when EnemyShip | EnemyPatrol =>
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnattack")), False);
               Set_Label
                 (Gtk_Button(Get_Object(Object, "btnattack")), "_Attack");
            when FullDocks =>
               Set_Label
                 (Gtk_Button(Get_Object(Builder, "btndock")),
                  "_Wait (full docks)");
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btndock")), False);
            when AttackOnBase =>
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnattack")), False);
               Set_Label
                 (Gtk_Button(Get_Object(Object, "btnattack")), "_Defend");
            when Disease =>
               if HaveTrader then
                  ItemIndex :=
                    FindItem
                      (Inventory => PlayerShip.Cargo,
                       ItemType =>
                         Factions_List(SkyBases(BaseIndex).Owner)
                           .HealingTools);
                  if ItemIndex > 0 then
                     Set_No_Show_All
                       (Gtk_Widget(Get_Object(Object, "btnfreemedicines")),
                        False);
                     Set_No_Show_All
                       (Gtk_Widget(Get_Object(Object, "btnpricedmedicines")),
                        False);
                  end if;
               end if;
            when None | DoublePrice | BaseRecovery =>
               if BaseIndex > 0 then
                  if SkyBases(BaseIndex).Reputation(1) > -25 then
                     declare
                        DockingCost: Positive;
                     begin
                        for Module of PlayerShip.Modules loop
                           if Module.MType = HULL then
                              DockingCost := Module.MaxModules;
                              exit;
                           end if;
                        end loop;
                        Set_Label
                          (Gtk_Button(Get_Object(Builder, "btndock")),
                           "_Dock (" &
                           Trim(Positive'Image(DockingCost), Both) & " " &
                           To_String(MoneyName) & ")");
                        Set_No_Show_All
                          (Gtk_Widget(Get_Object(Object, "btndock")), False);
                     end;
                  end if;
                  for Mission of AcceptedMissions loop
                     if HaveTrader and Mission.TargetX = PlayerShip.SkyX and
                       Mission.TargetY = PlayerShip.SkyY and
                       Mission.Finished then
                        case Mission.MType is
                           when Deliver =>
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btnfinishmission")),
                                 "_Complete delivery of " &
                                 To_String
                                   (Items_List(Mission.ItemIndex).Name));
                           when Destroy =>
                              if Mission.Finished then
                                 Set_Label
                                   (Gtk_Button
                                      (Get_Object(Object, "btnfinishmission")),
                                    "_Complete destroy " &
                                    To_String
                                      (ProtoShips_List(Mission.ShipIndex)
                                         .Name));
                              end if;
                           when Patrol =>
                              if Mission.Finished then
                                 Set_Label
                                   (Gtk_Button
                                      (Get_Object(Object, "btnfinishmission")),
                                    "_Complete Patrol area mission");
                              end if;
                           when Explore =>
                              if Mission.Finished then
                                 Set_Label
                                   (Gtk_Button
                                      (Get_Object(Object, "btnfinishmission")),
                                    "_Complete Explore area mission");
                              end if;
                           when Passenger =>
                              if Mission.Finished then
                                 Set_Label
                                   (Gtk_Button
                                      (Get_Object(Object, "btnfinishmission")),
                                    "_Complete Transport passenger mission");
                              end if;
                        end case;
                        Set_No_Show_All
                          (Gtk_Widget(Get_Object(Object, "btnfinishmission")),
                           False);
                     end if;
                  end loop;
               else
                  for Mission of AcceptedMissions loop
                     if Mission.TargetX = PlayerShip.SkyX and
                       Mission.TargetY = PlayerShip.SkyY and
                       not Mission.Finished then
                        case Mission.MType is
                           when Deliver | Passenger =>
                              null;
                           when Destroy =>
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btncurrentmission")),
                                 "_Search for " &
                                 To_String
                                   (ProtoShips_List(Mission.ShipIndex).Name));
                           when Patrol =>
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btncurrentmission")),
                                 "_Patrol area");
                           when Explore =>
                              Set_Label
                                (Gtk_Button
                                   (Get_Object(Object, "btncurrentmission")),
                                 "_Explore area");
                        end case;
                        Set_No_Show_All
                          (Gtk_Widget(Get_Object(Object, "btncurrentmission")),
                           False);
                     end if;
                  end loop;
               end if;
            when Trader =>
               if HaveTrader then
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btntrade")), False);
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnaskevents")), False);
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnaskbases")), False);
               end if;
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnattack")), False);
               Set_Label
                 (Gtk_Button(Get_Object(Object, "btnattack")), "Attack");
            when FriendlyShip =>
               if HaveTrader then
                  if Index
                      (ProtoShips_List
                         (Events_List
                            (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY)
                               .EventIndex)
                            .ShipIndex)
                         .Name,
                       To_String(TradersName)) >
                    0 then
                     Set_No_Show_All
                       (Gtk_Widget(Get_Object(Object, "btntrade")), False);
                     Set_No_Show_All
                       (Gtk_Widget(Get_Object(Object, "btnaskbases")), False);
                  end if;
                  Set_No_Show_All
                    (Gtk_Widget(Get_Object(Object, "btnaskevents")), False);
               end if;
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnattack")), False);
               Set_Label
                 (Gtk_Button(Get_Object(Object, "btnattack")), "Attack");
         end case;
      end if;
      ButtonsVisible := False;
      Foreach(OrdersBox, CheckButtons'Access);
      if ButtonsVisible then
         Hide(Gtk_Widget(Get_Object(Builder, "moremovemapbox")));
         Hide(Gtk_Widget(Get_Object(Builder, "btnboxwait")));
         Hide(Gtk_Widget(Get_Object(Builder, "btnboxdestination")));
         Show_All(OrdersBox);
         Grab_Focus(Get_Child(OrdersBox, 21));
      else
         ShowDialog
           ("Here are no available ship orders at this moment. Ship orders available mostly when you are at base or at event on map.");
      end if;
   end ShowOrders;

   procedure ExecuteStory(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      Step: Step_Data;
      Message: Unbounded_String;
   begin
      Hide(OrdersBox);
      if CurrentStory.CurrentStep = 0 then
         Step := Stories_List(CurrentStory.Index).StartingStep;
      elsif CurrentStory.CurrentStep > 0 then
         Step :=
           Stories_List(CurrentStory.Index).Steps(CurrentStory.CurrentStep);
      else
         Step := Stories_List(CurrentStory.Index).FinalStep;
      end if;
      if PlayerShip.Speed /= DOCKED and Step.FinishCondition = ASKINBASE then
         Message := To_Unbounded_String(DockShip(True));
         if Message /= Null_Unbounded_String then
            ShowDialog(To_String(Message));
            return;
         end if;
      end if;
      if ProgressStory then
         declare
            Tokens: Slice_Set;
         begin
            Create(Tokens, To_String(CurrentStory.Data), ";");
            case Step.FinishCondition is
               when DESTROYSHIP =>
                  if StartCombat
                      (To_Unbounded_String(Slice(Tokens, 3)), False) then
                     ShowCombatUI;
                     return;
                  end if;
               when others =>
                  null;
            end case;
            if CurrentStory.CurrentStep > -2 then
               if CurrentStory.CurrentStep > 0 then
                  Step :=
                    Stories_List(CurrentStory.Index).Steps
                      (CurrentStory.CurrentStep);
               else
                  Step := Stories_List(CurrentStory.Index).FinalStep;
               end if;
               for Text of Step.Texts loop
                  if CurrentStory.FinishedStep = Text.Condition then
                     ShowDialog(To_String(Text.Text));
                     CurrentStory.ShowText := False;
                     exit;
                  end if;
               end loop;
            else
               FinishStory;
            end if;
         end;
      else
         ShowDialog(To_String(Step.FailText));
         CurrentStory.ShowText := False;
      end if;
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
      DrawMap;
   end ExecuteStory;

   procedure HideOrders(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
   begin
      Hide(OrdersBox);
   end HideOrders;

   -- ****if* Maps.UI.OrdersMenu/SetAsHome
   -- FUNCTION
   -- Set the selected base as a home base for the player
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused
   -- SOURCE
   procedure SetAsHome(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      TraderIndex: constant Natural := FindMember(Talk);
      Price: Positive := 1000;
      MoneyIndex2: constant Natural := FindItem(PlayerShip.Cargo, MoneyIndex);
   begin
      CountPrice(Price, TraderIndex);
      if ShowConfirmDialog
          ("Are you sure want to change your home base (it cost" &
           Positive'Image(Price) & " " & To_String(MoneyName) & ")?",
           Gtk_Window(Get_Object(Builder, "skymapwindow"))) then
         if MoneyIndex2 = 0 then
            ShowDialog
              ("You don't have any " & To_String(MoneyName) &
               " for change ship home base.");
            return;
         end if;
         CountPrice(Price, TraderIndex);
         if PlayerShip.Cargo(MoneyIndex2).Amount < Price then
            ShowDialog
              ("You don't have enough " & To_String(MoneyName) &
               " for change ship home base.");
            return;
         end if;
         PlayerShip.HomeBase :=
           SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
         UpdateCargo
           (Ship => PlayerShip, CargoIndex => MoneyIndex2,
            Amount => (0 - Price));
         AddMessage
           ("You changed your ship home base to: " &
            To_String(SkyBases(PlayerShip.HomeBase).Name),
            OtherMessage);
         GainExp(1, TalkingSkill, TraderIndex);
         UpdateGame(10);
      end if;
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
      DrawMap;
   end SetAsHome;

   procedure CreateOrdersMenu is
      Button: Gtk_Button;
   begin
      OrdersBox := Gtk_Button_Box(Get_Object(Builder, "btnboxorders"));
      --OrdersBox := Gtk_Button_Box_New(Orientation_Vertical);
      --Button := Gtk_Button_New_With_Label("Story");
      --On_Clicked(Button, ExecuteStory'Access);
      --Pack_Start(OrdersBox, Button, False);
      Button := Gtk_Button_New_With_Mnemonic("Set as _home");
      On_Clicked(Button, SetAsHome'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("_Close");
      On_Clicked(Button, HideOrders'Access);
      Pack_Start(OrdersBox, Button);
   end CreateOrdersMenu;

end Maps.UI.OrdersMenu;
