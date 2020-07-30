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
with Gtk.Stack; use Gtk.Stack;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Bases; use Bases;
with Bases.LootUI; use Bases.LootUI;
with Bases.RecruitUI; use Bases.RecruitUI;
with Bases.ShipyardUI; use Bases.ShipyardUI;
with Bases.UI; use Bases.UI;
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
with Missions.UI; use Missions.UI;
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
         if SkyBases(BaseIndex).Population > 0 then
            Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btnescape")), False);
         end if;
         if HaveTrader and SkyBases(BaseIndex).Population > 0 then
            Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btntrade")), False);
            Set_No_Show_All
              (Gtk_Widget(Get_Object(Object, "btnschool")), False);
            if SkyBases(BaseIndex).Recruits.Length > 0 then
               Set_No_Show_All(Get_Child(OrdersBox, 9), False);
            end if;
            if DaysDifference(SkyBases(BaseIndex).AskedForEvents) > 6 then
               Set_No_Show_All(Get_Child(OrdersBox, 10), False);
            end if;
            if not SkyBases(BaseIndex).AskedForBases then
               Set_No_Show_All(Get_Child(OrdersBox, 11), False);
            end if;
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 then
                  Set_No_Show_All(Get_Child(OrdersBox, 13), False);
                  exit;
               end if;
            end loop;
            for Module of PlayerShip.Modules loop
               if Module.Durability < Module.MaxDurability then
                  Set_No_Show_All(Get_Child(OrdersBox, 14), False);
                  exit;
               end if;
            end loop;
            if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
                (To_Unbounded_String("shipyard")) then
               Set_No_Show_All(Get_Child(OrdersBox, 15), False);
            end if;
            if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
                (To_Unbounded_String("temple")) then
               Set_No_Show_All(Get_Child(OrdersBox, 12), False);
            end if;
            for I in Recipes_List.Iterate loop
               if Known_Recipes.Find_Index(Item => Recipes_Container.Key(I)) =
                 UnboundedString_Container.No_Index and
                 BasesTypes_List(SkyBases(BaseIndex).BaseType).Recipes.Contains
                   (Recipes_Container.Key(I)) and
                 Recipes_List(I).Reputation <=
                   SkyBases(BaseIndex).Reputation(1) then
                  Set_No_Show_All(Get_Child(OrdersBox, 16), False);
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
                  Set_No_Show_All(Get_Child(OrdersBox, 17), False);
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
                                (Gtk_Button(Get_Child(OrdersBox, 18)),
                                 "_Search for " &
                                 To_String
                                   (ProtoShips_List(Mission.ShipIndex).Name));
                           when Patrol =>
                              Set_Label
                                (Gtk_Button(Get_Child(OrdersBox, 18)),
                                 "_Patrol area");
                           when Explore =>
                              Set_Label
                                (Gtk_Button(Get_Child(OrdersBox, 18)),
                                 "_Explore area");
                        end case;
                        Set_No_Show_All(Get_Child(OrdersBox, 18), False);
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

   -- ****if* Maps.UI.OrdersMenu/BtnLootClicked
   -- FUNCTION
   -- Show or hide looting UI
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked
   -- SOURCE
   procedure BtnLootClicked(Self: access Gtk_Button_Record'Class) is
   -- ****
   begin
      if HideInfo("loot") then
         return;
      end if;
      HideOrders(Self);
      ShowLootUI;
   end BtnLootClicked;

   -- ****if* Maps.UI.OrdersMenu/StartMission
   -- FUNCTION
   -- Start the current mission
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure StartMission(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
      StartsCombat: Boolean := False;
   begin
      Hide(OrdersBox);
      for Mission of AcceptedMissions loop
         if Mission.TargetX = PlayerShip.SkyX and
           Mission.TargetY = PlayerShip.SkyY and not Mission.Finished then
            case Mission.MType is
               when Deliver | Passenger =>
                  null;
               when Destroy =>
                  UpdateGame(GetRandom(15, 45));
                  StartsCombat := CheckForEvent;
                  if not StartsCombat then
                     StartsCombat :=
                       StartCombat
                         (AcceptedMissions
                            (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY)
                               .MissionIndex)
                            .ShipIndex,
                          False);
                  end if;
               when Patrol =>
                  UpdateGame(GetRandom(45, 75));
                  StartsCombat := CheckForEvent;
                  if not StartsCombat then
                     UpdateMission
                       (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).MissionIndex);
                  end if;
               when Explore =>
                  UpdateGame(GetRandom(30, 60));
                  StartsCombat := CheckForEvent;
                  if not StartsCombat then
                     UpdateMission
                       (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).MissionIndex);
                  end if;
            end case;
            exit;
         end if;
      end loop;
      if StartsCombat then
         ShowCombatUI;
         return;
      end if;
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
      DrawMap;
   end StartMission;

   -- ****if* Maps.UI.OrdersMenu/HideUI
   -- FUNCTION
   -- Hide UI before show a new screen
   -- SOURCE
   procedure HideUI is
      -- ****
      VisibleChildName: constant String :=
        Get_Visible_Child_Name(Gtk_Stack(Get_Object(Builder, "gamestack")));
   begin
      Hide(OrdersBox);
      Show_All(Gtk_Widget(Get_Object(Builder, "btnclose")));
      Hide(Gtk_Widget(Get_Object(Builder, "shipmovementbox")));
      Hide(Gtk_Widget(Get_Object(Builder, "btnboxorders")));
      if VisibleChildName = "combat" then
         PreviousGameState := Combat_View;
      elsif VisibleChildName = "skymap" then
         PreviousGameState := SkyMap_View;
         Hide(Gtk_Widget(Get_Object(Builder, "menuwait")));
         Hide(Gtk_Widget(Get_Object(Builder, "btnboxdestination")));
      end if;
   end HideUI;

   -- ****if* Maps.UI.OrdersMenu/ShowMissions
   -- FUNCTION
   -- Show available missions in the base
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure ShowMissions(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      if HideInfo("availablemissions") then
         return;
      end if;
      HideUI;
      ShowMissionsUI;
   end ShowMissions;

   -- ****if* Maps.UI.OrdersMenu/ShowRecipes
   -- FUNCTION
   -- Show screen to buy recipes from the base
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure ShowRecipes(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      if HideInfo("base") then
         return;
      end if;
      HideUI;
      ShowBuyRecipesUI;
   end ShowRecipes;

   -- ****if* Maps.UI.OrdersMenu/ShowShipyard
   -- FUNCTION
   -- Show shipyard screen
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure ShowShipyard(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      if HideInfo("shipyard") then
         return;
      end if;
      HideUI;
      ShowShipyardUI;
   end ShowShipyard;

   -- ****if* Maps.UI.OrdersMenu/ShowRepair
   -- FUNCTION
   -- Show repair ship screen
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure ShowRepair(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      HideUI;
      ShowRepairUI;
   end ShowRepair;

   -- ****if* Maps.UI.OrdersMenu/ShowHospital
   -- FUNCTION
   -- Show heal wounded crew member screen
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure ShowHospital(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      HideUI;
      ShowHealUI;
   end ShowHospital;

   -- ****if* Maps.UI.OrdersMenu/ShowChurch
   -- FUNCTION
   -- Pray in the selected bases
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure ShowChurch(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      HideOrders(null);
      for I in PlayerShip.Crew.Iterate loop
         UpdateMorale(PlayerShip, Crew_Container.To_Index(I), 10);
      end loop;
      AddMessage
        ("You and your crew were praying for some time. Now you all feel a bit better.",
         OrderMessage);
      UpdateGame(30);
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
      DrawMap;
   end ShowChurch;

   -- ****if* Maps.UI.OrdersMenu/AskForBases
   -- FUNCTION
   -- Ask for known bases in the selected base
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure AskForBases(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      HideOrders(null);
      AskForBases;
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
      DrawMap;
   end AskForBases;

   -- ****if* Maps.UI.OrdersMenu/AskForEvents
   -- FUNCTION
   -- Ask for known events in the selected base
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure AskForEvents(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      HideOrders(null);
      AskForEvents;
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
      DrawMap;
   end AskForEvents;

   -- ****if* Maps.UI.OrdersMenu/ShowRecruit
   -- FUNCTION
   -- Show recruiting screen
   -- PARAMETERS
   -- Self - Gtk_Button which was clicked. Unused.
   -- SOURCE
   procedure ShowRecruit(Self: access Gtk_Button_Record'Class) is
      pragma Unreferenced(Self);
      -- ****
   begin
      if HideInfo("recruit") then
         return;
      end if;
      HideUI;
      ShowRecruitUI;
   end ShowRecruit;

   procedure CreateOrdersMenu is
      Button: Gtk_Button;
   begin
      OrdersBox := Gtk_Button_Box(Get_Object(Builder, "btnboxorders"));
      Button := Gtk_Button_New_With_Mnemonic("_Recruit");
      On_Clicked(Button, ShowRecruit'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("Ask for _events");
      On_Clicked(Button, AskForEvents'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("Ask for _bases");
      On_Clicked(Button, AskForBases'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("Pray");
      On_Clicked(Button, ShowChurch'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("Heal _wounded");
      On_Clicked(Button, ShowHospital'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("Re_pair ship");
      On_Clicked(Button, ShowRepair'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("Sh_ipyard");
      On_Clicked(Button, ShowShipyard'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("Bu_y recipes");
      On_Clicked(Button, ShowRecipes'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("_Missions");
      On_Clicked(Button, ShowMissions'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("_Patrol area");
      On_Clicked(Button, StartMission'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("_Loot");
      On_Clicked(Button, BtnLootClicked'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("Set as _home");
      On_Clicked(Button, SetAsHome'Access);
      Pack_Start(OrdersBox, Button);
      Button := Gtk_Button_New_With_Mnemonic("_Close");
      On_Clicked(Button, HideOrders'Access);
      Pack_Start(OrdersBox, Button);
   end CreateOrdersMenu;

   function HideInfo(StageName: String) return Boolean is
   begin
      if Get_Visible_Child_Name(Gtk_Stack(Get_Object(Builder, "gamestack"))) =
        StageName then
         if PreviousGameState = Combat_View then
            Set_Visible_Child_Name
              (Gtk_Stack(Get_Object(Builder, "gamestack")), "combat");
         else
            Show_All(Gtk_Widget(Get_Object(Builder, "menuwait")));
            Show_All(Gtk_Widget(Get_Object(Builder, "menuorders")));
            Set_Visible_Child_Name
              (Gtk_Stack(Get_Object(Builder, "gamestack")), "skymap");
         end if;
         return True;
      end if;
      return False;
   end HideInfo;

end Maps.UI.OrdersMenu;
