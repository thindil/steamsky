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
with Ada.Containers; use Ada.Containers;
with GNAT.String_Split; use GNAT.String_Split;
with Gtk.Window; use Gtk.Window;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter; use Gtk.Text_Iter;
with Gtk.Text_Tag; use Gtk.Text_Tag;
with Gtk.Text_View; use Gtk.Text_View;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Container; use Gtk.Container;
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Stack; use Gtk.Stack;
with Gtk.Accel_Map; use Gtk.Accel_Map;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Glib; use Glib;
with Gdk.Rectangle; use Gdk.Rectangle;
with Gdk.Device; use Gdk.Device;
with Gdk.Window; use Gdk.Window;
with Gdk.Types; use Gdk.Types;
with Gdk.Device_Manager; use Gdk.Device_Manager;
with Gdk.Screen; use Gdk.Screen;
with Game; use Game;
with Utils; use Utils;
with Utils.UI; use Utils.UI;
with Ships.UI; use Ships.UI;
with Ships.Movement; use Ships.Movement;
with Ships.Crew; use Ships.Crew;
with Ships.Cargo; use Ships.Cargo;
with Ships.Cargo.UI; use Ships.Cargo.UI;
with Messages; use Messages;
with Messages.UI; use Messages.UI;
with Crew; use Crew;
with Crew.UI; use Crew.UI;
with ShipModules; use ShipModules;
with Events; use Events;
with Events.UI; use Events.UI;
with Items; use Items;
with Config; use Config;
with Bases; use Bases;
with Bases.UI; use Bases.UI;
with Bases.SchoolUI; use Bases.SchoolUI;
with Bases.ShipyardUI; use Bases.ShipyardUI;
with Bases.LootUI; use Bases.LootUI;
with Bases.RecruitUI; use Bases.RecruitUI;
with Missions; use Missions;
with Missions.UI; use Missions.UI;
with Crafts; use Crafts;
with Combat; use Combat;
with Combat.UI; use Combat.UI;
with Help.UI; use Help.UI;
with Statistics.UI; use Statistics.UI;
with Trades; use Trades;
with Trades.UI; use Trades.UI;
with Crafts.UI; use Crafts.UI;
with BasesList; use BasesList;
with GameOptions; use GameOptions;
with Stories; use Stories;
with Stories.UI; use Stories.UI;
with Factions; use Factions;
with Themes; use Themes;

package body Maps.UI.Handlers is

   AccelsRemoved: Boolean := False;

   procedure QuitGameMenu(Object: access Gtkada_Builder_Record'Class) is
   begin
      if not QuitGame(Gtk_Window(Get_Object(Object, "skymapwindow"))) then
         ShowDialog
           ("Can't quit game.",
            Gtk_Window(Get_Object(Object, "skymapwindow")));
      end if;
   end QuitGameMenu;

   procedure HideMapInfoWindow(User_Data: access GObject_Record'Class) is
   begin
      Hide(Gtk_Window(User_Data));
      if User_Data = Get_Object(Builder, "orderswindow") then
         UpdateMapInfo;
      end if;
   end HideMapInfoWindow;

   procedure GetMapSize(Object: access Gtkada_Builder_Record'Class) is
      MapBuffer: constant Gtk_Text_Buffer :=
        Gtk_Text_Buffer(Get_Object(Object, "txtmap"));
      MapView: constant Gtk_Text_View :=
        Gtk_Text_View(Get_Object(Object, "mapview"));
      Iter: Gtk_Text_Iter;
      Location: Gdk_Rectangle;
      Result: Boolean;
   begin
      Get_Start_Iter(MapBuffer, Iter);
      Forward_Line(Iter, Result);
      Forward_Char(Iter, Result);
      Get_Iter_Location(MapView, Iter, Location);
      if Location.Y = 0 then
         return;
      end if;
      if (Get_Allocated_Height(Gtk_Widget(MapView)) / Location.Y) - 1 < 1 then
         return;
      end if;
      MapWidth :=
        Positive(Get_Allocated_Width(Gtk_Widget(MapView)) / Location.X) - 1;
      MapHeight :=
        Positive(Get_Allocated_Height(Gtk_Widget(MapView)) / Location.Y) - 1;
      MapCellWidth := Positive(Location.X);
      MapCellHeight := Positive(Location.Y);
      Set_Text(MapBuffer, "");
      DrawMap;
      Get_Size
        (Gtk_Window(Get_Object(Object, "skymapwindow")),
         Gint(GameSettings.WindowWidth), Gint(GameSettings.WindowHeight));
   end GetMapSize;

   function SetDestination
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      if PlayerShip.SkyX = MapX and PlayerShip.SkyY = MapY then
         ShowOrders(Object);
         return True;
      end if;
      PlayerShip.DestinationX := MapX;
      PlayerShip.DestinationY := MapY;
      AddMessage("You set travel destination for your ship.", OrderMessage);
      UpdateMessages;
      UpdateMoveButtons;
      return True;
   end SetDestination;

   procedure MoveMap(User_Data: access GObject_Record'Class) is
   begin
      if User_Data = Get_Object(Builder, "btncenter") then
         CenterX := PlayerShip.SkyX;
         CenterY := PlayerShip.SkyY;
      elsif User_Data = Get_Object(Builder, "btnmovemapok") then
         CenterX :=
           Positive(Get_Value(Gtk_Adjustment(Get_Object(Builder, "mapxadj"))));
         CenterY :=
           Positive(Get_Value(Gtk_Adjustment(Get_Object(Builder, "mapyadj"))));
      elsif User_Data = Get_Object(Builder, "btnmapup") then
         if CenterY - (MapHeight / 3) < 1 then
            CenterY := MapHeight / 3;
         else
            CenterY := CenterY - (MapHeight / 3);
         end if;
      elsif User_Data = Get_Object(Builder, "btnmapdown") then
         if CenterY + (MapHeight / 3) > 1024 then
            CenterY := 1024 - (MapHeight / 3);
         else
            CenterY := CenterY + (MapHeight / 3);
         end if;
      elsif User_Data = Get_Object(Builder, "btnmapleft") then
         if CenterX - (MapWidth / 3) < 1 then
            CenterX := MapWidth / 3;
         else
            CenterX := CenterX - (MapWidth / 3);
         end if;
      elsif User_Data = Get_Object(Builder, "btnmapright") then
         if CenterX + (MapWidth / 3) > 1024 then
            CenterX := 1024 - (MapWidth / 3);
         else
            CenterX := CenterX + (MapWidth / 3);
         end if;
      elsif User_Data = Get_Object(Builder, "btncenterhomebase") then
         CenterX := SkyBases(PlayerShip.HomeBase).SkyX;
         CenterY := SkyBases(PlayerShip.HomeBase).SkyY;
      end if;
      Set_Text(Gtk_Text_Buffer(Get_Object(Builder, "txtmap")), "");
      DrawMap;
      Hide(Gtk_Widget(Get_Object(Builder, "movemapwindow")));
   end MoveMap;

   procedure BtnDockClicked(Object: access Gtkada_Builder_Record'Class) is
      Message: Unbounded_String := Null_Unbounded_String;
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "orderswindow")));
      if PlayerShip.Speed = DOCKED then
         Message := To_Unbounded_String(DockShip(False));
         if Length(Message) > 0 then
            ShowDialog
              (To_String(Message),
               Gtk_Window(Get_Object(Object, "skymapwindow")));
            return;
         end if;
      else
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
            if Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                .EType =
              FullDocks then
               ShowWaitOrders(Object);
               return;
            end if;
         end if;
         Message := To_Unbounded_String(DockShip(True));
         if Length(Message) > 0 then
            ShowDialog
              (To_String(Message),
               Gtk_Window(Get_Object(Object, "skymapwindow")));
            return;
         end if;
         ShowOrders(Object);
      end if;
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
   end BtnDockClicked;

   procedure ChangeSpeed(Object: access Gtkada_Builder_Record'Class) is
   begin
      PlayerShip.Speed :=
        ShipSpeed'Val
          (Get_Active(Gtk_Combo_Box(Get_Object(Object, "cmbspeed"))) + 1);
   end ChangeSpeed;

   procedure MoveShip(User_Data: access GObject_Record'Class) is
      Message: Unbounded_String;
      Result: Natural;
      StartsCombat: Boolean := False;
      NewX, NewY: Integer := 0;
   begin
      if User_Data = Get_Object(Builder, "btnup") then -- Move up
         Result := MoveShip(0, -1, Message);
      elsif User_Data = Get_Object(Builder, "btnbottom") then -- Move down
         Result := MoveShip(0, 1, Message);
      elsif User_Data = Get_Object(Builder, "btnright") then -- Move right
         Result := MoveShip(1, 0, Message);
      elsif User_Data = Get_Object(Builder, "btnleft") then -- Move left
         Result := MoveShip(-1, 0, Message);
      elsif User_Data =
        Get_Object(Builder, "btnbottomleft") then -- Move down/left
         Result := MoveShip(-1, 1, Message);
      elsif User_Data =
        Get_Object(Builder, "btnbottomright") then -- Move down/right
         Result := MoveShip(1, 1, Message);
      elsif User_Data = Get_Object(Builder, "btnupleft") then -- Move up/left
         Result := MoveShip(-1, -1, Message);
      elsif User_Data = Get_Object(Builder, "btnupright") then -- Move up/right
         Result := MoveShip(1, -1, Message);
      elsif User_Data =
        Get_Object
          (Builder,
           "btnmovewait") then -- Move to destination or wait 1 game minute
         if PlayerShip.DestinationX = 0 and PlayerShip.DestinationY = 0 then
            Result := 1;
            UpdateGame(1);
            WaitInPlace(1);
         else
            if PlayerShip.DestinationX > PlayerShip.SkyX then
               NewX := 1;
            elsif PlayerShip.DestinationX < PlayerShip.SkyX then
               NewX := -1;
            end if;
            if PlayerShip.DestinationY > PlayerShip.SkyY then
               NewY := 1;
            elsif PlayerShip.DestinationY < PlayerShip.SkyY then
               NewY := -1;
            end if;
            Result := MoveShip(NewX, NewY, Message);
            if PlayerShip.DestinationX = PlayerShip.SkyX and
              PlayerShip.DestinationY = PlayerShip.SkyY then
               AddMessage
                 ("You reached your travel destination.", OrderMessage);
               PlayerShip.DestinationX := 0;
               PlayerShip.DestinationY := 0;
               if GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
               Result := 4;
            end if;
         end if;
      elsif User_Data =
        Get_Object(Builder, "btnmoveto") then -- Move to destination
         loop
            NewX := 0;
            NewY := 0;
            if PlayerShip.DestinationX > PlayerShip.SkyX then
               NewX := 1;
            elsif PlayerShip.DestinationX < PlayerShip.SkyX then
               NewX := -1;
            end if;
            if PlayerShip.DestinationY > PlayerShip.SkyY then
               NewY := 1;
            elsif PlayerShip.DestinationY < PlayerShip.SkyY then
               NewY := -1;
            end if;
            Result := MoveShip(NewX, NewY, Message);
            exit when Result = 0;
            StartsCombat := CheckForEvent;
            if StartsCombat then
               Result := 4;
               exit;
            end if;
            if Result = 8 then
               WaitForRest;
               if FindMember(Pilot) = 0 or FindMember(Engineer) = 0 then
                  WaitForRest;
               end if;
               Result := 1;
               StartsCombat := CheckForEvent;
               if StartsCombat then
                  Result := 4;
                  exit;
               end if;
            end if;
            if GameSettings.AutoMoveStop /= NEVER and
              SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
               declare
                  EventIndex: constant Positive :=
                    SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
               begin
                  case GameSettings.AutoMoveStop is
                     when ANY =>
                        if Events_List(EventIndex).EType = EnemyShip or
                          Events_List(EventIndex).EType = Trader or
                          Events_List(EventIndex).EType = FriendlyShip or
                          Events_List(EventIndex).EType = EnemyPatrol then
                           Result := 0;
                           exit;
                        end if;
                     when FRIENDLY =>
                        if Events_List(EventIndex).EType = Trader or
                          Events_List(EventIndex).EType = FriendlyShip then
                           Result := 0;
                           exit;
                        end if;
                     when Config.ENEMY =>
                        if Events_List(EventIndex).EType = EnemyShip or
                          Events_List(EventIndex).EType = EnemyPatrol then
                           Result := 0;
                           exit;
                        end if;
                     when NEVER =>
                        null;
                  end case;
               end;
            end if;
            if PlayerShip.DestinationX = PlayerShip.SkyX and
              PlayerShip.DestinationY = PlayerShip.SkyY then
               AddMessage
                 ("You reached your travel destination.", OrderMessage);
               PlayerShip.DestinationX := 0;
               PlayerShip.DestinationY := 0;
               if GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
               Result := 4;
               exit;
            end if;
            exit when Result = 6 or Result = 7;
         end loop;
      end if;
      case Result is
         when 1 => -- Ship moved, check for events
            StartsCombat := CheckForEvent;
            if not StartsCombat and GameSettings.AutoFinish then
               Message := To_Unbounded_String(AutoFinishMissions);
            end if;
         when 6 => -- Ship moved, but pilot needs rest, confirm
            if ShowConfirmDialog
                ("You don't have pilot on duty. Did you want to wait until your pilot rest?",
                 Gtk_Window(Get_Object(Builder, "skymapwindow"))) then
               WaitForRest;
               StartsCombat := CheckForEvent;
               if not StartsCombat and GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
            end if;
         when 7 => -- Ship moved, but engineer needs rest, confirm
            if ShowConfirmDialog
                ("You don't have engineer on duty. Did you want to wait until your engineer rest?",
                 Gtk_Window(Get_Object(Builder, "skymapwindow"))) then
               WaitForRest;
               StartsCombat := CheckForEvent;
               if not StartsCombat and GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
            end if;
         when 8 => -- Ship moved, but crew needs rest, autorest
            StartsCombat := CheckForEvent;
            if not StartsCombat then
               WaitForRest;
               if FindMember(Pilot) = 0 or FindMember(Engineer) = 0 then
                  WaitForRest;
               end if;
               StartsCombat := CheckForEvent;
            end if;
            if not StartsCombat and GameSettings.AutoFinish then
               Message := To_Unbounded_String(AutoFinishMissions);
            end if;
         when others =>
            null;
      end case;
      if Message /= Null_Unbounded_String then
         ShowDialog
           (To_String(Message),
            Gtk_Window(Get_Object(Builder, "skymapwindow")));
      end if;
      CenterX := PlayerShip.SkyX;
      CenterY := PlayerShip.SkyY;
      if StartsCombat then
         ShowCombatUI;
      else
         UpdateHeader;
         UpdateMessages;
         UpdateMoveButtons;
         DrawMap;
      end if;
   end MoveShip;

   procedure ShowOrders(Object: access Gtkada_Builder_Record'Class) is
      HaveTrader: Boolean := False;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MissionsLimit: Integer;
      Event: Events_Types := None;
      ItemIndex: Natural;
   begin
      UpdateMapInfo(True);
      Foreach
        (Gtk_Container(Get_Object(Object, "btnboxorders")),
         HideButtons'Access);
      if FindMember(Talk) > 0 then
         HaveTrader := True;
      end if;
      Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btncloseorders")), False);
      if CurrentStory.Index > 0 then
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
                             (ProtoShips_List(Positive'Value(Slice(Tokens, 3)))
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
            if SkyBases(BaseIndex).BaseType = Shipyard then
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnshipyard")), False);
            end if;
            for I in Recipes_List.Iterate loop
               if Known_Recipes.Find_Index(Item => Recipes_Container.Key(I)) =
                 UnboundedString_Container.No_Index and
                 Recipes_List(I).BaseType =
                   Bases_Types'Pos(SkyBases(BaseIndex).BaseType) + 1 then
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
                                   (ProtoShips_List(Mission.Target).Name));
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
               Set_No_Show_All
                 (Gtk_Widget(Get_Object(Object, "btnsethome")), False);
            end if;
         end if;
         if SkyBases(BaseIndex).Population = 0 then
            Set_No_Show_All(Gtk_Widget(Get_Object(Object, "btnloot")), False);
         end if;
      else
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
                     Set_Label
                       (Gtk_Button(Get_Object(Builder, "btndock")), "_Dock");
                     Set_No_Show_All
                       (Gtk_Widget(Get_Object(Object, "btndock")), False);
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
                                      (ProtoShips_List(Mission.Target).Name));
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
                                   (ProtoShips_List(Mission.Target).Name));
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
                            .Data)
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
      Foreach
        (Gtk_Container(Get_Object(Object, "btnboxorders")),
         CheckButtons'Access);
      if ButtonsVisible then
         Show_All(Gtk_Widget(Get_Object(Object, "orderswindow")));
      else
         ShowDialog
           ("Here are no available ship orders at this moment. Ship orders available mostly when you are at base or at event on map.",
            Gtk_Window(Get_Object(Object, "skymapwindow")));
      end if;
   end ShowOrders;

   procedure WaitOrder(User_Data: access GObject_Record'Class) is
      TimeNeeded: Natural := 0;
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "waitwindow")));
      if User_Data = Get_Object(Builder, "btnwait1min") then
         UpdateGame(1);
         WaitInPlace(1);
      elsif User_Data = Get_Object(Builder, "btnwait5min") then
         UpdateGame(5);
         WaitInPlace(5);
      elsif User_Data = Get_Object(Builder, "btnwait10min") then
         UpdateGame(10);
         WaitInPlace(10);
      elsif User_Data = Get_Object(Builder, "btnwait15min") then
         UpdateGame(15);
         WaitInPlace(15);
      elsif User_Data = Get_Object(Builder, "btnwait30min") then
         UpdateGame(30);
         WaitInPlace(30);
      elsif User_Data = Get_Object(Builder, "btnwait1hour") then
         UpdateGame(60);
         WaitInPlace(60);
      elsif User_Data = Get_Object(Builder, "btnwaitrest") then
         WaitForRest;
      elsif User_Data = Get_Object(Builder, "btnwaitheal") then
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Health < 100 and
              PlayerShip.Crew(I).Health > 0 and
              PlayerShip.Crew(I).Order = Rest then
               for Module of PlayerShip.Modules loop
                  if Modules_List(Module.ProtoIndex).MType = CABIN and
                    Module.Owner = Crew_Container.To_Index(I) then
                     if TimeNeeded <
                       (100 - PlayerShip.Crew(I).Health) * 15 then
                        TimeNeeded := (100 - PlayerShip.Crew(I).Health) * 15;
                     end if;
                     exit;
                  end if;
               end loop;
            end if;
         end loop;
         if TimeNeeded > 0 then
            UpdateGame(TimeNeeded);
            WaitInPlace(TimeNeeded);
         else
            return;
         end if;
      elsif User_Data = Get_Object(Builder, "waitxadj") then
         UpdateGame(Positive(Get_Value(Gtk_Adjustment(User_Data))));
      end if;
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
      DrawMap;
   end WaitOrder;

   procedure AttackOrder(Object: access Gtkada_Builder_Record'Class) is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "orderswindow")));
      ShowCombatUI;
   end AttackOrder;

   procedure ShowHelp(Object: access Gtkada_Builder_Record'Class) is
      VisibleChildName: constant String :=
        Get_Visible_Child_Name(Gtk_Stack(Get_Object(Object, "gamestack")));
   begin
      if VisibleChildName = "combat" then
         ShowHelpUI(5);
      elsif VisibleChildName = "crafts" then
         ShowHelpUI(6);
      elsif VisibleChildName = "crew" then
         ShowHelpUI(8);
      elsif VisibleChildName = "ship" then
         ShowHelpUI(7);
      elsif VisibleChildName = "trade" then
         ShowHelpUI(4);
      elsif VisibleChildName = "availablemissions" or
        VisibleChildName = "acceptedmissions" then
         ShowHelpUI(9);
      elsif VisibleChildName = "recruit" then
         ShowHelpUI(10);
      else
         ShowHelpUI(1);
      end if;
   end ShowHelp;

   procedure ShowInfo(User_Data: access GObject_Record'Class) is
      VisibleChildName: constant String :=
        Get_Visible_Child_Name(Gtk_Stack(Get_Object(Builder, "gamestack")));
   begin
      if User_Data = Get_Object(Builder, "menumissions") then
         if AcceptedMissions.Length = 0 then
            ShowDialog
              ("You didn't accepted any mission yet. You may ask for missions in bases. When your ship is docked to base, check Missions from ship orders menu.",
               Gtk_Window(Get_Object(Builder, "skymapwindow")));
            return;
         end if;
      elsif User_Data = Get_Object(Builder, "menuevents") then
         if Events_List.Length = 0 then
            ShowDialog
              ("You dont know any event yet. You may ask for events in bases. When your ship is docked to base, select Ask for Events from ship orders menu.",
               Gtk_Window(Get_Object(Builder, "skymapwindow")));
            return;
         end if;
      elsif User_Data = Get_Object(Builder, "menustory") then
         if FinishedStories.Length = 0 then
            ShowDialog
              ("You didn't discovered any story yet.",
               Gtk_Window(Get_Object(Builder, "skymapwindow")));
            return;
         end if;
      end if;
      Show_All(Gtk_Widget(Get_Object(Builder, "btnclose")));
      HideLastMessage(Builder);
      if VisibleChildName = "combat" then
         PreviousGameState := Combat_View;
      elsif VisibleChildName = "skymap" then
         PreviousGameState := SkyMap_View;
         Hide(Gtk_Widget(Get_Object(Builder, "menuwait")));
         Hide(Gtk_Widget(Get_Object(Builder, "menumovemap")));
         Hide(Gtk_Widget(Get_Object(Builder, "menuorders")));
      end if;
      if User_Data = Get_Object(Builder, "menumessages") then
         ShowMessagesUI;
      elsif User_Data = Get_Object(Builder, "menucargo") then
         ShowCargoUI;
      elsif User_Data = Get_Object(Builder, "menuship") then
         ShowShipUI;
      elsif User_Data = Get_Object(Builder, "menucrew") then
         ShowCrewUI;
      elsif User_Data = Get_Object(Builder, "menustats") then
         ShowStatsUI;
      elsif User_Data = Get_Object(Builder, "menumissions") then
         ShowAcceptedMissions;
      elsif User_Data = Get_Object(Builder, "btntrade") then
         Hide(Gtk_Widget(Get_Object(Builder, "orderswindow")));
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex = 0 then
            GenerateTraderCargo
              (Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                 .Data);
         end if;
         ShowTradeUI;
      elsif User_Data = Get_Object(Builder, "btnrecruit") then
         Hide(Gtk_Widget(Get_Object(Builder, "orderswindow")));
         ShowRecruitUI;
      elsif User_Data = Get_Object(Builder, "btnrecipes") then
         Hide(Gtk_Widget(Get_Object(Builder, "orderswindow")));
         ShowBuyRecipesUI;
      elsif User_Data = Get_Object(Builder, "btnrepair") then
         Hide(Gtk_Widget(Get_Object(Builder, "orderswindow")));
         ShowRepairUI;
      elsif User_Data = Get_Object(Builder, "btnheal") then
         Hide(Gtk_Widget(Get_Object(Builder, "orderswindow")));
         ShowHealUI;
      elsif User_Data = Get_Object(Builder, "btnschool") then
         Hide(Gtk_Widget(Get_Object(Builder, "orderswindow")));
         ShowSchoolUI;
      elsif User_Data = Get_Object(Builder, "btnshipyard") then
         Hide(Gtk_Widget(Get_Object(Builder, "orderswindow")));
         ShowShipyardUI;
      elsif User_Data = Get_Object(Builder, "btnloot") then
         Hide(Gtk_Widget(Get_Object(Builder, "orderswindow")));
         ShowLootUI;
      elsif User_Data = Get_Object(Builder, "menucrafting") then
         ShowCraftsUI;
      elsif User_Data = Get_Object(Builder, "menubaseslist") then
         ShowBasesListUI;
      elsif User_Data = Get_Object(Builder, "menuevents") then
         ShowEventsUI;
      elsif User_Data = Get_Object(Builder, "menuoptions") then
         ShowGameOptions;
      elsif User_Data = Get_Object(Builder, "menustory") then
         ShowStoriesUI;
      end if;
   end ShowInfo;

   procedure ResignFromGame(Object: access Gtkada_Builder_Record'Class) is
   begin
      if ShowConfirmDialog
          ("Are you sure want to resign from game?",
           Gtk_Window(Get_Object(Object, "skymapwindow"))) then
         Death(1, To_Unbounded_String("resignation"), PlayerShip);
         DeathConfirm;
      end if;
   end ResignFromGame;

   procedure ShowMissions(Object: access Gtkada_Builder_Record'Class) is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "orderswindow")));
      Show_All(Gtk_Widget(Get_Object(Builder, "btnclose")));
      ShowMissionsUI;
   end ShowMissions;

   procedure StartMission(Object: access Gtkada_Builder_Record'Class) is
      StartsCombat: Boolean := False;
   begin
      Hide(Gtk_Widget(Get_Object(Object, "orderswindow")));
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
                            .Target,
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

   procedure CompleteMission(Object: access Gtkada_Builder_Record'Class) is
   begin
      Hide(Gtk_Widget(Get_Object(Object, "orderswindow")));
      FinishMission(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).MissionIndex);
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
      DrawMap;
   end CompleteMission;

   procedure ExecuteOrder(User_Data: access GObject_Record'Class) is
      TraderIndex: constant Natural := FindMember(Talk);
      Price: Positive := 1000;
      MoneyIndex2: constant Natural := FindItem(PlayerShip.Cargo, MoneyIndex);
   begin
      Hide(Gtk_Widget(Get_Object(Builder, "orderswindow")));
      if User_Data = Get_Object(Builder, "btnaskevents") then
         AskForEvents;
      elsif User_Data = Get_Object(Builder, "btnaskbases") then
         AskForBases;
      elsif User_Data = Get_Object(Builder, "btnstory") then
         declare
            Step: Step_Data;
            Message: Unbounded_String;
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
            if PlayerShip.Speed /= DOCKED and
              Step.FinishCondition = ASKINBASE then
               Message := To_Unbounded_String(DockShip(True));
               if Message /= Null_Unbounded_String then
                  ShowDialog
                    (To_String(Message),
                     Gtk_Window(Get_Object(Builder, "skymapwindow")));
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
                            (Positive'Value(Slice(Tokens, 3)), False) then
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
                           ShowDialog
                             (To_String(Text.Text),
                              Gtk_Window(Get_Object(Builder, "skymapwindow")));
                           CurrentStory.ShowText := False;
                           exit;
                        end if;
                     end loop;
                  else
                     FinishStory;
                  end if;
               end;
            else
               ShowDialog
                 (To_String(Step.FailText),
                  Gtk_Window(Get_Object(Builder, "skymapwindow")));
               CurrentStory.ShowText := False;
            end if;
         end;
      else
         CountPrice(Price, TraderIndex);
         if ShowConfirmDialog
             ("Are you sure want to change your home base (it cost" &
              Positive'Image(Price) & " " & To_String(MoneyName) & ")?",
              Gtk_Window(Get_Object(Builder, "skymapwindow"))) then
            if MoneyIndex2 = 0 then
               ShowDialog
                 ("You don't have any " & To_String(MoneyName) &
                  " for change ship home base.",
                  Gtk_Window(Get_Object(Builder, "skymapwindow")));
               return;
            end if;
            CountPrice(Price, TraderIndex);
            if PlayerShip.Cargo(MoneyIndex2).Amount < Price then
               ShowDialog
                 ("You don't have enough " & To_String(MoneyName) &
                  " for change ship home base.",
                  Gtk_Window(Get_Object(Builder, "skymapwindow")));
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
      end if;
      UpdateHeader;
      UpdateMessages;
      UpdateMoveButtons;
      DrawMap;
   end ExecuteOrder;

   procedure DeliverMedicines(User_Data: access GObject_Record'Class) is
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      ItemIndex: constant Natural :=
        FindItem
          (Inventory => PlayerShip.Cargo,
           ItemType => Factions_List(SkyBases(BaseIndex).Owner).HealingTools);
      NewTime: constant Integer :=
        Events_List(EventIndex).Time - PlayerShip.Cargo(ItemIndex).Amount;
   begin
      if NewTime < 1 then
         DeleteEvent(EventIndex);
      else
         Events_List(EventIndex).Time := NewTime;
      end if;
      if User_Data = Get_Object(Builder, "btnfreemedicines") then
         GainRep(BaseIndex, (PlayerShip.Cargo(ItemIndex).Amount / 10));
         UpdateCargo
           (PlayerShip, PlayerShip.Cargo.Element(ItemIndex).ProtoIndex,
            (0 - PlayerShip.Cargo.Element(ItemIndex).Amount));
         AddMessage
           ("You gave " &
            To_String
              (Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Name) &
            " for free to base.",
            TradeMessage);
      else
         SellItems
           (ItemIndex,
            Integer'Image(PlayerShip.Cargo.Element(ItemIndex).Amount));
         GainRep
           (BaseIndex, ((PlayerShip.Cargo(ItemIndex).Amount / 20) * (-1)));
      end if;
   end DeliverMedicines;

   procedure ShowWaitOrders(Object: access Gtkada_Builder_Record'Class) is
      NeedHealing, NeedRest: Boolean := False;
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
      Set_Visible(Gtk_Widget(Get_Object(Object, "btnwaitheal")), NeedHealing);
      Set_Visible(Gtk_Widget(Get_Object(Object, "btnwaitrest")), NeedRest);
      Show_All(Gtk_Widget(Get_Object(Object, "waitwindow")));
   end ShowWaitOrders;

   function UpdateTooltip
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
      pragma Unreferenced(Object);
   begin
      UpdateMapInfo;
      return False;
   end UpdateTooltip;

   function MapKeyReleased(Self: access Gtk_Widget_Record'Class;
      Event: Gdk.Event.Gdk_Event_Key) return Boolean is
      pragma Unreferenced(Self);
      KeyMods: constant Gdk_Modifier_Type :=
        Event.State and Get_Default_Mod_Mask;
      Key: Gtk_Accel_Key;
      Found: Boolean;
      function CenterMapOn(AccelName, ButtonName: String) return Boolean is
      begin
         Lookup_Entry(AccelName, Key, Found);
         if not Found then
            return False;
         end if;
         if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
            MoveMap(Get_Object(Builder, ButtonName));
            return True;
         end if;
         return True;
      end CenterMapOn;
   begin
      if Get_Visible_Child_Name(Gtk_Stack(Get_Object(Builder, "gamestack"))) /=
        "skymap" then
         return True;
      end if;
      if not CenterMapOn("<movemapwindow>/btncenter", "btncenter") then
         return True;
      end if;
      if not CenterMapOn
          ("<movemapwindow>/btncenterhomebase", "btncenterhomebase") then
         return True;
      end if;
      Lookup_Entry("<skymapwindow>/mouseclick", Key, Found);
      if not Found then
         return True;
      end if;
      if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
         return SetDestination(Builder);
      end if;
      Lookup_Entry("<skymapwindow>/zoomin", Key, Found);
      if not Found then
         return True;
      end if;
      if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
         GameSettings.MapFontSize := GameSettings.MapFontSize - 1;
         if GameSettings.MapFontSize < 3 then
            GameSettings.MapFontSize := 3;
         end if;
         SetFontSize(MAPFONT);
         return False;
      end if;
      Lookup_Entry("<skymapwindow>/zoomout", Key, Found);
      if not Found then
         return True;
      end if;
      if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
         GameSettings.MapFontSize := GameSettings.MapFontSize + 1;
         if GameSettings.MapFontSize > 30 then
            GameSettings.MapFontSize := 30;
         end if;
         SetFontSize(MAPFONT);
         return False;
      end if;
      return True;
   end MapKeyReleased;

   function MapKeyPressed(Self: access Gtk_Widget_Record'Class;
      Event: Gdk.Event.Gdk_Event_Key) return Boolean is
      pragma Unreferenced(Self);
      KeyMods: constant Gdk_Modifier_Type :=
        Event.State and Get_Default_Mod_Mask;
      MouseX, MouseY, NewX, NewY: Gint;
      DeviceManager: constant Gdk_Device_Manager :=
        Get_Device_Manager
          (Get_Display(Gtk_Widget(Get_Object(Builder, "mapview"))));
      Mouse: constant Gdk_Device := Get_Client_Pointer(DeviceManager);
      Screen: Gdk_Screen :=
        Get_Screen(Get_Window(Gtk_Widget(Get_Object(Builder, "mapview"))));
      KeysNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("<skymapwindow>/cursorupleft"),
         To_Unbounded_String("<skymapwindow>/cursorup"),
         To_Unbounded_String("<skymapwindow>/cursorupright"),
         To_Unbounded_String("<skymapwindow>/cursorleft"),
         To_Unbounded_String("<skymapwindow>/cursorright"),
         To_Unbounded_String("<skymapwindow>/cursordownleft"),
         To_Unbounded_String("<skymapwindow>/cursordown"),
         To_Unbounded_String("<skymapwindow>/cursordownright"),
         To_Unbounded_String("<skymapwindow>/btnmapleft"),
         To_Unbounded_String("<skymapwindow>/btnmapright"),
         To_Unbounded_String("<skymapwindow>/btnmapup"),
         To_Unbounded_String("<skymapwindow>/btnmapdown"));
      Key: Gtk_Accel_Key;
      Found: Boolean;
   begin
      if Get_Visible_Child_Name(Gtk_Stack(Get_Object(Builder, "gamestack"))) /=
        "skymap" then
         return False;
      end if;
      Get_Position_Double(Mouse, Screen, Gdouble(MouseX), Gdouble(MouseY));
      if MouseX < 0 or MouseY < 0 then
         return False;
      end if;
      NewX := MouseX;
      NewY := MouseY;
      for I in KeysNames'Range loop
         Lookup_Entry(To_String(KeysNames(I)), Key, Found);
         if not Found then
            return False;
         end if;
         if Key.Accel_Key = Event.Keyval and Key.Accel_Mods = KeyMods then
            case I is
               when 1 =>
                  NewX := NewX - Gint(MapCellWidth);
                  NewY := NewY - Gint(MapCellHeight);
               when 2 =>
                  NewY := NewY - Gint(MapCellHeight);
               when 3 =>
                  NewX := NewX + Gint(MapCellWidth);
                  NewY := NewY - Gint(MapCellHeight);
               when 4 =>
                  NewX := NewX - Gint(MapCellWidth);
               when 5 =>
                  NewX := NewX + Gint(MapCellWidth);
               when 6 =>
                  NewX := NewX - Gint(MapCellWidth);
                  NewY := NewY + Gint(MapCellHeight);
               when 7 =>
                  NewY := NewY + Gint(MapCellHeight);
               when 8 =>
                  NewX := NewX + Gint(MapCellWidth);
                  NewY := NewY + Gint(MapCellHeight);
               when 9 =>
                  MoveMap(Get_Object(Builder, "btnmapleft"));
               when 10 =>
                  MoveMap(Get_Object(Builder, "btnmapright"));
               when 11 =>
                  MoveMap(Get_Object(Builder, "btnmapup"));
               when 12 =>
                  MoveMap(Get_Object(Builder, "btnmapdown"));
               when others =>
                  null;
            end case;
            exit;
         end if;
      end loop;
      if NewX /= MouseX or NewY /= MouseY then
         Warp(Mouse, Screen, NewX, NewY);
         UpdateMapInfo;
         return True;
      end if;
      return False;
   end MapKeyPressed;

   function ZoomMap(Self: access Gtk_Widget_Record'Class;
      Event: Gdk.Event.Gdk_Event_Scroll) return Boolean is
      pragma Unreferenced(Self);
   begin
      if Event.Direction = Scroll_Down then
         GameSettings.MapFontSize := GameSettings.MapFontSize - 1;
         if GameSettings.MapFontSize < 3 then
            GameSettings.MapFontSize := 3;
         end if;
      elsif Event.Direction = Scroll_Up then
         GameSettings.MapFontSize := GameSettings.MapFontSize + 1;
         if GameSettings.MapFontSize > 30 then
            GameSettings.MapFontSize := 30;
         end if;
      end if;
      SetFontSize(MAPFONT);
      return False;
   end ZoomMap;

   function DisableMenuShortcuts
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      if AccelsRemoved then
         return False;
      end if;
      Remove_Accel_Group
        (Gtk_Window(Get_Object(Object, "skymapwindow")),
         Gtk_Accel_Group(Get_Object(Object, "movementaccels")));
      AccelsRemoved := True;
      return False;
   end DisableMenuShortcuts;

   function EnableMenuShortcuts
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
   begin
      if not AccelsRemoved then
         return False;
      end if;
      Add_Accel_Group
        (Gtk_Window(Get_Object(Object, "skymapwindow")),
         Gtk_Accel_Group(Get_Object(Object, "movementaccels")));
      AccelsRemoved := False;
      return False;
   end EnableMenuShortcuts;

   procedure DisableMenuShortcutsProc
     (Object: access Gtkada_Builder_Record'Class) is
   begin
      if AccelsRemoved then
         return;
      end if;
      Remove_Accel_Group
        (Gtk_Window(Get_Object(Object, "skymapwindow")),
         Gtk_Accel_Group(Get_Object(Object, "movementaccels")));
      AccelsRemoved := True;
   end DisableMenuShortcutsProc;

   procedure EnableMenuShortcutsProc
     (Object: access Gtkada_Builder_Record'Class) is
   begin
      if not AccelsRemoved then
         return;
      end if;
      Add_Accel_Group
        (Gtk_Window(Get_Object(Object, "skymapwindow")),
         Gtk_Accel_Group(Get_Object(Object, "movementaccels")));
      AccelsRemoved := False;
   end EnableMenuShortcutsProc;

   function ToggleCloseButton
     (User_Data: access GObject_Record'Class) return Boolean is
      Button: constant Gtk_Widget := Gtk_Widget(User_Data);
   begin
      Set_Sensitive(Button, not Get_Sensitive(Button));
      return False;
   end ToggleCloseButton;

   procedure ToggleCloseButtonProc(User_Data: access GObject_Record'Class) is
      Button: constant Gtk_Widget := Gtk_Widget(User_Data);
   begin
      Set_Sensitive(Button, not Get_Sensitive(Button));
   end ToggleCloseButtonProc;

   function MoveMapInfo
     (Object: access Gtkada_Builder_Record'Class) return Boolean is
      MapInfo: constant Gtk_Widget :=
        Gtk_Widget(Get_Object(Object, "eventmaptooltip"));
   begin
      if Get_Valign(MapInfo) = Align_Start then
         Set_Valign(MapInfo, Align_End);
      else
         Set_Valign(MapInfo, Align_Start);
      end if;
      return False;
   end MoveMapInfo;

   function ShowMapButton
     (User_Data: access GObject_Record'Class) return Boolean is
   begin
      Show_All(Gtk_Widget(User_Data));
      return False;
   end ShowMapButton;

   function HideMapButton
     (User_Data: access GObject_Record'Class) return Boolean is
   begin
      Hide(Gtk_Widget(User_Data));
      return False;
   end HideMapButton;

end Maps.UI.Handlers;
