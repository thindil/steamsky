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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Crafts; use Crafts;
with Crew; use Crew;
with Events; use Events;
with Game; use Game;
with Items; use Items;
with Maps; use Maps;
with Missions; use Missions;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Stories; use Stories;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body OrdersMenu is

   -- ****if* OrdersMenu/Show_Orders_Command
   -- FUNCTION
   -- Add available options and show orders menu to the player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Orders_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Orders_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      HaveTrader: Boolean := False;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MissionsLimit: Integer;
      Event: Events_Types := None;
      ItemIndex: Natural;
      OrdersMenu: Tk_Menu;
   begin
      OrdersMenu.Interp := Interp;
      OrdersMenu.Name := New_String(".ordersmenu");
      if Winfo_Get(OrdersMenu, "ismapped") = "1" then
         if Invoke(OrdersMenu, "end") /= "" then
            return TCL_ERROR;
         end if;
         return TCL_OK;
      end if;
      Delete(OrdersMenu, "0", "end");
      if FindMember(Talk) > 0 then
         HaveTrader := True;
      end if;
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
                       Add(OrdersMenu, "command", "-label {Ask for " &
                           To_String
                             (Items_List(GetStepData(Step.FinishData, "item"))
                                .Name) & "} -underline 4");
                     end if;
                  end if;
               when DESTROYSHIP =>
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create(Tokens, To_String(CurrentStory.Data), ";");
                     if PlayerShip.SkyX = Positive'Value(Slice(Tokens, 1)) and
                       PlayerShip.SkyY = Positive'Value(Slice(Tokens, 2)) then
                           Add(OrdersMenu, "command", "-label {Search for " &
                           To_String
                             (ProtoShips_List
                                (To_Unbounded_String(Slice(Tokens, 3)))
                                .Name) & "} -underline 0");
                     end if;
                  end;
               when EXPLORE =>
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create(Tokens, To_String(CurrentStory.Data), ";");
                     if PlayerShip.SkyX = Positive'Value(Slice(Tokens, 1)) and
                       PlayerShip.SkyY = Positive'Value(Slice(Tokens, 2)) then
                       Add(OrdersMenu, "command", "-label {Search area} -underline 0");
                     end if;
                  end;
               when ANY | LOOT =>
                  null;
            end case;
         end;
      end if;
      if PlayerShip.Speed = DOCKED then
         Add(OrdersMenu, "command", "-label {Undock} -underline 0");
         if HaveTrader and SkyBases(BaseIndex).Population > 0 then
            Add(OrdersMenu, "command", "-label {Trade} -underline 0");
            Add(OrdersMenu, "command", "-label {School} -underline 0");
            if SkyBases(BaseIndex).Recruits.Length > 0 then
               Add(OrdersMenu, "command", "-label {Recruit} -underline 0");
            end if;
            if DaysDifference(SkyBases(BaseIndex).AskedForEvents) > 6 then
               Add(OrdersMenu, "command", "-label {Ask for events} -underline 8");
            end if;
            if not SkyBases(BaseIndex).AskedForBases then
               Add(OrdersMenu, "command", "-label {Ask for bases} -underline 8");
            end if;
            if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
                (To_Unbounded_String("temple")) then
               Add(OrdersMenu, "command", "-label {Pray}");
            end if;
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 then
                  Add(OrdersMenu, "command", "-label {Heal wounded} -underline 5");
                  exit;
               end if;
            end loop;
            for Module of PlayerShip.Modules loop
               if Module.Durability < Module.MaxDurability then
                  Add(OrdersMenu, "command", "-label {Repair ship} -underline 2");
                  exit;
               end if;
            end loop;
            if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
                (To_Unbounded_String("shipyard")) then
                Add(OrdersMenu, "command", "-label {Shipyard} -underline 2");
            end if;
            for I in Recipes_List.Iterate loop
               if Known_Recipes.Find_Index(Item => Recipes_Container.Key(I)) =
                 UnboundedString_Container.No_Index and
                 BasesTypes_List(SkyBases(BaseIndex).BaseType).Recipes.Contains
                   (Recipes_Container.Key(I)) and
                 Recipes_List(I).Reputation <=
                   SkyBases(BaseIndex).Reputation(1) then
                   Add(OrdersMenu, "command", "-label {Buy recipes} -underline 2");
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
                           Insert(OrdersMenu, "0", "command", "-label {Complete delivery of " &
                              To_String(Items_List(Mission.ItemIndex).Name) & "} -underline 0");
                        when Destroy =>
                           if Mission.Finished then
                                 Insert(OrdersMenu, "0", "command", "-label {Complete destroy " &
                                 To_String
                                   (ProtoShips_List(Mission.ShipIndex).Name) & "} -underline 0");
                           end if;
                        when Patrol =>
                           if Mission.Finished then
                                 Insert(OrdersMenu, "0", "command", "-label {Complete Patrol area mission} -underline 0");
                           end if;
                        when Explore =>
                           if Mission.Finished then
                                 Insert(OrdersMenu, "0", "command", "-label {Complete Explore area mission} -underline 0");
                           end if;
                        when Passenger =>
                           if Mission.Finished then
                                 Insert(OrdersMenu, "0", "command", "-label {Complete Transport passenger mission} -underline 0");
                           end if;
                     end case;
                  end if;
                  if Mission.StartBase = BaseIndex then
                     MissionsLimit := MissionsLimit - 1;
                  end if;
               end loop;
               if MissionsLimit > 0 then
                  Add(OrdersMenu, "command", "-label {Missions} -underline 0");
               end if;
            end if;
            if PlayerShip.HomeBase /= BaseIndex then
               Add(OrdersMenu, "command", "-label {Set as home} -underline 7");
            end if;
         end if;
         if SkyBases(BaseIndex).Population = 0 then
            Add(OrdersMenu, "command", "-label {Loot} -underline 0");
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
      if not ButtonsVisible then
         ShowDialog
           ("Here are no available ship orders at this moment. Ship orders available mostly when you are at base or at event on map.");
      end if;
      return TCL_OK;
   end Show_Orders_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowOrders", Show_Orders_Command'Access);
   end AddCommands;

end OrdersMenu;
