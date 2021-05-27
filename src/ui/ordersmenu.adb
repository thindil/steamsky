-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Combat; use Combat;
with Combat.UI; use Combat.UI;
with Crafts; use Crafts;
with Crew; use Crew;
with Dialogs; use Dialogs;
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
with Trades; use Trades;
with Utils; use Utils;
with Utils.UI; use Utils.UI;
with WaitMenu; use WaitMenu;

package body OrdersMenu is

   function Show_Orders_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      HaveTrader: Boolean := False;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MissionsLimit: Integer;
      Event: Events_Types := None;
      ItemIndex: Natural;
      OrdersMenu: constant Tk_Menu := Get_Widget(".orders", Interp);
   begin
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
            Step: constant Step_Data :=
              (if CurrentStory.CurrentStep = 0 then
                 Stories_List(CurrentStory.Index).StartingStep
               elsif CurrentStory.CurrentStep > 0 then
                 Stories_List(CurrentStory.Index).Steps
                   (CurrentStory.CurrentStep)
               else Stories_List(CurrentStory.Index).FinalStep);
         begin
            case Step.FinishCondition is
               when ASKINBASE =>
                  if BaseIndex > 0 then
                     if CurrentStory.Data = Null_Unbounded_String or
                       CurrentStory.Data = SkyBases(BaseIndex).Name then
                        Add
                          (OrdersMenu, "command",
                           "-label {Ask for " &
                           To_String
                             (Items_List(GetStepData(Step.FinishData, "item"))
                                .Name) &
                           "} -underline 4 -command ExecuteStory");
                     end if;
                  end if;
               when DESTROYSHIP =>
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create(Tokens, To_String(CurrentStory.Data), ";");
                     if PlayerShip.SkyX = Positive'Value(Slice(Tokens, 1)) and
                       PlayerShip.SkyY = Positive'Value(Slice(Tokens, 2)) then
                        Add
                          (OrdersMenu, "command",
                           "-label {Search for " &
                           To_String
                             (ProtoShips_List
                                (To_Unbounded_String(Slice(Tokens, 3)))
                                .Name) &
                           "} -underline 0 -command ExecuteStory");
                     end if;
                  end;
               when EXPLORE =>
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create(Tokens, To_String(CurrentStory.Data), ";");
                     if PlayerShip.SkyX = Positive'Value(Slice(Tokens, 1)) and
                       PlayerShip.SkyY = Positive'Value(Slice(Tokens, 2)) then
                        Add
                          (OrdersMenu, "command",
                           "-label {Search area} -underline 0 -command ExecuteStory");
                     end if;
                  end;
               when ANY | LOOT =>
                  null;
            end case;
         end;
      end if;
      if PlayerShip.Speed = DOCKED then
         Add
           (OrdersMenu, "command",
            "-label {Undock} -underline 0 -command {Docking}");
         if SkyBases(BaseIndex).Population > 0 then
            Add
              (OrdersMenu, "command",
               "-label {Escape} -underline 3 -command {Docking escape}");
         end if;
         if HaveTrader and SkyBases(BaseIndex).Population > 0 then
            Add
              (OrdersMenu, "command",
               "-label {Trade} -underline 0 -command ShowTrade");
            Add
              (OrdersMenu, "command",
               "-label {School} -underline 0 -command ShowSchool");
            if SkyBases(BaseIndex).Recruits.Length > 0 then
               Add
                 (OrdersMenu, "command",
                  "-label {Recruit} -underline 0 -command ShowRecruit");
            end if;
            if DaysDifference(SkyBases(BaseIndex).AskedForEvents) > 6 then
               Add
                 (OrdersMenu, "command",
                  "-label {Ask for events} -underline 8 -command AskForEvents");
            end if;
            if not SkyBases(BaseIndex).AskedForBases then
               Add
                 (OrdersMenu, "command",
                  "-label {Ask for bases} -underline 8 -command AskForBases");
            end if;
            if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
                (To_Unbounded_String("temple")) then
               Add(OrdersMenu, "command", "-label {Pray} -command Pray");
            end if;
            Add_Heal_Wounded_Menu_Loop :
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 then
                  Add
                    (OrdersMenu, "command",
                     "-label {Heal wounded} -underline 5 -command {ShowBaseUI heal}");
                  exit Add_Heal_Wounded_Menu_Loop;
               end if;
            end loop Add_Heal_Wounded_Menu_Loop;
            Add_Repair_Ship_Menu_Loop :
            for Module of PlayerShip.Modules loop
               if Module.Durability < Module.MaxDurability then
                  Add
                    (OrdersMenu, "command",
                     "-label {Repair ship} -underline 2 -command {ShowBaseUI repair}");
                  exit Add_Repair_Ship_Menu_Loop;
               end if;
            end loop Add_Repair_Ship_Menu_Loop;
            if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
                (To_Unbounded_String("shipyard")) then
               Add
                 (OrdersMenu, "command",
                  "-label {Shipyard} -underline 2 -command ShowShipyard");
            end if;
            Add_Buy_Recipes_Menu_Loop :
            for I in Recipes_List.Iterate loop
               if Known_Recipes.Find_Index(Item => Recipes_Container.Key(I)) =
                 UnboundedString_Container.No_Index and
                 BasesTypes_List(SkyBases(BaseIndex).BaseType).Recipes.Contains
                   (Recipes_Container.Key(I)) and
                 Recipes_List(I).Reputation <=
                   SkyBases(BaseIndex).Reputation(1) then
                  Add
                    (OrdersMenu, "command",
                     "-label {Buy recipes} -underline 2 -command {ShowBaseUI recipes}");
                  exit Add_Buy_Recipes_Menu_Loop;
               end if;
            end loop Add_Buy_Recipes_Menu_Loop;
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
               Add_Mission_Menu_Loop :
               for Mission of AcceptedMissions loop
                  if (Mission.Finished and Mission.StartBase = BaseIndex) or
                    (Mission.TargetX = PlayerShip.SkyX and
                     Mission.TargetY = PlayerShip.SkyY) then
                     case Mission.MType is
                        when Deliver =>
                           Insert
                             (OrdersMenu, "0", "command",
                              "-label {Complete delivery of " &
                              To_String(Items_List(Mission.ItemIndex).Name) &
                              "} -underline 0 -command CompleteMission");
                        when Destroy =>
                           if Mission.Finished then
                              Insert
                                (OrdersMenu, "0", "command",
                                 "-label {Complete destroy " &
                                 To_String
                                   (ProtoShips_List(Mission.ShipIndex).Name) &
                                 "} -underline 0 -command CompleteMission");
                           end if;
                        when Patrol =>
                           if Mission.Finished then
                              Insert
                                (OrdersMenu, "0", "command",
                                 "-label {Complete Patrol area mission} -underline 0 -command CompleteMission");
                           end if;
                        when Explore =>
                           if Mission.Finished then
                              Insert
                                (OrdersMenu, "0", "command",
                                 "-label {Complete Explore area mission} -underline 0 -command CompleteMission");
                           end if;
                        when Passenger =>
                           if Mission.Finished then
                              Insert
                                (OrdersMenu, "0", "command",
                                 "-label {Complete Transport passenger mission} -underline 0 -command CompleteMission");
                           end if;
                     end case;
                  end if;
                  if Mission.StartBase = BaseIndex then
                     MissionsLimit := MissionsLimit - 1;
                  end if;
               end loop Add_Mission_Menu_Loop;
               if MissionsLimit > 0 then
                  Add
                    (OrdersMenu, "command",
                     "-label Missions -underline 0 -command ShowBaseMissions");
               end if;
            end if;
            if PlayerShip.HomeBase /= BaseIndex then
               Add
                 (OrdersMenu, "command",
                  "-label {Set as home} -underline 7 -command SetAsHome");
            end if;
         end if;
         if SkyBases(BaseIndex).Population = 0 then
            Add
              (OrdersMenu, "command",
               "-label {Loot} -underline 0 -command ShowLoot");
         end if;
      else
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
            Event :=
              Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                .EType;
         end if;
         case Event is
            when EnemyShip | EnemyPatrol =>
               Add
                 (OrdersMenu, "command",
                  "-label {Attack} -underline 0 -command Attack");
            when FullDocks =>
               Add
                 (OrdersMenu, "command",
                  "-label {Wait (full docks)} -underline 0 -command ShowWait");
            when AttackOnBase =>
               Add
                 (OrdersMenu, "command",
                  "-label {Defend} -underline 0 -command Attack");
            when Disease =>
               if HaveTrader then
                  ItemIndex :=
                    FindItem
                      (Inventory => PlayerShip.Cargo,
                       ItemType =>
                         Factions_List(SkyBases(BaseIndex).Owner)
                           .HealingTools);
                  if ItemIndex > 0 then
                     Add
                       (OrdersMenu, "command",
                        "-label {Deliver medicines for free} -underline 0 -command {DeliverMedicines free}");
                     Add
                       (OrdersMenu, "command",
                        "-label {Deliver medicines for price} -underline 8 -command {DeliverMedicines paid}");
                  end if;
               end if;
            when None | DoublePrice | BaseRecovery =>
               if BaseIndex > 0 then
                  if SkyBases(BaseIndex).Reputation(1) > -25 then
                     declare
                        DockingCost: Positive;
                     begin
                        Count_Docking_Cost_Loop :
                        for Module of PlayerShip.Modules loop
                           if Module.MType = HULL then
                              DockingCost := Module.MaxModules;
                              exit Count_Docking_Cost_Loop;
                           end if;
                        end loop Count_Docking_Cost_Loop;
                        if SkyBases(BaseIndex).Population > 0 then
                           Add
                             (OrdersMenu, "command",
                              "-label {Dock (" &
                              Trim(Positive'Image(DockingCost), Left) & " " &
                              To_String(Money_Name) &
                              ")} -underline 0 -command {Docking}");
                        else
                           Add
                             (OrdersMenu, "command",
                              "-label {Dock} -underline 0 -command {Docking}");
                        end if;
                     end;
                  end if;
                  Complete_Mission_Menu_Loop :
                  for Mission of AcceptedMissions loop
                     if HaveTrader and Mission.TargetX = PlayerShip.SkyX and
                       Mission.TargetY = PlayerShip.SkyY and
                       Mission.Finished then
                        case Mission.MType is
                           when Deliver =>
                              Add
                                (OrdersMenu, "command",
                                 "-label {Complete delivery of " &
                                 To_String
                                   (Items_List(Mission.ItemIndex).Name) &
                                 "} -underline 0 -command CompleteMission");
                           when Destroy =>
                              if Mission.Finished then
                                 Add
                                   (OrdersMenu, "command",
                                    "-label {Complete destroy " &
                                    To_String
                                      (ProtoShips_List(Mission.ShipIndex)
                                         .Name) &
                                    "} -underline 0 -command CompleteMission");
                              end if;
                           when Patrol =>
                              if Mission.Finished then
                                 Add
                                   (OrdersMenu, "command",
                                    "-label {Complete Patrol area mission} -underline 0 -command CompleteMission");
                              end if;
                           when Explore =>
                              if Mission.Finished then
                                 Add
                                   (OrdersMenu, "command",
                                    "-label {Complete Explore area mission} -underline 0 -command CompleteMission");
                              end if;
                           when Passenger =>
                              if Mission.Finished then
                                 Add
                                   (OrdersMenu, "command",
                                    "-label {Complete Transport passenger mission} -underline 0 -command CompleteMission");
                              end if;
                        end case;
                     end if;
                  end loop Complete_Mission_Menu_Loop;
               else
                  Progress_Mission_Loop :
                  for Mission of AcceptedMissions loop
                     if Mission.TargetX = PlayerShip.SkyX and
                       Mission.TargetY = PlayerShip.SkyY and
                       not Mission.Finished then
                        case Mission.MType is
                           when Deliver | Passenger =>
                              null;
                           when Destroy =>
                              Add
                                (OrdersMenu, "command",
                                 "-label {Search for " &
                                 To_String
                                   (ProtoShips_List(Mission.ShipIndex).Name) &
                                 "} -underline 0 -command StartMission");
                           when Patrol =>
                              Add
                                (OrdersMenu, "command",
                                 "-label {Patrol area} -underline 0 -command StartMission");
                           when Explore =>
                              Add
                                (OrdersMenu, "command",
                                 "-label {Explore area} -underline 0 -command StartMission");
                        end case;
                     end if;
                  end loop Progress_Mission_Loop;
               end if;
            when Trader =>
               if HaveTrader then
                  Add
                    (OrdersMenu, "command",
                     "-label {Trade} -underline 0 -command {ShowTrader " &
                     To_String
                       (Events_List
                          (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                          .ShipIndex) &
                     "}");
                  Add
                    (OrdersMenu, "command",
                     "-label {Ask for events} -underline 8 -command AskForEvents");
                  Add
                    (OrdersMenu, "command",
                     "-label {Ask for bases} -underline 8 -command AskForBases");
               end if;
               Add
                 (OrdersMenu, "command",
                  "-label {Attack} -underline 0 -command Attack");
            when FriendlyShip =>
               if HaveTrader then
                  if Index
                      (ProtoShips_List
                         (Events_List
                            (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY)
                               .EventIndex)
                            .ShipIndex)
                         .Name,
                       To_String(Traders_Name)) >
                    0 then
                     Add
                       (OrdersMenu, "command",
                        "-label {Trade} -underline 0 -command {ShowTrader " &
                        To_String
                          (Events_List
                             (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY)
                                .EventIndex)
                             .ShipIndex) &
                        "}");
                     Add
                       (OrdersMenu, "command",
                        "-label {Ask for bases} -underline 8 -command AskForBases");
                  end if;
                  Add
                    (OrdersMenu, "command",
                     "-label {Ask for events} -underline 8 -command AskForEvents");
               end if;
               Add
                 (OrdersMenu, "command",
                  "-label {Attack} -underline 0 -command Attack");
         end case;
      end if;
      Add(OrdersMenu, "command", "-label {Close} -underline 0");
      if Index(OrdersMenu, "0") = Index(OrdersMenu, "end") then
         ShowMessage
           (Text =>
              "Here are no available ship orders at this moment. Ship orders available mostly when you are at base or at event on map.",
            Title => "No orders available");
      else
         Tk_Popup
           (OrdersMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
            Winfo_Get(Get_Main_Window(Interp), "pointery"));
      end if;
      return TCL_OK;
   end Show_Orders_Command;

   -- ****o* OrdersMenu/OrdersMenu.Docking_Command
   -- FUNCTION
   -- Dock or undock from the sky base
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Docking ?escape?
   -- If argument escape is present, escape from the base without paying,
   -- otherwise normal docking or undocking operation
   -- SOURCE
   function Docking_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Docking_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Message: Unbounded_String;
   begin
      if PlayerShip.Speed = DOCKED then
         Message :=
           (if Argc = 1 then To_Unbounded_String(DockShip(False))
            else To_Unbounded_String(DockShip(False, True)));
         if Length(Message) > 0 then
            ShowMessage
              (Text => To_String(Message), Title => "Can't undock from base");
            return TCL_OK;
         end if;
      else
         if SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
            if Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
                .EType =
              FullDocks then
               return Show_Wait_Command(ClientData, Interp, Argc, Argv);
            end if;
         end if;
         Message := To_Unbounded_String(DockShip(True));
         if Length(Message) > 0 then
            ShowMessage
              (Text => To_String(Message), Title => "Can't dock to base");
            return TCL_OK;
         end if;
      end if;
      ShowSkyMap;
      if PlayerShip.Speed = DOCKED then
         return Show_Orders_Command(ClientData, Interp, Argc, Argv);
      end if;
      return TCL_OK;
   end Docking_Command;

   -- ****o* OrdersMenu/OrdersMenu.Ask_For_Bases_Command
   -- FUNCTION
   -- Ask for bases in the currently visited base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- AskForBases
   -- SOURCE
   function Ask_For_Bases_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Ask_For_Bases_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      AskForBases;
      ShowSkyMap;
      return TCL_OK;
   end Ask_For_Bases_Command;

   -- ****o* OrdersMenu/OrdersMenu.Ask_For_Events_Command
   -- FUNCTION
   -- Ask for events in the currently visited base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- AskForEvents
   -- SOURCE
   function Ask_For_Events_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Ask_For_Events_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      AskForEvents;
      ShowSkyMap;
      return TCL_OK;
   end Ask_For_Events_Command;

   -- ****o* OrdersMenu/OrdersMenu.Attack_Command
   -- FUNCTION
   -- Start the combat
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Attack
   -- SOURCE
   function Attack_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Attack_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      ShowCombatUI;
      return TCL_OK;
   end Attack_Command;

   -- ****f* OrdersMenu/OrdersMenu.Pray_Command
   -- FUNCTION
   -- Pray in the selected base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Pray
   -- SOURCE
   function Pray_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Pray_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      Update_Morale_Loop :
      for I in PlayerShip.Crew.Iterate loop
         UpdateMorale(PlayerShip, Crew_Container.To_Index(I), 10);
      end loop Update_Morale_Loop;
      AddMessage
        ("You and your crew were praying for some time. Now you all feel a bit better.",
         OrderMessage);
      Update_Game(30);
      ShowSkyMap;
      return TCL_OK;
   end Pray_Command;

   -- ****f* OrdersMenu/OrdersMenu.Set_As_Home_Command
   -- FUNCTION
   -- Set the selected base as a home base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetAsHome
   -- SOURCE
   function Set_As_Home_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_As_Home_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      TraderIndex: constant Natural := FindMember(Talk);
      Price: Positive := 1_000;
   begin
      CountPrice(Price, TraderIndex);
      ShowQuestion
        ("Are you sure want to change your home base (it cost" &
         Positive'Image(Price) & " " & To_String(Money_Name) & ")?",
         "sethomebase");
      return TCL_OK;
   end Set_As_Home_Command;

   -- ****f* OrdersMenu/OrdersMenu.Show_Trader_Command
   -- FUNCTION
   -- Generate cargo for trader and show trading UI
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowTrader protoindex
   -- Protoindex is the index of ship prototype on which trader cargo will be
   -- generated
   -- SOURCE
   function Show_Trader_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Trader_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
   begin
      GenerateTraderCargo(To_Unbounded_String(CArgv.Arg(Argv, 1)));
      Tcl_Eval(Interp, "ShowTrade");
      return TCL_OK;
   end Show_Trader_Command;

   -- ****f* OrdersMenu/OrdersMenu.Start_Mission_Command
   -- FUNCTION
   -- Start the selected mission
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- StartMission
   -- SOURCE
   function Start_Mission_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Start_Mission_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      StartsCombat: Boolean := False;
   begin
      for Mission of AcceptedMissions loop
         if Mission.TargetX = PlayerShip.SkyX and
           Mission.TargetY = PlayerShip.SkyY and not Mission.Finished then
            case Mission.MType is
               when Deliver | Passenger =>
                  null;
               when Destroy =>
                  Update_Game(GetRandom(15, 45));
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
                  Update_Game(GetRandom(45, 75));
                  StartsCombat := CheckForEvent;
                  if not StartsCombat then
                     UpdateMission
                       (SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).MissionIndex);
                  end if;
               when Explore =>
                  Update_Game(GetRandom(30, 60));
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
         return TCL_OK;
      end if;
      UpdateHeader;
      UpdateMessages;
      ShowSkyMap;
      return TCL_OK;
   end Start_Mission_Command;

   -- ****f* OrdersMenu/OrdersMenu.Complete_Mission_Command
   -- FUNCTION
   -- Complete the selected mission in base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CompleteMission
   -- SOURCE
   function Complete_Mission_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Complete_Mission_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      FinishMission(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).MissionIndex);
      UpdateHeader;
      UpdateMessages;
      ShowSkyMap;
      return TCL_OK;
   end Complete_Mission_Command;

   -- ****f* OrdersMenu/OrdersMenu.Execute_Story_Command
   -- FUNCTION
   -- Execute the current step in the current story
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ExecuteStory
   -- SOURCE
   function Execute_Story_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Execute_Story_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      Step: Step_Data;
      Message: Unbounded_String;
   begin
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
            ShowInfo
              (Text => To_String(Message), Title => "Can't dock to base");
            return TCL_OK;
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
                     return TCL_OK;
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
                     ShowInfo(Text => To_String(Text.Text), Title => "Story");
                     CurrentStory.ShowText := False;
                     exit;
                  end if;
               end loop;
            else
               FinishStory;
            end if;
         end;
      else
         ShowInfo(Text => To_String(Step.FailText), Title => "Story");
         CurrentStory.ShowText := False;
      end if;
      UpdateHeader;
      UpdateMessages;
      ShowSkyMap;
      return TCL_OK;
   end Execute_Story_Command;

   -- ****f* OrdersMenu/OrdersMenu.Deliver_Medicines_Command
   -- FUNCTION
   -- Deliver medicines to the base
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DeliverMedicines type
   -- If argument type is free, deliver medicines for free, otherwise deliver
   -- medicines for a price
   -- SOURCE
   function Deliver_Medicines_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Deliver_Medicines_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
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
      if CArgv.Arg(Argv, 1) = "free" then
         GainRep(BaseIndex, (PlayerShip.Cargo(ItemIndex).Amount / 10));
         AddMessage
           ("You gave " &
            To_String
              (Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).Name) &
            " for free to base.",
            TradeMessage);
         UpdateCargo
           (PlayerShip, PlayerShip.Cargo.Element(ItemIndex).ProtoIndex,
            (0 - PlayerShip.Cargo.Element(ItemIndex).Amount));
      else
         begin
            GainRep
              (BaseIndex, ((PlayerShip.Cargo(ItemIndex).Amount / 20) * (-1)));
            SellItems
              (ItemIndex,
               Integer'Image(PlayerShip.Cargo.Element(ItemIndex).Amount));
         exception
            when Trade_No_Free_Cargo =>
               ShowMessage
                 (Text =>
                    "You can't sell medicines to the base because you don't have enough free cargo space for money.",
                  Title => "No free cargo space");
            when Trade_No_Money_In_Base =>
               ShowMessage
                 (Text =>
                    "You can't sell medicines to the base because the base don't have enough money to buy them.",
                  Title => "Can't sell medicines");
         end;
      end if;
      UpdateHeader;
      UpdateMessages;
      ShowSkyMap;
      return TCL_OK;
   end Deliver_Medicines_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowOrders", Show_Orders_Command'Access);
      AddCommand("Docking", Docking_Command'Access);
      AddCommand("AskForBases", Ask_For_Bases_Command'Access);
      AddCommand("AskForEvents", Ask_For_Events_Command'Access);
      AddCommand("Attack", Attack_Command'Access);
      AddCommand("Pray", Pray_Command'Access);
      AddCommand("SetAsHome", Set_As_Home_Command'Access);
      AddCommand("ShowTrader", Show_Trader_Command'Access);
      AddCommand("StartMission", Start_Mission_Command'Access);
      AddCommand("CompleteMission", Complete_Mission_Command'Access);
      AddCommand("ExecuteStory", Execute_Story_Command'Access);
      AddCommand("DeliverMedicines", Deliver_Medicines_Command'Access);
   end AddCommands;

end OrdersMenu;
