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
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
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
                           "} -underline 4");
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
                           "} -underline 0");
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
                           "-label {Search area} -underline 0");
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
            for Member of PlayerShip.Crew loop
               if Member.Health < 100 then
                  Add
                    (OrdersMenu, "command",
                     "-label {Heal wounded} -underline 5 -command {ShowBaseUI heal}");
                  exit;
               end if;
            end loop;
            for Module of PlayerShip.Modules loop
               if Module.Durability < Module.MaxDurability then
                  Add
                    (OrdersMenu, "command",
                     "-label {Repair ship} -underline 2 -command {ShowBaseUI repair}");
                  exit;
               end if;
            end loop;
            if BasesTypes_List(SkyBases(BaseIndex).BaseType).Flags.Contains
                (To_Unbounded_String("shipyard")) then
               Add
                 (OrdersMenu, "command",
                  "-label {Shipyard} -underline 2 -command ShowShipyard");
            end if;
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
                           Insert
                             (OrdersMenu, "0", "command",
                              "-label {Complete delivery of " &
                              To_String(Items_List(Mission.ItemIndex).Name) &
                              "} -underline 0");
                        when Destroy =>
                           if Mission.Finished then
                              Insert
                                (OrdersMenu, "0", "command",
                                 "-label {Complete destroy " &
                                 To_String
                                   (ProtoShips_List(Mission.ShipIndex).Name) &
                                 "} -underline 0");
                           end if;
                        when Patrol =>
                           if Mission.Finished then
                              Insert
                                (OrdersMenu, "0", "command",
                                 "-label {Complete Patrol area mission} -underline 0");
                           end if;
                        when Explore =>
                           if Mission.Finished then
                              Insert
                                (OrdersMenu, "0", "command",
                                 "-label {Complete Explore area mission} -underline 0");
                           end if;
                        when Passenger =>
                           if Mission.Finished then
                              Insert
                                (OrdersMenu, "0", "command",
                                 "-label {Complete Transport passenger mission} -underline 0");
                           end if;
                     end case;
                  end if;
                  if Mission.StartBase = BaseIndex then
                     MissionsLimit := MissionsLimit - 1;
                  end if;
               end loop;
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
                        "-label {Deliver medicines for free} -underline 0");
                     Add
                       (OrdersMenu, "command",
                        "-label {Deliver medicines for price} -underline 8");
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
                        Add
                          (OrdersMenu, "command",
                           "-label {Dock (" &
                           Trim(Positive'Image(DockingCost), Left) & " " &
                           To_String(MoneyName) &
                           ")} -underline 0 -command {Docking}");
                     end;
                  end if;
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
                                 "} -underline 0");
                           when Destroy =>
                              if Mission.Finished then
                                 Add
                                   (OrdersMenu, "command",
                                    "-label {Complete destroy " &
                                    To_String
                                      (ProtoShips_List(Mission.ShipIndex)
                                         .Name) &
                                    "} -underline 0");
                              end if;
                           when Patrol =>
                              if Mission.Finished then
                                 Add
                                   (OrdersMenu, "command",
                                    "-label {Complete Patrol area mission} -underline 0");
                              end if;
                           when Explore =>
                              if Mission.Finished then
                                 Add
                                   (OrdersMenu, "command",
                                    "-label {Complete Explore area mission} -underline 0");
                              end if;
                           when Passenger =>
                              if Mission.Finished then
                                 Add
                                   (OrdersMenu, "command",
                                    "-label {Complete Transport passenger mission} -underline 0");
                              end if;
                        end case;
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
                              Add
                                (OrdersMenu, "command",
                                 "-label {Search for " &
                                 To_String
                                   (ProtoShips_List(Mission.ShipIndex).Name) &
                                 "} -underline 0");
                           when Patrol =>
                              Add
                                (OrdersMenu, "command",
                                 "-label {Patrol area} -underline 0");
                           when Explore =>
                              Add
                                (OrdersMenu, "command",
                                 "-label {Explore area} -underline 0");
                        end case;
                     end if;
                  end loop;
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
                     "-label {Ask for events} -underline 8");
                  Add
                    (OrdersMenu, "command",
                     "-label {Ask for bases} -underline 8");
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
                       To_String(TradersName)) >
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
                        "-label {Ask for bases} -underline 8");
                  end if;
                  Add
                    (OrdersMenu, "command",
                     "-label {Ask for events} -underline 8");
               end if;
               Add
                 (OrdersMenu, "command",
                  "-label {Attack} -underline 0 -command Attack");
         end case;
      end if;
      Add(OrdersMenu, "command", "-label {Close} -underline 0");
      if Index(OrdersMenu, "0") = Index(OrdersMenu, "end") then
         ShowMessage
           ("Here are no available ship orders at this moment. Ship orders available mostly when you are at base or at event on map.");
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
            ShowMessage(To_String(Message));
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
            ShowMessage(To_String(Message));
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
      for I in PlayerShip.Crew.Iterate loop
         UpdateMorale(PlayerShip, Crew_Container.To_Index(I), 10);
      end loop;
      AddMessage
        ("You and your crew were praying for some time. Now you all feel a bit better.",
         OrderMessage);
      UpdateGame(30);
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
      Price: Positive := 1000;
      MoneyIndex2: constant Natural := FindItem(PlayerShip.Cargo, MoneyIndex);
   begin
      CountPrice(Price, TraderIndex);
      if MessageBox
          ("-message {Are you sure want to change your home base (it cost" &
           Positive'Image(Price) & " " & To_String(MoneyName) &
           ")?} -icon question -type yesno") /=
        "yes" then
         return TCL_OK;
      end if;
      if MoneyIndex2 = 0 then
         ShowMessage
           ("You don't have any " & To_String(MoneyName) &
            " for change ship home base.");
         return TCL_OK;
      end if;
      CountPrice(Price, TraderIndex);
      if PlayerShip.Cargo(MoneyIndex2).Amount < Price then
         ShowMessage
           ("You don't have enough " & To_String(MoneyName) &
            " for change ship home base.");
         return TCL_OK;
      end if;
      PlayerShip.HomeBase :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      UpdateCargo
        (Ship => PlayerShip, CargoIndex => MoneyIndex2, Amount => (0 - Price));
      AddMessage
        ("You changed your ship home base to: " &
         To_String(SkyBases(PlayerShip.HomeBase).Name),
         OtherMessage);
      GainExp(1, TalkingSkill, TraderIndex);
      UpdateGame(10);
      ShowSkyMap;
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
   end AddCommands;

end OrdersMenu;
