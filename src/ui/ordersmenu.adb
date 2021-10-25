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
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
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
      HaveTrader: Boolean := False;
      BaseIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      MissionsLimit: Integer;
      Event: Events_Types := None;
      ItemIndex: Natural;
      OrdersMenu: constant Ttk_Frame :=
        Create_Dialog(".gameframe.orders", "Ship orders");
      CloseButton: constant Ttk_Button :=
        Create
          (OrdersMenu & ".closebutton",
           "-text Close -command {CloseDialog " & OrdersMenu & "}");
      procedure Add_Button
        (Name, Label, Command: String; UnderLine: Natural;
         Row: Integer := -1) is
         Button: constant Ttk_Button :=
           Create
             (OrdersMenu & Name,
              "-text {" & Label & "} -command {" & Command & "} -underline" &
              Natural'Image(UnderLine) &
              (if Row = -1 then "" else " -row" & Integer'Image(Row)));
      begin
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -padx 5");
         Bind(Button, "<Escape>", "{" & CloseButton & " invoke;break}");
      end Add_Button;
   begin
      if Winfo_Get(OrdersMenu, "ismapped") = "1" then
         return Close_Dialog_Command(ClientData, Interp, Argc, Argv);
      end if;
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
                       CurrentStory.Data = Sky_Bases(BaseIndex).Name then
                        Add_Button
                          (".story",
                           "Ask for " &
                           To_String
                             (Items_List(GetStepData(Step.FinishData, "item"))
                                .Name),
                           "ExecuteStory", 4);
                     end if;
                  end if;
               when DESTROYSHIP =>
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create(Tokens, To_String(CurrentStory.Data), ";");
                     if Player_Ship.Sky_X =
                       Positive'Value(Slice(Tokens, 1)) and
                       Player_Ship.Sky_Y =
                         Positive'Value(Slice(Tokens, 2)) then
                        Add_Button
                          (".story",
                           "Search for " &
                           To_String
                             (Proto_Ships_List
                                (To_Unbounded_String(Slice(Tokens, 3)))
                                .Name),
                           "ExecuteStory", 0);
                     end if;
                  end;
               when EXPLORE =>
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create(Tokens, To_String(CurrentStory.Data), ";");
                     if Player_Ship.Sky_X =
                       Positive'Value(Slice(Tokens, 1)) and
                       Player_Ship.Sky_Y =
                         Positive'Value(Slice(Tokens, 2)) then
                        Add_Button(".story", "Search area", "ExecuteStory", 0);
                     end if;
                  end;
               when ANY | LOOT =>
                  null;
            end case;
         end;
      end if;
      if Player_Ship.Speed = DOCKED then
         Add_Button(".undock", "Undock", "Docking", 0);
         if Sky_Bases(BaseIndex).Population > 0 then
            Add_Button(".escape", "Escape", "Docking escape", 3);
         end if;
         if HaveTrader and Sky_Bases(BaseIndex).Population > 0 then
            Add_Button(".trade", "Trade", "ShowTrade", 0);
            Add_Button(".school", "School", "ShowSchool", 0);
            if Sky_Bases(BaseIndex).Recruits.Length > 0 then
               Add_Button(".recruits", "Recruit", "ShowRecruit", 0);
            end if;
            if Days_Difference(Sky_Bases(BaseIndex).Asked_For_Events) > 6 then
               Add_Button(".events", "Ask for events", "AskForEvents", 8);
            end if;
            if not Sky_Bases(BaseIndex).Asked_For_Bases then
               Add_Button(".bases", "Ask for bases", "AskForBases", 8);
            end if;
            if Bases_Types_List(Sky_Bases(BaseIndex).Base_Type).Flags.Contains
                (To_Unbounded_String("temple")) then
               Add_Button(".pray", "Pray", "Pray", 0);
            end if;
            Add_Heal_Wounded_Menu_Loop :
            for Member of Player_Ship.Crew loop
               if Member.Health < 100 then
                  Add_Button(".heal", "Heal wounded", "ShowBaseUI heal", 5);
                  exit Add_Heal_Wounded_Menu_Loop;
               end if;
            end loop Add_Heal_Wounded_Menu_Loop;
            Add_Repair_Ship_Menu_Loop :
            for Module of Player_Ship.Modules loop
               if Module.Durability < Module.Max_Durability then
                  Add_Button(".repair", "Repair ship", "ShowBaseUI repair", 2);
                  exit Add_Repair_Ship_Menu_Loop;
               end if;
            end loop Add_Repair_Ship_Menu_Loop;
            if Bases_Types_List(Sky_Bases(BaseIndex).Base_Type).Flags.Contains
                (To_Unbounded_String("shipyard")) then
               Add_Button(".shipyard", "Shipyard", "ShowShipyard", 2);
            end if;
            Add_Buy_Recipes_Menu_Loop :
            for I in Recipes_List.Iterate loop
               if Known_Recipes.Find_Index(Item => Recipes_Container.Key(I)) =
                 UnboundedString_Container.No_Index and
                 Bases_Types_List(Sky_Bases(BaseIndex).Base_Type).Recipes
                   .Contains
                   (Recipes_Container.Key(I)) and
                 Recipes_List(I).Reputation <=
                   Sky_Bases(BaseIndex).Reputation(1) then
                  Add_Button
                    (".recipes", "Buy recipes", "ShowBaseUI recipes", 2);
                  exit Add_Buy_Recipes_Menu_Loop;
               end if;
            end loop Add_Buy_Recipes_Menu_Loop;
            if Sky_Bases(BaseIndex).Missions.Length > 0 then
               MissionsLimit :=
                 (case Sky_Bases(BaseIndex).Reputation(1) is when 0 .. 25 => 1,
                    when 26 .. 50 => 3, when 51 .. 75 => 5,
                    when 76 .. 100 => 10, when others => 0);
               Add_Mission_Menu_Loop :
               for Mission of AcceptedMissions loop
                  if (Mission.Finished and Mission.StartBase = BaseIndex) or
                    (Mission.TargetX = Player_Ship.Sky_X and
                     Mission.TargetY = Player_Ship.Sky_Y) then
                     case Mission.MType is
                        when Deliver =>
                           Add_Button
                             (".mission",
                              "Complete delivery of " &
                              To_String(Items_List(Mission.ItemIndex).Name),
                              "CompleteMission", 0, 0);
                        when Destroy =>
                           if Mission.Finished then
                              Add_Button
                                (".mission",
                                 "Complete destroy " &
                                 To_String
                                   (Proto_Ships_List(Mission.ShipIndex).Name),
                                 "CompleteMission", 0, 0);
                           end if;
                        when Patrol =>
                           if Mission.Finished then
                              Add_Button
                                (".mission", "Complete Patrol area mission",
                                 "CompleteMission", 0, 0);
                           end if;
                        when Explore =>
                           if Mission.Finished then
                              Add_Button
                                (".mission", "Complete Explore area mission",
                                 "CompleteMission", 0, 0);
                           end if;
                        when Passenger =>
                           if Mission.Finished then
                              Add_Button
                                (".mission",
                                 "Complete Transport passenger mission",
                                 "CompleteMission", 0, 0);
                           end if;
                     end case;
                  end if;
                  if Mission.StartBase = BaseIndex then
                     MissionsLimit := MissionsLimit - 1;
                  end if;
               end loop Add_Mission_Menu_Loop;
               if MissionsLimit > 0 then
                  Add_Button(".missions", "Missions", "ShowBaseMissions", 0);
               end if;
            end if;
            if Player_Ship.Home_Base /= BaseIndex then
               Add_Button(".home", "Set as home", "SetAsHome", 7);
            end if;
         end if;
         if Sky_Bases(BaseIndex).Population = 0 then
            Add_Button(".loot", "Loot", "ShowLoot", 0);
         end if;
      else
         if SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex > 0 then
            Event :=
              Events_List
                (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex)
                .EType;
         end if;
         case Event is
            when EnemyShip | EnemyPatrol =>
               Add_Button(".event", "Attack", "Attack", 0);
            when FullDocks =>
               Add_Button(".event", "Wait (full docks)", "ShowWait", 0);
            when AttackOnBase =>
               Add_Button(".event", "Defend", "Attack", 0);
            when Disease =>
               if HaveTrader then
                  ItemIndex :=
                    FindItem
                      (Inventory => Player_Ship.Cargo,
                       ItemType =>
                         Factions_List(Sky_Bases(BaseIndex).Owner)
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
                  if Sky_Bases(BaseIndex).Reputation(1) > -25 then
                     declare
                        DockingCost: Positive;
                     begin
                        Count_Docking_Cost_Loop :
                        for Module of Player_Ship.Modules loop
                           if Module.M_Type = HULL then
                              DockingCost := Module.Max_Modules;
                              exit Count_Docking_Cost_Loop;
                           end if;
                        end loop Count_Docking_Cost_Loop;
                        if Sky_Bases(BaseIndex).Population > 0 then
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
                     if HaveTrader and Mission.TargetX = Player_Ship.Sky_X and
                       Mission.TargetY = Player_Ship.Sky_Y and
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
                                      (Proto_Ships_List(Mission.ShipIndex)
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
                     if Mission.TargetX = Player_Ship.Sky_X and
                       Mission.TargetY = Player_Ship.Sky_Y and
                       not Mission.Finished then
                        case Mission.MType is
                           when Deliver | Passenger =>
                              null;
                           when Destroy =>
                              Add
                                (OrdersMenu, "command",
                                 "-label {Search for " &
                                 To_String
                                   (Proto_Ships_List(Mission.ShipIndex).Name) &
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
                          (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                             .EventIndex)
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
                      (Proto_Ships_List
                         (Events_List
                            (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y)
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
                             (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y)
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
      if Player_Ship.Speed = DOCKED then
         Message :=
           (if Argc = 1 then To_Unbounded_String(DockShip(False))
            else To_Unbounded_String(DockShip(False, True)));
         if Length(Message) > 0 then
            ShowMessage
              (Text => To_String(Message), Title => "Can't undock from base");
            return TCL_OK;
         end if;
      else
         if SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex > 0 then
            if Events_List
                (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex)
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
      if Player_Ship.Speed = DOCKED then
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
      Ask_For_Bases;
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
      Ask_For_Events;
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
      for I in Player_Ship.Crew.Iterate loop
         UpdateMorale(Player_Ship, Crew_Container.To_Index(I), 10);
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
      Count_Price(Price, TraderIndex);
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
         if Mission.TargetX = Player_Ship.Sky_X and
           Mission.TargetY = Player_Ship.Sky_Y and not Mission.Finished then
            case Mission.MType is
               when Deliver | Passenger =>
                  null;
               when Destroy =>
                  Update_Game(Get_Random(15, 45));
                  StartsCombat := CheckForEvent;
                  if not StartsCombat then
                     StartsCombat :=
                       StartCombat
                         (AcceptedMissions
                            (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                               .MissionIndex)
                            .ShipIndex,
                          False);
                  end if;
               when Patrol =>
                  Update_Game(Get_Random(45, 75));
                  StartsCombat := CheckForEvent;
                  if not StartsCombat then
                     UpdateMission
                       (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                          .MissionIndex);
                  end if;
               when Explore =>
                  Update_Game(Get_Random(30, 60));
                  StartsCombat := CheckForEvent;
                  if not StartsCombat then
                     UpdateMission
                       (SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                          .MissionIndex);
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
      Update_Messages;
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
      FinishMission(SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).MissionIndex);
      UpdateHeader;
      Update_Messages;
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
      Step: Step_Data :=
        (if CurrentStory.CurrentStep = 0 then
           Stories_List(CurrentStory.Index).StartingStep
         elsif CurrentStory.CurrentStep > 0 then
           Stories_List(CurrentStory.Index).Steps(CurrentStory.CurrentStep)
         else Stories_List(CurrentStory.Index).FinalStep);
      Message: Unbounded_String;
   begin
      if Player_Ship.Speed /= DOCKED and Step.FinishCondition = ASKINBASE then
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
               Step :=
                 (if CurrentStory.CurrentStep > 0 then
                    Stories_List(CurrentStory.Index).Steps
                      (CurrentStory.CurrentStep)
                  else Stories_List(CurrentStory.Index).FinalStep);
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
      Update_Messages;
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
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      EventIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex;
      ItemIndex: constant Natural :=
        FindItem
          (Inventory => Player_Ship.Cargo,
           ItemType => Factions_List(Sky_Bases(BaseIndex).Owner).HealingTools);
      NewTime: constant Integer :=
        Events_List(EventIndex).Time - Player_Ship.Cargo(ItemIndex).Amount;
   begin
      if NewTime < 1 then
         DeleteEvent(EventIndex);
      else
         Events_List(EventIndex).Time := NewTime;
      end if;
      if CArgv.Arg(Argv, 1) = "free" then
         Gain_Rep(BaseIndex, (Player_Ship.Cargo(ItemIndex).Amount / 10));
         AddMessage
           ("You gave " &
            To_String
              (Items_List(Player_Ship.Cargo(ItemIndex).ProtoIndex).Name) &
            " for free to base.",
            TradeMessage);
         UpdateCargo
           (Player_Ship, Player_Ship.Cargo.Element(ItemIndex).ProtoIndex,
            (0 - Player_Ship.Cargo.Element(ItemIndex).Amount));
      else
         begin
            Gain_Rep
              (BaseIndex, ((Player_Ship.Cargo(ItemIndex).Amount / 20) * (-1)));
            SellItems
              (ItemIndex,
               Integer'Image(Player_Ship.Cargo.Element(ItemIndex).Amount));
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
      Update_Messages;
      ShowSkyMap;
      return TCL_OK;
   end Deliver_Medicines_Command;

   procedure AddCommands is
   begin
      Add_Command("ShowOrders", Show_Orders_Command'Access);
      Add_Command("Docking", Docking_Command'Access);
      Add_Command("AskForBases", Ask_For_Bases_Command'Access);
      Add_Command("AskForEvents", Ask_For_Events_Command'Access);
      Add_Command("Attack", Attack_Command'Access);
      Add_Command("Pray", Pray_Command'Access);
      Add_Command("SetAsHome", Set_As_Home_Command'Access);
      Add_Command("ShowTrader", Show_Trader_Command'Access);
      Add_Command("StartMission", Start_Mission_Command'Access);
      Add_Command("CompleteMission", Complete_Mission_Command'Access);
      Add_Command("ExecuteStory", Execute_Story_Command'Access);
      Add_Command("DeliverMedicines", Deliver_Medicines_Command'Access);
   end AddCommands;

end OrdersMenu;
