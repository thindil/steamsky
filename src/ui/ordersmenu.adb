-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
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
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      MissionsLimit: Integer;
      Event: Events_Types := NONE;
      ItemIndex: Natural;
      OrdersMenu: constant Ttk_Frame :=
        Create_Dialog(".gameframe.orders", "Ship orders");
      CloseButton: constant Ttk_Button :=
        Create
          (OrdersMenu & ".closebutton",
           "-text Close -command {CloseDialog " & OrdersMenu & "}");
      Last_Button: Ttk_Button := Get_Widget(".", Interp);
      type Order_Shortcut is record
         ButtonName: Unbounded_String;
         Shortcut: Character;
      end record;
      package Shortcuts_Container is new Vectors
        (Index_Type => Positive, Element_Type => Order_Shortcut);
      Shortcuts: Shortcuts_Container.Vector;
      procedure Add_Button
        (Name, Label, Command, Shortcut: String; UnderLine: Natural;
         Row: Integer := -1) is
         Button: constant Ttk_Button :=
           Create
             (OrdersMenu & Name,
              "-text {" & Label & "} -command {CloseDialog " & OrdersMenu &
              ";" & Command & "} -underline" & Natural'Image(UnderLine) &
              (if Row = -1 then "" else " -row" & Integer'Image(Row)));
      begin
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky we -padx 5");
         Bind(Button, "<Escape>", "{" & CloseButton & " invoke;break}");
         Last_Button := Button;
         Shortcuts.Append
           ((To_Unbounded_String(OrdersMenu & Name),
             Shortcut(Shortcut'First)));
      end Add_Button;
   begin
      if Winfo_Get(OrdersMenu, "ismapped") = "1" then
         return Close_Dialog_Command(ClientData, Interp, Argc, Argv);
      end if;
      if Find_Member(TALK) > 0 then
         HaveTrader := True;
      end if;
      if Current_Story.Index /= Null_Unbounded_String then
         declare
            Step: constant Step_Data :=
              (if Current_Story.Current_Step = 0 then
                 Stories_List(Current_Story.Index).Starting_Step
               elsif Current_Story.Current_Step > 0 then
                 Stories_List(Current_Story.Index).Steps
                   (Current_Story.Current_Step)
               else Stories_List(Current_Story.Index).Final_Step);
         begin
            case Step.Finish_Condition is
               when ASKINBASE =>
                  if BaseIndex > 0 then
                     if Current_Story.Data = Null_Unbounded_String or
                       Current_Story.Data = Sky_Bases(BaseIndex).Name then
                        Add_Button
                          (".story",
                           "Ask for " &
                           To_String
                             (Items_List
                                (Tiny_String.To_Bounded_String
                                   (Source =>
                                      To_String
                                        (Source =>
                                           Get_Step_Data
                                             (Step.Finish_Data, "item"))))
                                .Name),
                           "ExecuteStory", "f", 4);
                     end if;
                  end if;
               when DESTROYSHIP =>
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create(Tokens, To_String(Current_Story.Data), ";");
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
                           "ExecuteStory", "s", 0);
                     end if;
                  end;
               when EXPLORE =>
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create(Tokens, To_String(Current_Story.Data), ";");
                     if Player_Ship.Sky_X =
                       Positive'Value(Slice(Tokens, 1)) and
                       Player_Ship.Sky_Y =
                         Positive'Value(Slice(Tokens, 2)) then
                        Add_Button
                          (".story", "Search area", "ExecuteStory", "s", 0);
                     end if;
                  end;
               when ANY | LOOT =>
                  null;
            end case;
         end;
      end if;
      if Player_Ship.Speed = DOCKED then
         Add_Button(".undock", "Undock", "Docking", "d", 0);
         if Sky_Bases(BaseIndex).Population > 0 then
            Add_Button(".escape", "Escape", "Docking escape", "a", 3);
         end if;
         if HaveTrader and Sky_Bases(BaseIndex).Population > 0 then
            Add_Button(".trade", "Trade", "ShowTrade", "t", 0);
            Add_Button(".school", "School", "ShowSchool", "s", 0);
            if Sky_Bases(BaseIndex).Recruits.Length > 0 then
               Add_Button(".recruits", "Recruit", "ShowRecruit", "r", 0);
            end if;
            if Days_Difference(Sky_Bases(BaseIndex).Asked_For_Events) > 6 then
               Add_Button(".events", "Ask for events", "AskForEvents", "e", 8);
            end if;
            if not Sky_Bases(BaseIndex).Asked_For_Bases then
               Add_Button(".bases", "Ask for bases", "AskForBases", "b", 8);
            end if;
            if Bases_Types_List(Sky_Bases(BaseIndex).Base_Type).Flags.Contains
                (To_Unbounded_String("temple")) then
               Add_Button(".pray", "Pray", "Pray", "p", 0);
            end if;
            Add_Heal_Wounded_Menu_Loop :
            for Member of Player_Ship.Crew loop
               if Member.Health < 100 then
                  Add_Button
                    (".heal", "Heal wounded", "ShowBaseUI heal", "w", 5);
                  exit Add_Heal_Wounded_Menu_Loop;
               end if;
            end loop Add_Heal_Wounded_Menu_Loop;
            Add_Repair_Ship_Menu_Loop :
            for Module of Player_Ship.Modules loop
               if Module.Durability < Module.Max_Durability then
                  Add_Button
                    (".repair", "Repair ship", "ShowBaseUI repair", "p", 2);
                  exit Add_Repair_Ship_Menu_Loop;
               end if;
            end loop Add_Repair_Ship_Menu_Loop;
            if Bases_Types_List(Sky_Bases(BaseIndex).Base_Type).Flags.Contains
                (To_Unbounded_String("shipyard")) then
               Add_Button(".shipyard", "Shipyard", "ShowShipyard", "i", 2);
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
                    (".recipes", "Buy recipes", "ShowBaseUI recipes", "y", 2);
                  exit Add_Buy_Recipes_Menu_Loop;
               end if;
            end loop Add_Buy_Recipes_Menu_Loop;
            if Sky_Bases(BaseIndex).Missions.Length > 0 then
               MissionsLimit :=
                 (case Sky_Bases(BaseIndex).Reputation(1) is when 0 .. 25 => 1,
                    when 26 .. 50 => 3, when 51 .. 75 => 5,
                    when 76 .. 100 => 10, when others => 0);
               Add_Mission_Menu_Loop :
               for Mission of Accepted_Missions loop
                  if (Mission.Finished and Mission.Start_Base = BaseIndex) or
                    (Mission.Target_X = Player_Ship.Sky_X and
                     Mission.Target_Y = Player_Ship.Sky_Y) then
                     case Mission.M_Type is
                        when DELIVER =>
                           Add_Button
                             (".mission",
                              "Complete delivery of " &
                              To_String(Items_List(Mission.Item_Index).Name),
                              "CompleteMission", "c", 0, 0);
                        when DESTROY =>
                           if Mission.Finished then
                              Add_Button
                                (".mission",
                                 "Complete destroy " &
                                 To_String
                                   (Proto_Ships_List(Mission.Ship_Index).Name),
                                 "CompleteMission", "c", 0, 0);
                           end if;
                        when PATROL =>
                           if Mission.Finished then
                              Add_Button
                                (".mission", "Complete Patrol area mission",
                                 "CompleteMission", "c", 0, 0);
                           end if;
                        when EXPLORE =>
                           if Mission.Finished then
                              Add_Button
                                (".mission", "Complete Explore area mission",
                                 "CompleteMission", "c", 0, 0);
                           end if;
                        when PASSENGER =>
                           if Mission.Finished then
                              Add_Button
                                (".mission",
                                 "Complete Transport passenger mission",
                                 "CompleteMission", "c", 0, 0);
                           end if;
                     end case;
                  end if;
                  if Mission.Start_Base = BaseIndex then
                     MissionsLimit := MissionsLimit - 1;
                  end if;
               end loop Add_Mission_Menu_Loop;
               if MissionsLimit > 0 then
                  Add_Button
                    (".missions", "Missions", "ShowBaseMissions", "m", 0);
               end if;
            end if;
            if Player_Ship.Home_Base /= BaseIndex then
               Add_Button(".home", "Set as home", "SetAsHome", "h", 7);
            end if;
         end if;
         if Sky_Bases(BaseIndex).Population = 0 then
            Add_Button(".loot", "Loot", "ShowLoot", "l", 0);
         end if;
      else
         if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index > 0 then
            Event :=
              Events_List
                (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
                .E_Type;
         end if;
         case Event is
            when ENEMYSHIP | ENEMYPATROL =>
               Add_Button(".event", "Attack", "Attack", "a", 0);
            when FULLDOCKS =>
               Add_Button(".event", "Wait (full docks)", "ShowWait", "w", 0);
            when ATTACKONBASE =>
               Add_Button(".event", "Defend", "Attack", "d", 0);
            when DISEASE =>
               if HaveTrader then
                  ItemIndex :=
                    Find_Item
                      (Inventory => Player_Ship.Cargo,
                       Item_Type =>
                         Factions_List(Sky_Bases(BaseIndex).Owner)
                           .Healing_Tools);
                  if ItemIndex > 0 then
                     Add_Button
                       (".deliverfree", "Deliver medicines for free",
                        "DeliverMedicines free", "d", 0);
                     Add_Button
                       (".deliverprice", "Deliver medicines for price",
                        "DeliverMedicines paid", "m", 8);
                  end if;
               end if;
            when NONE | DOUBLEPRICE | BASERECOVERY =>
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
                           Add_Button
                             (".dock",
                              "Dock (" &
                              Trim(Positive'Image(DockingCost), Left) & " " &
                              To_String(Money_Name) & ")",
                              "Docking", "d", 0);
                        else
                           Add_Button(".dock", "Dock", "Docking", "d", 0);
                        end if;
                     end;
                  end if;
                  Complete_Mission_Menu_Loop :
                  for Mission of Accepted_Missions loop
                     if HaveTrader and Mission.Target_X = Player_Ship.Sky_X and
                       Mission.Target_Y = Player_Ship.Sky_Y and
                       Mission.Finished then
                        case Mission.M_Type is
                           when DELIVER =>
                              Add_Button
                                (".mission",
                                 "Complete delivery of " &
                                 To_String
                                   (Items_List(Mission.Item_Index).Name),
                                 "CompleteMission", "c", 0);
                           when DESTROY =>
                              if Mission.Finished then
                                 Add_Button
                                   (".mission",
                                    "Complete destroy " &
                                    To_String
                                      (Proto_Ships_List(Mission.Ship_Index)
                                         .Name),
                                    "CompleteMission", "c", 0);
                              end if;
                           when PATROL =>
                              if Mission.Finished then
                                 Add_Button
                                   (".mission", "Complete Patrol area mission",
                                    "CompleteMission", "c", 0);
                              end if;
                           when EXPLORE =>
                              if Mission.Finished then
                                 Add_Button
                                   (".mission",
                                    "Complete Explore area mission",
                                    "CompleteMission", "c", 0);
                              end if;
                           when PASSENGER =>
                              if Mission.Finished then
                                 Add_Button
                                   (".mission",
                                    "Complete Transport passenger mission",
                                    "CompleteMission", "c", 0);
                              end if;
                        end case;
                     end if;
                  end loop Complete_Mission_Menu_Loop;
               else
                  Progress_Mission_Loop :
                  for Mission of Accepted_Missions loop
                     if Mission.Target_X = Player_Ship.Sky_X and
                       Mission.Target_Y = Player_Ship.Sky_Y and
                       not Mission.Finished then
                        case Mission.M_Type is
                           when DELIVER | PASSENGER =>
                              null;
                           when DESTROY =>
                              Add_Button
                                (".mission",
                                 "Search for " &
                                 To_String
                                   (Proto_Ships_List(Mission.Ship_Index).Name),
                                 "StartMission", "s", 0);
                           when PATROL =>
                              Add_Button
                                (".mission", "Patrol area", "StartMission",
                                 "p", 0);
                           when EXPLORE =>
                              Add_Button
                                (".mission", "Explore area", "StartMission",
                                 "e", 0);
                        end case;
                     end if;
                  end loop Progress_Mission_Loop;
               end if;
            when TRADER =>
               if HaveTrader then
                  Add_Button
                    (".trade", "Trade",
                     "ShowTrader " &
                     To_String
                       (Events_List
                          (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                             .Event_Index)
                          .Ship_Index),
                     "t", 0);
                  Add_Button
                    (".askevents", "Ask for events", "AskForEvents", "e", 8);
                  Add_Button
                    (".askbases", "Ask for bases", "AskForBases", "b", 8);
               end if;
               Add_Button(".attack", "Attack", "Attack", "a", 0);
            when FRIENDLYSHIP =>
               if HaveTrader then
                  if Index
                      (Proto_Ships_List
                         (Events_List
                            (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                               .Event_Index)
                            .Ship_Index)
                         .Name,
                       To_String(Traders_Name)) >
                    0 then
                     Add_Button
                       (".trade", "Trade",
                        "ShowTrader " &
                        To_String
                          (Events_List
                             (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                                .Event_Index)
                             .Ship_Index),
                        "t", 0);
                     Add_Button
                       (".askbases", "Ask for bases", "AskForBases", "b", 8);
                  end if;
                  Add_Button
                    (".askevents", "Ask for events", "AskForEvents", "e", 8);
               end if;
               Add_Button(".attack", "Attack", "Attack", "a", 0);
         end case;
      end if;
      if Last_Button = Get_Widget(".", Interp) then
         Show_Message
           (Text =>
              "Here are no available ship orders at this moment. Ship orders available mostly when you are at base or at event on map.",
            Title => "No orders available");
      else
         Tcl.Tk.Ada.Grid.Grid(CloseButton, "-sticky we -padx 5 -pady {0 5}");
         Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
         Bind(Last_Button, "<Tab>", "{focus " & CloseButton & ";break}");
         for Shortcut of Shortcuts loop
            Bind
              (CloseButton, "<Alt-" & Shortcut.Shortcut & ">",
               "{" & To_String(Shortcut.ButtonName) & " invoke;break}");
         end loop;
         declare
            MenuButton: Ttk_Button;
         begin
            for Button of Shortcuts loop
               MenuButton := Get_Widget(To_String(Button.ButtonName), Interp);
               for Shortcut of Shortcuts loop
                  Bind
                    (MenuButton, "<Alt-" & Shortcut.Shortcut & ">",
                     "{" & To_String(Shortcut.ButtonName) & " invoke;break}");
               end loop;
            end loop;
         end;
         Show_Dialog
           (Dialog => OrdersMenu, Parent_Frame => ".gameframe",
            Relative_X => 0.4,
            Relative_Y => (if Player_Ship.Speed = DOCKED then 0.1 else 0.3));
         Focus(CloseButton);
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
            Show_Message
              (Text => To_String(Message), Title => "Can't undock from base");
            return TCL_OK;
         end if;
      else
         if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index > 0 then
            if Events_List
                (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
                .E_Type =
              FULLDOCKS then
               return Show_Wait_Command(ClientData, Interp, Argc, Argv);
            end if;
         end if;
         Message := To_Unbounded_String(DockShip(True));
         if Length(Message) > 0 then
            Show_Message
              (Text => To_String(Message), Title => "Can't dock to base");
            return TCL_OK;
         end if;
      end if;
      Show_Sky_Map;
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
      Show_Sky_Map;
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
      Show_Sky_Map;
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
         Update_Morale(Player_Ship, Crew_Container.To_Index(I), 10);
      end loop Update_Morale_Loop;
      Add_Message
        ("You and your crew were praying for some time. Now you all feel a bit better.",
         ORDERMESSAGE);
      Update_Game(30);
      Show_Sky_Map;
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
      TraderIndex: constant Natural := Find_Member(TALK);
      Price: Positive := 1_000;
   begin
      Count_Price(Price, TraderIndex);
      Show_Question
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
      for Mission of Accepted_Missions loop
         if Mission.Target_X = Player_Ship.Sky_X and
           Mission.Target_Y = Player_Ship.Sky_Y and not Mission.Finished then
            case Mission.M_Type is
               when DELIVER | PASSENGER =>
                  null;
               when DESTROY =>
                  Update_Game(Get_Random(15, 45));
                  StartsCombat := Check_For_Event;
                  if not StartsCombat then
                     StartsCombat :=
                       StartCombat
                         (Accepted_Missions
                            (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                               .Mission_Index)
                            .Ship_Index,
                          False);
                  end if;
               when PATROL =>
                  Update_Game(Get_Random(45, 75));
                  StartsCombat := Check_For_Event;
                  if not StartsCombat then
                     Update_Mission
                       (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                          .Mission_Index);
                  end if;
               when EXPLORE =>
                  Update_Game(Get_Random(30, 60));
                  StartsCombat := Check_For_Event;
                  if not StartsCombat then
                     Update_Mission
                       (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                          .Mission_Index);
                  end if;
            end case;
            exit;
         end if;
      end loop;
      if StartsCombat then
         ShowCombatUI;
         return TCL_OK;
      end if;
      Update_Header;
      Update_Messages;
      Show_Sky_Map;
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
      Finish_Mission
        (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Mission_Index);
      Update_Header;
      Update_Messages;
      Show_Sky_Map;
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
        (if Current_Story.Current_Step = 0 then
           Stories_List(Current_Story.Index).Starting_Step
         elsif Current_Story.Current_Step > 0 then
           Stories_List(Current_Story.Index).Steps(Current_Story.Current_Step)
         else Stories_List(Current_Story.Index).Final_Step);
      Message: Unbounded_String;
   begin
      if Player_Ship.Speed /= DOCKED and Step.Finish_Condition = ASKINBASE then
         Message := To_Unbounded_String(DockShip(True));
         if Message /= Null_Unbounded_String then
            Show_Info
              (Text => To_String(Message), Title => "Can't dock to base");
            return TCL_OK;
         end if;
      end if;
      if Progress_Story then
         declare
            Tokens: Slice_Set;
         begin
            Create(Tokens, To_String(Current_Story.Data), ";");
            case Step.Finish_Condition is
               when DESTROYSHIP =>
                  if StartCombat
                      (To_Unbounded_String(Slice(Tokens, 3)), False) then
                     ShowCombatUI;
                     return TCL_OK;
                  end if;
               when others =>
                  null;
            end case;
            if Current_Story.Current_Step > -2 then
               Step :=
                 (if Current_Story.Current_Step > 0 then
                    Stories_List(Current_Story.Index).Steps
                      (Current_Story.Current_Step)
                  else Stories_List(Current_Story.Index).Final_Step);
               for Text of Step.Texts loop
                  if Current_Story.Finished_Step = Text.Condition then
                     Show_Info(Text => To_String(Text.Text), Title => "Story");
                     Current_Story.Show_Text := False;
                     exit;
                  end if;
               end loop;
            else
               Finish_Story;
            end if;
         end;
      else
         Show_Info(Text => To_String(Step.Fail_Text), Title => "Story");
         Current_Story.Show_Text := False;
      end if;
      Update_Header;
      Update_Messages;
      Show_Sky_Map;
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
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      EventIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index;
      ItemIndex: constant Natural :=
        Find_Item
          (Inventory => Player_Ship.Cargo,
           Item_Type =>
             Factions_List(Sky_Bases(BaseIndex).Owner).Healing_Tools);
      NewTime: constant Integer :=
        Events_List(EventIndex).Time - Player_Ship.Cargo(ItemIndex).Amount;
   begin
      if NewTime < 1 then
         Delete_Event(EventIndex);
      else
         Events_List(EventIndex).Time := NewTime;
      end if;
      if CArgv.Arg(Argv, 1) = "free" then
         Gain_Rep(BaseIndex, (Player_Ship.Cargo(ItemIndex).Amount / 10));
         Add_Message
           ("You gave " &
            To_String
              (Items_List(Player_Ship.Cargo(ItemIndex).Proto_Index).Name) &
            " for free to base.",
            TRADEMESSAGE);
         UpdateCargo
           (Player_Ship, Player_Ship.Cargo.Element(ItemIndex).Proto_Index,
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
               Show_Message
                 (Text =>
                    "You can't sell medicines to the base because you don't have enough free cargo space for money.",
                  Title => "No free cargo space");
            when Trade_No_Money_In_Base =>
               Show_Message
                 (Text =>
                    "You can't sell medicines to the base because the base don't have enough money to buy them.",
                  Title => "Can't sell medicines");
         end;
      end if;
      Update_Header;
      Update_Messages;
      Show_Sky_Map;
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
