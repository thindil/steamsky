-- Copyright (c) 2020-2023 Bartek thindil Jasicki <thindil@laeran.pl>
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
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tiny_String;

      Have_Trader: Boolean := False;
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Missions_Limit: Integer;
      Event: Events_Types := NONE;
      Item_Index: Natural;
      Orders_Menu: constant Ttk_Frame :=
        Create_Dialog(Name => ".gameframe.orders", Title => "Ship orders");
      Dialog_Close_Button: constant Ttk_Button :=
        Create
          (pathName => Orders_Menu & ".closebutton",
           options =>
             "-text Close -command {CloseDialog " & Orders_Menu & "}");
      Last_Button: Ttk_Button := Get_Widget(pathName => ".", Interp => Interp);
      type Order_Shortcut is record
         Button_Name: Unbounded_String;
         Shortcut: Character;
      end record;
      package Shortcuts_Container is new Vectors
        (Index_Type => Positive, Element_Type => Order_Shortcut);
      Shortcuts: Shortcuts_Container.Vector;
      procedure Add_Button
        (Name, Label, Command, Shortcut: String; Underline: Natural;
         Row: Integer := -1) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Orders_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Orders_Menu &
                ";" & Command & "} -underline" & Natural'Image(Underline));
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Row = -1 then "" else " -row" & Integer'Image(Row)));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{" & Dialog_Close_Button & " invoke;break}");
         Last_Button := Button;
         Shortcuts.Append
           (New_Item =>
              (Button_Name =>
                 To_Unbounded_String(Source => Orders_Menu & Name),
               Shortcut => Shortcut(Shortcut'First)));
      end Add_Button;
   begin
      if Winfo_Get(Widgt => Orders_Menu, Info => "ismapped") = "1" then
         return
           Close_Dialog_Command
             (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
              Argv => Argv);
      end if;
      if Find_Member(Order => TALK) > 0 then
         Have_Trader := True;
      end if;
      if Current_Story.Index /= Null_Unbounded_String then
         Show_Story_Button_Block :
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
                  if Base_Index > 0 then
                     if Current_Story.Data = Null_Unbounded_String or
                       To_String(Source => Current_Story.Data) =
                         To_String(Source => Sky_Bases(Base_Index).Name) then
                        Add_Button
                          (Name => ".story",
                           Label =>
                             "Ask for " &
                             To_String
                               (Source =>
                                  Get_Proto_Item
                                    (Index =>
                                       Positive'Value
                                         (To_String
                                            (Source =>
                                               Get_Step_Data
                                                 (Finish_Data =>
                                                    Step.Finish_Data,
                                                  Name => "item"))))
                                    .Name),
                           Command => "ExecuteStory", Shortcut => "f",
                           Underline => 4);
                     end if;
                  end if;
               when DESTROYSHIP =>
                  Show_Destroy_Ship_Button_Block :
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create
                       (S => Tokens,
                        From => To_String(Source => Current_Story.Data),
                        Separators => ";");
                     if Player_Ship.Sky_X =
                       Positive'Value(Slice(S => Tokens, Index => 1)) and
                       Player_Ship.Sky_Y =
                         Positive'Value(Slice(S => Tokens, Index => 2)) then
                        Add_Button
                          (Name => ".story",
                           Label =>
                             "Search for " &
                             To_String
                               (Source =>
                                  Proto_Ships_List
                                    (Positive'Value
                                       (Slice(S => Tokens, Index => 3)))
                                    .Name),
                           Command => "ExecuteStory", Shortcut => "s",
                           Underline => 0);
                     end if;
                  end Show_Destroy_Ship_Button_Block;
               when EXPLORE =>
                  Show_Explore_Story_Button_Block :
                  declare
                     Tokens: Slice_Set;
                  begin
                     Create
                       (S => Tokens,
                        From => To_String(Source => Current_Story.Data),
                        Separators => ";");
                     if Player_Ship.Sky_X =
                       Positive'Value(Slice(S => Tokens, Index => 1)) and
                       Player_Ship.Sky_Y =
                         Positive'Value(Slice(S => Tokens, Index => 2)) then
                        Add_Button
                          (Name => ".story", Label => "Search area",
                           Command => "ExecuteStory", Shortcut => "s",
                           Underline => 0);
                     end if;
                  end Show_Explore_Story_Button_Block;
               when ANY | LOOT =>
                  null;
            end case;
         end Show_Story_Button_Block;
      end if;
      if Player_Ship.Speed = DOCKED then
         Add_Button
           (Name => ".undock", Label => "Undock", Command => "Docking",
            Shortcut => "d", Underline => 0);
         if Sky_Bases(Base_Index).Population > 0 then
            Add_Button
              (Name => ".escape", Label => "Escape",
               Command => "Docking escape", Shortcut => "a", Underline => 3);
         end if;
         if Have_Trader and Sky_Bases(Base_Index).Population > 0 then
            Add_Button
              (Name => ".trade", Label => "Trade", Command => "ShowTrade",
               Shortcut => "t", Underline => 0);
            Add_Button
              (Name => ".school", Label => "School", Command => "ShowSchool",
               Shortcut => "s", Underline => 0);
            if Recruit_Container.Length
                (Container => Sky_Bases(Base_Index).Recruits) >
              0 then
               Add_Button
                 (Name => ".recruits", Label => "Recruit",
                  Command => "ShowRecruit", Shortcut => "r", Underline => 0);
            end if;
            if Days_Difference
                (Date_To_Compare => Sky_Bases(Base_Index).Asked_For_Events) >
              6 then
               Add_Button
                 (Name => ".events", Label => "Ask for events",
                  Command => "AskForEvents", Shortcut => "e", Underline => 8);
            end if;
            if not Sky_Bases(Base_Index).Asked_For_Bases then
               Add_Button
                 (Name => ".bases", Label => "Ask for bases",
                  Command => "AskForBases", Shortcut => "b", Underline => 8);
            end if;
            if Has_Flag
                (Base_Type => Sky_Bases(Base_Index).Base_Type,
                 Flag => "temple") then
               Add_Button
                 (Name => ".pray", Label => "Pray", Command => "Pray",
                  Shortcut => "p", Underline => 0);
            end if;
            Add_Heal_Wounded_Menu_Loop :
            for Member of Player_Ship.Crew loop
               if Member.Health < 100 then
                  Add_Button
                    (Name => ".heal", Label => "Heal wounded",
                     Command => "ShowBaseUI heal", Shortcut => "w",
                     Underline => 5);
                  exit Add_Heal_Wounded_Menu_Loop;
               end if;
            end loop Add_Heal_Wounded_Menu_Loop;
            Add_Repair_Ship_Menu_Loop :
            for Module of Player_Ship.Modules loop
               if Module.Durability < Module.Max_Durability then
                  Add_Button
                    (Name => ".repair", Label => "Repair ship",
                     Command => "ShowBaseUI repair", Shortcut => "p",
                     Underline => 2);
                  exit Add_Repair_Ship_Menu_Loop;
               end if;
            end loop Add_Repair_Ship_Menu_Loop;
            if Has_Flag
                (Base_Type => Sky_Bases(Base_Index).Base_Type,
                 Flag => "shipyard") then
               Add_Button
                 (Name => ".shipyard", Label => "Shipyard",
                  Command => "ShowShipyard", Shortcut => "i", Underline => 2);
            end if;
            Add_Buy_Recipes_Menu_Loop :
            for I in 1 .. Get_Recipes_Amount loop
               if Known_Recipes.Find_Index
                   (Item =>
                      To_Bounded_String
                        (Source => Trim(Source => I'Img, Side => Both))) =
                 UnboundedString_Container.No_Index and
                 Has_Recipe
                   (Base_Type => Sky_Bases(Base_Index).Base_Type,
                    Recipe => Trim(Source => I'Img, Side => Both)) and
                 Get_Recipe
                     (Recipe_Index =>
                        To_Bounded_String
                          (Source => Trim(Source => I'Img, Side => Both)))
                     .Reputation <=
                   Sky_Bases(Base_Index).Reputation.Level then
                  Add_Button
                    (Name => ".recipes", Label => "Buy recipes",
                     Command => "ShowBaseUI recipes", Shortcut => "y",
                     Underline => 2);
                  exit Add_Buy_Recipes_Menu_Loop;
               end if;
            end loop Add_Buy_Recipes_Menu_Loop;
            if Sky_Bases(Base_Index).Missions.Length > 0 then
               Missions_Limit :=
                 (case Sky_Bases(Base_Index).Reputation.Level is
                    when 0 .. 25 => 1, when 26 .. 50 => 3, when 51 .. 75 => 5,
                    when 76 .. 100 => 10, when others => 0);
               Add_Mission_Menu_Loop :
               for Mission of Accepted_Missions loop
                  if (Mission.Finished and Mission.Start_Base = Base_Index) or
                    (Mission.Target_X = Player_Ship.Sky_X and
                     Mission.Target_Y = Player_Ship.Sky_Y) then
                     case Mission.M_Type is
                        when DELIVER =>
                           Add_Button
                             (Name => ".mission",
                              Label =>
                                "Complete delivery of " &
                                To_String
                                  (Source =>
                                     Get_Proto_Item
                                       (Index => Mission.Item_Index)
                                       .Name),
                              Command => "CompleteMission", Shortcut => "c",
                              Underline => 0, Row => 0);
                        when DESTROY =>
                           if Mission.Finished then
                              Add_Button
                                (Name => ".mission",
                                 Label =>
                                   "Complete destroy " &
                                   To_String
                                     (Source =>
                                        Proto_Ships_List(Mission.Ship_Index)
                                          .Name),
                                 Command => "CompleteMission", Shortcut => "c",
                                 Underline => 0, Row => 0);
                           end if;
                        when PATROL =>
                           if Mission.Finished then
                              Add_Button
                                (Name => ".mission",
                                 Label => "Complete Patrol area mission",
                                 Command => "CompleteMission", Shortcut => "c",
                                 Underline => 0, Row => 0);
                           end if;
                        when EXPLORE =>
                           if Mission.Finished then
                              Add_Button
                                (Name => ".mission",
                                 Label => "Complete Explore area mission",
                                 Command => "CompleteMission", Shortcut => "c",
                                 Underline => 0, Row => 0);
                           end if;
                        when PASSENGER =>
                           if Mission.Finished then
                              Add_Button
                                (Name => ".mission",
                                 Label =>
                                   "Complete Transport passenger mission",
                                 Command => "CompleteMission", Shortcut => "c",
                                 Underline => 0, Row => 0);
                           end if;
                     end case;
                  end if;
                  if Mission.Start_Base = Base_Index then
                     Missions_Limit := Missions_Limit - 1;
                  end if;
               end loop Add_Mission_Menu_Loop;
               if Missions_Limit > 0 then
                  Add_Button
                    (Name => ".missions", Label => "Missions",
                     Command => "ShowBaseMissions", Shortcut => "m",
                     Underline => 0);
               end if;
            end if;
            if Player_Ship.Home_Base /= Base_Index then
               Add_Button
                 (Name => ".home", Label => "Set as home",
                  Command => "SetAsHome", Shortcut => "h", Underline => 7);
            end if;
         end if;
         if Sky_Bases(Base_Index).Population = 0 then
            Add_Button
              (Name => ".loot", Label => "Loot", Command => "ShowLoot",
               Shortcut => "l", Underline => 0);
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
               Add_Button
                 (Name => ".event", Label => "Attack", Command => "Attack",
                  Shortcut => "a", Underline => 0);
            when FULLDOCKS =>
               Add_Button
                 (Name => ".event", Label => "Wait (full docks)",
                  Command => "ShowWait", Shortcut => "w", Underline => 0);
            when ATTACKONBASE =>
               Add_Button
                 (Name => ".event", Label => "Defend", Command => "Attack",
                  Shortcut => "d", Underline => 0);
            when DISEASE =>
               if Have_Trader then
                  Item_Index :=
                    Find_Item
                      (Inventory => Player_Ship.Cargo,
                       Item_Type =>
                         Get_Faction(Index => Sky_Bases(Base_Index).Owner)
                           .Healing_Tools);
                  if Item_Index > 0 then
                     Add_Button
                       (Name => ".deliverfree",
                        Label => "Deliver medicines for free",
                        Command => "DeliverMedicines free", Shortcut => "d",
                        Underline => 0);
                     Add_Button
                       (Name => ".deliverprice",
                        Label => "Deliver medicines for price",
                        Command => "DeliverMedicines paid", Shortcut => "m",
                        Underline => 8);
                  end if;
               end if;
            when NONE | DOUBLEPRICE | BASERECOVERY =>
               if Base_Index > 0 then
                  if Sky_Bases(Base_Index).Reputation.Level > -25 then
                     Show_Docking_Button_Block :
                     declare
                        Docking_Cost: Positive;
                     begin
                        Count_Docking_Cost_Loop :
                        for Module of Player_Ship.Modules loop
                           if Module.M_Type = HULL then
                              Docking_Cost := Module.Max_Modules;
                              exit Count_Docking_Cost_Loop;
                           end if;
                        end loop Count_Docking_Cost_Loop;
                        if Sky_Bases(Base_Index).Population > 0 then
                           Add_Button
                             (Name => ".dock",
                              Label =>
                                "Dock (" &
                                Trim
                                  (Source => Positive'Image(Docking_Cost),
                                   Side => Left) &
                                " " & To_String(Source => Money_Name) & ")",
                              Command => "Docking", Shortcut => "d",
                              Underline => 0);
                        else
                           Add_Button
                             (Name => ".dock", Label => "Dock",
                              Command => "Docking", Shortcut => "d",
                              Underline => 0);
                        end if;
                     end Show_Docking_Button_Block;
                  end if;
                  Complete_Mission_Menu_Loop :
                  for Mission of Accepted_Missions loop
                     if Have_Trader and
                       Mission.Target_X = Player_Ship.Sky_X and
                       Mission.Target_Y = Player_Ship.Sky_Y and
                       Mission.Finished then
                        case Mission.M_Type is
                           when DELIVER =>
                              Add_Button
                                (Name => ".mission",
                                 Label =>
                                   "Complete delivery of " &
                                   To_String
                                     (Source =>
                                        Get_Proto_Item
                                          (Index => Mission.Item_Index)
                                          .Name),
                                 Command => "CompleteMission", Shortcut => "c",
                                 Underline => 0);
                           when DESTROY =>
                              if Mission.Finished then
                                 Add_Button
                                   (Name => ".mission",
                                    Label =>
                                      "Complete destroy " &
                                      To_String
                                        (Source =>
                                           Proto_Ships_List(Mission.Ship_Index)
                                             .Name),
                                    Command => "CompleteMission",
                                    Shortcut => "c", Underline => 0);
                              end if;
                           when PATROL =>
                              if Mission.Finished then
                                 Add_Button
                                   (Name => ".mission",
                                    Label => "Complete Patrol area mission",
                                    Command => "CompleteMission",
                                    Shortcut => "c", Underline => 0);
                              end if;
                           when EXPLORE =>
                              if Mission.Finished then
                                 Add_Button
                                   (Name => ".mission",
                                    Label => "Complete Explore area mission",
                                    Command => "CompleteMission",
                                    Shortcut => "c", Underline => 0);
                              end if;
                           when PASSENGER =>
                              if Mission.Finished then
                                 Add_Button
                                   (Name => ".mission",
                                    Label =>
                                      "Complete Transport passenger mission",
                                    Command => "CompleteMission",
                                    Shortcut => "c", Underline => 0);
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
                                (Name => ".mission",
                                 Label =>
                                   "Search for " &
                                   To_String
                                     (Source =>
                                        Proto_Ships_List(Mission.Ship_Index)
                                          .Name),
                                 Command => "StartMission", Shortcut => "s",
                                 Underline => 0);
                           when PATROL =>
                              Add_Button
                                (Name => ".mission", Label => "Patrol area",
                                 Command => "StartMission", Shortcut => "p",
                                 Underline => 0);
                           when EXPLORE =>
                              Add_Button
                                (Name => ".mission", Label => "Explore area",
                                 Command => "StartMission", Shortcut => "e",
                                 Underline => 0);
                        end case;
                     end if;
                  end loop Progress_Mission_Loop;
               end if;
            when TRADER =>
               if Have_Trader then
                  Add_Button
                    (Name => ".trade", Label => "Trade",
                     Command =>
                       "ShowTrader " &
                       Positive'Image
                         (Events_List
                            (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                               .Event_Index)
                            .Ship_Index),
                     Shortcut => "t", Underline => 0);
                  Add_Button
                    (Name => ".askevents", Label => "Ask for events",
                     Command => "AskForEvents", Shortcut => "e",
                     Underline => 8);
                  Add_Button
                    (Name => ".askbases", Label => "Ask for bases",
                     Command => "AskForBases", Shortcut => "b",
                     Underline => 8);
               end if;
               Add_Button
                 (Name => ".attack", Label => "Attack", Command => "Attack",
                  Shortcut => "a", Underline => 0);
            when FRIENDLYSHIP =>
               if Have_Trader then
                  if Index
                      (Source =>
                         Proto_Ships_List
                           (Events_List
                              (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                                 .Event_Index)
                              .Ship_Index)
                           .Name,
                       Pattern => To_String(Source => Traders_Name)) >
                    0 then
                     Add_Button
                       (Name => ".trade", Label => "Trade",
                        Command =>
                          "ShowTrader " &
                          Positive'Image
                            (Events_List
                               (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                                  .Event_Index)
                               .Ship_Index),
                        Shortcut => "t", Underline => 0);
                     Add_Button
                       (Name => ".askbases", Label => "Ask for bases",
                        Command => "AskForBases", Shortcut => "b",
                        Underline => 8);
                  end if;
                  Add_Button
                    (Name => ".askevents", Label => "Ask for events",
                     Command => "AskForEvents", Shortcut => "e",
                     Underline => 8);
               end if;
               Add_Button
                 (Name => ".attack", Label => "Attack", Command => "Attack",
                  Shortcut => "a", Underline => 0);
         end case;
      end if;
      if Last_Button = Get_Widget(pathName => ".", Interp => Interp) then
         Show_Message
           (Text =>
              "Here are no available ship orders at this moment. Ship orders available mostly when you are at base or at event on map.",
            Title => "No orders available");
      else
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Dialog_Close_Button,
            Options => "-sticky we -padx 5 -pady {0 5}");
         Bind
           (Widgt => Dialog_Close_Button, Sequence => "<Escape>",
            Script => "{" & Dialog_Close_Button & " invoke;break}");
         Bind
           (Widgt => Last_Button, Sequence => "<Tab>",
            Script => "{focus " & Dialog_Close_Button & ";break}");
         Set_Shortcuts_Loop :
         for Shortcut of Shortcuts loop
            Bind
              (Widgt => Dialog_Close_Button,
               Sequence => "<Alt-" & Shortcut.Shortcut & ">",
               Script =>
                 "{" & To_String(Source => Shortcut.Button_Name) &
                 " invoke;break}");
         end loop Set_Shortcuts_Loop;
         Add_Shortcuts_To_Buttons_Block :
         declare
            Menu_Button: Ttk_Button;
         begin
            Set_Buttons_Loop :
            for Button of Shortcuts loop
               Menu_Button :=
                 Get_Widget
                   (pathName => To_String(Source => Button.Button_Name),
                    Interp => Interp);
               Set_Button_Shortcuts_Loop :
               for Shortcut of Shortcuts loop
                  Bind
                    (Widgt => Menu_Button,
                     Sequence => "<Alt-" & Shortcut.Shortcut & ">",
                     Script =>
                       "{" & To_String(Source => Shortcut.Button_Name) &
                       " invoke;break}");
               end loop Set_Button_Shortcuts_Loop;
            end loop Set_Buttons_Loop;
         end Add_Shortcuts_To_Buttons_Block;
         Show_Dialog
           (Dialog => Orders_Menu, Parent_Frame => ".gameframe",
            Relative_X => 0.4,
            Relative_Y => (if Player_Ship.Speed = DOCKED then 0.1 else 0.3));
         Focus(Widgt => Dialog_Close_Button);
      end if;
      return TCL_OK;
   end Show_Orders_Command;

   -- ****o* OrdersMenu/OrdersMenu.Docking_Command
   -- FUNCTION
   -- Dock or undock from the sky base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Docking ?escape?
   -- If argument escape is present, escape from the base without paying,
   -- otherwise normal docking or undocking operation
   -- SOURCE
   function Docking_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Docking_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      Message: Unbounded_String;
   begin
      if Player_Ship.Speed = DOCKED then
         Message :=
           (if Argc = 1 then
              To_Unbounded_String(Source => Dock_Ship(Docking => False))
            else To_Unbounded_String
                (Source => Dock_Ship(Docking => False, Escape => True)));
         if Length(Source => Message) > 0 then
            Show_Message
              (Text => To_String(Source => Message),
               Title => "Can't undock from base");
            return TCL_OK;
         end if;
      else
         if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index > 0 then
            if Events_List
                (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
                .E_Type =
              FULLDOCKS then
               return
                 Show_Wait_Command
                   (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
                    Argv => Argv);
            end if;
         end if;
         Message := To_Unbounded_String(Source => Dock_Ship(Docking => True));
         if Length(Source => Message) > 0 then
            Show_Message
              (Text => To_String(Source => Message),
               Title => "Can't dock to base");
            return TCL_OK;
         end if;
      end if;
      Show_Sky_Map;
      if Player_Ship.Speed = DOCKED then
         return
           Show_Orders_Command
             (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
              Argv => Argv);
      end if;
      return TCL_OK;
   end Docking_Command;

   -- ****o* OrdersMenu/OrdersMenu.Ask_For_Bases_Command
   -- FUNCTION
   -- Ask for bases in the currently visited base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- AskForBases
   -- SOURCE
   function Ask_For_Bases_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Ask_For_Bases_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      Ask_For_Bases;
      Show_Sky_Map;
      return TCL_OK;
   end Ask_For_Bases_Command;

   -- ****o* OrdersMenu/OrdersMenu.Ask_For_Events_Command
   -- FUNCTION
   -- Ask for events in the currently visited base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- AskForEvents
   -- SOURCE
   function Ask_For_Events_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Ask_For_Events_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      Ask_For_Events;
      Show_Sky_Map;
      return TCL_OK;
   end Ask_For_Events_Command;

   -- ****o* OrdersMenu/OrdersMenu.Attack_Command
   -- FUNCTION
   -- Start the combat
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Attack
   -- SOURCE
   function Attack_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Attack_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      Show_Combat_Ui;
      return TCL_OK;
   end Attack_Command;

   -- ****f* OrdersMenu/OrdersMenu.Pray_Command
   -- FUNCTION
   -- Pray in the selected base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Pray
   -- SOURCE
   function Pray_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Pray_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      Update_Morale_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Update_Morale
           (Ship => Player_Ship,
            Member_Index => Crew_Container.To_Index(Position => I),
            Value => 10);
      end loop Update_Morale_Loop;
      Add_Message
        (Message =>
           "You and your crew were praying for some time. Now you all feel a bit better.",
         M_Type => ORDERMESSAGE);
      Update_Game(Minutes => 30);
      Show_Sky_Map;
      return TCL_OK;
   end Pray_Command;

   -- ****f* OrdersMenu/OrdersMenu.Set_As_Home_Command
   -- FUNCTION
   -- Set the selected base as a home base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetAsHome
   -- SOURCE
   function Set_As_Home_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_As_Home_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
      Trader_Index: constant Natural := Find_Member(Order => TALK);
      Price: Positive := 1_000;
   begin
      Count_Price(Price => Price, Trader_Index => Trader_Index);
      Show_Question
        (Question =>
           "Are you sure want to change your home base (it cost" &
           Positive'Image(Price) & " " & To_String(Source => Money_Name) &
           ")?",
         Result => "sethomebase");
      return TCL_OK;
   end Set_As_Home_Command;

   -- ****f* OrdersMenu/OrdersMenu.Show_Trader_Command
   -- FUNCTION
   -- Generate cargo for trader and show trading UI
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowTrader protoindex
   -- Protoindex is the index of ship prototype on which trader cargo will be
   -- generated
   -- SOURCE
   function Show_Trader_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Trader_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
   begin
      Generate_Trader_Cargo
        (Proto_Index => Positive'Value(CArgv.Arg(Argv => Argv, N => 1)));
      Tcl_Eval(interp => Interp, strng => "ShowTrade");
      return TCL_OK;
   end Show_Trader_Command;

   -- ****f* OrdersMenu/OrdersMenu.Start_Mission_Command
   -- FUNCTION
   -- Start the selected mission
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- StartMission
   -- SOURCE
   function Start_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Start_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
      Starts_Combat: Boolean := False;
   begin
      Check_Missions_Loop :
      for Mission of Accepted_Missions loop
         if Mission.Target_X = Player_Ship.Sky_X and
           Mission.Target_Y = Player_Ship.Sky_Y and not Mission.Finished then
            case Mission.M_Type is
               when DELIVER | PASSENGER =>
                  null;
               when DESTROY =>
                  Update_Game(Minutes => Get_Random(Min => 15, Max => 45));
                  Starts_Combat := Check_For_Event;
                  if not Starts_Combat then
                     Starts_Combat :=
                       Start_Combat
                         (Enemy_Index =>
                            Accepted_Missions
                              (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                                 .Mission_Index)
                              .Ship_Index,
                          New_Combat => False);
                  end if;
               when PATROL =>
                  Update_Game(Minutes => Get_Random(Min => 45, Max => 75));
                  Starts_Combat := Check_For_Event;
                  if not Starts_Combat then
                     Update_Mission
                       (Mission_Index =>
                          Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                            .Mission_Index);
                  end if;
               when EXPLORE =>
                  Update_Game(Minutes => Get_Random(Min => 30, Max => 60));
                  Starts_Combat := Check_For_Event;
                  if not Starts_Combat then
                     Update_Mission
                       (Mission_Index =>
                          Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                            .Mission_Index);
                  end if;
            end case;
            exit Check_Missions_Loop;
         end if;
      end loop Check_Missions_Loop;
      if Starts_Combat then
         Show_Combat_Ui;
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
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CompleteMission
   -- SOURCE
   function Complete_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Complete_Mission_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      Finish_Mission
        (Mission_Index =>
           Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Mission_Index);
      Update_Header;
      Update_Messages;
      Show_Sky_Map;
      return TCL_OK;
   end Complete_Mission_Command;

   -- ****f* OrdersMenu/OrdersMenu.Execute_Story_Command
   -- FUNCTION
   -- Execute the current step in the current story
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ExecuteStory
   -- SOURCE
   function Execute_Story_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Execute_Story_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
      Step: Step_Data :=
        (if Current_Story.Current_Step = 0 then
           Stories_List(Current_Story.Index).Starting_Step
         elsif Current_Story.Current_Step > 0 then
           Stories_List(Current_Story.Index).Steps(Current_Story.Current_Step)
         else Stories_List(Current_Story.Index).Final_Step);
      Message: Unbounded_String;
   begin
      if Player_Ship.Speed /= DOCKED and Step.Finish_Condition = ASKINBASE then
         Message := To_Unbounded_String(Source => Dock_Ship(Docking => True));
         if Message /= Null_Unbounded_String then
            Show_Info
              (Text => To_String(Source => Message),
               Title => "Can't dock to base");
            return TCL_OK;
         end if;
      end if;
      if Progress_Story then
         Progress_Story_Block :
         declare
            Tokens: Slice_Set;
         begin
            Create
              (S => Tokens, From => To_String(Source => Current_Story.Data),
               Separators => ";");
            case Step.Finish_Condition is
               when DESTROYSHIP =>
                  if Start_Combat
                      (Enemy_Index =>
                         Positive'Value(Slice(S => Tokens, Index => 3)),
                       New_Combat => False) then
                     Show_Combat_Ui;
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
               Show_Current_Story_Loop :
               for Text of Step.Texts loop
                  if Current_Story.Finished_Step = Text.Condition then
                     Show_Info
                       (Text => To_String(Source => Text.Text),
                        Title => "Story");
                     Current_Story.Show_Text := False;
                     exit Show_Current_Story_Loop;
                  end if;
               end loop Show_Current_Story_Loop;
            else
               Finish_Story;
            end if;
         end Progress_Story_Block;
      else
         Show_Info
           (Text => To_String(Source => Step.Fail_Text), Title => "Story");
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
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- DeliverMedicines type
   -- If argument type is free, deliver medicines for free, otherwise deliver
   -- medicines for a price
   -- SOURCE
   function Deliver_Medicines_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Deliver_Medicines_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Base_Index: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Event_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index;
      Item_Index: constant Natural :=
        Find_Item
          (Inventory => Player_Ship.Cargo,
           Item_Type =>
             Get_Faction(Index => Sky_Bases(Base_Index).Owner).Healing_Tools);
      New_Time: constant Integer :=
        Events_List(Event_Index).Time -
        Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => Item_Index)
          .Amount;
   begin
      if New_Time < 1 then
         Delete_Event(Event_Index => Event_Index);
      else
         Events_List(Event_Index).Time := New_Time;
      end if;
      if CArgv.Arg(Argv => Argv, N => 1) = "free" then
         Gain_Rep
           (Base_Index => Base_Index,
            Points =>
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => Item_Index)
                 .Amount /
               10));
         Add_Message
           (Message =>
              "You gave " &
              To_String
                (Source =>
                   Get_Proto_Item
                     (Index =>
                        Inventory_Container.Element
                          (Container => Player_Ship.Cargo, Index => Item_Index)
                          .Proto_Index)
                     .Name) &
              " for free to base.",
            M_Type => TRADEMESSAGE);
         Update_Cargo
           (Ship => Player_Ship,
            Proto_Index =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Item_Index)
                .Proto_Index,
            Amount =>
              (0 -
               Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => Item_Index)
                 .Amount));
      else
         Sell_Medicines_Block :
         begin
            Gain_Rep
              (Base_Index => Base_Index,
               Points =>
                 ((Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Item_Index)
                     .Amount /
                   20) *
                  (-1)));
            Sell_Items
              (Item_Index => Item_Index,
               Amount =>
                 Integer'Image
                   (Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => Item_Index)
                      .Amount));
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
         end Sell_Medicines_Block;
      end if;
      Update_Header;
      Update_Messages;
      Show_Sky_Map;
      return TCL_OK;
   end Deliver_Medicines_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowOrders", Ada_Command => Show_Orders_Command'Access);
      Add_Command(Name => "Docking", Ada_Command => Docking_Command'Access);
      Add_Command
        (Name => "AskForBases", Ada_Command => Ask_For_Bases_Command'Access);
      Add_Command
        (Name => "AskForEvents", Ada_Command => Ask_For_Events_Command'Access);
      Add_Command(Name => "Attack", Ada_Command => Attack_Command'Access);
      Add_Command(Name => "Pray", Ada_Command => Pray_Command'Access);
      Add_Command
        (Name => "SetAsHome", Ada_Command => Set_As_Home_Command'Access);
      Add_Command
        (Name => "ShowTrader", Ada_Command => Show_Trader_Command'Access);
      Add_Command
        (Name => "StartMission", Ada_Command => Start_Mission_Command'Access);
      Add_Command
        (Name => "CompleteMission",
         Ada_Command => Complete_Mission_Command'Access);
      Add_Command
        (Name => "ExecuteStory", Ada_Command => Execute_Story_Command'Access);
      Add_Command
        (Name => "DeliverMedicines",
         Ada_Command => Deliver_Medicines_Command'Access);
   end Add_Commands;

end OrdersMenu;
