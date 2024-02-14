-- Copyright (c) 2020-2024 Bartek thindil Jasicki
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

with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.String_Split;
with Tcl.Ada;
with Bases; use Bases;
with Combat; use Combat;
with Combat.UI; use Combat.UI;
with Crew;
with Dialogs; use Dialogs;
with Events; use Events;
with Factions;
with Game; use Game;
with Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Missions; use Missions;
with Ships; use Ships;
with Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement;
with Stories;
with Trades; use Trades;
with Utils;
with Utils.UI; use Utils.UI;
with Interfaces.C;
with CArgv;
with Tcl; use Tcl;

package body OrdersMenu is

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
            Amount => 10);
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
      use Crew;

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
      use Tcl.Ada;

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
      use Utils;

      Starts_Combat, U_Mission: Boolean := False;
      Mission: Mission_Data := Empty_Mission;
   begin
      Check_Missions_Loop :
      for I in 1 .. Get_Accepted_Missions_Amount loop
         Mission := Get_Accepted_Mission(Mission_Index => I);
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
                            Get_Accepted_Mission
                              (Mission_Index =>
                                 Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                                   .Mission_Index)
                              .Ship_Index,
                          New_Combat => False);
                  end if;
               when PATROL =>
                  Update_Game(Minutes => Get_Random(Min => 45, Max => 75));
                  Starts_Combat := Check_For_Event;
                  if not Starts_Combat then
                     U_Mission := True;
                  end if;
               when EXPLORE =>
                  Update_Game(Minutes => Get_Random(Min => 30, Max => 60));
                  Starts_Combat := Check_For_Event;
                  if not Starts_Combat then
                     U_Mission := True;
                  end if;
            end case;
            exit Check_Missions_Loop;
         end if;
      end loop Check_Missions_Loop;
      if Starts_Combat then
         Show_Combat_Ui;
         return TCL_OK;
      end if;
      if U_Mission then
         Update_Mission
           (Mission_Index =>
              Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Mission_Index);
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
      use Ships.Movement;
      use Stories;

      Step: Step_Data :=
        (if Get_Current_Story.Current_Step = 0 then
           Get_Story(Index => Get_Current_Story.Index).Starting_Step
         elsif Get_Current_Story.Current_Step > 0 then
           Get_Story(Index => Get_Current_Story.Index).Steps
             (Get_Current_Story.Current_Step)
         else Get_Story(Index => Get_Current_Story.Index).Final_Step);
      Message: Unbounded_String := Null_Unbounded_String;
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
            use GNAT.String_Split;

            Tokens: Slice_Set;
         begin
            Create
              (S => Tokens,
               From => To_String(Source => Get_Current_Story.Data),
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
            if Get_Current_Story.Current_Step > -2 then
               Step :=
                 (if Get_Current_Story.Current_Step > 0 then
                    Get_Story(Index => Get_Current_Story.Index).Steps
                      (Get_Current_Story.Current_Step)
                  else Get_Story(Index => Get_Current_Story.Index).Final_Step);
               Show_Current_Story_Loop :
               for Text of Step.Texts loop
                  if Get_Current_Story.Finished_Step = Text.Condition then
                     Show_Info
                       (Text => To_String(Source => Text.Text),
                        Title => "Story");
                     Set_Story_Show_Text;
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
         Set_Story_Show_Text;
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
      use Factions;
      use Items;
      use Ships.Cargo;
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
      Event: constant Event_Data := Get_Event(Index => Event_Index);
      New_Time: constant Integer :=
        Event.Time -
        Inventory_Container.Element
          (Container => Player_Ship.Cargo, Index => Item_Index)
          .Amount;
   begin
      if New_Time < 1 then
         Delete_Event(Event_Index => Event_Index);
      else
         Get_Ada_Event
           (Index => Event_Index, X => Event.Sky_X, Y => Event.Sky_Y,
            Time => Event.Time, E_Type => Events_Types'Pos(Event.E_Type),
            Data => Event.Data);
      end if;
      if CArgv.Arg(Argv => Argv, N => 1) = "free" then
         Gain_Rep
           (Base_Index => Base_Index,
            Points =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Item_Index)
                .Amount /
              10);
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
         --## rule off SIMPLIFIABLE_EXPRESSIONS
         Update_Cargo
           (Ship => Player_Ship,
            Proto_Index =>
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => Item_Index)
                .Proto_Index,
            Amount =>
              -(Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => Item_Index)
                 .Amount));
         --## rule on SIMPLIFIABLE_EXPRESSIONS
      else
         Sell_Medicines_Block :
         begin
            --## rule off SIMPLIFIABLE_EXPRESSIONS
            Gain_Rep
              (Base_Index => Base_Index,
               Points =>
                 ((Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Item_Index)
                     .Amount /
                   20) *
                  (-1)));
            --## rule on SIMPLIFIABLE_EXPRESSIONS
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
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaOrdersMenuCommands";
   begin
      Add_Ada_Commands;
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
