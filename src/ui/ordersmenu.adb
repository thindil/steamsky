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
with Ada.Strings.Unbounded;
with GNAT.String_Split;
with Bases;
with Combat;
with Combat.UI;
with Dialogs; use Dialogs;
with Events;
with Factions;
with Game;
with Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages;
with Ships; use Ships;
with Ships.Cargo;
with Ships.Movement;
with Stories;
with Trades;
with Utils;
with Utils.UI; use Utils.UI;
with Interfaces.C;
with CArgv;
with Tcl; use Tcl;

package body OrdersMenu is

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
      use Ada.Strings.Unbounded;
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
            use Combat;
            use Combat.UI;

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
      use Bases;
      use Events;
      use Factions;
      use Items;
      use Messages;
      use Ships.Cargo;
      use Game.Tiny_String;

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
         declare
            use Trades;
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
      Add_Command
        (Name => "ExecuteStory", Ada_Command => Execute_Story_Command'Access);
      Add_Command
        (Name => "DeliverMedicines",
         Ada_Command => Deliver_Medicines_Command'Access);
   end Add_Commands;

end OrdersMenu;
