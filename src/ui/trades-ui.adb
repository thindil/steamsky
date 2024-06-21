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

with Ada.Characters.Latin_1;
-- with Ada.Exceptions;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
-- with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Bases.Cargo; use Bases.Cargo;
with BasesTypes;
with CoreUI;
with Crew;
with Dialogs; use Dialogs;
with Game; use Game;
with Maps; use Maps;
-- with Maps.UI;
with Ships.Cargo;
with Ships.Crew;
with Utils.UI; use Utils.UI;

package body Trades.UI is

   -- ****o* TUI/TUI.Show_Trade_Command
   -- FUNCTION
   -- Show information about trading
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowTrade ?itemtype? ?searchstring?
   -- Itemtype is type of items to show, searchstring is string which is
   -- looking for in items names
   -- SOURCE
   function Show_Trade_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showTradeCommand";
      -- ****

   -- ****if* TUI/TUI.Item_Index
   -- FUNCTION
   -- Index of the currently selected item
   -- SOURCE
   Item_Index: Integer;
   -- ****

   -- ****o* TUI/TUI.Show_Trade_Item_Info_Command
   -- FUNCTION
   -- Show information about the selected item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowTradeItemInfo itemindex
   -- ItemIndex is the index of the item which menu will be show. If index
   -- starts with minus means item in base/trader cargo only. Otherwise it is
   -- index in the player ship cargo.
   -- SOURCE
   function Show_Trade_Item_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Trade_Item_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Ada.Characters.Latin_1;
      use BasesTypes;
      use Crew;
      use Ships.Cargo;
      use Ships.Crew;
      use Short_String;
      use Tiny_String;

      Item_Info: Unbounded_String := Null_Unbounded_String;
      Proto_Index: Natural;
      Cargo_Index, Base_Cargo_Index, Price: Natural := 0;
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Item_Types: constant array(1 .. 6) of Tiny_String.Bounded_String :=
        (1 => Weapon_Type, 2 => Chest_Armor, 3 => Head_Armor, 4 => Arms_Armor,
         5 => Legs_Armor, 6 => Shield_Type);
      Max_Sell_Amount, Max_Buy_Amount: Integer := 0;
      Money_Index_2: constant Natural :=
        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      Base_Type: Tiny_String.Bounded_String := Tiny_String.Null_Bounded_String;
   begin
      Item_Index := Integer'Value(CArgv.Arg(Argv => Argv, N => 1));
      if Item_Index < 0 then
         Base_Cargo_Index := abs Item_Index;
      else
         Cargo_Index := Item_Index;
      end if;
      if Cargo_Index >
        Natural
          (Inventory_Container.Length(Container => Player_Ship.Cargo)) then
         return TCL_OK;
      end if;
      if Base_Index = 0 and
        Base_Cargo_Index >
          Natural(BaseCargo_Container.Length(Container => Trader_Cargo)) then
         return TCL_OK;
      elsif Base_Index > 0
        and then Base_Cargo_Index >
          Natural
            (BaseCargo_Container.Length
               (Container => Sky_Bases(Base_Index).Cargo)) then
         return TCL_OK;
      end if;
      if Cargo_Index > 0 then
         Proto_Index :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Cargo_Index)
             .Proto_Index;
      else
         Proto_Index :=
           (if Base_Index = 0 then
              BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => Base_Cargo_Index)
                .Proto_Index
            else BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Base_Cargo_Index)
                .Proto_Index);
      end if;
      if Get_Proto_Item(Index => Proto_Index).I_Type = Weapon_Type then
         Append
           (Source => Item_Info,
            New_Item =>
              "Skill: {gold}" &
              To_String
                (Source =>
                   SkillsData_Container.Element
                     (Container => Skills_List,
                      Index =>
                        Skills_Amount_Range
                          (Get_Proto_Item(Index => Proto_Index).Value(3)))
                     .Name) &
              "/" &
              To_String
                (Source =>
                   AttributesData_Container.Element
                     (Container => Attributes_List,
                      Index =>
                        SkillsData_Container.Element
                          (Container => Skills_List,
                           Index =>
                             Skills_Amount_Range
                               (Get_Proto_Item(Index => Proto_Index).Value(3)))
                          .Attribute)
                     .Name) &
              (if Get_Proto_Item(Index => Proto_Index).Value(4) = 1 then
                 LF & "Can be used with shield."
               else LF & "Can't be used with shield (two-handed weapon).") &
              LF & "{/gold}Damage type: {gold}");
         case Get_Proto_Item(Index => Proto_Index).Value(5) is
            when 1 =>
               Append(Source => Item_Info, New_Item => "cutting");
            when 2 =>
               Append(Source => Item_Info, New_Item => "impaling");
            when 3 =>
               Append(Source => Item_Info, New_Item => "blunt");
            when others =>
               null;
         end case;
         Append(Source => Item_Info, New_Item => "{/gold}");
      end if;
      Show_More_Info_Loop :
      for ItemType of Item_Types loop
         if Get_Proto_Item(Index => Proto_Index).I_Type = ItemType then
            if Item_Info /= Null_Unbounded_String then
               Append(Source => Item_Info, New_Item => LF);
            end if;
            Append
              (Source => Item_Info,
               New_Item =>
                 "Damage chance: {gold}" &
                 Get_Item_Chance_To_Damage
                   (Item_Data =>
                      Get_Proto_Item(Index => Proto_Index).Value(1)) &
                 LF & "{/gold}Strength:{gold}" &
                 Integer'Image(Get_Proto_Item(Index => Proto_Index).Value(2)) &
                 "{/gold}");
            exit Show_More_Info_Loop;
         end if;
      end loop Show_More_Info_Loop;
      if Is_Tool(Item_Type => Get_Proto_Item(Index => Proto_Index).I_Type) then
         if Item_Info /= Null_Unbounded_String then
            Append(Source => Item_Info, New_Item => LF);
         end if;
         Append
           (Source => Item_Info,
            New_Item =>
              "Damage chance: {gold}" &
              Get_Item_Chance_To_Damage
                (Item_Data => Get_Proto_Item(Index => Proto_Index).Value(1)) &
              "{/gold}");
      end if;
      if Length(Source => Get_Proto_Item(Index => Proto_Index).I_Type) > 4
        and then
        (Slice
           (Source => Get_Proto_Item(Index => Proto_Index).I_Type, Low => 1,
            High => 4) =
         "Ammo" or
         Get_Proto_Item(Index => Proto_Index).I_Type =
           To_Bounded_String(Source => "Harpoon")) then
         if Item_Info /= Null_Unbounded_String then
            Append(Source => Item_Info, New_Item => LF);
         end if;
         Append
           (Source => Item_Info,
            New_Item =>
              "Strength:{gold}" &
              Integer'Image(Get_Proto_Item(Index => Proto_Index).Value(1)) &
              "{/gold}");
      end if;
      if Get_Proto_Item(Index => Proto_Index).Description /=
        Short_String.Null_Bounded_String then
         if Item_Info /= Null_Unbounded_String then
            Append(Source => Item_Info, New_Item => LF & LF);
         end if;
         Append
           (Source => Item_Info,
            New_Item =>
              To_String
                (Source => Get_Proto_Item(Index => Proto_Index).Description));
      end if;
      Base_Type :=
        (if Base_Index > 0 then Sky_Bases(Base_Index).Base_Type
         else To_Bounded_String(Source => "0"));
      if Item_Index > 0 then
         Base_Cargo_Index :=
           Find_Base_Cargo
             (Proto_Index => Proto_Index,
              Durability =>
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => Cargo_Index)
                  .Durability);
         if Base_Cargo_Index > 0 then
            Price :=
              (if Base_Index > 0 then
                 BaseCargo_Container.Element
                   (Container => Sky_Bases(Base_Index).Cargo,
                    Index => Base_Cargo_Index)
                   .Price
               else BaseCargo_Container.Element
                   (Container => Trader_Cargo, Index => Base_Cargo_Index)
                   .Price);
         else
            Price :=
              Get_Price(Base_Type => Base_Type, Item_Index => Proto_Index);
         end if;
      else
         Item_Index :=
           Find_Item
             (Inventory => Player_Ship.Cargo, Proto_Index => Proto_Index,
              Durability =>
                (if Base_Index > 0 then
                   BaseCargo_Container.Element
                     (Container => Sky_Bases(Base_Index).Cargo,
                      Index => Base_Cargo_Index)
                     .Durability
                 else BaseCargo_Container.Element
                     (Container => Trader_Cargo, Index => Base_Cargo_Index)
                     .Durability));
         Price :=
           (if Base_Index > 0 then
              BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Base_Cargo_Index)
                .Price
            else BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => Base_Cargo_Index)
                .Price);
      end if;
      if Item_Index > 0 then
         Max_Sell_Amount :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Item_Index)
             .Amount;
         Count_Sell_Amount_Block :
         declare
            Max_Price: Natural := Max_Sell_Amount * Price;
            Weight: Integer;
         begin
            Count_Price
              (Price => Max_Price, Trader_Index => Find_Member(Order => TALK),
               Reduce => False);
            if Base_Index > 0
              and then Max_Price >
                BaseCargo_Container.Element
                  (Container => Sky_Bases(Base_Index).Cargo, Index => 1)
                  .Amount then
               Max_Sell_Amount :=
                 Natural
                   (Float'Floor
                      (Float(Max_Sell_Amount) *
                       (Float
                          (BaseCargo_Container.Element
                             (Container => Sky_Bases(Base_Index).Cargo,
                              Index => 1)
                             .Amount) /
                        Float(Max_Price))));
            elsif Base_Index = 0
              and then Max_Price >
                BaseCargo_Container.Element
                  (Container => Trader_Cargo, Index => 1)
                  .Amount then
               Max_Sell_Amount :=
                 Natural
                   (Float'Floor
                      (Float(Max_Sell_Amount) *
                       (Float
                          (BaseCargo_Container.Element
                             (Container => Trader_Cargo, Index => 1)
                             .Amount) /
                        Float(Max_Price))));
            end if;
            Max_Price := Max_Sell_Amount * Price;
            if Max_Price > 0 then
               Count_Price
                 (Price => Max_Price,
                  Trader_Index => Find_Member(Order => TALK), Reduce => False);
            end if;
            --## rule off SIMPLIFIABLE_EXPRESSIONS
            Weight :=
              Free_Cargo
                (Amount =>
                   (Get_Proto_Item(Index => Proto_Index).Weight *
                    Max_Sell_Amount) -
                   Max_Price);
            --## rule on SIMPLIFIABLE_EXPRESSIONS
            Count_Sell_Amount_Loop :
            while Weight < 0 loop
               Max_Sell_Amount :=
                 Integer
                   (Float'Floor
                      (Float(Max_Sell_Amount) *
                       (Float(Max_Price + Weight) / Float(Max_Price))));
               exit Count_Sell_Amount_Loop when Max_Sell_Amount < 1;
               Max_Price := Max_Sell_Amount * Price;
               Count_Price
                 (Price => Max_Price,
                  Trader_Index => Find_Member(Order => TALK), Reduce => False);
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Weight :=
                 Free_Cargo
                   (Amount =>
                      (Get_Proto_Item(Index => Proto_Index).Weight *
                       Max_Sell_Amount) -
                      Max_Price);
               --## rule on SIMPLIFIABLE_EXPRESSIONS
            end loop Count_Sell_Amount_Loop;
         end Count_Sell_Amount_Block;
      end if;
      if Base_Cargo_Index > 0 and Money_Index_2 > 0 and
        ((Base_Index > 0 and
          Is_Buyable(Base_Type => Base_Type, Item_Index => Proto_Index)) or
         Base_Index = 0) then
         Max_Buy_Amount :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Money_Index_2)
             .Amount /
           Price;
         Count_Buy_Amount_Block :
         declare
            Max_Price: Natural := Max_Buy_Amount * Price;
            Weight: Integer := 0;
         begin
            if Max_Buy_Amount > 0 then
               Count_Price
                 (Price => Max_Price,
                  Trader_Index => Find_Member(Order => TALK));
               if Max_Price < Max_Buy_Amount * Price then
                  Max_Buy_Amount :=
                    Natural
                      (Float'Floor
                         (Float(Max_Buy_Amount) *
                          ((Float(Max_Buy_Amount) * Float(Price)) /
                           Float(Max_Price))));
               end if;
               if Base_Index > 0
                 and then Max_Buy_Amount >
                   BaseCargo_Container.Element
                     (Container => Sky_Bases(Base_Index).Cargo,
                      Index => Base_Cargo_Index)
                     .Amount then
                  Max_Buy_Amount :=
                    BaseCargo_Container.Element
                      (Container => Sky_Bases(Base_Index).Cargo,
                       Index => Base_Cargo_Index)
                      .Amount;
               elsif Base_Index = 0
                 and then Max_Buy_Amount >
                   BaseCargo_Container.Element
                     (Container => Trader_Cargo, Index => Base_Cargo_Index)
                     .Amount then
                  Max_Buy_Amount :=
                    BaseCargo_Container.Element
                      (Container => Trader_Cargo, Index => Base_Cargo_Index)
                      .Amount;
               end if;
               Max_Price := Max_Buy_Amount * Price;
               Count_Price
                 (Price => Max_Price,
                  Trader_Index => Find_Member(Order => TALK));
               --## rule off SIMPLIFIABLE_EXPRESSIONS
               Weight :=
                 Free_Cargo
                   (Amount =>
                      Max_Price -
                      (Get_Proto_Item(Index => Proto_Index).Weight *
                       Max_Buy_Amount));
               --## rule on SIMPLIFIABLE_EXPRESSIONS
               Count_Buy_Amount_Loop :
               while Weight < 0 loop
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Max_Buy_Amount :=
                    Max_Buy_Amount +
                    (Weight / Get_Proto_Item(Index => Proto_Index).Weight) - 1;
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
                  if Max_Buy_Amount < 0 then
                     Max_Buy_Amount := 0;
                  end if;
                  exit Count_Buy_Amount_Loop when Max_Buy_Amount = 0;
                  Max_Price := Max_Buy_Amount * Price;
                  Count_Price
                    (Price => Max_Price,
                     Trader_Index => Find_Member(Order => TALK));
                  --## rule off SIMPLIFIABLE_EXPRESSIONS
                  Weight :=
                    Free_Cargo
                      (Amount =>
                         Max_Price -
                         (Get_Proto_Item(Index => Proto_Index).Weight *
                          Max_Buy_Amount));
                  --## rule on SIMPLIFIABLE_EXPRESSIONS
               end loop Count_Buy_Amount_Loop;
            end if;
         end Count_Buy_Amount_Block;
         if Item_Index = 0 then
            Item_Index := -Base_Cargo_Index;
         end if;
      end if;
      Show_Info
        (Text => To_String(Source => Item_Info),
         Title =>
           To_String(Source => Get_Proto_Item(Index => Proto_Index).Name),
         Button_1 =>
           (if Max_Buy_Amount = 0 then Empty_Button_Settings
            else
              (Tooltip =>
                 To_Unbounded_String(Source => "Buy item from the base"),
               Command =>
                 To_Unbounded_String
                   (Source =>
                      "TradeAmount buy" & Natural'Image(Max_Buy_Amount) &
                      Natural'Image(Price)),
               Icon => To_Unbounded_String(Source => "buy2icon"),
               Text => To_Unbounded_String(Source => "Buy"),
               Color => Null_Unbounded_String)),
         Button_2 =>
           (if Max_Sell_Amount = 0 then Empty_Button_Settings
            else
              (Tooltip =>
                 To_Unbounded_String
                   (Source => "Sell item from the ship cargo"),
               Command =>
                 To_Unbounded_String
                   (Source =>
                      "TradeAmount sell" & Natural'Image(Max_Sell_Amount) &
                      Natural'Image(Price)),
               Icon => To_Unbounded_String(Source => "sell2icon"),
               Text => To_Unbounded_String(Source => "Sell"),
               Color => Null_Unbounded_String)));
      return TCL_OK;
   end Show_Trade_Item_Info_Command;

   -- ****o* TUI/TUI.Search_Trade_Command
   -- FUNCTION
   -- Show only this items which contains the selected sequence
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SearchTrade
   -- SOURCE
   function Search_Trade_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Trade_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use CoreUI;

      Type_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => Main_Paned & ".tradeframe.canvas.trade.options.type",
           Interp => Interp);
      Search_Text: constant String := CArgv.Arg(Argv => Argv, N => 1);
   begin
      if Search_Text'Length = 0 then
         return
           Show_Trade_Command
             (Client_Data => Client_Data, Interp => Interp, Argc => 2,
              Argv => CArgv.Empty & "ShowTrade" & Get(Widgt => Type_Box));
      end if;
      return
        Show_Trade_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 3,
           Argv =>
             CArgv.Empty & "ShowTrade" & Get(Widgt => Type_Box) & Search_Text);
   end Search_Trade_Command;

   -- ****o* TUI/TUI.Trade_Amount_Command
   -- FUNCTION
   -- Show dialog to enter amount of items to sell or buy
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- TradeAmount action baseindex
   -- Action which will be taken. Can be buy or sell. BaseIndex is the index
   -- of the base from which item will be bought. If zero it mean buying from
   -- trader ship.
   -- SOURCE
   function Trade_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Trade_Amount_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
   begin
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      if CArgv.Arg(Argv => Argv, N => 1) = "sell" then
         Show_Manipulate_Item
           (Title =>
              "Sell " &
              Get_Item_Name
                (Item =>
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Item_Index)),
            Command => "TradeItem sell", Action => "sell",
            Item_Index => Item_Index,
            Max_Amount => Natural'Value(CArgv.Arg(Argv => Argv, N => 2)),
            Cost => Natural'Value(CArgv.Arg(Argv => Argv, N => 3)));
      else
         if Item_Index > 0 then
            Show_Manipulate_Item
              (Title =>
                 "Buy " &
                 Get_Item_Name
                   (Item =>
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => Item_Index)),
               Command => "TradeItem buy", Action => "buy",
               Item_Index => Item_Index,
               Max_Amount => Natural'Value(CArgv.Arg(Argv => Argv, N => 2)),
               Cost => Natural'Value(CArgv.Arg(Argv => Argv, N => 3)));
         else
            if Base_Index > 0 then
               Show_Manipulate_Item
                 (Title =>
                    "Buy " &
                    To_String
                      (Source =>
                         Get_Proto_Item
                           (Index =>
                              BaseCargo_Container.Element
                                (Container => Sky_Bases(Base_Index).Cargo,
                                 Index => abs Item_Index)
                                .Proto_Index)
                           .Name),
                  Command => "TradeItem buy", Action => "buy",
                  Item_Index => abs Item_Index,
                  Max_Amount => Natural'Value(CArgv.Arg(Argv => Argv, N => 2)),
                  Cost => Natural'Value(CArgv.Arg(Argv => Argv, N => 3)));
            else
               Show_Manipulate_Item
                 (Title =>
                    "Buy " &
                    To_String
                      (Source =>
                         Get_Proto_Item
                           (Index =>
                              BaseCargo_Container.Element
                                (Container => Trader_Cargo,
                                 Index => abs Item_Index)
                                .Proto_Index)
                           .Name),
                  Command => "TradeItem buy", Action => "buy",
                  Item_Index => abs Item_Index,
                  Max_Amount => Natural'Value(CArgv.Arg(Argv => Argv, N => 2)),
                  Cost => Natural'Value(CArgv.Arg(Argv => Argv, N => 3)));
            end if;
         end if;
      end if;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      return TCL_OK;
   end Trade_Amount_Command;

   -- ****o* TUI/TUI.Sort_Items_Command
   -- FUNCTION
   -- Sort the trading list
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortTradeItems x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Items_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "sortTradeItemsCommand";
      -- ****

   -- ****o* TUI/TUI.Trade_Item_Command
   -- FUNCTION
   -- Buy or sell the selected item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- TradeItem tradetype
   -- Tradetype is type of trade action. Can be buy, buymax, sell, sellmax
   -- SOURCE
   function Trade_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Trade_Item_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      use Ada.Exceptions;
--      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
--      use Maps.UI;
--
--      Base_Index: constant Natural :=
--        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
--      Base_Cargo_Index, Cargo_Index: Natural := 0;
--      Trader: String(1 .. 4);
--      Proto_Index: Natural;
--      Amount_Box: constant Ttk_SpinBox :=
--        Get_Widget(pathName => ".itemdialog.amount", Interp => Interp);
--      Type_Box: constant Ttk_ComboBox :=
--        Get_Widget
--          (pathName =>
--             ".gameframe.paned.tradeframe.canvas.trade.options.type");
      function Trade_Ada_Item_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
        Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
        Import => True,
        Convention => C,
        External_Name => "tradeItemCommand";
      procedure Get_Ada_Trade_Item_Index(I_Index: Integer) with
        Import => True,
        Convention => C,
        External_Name => "getTradeItemIndex";
   begin
      Get_Ada_Trade_Item_Index(I_Index => Item_Index);
      return Trade_Ada_Item_Command(C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv);
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
--      if Item_Index < 0 then
--         Base_Cargo_Index := abs Item_Index;
--      else
--         Cargo_Index := Item_Index;
--      end if;
--      --## rule on DIRECTLY_ACCESSED_GLOBALS
--      if Cargo_Index > 0 then
--         Proto_Index :=
--           Inventory_Container.Element
--             (Container => Player_Ship.Cargo, Index => Cargo_Index)
--             .Proto_Index;
--         if Base_Cargo_Index = 0 then
--            Base_Cargo_Index := Find_Base_Cargo(Proto_Index => Proto_Index);
--         end if;
--      else
--         Proto_Index :=
--           (if Base_Index = 0 then
--              BaseCargo_Container.Element
--                (Container => Trader_Cargo, Index => Base_Cargo_Index)
--                .Proto_Index
--            else BaseCargo_Container.Element
--                (Container => Sky_Bases(Base_Index).Cargo,
--                 Index => Base_Cargo_Index)
--                .Proto_Index);
--      end if;
--      Trader := (if Base_Index > 0 then "base" else "ship");
--      if Argc > 2 then
--         if CArgv.Arg(Argv => Argv, N => 1) in "buy" then
--            Buy_Items
--              (Base_Item_Index => Base_Cargo_Index,
--               Amount => CArgv.Arg(Argv => Argv, N => 2));
--         else
--            Sell_Items
--              (Item_Index => Cargo_Index,
--               Amount => CArgv.Arg(Argv => Argv, N => 2));
--         end if;
--      else
--         if CArgv.Arg(Argv => Argv, N => 1) in "buy" then
--            Buy_Items
--              (Base_Item_Index => Base_Cargo_Index,
--               Amount => Get(Widgt => Amount_Box));
--         else
--            Sell_Items
--              (Item_Index => Cargo_Index, Amount => Get(Widgt => Amount_Box));
--         end if;
--         if Close_Dialog_Command
--             (Client_Data => Client_Data, Interp => Interp, Argc => 2,
--              Argv => CArgv.Empty & "CloseDialog" & ".itemdialog") =
--           TCL_ERROR then
--            return TCL_ERROR;
--         end if;
--      end if;
--      Update_Header;
--      Update_Messages;
--      Unbind(Widgt => Type_Box, Sequence => "<<ComboBoxSelected>>");
--      Current(ComboBox => Type_Box, NewIndex => "0");
--      Bind
--        (Widgt => Type_Box, Sequence => "<<ComboBoxSelected>>",
--         Script => "{ShowTrade [" & Type_Box & " get]}");
--      --## rule off DIRECTLY_ACCESSED_GLOBALS
----      if Items_Sort_Order /= Default_Items_Sort_Order then
----         return
----           Sort_Items_Command
----             (Client_Data => Client_Data, Interp => Interp, Argc => 2,
----              Argv => CArgv.Empty & "SortTradeItem" & "-1");
----      end if;
--      --## rule on DIRECTLY_ACCESSED_GLOBALS
--      return
--        Show_Trade_Command
--          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
--           Argv => CArgv.Empty & "ShowTrade" & "All");
--   exception
--      when An_Exception : Trade_Cant_Buy =>
--         Show_Message
--           (Text =>
--              "You can't buy " & Exception_Message(X => An_Exception) &
--              " in this " & Trader & ".",
--            Title => "Can't buy items");
--         return TCL_OK;
--      when An_Exception : Trade_Not_For_Sale_Now =>
--         Show_Message
--           (Text =>
--              "You can't buy " & Exception_Message(X => An_Exception) &
--              " in this base at this moment.",
--            Title => "Can't buy items");
--         return TCL_OK;
--      when An_Exception : Trade_Buying_Too_Much =>
--         Show_Message
--           (Text =>
--              Trader & " don't have that much " &
--              Exception_Message(X => An_Exception) & " for sale.",
--            Title => "Not enough items");
--         return TCL_OK;
--      when Trade_No_Free_Cargo =>
--         Show_Message
--           (Text => "You don't have that much free space in your ship cargo.",
--            Title => "No free cargo space");
--         return TCL_OK;
--      when An_Exception : Trade_No_Money =>
--         Show_Message
--           (Text =>
--              "You don't have any " & To_String(Source => Money_Name) &
--              " to buy " & Exception_Message(X => An_Exception) & ".",
--            Title => "No money to buy items");
--         return TCL_OK;
--      when An_Exception : Trade_Not_Enough_Money =>
--         Show_Message
--           (Text =>
--              "You don't have enough " & To_String(Source => Money_Name) &
--              " to buy so much " & Exception_Message(X => An_Exception) & ".",
--            Title => "Not enough money to buy items");
--         return TCL_OK;
--      when Trade_Invalid_Amount =>
--         if CArgv.Arg(Argv => Argv, N => 1) = "buy" then
--            Show_Message
--              (Text => "You entered invalid amount to buy.",
--               Title => "Invalid amount of items");
--         else
--            Show_Message
--              (Text => "You entered invalid amount to sell.",
--               Title => "Invalid amount of items");
--         end if;
--         return TCL_OK;
--      when An_Exception : Trade_Too_Much_For_Sale =>
--         Show_Message
--           (Text =>
--              "You dont have that much " &
--              Exception_Message(X => An_Exception) & " in ship cargo.",
--            Title => "Not enough items for sale");
--         return TCL_OK;
--      when An_Exception : Trade_No_Money_In_Base =>
--         Show_Message
--           (Text =>
--              "You can't sell so much " &
--              Exception_Message(X => An_Exception) & " because " & Trader &
--              " don't have that much " & To_String(Source => Money_Name) &
--              " to buy it.",
--            Title => "Too much items for sale");
--         return TCL_OK;
--      when Trade_No_Trader =>
--         Show_Message
--           (Text =>
--              "You don't have assigned anyone in crew to talk in bases duty.",
--            Title => "No trader assigned");
--         return TCL_OK;
   end Trade_Item_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowTrade", Ada_Command => Show_Trade_Command'Access);
      Add_Command
        (Name => "ShowTradeItemInfo",
         Ada_Command => Show_Trade_Item_Info_Command'Access);
      Add_Command
        (Name => "TradeItem", Ada_Command => Trade_Item_Command'Access);
      Add_Command
        (Name => "SearchTrade", Ada_Command => Search_Trade_Command'Access);
      Add_Command
        (Name => "TradeAmount", Ada_Command => Trade_Amount_Command'Access);
      Add_Command
        (Name => "SortTradeItems", Ada_Command => Sort_Items_Command'Access);
   end Add_Commands;

end Trades.UI;
