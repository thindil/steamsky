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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases.Cargo; use Bases.Cargo;
with BasesTypes; use BasesTypes;
with Config; use Config;
with CoreUI; use CoreUI;
with Crew; use Crew;
with Dialogs; use Dialogs;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Trades.UI is

   -- ****iv* TUI/TUI.Trade_Table
   -- FUNCTION
   -- Table with info about the available items to trade
   -- SOURCE
   Trade_Table: Table_Widget (Amount => 8);
   -- ****

   -- ****iv* TUI/TUI.Items_Indexes
   -- FUNCTION
   -- Indexes of the items for trade
   -- SOURCE
   Items_Indexes: Natural_Container.Vector;
   -- ****

   -- ****it* TUI/TUI.Items_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the trading list
   -- OPTIONS
   -- NAMEASC        - Sort items by name ascending
   -- NAMEDESC       - Sort items by name descending
   -- TYPEASC        - Sort items by type ascending
   -- TYPEDESC       - Sort items by type descending
   -- DURABILITYASC  - Sort items by durability ascending
   -- DURABILITYDESC - Sort items by durability descending
   -- PRICEASC       - Sort items by price ascending
   -- PRICEDESC      - Sort items by price descending
   -- PROFITASC      - Sort items by profit ascending
   -- PROFITDESC     - Sort items by profit descending
   -- WEIGHTASC      - Sort items by weight ascending
   -- WEIGHTDESC     - Sort items by weight descending
   -- OWNEDASC       - Sort items by owned amount ascending
   -- OWNEDDESC      - Sort items by owned amount descending
   -- AVAILABLEASC   - Sort items by available amount ascending
   -- AVAILABLEDESC  - Sort items by available amount descending
   -- NONE           - No sorting modules (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Items_Sort_Orders is
     (NAMEASC, NAMEDESC, TYPEASC, TYPEDESC, DURABILITYASC, DURABILITYDESC,
      PRICEASC, PRICEDESC, PROFITASC, PROFITDESC, WEIGHTASC, WEIGHTDESC,
      OWNEDASC, OWNEDDESC, AVAILABLEASC, AVAILABLEDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* TUI/TUI.Default_Items_Sort_Order
      -- FUNCTION
      -- Default sorting order for the trading list
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Items_Sort_Order: constant Items_Sort_Orders := NONE;
   -- ****

   -- ****iv* TUI/TUI.Items_Sort_Order
   -- FUNCTION
   -- The current sorting order for the trading list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Items_Sort_Order: Items_Sort_Orders := Default_Items_Sort_Order;
   -- ****

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
      Convention => C;
      -- ****

   function Show_Trade_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
      use Tiny_String;

      Trade_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".tradeframe", Interp => Interp);
      Trade_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Trade_Frame & ".canvas", Interp => Interp);
      Label: Ttk_Label :=
        Get_Widget
          (pathName => Trade_Canvas & ".trade.options.typelabel",
           Interp => Interp);
      Item_Name, Trade_Info, Item_Durability: Unbounded_String;
      Proto_Index: Objects_Container.Extended_Index;
      Base_Type, Item_Type: Bounded_String;
      Items_Types: Unbounded_String := To_Unbounded_String(Source => "All");
      Price: Positive;
      Combo_Box: Ttk_ComboBox;
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Base_Cargo: BaseCargo_Container.Vector (Capacity => 16);
      Base_Cargo_Index, Base_Amount: Natural;
      Indexes_List: Positive_Container.Vector;
      Event_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index;
      Profit: Integer;
      Money_Index_2: constant Natural :=
        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      Search_Entry: constant Ttk_Entry :=
        Get_Widget
          (pathName => Trade_Canvas & ".trade.options.search",
           Interp => Interp);
      Page: constant Positive :=
        (if Argc = 4 then Positive'Value(CArgv.Arg(Argv => Argv, N => 3))
         else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      Arguments: constant String :=
        (if Argc > 2 then
           "{" & CArgv.Arg(Argv => Argv, N => 1) & "} {" &
           CArgv.Arg(Argv => Argv, N => 2) & "}"
         elsif Argc = 2 then CArgv.Arg(Argv => Argv, N => 1) & " {}"
         else "All {}");
      Current_Item_Index: Positive := 1;
   begin
      if Winfo_Get(Widgt => Label, Info => "exists") = "0" then
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "trade.tcl");
         Bind
           (Widgt => Trade_Frame, Sequence => "<Configure>",
            Script => "{ResizeCanvas %W.canvas %w %h}");
         Trade_Frame := Get_Widget(pathName => Trade_Canvas & ".trade");
         Trade_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Trade_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Type"),
                 3 => To_Unbounded_String(Source => "Durability"),
                 4 => To_Unbounded_String(Source => "Price"),
                 5 => To_Unbounded_String(Source => "Profit"),
                 6 => To_Unbounded_String(Source => "Weight"),
                 7 => To_Unbounded_String(Source => "Owned"),
                 8 => To_Unbounded_String(Source => "Available")),
              Scrollbar =>
                Get_Widget(pathName => Main_Paned & ".tradeframe.scrolly"),
              Command => "SortTradeItems",
              Tooltip => "Press mouse button to sort the items.");
      elsif Winfo_Get(Widgt => Label, Info => "ismapped") = "1" and
        Argc = 1 then
         Items_Sort_Order := Default_Items_Sort_Order;
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         configure(Widgt => Close_Button, options => "-command ShowSkyMap");
         if Base_Index = 0 and Event_Index > 0 then
            Delete_Event(Event_Index => Event_Index);
         end if;
         Show_Sky_Map(Clear => True);
         return TCL_OK;
      end if;
      Tcl_SetVar
        (interp => Interp, varName => "gamestate", newValue => "trade");
      if Argc < 3 then
         Delete
           (TextEntry => Search_Entry, FirstIndex => "0", LastIndex => "end");
      end if;
      configure
        (Widgt => Close_Button, options => "-command {ShowSkyMap ShowTrade}");
      Trade_Frame.Name := New_String(Str => Trade_Canvas & ".trade");
      Combo_Box :=
        Get_Widget
          (pathName => Trade_Frame & ".options.type", Interp => Interp);
      Clear_Table(Table => Trade_Table);
      if Base_Index > 0 then
         Base_Type := Sky_Bases(Base_Index).Base_Type;
         BaseCargo_Container.Assign
           (Target => Base_Cargo, Source => Sky_Bases(Base_Index).Cargo);
      else
         Base_Type := To_Bounded_String(Source => "0");
         BaseCargo_Container.Assign
           (Target => Base_Cargo, Source => Trader_Cargo);
      end if;
      if Items_Sort_Order = Default_Items_Sort_Order then
         Items_Indexes.Clear;
         Fill_Item_Indexes_Loop :
         for I in
           Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
             Inventory_Container.Last_Index
               (Container => Player_Ship.Cargo) loop
            Items_Indexes.Append(New_Item => I);
         end loop Fill_Item_Indexes_Loop;
         Items_Indexes.Append(New_Item => 0);
         Add_Cargo_Indexes_Loop:
         for I in
           BaseCargo_Container.First_Index(Container => Base_Cargo) ..
             BaseCargo_Container.Last_Index(Container => Base_Cargo) loop
            Items_Indexes.Append(New_Item => I);
         end loop Add_Cargo_Indexes_Loop;
      end if;
      Show_Cargo_Items_Loop :
      for I of Items_Indexes loop
         Current_Item_Index := Current_Item_Index + 1;
         exit Show_Cargo_Items_Loop when I = 0;
         if Get_Price
             (Base_Type => Base_Type,
              Item_Index => Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I)
                .Proto_Index) =
           0 then
            goto End_Of_Cargo_Loop;
         end if;
         Proto_Index :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => I)
             .Proto_Index;
         Base_Cargo_Index :=
           Find_Base_Cargo
             (Proto_Index => Proto_Index,
              Durability => Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I)
                .Durability);
         if Base_Cargo_Index > 0 then
            Indexes_List.Append(New_Item => Base_Cargo_Index);
         end if;
         Item_Type :=
           (if
              Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .Show_Type =
              Null_Bounded_String
            then
              Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .I_Type
            else Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .Show_Type);
         if Index(Source => Items_Types, Pattern => To_String(Source => "{" & Item_Type & "}")) = 0 then
            Append(Source => Items_Types, New_Item => " {" & To_String(Source => Item_Type) & "}");
         end if;
         if Argc > 1 and then CArgv.Arg(Argv => Argv, N => 1) /= "All"
           and then To_String(Source => Item_Type) /= CArgv.Arg(Argv => Argv, N => 1) then
            goto End_Of_Cargo_Loop;
         end if;
         Item_Name :=
           To_Unbounded_String
             (Source => Get_Item_Name
                (Item => Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => I),
                 Damage_Info => False, To_Lower => False));
         if Argc = 3
           and then
             Index
               (Source => To_Lower(Item => To_String(Source => Item_Name)), Pattern => To_Lower(Item => CArgv.Arg(Argv => Argv, N => 2))) =
             0 then
            goto End_Of_Cargo_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Cargo_Loop;
         end if;
         if Base_Cargo_Index = 0 then
            Price := Get_Price(Base_Type => Base_Type, Item_Index => Proto_Index);
         else
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
         if Event_Index > 0 then
            if Events_List(Event_Index).E_Type = DOUBLEPRICE
              and then Events_List(Event_Index).Item_Index = Proto_Index then
               Price := Price * 2;
            end if;
         end if;
         Profit :=
           Price -
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => I)
             .Price;
         Base_Amount := 0;
         if Base_Cargo_Index > 0 and Is_Buyable(Base_Type => Base_Type, Item_Index => Proto_Index) then
            Base_Amount :=
              BaseCargo_Container.Element
                (Container => Base_Cargo, Index => Base_Cargo_Index)
                .Amount;
         end if;
         Add_Button
           (Table => Trade_Table, Text => To_String(Source => Item_Name),
            Tooltip => "Show available options for item",
            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 1);
         Add_Button
           (Table => Trade_Table, Text => To_String(Source => Item_Type),
            Tooltip => "Show available options for item",
            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 2);
         Item_Durability :=
           (if
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I)
                .Durability <
              100
            then
              To_Unbounded_String
                (Source => Get_Item_Damage
                   (Item_Durability => Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => I)
                      .Durability))
            else To_Unbounded_String(Source => "Unused"));
         Add_Progress_Bar
           (Table => Trade_Table,
            Value => Inventory_Container.Element
              (Container => Player_Ship.Cargo, Index => I)
              .Durability,
            Max_Value => Default_Item_Durability, Tooltip => To_String(Source => Item_Durability),
            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 3);
         Add_Button
           (Trade_Table, Positive'Image(Price),
            "Show available options for item",
            "ShowTradeItemInfo" & Positive'Image(I), 4);
         Add_Button
           (Table => Trade_Table, Text => Positive'Image(Profit),
            Tooltip => "Show available options for item",
            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 5,
            Color =>
              (if Profit > 0 then "green" elsif Profit < 0 then "red"
               else ""));
         Add_Button
           (Table => Trade_Table,
            Text => Positive'Image
              (Objects_Container.Element
                 (Container => Items_List, Index => Proto_Index)
                 .Weight) &
            " kg",
            Tooltip => "Show available options for item",
            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 6);
         Add_Button
           (Table => Trade_Table,
            Text => Positive'Image
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => I)
                 .Amount),
            Tooltip => "Show available options for item",
            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 7);
         Add_Button
           (Table => Trade_Table, Text => Positive'Image(Base_Amount),
            Tooltip => "Show available options for item",
            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 8, New_Row => True);
         exit Show_Cargo_Items_Loop when Trade_Table.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Cargo_Loop>>
      end loop Show_Cargo_Items_Loop;
      Show_Trader_Items_Loop :
      for I in Current_Item_Index .. Items_Indexes.Last_Index loop
         exit Show_Trader_Items_Loop when Trade_Table.Row =
           Game_Settings.Lists_Limit + 1;
         if Indexes_List.Find_Index(Item => Items_Indexes(I)) > 0 or
           not Is_Buyable
             (Base_Type => Base_Type,
              Item_Index =>
                BaseCargo_Container.Element
                  (Container => Base_Cargo, Index => Items_Indexes(I))
                  .Proto_Index,
              Base_Index => Base_Index) or
           BaseCargo_Container.Element
               (Container => Base_Cargo, Index => Items_Indexes(I))
               .Amount =
             0 then
            goto End_Of_Trader_Loop;
         end if;
         Proto_Index :=
           BaseCargo_Container.Element
             (Container => Base_Cargo, Index => Items_Indexes(I))
             .Proto_Index;
         Item_Type :=
           (if
              Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .Show_Type =
              Null_Bounded_String
            then
              Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .I_Type
            else Objects_Container.Element
                (Container => Items_List, Index => Proto_Index)
                .Show_Type);
         if Index(Source => Items_Types, Pattern => To_String(Source => "{" & Item_Type & "}")) = 0 then
            Append(Source => Items_Types, New_Item => " {" & To_String(Source => Item_Type) & "}");
         end if;
         if Argc > 1 and then CArgv.Arg(Argv => Argv, N => 1) /= "All"
           and then To_String(Source => Item_Type) /= CArgv.Arg(Argv => Argv, N => 1) then
            goto End_Of_Trader_Loop;
         end if;
         Item_Name :=
           To_Unbounded_String
             (Source =>
                To_String
                  (Source =>
                     Objects_Container.Element
                       (Container => Items_List, Index => Proto_Index)
                       .Name));
         if Argc = 3
           and then
             Index
               (Source => To_Lower(Item => To_String(Source => Item_Name)), Pattern => To_Lower(Item => CArgv.Arg(Argv => Argv, N => 2))) =
             0 then
            goto End_Of_Trader_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Trader_Loop;
         end if;
         Price :=
           (if Base_Index > 0 then
              BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Items_Indexes(I))
                .Price
            else BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => Items_Indexes(I))
                .Price);
         if Event_Index > 0 then
            if Events_List(Event_Index).E_Type = DOUBLEPRICE
              and then Events_List(Event_Index).Item_Index = Proto_Index then
               Price := Price * 2;
            end if;
         end if;
         Base_Amount :=
           (if Base_Index = 0 then
              BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => Items_Indexes(I))
                .Amount
            else BaseCargo_Container.Element
                (Container => Sky_Bases(Base_Index).Cargo,
                 Index => Items_Indexes(I))
                .Amount);
         Add_Button
           (Trade_Table, To_String(Item_Name),
            "Show available options for item",
            "ShowTradeItemInfo -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            1);
         Add_Button
           (Trade_Table, To_String(Item_Type),
            "Show available options for item",
            "ShowTradeItemInfo -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            2);
         Item_Durability :=
           (if
              BaseCargo_Container.Element
                (Container => Base_Cargo, Index => Items_Indexes(I))
                .Durability <
              100
            then
              To_Unbounded_String
                (Get_Item_Damage
                   (BaseCargo_Container.Element
                      (Container => Base_Cargo, Index => Items_Indexes(I))
                      .Durability))
            else To_Unbounded_String("Unused"));
         Add_Progress_Bar
           (Trade_Table,
            BaseCargo_Container.Element
              (Container => Base_Cargo, Index => Items_Indexes(I))
              .Durability,
            Default_Item_Durability, To_String(Item_Durability),
            "ShowTradeItemInfo -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            3);
         Add_Button
           (Trade_Table, Positive'Image(Price),
            "Show available options for item",
            "ShowTradeItemInfo -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            4);
         Add_Button
           (Trade_Table, Integer'Image(-(Price)),
            "Show available options for item",
            "ShowTradeItemInfo -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            5, False, "red");
         Add_Button
           (Trade_Table,
            Positive'Image
              (Objects_Container.Element
                 (Container => Items_List, Index => Proto_Index)
                 .Weight) &
            " kg",
            "Show available options for item",
            "ShowTradeItemInfo -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            6);
         Add_Button
           (Trade_Table, " 0", "Show available options for item",
            "ShowTradeItemInfo -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            7);
         Add_Button
           (Trade_Table, Natural'Image(Base_Amount),
            "Show available options for item",
            "ShowTradeItemInfo -" &
            Trim(Positive'Image(Items_Indexes(I)), Left),
            8, True);
         <<End_Of_Trader_Loop>>
      end loop Show_Trader_Items_Loop;
      if Page > 1 then
         if Trade_Table.Row < Game_Settings.Lists_Limit + 1 then
            Add_Pagination
              (Trade_Table,
               "ShowTrade " & Arguments & Positive'Image(Page - 1), "");
         else
            Add_Pagination
              (Trade_Table,
               "ShowTrade " & Arguments & Positive'Image(Page - 1),
               "ShowTrade " & Arguments & Positive'Image(Page + 1));
         end if;
      elsif Trade_Table.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (Trade_Table, "",
            "ShowTrade " & Arguments & Positive'Image(Page + 1));
      end if;
      Update_Table
        (Trade_Table, (if Focus = Widget_Image(Search_Entry) then False));
      Tcl_Eval(Get_Context, "update");
      configure(Combo_Box, "-values [list " & To_String(Items_Types) & "]");
      if Argc = 1 then
         Current(Combo_Box, "0");
      end if;
      if Money_Index_2 > 0 then
         Trade_Info :=
           To_Unbounded_String
             ("You have" &
              Natural'Image
                (Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => Money_Index_2)
                   .Amount) &
              " " & To_String(Money_Name) & ".");
      else
         Trade_Info :=
           To_Unbounded_String
             ("You don't have any " & To_String(Money_Name) &
              " to buy anything.");
      end if;
      declare
         FreeSpace: Integer := Free_Cargo(0);
      begin
         if FreeSpace < 0 then
            FreeSpace := 0;
         end if;
         Append
           (Trade_Info,
            LF & "Free cargo space:" & Integer'Image(FreeSpace) & " kg.");
      end;
      Label.Name := New_String(Trade_Frame & ".options.playerinfo");
      configure(Label, "-text {" & To_String(Trade_Info) & "}");
      Trade_Info := Null_Unbounded_String;
      if Base_Index > 0 then
         if BaseCargo_Container.Element
             (Container => Sky_Bases(Base_Index).Cargo, Index => 1)
             .Amount =
           0 then
            Append
              (Trade_Info,
               "Base doesn't have any " & To_String(Money_Name) &
               "to buy anything.");
         else
            Append
              (Trade_Info,
               "Base has" &
               Positive'Image
                 (BaseCargo_Container.Element
                    (Container => Sky_Bases(Base_Index).Cargo, Index => 1)
                    .Amount) &
               " " & To_String(Money_Name) & ".");
         end if;
      else
         if BaseCargo_Container.Element(Container => Trader_Cargo, Index => 1)
             .Amount =
           0 then
            Append
              (Trade_Info,
               "Ship doesn't have any " & To_String(Money_Name) &
               "to buy anything.");
         else
            Append
              (Trade_Info,
               "Ship has" &
               Positive'Image
                 (BaseCargo_Container.Element
                    (Container => Trader_Cargo, Index => 1)
                    .Amount) &
               " " & To_String(Money_Name) & ".");
         end if;
      end if;
      Label.Name := New_String(Trade_Frame & ".options.baseinfo");
      configure(Label, "-text {" & To_String(Trade_Info) & "}");
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      configure
        (Trade_Canvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (Trade_Canvas, "window", "0 0 -anchor nw -window " & Trade_Frame);
      Tcl_Eval(Get_Context, "update");
      configure
        (Trade_Canvas,
         "-scrollregion [list " & BBox(Trade_Canvas, "all") & "]");
      Xview_Move_To(Trade_Canvas, "0.0");
      Yview_Move_To(Trade_Canvas, "0.0");
      Show_Screen("tradeframe");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Trade_Command;

   -- ****if* TUI/TUI.ItemIndex
   -- FUNCTION
   -- Index of the currently selected item
   -- SOURCE
   ItemIndex: Integer;
   -- ****

   -- ****o* TUI/TUI.Show_Trade_Item_Info_Command
   -- FUNCTION
   -- Show information about the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowTradeItemInfo itemindex
   -- ItemIndex is the index of the item which menu will be show. If index
   -- starts with minus means item in base/trader cargo only. Otherwise it is
   -- index in the player ship cargo.
   -- SOURCE
   function Show_Trade_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Trade_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Short_String;
      use Tiny_String;

      ItemInfo: Unbounded_String;
      ProtoIndex: Objects_Container.Extended_Index;
      CargoIndex, BaseCargoIndex, Price: Natural := 0;
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      ItemTypes: constant array(1 .. 6) of Tiny_String.Bounded_String :=
        (Weapon_Type, Chest_Armor, Head_Armor, Arms_Armor, Legs_Armor,
         Shield_Type);
      MaxSellAmount, MaxBuyAmount: Integer := 0;
      MoneyIndex2: constant Natural :=
        Find_Item(Player_Ship.Cargo, Money_Index);
      BaseType: Tiny_String.Bounded_String;
   begin
      ItemIndex := Integer'Value(CArgv.Arg(Argv, 1));
      if ItemIndex < 0 then
         BaseCargoIndex := abs (ItemIndex);
      else
         CargoIndex := ItemIndex;
      end if;
      if CargoIndex >
        Natural
          (Inventory_Container.Length(Container => Player_Ship.Cargo)) then
         return TCL_OK;
      end if;
      if BaseIndex = 0 and
        BaseCargoIndex >
          Natural(BaseCargo_Container.Length(Container => Trader_Cargo)) then
         return TCL_OK;
      elsif BaseIndex > 0
        and then BaseCargoIndex >
          Natural
            (BaseCargo_Container.Length
               (Container => Sky_Bases(BaseIndex).Cargo)) then
         return TCL_OK;
      end if;
      if CargoIndex > 0 then
         ProtoIndex :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => CargoIndex)
             .Proto_Index;
      else
         ProtoIndex :=
           (if BaseIndex = 0 then
              BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => BaseCargoIndex)
                .Proto_Index
            else BaseCargo_Container.Element
                (Container => Sky_Bases(BaseIndex).Cargo,
                 Index => BaseCargoIndex)
                .Proto_Index);
      end if;
      if Objects_Container.Element
          (Container => Items_List, Index => ProtoIndex)
          .I_Type =
        Weapon_Type then
         Append
           (ItemInfo,
            "Skill: " &
            To_String
              (SkillsData_Container.Element
                 (Skills_List,
                  Skills_Amount_Range
                    (Objects_Container.Element
                       (Container => Items_List, Index => ProtoIndex)
                       .Value
                       (3)))
                 .Name) &
            "/" &
            To_String
              (AttributesData_Container.Element
                 (Attributes_List,
                  SkillsData_Container.Element
                    (Skills_List,
                     Skills_Amount_Range
                       (Objects_Container.Element
                          (Container => Items_List, Index => ProtoIndex)
                          .Value
                          (3)))
                    .Attribute)
                 .Name) &
            (if
               Objects_Container.Element
                 (Container => Items_List, Index => ProtoIndex)
                 .Value
                 (4) =
               1
             then LF & "Can be used with shield."
             else LF & "Can't be used with shield (two-handed weapon).") &
            LF & "Damage type: ");
         case Objects_Container.Element
           (Container => Items_List, Index => ProtoIndex)
           .Value
           (5) is
            when 1 =>
               Append(ItemInfo, "cutting");
            when 2 =>
               Append(ItemInfo, "impaling");
            when 3 =>
               Append(ItemInfo, "blunt");
            when others =>
               null;
         end case;
      end if;
      Show_More_Info_Loop :
      for ItemType of ItemTypes loop
         if Objects_Container.Element
             (Container => Items_List, Index => ProtoIndex)
             .I_Type =
           ItemType then
            if ItemInfo /= Null_Unbounded_String then
               Append(ItemInfo, LF);
            end if;
            Append
              (ItemInfo,
               "Damage chance: " &
               Get_Item_Chance_To_Damage
                 (Objects_Container.Element
                    (Container => Items_List, Index => ProtoIndex)
                    .Value
                    (1)) &
               LF & "Strength:" &
               Integer'Image
                 (Objects_Container.Element
                    (Container => Items_List, Index => ProtoIndex)
                    .Value
                    (2)));
            exit Show_More_Info_Loop;
         end if;
      end loop Show_More_Info_Loop;
      if TinyString_Indefinite_Container.Contains
          (Container => Tools_List,
           Item =>
             Objects_Container.Element
               (Container => Items_List, Index => ProtoIndex)
               .I_Type) then
         if ItemInfo /= Null_Unbounded_String then
            Append(ItemInfo, LF);
         end if;
         Append
           (ItemInfo,
            "Damage chance: " &
            Get_Item_Chance_To_Damage
              (Objects_Container.Element
                 (Container => Items_List, Index => ProtoIndex)
                 .Value
                 (1)));
      end if;
      if Length
          (Objects_Container.Element
             (Container => Items_List, Index => ProtoIndex)
             .I_Type) >
        4
        and then
        (Slice
           (Objects_Container.Element
              (Container => Items_List, Index => ProtoIndex)
              .I_Type,
            1, 4) =
         "Ammo" or
         Objects_Container.Element
             (Container => Items_List, Index => ProtoIndex)
             .I_Type =
           To_Bounded_String("Harpoon")) then
         if ItemInfo /= Null_Unbounded_String then
            Append(ItemInfo, LF);
         end if;
         Append
           (ItemInfo,
            "Strength:" &
            Integer'Image
              (Objects_Container.Element
                 (Container => Items_List, Index => ProtoIndex)
                 .Value
                 (1)));
      end if;
      if Objects_Container.Element
          (Container => Items_List, Index => ProtoIndex)
          .Description /=
        Short_String.Null_Bounded_String then
         if ItemInfo /= Null_Unbounded_String then
            Append(ItemInfo, LF & LF);
         end if;
         Append
           (ItemInfo,
            To_String
              (Source =>
                 Objects_Container.Element
                   (Container => Items_List, Index => ProtoIndex)
                   .Description));
      end if;
      BaseType :=
        (if BaseIndex > 0 then Sky_Bases(BaseIndex).Base_Type
         else To_Bounded_String("0"));
      if ItemIndex > 0 then
         BaseCargoIndex :=
           Find_Base_Cargo
             (ProtoIndex,
              Inventory_Container.Element(Player_Ship.Cargo, CargoIndex)
                .Durability);
         if BaseCargoIndex > 0 then
            Price :=
              (if BaseIndex > 0 then
                 BaseCargo_Container.Element
                   (Container => Sky_Bases(BaseIndex).Cargo,
                    Index => BaseCargoIndex)
                   .Price
               else BaseCargo_Container.Element
                   (Container => Trader_Cargo, Index => BaseCargoIndex)
                   .Price);
         else
            Price := Get_Price(BaseType, ProtoIndex);
         end if;
      else
         ItemIndex :=
           Find_Item
             (Inventory => Player_Ship.Cargo, Proto_Index => ProtoIndex,
              Durability =>
                (if BaseIndex > 0 then
                   BaseCargo_Container.Element
                     (Container => Sky_Bases(BaseIndex).Cargo,
                      Index => BaseCargoIndex)
                     .Durability
                 else BaseCargo_Container.Element
                     (Container => Trader_Cargo, Index => BaseCargoIndex)
                     .Durability));
         Price :=
           (if BaseIndex > 0 then
              BaseCargo_Container.Element
                (Container => Sky_Bases(BaseIndex).Cargo,
                 Index => BaseCargoIndex)
                .Price
            else BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => BaseCargoIndex)
                .Price);
      end if;
      if ItemIndex > 0 then
         MaxSellAmount :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => ItemIndex)
             .Amount;
         declare
            MaxPrice: Natural := MaxSellAmount * Price;
            Weight: Integer;
         begin
            Count_Price(MaxPrice, Find_Member(TALK), False);
            if BaseIndex > 0
              and then MaxPrice >
                BaseCargo_Container.Element
                  (Container => Sky_Bases(BaseIndex).Cargo, Index => 1)
                  .Amount then
               MaxSellAmount :=
                 Natural
                   (Float'Floor
                      (Float(MaxSellAmount) *
                       (Float
                          (BaseCargo_Container.Element
                             (Container => Sky_Bases(BaseIndex).Cargo,
                              Index => 1)
                             .Amount) /
                        Float(MaxPrice))));
            elsif BaseIndex = 0
              and then MaxPrice >
                BaseCargo_Container.Element
                  (Container => Trader_Cargo, Index => 1)
                  .Amount then
               MaxSellAmount :=
                 Natural
                   (Float'Floor
                      (Float(MaxSellAmount) *
                       (Float
                          (BaseCargo_Container.Element
                             (Container => Trader_Cargo, Index => 1)
                             .Amount) /
                        Float(MaxPrice))));
            end if;
            MaxPrice := MaxSellAmount * Price;
            if MaxPrice > 0 then
               Count_Price(MaxPrice, Find_Member(TALK), False);
            end if;
            Weight :=
              Free_Cargo
                ((Objects_Container.Element
                    (Container => Items_List, Index => ProtoIndex)
                    .Weight *
                  MaxSellAmount) -
                 MaxPrice);
            Count_Sell_Amount_loop :
            while Weight < 0 loop
               MaxSellAmount :=
                 Integer
                   (Float'Floor
                      (Float(MaxSellAmount) *
                       (Float(MaxPrice + Weight) / Float(MaxPrice))));
               exit Count_Sell_Amount_loop when MaxSellAmount < 1;
               MaxPrice := MaxSellAmount * Price;
               Count_Price(MaxPrice, Find_Member(TALK), False);
               Weight :=
                 Free_Cargo
                   ((Objects_Container.Element
                       (Container => Items_List, Index => ProtoIndex)
                       .Weight *
                     MaxSellAmount) -
                    MaxPrice);
            end loop Count_Sell_Amount_loop;
         end;
      end if;
      if BaseCargoIndex > 0 and MoneyIndex2 > 0 and
        Is_Buyable(BaseType, ProtoIndex) then
         MaxBuyAmount :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => MoneyIndex2)
             .Amount /
           Price;
         declare
            MaxPrice: Natural := MaxBuyAmount * Price;
            Weight: Integer;
         begin
            if MaxBuyAmount > 0 then
               Count_Price(MaxPrice, Find_Member(TALK));
               if MaxPrice < (MaxBuyAmount * Price) then
                  MaxBuyAmount :=
                    Natural
                      (Float'Floor
                         (Float(MaxBuyAmount) *
                          ((Float(MaxBuyAmount) * Float(Price)) /
                           Float(MaxPrice))));
               end if;
               if BaseIndex > 0
                 and then MaxBuyAmount >
                   BaseCargo_Container.Element
                     (Container => Sky_Bases(BaseIndex).Cargo,
                      Index => BaseCargoIndex)
                     .Amount then
                  MaxBuyAmount :=
                    BaseCargo_Container.Element
                      (Container => Sky_Bases(BaseIndex).Cargo,
                       Index => BaseCargoIndex)
                      .Amount;
               elsif BaseIndex = 0
                 and then MaxBuyAmount >
                   BaseCargo_Container.Element
                     (Container => Trader_Cargo, Index => BaseCargoIndex)
                     .Amount then
                  MaxBuyAmount :=
                    BaseCargo_Container.Element
                      (Container => Trader_Cargo, Index => BaseCargoIndex)
                      .Amount;
               end if;
               MaxPrice := MaxBuyAmount * Price;
               Count_Price(MaxPrice, Find_Member(TALK));
               Weight :=
                 Free_Cargo
                   (MaxPrice -
                    (Objects_Container.Element
                       (Container => Items_List, Index => ProtoIndex)
                       .Weight *
                     MaxBuyAmount));
               Count_Buy_Amount_Loop :
               while Weight < 0 loop
                  MaxBuyAmount :=
                    MaxBuyAmount +
                    (Weight /
                     Objects_Container.Element
                       (Container => Items_List, Index => ProtoIndex)
                       .Weight) -
                    1;
                  if MaxBuyAmount < 0 then
                     MaxBuyAmount := 0;
                  end if;
                  exit Count_Buy_Amount_Loop when MaxBuyAmount = 0;
                  MaxPrice := MaxBuyAmount * Price;
                  Count_Price(MaxPrice, Find_Member(TALK));
                  Weight :=
                    Free_Cargo
                      (MaxPrice -
                       (Objects_Container.Element
                          (Container => Items_List, Index => ProtoIndex)
                          .Weight *
                        MaxBuyAmount));
               end loop Count_Buy_Amount_Loop;
            end if;
         end;
         if ItemIndex = 0 then
            ItemIndex := -(BaseCargoIndex);
         end if;
      end if;
      Show_Info
        (Text => To_String(ItemInfo),
         Title =>
           To_String
             (Objects_Container.Element
                (Container => Items_List, Index => ProtoIndex)
                .Name),
         Button_1 =>
           (if MaxBuyAmount = 0 then Empty_Button_Settings
            else
              (Tooltip =>
                 To_Unbounded_String(Source => "Buy item from the base"),
               Command =>
                 To_Unbounded_String
                   (Source =>
                      "TradeAmount buy" & Natural'Image(MaxBuyAmount) &
                      Natural'Image(Price)),
               Icon => To_Unbounded_String(Source => "buyicon"),
               Text => To_Unbounded_String(Source => "Buy"))),
         Button_2 =>
           (if MaxSellAmount = 0 then Empty_Button_Settings
            else
              (Tooltip =>
                 To_Unbounded_String
                   (Source => "Sell item from the ship cargo"),
               Command =>
                 To_Unbounded_String
                   (Source =>
                      "TradeAmount sell" & Natural'Image(MaxSellAmount) &
                      Natural'Image(Price)),
               Icon => To_Unbounded_String(Source => "sellicon"),
               Text => To_Unbounded_String(Source => "Sell"))));
      return TCL_OK;
   end Show_Trade_Item_Info_Command;

   -- ****o* TUI/TUI.Trade_Item_Command
   -- FUNCTION
   -- Buy or sell the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- TradeItem tradetype
   -- Tradetype is type of trade action. Can be buy, buymax, sell, sellmax
   -- SOURCE
   function Trade_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Trade_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      BaseCargoIndex, CargoIndex: Natural := 0;
      Trader: String(1 .. 4);
      ProtoIndex: Objects_Container.Extended_Index;
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget
          (Main_Paned & ".tradeframe.canvas.trade.options.type", Interp);
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".itemdialog.amount", Interp);
   begin
      if ItemIndex < 0 then
         BaseCargoIndex := abs (ItemIndex);
      else
         CargoIndex := ItemIndex;
      end if;
      if CargoIndex > 0 then
         ProtoIndex :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => CargoIndex)
             .Proto_Index;
         if BaseCargoIndex = 0 then
            BaseCargoIndex := Find_Base_Cargo(ProtoIndex);
         end if;
      else
         ProtoIndex :=
           (if BaseIndex = 0 then
              BaseCargo_Container.Element
                (Container => Trader_Cargo, Index => BaseCargoIndex)
                .Proto_Index
            else BaseCargo_Container.Element
                (Container => Sky_Bases(BaseIndex).Cargo,
                 Index => BaseCargoIndex)
                .Proto_Index);
      end if;
      Trader := (if BaseIndex > 0 then "base" else "ship");
      if Argc > 2 then
         if CArgv.Arg(Argv, 1) in "buy" then
            Buy_Items(BaseCargoIndex, CArgv.Arg(Argv, 2));
         else
            Sell_Items(CargoIndex, CArgv.Arg(Argv, 2));
         end if;
      else
         if CArgv.Arg(Argv, 1) in "buy" then
            Buy_Items(BaseCargoIndex, Get(AmountBox));
         else
            Sell_Items(CargoIndex, Get(AmountBox));
         end if;
         if Close_Dialog_Command
             (ClientData, Interp, 2,
              CArgv.Empty & "CloseDialog" & ".itemdialog") =
           TCL_ERROR then
            return TCL_ERROR;
         end if;
      end if;
      Update_Header;
      Update_Messages;
      return
        Show_Trade_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowTrade" & Get(TypeBox));
   exception
      when An_Exception : Trade_Cant_Buy =>
         Show_Message
           (Text =>
              "You can't buy " & Exception_Message(An_Exception) &
              " in this " & Trader & ".",
            Title => "Can't buy items");
         return TCL_OK;
      when An_Exception : Trade_Not_For_Sale_Now =>
         Show_Message
           (Text =>
              "You can't buy " & Exception_Message(An_Exception) &
              " in this base at this moment.",
            Title => "Can't buy items");
         return TCL_OK;
      when An_Exception : Trade_Buying_Too_Much =>
         Show_Message
           (Text =>
              Trader & " don't have that much " &
              Exception_Message(An_Exception) & " for sale.",
            Title => "Not enough items");
         return TCL_OK;
      when Trade_No_Free_Cargo =>
         Show_Message
           (Text => "You don't have that much free space in your ship cargo.",
            Title => "No free cargo space");
         return TCL_OK;
      when An_Exception : Trade_No_Money =>
         Show_Message
           (Text =>
              "You don't have any " & To_String(Money_Name) & " to buy " &
              Exception_Message(An_Exception) & ".",
            Title => "No money to buy items");
         return TCL_OK;
      when An_Exception : Trade_Not_Enough_Money =>
         Show_Message
           (Text =>
              "You don't have enough " & To_String(Money_Name) &
              " to buy so much " & Exception_Message(An_Exception) & ".",
            Title => "Not enough money to buy items");
         return TCL_OK;
      when Trade_Invalid_Amount =>
         if CArgv.Arg(Argv, 1) = "buy" then
            Show_Message
              (Text => "You entered invalid amount to buy.",
               Title => "Invalid amount of items");
         else
            Show_Message
              (Text => "You entered invalid amount to sell.",
               Title => "Invalid amount of items");
         end if;
         return TCL_OK;
      when An_Exception : Trade_Too_Much_For_Sale =>
         Show_Message
           (Text =>
              "You dont have that much " & Exception_Message(An_Exception) &
              " in ship cargo.",
            Title => "Not enough items for sale");
         return TCL_OK;
      when An_Exception : Trade_No_Money_In_Base =>
         Show_Message
           (Text =>
              "You can't sell so much " & Exception_Message(An_Exception) &
              " because " & Trader & " don't have that much " &
              To_String(Money_Name) & " to buy it.",
            Title => "Too much items for sale");
         return TCL_OK;
      when Trade_No_Trader =>
         Show_Message
           (Text =>
              "You don't have assigned anyone in crew to talk in bases duty.",
            Title => "No trader assigned");
         return TCL_OK;
   end Trade_Item_Command;

   -- ****o* TUI/TUI.Search_Trade_Command
   -- FUNCTION
   -- Show only this items which contains the selected sequence
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SearchTrade
   -- SOURCE
   function Search_Trade_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Trade_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget
          (Main_Paned & ".tradeframe.canvas.trade.options.type", Interp);
      SearchText: constant String := CArgv.Arg(Argv, 1);
   begin
      if SearchText'Length = 0 then
         return
           Show_Trade_Command
             (ClientData, Interp, 2, CArgv.Empty & "ShowTrade" & Get(TypeBox));
      end if;
      return
        Show_Trade_Command
          (ClientData, Interp, 3,
           CArgv.Empty & "ShowTrade" & Get(TypeBox) & SearchText);
   end Search_Trade_Command;

   -- ****o* TUI/TUI.Trade_Amount_Command
   -- FUNCTION
   -- Show dialog to enter amount of items to sell or buy
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- TradeAmount action baseindex
   -- Action which will be taken. Can be buy or sell. BaseIndex is the index
   -- of the base from which item will be bought. If zero it mean buying from
   -- trader ship.
   -- SOURCE
   function Trade_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Trade_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
   begin
      if CArgv.Arg(Argv, 1) = "sell" then
         Show_Manipulate_Item
           ("Sell " &
            Get_Item_Name
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => ItemIndex)),
            "TradeItem sell", "sell", ItemIndex,
            Natural'Value(CArgv.Arg(Argv, 2)),
            Natural'Value(CArgv.Arg(Argv, 3)));
      else
         if ItemIndex > 0 then
            Show_Manipulate_Item
              ("Buy " &
               Get_Item_Name
                 (Inventory_Container.Element
                    (Container => Player_Ship.Cargo, Index => ItemIndex)),
               "TradeItem buy", "buy", ItemIndex,
               Natural'Value(CArgv.Arg(Argv, 2)),
               Natural'Value(CArgv.Arg(Argv, 3)));
         else
            if BaseIndex > 0 then
               Show_Manipulate_Item
                 ("Buy " &
                  To_String
                    (Objects_Container.Element
                       (Container => Items_List,
                        Index =>
                          BaseCargo_Container.Element
                            (Container => Sky_Bases(BaseIndex).Cargo,
                             Index => abs (ItemIndex))
                            .Proto_Index)
                       .Name),
                  "TradeItem buy", "buy", abs (ItemIndex),
                  Natural'Value(CArgv.Arg(Argv, 2)),
                  Natural'Value(CArgv.Arg(Argv, 3)));
            else
               Show_Manipulate_Item
                 ("Buy " &
                  To_String
                    (Objects_Container.Element
                       (Container => Items_List,
                        Index =>
                          BaseCargo_Container.Element
                            (Container => Trader_Cargo,
                             Index => abs (ItemIndex))
                            .Proto_Index)
                       .Name),
                  "TradeItem buy", "buy", abs (ItemIndex),
                  Natural'Value(CArgv.Arg(Argv, 2)),
                  Natural'Value(CArgv.Arg(Argv, 3)));
            end if;
         end if;
      end if;
      return TCL_OK;
   end Trade_Amount_Command;

   -- ****o* TUI/TUI.Sort_Items_Command
   -- FUNCTION
   -- Sort the trading list
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortTradeItems x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Items_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Items_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number(Trade_Table, Natural'Value(CArgv.Arg(Argv, 1)));
      type Local_Item_Data is record
         Name: Unbounded_String;
         IType: Bounded_String;
         Damage: Float;
         Price: Natural;
         Profit: Integer;
         Weight: Positive;
         Owned: Natural;
         Available: Natural;
         Id: Positive;
      end record;
      BaseIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Indexes_List: Positive_Container.Vector;
      BaseCargo: BaseCargo_Container.Vector (Capacity => 16);
      BaseCargoIndex, Price: Natural;
      BaseType: Bounded_String;
      ProtoIndex: Objects_Container.Extended_Index;
      EventIndex: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index;
      package Items_Container is new Vectors
        (Index_Type => Positive, Element_Type => Local_Item_Data);
      Local_Items: Items_Container.Vector;
      function "<"(Left, Right: Local_Item_Data) return Boolean is
      begin
         if Items_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Items_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Items_Sort_Order = TYPEASC and then Left.IType < Right.IType then
            return True;
         end if;
         if Items_Sort_Order = TYPEDESC and then Left.IType > Right.IType then
            return True;
         end if;
         if Items_Sort_Order = DURABILITYASC
           and then Left.Damage < Right.Damage then
            return True;
         end if;
         if Items_Sort_Order = DURABILITYDESC
           and then Left.Damage > Right.Damage then
            return True;
         end if;
         if Items_Sort_Order = PRICEASC and then Left.Price < Right.Price then
            return True;
         end if;
         if Items_Sort_Order = PRICEDESC and then Left.Price > Right.Price then
            return True;
         end if;
         if Items_Sort_Order = PROFITASC
           and then Left.Profit < Right.Profit then
            return True;
         end if;
         if Items_Sort_Order = PROFITDESC
           and then Left.Profit > Right.Profit then
            return True;
         end if;
         if Items_Sort_Order = WEIGHTASC
           and then Left.Weight < Right.Weight then
            return True;
         end if;
         if Items_Sort_Order = WEIGHTDESC
           and then Left.Weight > Right.Weight then
            return True;
         end if;
         if Items_Sort_Order = OWNEDASC and then Left.Owned < Right.Owned then
            return True;
         end if;
         if Items_Sort_Order = OWNEDDESC and then Left.Owned > Right.Owned then
            return True;
         end if;
         if Items_Sort_Order = AVAILABLEASC
           and then Left.Available < Right.Available then
            return True;
         end if;
         if Items_Sort_Order = AVAILABLEDESC
           and then Left.Available > Right.Available then
            return True;
         end if;
         return False;
      end "<";
      package Sort_Items is new Items_Container.Generic_Sorting;
   begin
      case Column is
         when 1 =>
            if Items_Sort_Order = NAMEASC then
               Items_Sort_Order := NAMEDESC;
            else
               Items_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Items_Sort_Order = TYPEASC then
               Items_Sort_Order := TYPEDESC;
            else
               Items_Sort_Order := TYPEASC;
            end if;
         when 3 =>
            if Items_Sort_Order = DURABILITYASC then
               Items_Sort_Order := DURABILITYDESC;
            else
               Items_Sort_Order := DURABILITYASC;
            end if;
         when 4 =>
            if Items_Sort_Order = PRICEASC then
               Items_Sort_Order := PRICEDESC;
            else
               Items_Sort_Order := PRICEASC;
            end if;
         when 5 =>
            if Items_Sort_Order = PROFITASC then
               Items_Sort_Order := PROFITDESC;
            else
               Items_Sort_Order := PROFITASC;
            end if;
         when 6 =>
            if Items_Sort_Order = WEIGHTASC then
               Items_Sort_Order := WEIGHTDESC;
            else
               Items_Sort_Order := WEIGHTASC;
            end if;
         when 7 =>
            if Items_Sort_Order = OWNEDASC then
               Items_Sort_Order := OWNEDDESC;
            else
               Items_Sort_Order := OWNEDASC;
            end if;
         when 8 =>
            if Items_Sort_Order = AVAILABLEASC then
               Items_Sort_Order := AVAILABLEDESC;
            else
               Items_Sort_Order := AVAILABLEASC;
            end if;
         when others =>
            null;
      end case;
      if Items_Sort_Order = Default_Items_Sort_Order then
         return TCL_OK;
      end if;
      if BaseIndex > 0 then
         BaseCargo_Container.Assign
           (Target => BaseCargo, Source => Sky_Bases(BaseIndex).Cargo);
         BaseType := Sky_Bases(BaseIndex).Base_Type;
      else
         BaseCargo_Container.Assign
           (Target => BaseCargo, Source => Trader_Cargo);
         BaseType := To_Bounded_String("0");
      end if;
      for I in
        Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
          Inventory_Container.Last_Index(Container => Player_Ship.Cargo) loop
         ProtoIndex :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => I)
             .Proto_Index;
         BaseCargoIndex :=
           Find_Base_Cargo
             (ProtoIndex,
              Inventory_Container.Element
                (Container => Player_Ship.Cargo, Index => I)
                .Durability);
         if BaseCargoIndex > 0 then
            Indexes_List.Append(New_Item => BaseCargoIndex);
            Price :=
              BaseCargo_Container.Element
                (Container => BaseCargo, Index => BaseCargoIndex)
                .Price;
         else
            Price := Get_Price(BaseType, ProtoIndex);
         end if;
         if EventIndex > 0 then
            if Events_List(EventIndex).E_Type = DOUBLEPRICE
              and then Events_List(EventIndex).Item_Index = ProtoIndex then
               Price := Price * 2;
            end if;
         end if;
         Local_Items.Append
           (New_Item =>
              (Name =>
                 To_Unbounded_String
                   (Get_Item_Name
                      (Inventory_Container.Element
                         (Container => Player_Ship.Cargo, Index => I))),
               IType =>
                 (if
                    Objects_Container.Element
                      (Container => Items_List, Index => ProtoIndex)
                      .Show_Type =
                    Null_Bounded_String
                  then
                    Objects_Container.Element
                      (Container => Items_List, Index => ProtoIndex)
                      .I_Type
                  else Objects_Container.Element
                      (Container => Items_List, Index => ProtoIndex)
                      .Show_Type),
               Damage =>
                 Float
                   (Inventory_Container.Element
                      (Container => Player_Ship.Cargo, Index => I)
                      .Durability) /
                 Float(Default_Item_Durability),
               Price => Price,
               Profit =>
                 Price -
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => I)
                   .Price,
               Weight =>
                 Objects_Container.Element
                   (Container => Items_List, Index => ProtoIndex)
                   .Weight,
               Owned =>
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => I)
                   .Amount,
               Available =>
                 (if BaseCargoIndex > 0 then
                    BaseCargo_Container.Element
                      (Container => BaseCargo, Index => BaseCargoIndex)
                      .Amount
                  else 0),
               Id => I));
      end loop;
      Sort_Items.Sort(Local_Items);
      Items_Indexes.Clear;
      for Item of Local_Items loop
         Items_Indexes.Append(Item.Id);
      end loop;
      Items_Indexes.Append(0);
      Local_Items.Clear;
      for I in
        BaseCargo_Container.First_Index(Container => BaseCargo) ..
          BaseCargo_Container.Last_Index(Container => BaseCargo) loop
         if Indexes_List.Find_Index(Item => I) = 0 then
            ProtoIndex :=
              BaseCargo_Container.Element(Container => BaseCargo, Index => I)
                .Proto_Index;
            Price :=
              BaseCargo_Container.Element(Container => BaseCargo, Index => I)
                .Price;
            if EventIndex > 0 then
               if Events_List(EventIndex).E_Type = DOUBLEPRICE
                 and then Events_List(EventIndex).Item_Index = ProtoIndex then
                  Price := Price * 2;
               end if;
            end if;
            Local_Items.Append
              (New_Item =>
                 (Name =>
                    To_Unbounded_String
                      (Source =>
                         To_String
                           (Source =>
                              Objects_Container.Element
                                (Container => Items_List, Index => ProtoIndex)
                                .Name)),
                  IType =>
                    (if
                       Objects_Container.Element
                         (Container => Items_List, Index => ProtoIndex)
                         .Show_Type =
                       Null_Bounded_String
                     then
                       Objects_Container.Element
                         (Container => Items_List, Index => ProtoIndex)
                         .I_Type
                     else Objects_Container.Element
                         (Container => Items_List, Index => ProtoIndex)
                         .Show_Type),
                  Damage =>
                    Float
                      (BaseCargo_Container.Element
                         (Container => BaseCargo, Index => I)
                         .Durability) /
                    Float(Default_Item_Durability),
                  Price => Price, Profit => -(Price),
                  Weight =>
                    Objects_Container.Element
                      (Container => Items_List, Index => ProtoIndex)
                      .Weight,
                  Owned => 0,
                  Available =>
                    BaseCargo_Container.Element
                      (Container => BaseCargo, Index => I)
                      .Amount,
                  Id => I));
         end if;
      end loop;
      Sort_Items.Sort(Local_Items);
      for Item of Local_Items loop
         Items_Indexes.Append(Item.Id);
      end loop;
      return
        Show_Trade_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowTrade" & "All");
   end Sort_Items_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowTrade", Show_Trade_Command'Access);
      Add_Command("ShowTradeItemInfo", Show_Trade_Item_Info_Command'Access);
      Add_Command("TradeItem", Trade_Item_Command'Access);
      Add_Command("SearchTrade", Search_Trade_Command'Access);
      Add_Command("TradeAmount", Trade_Amount_Command'Access);
      Add_Command("SortTradeItems", Sort_Items_Command'Access);
   end Add_Commands;

end Trades.UI;
