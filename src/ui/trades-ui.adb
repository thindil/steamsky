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

-- with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with Ada.Exceptions;
-- with Ada.Strings;
-- with Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
-- with Interfaces.C.Strings;
-- with GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
-- with Tcl.Ada;
with Tcl.Tk.Ada;
-- with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame;
-- with Tcl.Tk.Ada.Widgets.TtkLabel;
-- with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
-- with Tcl.Tk.Ada.Winfo;
with Bases.Cargo; use Bases.Cargo;
with BasesTypes; use BasesTypes;
-- with Config;
with CoreUI; use CoreUI;
with Crew;
with Dialogs; use Dialogs;
with Events;
with Game; use Game;
with Maps; use Maps;
with Maps.UI;
with Ships.Cargo;
with Ships.Crew;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Trades.UI is

   -- ****iv* TUI/TUI.Trade_Table
   -- FUNCTION
   -- Table with info about the available items to trade
   -- SOURCE
   Trade_Table: Table_Widget (Amount => 8);
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
--      pragma Unreferenced(Client_Data);
--      use Ada.Characters.Handling;
--      use Ada.Strings;
--      use Ada.Strings.Fixed;
--      use Interfaces.C.Strings;
--      use GNAT.Directory_Operations;
--      use Tcl.Ada;
--      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets.Canvas;
--      use Tcl.Tk.Ada.Widgets.TtkEntry;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
--      use Tcl.Tk.Ada.Widgets.TtkLabel;
--      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
--      use Tcl.Tk.Ada.Winfo;
--      use Config;
--      use Tiny_String;
--
      Trade_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".tradeframe", Interp => Interp);
      Trade_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Trade_Frame & ".canvas", Interp => Interp);
--      Label: Ttk_Label :=
--        Get_Widget
--          (pathName => Trade_Canvas & ".trade.options.typelabel",
--           Interp => Interp);
--      Item_Name, Trade_Info, Item_Durability: Unbounded_String :=
--        Null_Unbounded_String;
--      Proto_Index: Natural := 0;
--      Base_Type: Bounded_String;
--      Item_Type: Bounded_String := Null_Bounded_String;
--      Items_Types: Unbounded_String := To_Unbounded_String(Source => "All");
--      Price: Positive := 1;
--      Combo_Box: Ttk_ComboBox;
--      Base_Index: Natural;
--      Base_Cargo: BaseCargo_Container.Vector (Capacity => 16);
--      Base_Cargo_Index, Base_Amount: Natural := 0;
--      --## rule off IMPROPER_INITIALIZATION
--      Indexes_List: Positive_Container.Vector;
--      --## rule on IMPROPER_INITIALIZATION
--      Event_Index: Natural;
--      Profit: Integer := 0;
--      Money_Index_2: constant Natural :=
--        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
--      Search_Entry: constant Ttk_Entry :=
--        Get_Widget
--          (pathName => Trade_Canvas & ".trade.options.search",
--           Interp => Interp);
--      --## rule off SIMPLIFIABLE_EXPRESSIONS
--      Page: constant Positive :=
--        (if Argc = 4 then Positive'Value(CArgv.Arg(Argv => Argv, N => 3))
--         else 1);
--      Start_Row: constant Positive :=
--        ((Page - 1) * Get_Integer_Setting(Name => "listsLimit")) + 1;
--      --## rule on SIMPLIFIABLE_EXPRESSIONS
--      Current_Row: Positive := 1;
--      Arguments: constant String :=
--        (if Argc > 2 then
--           "{" & CArgv.Arg(Argv => Argv, N => 1) & "} {" &
--           CArgv.Arg(Argv => Argv, N => 2) & "}"
--         elsif Argc = 2 then CArgv.Arg(Argv => Argv, N => 1) & " {}"
--         else "All {}");
--      Current_Item_Index: Positive := 1;
      function Show_Ada_Trade_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Import => True,
         Convention => C,
         External_Name => "showTradeCommand";
   begin
      if Show_Ada_Trade_Command(C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv) = TCL_ERROR then
         return TCL_ERROR;
      end if;
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
         Tooltip_Text => "Press mouse button to sort the items.");
      return TCL_OK;
--      Get_Ship_From_Nim(Ship => Player_Ship);
--      Base_Index := Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
--      Set_Base_Cargo(Base_Index => Base_Index);
--      Event_Index := Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index;
--      if Winfo_Get(Widgt => Label, Info => "exists") = "0" then
--         Tcl_EvalFile
--           (interp => Get_Context,
--            fileName =>
--              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
--              "trade.tcl");
--         Bind
--           (Widgt => Trade_Frame, Sequence => "<Configure>",
--            Script => "{ResizeCanvas %W.canvas %w %h}");
--         Trade_Frame := Get_Widget(pathName => Trade_Canvas & ".trade");
--         Trade_Table :=
--           Create_Table
--             (Parent => Widget_Image(Win => Trade_Frame),
--              Headers =>
--                (1 => To_Unbounded_String(Source => "Name"),
--                 2 => To_Unbounded_String(Source => "Type"),
--                 3 => To_Unbounded_String(Source => "Durability"),
--                 4 => To_Unbounded_String(Source => "Price"),
--                 5 => To_Unbounded_String(Source => "Profit"),
--                 6 => To_Unbounded_String(Source => "Weight"),
--                 7 => To_Unbounded_String(Source => "Owned"),
--                 8 => To_Unbounded_String(Source => "Available")),
--              Scrollbar =>
--                Get_Widget(pathName => Main_Paned & ".tradeframe.scrolly"),
--              Command => "SortTradeItems",
--              Tooltip_Text => "Press mouse button to sort the items.");
--      elsif Winfo_Get(Widgt => Label, Info => "ismapped") = "1" and
--        Argc = 1 then
--         Items_Sort_Order := Default_Items_Sort_Order;
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
--         configure(Widgt => Close_Button, options => "-command ShowSkyMap");
--         if Base_Index = 0 and Event_Index > 0 then
--            Delete_Event(Event_Index => Event_Index);
--         end if;
--         Show_Sky_Map(Clear => True);
--         return TCL_OK;
--      end if;
--      Tcl_SetVar
--        (interp => Interp, varName => "gamestate", newValue => "trade");
--      if Argc < 3 then
--         Delete
--           (TextEntry => Search_Entry, FirstIndex => "0", LastIndex => "end");
--      end if;
--      configure
--        (Widgt => Close_Button, options => "-command {ShowSkyMap ShowTrade}");
--      Trade_Frame.Name := New_String(Str => Trade_Canvas & ".trade");
--      Combo_Box :=
--        Get_Widget
--          (pathName => Trade_Frame & ".options.type", Interp => Interp);
--      Clear_Table(Table => Trade_Table);
--      if Base_Index > 0 then
--         Base_Type := Sky_Bases(Base_Index).Base_Type;
--         BaseCargo_Container.Assign
--           (Target => Base_Cargo, Source => Sky_Bases(Base_Index).Cargo);
--      else
--         Base_Type := To_Bounded_String(Source => "0");
--         BaseCargo_Container.Assign
--           (Target => Base_Cargo, Source => Trader_Cargo);
--      end if;
--      if Items_Sort_Order = Default_Items_Sort_Order then
--         Items_Indexes.Clear;
--         Fill_Item_Indexes_Loop :
--         for I in
--           Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
--             Inventory_Container.Last_Index
--               (Container => Player_Ship.Cargo) loop
--            Items_Indexes.Append(New_Item => I);
--         end loop Fill_Item_Indexes_Loop;
--         Items_Indexes.Append(New_Item => 0);
--         Add_Cargo_Indexes_Loop :
--         for I in
--           BaseCargo_Container.First_Index(Container => Base_Cargo) ..
--             BaseCargo_Container.Last_Index(Container => Base_Cargo) loop
--            Items_Indexes.Append(New_Item => I);
--         end loop Add_Cargo_Indexes_Loop;
--      end if;
--      Fill_Cargo_Types_Loop :
--      for I of Items_Indexes loop
--         exit Fill_Cargo_Types_Loop when I = 0;
--         Proto_Index :=
--           Inventory_Container.Element
--             (Container => Player_Ship.Cargo, Index => I)
--             .Proto_Index;
--         Item_Type :=
--           (if
--              Get_Proto_Item(Index => Proto_Index).Show_Type =
--              Null_Bounded_String
--            then Get_Proto_Item(Index => Proto_Index).I_Type
--            else Get_Proto_Item(Index => Proto_Index).Show_Type);
--         if Index
--             (Source => Items_Types,
--              Pattern => To_String(Source => "{" & Item_Type & "}")) =
--           0 and
--           Get_Proto_Item(Index => Proto_Index).Price > 0 then
--            Append
--              (Source => Items_Types,
--               New_Item => " {" & To_String(Source => Item_Type) & "}");
--         end if;
--      end loop Fill_Cargo_Types_Loop;
--      Show_Cargo_Items_Loop :
--      for I of Items_Indexes loop
--         Current_Item_Index := Current_Item_Index + 1;
--         exit Show_Cargo_Items_Loop when I = 0;
--         if Get_Price
--             (Base_Type => Base_Type,
--              Item_Index =>
--                Inventory_Container.Element
--                  (Container => Player_Ship.Cargo, Index => I)
--                  .Proto_Index) =
--           0 then
--            goto End_Of_Cargo_Loop;
--         end if;
--         Proto_Index :=
--           Inventory_Container.Element
--             (Container => Player_Ship.Cargo, Index => I)
--             .Proto_Index;
--         Base_Cargo_Index :=
--           Find_Base_Cargo
--             (Proto_Index => Proto_Index,
--              Durability =>
--                Inventory_Container.Element
--                  (Container => Player_Ship.Cargo, Index => I)
--                  .Durability);
--         if Base_Cargo_Index > 0 then
--            Indexes_List.Append(New_Item => Base_Cargo_Index);
--         end if;
--         Item_Type :=
--           (if
--              Get_Proto_Item(Index => Proto_Index).Show_Type =
--              Null_Bounded_String
--            then Get_Proto_Item(Index => Proto_Index).I_Type
--            else Get_Proto_Item(Index => Proto_Index).Show_Type);
--         if Argc > 1 and then CArgv.Arg(Argv => Argv, N => 1) /= "All"
--           and then To_String(Source => Item_Type) /=
--             CArgv.Arg(Argv => Argv, N => 1) then
--            goto End_Of_Cargo_Loop;
--         end if;
--         Item_Name :=
--           To_Unbounded_String
--             (Source =>
--                Get_Item_Name
--                  (Item =>
--                     Inventory_Container.Element
--                       (Container => Player_Ship.Cargo, Index => I),
--                   Damage_Info => False, To_Lower => False));
--         if Argc = 3
--           and then
--             Index
--               (Source => To_Lower(Item => To_String(Source => Item_Name)),
--                Pattern => To_Lower(Item => CArgv.Arg(Argv => Argv, N => 2))) =
--             0 then
--            goto End_Of_Cargo_Loop;
--         end if;
--         if Current_Row < Start_Row then
--            Current_Row := Current_Row + 1;
--            goto End_Of_Cargo_Loop;
--         end if;
--         if Base_Cargo_Index = 0 then
--            Price :=
--              Get_Price(Base_Type => Base_Type, Item_Index => Proto_Index);
--         else
--            Price :=
--              (if Base_Index > 0 then
--                 BaseCargo_Container.Element
--                   (Container => Sky_Bases(Base_Index).Cargo,
--                    Index => Base_Cargo_Index)
--                   .Price
--               else BaseCargo_Container.Element
--                   (Container => Trader_Cargo, Index => Base_Cargo_Index)
--                   .Price);
--         end if;
--         if Event_Index > 0 then
--            if Get_Event(Index => Event_Index).E_Type = DOUBLEPRICE
--              and then Get_Event(Index => Event_Index).Item_Index =
--                Proto_Index then
--               Price := Price * 2;
--            end if;
--         end if;
--         Profit :=
--           Price -
--           Inventory_Container.Element
--             (Container => Player_Ship.Cargo, Index => I)
--             .Price;
--         Base_Amount := 0;
--         if Base_Index > 0 then
--            if Base_Cargo_Index > 0 and
--              Is_Buyable
--                (Base_Type => Base_Type, Item_Index => Proto_Index) then
--               Base_Amount :=
--                 BaseCargo_Container.Element
--                   (Container => Base_Cargo, Index => Base_Cargo_Index)
--                   .Amount;
--            end if;
--         else
--            if Base_Cargo_Index > 0 then
--               Base_Amount :=
--                 BaseCargo_Container.Element
--                   (Container => Base_Cargo, Index => Base_Cargo_Index)
--                   .Amount;
--            end if;
--         end if;
--         Add_Button
--           (Table => Trade_Table, Text => To_String(Source => Item_Name),
--            Tooltip => "Show available options for item",
--            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 1);
--         Add_Button
--           (Table => Trade_Table, Text => To_String(Source => Item_Type),
--            Tooltip => "Show available options for item",
--            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 2);
--         Item_Durability :=
--           (if
--              Inventory_Container.Element
--                (Container => Player_Ship.Cargo, Index => I)
--                .Durability <
--              100
--            then
--              To_Unbounded_String
--                (Source =>
--                   Get_Item_Damage
--                     (Item_Durability =>
--                        Inventory_Container.Element
--                          (Container => Player_Ship.Cargo, Index => I)
--                          .Durability))
--            else To_Unbounded_String(Source => "Unused"));
--         Add_Progress_Bar
--           (Table => Trade_Table,
--            Value =>
--              Inventory_Container.Element
--                (Container => Player_Ship.Cargo, Index => I)
--                .Durability,
--            Max_Value => Default_Item_Durability,
--            Tooltip => To_String(Source => Item_Durability),
--            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 3);
--         Add_Button
--           (Table => Trade_Table, Text => Positive'Image(Price),
--            Tooltip => "Show available options for item",
--            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 4);
--         Add_Button
--           (Table => Trade_Table, Text => Positive'Image(Profit),
--            Tooltip => "Show available options for item",
--            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 5,
--            Color =>
--              (if Profit > 0 then
--                 Tcl_GetVar
--                   (interp => Interp,
--                    varName =>
--                      "ttk::theme::" &
--                      To_String(Source => Get_Interface_Theme) &
--                      "::colors(-green)")
--               elsif Profit < 0 then
--                 Tcl_GetVar
--                   (interp => Interp,
--                    varName =>
--                      "ttk::theme::" &
--                      To_String(Source => Get_Interface_Theme) &
--                      "::colors(-red)")
--               else ""));
--         Add_Button
--           (Table => Trade_Table,
--            Text =>
--              Positive'Image(Get_Proto_Item(Index => Proto_Index).Weight) &
--              " kg",
--            Tooltip => "Show available options for item",
--            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 6);
--         Add_Button
--           (Table => Trade_Table,
--            Text =>
--              Positive'Image
--                (Inventory_Container.Element
--                   (Container => Player_Ship.Cargo, Index => I)
--                   .Amount),
--            Tooltip => "Show available options for item",
--            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 7);
--         Add_Button
--           (Table => Trade_Table, Text => Positive'Image(Base_Amount),
--            Tooltip => "Show available options for item",
--            Command => "ShowTradeItemInfo" & Positive'Image(I), Column => 8,
--            New_Row => True);
--         exit Show_Cargo_Items_Loop when Trade_Table.Row =
--           Get_Integer_Setting(Name => "listsLimit") + 1;
--         <<End_Of_Cargo_Loop>>
--      end loop Show_Cargo_Items_Loop;
--      Current_Item_Index :=
--        Natural(Inventory_Container.Length(Container => Player_Ship.Cargo)) +
--        2;
--      --## rule off SIMPLIFIABLE_STATEMENTS
--      Fill_Trader_Types_Loop :
--      for I in Current_Item_Index .. Items_Indexes.Last_Index loop
--         Proto_Index :=
--           BaseCargo_Container.Element
--             (Container => Base_Cargo, Index => Items_Indexes(I))
--             .Proto_Index;
--         Item_Type :=
--           (if
--              Get_Proto_Item(Index => Proto_Index).Show_Type =
--              Null_Bounded_String
--            then Get_Proto_Item(Index => Proto_Index).I_Type
--            else Get_Proto_Item(Index => Proto_Index).Show_Type);
--         if Is_Buyable
--             (Base_Type => Base_Type, Item_Index => Proto_Index,
--              Base_Index => Base_Index) and
--           BaseCargo_Container.Element
--               (Container => Base_Cargo, Index => Items_Indexes(I))
--               .Amount >
--             0 and
--           Index
--               (Source => Items_Types,
--                Pattern => To_String(Source => "{" & Item_Type & "}")) =
--             0 then
--            Append
--              (Source => Items_Types,
--               New_Item => " {" & To_String(Source => Item_Type) & "}");
--         end if;
--      end loop Fill_Trader_Types_Loop;
--      Show_Trader_Items_Loop :
--      for I in Current_Item_Index .. Items_Indexes.Last_Index loop
--         exit Show_Trader_Items_Loop when Trade_Table.Row =
--           Get_Integer_Setting(Name => "listsLimit") + 1;
--         if Indexes_List.Find_Index(Item => Items_Indexes(I)) > 0 or
--           not Is_Buyable
--             (Base_Type => Base_Type,
--              Item_Index =>
--                BaseCargo_Container.Element
--                  (Container => Base_Cargo, Index => Items_Indexes(I))
--                  .Proto_Index,
--              Base_Index => Base_Index) or
--           BaseCargo_Container.Element
--               (Container => Base_Cargo, Index => Items_Indexes(I))
--               .Amount =
--             0 then
--            goto End_Of_Trader_Loop;
--         end if;
--         Proto_Index :=
--           BaseCargo_Container.Element
--             (Container => Base_Cargo, Index => Items_Indexes(I))
--             .Proto_Index;
--         Item_Type :=
--           (if
--              Get_Proto_Item(Index => Proto_Index).Show_Type =
--              Null_Bounded_String
--            then Get_Proto_Item(Index => Proto_Index).I_Type
--            else Get_Proto_Item(Index => Proto_Index).Show_Type);
--         if Argc > 1 and then CArgv.Arg(Argv => Argv, N => 1) /= "All"
--           and then To_String(Source => Item_Type) /=
--             CArgv.Arg(Argv => Argv, N => 1) then
--            goto End_Of_Trader_Loop;
--         end if;
--         Item_Name :=
--           To_Unbounded_String
--             (Source =>
--                To_String
--                  (Source => Get_Proto_Item(Index => Proto_Index).Name));
--         if Argc = 3
--           and then
--             Index
--               (Source => To_Lower(Item => To_String(Source => Item_Name)),
--                Pattern => To_Lower(Item => CArgv.Arg(Argv => Argv, N => 2))) =
--             0 then
--            goto End_Of_Trader_Loop;
--         end if;
--         if Current_Row < Start_Row then
--            Current_Row := Current_Row + 1;
--            goto End_Of_Trader_Loop;
--         end if;
--         Price :=
--           (if Base_Index > 0 then
--              BaseCargo_Container.Element
--                (Container => Sky_Bases(Base_Index).Cargo,
--                 Index => Items_Indexes(I))
--                .Price
--            else BaseCargo_Container.Element
--                (Container => Trader_Cargo, Index => Items_Indexes(I))
--                .Price);
--         if Event_Index > 0 then
--            if Get_Event(Index => Event_Index).E_Type = DOUBLEPRICE
--              and then Get_Event(Index => Event_Index).Item_Index =
--                Proto_Index then
--               Price := Price * 2;
--            end if;
--         end if;
--         Base_Amount :=
--           (if Base_Index = 0 then
--              BaseCargo_Container.Element
--                (Container => Trader_Cargo, Index => Items_Indexes(I))
--                .Amount
--            else BaseCargo_Container.Element
--                (Container => Sky_Bases(Base_Index).Cargo,
--                 Index => Items_Indexes(I))
--                .Amount);
--         Add_Button
--           (Table => Trade_Table, Text => To_String(Source => Item_Name),
--            Tooltip => "Show available options for item",
--            Command =>
--              "ShowTradeItemInfo -" &
--              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
--            Column => 1);
--         Add_Button
--           (Table => Trade_Table, Text => To_String(Source => Item_Type),
--            Tooltip => "Show available options for item",
--            Command =>
--              "ShowTradeItemInfo -" &
--              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
--            Column => 2);
--         Item_Durability :=
--           (if
--              BaseCargo_Container.Element
--                (Container => Base_Cargo, Index => Items_Indexes(I))
--                .Durability <
--              100
--            then
--              To_Unbounded_String
--                (Source =>
--                   Get_Item_Damage
--                     (Item_Durability =>
--                        BaseCargo_Container.Element
--                          (Container => Base_Cargo, Index => Items_Indexes(I))
--                          .Durability))
--            else To_Unbounded_String(Source => "Unused"));
--         Add_Progress_Bar
--           (Table => Trade_Table,
--            Value =>
--              BaseCargo_Container.Element
--                (Container => Base_Cargo, Index => Items_Indexes(I))
--                .Durability,
--            Max_Value => Default_Item_Durability,
--            Tooltip => To_String(Source => Item_Durability),
--            Command =>
--              "ShowTradeItemInfo -" &
--              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
--            Column => 3);
--         Add_Button
--           (Table => Trade_Table, Text => Positive'Image(Price),
--            Tooltip => "Show available options for item",
--            Command =>
--              "ShowTradeItemInfo -" &
--              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
--            Column => 4);
--         Add_Button
--           (Table => Trade_Table, Text => Integer'Image(-Price),
--            Tooltip => "Show available options for item",
--            Command =>
--              "ShowTradeItemInfo -" &
--              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
--            Column => 5, New_Row => False,
--            Color =>
--              Tcl_GetVar
--                (interp => Interp,
--                 varName =>
--                   "ttk::theme::" & To_String(Source => Get_Interface_Theme) &
--                   "::colors(-red)"));
--         Add_Button
--           (Table => Trade_Table,
--            Text =>
--              Positive'Image(Get_Proto_Item(Index => Proto_Index).Weight) &
--              " kg",
--            Tooltip => "Show available options for item",
--            Command =>
--              "ShowTradeItemInfo -" &
--              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
--            Column => 6);
--         Add_Button
--           (Table => Trade_Table, Text => " 0",
--            Tooltip => "Show available options for item",
--            Command =>
--              "ShowTradeItemInfo -" &
--              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
--            Column => 7);
--         Add_Button
--           (Table => Trade_Table, Text => Natural'Image(Base_Amount),
--            Tooltip => "Show available options for item",
--            Command =>
--              "ShowTradeItemInfo -" &
--              Trim(Source => Positive'Image(Items_Indexes(I)), Side => Left),
--            Column => 8, New_Row => True);
--         <<End_Of_Trader_Loop>>
--      end loop Show_Trader_Items_Loop;
--      --## rule on SIMPLIFIABLE_STATEMENTS
--      if Page > 1 then
--         if Trade_Table.Row <
--           Get_Integer_Setting(Name => "listsLimit") + 1 then
--            Add_Pagination
--              (Table => Trade_Table,
--               Previous_Command =>
--                 "ShowTrade " & Arguments & Positive'Image(Page - 1),
--               Next_Command => "");
--         else
--            Add_Pagination
--              (Table => Trade_Table,
--               Previous_Command =>
--                 "ShowTrade " & Arguments & Positive'Image(Page - 1),
--               Next_Command =>
--                 "ShowTrade " & Arguments & Positive'Image(Page + 1));
--         end if;
--      elsif Trade_Table.Row =
--        Get_Integer_Setting(Name => "listsLimit") + 1 then
--         Add_Pagination
--           (Table => Trade_Table, Previous_Command => "",
--            Next_Command =>
--              "ShowTrade " & Arguments & Positive'Image(Page + 1));
--      end if;
--      Update_Table
--        (Table => Trade_Table,
--         Grab_Focus =>
--           (if Focus = Widget_Image(Win => Search_Entry) then False));
--      Tcl_Eval(interp => Get_Context, strng => "update");
--      configure
--        (Widgt => Combo_Box,
--         options => "-values [list " & To_String(Source => Items_Types) & "]");
--      if Argc = 1 then
--         Current(ComboBox => Combo_Box, NewIndex => "0");
--      end if;
--      if Money_Index_2 > 0 then
--         Trade_Info :=
--           To_Unbounded_String
--             (Source =>
--                "You have" &
--                Natural'Image
--                  (Inventory_Container.Element
--                     (Container => Player_Ship.Cargo, Index => Money_Index_2)
--                     .Amount) &
--                " " & To_String(Source => Money_Name) & ".");
--      else
--         Trade_Info :=
--           To_Unbounded_String
--             (Source =>
--                "You don't have any " & To_String(Source => Money_Name) &
--                " to buy anything.");
--      end if;
--      Show_Free_Space_Block :
--      declare
--         Free_Space: Integer := Free_Cargo(Amount => 0);
--      begin
--         if Free_Space < 0 then
--            Free_Space := 0;
--         end if;
--         Append
--           (Source => Trade_Info,
--            New_Item =>
--              LF & "Free cargo space:" & Integer'Image(Free_Space) & " kg.");
--      end Show_Free_Space_Block;
--      Label.Name := New_String(Str => Trade_Frame & ".options.playerinfo");
--      configure
--        (Widgt => Label,
--         options => "-text {" & To_String(Source => Trade_Info) & "}");
--      Trade_Info := Null_Unbounded_String;
--      if Base_Index > 0 then
--         if BaseCargo_Container.Element
--             (Container => Sky_Bases(Base_Index).Cargo, Index => 1)
--             .Amount =
--           0 then
--            Append
--              (Source => Trade_Info,
--               New_Item =>
--                 "Base doesn't have any " & To_String(Source => Money_Name) &
--                 "to buy anything.");
--         else
--            Append
--              (Source => Trade_Info,
--               New_Item =>
--                 "Base has" &
--                 Positive'Image
--                   (BaseCargo_Container.Element
--                      (Container => Sky_Bases(Base_Index).Cargo, Index => 1)
--                      .Amount) &
--                 " " & To_String(Source => Money_Name) & ".");
--         end if;
--      else
--         if BaseCargo_Container.Element(Container => Trader_Cargo, Index => 1)
--             .Amount =
--           0 then
--            Append
--              (Source => Trade_Info,
--               New_Item =>
--                 "Ship doesn't have any " & To_String(Source => Money_Name) &
--                 "to buy anything.");
--         else
--            Append
--              (Source => Trade_Info,
--               New_Item =>
--                 "Ship has" &
--                 Positive'Image
--                   (BaseCargo_Container.Element
--                      (Container => Trader_Cargo, Index => 1)
--                      .Amount) &
--                 " " & To_String(Source => Money_Name) & ".");
--         end if;
--      end if;
--      Label.Name := New_String(Str => Trade_Frame & ".options.baseinfo");
--      configure
--        (Widgt => Label,
--         options => "-text {" & To_String(Source => Trade_Info) & "}");
--      Tcl.Tk.Ada.Grid.Grid
--        (Slave => Close_Button, Options => "-row 0 -column 1");
--      configure
--        (Widgt => Trade_Canvas,
--         options =>
--           "-height [expr " & SashPos(Paned => Main_Paned, Index => "0") &
--           " - 20] -width " & cget(Widgt => Main_Paned, option => "-width"));
--      Tcl_Eval(interp => Get_Context, strng => "update");
--      Canvas_Create
--        (Parent => Trade_Canvas, Child_Type => "window",
--         Options => "0 0 -anchor nw -window " & Trade_Frame);
--      Tcl_Eval(interp => Get_Context, strng => "update");
--      configure
--        (Widgt => Trade_Canvas,
--         options =>
--           "-scrollregion [list " &
--           BBox(CanvasWidget => Trade_Canvas, TagOrId => "all") & "]");
--      Xview_Move_To(CanvasWidget => Trade_Canvas, Fraction => "0.0");
--      Yview_Move_To(CanvasWidget => Trade_Canvas, Fraction => "0.0");
--      Show_Screen(New_Screen_Name => "tradeframe");
--      Tcl_SetResult(interp => Interp, str => "1");
--      return TCL_OK;
   end Show_Trade_Command;

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
      Convention => C;
      -- ****

   function Sort_Items_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Ada.Containers;
      use Events;
      use Tiny_String;

      X_Pos: constant Integer :=
        Integer'Value(CArgv.Arg(Argv => Argv, N => 1));
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Column: constant Positive :=
        (if X_Pos > -1 then
           Get_Column_Number(Table => Trade_Table, X_Position => X_Pos)
         else Items_Sort_Orders'Pos(Items_Sort_Order) + 1);
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      --## rule off TYPE_INITIAL_VALUES
      type Local_Item_Data is record
         Name: Unbounded_String;
         I_Type: Bounded_String;
         Damage: Float;
         Price: Natural;
         Profit: Integer;
         Weight: Positive;
         Owned: Natural;
         Available: Natural;
         Id: Positive;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      --## rule off IMPROPER_INITIALIZATION
      Indexes_List: Positive_Container.Vector;
      --## rule off IMPROPER_INITIALIZATION
      Base_Cargo: BaseCargo_Container.Vector (Capacity => 16);
      Base_Cargo_Index, Price: Natural;
      Base_Type: Bounded_String;
      Proto_Index: Natural;
      Event_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index;
      package Items_Container is new Vectors
        (Index_Type => Positive, Element_Type => Local_Item_Data);
      --## rule off IMPROPER_INITIALIZATION
      Local_Items: Items_Container.Vector;
      --## rule on IMPROPER_INITIALIZATION
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Type_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName => Main_Paned & ".tradeframe.canvas.trade.options.type",
           Interp => Interp);
      function "<"(Left, Right: Local_Item_Data) return Boolean is
      begin
         if Items_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Items_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Items_Sort_Order = TYPEASC and then Left.I_Type < Right.I_Type then
            return True;
         end if;
         if Items_Sort_Order = TYPEDESC
           and then Left.I_Type > Right.I_Type then
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
      Items_Indexes: Natural_Container.Vector;
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
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      if Base_Index > 0 then
         BaseCargo_Container.Assign
           (Target => Base_Cargo, Source => Sky_Bases(Base_Index).Cargo);
         Base_Type := Sky_Bases(Base_Index).Base_Type;
      else
         BaseCargo_Container.Assign
           (Target => Base_Cargo, Source => Trader_Cargo);
         Base_Type := To_Bounded_String(Source => "0");
      end if;
      Add_Cargo_Items_Loop :
      for I in
        Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
          Inventory_Container.Last_Index(Container => Player_Ship.Cargo) loop
         Proto_Index :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => I)
             .Proto_Index;
         Base_Cargo_Index :=
           Find_Base_Cargo
             (Proto_Index => Proto_Index,
              Durability =>
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => I)
                  .Durability);
         if Base_Cargo_Index > 0 then
            Indexes_List.Append(New_Item => Base_Cargo_Index);
            Price :=
              BaseCargo_Container.Element
                (Container => Base_Cargo, Index => Base_Cargo_Index)
                .Price;
         else
            Price :=
              Get_Price(Base_Type => Base_Type, Item_Index => Proto_Index);
         end if;
         if Event_Index > 0 then
            if Get_Event(Index => Event_Index).E_Type = DOUBLEPRICE
              and then Get_Event(Index => Event_Index).Item_Index =
                Proto_Index then
               Price := Price * 2;
            end if;
         end if;
         Local_Items.Append
           (New_Item =>
              (Name =>
                 To_Unbounded_String
                   (Source =>
                      Get_Item_Name
                        (Item =>
                           Inventory_Container.Element
                             (Container => Player_Ship.Cargo, Index => I))),
               I_Type =>
                 (if
                    Get_Proto_Item(Index => Proto_Index).Show_Type =
                    Null_Bounded_String
                  then Get_Proto_Item(Index => Proto_Index).I_Type
                  else Get_Proto_Item(Index => Proto_Index).Show_Type),
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
               Weight => Get_Proto_Item(Index => Proto_Index).Weight,
               Owned =>
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => I)
                   .Amount,
               Available =>
                 (if Base_Cargo_Index > 0 then
                    BaseCargo_Container.Element
                      (Container => Base_Cargo, Index => Base_Cargo_Index)
                      .Amount
                  else 0),
               Id => I));
      end loop Add_Cargo_Items_Loop;
      Sort_Items.Sort(Container => Local_Items);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Items_Indexes.Clear;
      Add_Cargo_Indexes_Loop :
      for Item of Local_Items loop
         Items_Indexes.Append(New_Item => Item.Id);
      end loop Add_Cargo_Indexes_Loop;
      Items_Indexes.Append(New_Item => 0);
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Local_Items.Clear;
      Add_Base_Item_Loop :
      for I in
        BaseCargo_Container.First_Index(Container => Base_Cargo) ..
          BaseCargo_Container.Last_Index(Container => Base_Cargo) loop
         if Indexes_List.Find_Index(Item => I) = 0 then
            Proto_Index :=
              BaseCargo_Container.Element(Container => Base_Cargo, Index => I)
                .Proto_Index;
            Price :=
              BaseCargo_Container.Element(Container => Base_Cargo, Index => I)
                .Price;
            if Event_Index > 0 then
               if Get_Event(Index => Event_Index).E_Type = DOUBLEPRICE
                 and then Get_Event(Index => Event_Index).Item_Index =
                   Proto_Index then
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
                              Get_Proto_Item(Index => Proto_Index).Name)),
                  I_Type =>
                    (if
                       Get_Proto_Item(Index => Proto_Index).Show_Type =
                       Null_Bounded_String
                     then Get_Proto_Item(Index => Proto_Index).I_Type
                     else Get_Proto_Item(Index => Proto_Index).Show_Type),
                  Damage =>
                    Float
                      (BaseCargo_Container.Element
                         (Container => Base_Cargo, Index => I)
                         .Durability) /
                    Float(Default_Item_Durability),
                  Price => Price, Profit => -Price,
                  Weight => Get_Proto_Item(Index => Proto_Index).Weight,
                  Owned => 0,
                  Available =>
                    BaseCargo_Container.Element
                      (Container => Base_Cargo, Index => I)
                      .Amount,
                  Id => I));
         end if;
      end loop Add_Base_Item_Loop;
      Sort_Items.Sort(Container => Local_Items);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Add_Base_Indexes_Loop :
      for Item of Local_Items loop
         Items_Indexes.Append(New_Item => Item.Id);
      end loop Add_Base_Indexes_Loop;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      return
        Show_Trade_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => CArgv.Empty & "ShowTrade" & Get(Widgt => Type_Box));
   end Sort_Items_Command;

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
      use Ada.Exceptions;
      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
      use Maps.UI;

      Base_Index: constant Natural :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Base_Cargo_Index, Cargo_Index: Natural := 0;
      Trader: String(1 .. 4);
      Proto_Index: Natural;
      Amount_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => ".itemdialog.amount", Interp => Interp);
      Type_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName =>
             ".gameframe.paned.tradeframe.canvas.trade.options.type");
   begin
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      if Item_Index < 0 then
         Base_Cargo_Index := abs Item_Index;
      else
         Cargo_Index := Item_Index;
      end if;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      if Cargo_Index > 0 then
         Proto_Index :=
           Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => Cargo_Index)
             .Proto_Index;
         if Base_Cargo_Index = 0 then
            Base_Cargo_Index := Find_Base_Cargo(Proto_Index => Proto_Index);
         end if;
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
      Trader := (if Base_Index > 0 then "base" else "ship");
      if Argc > 2 then
         if CArgv.Arg(Argv => Argv, N => 1) in "buy" then
            Buy_Items
              (Base_Item_Index => Base_Cargo_Index,
               Amount => CArgv.Arg(Argv => Argv, N => 2));
         else
            Sell_Items
              (Item_Index => Cargo_Index,
               Amount => CArgv.Arg(Argv => Argv, N => 2));
         end if;
      else
         if CArgv.Arg(Argv => Argv, N => 1) in "buy" then
            Buy_Items
              (Base_Item_Index => Base_Cargo_Index,
               Amount => Get(Widgt => Amount_Box));
         else
            Sell_Items
              (Item_Index => Cargo_Index, Amount => Get(Widgt => Amount_Box));
         end if;
         if Close_Dialog_Command
             (Client_Data => Client_Data, Interp => Interp, Argc => 2,
              Argv => CArgv.Empty & "CloseDialog" & ".itemdialog") =
           TCL_ERROR then
            return TCL_ERROR;
         end if;
      end if;
      Update_Header;
      Update_Messages;
      Unbind(Widgt => Type_Box, Sequence => "<<ComboBoxSelected>>");
      Current(ComboBox => Type_Box, NewIndex => "0");
      Bind
        (Widgt => Type_Box, Sequence => "<<ComboBoxSelected>>",
         Script => "{ShowTrade [" & Type_Box & " get]}");
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      if Items_Sort_Order /= Default_Items_Sort_Order then
         return
           Sort_Items_Command
             (Client_Data => Client_Data, Interp => Interp, Argc => 2,
              Argv => CArgv.Empty & "SortTradeItem" & "-1");
      end if;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      return
        Show_Trade_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => CArgv.Empty & "ShowTrade" & "All");
   exception
      when An_Exception : Trade_Cant_Buy =>
         Show_Message
           (Text =>
              "You can't buy " & Exception_Message(X => An_Exception) &
              " in this " & Trader & ".",
            Title => "Can't buy items");
         return TCL_OK;
      when An_Exception : Trade_Not_For_Sale_Now =>
         Show_Message
           (Text =>
              "You can't buy " & Exception_Message(X => An_Exception) &
              " in this base at this moment.",
            Title => "Can't buy items");
         return TCL_OK;
      when An_Exception : Trade_Buying_Too_Much =>
         Show_Message
           (Text =>
              Trader & " don't have that much " &
              Exception_Message(X => An_Exception) & " for sale.",
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
              "You don't have any " & To_String(Source => Money_Name) &
              " to buy " & Exception_Message(X => An_Exception) & ".",
            Title => "No money to buy items");
         return TCL_OK;
      when An_Exception : Trade_Not_Enough_Money =>
         Show_Message
           (Text =>
              "You don't have enough " & To_String(Source => Money_Name) &
              " to buy so much " & Exception_Message(X => An_Exception) & ".",
            Title => "Not enough money to buy items");
         return TCL_OK;
      when Trade_Invalid_Amount =>
         if CArgv.Arg(Argv => Argv, N => 1) = "buy" then
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
              "You dont have that much " &
              Exception_Message(X => An_Exception) & " in ship cargo.",
            Title => "Not enough items for sale");
         return TCL_OK;
      when An_Exception : Trade_No_Money_In_Base =>
         Show_Message
           (Text =>
              "You can't sell so much " &
              Exception_Message(X => An_Exception) & " because " & Trader &
              " don't have that much " & To_String(Source => Money_Name) &
              " to buy it.",
            Title => "Too much items for sale");
         return TCL_OK;
      when Trade_No_Trader =>
         Show_Message
           (Text =>
              "You don't have assigned anyone in crew to talk in bases duty.",
            Title => "No trader assigned");
         return TCL_OK;
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
