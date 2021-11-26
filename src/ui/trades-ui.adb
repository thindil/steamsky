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
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
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

   -- ****iv* TUI/TUI.TradeTable
   -- FUNCTION
   -- Table with info about the available items to trade
   -- SOURCE
   TradeTable: Table_Widget (8);
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
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowTrade ?itemtype? ?searchstring?
   -- Itemtype is type of items to show, searchstring is string which is
   -- looking for in items names
   -- SOURCE
   function Show_Trade_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Trade_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      use Tiny_String;

      TradeFrame: Ttk_Frame := Get_Widget(Main_Paned & ".tradeframe", Interp);
      TradeCanvas: constant Tk_Canvas :=
        Get_Widget(TradeFrame & ".canvas", Interp);
      Label: Ttk_Label :=
        Get_Widget(TradeCanvas & ".trade.options.typelabel", Interp);
      ItemType, BaseType, ItemName, TradeInfo,
      ItemDurability: Unbounded_String;
      ProtoIndex: Bounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      Price: Positive;
      ComboBox: Ttk_ComboBox;
      BaseIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      BaseCargo: BaseCargo_Container.Vector;
      BaseCargoIndex, BaseAmount: Natural;
      IndexesList: Positive_Container.Vector;
      EventIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex;
      Profit: Integer;
      MoneyIndex2: constant Natural :=
        FindItem(Player_Ship.Cargo, Money_Index);
      SearchEntry: constant Ttk_Entry :=
        Get_Widget(TradeCanvas & ".trade.options.search", Interp);
      Page: constant Positive :=
        (if Argc = 4 then Positive'Value(CArgv.Arg(Argv, 3)) else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      Arguments: constant String :=
        (if Argc > 2 then
           "{" & CArgv.Arg(Argv, 1) & "} {" & CArgv.Arg(Argv, 2) & "}"
         elsif Argc = 2 then CArgv.Arg(Argv, 1) & " {}" else "All {}");
      Current_Item_Index: Positive := 1;
   begin
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "trade.tcl");
         Bind(TradeFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         TradeFrame := Get_Widget(TradeCanvas & ".trade");
         TradeTable :=
           CreateTable
             (Widget_Image(TradeFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Type"),
               To_Unbounded_String("Durability"), To_Unbounded_String("Price"),
               To_Unbounded_String("Profit"), To_Unbounded_String("Weight"),
               To_Unbounded_String("Owned"), To_Unbounded_String("Available")),
              Get_Widget(Main_Paned & ".tradeframe.scrolly"), "SortTradeItems",
              "Press mouse button to sort the items.");
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
         Items_Sort_Order := Default_Items_Sort_Order;
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         configure(Close_Button, "-command ShowSkyMap");
         if BaseIndex = 0 and EventIndex > 0 then
            Delete_Event(EventIndex);
         end if;
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Tcl_SetVar(Interp, "gamestate", "trade");
      if Argc < 3 then
         Delete(SearchEntry, "0", "end");
      end if;
      configure(Close_Button, "-command {ShowSkyMap ShowTrade}");
      TradeFrame.Name := New_String(TradeCanvas & ".trade");
      ComboBox := Get_Widget(TradeFrame & ".options.type", Interp);
      ClearTable(TradeTable);
      if BaseIndex > 0 then
         BaseType := Sky_Bases(BaseIndex).Base_Type;
         BaseCargo := Sky_Bases(BaseIndex).Cargo;
      else
         BaseType := To_Unbounded_String("0");
         BaseCargo := TraderCargo;
      end if;
      if Items_Sort_Order = Default_Items_Sort_Order then
         Items_Indexes.Clear;
         for I in Player_Ship.Cargo.Iterate loop
            Items_Indexes.Append(Inventory_Container.To_Index(I));
         end loop;
         Items_Indexes.Append(0);
         for I in BaseCargo.Iterate loop
            Items_Indexes.Append(BaseCargo_Container.To_Index(I));
         end loop;
      end if;
      Show_Cargo_Items_Loop :
      for I of Items_Indexes loop
         Current_Item_Index := Current_Item_Index + 1;
         exit Show_Cargo_Items_Loop when I = 0;
         if Get_Price(BaseType, Player_Ship.Cargo(I).ProtoIndex) = 0 then
            goto End_Of_Cargo_Loop;
         end if;
         ProtoIndex := Player_Ship.Cargo(I).ProtoIndex;
         BaseCargoIndex :=
           Find_Base_Cargo(ProtoIndex, Player_Ship.Cargo(I).Durability);
         if BaseCargoIndex > 0 then
            IndexesList.Append(New_Item => BaseCargoIndex);
         end if;
         ItemType :=
           (if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
              Items_List(ProtoIndex).IType
            else Items_List(ProtoIndex).ShowType);
         if Index(ItemsTypes, To_String("{" & ItemType & "}")) = 0 then
            Append(ItemsTypes, " {" & ItemType & "}");
         end if;
         if Argc > 1 and then CArgv.Arg(Argv, 1) /= "All"
           and then To_String(ItemType) /= CArgv.Arg(Argv, 1) then
            goto End_Of_Cargo_Loop;
         end if;
         ItemName :=
           To_Unbounded_String
             (GetItemName(Player_Ship.Cargo(I), False, False));
         if Argc = 3
           and then
             Index
               (To_Lower(To_String(ItemName)), To_Lower(CArgv.Arg(Argv, 2))) =
             0 then
            goto End_Of_Cargo_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Cargo_Loop;
         end if;
         if BaseCargoIndex = 0 then
            Price := Get_Price(BaseType, ProtoIndex);
         else
            Price :=
              (if BaseIndex > 0 then
                 Sky_Bases(BaseIndex).Cargo(BaseCargoIndex).Price
               else TraderCargo(BaseCargoIndex).Price);
         end if;
         if EventIndex > 0 then
            if Events_List(EventIndex).E_Type = DOUBLEPRICE
              and then Events_List(EventIndex).Item_Index = ProtoIndex then
               Price := Price * 2;
            end if;
         end if;
         Profit := Price - Player_Ship.Cargo(I).Price;
         BaseAmount := 0;
         if BaseCargoIndex > 0 and Is_Buyable(BaseType, ProtoIndex) then
            BaseAmount := BaseCargo(BaseCargoIndex).Amount;
         end if;
         AddButton
           (TradeTable, To_String(ItemName), "Show available options for item",
            "ShowTradeMenu" & Positive'Image(I), 1);
         AddButton
           (TradeTable, To_String(ItemType), "Show available options for item",
            "ShowTradeMenu" & Positive'Image(I), 2);
         ItemDurability :=
           (if Player_Ship.Cargo(I).Durability < 100 then
              To_Unbounded_String
                (GetItemDamage(Player_Ship.Cargo(I).Durability))
            else To_Unbounded_String("Unused"));
         AddProgressBar
           (TradeTable, Player_Ship.Cargo(I).Durability,
            Default_Item_Durability, To_String(ItemDurability),
            "ShowTradeMenu" & Positive'Image(I), 3);
         AddButton
           (TradeTable, Positive'Image(Price),
            "Show available options for item",
            "ShowTradeMenu" & Positive'Image(I), 4);
         AddButton
           (Table => TradeTable, Text => Positive'Image(Profit),
            Tooltip => "Show available options for item",
            Command => "ShowTradeMenu" & Positive'Image(I), Column => 5,
            Color =>
              (if Profit > 0 then "green" elsif Profit < 0 then "red"
               else ""));
         AddButton
           (TradeTable, Positive'Image(Items_List(ProtoIndex).Weight) & " kg",
            "Show available options for item",
            "ShowTradeMenu" & Positive'Image(I), 6);
         AddButton
           (TradeTable, Positive'Image(Player_Ship.Cargo(I).Amount),
            "Show available options for item",
            "ShowTradeMenu" & Positive'Image(I), 7);
         AddButton
           (TradeTable, Positive'Image(BaseAmount),
            "Show available options for item",
            "ShowTradeMenu" & Positive'Image(I), 8, True);
         exit Show_Cargo_Items_Loop when TradeTable.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Cargo_Loop>>
      end loop Show_Cargo_Items_Loop;
      Show_Trader_Items_Loop :
      for I in Current_Item_Index .. Items_Indexes.Last_Index loop
         exit Show_Trader_Items_Loop when TradeTable.Row =
           Game_Settings.Lists_Limit + 1;
         if IndexesList.Find_Index(Item => Items_Indexes(I)) > 0 or
           not Is_Buyable
             (Base_Type => BaseType,
              Item_Index => BaseCargo(Items_Indexes(I)).Proto_Index,
              Base_Index => BaseIndex) or
           BaseCargo(Items_Indexes(I)).Amount = 0 then
            goto End_Of_Trader_Loop;
         end if;
         ProtoIndex := BaseCargo(Items_Indexes(I)).Proto_Index;
         ItemType :=
           (if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
              Items_List(ProtoIndex).IType
            else Items_List(ProtoIndex).ShowType);
         if Index(ItemsTypes, To_String("{" & ItemType & "}")) = 0 then
            Append(ItemsTypes, " {" & ItemType & "}");
         end if;
         if Argc > 1 and then CArgv.Arg(Argv, 1) /= "All"
           and then To_String(ItemType) /= CArgv.Arg(Argv, 1) then
            goto End_Of_Trader_Loop;
         end if;
         ItemName := Items_List(ProtoIndex).Name;
         if Argc = 3
           and then
             Index
               (To_Lower(To_String(ItemName)), To_Lower(CArgv.Arg(Argv, 2))) =
             0 then
            goto End_Of_Trader_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Trader_Loop;
         end if;
         Price :=
           (if BaseIndex > 0 then
              Sky_Bases(BaseIndex).Cargo(Items_Indexes(I)).Price
            else TraderCargo(Items_Indexes(I)).Price);
         if EventIndex > 0 then
            if Events_List(EventIndex).E_Type = DOUBLEPRICE
              and then Events_List(EventIndex).Item_Index = ProtoIndex then
               Price := Price * 2;
            end if;
         end if;
         BaseAmount :=
           (if BaseIndex = 0 then TraderCargo(Items_Indexes(I)).Amount
            else Sky_Bases(BaseIndex).Cargo(Items_Indexes(I)).Amount);
         AddButton
           (TradeTable, To_String(ItemName), "Show available options for item",
            "ShowTradeMenu -" & Trim(Positive'Image(Items_Indexes(I)), Left),
            1);
         AddButton
           (TradeTable, To_String(ItemType), "Show available options for item",
            "ShowTradeMenu -" & Trim(Positive'Image(Items_Indexes(I)), Left),
            2);
         ItemDurability :=
           (if BaseCargo(Items_Indexes(I)).Durability < 100 then
              To_Unbounded_String
                (GetItemDamage(BaseCargo(Items_Indexes(I)).Durability))
            else To_Unbounded_String("Unused"));
         AddProgressBar
           (TradeTable, BaseCargo(Items_Indexes(I)).Durability,
            Default_Item_Durability, To_String(ItemDurability),
            "ShowTradeMenu -" & Trim(Positive'Image(Items_Indexes(I)), Left),
            3);
         AddButton
           (TradeTable, Positive'Image(Price),
            "Show available options for item",
            "ShowTradeMenu -" & Trim(Positive'Image(Items_Indexes(I)), Left),
            4);
         AddButton
           (TradeTable, Integer'Image(-(Price)),
            "Show available options for item",
            "ShowTradeMenu -" & Trim(Positive'Image(Items_Indexes(I)), Left),
            5, False, "red");
         AddButton
           (TradeTable, Positive'Image(Items_List(ProtoIndex).Weight) & " kg",
            "Show available options for item",
            "ShowTradeMenu -" & Trim(Positive'Image(Items_Indexes(I)), Left),
            6);
         AddButton
           (TradeTable, " 0", "Show available options for item",
            "ShowTradeMenu -" & Trim(Positive'Image(Items_Indexes(I)), Left),
            7);
         AddButton
           (TradeTable, Natural'Image(BaseAmount),
            "Show available options for item",
            "ShowTradeMenu -" & Trim(Positive'Image(Items_Indexes(I)), Left),
            8, True);
         <<End_Of_Trader_Loop>>
      end loop Show_Trader_Items_Loop;
      if Page > 1 then
         if TradeTable.Row < Game_Settings.Lists_Limit + 1 then
            AddPagination
              (TradeTable, "ShowTrade " & Arguments & Positive'Image(Page - 1),
               "");
         else
            AddPagination
              (TradeTable, "ShowTrade " & Arguments & Positive'Image(Page - 1),
               "ShowTrade " & Arguments & Positive'Image(Page + 1));
         end if;
      elsif TradeTable.Row = Game_Settings.Lists_Limit + 1 then
         AddPagination
           (TradeTable, "",
            "ShowTrade " & Arguments & Positive'Image(Page + 1));
      end if;
      UpdateTable
        (TradeTable, (if Focus = Widget_Image(SearchEntry) then False));
      Tcl_Eval(Get_Context, "update");
      configure(ComboBox, "-values [list " & To_String(ItemsTypes) & "]");
      if Argc = 1 then
         Current(ComboBox, "0");
      end if;
      if MoneyIndex2 > 0 then
         TradeInfo :=
           To_Unbounded_String
             ("You have" &
              Natural'Image(Player_Ship.Cargo(MoneyIndex2).Amount) & " " &
              To_String(Money_Name) & ".");
      else
         TradeInfo :=
           To_Unbounded_String
             ("You don't have any " & To_String(Money_Name) &
              " to buy anything.");
      end if;
      declare
         FreeSpace: Integer := FreeCargo(0);
      begin
         if FreeSpace < 0 then
            FreeSpace := 0;
         end if;
         Append
           (TradeInfo,
            LF & "Free cargo space:" & Integer'Image(FreeSpace) & " kg.");
      end;
      Label.Name := New_String(TradeFrame & ".options.playerinfo");
      configure(Label, "-text {" & To_String(TradeInfo) & "}");
      TradeInfo := Null_Unbounded_String;
      if BaseIndex > 0 then
         if Sky_Bases(BaseIndex).Cargo(1).Amount = 0 then
            Append
              (TradeInfo,
               "Base doesn't have any " & To_String(Money_Name) &
               "to buy anything.");
         else
            Append
              (TradeInfo,
               "Base has" &
               Positive'Image(Sky_Bases(BaseIndex).Cargo(1).Amount) & " " &
               To_String(Money_Name) & ".");
         end if;
      else
         if TraderCargo(1).Amount = 0 then
            Append
              (TradeInfo,
               "Ship doesn't have any " & To_String(Money_Name) &
               "to buy anything.");
         else
            Append
              (TradeInfo,
               "Ship has" & Positive'Image(TraderCargo(1).Amount) & " " &
               To_String(Money_Name) & ".");
         end if;
      end if;
      Label.Name := New_String(TradeFrame & ".options.baseinfo");
      configure(Label, "-text {" & To_String(TradeInfo) & "}");
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      configure
        (TradeCanvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (TradeCanvas, "window", "0 0 -anchor nw -window " & TradeFrame);
      Tcl_Eval(Get_Context, "update");
      configure
        (TradeCanvas, "-scrollregion [list " & BBox(TradeCanvas, "all") & "]");
      Xview_Move_To(TradeCanvas, "0.0");
      Yview_Move_To(TradeCanvas, "0.0");
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
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowTradeItemInfo
   -- SOURCE
   function Show_Trade_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Trade_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      use Tiny_String;

      ItemInfo: Unbounded_String;
      ProtoIndex: Bounded_String;
      CargoIndex, BaseCargoIndex: Natural := 0;
      BaseIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      ItemTypes: constant array(1 .. 6) of Unbounded_String :=
        (Weapon_Type, Chest_Armor, Head_Armor, Arms_Armor, Legs_Armor,
         Shield_Type);
   begin
      if ItemIndex < 0 then
         BaseCargoIndex := abs (ItemIndex);
      else
         CargoIndex := ItemIndex;
      end if;
      if CargoIndex > Natural(Player_Ship.Cargo.Length) then
         return TCL_OK;
      end if;
      if BaseIndex = 0 and BaseCargoIndex > Natural(TraderCargo.Length) then
         return TCL_OK;
      elsif BaseIndex > 0
        and then BaseCargoIndex >
          Natural(Sky_Bases(BaseIndex).Cargo.Length) then
         return TCL_OK;
      end if;
      if CargoIndex > 0 then
         ProtoIndex := Player_Ship.Cargo(CargoIndex).ProtoIndex;
      else
         ProtoIndex :=
           (if BaseIndex = 0 then TraderCargo(BaseCargoIndex).Proto_Index
            else Sky_Bases(BaseIndex).Cargo(BaseCargoIndex).Proto_Index);
      end if;
      if Items_List(ProtoIndex).IType = Weapon_Type then
         Append
           (ItemInfo,
            "Skill: " &
            To_String
              (SkillsData_Container.Element
                 (Skills_List, Items_List(ProtoIndex).Value(3))
                 .Name) &
            "/" &
            To_String
              (AttributesData_Container.Element
                 (Attributes_List,
                  SkillsData_Container.Element
                    (Skills_List, Items_List(ProtoIndex).Value(3))
                    .Attribute)
                 .Name) &
            (if Items_List(ProtoIndex).Value(4) = 1 then
               LF & "Can be used with shield."
             else LF & "Can't be used with shield (two-handed weapon).") &
            LF & "Damage type: ");
         case Items_List(ProtoIndex).Value(5) is
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
         if Items_List(ProtoIndex).IType = ItemType then
            if ItemInfo /= Null_Unbounded_String then
               Append(ItemInfo, LF);
            end if;
            Append
              (ItemInfo,
               "Damage chance: " &
               GetItemChanceToDamage(Items_List(ProtoIndex).Value(1)) & LF &
               "Strength:" & Integer'Image(Items_List(ProtoIndex).Value(2)));
            exit Show_More_Info_Loop;
         end if;
      end loop Show_More_Info_Loop;
      if Tools_List.Contains(Items_List(ProtoIndex).IType) then
         if ItemInfo /= Null_Unbounded_String then
            Append(ItemInfo, LF);
         end if;
         Append
           (ItemInfo,
            "Damage chance: " &
            GetItemChanceToDamage(Items_List(ProtoIndex).Value(1)));
      end if;
      if Length(Items_List(ProtoIndex).IType) > 4
        and then
        (Slice(Items_List(ProtoIndex).IType, 1, 4) = "Ammo" or
         Items_List(ProtoIndex).IType = To_Unbounded_String("Harpoon")) then
         if ItemInfo /= Null_Unbounded_String then
            Append(ItemInfo, LF);
         end if;
         Append
           (ItemInfo,
            "Strength:" & Integer'Image(Items_List(ProtoIndex).Value(1)));
      end if;
      if Items_List(ProtoIndex).Description /= Null_Unbounded_String then
         if ItemInfo /= Null_Unbounded_String then
            Append(ItemInfo, LF & LF);
         end if;
         Append(ItemInfo, Items_List(ProtoIndex).Description);
      end if;
      ShowInfo
        (Text => To_String(ItemInfo),
         Title => To_String(Items_List(ProtoIndex).Name));
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
      use Tiny_String;

      BaseIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      BaseCargoIndex, CargoIndex: Natural := 0;
      Trader: String(1 .. 4);
      ProtoIndex: Bounded_String;
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
         ProtoIndex := Player_Ship.Cargo(CargoIndex).ProtoIndex;
         if BaseCargoIndex = 0 then
            BaseCargoIndex := Find_Base_Cargo(ProtoIndex);
         end if;
      else
         ProtoIndex :=
           (if BaseIndex = 0 then TraderCargo(BaseCargoIndex).Proto_Index
            else Sky_Bases(BaseIndex).Cargo(BaseCargoIndex).Proto_Index);
      end if;
      Trader := (if BaseIndex > 0 then "base" else "ship");
      if Argc > 2 then
         if CArgv.Arg(Argv, 1) in "buy" then
            BuyItems(BaseCargoIndex, CArgv.Arg(Argv, 2));
         else
            SellItems(CargoIndex, CArgv.Arg(Argv, 2));
         end if;
      else
         if CArgv.Arg(Argv, 1) in "buy" then
            BuyItems(BaseCargoIndex, Get(AmountBox));
         else
            SellItems(CargoIndex, Get(AmountBox));
         end if;
         if Close_Dialog_Command
             (ClientData, Interp, 2,
              CArgv.Empty & "CloseDialog" & ".itemdialog") =
           TCL_ERROR then
            return TCL_ERROR;
         end if;
      end if;
      UpdateHeader;
      Update_Messages;
      return
        Show_Trade_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowTrade" & Get(TypeBox));
   exception
      when An_Exception : Trade_Cant_Buy =>
         ShowMessage
           (Text =>
              "You can't buy " & Exception_Message(An_Exception) &
              " in this " & Trader & ".",
            Title => "Can't buy items");
         return TCL_OK;
      when An_Exception : Trade_Not_For_Sale_Now =>
         ShowMessage
           (Text =>
              "You can't buy " & Exception_Message(An_Exception) &
              " in this base at this moment.",
            Title => "Can't buy items");
         return TCL_OK;
      when An_Exception : Trade_Buying_Too_Much =>
         ShowMessage
           (Text =>
              Trader & " don't have that much " &
              Exception_Message(An_Exception) & " for sale.",
            Title => "Not enough items");
         return TCL_OK;
      when Trade_No_Free_Cargo =>
         ShowMessage
           (Text => "You don't have that much free space in your ship cargo.",
            Title => "No free cargo space");
         return TCL_OK;
      when An_Exception : Trade_No_Money =>
         ShowMessage
           (Text =>
              "You don't have any " & To_String(Money_Name) & " to buy " &
              Exception_Message(An_Exception) & ".",
            Title => "No money to buy items");
         return TCL_OK;
      when An_Exception : Trade_Not_Enough_Money =>
         ShowMessage
           (Text =>
              "You don't have enough " & To_String(Money_Name) &
              " to buy so much " & Exception_Message(An_Exception) & ".",
            Title => "Not enough money to buy items");
         return TCL_OK;
      when Trade_Invalid_Amount =>
         if CArgv.Arg(Argv, 1) = "buy" then
            ShowMessage
              (Text => "You entered invalid amount to buy.",
               Title => "Invalid amount of items");
         else
            ShowMessage
              (Text => "You entered invalid amount to sell.",
               Title => "Invalid amount of items");
         end if;
         return TCL_OK;
      when An_Exception : Trade_Too_Much_For_Sale =>
         ShowMessage
           (Text =>
              "You dont have that much " & Exception_Message(An_Exception) &
              " in ship cargo.",
            Title => "Not enough items for sale");
         return TCL_OK;
      when An_Exception : Trade_No_Money_In_Base =>
         ShowMessage
           (Text =>
              "You can't sell so much " & Exception_Message(An_Exception) &
              " because " & Trader & " don't have that much " &
              To_String(Money_Name) & " to buy it.",
            Title => "Too much items for sale");
         return TCL_OK;
      when Trade_No_Trader =>
         ShowMessage
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

   -- ****o* TUI/TUI.Show_Trade_Menu_Command
   -- FUNCTION
   -- Show trade menu with buy/sell options for the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowTradeMenu itemindex
   -- ItemIndex is the index of the item which menu will be show. If index
   -- starts with minus means item in base/trader cargo only. Otherwise it is
   -- index in the player ship cargo.
   -- SOURCE
   function Show_Trade_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Trade_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      MoneyIndex2: constant Natural :=
        FindItem(Player_Ship.Cargo, Money_Index);
      BaseIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      BaseCargoIndex2, Price: Natural;
      BaseType: Unbounded_String;
      ProtoIndex: Bounded_String;
      Trade_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".trademenu", Title => "Item actions", Parent_Name => ".");
      Can_Sell, Can_Buy: Boolean := False;
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Trade_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Trade_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Trade_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script =>
                 "{focus " & Trade_Menu & "." &
                 (if Can_Sell then "sell" elsif Can_Buy then "buy"
                  else "info") &
                 ";break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      ItemIndex := Integer'Value(CArgv.Arg(Argv, 1));
      BaseType :=
        (if BaseIndex > 0 then Sky_Bases(BaseIndex).Base_Type
         else To_Unbounded_String("0"));
      if ItemIndex > 0 then
         ProtoIndex := Player_Ship.Cargo(ItemIndex).ProtoIndex;
         BaseCargoIndex2 := Find_Base_Cargo(ProtoIndex);
         Change_Title
           (Trade_Menu,
            GetItemName(Player_Ship.Cargo(ItemIndex), False, False) &
            " actions");
      else
         BaseCargoIndex2 := abs (ItemIndex);
         ProtoIndex :=
           (if BaseIndex = 0 then TraderCargo(BaseCargoIndex2).Proto_Index
            else Sky_Bases(BaseIndex).Cargo(BaseCargoIndex2).Proto_Index);
         Change_Title
           (Trade_Menu, To_String(Items_List(ProtoIndex).Name) & " actions");
      end if;
      if ItemIndex > 0 then
         if BaseCargoIndex2 > 0 then
            Price :=
              (if BaseIndex > 0 then
                 Sky_Bases(BaseIndex).Cargo(BaseCargoIndex2).Price
               else TraderCargo(BaseCargoIndex2).Price);
         else
            Price := Get_Price(BaseType, ProtoIndex);
         end if;
      else
         Price :=
           (if BaseIndex > 0 then
              Sky_Bases(BaseIndex).Cargo(BaseCargoIndex2).Price
            else TraderCargo(BaseCargoIndex2).Price);
      end if;
      declare
         EventIndex: constant Natural :=
           SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex;
      begin
         if EventIndex > 0 then
            if Events_List(EventIndex).E_Type = DOUBLEPRICE
              and then Events_List(EventIndex).Item_Index = ProtoIndex then
               Price := Price * 2;
            end if;
         end if;
      end;
      if ItemIndex > 0 then
         declare
            MaxSellAmount: Integer := Player_Ship.Cargo(ItemIndex).Amount;
            MaxPrice: Natural := MaxSellAmount * Price;
            Weight: Integer;
         begin
            Count_Price(MaxPrice, FindMember(TALK), False);
            if BaseIndex > 0
              and then MaxPrice > Sky_Bases(BaseIndex).Cargo(1).Amount then
               MaxSellAmount :=
                 Natural
                   (Float'Floor
                      (Float(MaxSellAmount) *
                       (Float(Sky_Bases(BaseIndex).Cargo(1).Amount) /
                        Float(MaxPrice))));
            elsif BaseIndex = 0 and then MaxPrice > TraderCargo(1).Amount then
               MaxSellAmount :=
                 Natural
                   (Float'Floor
                      (Float(MaxSellAmount) *
                       (Float(TraderCargo(1).Amount) / Float(MaxPrice))));
            end if;
            MaxPrice := MaxSellAmount * Price;
            if MaxPrice > 0 then
               Count_Price(MaxPrice, FindMember(TALK), False);
            end if;
            Weight :=
              FreeCargo
                ((Items_List(ProtoIndex).Weight * MaxSellAmount) - MaxPrice);
            Count_Sell_Amount_loop :
            while Weight < 0 loop
               MaxSellAmount :=
                 Integer
                   (Float'Floor
                      (Float(MaxSellAmount) *
                       (Float(MaxPrice + Weight) / Float(MaxPrice))));
               exit Count_Sell_Amount_loop when MaxSellAmount < 1;
               MaxPrice := MaxSellAmount * Price;
               Count_Price(MaxPrice, FindMember(TALK), False);
               Weight :=
                 FreeCargo
                   ((Items_List(ProtoIndex).Weight * MaxSellAmount) -
                    MaxPrice);
            end loop Count_Sell_Amount_loop;
            if MaxSellAmount > 0 then
               Can_Sell := True;
               Add_Button
                 (Name => ".sell", Label => "Sell selected amount",
                  Command =>
                    "TradeAmount sell" & Natural'Image(MaxSellAmount) &
                    Natural'Image(Price));
               Add_Button
                 (Name => ".sellall",
                  Label => "Sell" & Natural'Image(MaxSellAmount) & " of them",
                  Command =>
                    "TradeAmount sell" & Natural'Image(MaxSellAmount));
            end if;
         end;
      end if;
      if BaseCargoIndex2 > 0 and MoneyIndex2 > 0 and
        Is_Buyable(BaseType, ProtoIndex) then
         declare
            MaxBuyAmount: Integer :=
              Player_Ship.Cargo(MoneyIndex2).Amount / Price;
            MaxPrice: Natural := MaxBuyAmount * Price;
            Weight: Integer;
         begin
            if MaxBuyAmount > 0 then
               Count_Price(MaxPrice, FindMember(TALK));
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
                   Sky_Bases(BaseIndex).Cargo(BaseCargoIndex2).Amount then
                  MaxBuyAmount :=
                    Sky_Bases(BaseIndex).Cargo(BaseCargoIndex2).Amount;
               elsif BaseIndex = 0
                 and then MaxBuyAmount >
                   TraderCargo(BaseCargoIndex2).Amount then
                  MaxBuyAmount := TraderCargo(BaseCargoIndex2).Amount;
               end if;
               MaxPrice := MaxBuyAmount * Price;
               Count_Price(MaxPrice, FindMember(TALK));
               Weight :=
                 FreeCargo
                   (MaxPrice - (Items_List(ProtoIndex).Weight * MaxBuyAmount));
               Count_Buy_Amount_Loop :
               while Weight < 0 loop
                  MaxBuyAmount :=
                    MaxBuyAmount + (Weight / Items_List(ProtoIndex).Weight) -
                    1;
                  if MaxBuyAmount < 0 then
                     MaxBuyAmount := 0;
                  end if;
                  exit Count_Buy_Amount_Loop when MaxBuyAmount = 0;
                  MaxPrice := MaxBuyAmount * Price;
                  Count_Price(MaxPrice, FindMember(TALK));
                  Weight :=
                    FreeCargo
                      (MaxPrice -
                       (Items_List(ProtoIndex).Weight * MaxBuyAmount));
               end loop Count_Buy_Amount_Loop;
               if MaxBuyAmount > 0 then
                  Can_Buy := True;
                  Add_Button
                    (Name => ".buy", Label => "Buy selected amount",
                     Command =>
                       "TradeAmount buy" & Natural'Image(MaxBuyAmount) &
                       Natural'Image(Price));
                  Add_Button
                    (Name => ".buyall",
                     Label => "Buy" & Natural'Image(MaxBuyAmount) & " of them",
                     Command =>
                       "TradeAmount buy" & Natural'Image(MaxBuyAmount));
               end if;
            end if;
         end;
      end if;
      Add_Button
        (Name => ".info", Label => "Show more info about the item",
         Command => "ShowTradeItemInfo");
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Trade_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Trade_Menu_Command;

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
      BaseIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
   begin
      if CArgv.Arg(Argv, 1) = "sell" then
         ShowManipulateItem
           ("Sell " & GetItemName(Player_Ship.Cargo(ItemIndex)),
            "TradeItem sell", "sell", ItemIndex,
            Natural'Value(CArgv.Arg(Argv, 2)),
            Natural'Value(CArgv.Arg(Argv, 3)));
      else
         if ItemIndex > 0 then
            ShowManipulateItem
              ("Buy " & GetItemName(Player_Ship.Cargo(ItemIndex)),
               "TradeItem buy", "buy", ItemIndex,
               Natural'Value(CArgv.Arg(Argv, 2)),
               Natural'Value(CArgv.Arg(Argv, 3)));
         else
            if BaseIndex > 0 then
               ShowManipulateItem
                 ("Buy " &
                  To_String
                    (Items_List
                       (Sky_Bases(BaseIndex).Cargo(abs (ItemIndex))
                          .Proto_Index)
                       .Name),
                  "TradeItem buy", "buy", abs (ItemIndex),
                  Natural'Value(CArgv.Arg(Argv, 2)),
                  Natural'Value(CArgv.Arg(Argv, 3)));
            else
               ShowManipulateItem
                 ("Buy " &
                  To_String
                    (Items_List(TraderCargo(abs (ItemIndex)).Proto_Index)
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
        Get_Column_Number(TradeTable, Natural'Value(CArgv.Arg(Argv, 1)));
      type Local_Item_Data is record
         Name: Unbounded_String;
         IType: Unbounded_String;
         Damage: Float;
         Price: Natural;
         Profit: Integer;
         Weight: Positive;
         Owned: Natural;
         Available: Natural;
         Id: Positive;
      end record;
      BaseIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).BaseIndex;
      Indexes_List: Positive_Container.Vector;
      BaseCargo: BaseCargo_Container.Vector;
      BaseCargoIndex, Price: Natural;
      BaseType: Unbounded_String;
      ProtoIndex: Bounded_String;
      EventIndex: constant Natural :=
        SkyMap(Player_Ship.Sky_X, Player_Ship.Sky_Y).EventIndex;
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
         BaseCargo := Sky_Bases(BaseIndex).Cargo;
         BaseType := Sky_Bases(BaseIndex).Base_Type;
      else
         BaseCargo := TraderCargo;
         BaseType := To_Unbounded_String("0");
      end if;
      for I in Player_Ship.Cargo.Iterate loop
         ProtoIndex := Player_Ship.Cargo(I).ProtoIndex;
         BaseCargoIndex :=
           Find_Base_Cargo(ProtoIndex, Player_Ship.Cargo(I).Durability);
         if BaseCargoIndex > 0 then
            Indexes_List.Append(New_Item => BaseCargoIndex);
            Price := BaseCargo(BaseCargoIndex).Price;
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
              (Name => To_Unbounded_String(GetItemName(Player_Ship.Cargo(I))),
               IType =>
                 (if Items_List(ProtoIndex).ShowType = Null_Unbounded_String
                  then Items_List(ProtoIndex).IType
                  else Items_List(ProtoIndex).ShowType),
               Damage =>
                 Float(Player_Ship.Cargo(I).Durability) /
                 Float(Default_Item_Durability),
               Price => Price, Profit => Price - Player_Ship.Cargo(I).Price,
               Weight => Items_List(ProtoIndex).Weight,
               Owned => Player_Ship.Cargo(I).Amount,
               Available =>
                 (if BaseCargoIndex > 0 then BaseCargo(BaseCargoIndex).Amount
                  else 0),
               Id => Inventory_Container.To_Index(I)));
      end loop;
      Sort_Items.Sort(Local_Items);
      Items_Indexes.Clear;
      for Item of Local_Items loop
         Items_Indexes.Append(Item.Id);
      end loop;
      Items_Indexes.Append(0);
      Local_Items.Clear;
      for I in BaseCargo.First_Index .. BaseCargo.Last_Index loop
         if Indexes_List.Find_Index(Item => I) = 0 then
            ProtoIndex := BaseCargo(I).Proto_Index;
            Price := BaseCargo(I).Price;
            if EventIndex > 0 then
               if Events_List(EventIndex).E_Type = DOUBLEPRICE
                 and then Events_List(EventIndex).Item_Index = ProtoIndex then
                  Price := Price * 2;
               end if;
            end if;
            Local_Items.Append
              (New_Item =>
                 (Name => Items_List(ProtoIndex).Name,
                  IType =>
                    (if Items_List(ProtoIndex).ShowType = Null_Unbounded_String
                     then Items_List(ProtoIndex).IType
                     else Items_List(ProtoIndex).ShowType),
                  Damage =>
                    Float(BaseCargo(I).Durability) /
                    Float(Default_Item_Durability),
                  Price => Price, Profit => -(Price),
                  Weight => Items_List(ProtoIndex).Weight, Owned => 0,
                  Available => BaseCargo(I).Amount, Id => I));
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

   procedure AddCommands is
   begin
      Add_Command("ShowTrade", Show_Trade_Command'Access);
      Add_Command("ShowTradeItemInfo", Show_Trade_Item_Info_Command'Access);
      Add_Command("TradeItem", Trade_Item_Command'Access);
      Add_Command("SearchTrade", Search_Trade_Command'Access);
      Add_Command("ShowTradeMenu", Show_Trade_Menu_Command'Access);
      Add_Command("TradeAmount", Trade_Amount_Command'Access);
      Add_Command("SortTradeItems", Sort_Items_Command'Access);
   end AddCommands;

end Trades.UI;
