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
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases.Cargo; use Bases.Cargo;
with BasesTypes; use BasesTypes;
with Crew; use Crew;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Utils.UI; use Utils.UI;

package body Trades.UI is

   -- ****o* TUI/Show_Trade_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Trade_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      TradeFrame: Ttk_Frame := Get_Widget(Paned & ".tradeframe", Interp);
      TradeCanvas: constant Tk_Canvas :=
        Get_Widget(TradeFrame & ".canvas", Interp);
      Label: Ttk_Label :=
        Get_Widget(TradeCanvas & ".trade.options.typelabel", Interp);
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      ItemType, ProtoIndex, BaseType, ItemName, ProgressBarStyle,
      TradeInfo: Unbounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      Price: Positive;
      ComboBox: Ttk_ComboBox;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargo: BaseCargo_Container.Vector;
      BaseCargoIndex, BaseAmount: Natural;
      IndexesList: Positive_Container.Vector;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      Profit: Integer;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Item: Ttk_Frame;
      ItemButton: Ttk_Button;
      Row: Positive := 1;
      DurabilityBar: Ttk_ProgressBar;
      MoneyIndex2: constant Natural := FindItem(PlayerShip.Cargo, MoneyIndex);
   begin
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "trade.tcl");
         Bind(TradeFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp trade}");
      TradeFrame.Name := New_String(TradeCanvas & ".trade");
      ComboBox := Get_Widget(TradeFrame & ".options.type", Interp);
      TradeFrame.Name := New_String(TradeCanvas & ".trade.list");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(TradeFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (TradeFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item := Get_Widget(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      if BaseIndex > 0 then
         BaseType := SkyBases(BaseIndex).BaseType;
         BaseCargo := SkyBases(BaseIndex).Cargo;
      else
         BaseType := To_Unbounded_String("0");
         BaseCargo := TraderCargo;
      end if;
      for I in PlayerShip.Cargo.Iterate loop
         if Get_Price(BaseType, PlayerShip.Cargo(I).ProtoIndex) = 0 then
            goto End_Of_Cargo_Loop;
         end if;
         ProtoIndex := PlayerShip.Cargo(I).ProtoIndex;
         BaseCargoIndex :=
           FindBaseCargo(ProtoIndex, PlayerShip.Cargo(I).Durability);
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
           To_Unbounded_String(GetItemName(PlayerShip.Cargo(I), False, False));
         if Argc = 3
           and then
             Index
               (To_Lower(To_String(ItemName)), To_Lower(CArgv.Arg(Argv, 2))) =
             0 then
            goto End_Of_Cargo_Loop;
         end if;
         if BaseCargoIndex = 0 then
            Price := Get_Price(BaseType, ProtoIndex);
         else
            Price :=
              (if BaseIndex > 0 then
                 SkyBases(BaseIndex).Cargo(BaseCargoIndex).Price
               else TraderCargo(BaseCargoIndex).Price);
         end if;
         if EventIndex > 0 then
            if Events_List(EventIndex).EType = DoublePrice
              and then Events_List(EventIndex).ItemIndex = ProtoIndex then
               Price := Price * 2;
            end if;
         end if;
         Profit := Price - PlayerShip.Cargo(I).Price;
         BaseAmount := 0;
         if BaseCargoIndex > 0 and Is_Buyable(BaseType, ProtoIndex) then
            BaseAmount :=
              (if BaseIndex = 0 then TraderCargo(BaseCargoIndex).Amount
               else SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount);
         end if;
         ItemButton :=
           Create
             (TradeFrame & ".item" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & To_String(ItemName) & "} -command {ShowTradeMenu" &
              Positive'Image(Inventory_Container.To_Index(I)) & "}");
         Add(ItemButton, "Show available item options");
         Tcl.Tk.Ada.Grid.Grid
           (ItemButton, "-row" & Positive'Image(Row) & " -sticky w");
         Label :=
           Create
             (TradeFrame & ".type" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & To_String(ItemType) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 1 -sticky w");
         ProgressBarStyle :=
           (if PlayerShip.Cargo(I).Durability > 74 then
              To_Unbounded_String(" -style green.Horizontal.TProgressbar")
            elsif PlayerShip.Cargo(I).Durability > 24 then
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar")
            else To_Unbounded_String(" -style Horizontal.TProgressbar"));
         DurabilityBar :=
           Create
             (TradeFrame & ".durability" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(PlayerShip.Cargo(I).Durability) &
              "}" & To_String(ProgressBarStyle));
         Tcl.Tk.Ada.Grid.Grid
           (DurabilityBar, "-row" & Positive'Image(Row) & " -column 2");
         Label :=
           Create
             (TradeFrame & ".price" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & Positive'Image(Price) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 3 -sticky w");
         if Profit > 0 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style Headergreen.TLabel");
         elsif Profit < 0 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style Headerred.TLabel");
         else
            ProgressBarStyle := To_Unbounded_String(" -style TLabel");
         end if;
         Label :=
           Create
             (TradeFrame & ".profit" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & Integer'Image(Profit) & "}" &
              To_String(ProgressBarStyle));
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 4 -sticky w");
         Label :=
           Create
             (TradeFrame & ".owned" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & Positive'Image(PlayerShip.Cargo(I).Amount) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 5 -sticky w");
         Label :=
           Create
             (TradeFrame & ".available" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & Natural'Image(BaseAmount) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 6 -sticky w");
         Row := Row + 1;
         <<End_Of_Cargo_Loop>>
      end loop;
      for I in BaseCargo.First_Index .. BaseCargo.Last_Index loop
         if IndexesList.Find_Index(Item => I) > 0 or
           not Is_Buyable
             (BaseType => BaseType, ItemIndex => BaseCargo(I).ProtoIndex,
              BaseIndex => BaseIndex) then
            goto End_Of_Trader_Loop;
         end if;
         ProtoIndex := BaseCargo(I).ProtoIndex;
         ItemType :=
           (if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
              Items_List(ProtoIndex).IType
            else Items_List(ProtoIndex).ShowType);
         if Index(ItemsTypes, To_String("{" & ItemType & "}")) = 0 then
            Append(ItemsTypes, " {" & ItemType & "}");
         end if;
         if Argc = 2 and then CArgv.Arg(Argv, 1) /= "All"
           and then To_String(ItemType) /= CArgv.Arg(Argv, 1) then
            goto End_Of_Trader_Loop;
         end if;
         ItemName := Items_List(ProtoIndex).Name;
         if Argc = 3 and then Index(ItemName, CArgv.Arg(Argv, 2)) = 0 then
            goto End_Of_Trader_Loop;
         end if;
         Price :=
           (if BaseIndex > 0 then SkyBases(BaseIndex).Cargo(I).Price
            else TraderCargo(I).Price);
         if EventIndex > 0 then
            if Events_List(EventIndex).EType = DoublePrice
              and then Events_List(EventIndex).ItemIndex = ProtoIndex then
               Price := Price * 2;
            end if;
         end if;
         BaseAmount :=
           (if BaseIndex = 0 then TraderCargo(I).Amount
            else SkyBases(BaseIndex).Cargo(I).Amount);
         ItemButton :=
           Create
             (TradeFrame & ".item-" & Trim(Positive'Image(I), Left),
              "-text {" & To_String(ItemName) & "} -command {ShowTradeMenu -" &
              Trim(Positive'Image(I), Left) & "}");
         Add(ItemButton, "Show available item options");
         Tcl.Tk.Ada.Grid.Grid
           (ItemButton, "-row" & Positive'Image(Row) & " -sticky w");
         Label :=
           Create
             (TradeFrame & ".typeb" & Trim(Positive'Image(I), Left),
              "-text {" & To_String(ItemType) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 1 -sticky w");
         ProgressBarStyle :=
           (if BaseCargo(I).Durability > 74 then
              To_Unbounded_String(" -style green.Horizontal.TProgressbar")
            elsif BaseCargo(I).Durability > 24 then
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar")
            else To_Unbounded_String(" -style Horizontal.TProgressbar"));
         DurabilityBar :=
           Create
             (TradeFrame & ".durabilityb" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(BaseCargo(I).Durability) & "}" &
              To_String(ProgressBarStyle));
         Tcl.Tk.Ada.Grid.Grid
           (DurabilityBar, "-row" & Positive'Image(Row) & " -column 2");
         Label :=
           Create
             (TradeFrame & ".priceb" & Trim(Positive'Image(I), Left),
              "-text {" & Positive'Image(Price) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 3 -sticky w");
         Label :=
           Create
             (TradeFrame & ".profitb" & Trim(Positive'Image(I), Left),
              "-text {" & Integer'Image(-(Price)) &
              "} -style Headerred.TLabel");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 4 -sticky w");
         Label :=
           Create
             (TradeFrame & ".ownedb" & Trim(Positive'Image(I), Left),
              "-text { 0}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 5 -sticky w");
         Label :=
           Create
             (TradeFrame & ".availableb" & Trim(Positive'Image(I), Left),
              "-text {" & Natural'Image(BaseAmount) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Positive'Image(Row) & " -column 6 -sticky w");
         Row := Row + 1;
         <<End_Of_Trader_Loop>>
      end loop;
      configure(ComboBox, "-values [list " & To_String(ItemsTypes) & "]");
      if Argc = 1 then
         Current(ComboBox, "0");
      end if;
      if MoneyIndex2 > 0 then
         TradeInfo :=
           To_Unbounded_String
             ("You have" &
              Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) & " " &
              To_String(MoneyName) & ".");
      else
         TradeInfo :=
           To_Unbounded_String
             ("You don't have any " & To_String(MoneyName) &
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
      Label.Name :=
        New_String
          (".gameframe.paned.tradeframe.canvas.trade.options.playerinfo");
      configure(Label, "-text {" & To_String(TradeInfo) & "}");
      TradeInfo := Null_Unbounded_String;
      if BaseIndex > 0 then
         if SkyBases(BaseIndex).Cargo(1).Amount = 0 then
            Append
              (TradeInfo,
               "Base don't have any " & To_String(MoneyName) &
               "to buy anything.");
         else
            Append
              (TradeInfo,
               "Base have" &
               Positive'Image(SkyBases(BaseIndex).Cargo(1).Amount) & " " &
               To_String(MoneyName) & ".");
         end if;
      else
         if TraderCargo(1).Amount = 0 then
            Append
              (TradeInfo,
               "Ship don't have any " & To_String(MoneyName) &
               "to buy anything.");
         else
            Append
              (TradeInfo,
               "Ship have" & Positive'Image(TraderCargo(1).Amount) & " " &
               To_String(MoneyName) & ".");
         end if;
      end if;
      Label.Name :=
        New_String
          (".gameframe.paned.tradeframe.canvas.trade.options.baseinfo");
      configure(Label, "-text {" & To_String(TradeInfo) & "}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      TradeFrame.Name := New_String(Widget_Image(TradeCanvas) & ".trade");
      configure
        (TradeCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (TradeCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(TradeFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (TradeCanvas, "-scrollregion [list " & BBox(TradeCanvas, "all") & "]");
      ShowScreen("tradeframe");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Trade_Command;

   -- ****if* TUI/ItemIndex
   -- FUNCTION
   -- Index of the currently selected item
   -- SOURCE
   ItemIndex: Integer;
   -- ****

   -- ****o* TUI/Show_Trade_Item_Info_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Trade_Item_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      ItemInfo, ProtoIndex: Unbounded_String;
      CargoIndex, BaseCargoIndex, BaseCargoIndex2: Natural := 0;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: Unbounded_String;
      Price: Natural;
      ItemTypes: constant array(Positive range <>) of Unbounded_String :=
        (WeaponType, ChestArmor, HeadArmor, ArmsArmor, LegsArmor, ShieldType);
   begin
      if ItemIndex < 0 then
         BaseCargoIndex := abs (ItemIndex);
      else
         CargoIndex := ItemIndex;
      end if;
      if CargoIndex > Natural(PlayerShip.Cargo.Length) then
         return TCL_OK;
      end if;
      BaseType :=
        (if BaseIndex > 0 then SkyBases(BaseIndex).BaseType
         else To_Unbounded_String("0"));
      if BaseIndex = 0 and BaseCargoIndex > Natural(TraderCargo.Length) then
         return TCL_OK;
      elsif BaseIndex > 0
        and then BaseCargoIndex >
          Natural(SkyBases(BaseIndex).Cargo.Length) then
         return TCL_OK;
      end if;
      if CargoIndex > 0 then
         ProtoIndex := PlayerShip.Cargo(CargoIndex).ProtoIndex;
         if BaseCargoIndex = 0 then
            BaseCargoIndex2 := FindBaseCargo(ProtoIndex);
         end if;
      else
         ProtoIndex :=
           (if BaseIndex = 0 then TraderCargo(BaseCargoIndex).ProtoIndex
            else SkyBases(BaseIndex).Cargo(BaseCargoIndex).ProtoIndex);
      end if;
      if BaseCargoIndex = 0 then
         if BaseCargoIndex2 > 0 then
            Price :=
              (if BaseIndex > 0 then
                 SkyBases(BaseIndex).Cargo(BaseCargoIndex2).Price
               else TraderCargo(BaseCargoIndex2).Price);
         else
            Price := Get_Price(BaseType, ProtoIndex);
         end if;
      else
         Price :=
           (if BaseIndex > 0 then
              SkyBases(BaseIndex).Cargo(BaseCargoIndex).Price
            else TraderCargo(BaseCargoIndex).Price);
      end if;
      declare
         EventIndex: constant Natural :=
           SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      begin
         if EventIndex > 0 then
            if Events_List(EventIndex).EType = DoublePrice
              and then Events_List(EventIndex).ItemIndex = ProtoIndex then
               Price := Price * 2;
            end if;
         end if;
      end;
      Append
        (ItemInfo,
         "Weight:" & Integer'Image(Items_List(ProtoIndex).Weight) & " kg");
      if Items_List(ProtoIndex).IType = WeaponType then
         Append
           (ItemInfo,
            LF & "Skill: " &
            To_String(Skills_List(Items_List(ProtoIndex).Value(3)).Name) &
            "/" &
            To_String
              (Attributes_List
                 (Skills_List(Items_List(ProtoIndex).Value(3)).Attribute)
                 .Name));
         if Items_List(ProtoIndex).Value(4) = 1 then
            Append(ItemInfo, LF & "Can be used with shield.");
         else
            Append
              (ItemInfo,
               LF & "Can't be used with shield (two-handed weapon).");
         end if;
         Append(ItemInfo, LF & "Damage type: ");
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
      for ItemType of ItemTypes loop
         if Items_List(ProtoIndex).IType = ItemType then
            Append
              (ItemInfo,
               LF & "Damage chance: " &
               GetItemChanceToDamage(Items_List(ProtoIndex).Value(1)));
            Append
              (ItemInfo,
               LF & "Strength:" &
               Integer'Image(Items_List(ProtoIndex).Value(2)));
            exit;
         end if;
      end loop;
      if Tools_List.Contains(Items_List(ProtoIndex).IType) then
         Append
           (ItemInfo,
            LF & "Damage chance: " &
            GetItemChanceToDamage(Items_List(ProtoIndex).Value(1)));
      end if;
      if Length(Items_List(ProtoIndex).IType) > 4
        and then
        (Slice(Items_List(ProtoIndex).IType, 1, 4) = "Ammo" or
         Items_List(ProtoIndex).IType = To_Unbounded_String("Harpoon")) then
         Append
           (ItemInfo,
            LF & "Strength:" & Integer'Image(Items_List(ProtoIndex).Value(1)));
      end if;
      if Items_List(ProtoIndex).Description /= Null_Unbounded_String then
         Append
           (ItemInfo, LF & LF & To_String(Items_List(ProtoIndex).Description));
      end if;
      ShowInfo(To_String(ItemInfo));
      return TCL_OK;
   end Show_Trade_Item_Info_Command;

   -- ****o* TUI/Trade_Item_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Trade_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargoIndex, CargoIndex: Natural := 0;
      Trader: String(1 .. 4);
      ProtoIndex: Unbounded_String;
      TypeBox: Ttk_ComboBox;
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".itemdialog.amount", Interp);
   begin
      if ItemIndex < 0 then
         BaseCargoIndex := abs (ItemIndex);
      else
         CargoIndex := ItemIndex;
      end if;
      if CargoIndex > 0 then
         ProtoIndex := PlayerShip.Cargo(CargoIndex).ProtoIndex;
         if BaseCargoIndex = 0 then
            BaseCargoIndex := FindBaseCargo(ProtoIndex);
         end if;
      else
         ProtoIndex :=
           (if BaseIndex = 0 then TraderCargo(BaseCargoIndex).ProtoIndex
            else SkyBases(BaseIndex).Cargo(BaseCargoIndex).ProtoIndex);
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
      UpdateMessages;
      TypeBox.Interp := Interp;
      TypeBox.Name :=
        New_String(".gameframe.paned.tradeframe.canvas.trade.options.type");
      return Show_Trade_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowTrade" & Get(TypeBox));
   exception
      when An_Exception : Trade_Cant_Buy =>
         ShowMessage
           ("You can't buy " & Exception_Message(An_Exception) & " in this " &
            Trader & ".");
         return TCL_OK;
      when An_Exception : Trade_Not_For_Sale_Now =>
         ShowMessage
           ("You can't buy " & Exception_Message(An_Exception) &
            " in this base at this moment.");
         return TCL_OK;
      when An_Exception : Trade_Buying_Too_Much =>
         ShowMessage
           (Trader & " don't have that much " &
            Exception_Message(An_Exception) & " for sale.");
         return TCL_OK;
      when Trade_No_Free_Cargo =>
         ShowMessage
           ("You don't have that much free space in your ship cargo.");
         return TCL_OK;
      when An_Exception : Trade_No_Money =>
         ShowMessage
           ("You don't have any " & To_String(MoneyName) & " to buy " &
            Exception_Message(An_Exception) & ".");
         return TCL_OK;
      when An_Exception : Trade_Not_Enough_Money =>
         ShowMessage
           ("You don't have enough " & To_String(MoneyName) &
            " to buy so much " & Exception_Message(An_Exception) & ".");
         return TCL_OK;
      when Trade_Invalid_Amount =>
         if CArgv.Arg(Argv, 1) = "buy" then
            ShowMessage("You entered invalid amount to buy.");
         else
            ShowMessage("You entered invalid amount to sell.");
         end if;
         return TCL_OK;
      when An_Exception : Trade_Too_Much_For_Sale =>
         ShowMessage
           ("You dont have that much " & Exception_Message(An_Exception) &
            " in ship cargo.");
         return TCL_OK;
      when An_Exception : Trade_No_Money_In_Base =>
         ShowMessage
           ("You can't sell so much " & Exception_Message(An_Exception) &
            " because " & Trader & " don't have that much " &
            To_String(MoneyName) & " to buy it.");
         return TCL_OK;
      when Trade_No_Trader =>
         ShowMessage
           ("You don't have assigned anyone in crew to talk in bases duty.");
         return TCL_OK;
   end Trade_Item_Command;

   -- ****o* TUI/Search_Trade_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Trade_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget
          (".gameframe.paned.tradeframe.canvas.trade.options.type", Interp);
      SearchText: constant String := CArgv.Arg(Argv, 1);
   begin
      if SearchText'Length = 0 then
         return Show_Trade_Command
             (ClientData, Interp, 2, CArgv.Empty & "ShowTrade" & Get(TypeBox));
      end if;
      return Show_Trade_Command
          (ClientData, Interp, 3,
           CArgv.Empty & "ShowTrade" & Get(TypeBox) & SearchText);
   end Search_Trade_Command;

   -- ****o* TUI/Show_Trade_Menu_Command
   -- FUNCTION
   -- Show trade menu with buy/sell options for the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Trade_Menu_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      TradeMenu: Tk_Menu := Get_Widget(".trademenu", Interp);
      MoneyIndex2: constant Natural := FindItem(PlayerShip.Cargo, MoneyIndex);
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargoIndex2, Price: Natural;
      ProtoIndex, BaseType: Unbounded_String;
   begin
      ItemIndex := Integer'Value(CArgv.Arg(Argv, 1));
      if Winfo_Get(TradeMenu, "exists") = "0" then
         TradeMenu := Create(".trademenu", "-tearoff false");
      end if;
      Delete(TradeMenu, "0", "end");
      BaseType :=
        (if BaseIndex > 0 then SkyBases(BaseIndex).BaseType
         else To_Unbounded_String("0"));
      if ItemIndex > 0 then
         ProtoIndex := PlayerShip.Cargo(ItemIndex).ProtoIndex;
         BaseCargoIndex2 := FindBaseCargo(ProtoIndex);
      else
         BaseCargoIndex2 := abs (ItemIndex);
         ProtoIndex :=
           (if BaseIndex = 0 then TraderCargo(BaseCargoIndex2).ProtoIndex
            else SkyBases(BaseIndex).Cargo(BaseCargoIndex2).ProtoIndex);
      end if;
      if ItemIndex > 0 then
         if BaseCargoIndex2 > 0 then
            Price :=
              (if BaseIndex > 0 then
                 SkyBases(BaseIndex).Cargo(BaseCargoIndex2).Price
               else TraderCargo(BaseCargoIndex2).Price);
         else
            Price := Get_Price(BaseType, ProtoIndex);
         end if;
      else
         Price :=
           (if BaseIndex > 0 then
              SkyBases(BaseIndex).Cargo(BaseCargoIndex2).Price
            else TraderCargo(BaseCargoIndex2).Price);
      end if;
      declare
         EventIndex: constant Natural :=
           SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      begin
         if EventIndex > 0 then
            if Events_List(EventIndex).EType = DoublePrice
              and then Events_List(EventIndex).ItemIndex = ProtoIndex then
               Price := Price * 2;
            end if;
         end if;
      end;
      if ItemIndex > 0 then
         declare
            MaxSellAmount: Integer := PlayerShip.Cargo(ItemIndex).Amount;
            MaxPrice: Natural := MaxSellAmount * Price;
            Weight: Integer;
         begin
            CountPrice(MaxPrice, FindMember(Talk), False);
            if BaseIndex > 0
              and then MaxPrice > SkyBases(BaseIndex).Cargo(1).Amount then
               MaxSellAmount :=
                 Natural
                   (Float'Floor
                      (Float(MaxSellAmount) *
                       (Float(SkyBases(BaseIndex).Cargo(1).Amount) /
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
               CountPrice(MaxPrice, FindMember(Talk), False);
            end if;
            Weight :=
              FreeCargo
                ((Items_List(ProtoIndex).Weight * MaxSellAmount) - MaxPrice);
            while Weight < 0 loop
               MaxSellAmount :=
                 Integer
                   (Float'Floor
                      (Float(MaxSellAmount) *
                       (Float(MaxPrice + Weight) / Float(MaxPrice))));
               exit when MaxSellAmount < 1;
               MaxPrice := MaxSellAmount * Price;
               CountPrice(MaxPrice, FindMember(Talk), False);
               Weight :=
                 FreeCargo
                   ((Items_List(ProtoIndex).Weight * MaxSellAmount) -
                    MaxPrice);
            end loop;
            Menu.Add
              (TradeMenu, "command",
               "-label {Sell selected amount} -command {TradeAmount sell}");
            Menu.Add
              (TradeMenu, "command",
               "-label {Sell" & Natural'Image(MaxSellAmount) &
               " of them} -command {TradeItem sell" &
               Natural'Image(MaxSellAmount) & "}");
         end;
      end if;
      if BaseCargoIndex2 > 0 and MoneyIndex2 > 0 and
        Is_Buyable(BaseType, ProtoIndex) then
         declare
            MaxBuyAmount: Integer :=
              PlayerShip.Cargo(MoneyIndex2).Amount / Price;
            MaxPrice: Natural := MaxBuyAmount * Price;
            Weight: Integer;
         begin
            if MaxBuyAmount > 0 then
               CountPrice(MaxPrice, FindMember(Talk));
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
                   SkyBases(BaseIndex).Cargo(BaseCargoIndex2).Amount then
                  MaxBuyAmount :=
                    SkyBases(BaseIndex).Cargo(BaseCargoIndex2).Amount;
               elsif BaseIndex = 0
                 and then MaxBuyAmount >
                   TraderCargo(BaseCargoIndex2).Amount then
                  MaxBuyAmount := TraderCargo(BaseCargoIndex2).Amount;
               end if;
               MaxPrice := MaxBuyAmount * Price;
               CountPrice(MaxPrice, FindMember(Talk));
               Weight :=
                 FreeCargo
                   (MaxPrice - (Items_List(ProtoIndex).Weight * MaxBuyAmount));
               while Weight < 0 loop
                  MaxBuyAmount :=
                    MaxBuyAmount + (Weight / Items_List(ProtoIndex).Weight) -
                    1;
                  if MaxBuyAmount < 0 then
                     MaxBuyAmount := 0;
                  end if;
                  exit when MaxBuyAmount = 0;
                  MaxPrice := MaxBuyAmount * Price;
                  CountPrice(MaxPrice, FindMember(Talk));
                  Weight :=
                    FreeCargo
                      (MaxPrice -
                       (Items_List(ProtoIndex).Weight * MaxBuyAmount));
               end loop;
               if MaxBuyAmount > 0 then
                  Menu.Add
                    (TradeMenu, "command",
                     "-label {Buy selected amount} -command {TradeAmount buy" &
                     Natural'Image(MaxBuyAmount) & "}");
                  Menu.Add
                    (TradeMenu, "command",
                     "-label {Buy" & Natural'Image(MaxBuyAmount) &
                     " of them} -command {TradeItem buy" &
                     Natural'Image(MaxBuyAmount) & "}");
               end if;
            end if;
         end;
      end if;
      Menu.Add
        (TradeMenu, "command",
         "-label {Show more info about the item} -command ShowTradeItemInfo");
      Tk_Popup
        (TradeMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
         Winfo_Get(Get_Main_Window(Interp), "pointery"));
      return TCL_OK;
   end Show_Trade_Menu_Command;

   -- ****o* TUI/Trade_Amount_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Trade_Amount_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      if CArgv.Arg(Argv, 1) = "sell" then
         ShowManipulateItem
           ("Sell " & GetItemName(PlayerShip.Cargo(ItemIndex)),
            "TradeItem sell", "sell", ItemIndex);
      else
         ShowManipulateItem
           ("Buy " & GetItemName(PlayerShip.Cargo(ItemIndex)), "TradeItem buy",
            "buy", abs (ItemIndex), Natural'Value(CArgv.Arg(Argv, 2)));
      end if;
      return TCL_OK;
   end Trade_Amount_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowTrade", Show_Trade_Command'Access);
      AddCommand("ShowTradeItemInfo", Show_Trade_Item_Info_Command'Access);
      AddCommand("TradeItem", Trade_Item_Command'Access);
      AddCommand("SearchTrade", Search_Trade_Command'Access);
      AddCommand("ShowTradeMenu", Show_Trade_Menu_Command'Access);
      AddCommand("TradeAmount", Trade_Amount_Command'Access);
   end AddCommands;

end Trades.UI;
