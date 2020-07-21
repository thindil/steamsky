-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases.Cargo; use Bases.Cargo;
with BasesTypes; use BasesTypes;
with Crew; use Crew;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Utils.UI; use Utils.UI;

package body Trades.UI is

   -- ****f* TUI/Show_Trade_Command
   -- FUNCTION
   -- Show information about trading
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      pragma Unreferenced(ClientData, Argv);
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      TradeCanvas: Tk_Canvas;
      TradeFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      ItemsView: Ttk_Tree_View;
      ItemDurability, ItemType, ProtoIndex, BaseType: Unbounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      Price: Positive;
      ComboBox: Ttk_ComboBox;
      FirstIndex: Natural := 0;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargo: BaseCargo_Container.Vector;
      BaseCargoIndex, BaseAmount: Natural;
      IndexesList: Positive_Container.Vector;
      EventIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
      Profit: Integer;
      procedure AddType is
      begin
         if Index
             (ItemsTypes,
              To_String("{" & Items_List(ProtoIndex).IType & "}")) =
           0 and
           Index
               (ItemsTypes,
                To_String("{" & Items_List(ProtoIndex).ShowType & "}")) =
             0 then
            if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
               Append(ItemsTypes, " {" & Items_List(ProtoIndex).IType & "}");
            else
               Append
                 (ItemsTypes, " {" & Items_List(ProtoIndex).ShowType & "}");
            end if;
         end if;
      end AddType;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      TradeFrame.Interp := Interp;
      TradeFrame.Name := New_String(Widget_Image(Paned) & ".tradeframe");
      TradeCanvas.Interp := Interp;
      TradeCanvas.Name := New_String(Widget_Image(TradeFrame) & ".canvas");
      Label.Interp := Interp;
      Label.Name :=
        New_String(Widget_Image(TradeCanvas) & ".trade.options.typelabel");
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
      TradeFrame.Name := New_String(Widget_Image(TradeCanvas) & ".trade");
      ComboBox.Interp := Interp;
      ComboBox.Name := New_String(Widget_Image(TradeFrame) & ".options.type");
      ItemsView.Interp := Interp;
      ItemsView.Name := New_String(Widget_Image(TradeFrame) & ".trade.view");
      Delete(ItemsView, "[list " & Children(ItemsView, "{}") & "]");
      if BaseIndex > 0 then
         BaseType := SkyBases(BaseIndex).BaseType;
         BaseCargo := SkyBases(BaseIndex).Cargo;
      else
         BaseType := To_Unbounded_String("0");
         BaseCargo := TraderCargo;
      end if;
      for I in PlayerShip.Cargo.Iterate loop
         if Get_Price(BaseType, PlayerShip.Cargo(I).ProtoIndex) > 0 then
            ProtoIndex := PlayerShip.Cargo(I).ProtoIndex;
            BaseCargoIndex :=
              FindBaseCargo(ProtoIndex, PlayerShip.Cargo(I).Durability);
            if BaseCargoIndex > 0 then
               IndexesList.Append(New_Item => BaseCargoIndex);
            end if;
            if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
               ItemType := Items_List(ProtoIndex).IType;
            else
               ItemType := Items_List(ProtoIndex).ShowType;
            end if;
            AddType;
            if PlayerShip.Cargo(I).Durability < 100 then
               ItemDurability :=
                 To_Unbounded_String
                   (GetItemDamage(PlayerShip.Cargo(I).Durability));
            else
               ItemDurability := Null_Unbounded_String;
            end if;
            if BaseCargoIndex = 0 then
               Price := Get_Price(BaseType, ProtoIndex);
            else
               if BaseIndex > 0 then
                  Price := SkyBases(BaseIndex).Cargo(BaseCargoIndex).Price;
               else
                  Price := TraderCargo(BaseCargoIndex).Price;
               end if;
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
               if BaseIndex = 0 then
                  BaseAmount := TraderCargo(BaseCargoIndex).Amount;
               else
                  BaseAmount :=
                    SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount;
               end if;
            end if;
            if FirstIndex = 0 then
               FirstIndex := Inventory_Container.To_Index(I);
            end if;
            Insert
              (ItemsView,
               "{} end -id" & Positive'Image(Inventory_Container.To_Index(I)) &
               " -values [list {" &
               GetItemName(PlayerShip.Cargo(I), False, False) & "} {" &
               To_String(ItemType) & "} {" & To_String(ItemDurability) &
               "} {" & Positive'Image(Price) & "} {" & Integer'Image(Profit) &
               "} {" & Natural'Image(PlayerShip.Cargo(I).Amount) & "} {" &
               Natural'Image(BaseAmount) & "}]");
         end if;
      end loop;
      for I in BaseCargo.First_Index .. BaseCargo.Last_Index loop
         if IndexesList.Find_Index(Item => I) = 0 and
           Is_Buyable
             (BaseType => BaseType, ItemIndex => BaseCargo(I).ProtoIndex,
              BaseIndex => BaseIndex) then
            ProtoIndex := BaseCargo(I).ProtoIndex;
            if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
               ItemType := Items_List(ProtoIndex).IType;
            else
               ItemType := Items_List(ProtoIndex).ShowType;
            end if;
            AddType;
            if BaseCargo(I).Durability < 100 then
               ItemDurability :=
                 To_Unbounded_String(GetItemDamage(BaseCargo(I).Durability));
            else
               ItemDurability := Null_Unbounded_String;
            end if;
            if BaseIndex > 0 then
               Price := SkyBases(BaseIndex).Cargo(I).Price;
            else
               Price := TraderCargo(I).Price;
            end if;
            if EventIndex > 0 then
               if Events_List(EventIndex).EType = DoublePrice
                 and then Events_List(EventIndex).ItemIndex = ProtoIndex then
                  Price := Price * 2;
               end if;
            end if;
            if BaseIndex = 0 then
               BaseAmount := TraderCargo(I).Amount;
            else
               BaseAmount := SkyBases(BaseIndex).Cargo(I).Amount;
            end if;
            Insert
              (ItemsView,
               "{} end -id b" & Trim(Positive'Image(I), Left) &
               " -values [list {" & To_String(Items_List(ProtoIndex).Name) &
               "} {" & To_String(ItemType) & "} {" &
               To_String(ItemDurability) & "} {" & Positive'Image(Price) &
               "} {" & Integer'Image(-(Price)) & "} {0} {" &
               Positive'Image(BaseAmount) & "}]");
         end if;
      end loop;
      Selection_Set(ItemsView, "[list" & Natural'Image(FirstIndex) & "]");
      configure(ComboBox, "-values [list " & To_String(ItemsTypes) & "]");
      if Argc = 1 then
         Current(ComboBox, "0");
      end if;
      TradeFrame.Name := New_String(Widget_Image(TradeCanvas) & ".trade.item");
      Tcl.Tk.Ada.Grid.Grid(TradeFrame);
      Label.Name := New_String(Widget_Image(TradeFrame) & ".sellframe.error");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      TradeFrame.Name := New_String(Widget_Image(TradeCanvas) & ".trade");
      configure
        (TradeCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (TradeCanvas, "window",
         "[expr " & Winfo_Get(TradeFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(TradeFrame, "reqheight") & " / 2] -window " &
         Widget_Image(TradeFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (TradeCanvas, "-scrollregion [list " & BBox(TradeCanvas, "all") & "]");
      ShowScreen("tradeframe");
      return TCL_OK;
   end Show_Trade_Command;

   -- ****if* TUI/ItemIndex
   -- FUNCTION
   -- Index of the currently selected item
   -- SOURCE
   ItemIndex: Integer;
   -- ****

   -- ****f* TUI/Show_Trade_Item_Info_Command
   -- FUNCTION
   -- Show information about the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      pragma Unreferenced(ClientData, Argc, Argv);
      TradeView: Ttk_Tree_View;
      ItemInfo, ProtoIndex: Unbounded_String;
      CargoIndex, BaseCargoIndex, BaseCargoIndex2: Natural := 0;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType, SelectedItem: Unbounded_String;
      Price, MoneyIndex2: Natural;
      ItemTypes: constant array(Positive range <>) of Unbounded_String :=
        (WeaponType, ChestArmor, HeadArmor, ArmsArmor, LegsArmor, ShieldType);
      ItemText: Tk_Text;
      Frame: Ttk_Frame;
      Label: Ttk_Label;
   begin
      TradeView.Interp := Interp;
      TradeView.Name :=
        New_String(".paned.tradeframe.canvas.trade.trade.view");
      SelectedItem := To_Unbounded_String(Selection(TradeView));
      if Element(SelectedItem, 1) = 'b' then
         ItemIndex :=
           -(Positive'Value(Slice(SelectedItem, 2, Length(SelectedItem))));
         BaseCargoIndex := abs (ItemIndex);
      else
         ItemIndex := Positive'Value(To_String(SelectedItem));
         CargoIndex := ItemIndex;
      end if;
      if CargoIndex > Natural(PlayerShip.Cargo.Length) then
         return TCL_OK;
      end if;
      if BaseIndex > 0 then
         BaseType := SkyBases(BaseIndex).BaseType;
      else
         BaseType := To_Unbounded_String("0");
      end if;
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
         if BaseIndex = 0 then
            ProtoIndex := TraderCargo(BaseCargoIndex).ProtoIndex;
         else
            ProtoIndex := SkyBases(BaseIndex).Cargo(BaseCargoIndex).ProtoIndex;
         end if;
      end if;
      if BaseCargoIndex = 0 then
         if BaseCargoIndex2 > 0 then
            if BaseIndex > 0 then
               Price := SkyBases(BaseIndex).Cargo(BaseCargoIndex2).Price;
            else
               Price := TraderCargo(BaseCargoIndex2).Price;
            end if;
         else
            Price := Get_Price(BaseType, ProtoIndex);
         end if;
      else
         if BaseIndex > 0 then
            Price := SkyBases(BaseIndex).Cargo(BaseCargoIndex).Price;
         else
            Price := TraderCargo(BaseCargoIndex).Price;
         end if;
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
      ItemText.Interp := Interp;
      ItemText.Name :=
        New_String(".paned.tradeframe.canvas.trade.item.info.text");
      configure(ItemText, "-state normal");
      Delete(ItemText, "1.0", "end");
      Insert(ItemText, "end", "{" & To_String(ItemInfo) & "}");
      configure(ItemText, "-state disabled");
      if Price = 0 then
         CargoIndex := 0;
         BaseCargoIndex := 0;
      end if;
      Frame.Interp := Interp;
      Frame.Name :=
        New_String(".paned.tradeframe.canvas.trade.item.sellframe");
      if CargoIndex > 0 then
         declare
            MaxSellAmount: Integer := PlayerShip.Cargo(CargoIndex).Amount;
            MaxPrice: Natural := MaxSellAmount * Price;
            Weight: Integer;
            AmountBox: Ttk_SpinBox;
            AmountLabel: Ttk_Label;
         begin
            AmountBox.Interp := Interp;
            AmountBox.Name := New_String(Widget_Image(Frame) & ".amount");
            Set(AmountBox, "1");
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
            if MaxSellAmount > 0 then
               configure(AmountBox, "-to" & Natural'Image(MaxSellAmount));
               configure
                 (AmountBox,
                  "-to" & Natural'Image(MaxSellAmount) &
                  " -validatecommand {CheckAmount %W" &
                  Positive'Image(ItemIndex) &
                  " %P} -command {ValidateAmount " & Widget_Image(AmountBox) &
                  Positive'Image(ItemIndex) & "}");
               AmountLabel.Interp := Interp;
               AmountLabel.Name :=
                 New_String(Widget_Image(Frame) & ".amountlbl");
               configure
                 (AmountLabel,
                  "-text {(max" & Natural'Image(MaxSellAmount) & "):}");
               Tcl.Tk.Ada.Grid.Grid(Frame);
               if MaxSellAmount > 1
                 and then MaxSellAmount =
                   PlayerShip.Cargo(CargoIndex).Amount then
                  AmountLabel.Name :=
                    New_String(Widget_Image(Frame) & ".orlbl");
                  Tcl.Tk.Ada.Grid.Grid(AmountLabel);
                  AmountLabel.Name :=
                    New_String(Widget_Image(Frame) & ".sellmax");
                  Tcl.Tk.Ada.Grid.Grid(AmountLabel);
               else
                  AmountLabel.Name :=
                    New_String(Widget_Image(Frame) & ".orlbl");
                  Tcl.Tk.Ada.Grid.Grid_Remove(AmountLabel);
                  AmountLabel.Name :=
                    New_String(Widget_Image(Frame) & ".sellmax");
                  Tcl.Tk.Ada.Grid.Grid_Remove(AmountLabel);
               end if;
            end if;
         end;
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
      end if;
      MoneyIndex2 := FindItem(PlayerShip.Cargo, MoneyIndex);
      Frame.Name := New_String(".paned.tradeframe.canvas.trade.item.buyframe");
      if BaseCargoIndex = 0 then
         BaseCargoIndex := BaseCargoIndex2;
      end if;
      if BaseCargoIndex > 0 and MoneyIndex2 > 0 and
        Is_Buyable(BaseType, ProtoIndex) then
         if BaseIndex > 0
           and then SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount > 0 then
            Tcl.Tk.Ada.Grid.Grid(Frame);
         elsif BaseIndex = 0
           and then TraderCargo(BaseCargoIndex).Amount > 0 then
            Tcl.Tk.Ada.Grid.Grid(Frame);
         end if;
         declare
            MaxBuyAmount: Integer :=
              PlayerShip.Cargo(MoneyIndex2).Amount / Price;
            MaxPrice: Natural := MaxBuyAmount * Price;
            Weight: Integer;
            AmountBox: Ttk_SpinBox;
            AmountLabel: Ttk_Label;
         begin
            AmountBox.Interp := Interp;
            AmountBox.Name := New_String(Widget_Image(Frame) & ".amount");
            if MaxBuyAmount > 0 then
               Set(AmountBox, "1");
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
                   SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount then
                  MaxBuyAmount :=
                    SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount;
               elsif BaseIndex = 0
                 and then MaxBuyAmount >
                   TraderCargo(BaseCargoIndex).Amount then
                  MaxBuyAmount := TraderCargo(BaseCargoIndex).Amount;
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
                  configure(AmountBox, "-to" & Natural'Image(MaxBuyAmount));
                  configure
                    (AmountBox,
                     "-to" & Natural'Image(MaxBuyAmount) &
                     " -validatecommand {ValidateSpinbox %S %s" &
                     Positive'Image(MaxBuyAmount) & "}");
                  AmountLabel.Interp := Interp;
                  AmountLabel.Name :=
                    New_String(Widget_Image(Frame) & ".amountlbl");
                  configure
                    (AmountLabel,
                     "-text {(max" & Natural'Image(MaxBuyAmount) & "):}");
                  if MaxBuyAmount = 1 then
                     AmountLabel.Name :=
                       New_String(Widget_Image(Frame) & ".orlbl");
                     Tcl.Tk.Ada.Grid.Grid_Remove(AmountLabel);
                     AmountLabel.Name :=
                       New_String(Widget_Image(Frame) & ".buymax");
                     Tcl.Tk.Ada.Grid.Grid_Remove(AmountLabel);
                  end if;
               else
                  Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
               end if;
            else
               Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
            end if;
         end;
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
      end if;
      Label.Interp := Interp;
      Label.Name :=
        New_String(".paned.tradeframe.canvas.trade.item.shipmoney");
      if MoneyIndex2 > 0 then
         configure
           (Label,
            "-text {You have" &
            Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) & " " &
            To_String(MoneyName) & ".}");
      else
         configure
           (Label,
            "-text {You don't have any " & To_String(MoneyName) &
            " to buy anything.}");
      end if;
      Label.Name :=
        New_String(".paned.tradeframe.canvas.trade.item.shipspace");
      declare
         FreeSpace: Integer := FreeCargo(0);
      begin
         if FreeSpace < 0 then
            FreeSpace := 0;
         end if;
         configure
           (Label,
            "-text {Free cargo space:" & Integer'Image(FreeSpace) & " kg}");
      end;
      Label.Name :=
        New_String(".paned.tradeframe.canvas.trade.item.basemoney");
      if BaseIndex > 0 then
         if SkyBases(BaseIndex).Cargo(1).Amount = 0 then
            configure
              (Label,
               "-text {Base don't have any " & To_String(MoneyName) &
               "to buy anything.}");
         else
            configure
              (Label,
               "-text {Base have" &
               Positive'Image(SkyBases(BaseIndex).Cargo(1).Amount) & " " &
               To_String(MoneyName) & ".}");
         end if;
      else
         if TraderCargo(1).Amount = 0 then
            configure
              (Label,
               "-text {Ship don't have any " & To_String(MoneyName) &
               "to buy anything.}");
         else
            configure
              (Label,
               "-text {Ship have" & Positive'Image(TraderCargo(1).Amount) &
               " " & To_String(MoneyName) & ".}");
         end if;
      end if;
      return TCL_OK;
   end Show_Trade_Item_Info_Command;

   -- ****f* TUI/Trade_Item_Command
   -- FUNCTION
   -- Buy or sell the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
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
      Amount: Natural;
      ProtoIndex: Unbounded_String;
      SpinBox: Ttk_SpinBox;
      Label: Ttk_Label;
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
         if BaseIndex = 0 then
            ProtoIndex := TraderCargo(BaseCargoIndex).ProtoIndex;
         else
            ProtoIndex := SkyBases(BaseIndex).Cargo(BaseCargoIndex).ProtoIndex;
         end if;
      end if;
      if BaseIndex > 0 then
         Trader := "base";
      else
         Trader := "ship";
      end if;
      SpinBox.Interp := Interp;
      Label.Interp := Interp;
      if CArgv.Arg(Argv, 1) in "buy" | "buymax" then
         SpinBox.Name :=
           New_String(".paned.tradeframe.canvas.trade.item.buyframe.amount");
         Label.Name :=
           New_String
             (".paned.tradeframe.canvas.trade.item.buyframe.amountlbl");
         if CArgv.Arg(Argv, 1) = "buy" then
            Amount := Positive'Value(Get(SpinBox));
         else
            Amount :=
              Natural'Value
                (cget(Label, "-text")(5 .. cget(Label, "-text")'Length - 2));
         end if;
         BuyItems(BaseCargoIndex, Natural'Image(Amount));
      else
         SpinBox.Name :=
           New_String(".paned.tradeframe.canvas.trade.item.sellframe.amount");
         Amount := Positive'Value(Get(SpinBox));
         if CArgv.Arg(Argv, 1) = "sell" then
            SellItems(CargoIndex, Natural'Image(Amount));
         else
            SellItems
              (CargoIndex,
               Positive'Image(PlayerShip.Cargo.Element(CargoIndex).Amount));
         end if;
      end if;
      UpdateHeader;
      return Show_Trade_Command(ClientData, Interp, Argc, Argv);
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

   procedure AddCommands is
   begin
      AddCommand("ShowTrade", Show_Trade_Command'Access);
      AddCommand("ShowTradeItemInfo", Show_Trade_Item_Info_Command'Access);
      AddCommand("TradeItem", Trade_Item_Command'Access);
   end AddCommands;

end Trades.UI;
