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
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
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
      pragma Unreferenced(ClientData);
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      TradeCanvas: Tk_Canvas;
      TradeFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      ItemsView: Ttk_Tree_View;
      ItemDurability, ItemType, ProtoIndex, BaseType: Unbounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      ItemWeight, Price: Positive;
      ComboBox: Ttk_ComboBox;
      FirstIndex: Natural := 0;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargo: BaseCargo_Container.Vector;
      BaseCargoIndex: Natural;
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
        New_String(Widget_Image(TradeCanvas) & ".trade.type.label");
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
                  To_Unbounded_String(GetItemDamage(PlayerShip.Cargo(I).Durability));
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
            if Profit < 0 then
               Set(ItemsList, ItemsIter, 11, "red");
            elsif Profit > 0 then
               Set(ItemsList, ItemsIter, 11, "green");
            end if;
            Set(ItemsList, ItemsIter, 7, Gint(PlayerShip.Cargo(I).Amount));
            if BaseCargoIndex > 0 and Is_Buyable(BaseType, ProtoIndex) then
               if BaseIndex = 0 then
                  Set
                    (ItemsList, ItemsIter, 8,
                     Gint(TraderCargo(BaseCargoIndex).Amount));
               else
                  Set
                    (ItemsList, ItemsIter, 8,
                     Gint(SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount));
               end if;
            end if;
            Insert(ItemsView, "{} end -id"  & Positive'Image(Inventory_Container.To_Index(I)) &
            " -values [list {" &
            GetItemName(PlayerShip.Cargo(I), False, False) & "} {" & To_String(ItemType) & "} {" &
            To_String(ItemDurability) & "} {" & Positve'Image(Price) & "} {");
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
   ItemIndex: Positive;
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
--      GiveFrame: Ttk_Frame;
--      SpinBox: Ttk_SpinBox;
   begin
      TradeView.Interp := Interp;
      TradeView.Name :=
        New_String(".paned.tradeframe.canvas.trade.trade.view");
      ItemIndex := Positive'Value(Selection(TradeView));
--      ShowInventoryItemInfo
--        (".paned.tradeframe.canvas.trade.item.info.text", ItemIndex, 0);
--      GiveFrame.Interp := Interp;
--      GiveFrame.Name :=
--        New_String(".paned.tradeframe.canvas.trade.item.giveframe");
--      if Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).IType =
--        MissionItemsType then
--         Tcl.Tk.Ada.Grid.Grid_Remove(GiveFrame);
--      else
--         Tcl.Tk.Ada.Grid.Grid(GiveFrame);
--      end if;
--      SpinBox.Interp := Interp;
--      SpinBox.Name :=
--        New_String(".paned.tradeframe.canvas.trade.item.dropframe.amount");
--      Set(SpinBox, "1");
--      configure
--        (SpinBox,
--         "-to" & Positive'Image(PlayerShip.Cargo(ItemIndex).Amount) &
--         " -validatecommand {CheckAmount %W" & Positive'Image(ItemIndex) &
--         " %P} -command {ValidateAmount " & Widget_Image(SpinBox) &
--         Positive'Image(ItemIndex) & "}");
--      SpinBox.Name := New_String(Widget_Image(GiveFrame) & ".amount");
--      Set(SpinBox, "1");
--      configure
--        (SpinBox,
--         "-to" & Positive'Image(PlayerShip.Cargo(ItemIndex).Amount) &
--         " -validatecommand {CheckAmount %W" & Positive'Image(ItemIndex) &
--         " %P} -command {ValidateAmount " & Widget_Image(SpinBox) &
--         Positive'Image(ItemIndex) & "}");
      return TCL_OK;
   end Show_Trade_Item_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowTrade", Show_Trade_Command'Access);
      AddCommand("ShowTradeItemInfo", Show_Trade_Item_Info_Command'Access);
   end AddCommands;

end Trades.UI;
