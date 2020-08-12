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
with Events; use Events;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Utils.UI; use Utils.UI;

package body Bases.LootUI is

   -- ****o* LUI/Show_Loot_Command
   -- FUNCTION
   -- Show information about looting
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLoot
   -- SOURCE
   function Show_Loot_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Loot_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      LootCanvas: Tk_Canvas;
      LootFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      ItemsView: Ttk_Tree_View;
      ItemDurability, ItemType, ProtoIndex, FirstIndex,
      ItemName: Unbounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      ComboBox: Ttk_ComboBox;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargo: BaseCargo_Container.Vector;
      BaseCargoIndex, BaseAmount: Natural;
      IndexesList: Positive_Container.Vector;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      LootFrame.Interp := Interp;
      LootFrame.Name := New_String(Widget_Image(Paned) & ".lootframe");
      LootCanvas.Interp := Interp;
      LootCanvas.Name := New_String(Widget_Image(LootFrame) & ".canvas");
      Label.Interp := Interp;
      Label.Name :=
        New_String(Widget_Image(LootCanvas) & ".loot.options.typelabel");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "loot.tcl");
         Bind(LootFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp trade}");
      LootFrame.Name := New_String(Widget_Image(LootCanvas) & ".loot");
      ComboBox.Interp := Interp;
      ComboBox.Name := New_String(Widget_Image(LootFrame) & ".options.type");
      ItemsView.Interp := Interp;
      ItemsView.Name := New_String(Widget_Image(LootFrame) & ".loot.view");
      Delete(ItemsView, "[list " & Children(ItemsView, "{}") & "]");
      BaseCargo := SkyBases(BaseIndex).Cargo;
      for I in PlayerShip.Cargo.Iterate loop
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
         if Index(ItemsTypes, To_String("{" & ItemType & "}")) = 0 then
            Append(ItemsTypes, " {" & ItemType & "}");
         end if;
         ItemName :=
           To_Unbounded_String(GetItemName(PlayerShip.Cargo(I), False, False));
         if PlayerShip.Cargo(I).Durability < 100 then
            ItemDurability :=
              To_Unbounded_String
                (GetItemDamage(PlayerShip.Cargo(I).Durability));
         else
            ItemDurability := Null_Unbounded_String;
         end if;
         BaseAmount := 0;
         if BaseCargoIndex > 0 then
            BaseAmount := SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount;
         end if;
         if FirstIndex = Null_Unbounded_String then
            FirstIndex :=
              To_Unbounded_String
                (Positive'Image(Inventory_Container.To_Index(I)));
         end if;
         Insert
           (ItemsView,
            "{} end -id" & Positive'Image(Inventory_Container.To_Index(I)) &
            " -values [list {" & To_String(ItemName) & "} {" &
            To_String(ItemType) & "} {" & To_String(ItemDurability) & "} {" &
            Natural'Image(PlayerShip.Cargo(I).Amount) & "} {" &
            Natural'Image(BaseAmount) & "}]");
      end loop;
      for I in BaseCargo.First_Index .. BaseCargo.Last_Index loop
         if IndexesList.Find_Index(Item => I) = 0 then
            ProtoIndex := BaseCargo(I).ProtoIndex;
            if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
               ItemType := Items_List(ProtoIndex).IType;
            else
               ItemType := Items_List(ProtoIndex).ShowType;
            end if;
            if Index(ItemsTypes, To_String("{" & ItemType & "}")) = 0 then
               Append(ItemsTypes, " {" & ItemType & "}");
            end if;
            ItemName := Items_List(ProtoIndex).Name;
            if BaseCargo(I).Durability < 100 then
               ItemDurability :=
                 To_Unbounded_String(GetItemDamage(BaseCargo(I).Durability));
            else
               ItemDurability := Null_Unbounded_String;
            end if;
            BaseAmount := SkyBases(BaseIndex).Cargo(I).Amount;
            if FirstIndex = Null_Unbounded_String then
               FirstIndex :=
                 To_Unbounded_String(" b" & Trim(Positive'Image(I), Left));
            end if;
            Insert
              (ItemsView,
               "{} end -id b" & Trim(Positive'Image(I), Left) &
               " -values [list {" & To_String(ItemName) & "} {" &
               To_String(ItemType) & "} {" & To_String(ItemDurability) &
               "} {0} {" & Positive'Image(BaseAmount) & "}]");
         end if;
      end loop;
      Selection_Set(ItemsView, "[list" & To_String(FirstIndex) & "]");
      configure(ComboBox, "-values [list " & To_String(ItemsTypes) & "]");
      if Argc = 1 then
         Current(ComboBox, "0");
      end if;
      LootFrame.Name := New_String(Widget_Image(LootCanvas) & ".loot.item");
      Tcl.Tk.Ada.Grid.Grid(LootFrame);
      Label.Name := New_String(Widget_Image(LootFrame) & ".dropframe.error");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      LootFrame.Name := New_String(Widget_Image(LootCanvas) & ".loot");
      configure
        (LootCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (LootCanvas, "window",
         "[expr " & Winfo_Get(LootFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(LootFrame, "reqheight") & " / 2] -window " &
         Widget_Image(LootFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (LootCanvas, "-scrollregion [list " & BBox(LootCanvas, "all") & "]");
      ShowScreen("lootframe");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Loot_Command;

   -- ****if* LUI/ItemIndex
   -- FUNCTION
   -- Index of the currently selected item
   -- SOURCE
   ItemIndex: Integer;
   -- ****

   -- ****o* LUI/Show_Loot_Item_Info_Command
   -- FUNCTION
   -- Show information about the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLootItemInfo
   -- SOURCE
   function Show_Loot_Item_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Loot_Item_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      LootView: Ttk_Tree_View;
      ItemInfo, ProtoIndex: Unbounded_String;
      CargoIndex, BaseCargoIndex, BaseCargoIndex2: Natural := 0;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      SelectedItem: Unbounded_String;
      ItemTypes: constant array(Positive range <>) of Unbounded_String :=
        (WeaponType, ChestArmor, HeadArmor, ArmsArmor, LegsArmor, ShieldType);
      ItemText: Tk_Text;
      Frame: Ttk_Frame;
      Label: Ttk_Label;
   begin
      LootView.Interp := Interp;
      LootView.Name := New_String(".paned.lootframe.canvas.loot.loot.view");
      SelectedItem := To_Unbounded_String(Selection(LootView));
      if SelectedItem = Null_Unbounded_String then
         return TCL_OK;
      end if;
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
      if BaseCargoIndex > Natural(SkyBases(BaseIndex).Cargo.Length) then
         return TCL_OK;
      end if;
      if CargoIndex > 0 then
         ProtoIndex := PlayerShip.Cargo(CargoIndex).ProtoIndex;
         if BaseCargoIndex = 0 then
            BaseCargoIndex2 := FindBaseCargo(ProtoIndex);
         end if;
      else
         ProtoIndex := SkyBases(BaseIndex).Cargo(BaseCargoIndex).ProtoIndex;
      end if;
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
        New_String(".paned.lootframe.canvas.loot.item.info.text");
      configure(ItemText, "-state normal");
      Delete(ItemText, "1.0", "end");
      Insert(ItemText, "end", "{" & To_String(ItemInfo) & "}");
      configure(ItemText, "-state disabled");
      Frame.Interp := Interp;
      Frame.Name := New_String(".paned.lootframe.canvas.loot.item.dropframe");
      if CargoIndex > 0 then
         declare
            MaxDropAmount: constant Integer :=
              PlayerShip.Cargo(CargoIndex).Amount;
            AmountBox: Ttk_SpinBox;
            AmountLabel: Ttk_Label;
         begin
            AmountBox.Interp := Interp;
            AmountBox.Name := New_String(Widget_Image(Frame) & ".amount");
            Set(AmountBox, "1");
            if MaxDropAmount > 0 then
               configure(AmountBox, "-to" & Natural'Image(MaxDropAmount));
               configure
                 (AmountBox,
                  "-to" & Natural'Image(MaxDropAmount) &
                  " -validatecommand {CheckAmount %W" &
                  Positive'Image(ItemIndex) &
                  " %P} -command {ValidateAmount " & Widget_Image(AmountBox) &
                  Positive'Image(ItemIndex) & "}");
               AmountLabel.Interp := Interp;
               AmountLabel.Name :=
                 New_String(Widget_Image(Frame) & ".amountlbl");
               configure
                 (AmountLabel,
                  "-text {(max" & Natural'Image(MaxDropAmount) & "):}");
               Tcl.Tk.Ada.Grid.Grid(Frame);
               if MaxDropAmount > 1
                 and then MaxDropAmount =
                   PlayerShip.Cargo(CargoIndex).Amount then
                  AmountLabel.Name :=
                    New_String(Widget_Image(Frame) & ".orlbl");
                  Tcl.Tk.Ada.Grid.Grid(AmountLabel);
                  AmountLabel.Name :=
                    New_String(Widget_Image(Frame) & ".dropmax");
                  Tcl.Tk.Ada.Grid.Grid(AmountLabel);
               else
                  AmountLabel.Name :=
                    New_String(Widget_Image(Frame) & ".orlbl");
                  Tcl.Tk.Ada.Grid.Grid_Remove(AmountLabel);
                  AmountLabel.Name :=
                    New_String(Widget_Image(Frame) & ".dropmax");
                  Tcl.Tk.Ada.Grid.Grid_Remove(AmountLabel);
               end if;
            end if;
         end;
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
      end if;
      Frame.Name := New_String(".paned.lootframe.canvas.loot.item.takeframe");
      if BaseCargoIndex = 0 then
         BaseCargoIndex := BaseCargoIndex2;
      end if;
      if BaseCargoIndex > 0 then
         if SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount > 0 then
            Tcl.Tk.Ada.Grid.Grid(Frame);
         end if;
         declare
            MaxTakeAmount: constant Integer :=
              SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount;
            AmountBox: Ttk_SpinBox;
            AmountLabel: Ttk_Label;
         begin
            AmountBox.Interp := Interp;
            AmountBox.Name := New_String(Widget_Image(Frame) & ".amount");
            if MaxTakeAmount > 0 then
               Set(AmountBox, "1");
               configure(AmountBox, "-to" & Natural'Image(MaxTakeAmount));
               configure
                 (AmountBox,
                  "-to" & Natural'Image(MaxTakeAmount) &
                  " -validatecommand {ValidateSpinbox %S %s" &
                  Positive'Image(MaxTakeAmount) & "}");
               AmountLabel.Interp := Interp;
               AmountLabel.Name :=
                 New_String(Widget_Image(Frame) & ".amountlbl");
               configure
                 (AmountLabel,
                  "-text {(max" & Natural'Image(MaxTakeAmount) & "):}");
               if MaxTakeAmount = 1 then
                  AmountLabel.Name :=
                    New_String(Widget_Image(Frame) & ".orlbl");
                  Tcl.Tk.Ada.Grid.Grid_Remove(AmountLabel);
                  AmountLabel.Name :=
                    New_String(Widget_Image(Frame) & ".takemax");
                  Tcl.Tk.Ada.Grid.Grid_Remove(AmountLabel);
               end if;
            else
               Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
            end if;
         end;
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
      end if;
      Label.Interp := Interp;
      Label.Name := New_String(".paned.lootframe.canvas.loot.item.shipspace");
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
      return TCL_OK;
   end Show_Loot_Item_Info_Command;

   -- ****o* LUI/Loot_Item_Command
   -- FUNCTION
   -- Take or drop the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- LootItem actiontype
   -- actiontype can be: drop, dropall, take, takeall
   -- SOURCE
   function Loot_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Loot_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargoIndex, CargoIndex: Natural := 0;
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
         ProtoIndex := SkyBases(BaseIndex).Cargo(BaseCargoIndex).ProtoIndex;
      end if;
      SpinBox.Interp := Interp;
      Label.Interp := Interp;
      if CArgv.Arg(Argv, 1) in "drop" | "dropall" then
         SpinBox.Name :=
           New_String(".paned.lootframe.canvas.loot.item.dropframe.amount");
         Label.Name :=
           New_String(".paned.lootframe.canvas.loot.item.dropframe.amountlbl");
         if CArgv.Arg(Argv, 1) = "drop" then
            Amount := Positive'Value(Get(SpinBox));
         else
            Amount :=
              Natural'Value
                (cget(Label, "-text")(5 .. cget(Label, "-text")'Length - 2));
         end if;
         if BaseCargoIndex > 0 then
            UpdateBaseCargo
              (CargoIndex => BaseCargoIndex, Amount => Amount,
               Durability => PlayerShip.Cargo.Element(CargoIndex).Durability);
         else
            UpdateBaseCargo
              (ProtoIndex, Amount,
               PlayerShip.Cargo.Element(CargoIndex).Durability);
         end if;
         UpdateCargo
           (Ship => PlayerShip, CargoIndex => CargoIndex,
            Amount => (0 - Amount),
            Durability => PlayerShip.Cargo.Element(CargoIndex).Durability);
         AddMessage
           ("You drop" & Positive'Image(Amount) & " " &
            To_String(Items_List(ProtoIndex).Name) & ".",
            OrderMessage);
      else
         SpinBox.Name :=
           New_String(".paned.lootframe.canvas.loot.item.takeframe.amount");
         if CArgv.Arg(Argv, 1) = "take" then
            Amount := Positive'Value(Get(SpinBox));
         else
            Amount := SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount;
         end if;
         if FreeCargo(0 - (Amount * Items_List(ProtoIndex).Weight)) < 0 then
            ShowMessage
              ("You can't take that much " &
               To_String(Items_List(ProtoIndex).Name) & ".");
            return TCL_OK;
         end if;
         if CargoIndex > 0 then
            UpdateCargo
              (Ship => PlayerShip, CargoIndex => CargoIndex, Amount => Amount,
               Durability =>
                 SkyBases(BaseIndex).Cargo(BaseCargoIndex).Durability);
         else
            UpdateCargo
              (PlayerShip, ProtoIndex, Amount,
               SkyBases(BaseIndex).Cargo(BaseCargoIndex).Durability);
         end if;
         UpdateBaseCargo
           (CargoIndex => BaseCargoIndex, Amount => (0 - Amount),
            Durability =>
              SkyBases(BaseIndex).Cargo.Element(BaseCargoIndex).Durability);
         AddMessage
           ("You took" & Positive'Image(Amount) & " " &
            To_String(Items_List(ProtoIndex).Name) & ".",
            OrderMessage);
      end if;
      UpdateHeader;
      UpdateMessages;
      return Show_Loot_Command(ClientData, Interp, Argc, Argv);
   end Loot_Item_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowLoot", Show_Loot_Command'Access);
      AddCommand("ShowLootItemInfo", Show_Loot_Item_Info_Command'Access);
      AddCommand("LootItem", Loot_Item_Command'Access);
   end AddCommands;

end Bases.LootUI;
