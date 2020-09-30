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
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Config; use Config;
with Crew.Inventory; use Crew.Inventory;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Utils.UI; use Utils.UI;

package body Crew.UI.Inventory is

   -- ****iv* Inventory/MemberIndex
   -- FUNCTION
   -- Index of the crew member which inventory will be show
   -- SOURCE
   MemberIndex: Positive;
   -- ****

   -- ****o* Inventory/Show_Inventory_Command
   -- FUNCTION
   -- Show information about the selected crew member's inventory
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowInventory MemberIndex
   -- MemberIndex parameter is the index of the member of players ship crew
   -- which inventory will be shown
   -- SOURCE
   function Show_Inventory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Inventory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Paned: Ttk_PanedWindow;
      InventoryCanvas: Tk_Canvas;
      InventoryFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      ItemsView: Ttk_Tree_View;
      ItemDurability, ItemType, ProtoIndex, Used: Unbounded_String;
      ItemWeight: Positive;
      FirstIndex: Natural := 0;
   begin
      MemberIndex := Positive'Value(CArgv.Arg(Argv, 1));
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      InventoryFrame.Interp := Interp;
      InventoryFrame.Name :=
        New_String(Widget_Image(Paned) & ".inventoryframe");
      InventoryCanvas.Interp := Interp;
      InventoryCanvas.Name :=
        New_String(Widget_Image(InventoryFrame) & ".canvas");
      if Winfo_Get(InventoryCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "inventory.tcl");
         Bind(InventoryFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp crew}");
      ItemsView.Interp := Interp;
      ItemsView.Name :=
        New_String(Widget_Image(InventoryCanvas) & ".inventory.list.view");
      Delete(ItemsView, "[list " & Children(ItemsView, "{}") & "]");
      for I in PlayerShip.Crew(MemberIndex).Inventory.Iterate loop
         if PlayerShip.Crew(MemberIndex).Inventory(I).Durability = 100 then
            ItemDurability := Null_Unbounded_String;
         else
            ItemDurability :=
              To_Unbounded_String
                (GetItemDamage
                   (PlayerShip.Crew(MemberIndex).Inventory(I).Durability));
         end if;
         ProtoIndex := PlayerShip.Crew(MemberIndex).Inventory(I).ProtoIndex;
         if ItemIsUsed(MemberIndex, Inventory_Container.To_Index(I)) then
            Used := To_Unbounded_String("Yes");
         else
            Used := To_Unbounded_String("No");
         end if;
         if Items_List(ProtoIndex).ShowType /= Null_Unbounded_String then
            ItemType := Items_List(ProtoIndex).ShowType;
         else
            ItemType := Items_List(ProtoIndex).IType;
         end if;
         if FirstIndex = 0 then
            FirstIndex := Inventory_Container.To_Index(I);
         end if;
         ItemWeight :=
           PlayerShip.Crew(MemberIndex).Inventory(I).Amount *
           Items_List(ProtoIndex).Weight;
         Insert
           (ItemsView,
            "{} end -id" & Positive'Image(Inventory_Container.To_Index(I)) &
            " -values [list {" &
            GetItemName
              (PlayerShip.Crew(MemberIndex).Inventory(I), False, False) &
            "} {" & To_String(Used) & "} {" & To_String(ItemType) & "} {" &
            To_String(ItemDurability) & "}" &
            Positive'Image(PlayerShip.Crew(MemberIndex).Inventory(I).Amount) &
            " " & Positive'Image(ItemWeight) & "]");
      end loop;
      Selection_Set(ItemsView, "[list" & Natural'Image(FirstIndex) & "]");
      InventoryFrame.Name :=
        New_String(Widget_Image(InventoryCanvas) & ".inventory.item");
      if GameSettings.ShowInventoryInfo then
         Tcl.Tk.Ada.Grid.Grid(InventoryFrame, "-row 0 -column 1");
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(InventoryFrame);
      end if;
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      InventoryFrame.Name :=
        New_String(Widget_Image(InventoryCanvas) & ".inventory");
      configure
        (InventoryCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (InventoryCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(InventoryFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (InventoryCanvas,
         "-scrollregion [list " & BBox(InventoryCanvas, "all") & "]");
      ShowScreen("inventoryframe");
      return TCL_OK;
   end Show_Inventory_Command;

   -- ****if* Inventory/ItemIndex
   -- FUNCTION
   -- Index of the currently selected item
   -- SOURCE
   ItemIndex: Positive;
   -- ****

   -- ****o* Inventory/Show_Inventory_Item_Info_Command
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
   -- ShowInventoryItemInfo
   -- SOURCE
   function Show_Inventory_Item_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Inventory_Item_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      InventoryView: Ttk_Tree_View;
      AmountBox: Ttk_SpinBox;
   begin
      if not GameSettings.ShowInventoryInfo then
         return TCL_OK;
      end if;
      InventoryView.Interp := Interp;
      InventoryView.Name :=
        New_String(".paned.inventoryframe.canvas.inventory.list.view");
      ItemIndex := Positive'Value(Selection(InventoryView));
      ShowInventoryItemInfo
        (".paned.inventoryframe.canvas.inventory.item.info.text", ItemIndex,
         MemberIndex);
      if ItemIsUsed(MemberIndex, ItemIndex) then
         Tcl_SetVar(Interp, "useitem", "1");
      else
         Tcl_SetVar(Interp, "useitem", "0");
      end if;
      AmountBox.Interp := Interp;
      AmountBox.Name :=
        New_String(".paned.inventoryframe.canvas.inventory.item.amount");
      configure
        (AmountBox,
         "-from 1 -to" &
         Positive'Image
           (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Amount) &
         " -validatecommand {ValidateSpinbox %S %s" &
         Positive'Image
           (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Amount) &
         "}");
      Set(AmountBox, "1");
      return TCL_OK;
   end Show_Inventory_Item_Info_Command;

   -- ****o* Inventory/Set_Use_Item_Command
   -- FUNCTION
   -- Set if item is used by a crew member or not
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetUseItem
   -- SOURCE
   function Set_Use_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Use_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc, Argv);
      ItemType: constant Unbounded_String :=
        Items_List
          (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
          .IType;
   begin
      if Tcl_GetVar(Interp, "useitem") = "1" then
         if ItemType = WeaponType then
            if Items_List
                (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
                .Value
                (4) =
              2 and
              PlayerShip.Crew(MemberIndex).Equipment(2) /= 0 then
               ShowMessage
                 (To_String(PlayerShip.Crew(MemberIndex).Name) &
                  " can't use this weapon because have shield equiped. Take off shield first.");
               return TCL_OK;
            end if;
            PlayerShip.Crew(MemberIndex).Equipment(1) := ItemIndex;
         elsif ItemType = ShieldType then
            if PlayerShip.Crew(MemberIndex).Equipment(1) > 0 then
               if Items_List
                   (PlayerShip.Crew(MemberIndex).Inventory
                      (PlayerShip.Crew(MemberIndex).Equipment(1))
                      .ProtoIndex)
                   .Value
                   (4) =
                 2 then
                  ShowMessage
                    (To_String(PlayerShip.Crew(MemberIndex).Name) &
                     " can't use shield because have equiped two-hand weapon. Take off weapon first.");
                  return TCL_OK;
               end if;
            end if;
            PlayerShip.Crew(MemberIndex).Equipment(2) := ItemIndex;
         elsif ItemType = HeadArmor then
            PlayerShip.Crew(MemberIndex).Equipment(3) := ItemIndex;
         elsif ItemType = ChestArmor then
            PlayerShip.Crew(MemberIndex).Equipment(4) := ItemIndex;
         elsif ItemType = ArmsArmor then
            PlayerShip.Crew(MemberIndex).Equipment(5) := ItemIndex;
         elsif ItemType = LegsArmor then
            PlayerShip.Crew(MemberIndex).Equipment(6) := ItemIndex;
         elsif Tools_List.Find_Index(Item => ItemType) /=
           UnboundedString_Container.No_Index then
            PlayerShip.Crew(MemberIndex).Equipment(7) := ItemIndex;
         end if;
      else
         TakeOffItem(MemberIndex, ItemIndex);
      end if;
      return Show_Inventory_Command
          (ClientData, Interp, 2,
           CArgv.Empty & "ShowInventory" &
           Trim(Positive'Image(MemberIndex), Left));
   end Set_Use_Item_Command;

   -- ****o* Inventory/Move_Item_Command
   -- FUNCTION
   -- Move the selected item to the ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- MoveItem
   -- SOURCE
   function Move_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Item_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc, Argv);
      Amount: Positive;
      AmountBox: Ttk_SpinBox;
   begin
      AmountBox.Interp := Interp;
      AmountBox.Name :=
        New_String(".paned.inventoryframe.canvas.inventory.item.amount");
      Amount := Positive'Value(Get(AmountBox));
      if FreeCargo
          (0 -
           (Items_List
              (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex)
              .Weight *
            Amount)) <
        0 then
         ShowMessage
           ("No free space in ship cargo for that amount of " &
            GetItemName(PlayerShip.Crew(MemberIndex).Inventory(ItemIndex)));
         return TCL_OK;
      end if;
      UpdateCargo
        (Ship => PlayerShip,
         ProtoIndex =>
           PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex,
         Amount => Amount,
         Durability =>
           PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Durability,
         Price => PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Price);
      UpdateInventory
        (MemberIndex => MemberIndex, Amount => (0 - Amount),
         InventoryIndex => ItemIndex);
      if
        (PlayerShip.Crew(MemberIndex).Order = Clean and
         FindItem
             (Inventory => PlayerShip.Crew(MemberIndex).Inventory,
              ItemType => CleaningTools) =
           0) or
        ((PlayerShip.Crew(MemberIndex).Order = Upgrading or
          PlayerShip.Crew(MemberIndex).Order = Repair) and
         FindItem
             (Inventory => PlayerShip.Crew(MemberIndex).Inventory,
              ItemType => RepairTools) =
           0) then
         GiveOrders(PlayerShip, MemberIndex, Rest);
      end if;
      return Show_Inventory_Command
          (ClientData, Interp, 2,
           CArgv.Empty & "ShowInventory" &
           Trim(Positive'Image(MemberIndex), Left));
   end Move_Item_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowInventory", Show_Inventory_Command'Access);
      AddCommand
        ("ShowInventoryItemInfo", Show_Inventory_Item_Info_Command'Access);
   end AddCommands;

end Crew.UI.Inventory;
