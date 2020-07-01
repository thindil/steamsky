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
with Config; use Config;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with Utils.UI; use Utils.UI;

package body Ships.Cargo.UI is

   -- ****f* CUI2/Show_Cargo_Info_Command
   -- FUNCTION
   -- Show information about the player's ship cargo
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Cargo_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      CargoCanvas: Tk_Canvas;
      CargoFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      ItemsView: Ttk_Tree_View;
      ItemDurability, ItemType, ProtoIndex: Unbounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      ItemWeight: Positive;
      ComboBox: Ttk_ComboBox;
      FirstIndex: Natural := 0;
      MembersNames: Unbounded_String;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      CargoFrame.Interp := Interp;
      CargoFrame.Name := New_String(Widget_Image(Paned) & ".cargoframe");
      CargoCanvas.Interp := Interp;
      CargoCanvas.Name := New_String(Widget_Image(CargoFrame) & ".canvas");
      Label.Interp := Interp;
      Label.Name :=
        New_String(Widget_Image(CargoCanvas) & ".cargo.type.label");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "cargo.tcl");
         Bind(CargoFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      CargoFrame.Name := New_String(Widget_Image(CargoCanvas) & ".cargo");
      ComboBox.Interp := Interp;
      ComboBox.Name := New_String(Widget_Image(CargoFrame) & ".type.combo");
      ItemsView.Interp := Interp;
      ItemsView.Name := New_String(Widget_Image(CargoFrame) & ".cargo.view");
      Delete(ItemsView, "[list " & Children(ItemsView, "{}") & "]");
      for I in PlayerShip.Cargo.Iterate loop
         if PlayerShip.Cargo(I).Durability = 100 then
            ItemDurability := Null_Unbounded_String;
         else
            ItemDurability :=
              To_Unbounded_String
                (GetItemDamage(PlayerShip.Cargo(I).Durability));
         end if;
         ProtoIndex := PlayerShip.Cargo(I).ProtoIndex;
         if Items_List(ProtoIndex).ShowType /= Null_Unbounded_String then
            ItemType := Items_List(ProtoIndex).ShowType;
         else
            ItemType := Items_List(ProtoIndex).IType;
         end if;
         if Index(ItemsTypes, "{" & To_String(ItemType) & "}") = 0 then
            Append(ItemsTypes, " {" & To_String(ItemType) & "}");
         end if;
         if Argc = 2 and then CArgv.Arg(Argv, 1) /= "All"
           and then To_String(ItemType) /= CArgv.Arg(Argv, 1) then
            goto End_Of_Loop;
         end if;
         if FirstIndex = 0 then
            FirstIndex := Inventory_Container.To_Index(I);
         end if;
         ItemWeight :=
           PlayerShip.Cargo(I).Amount * Items_List(ProtoIndex).Weight;
         Insert
           (ItemsView,
            "{} end -id" & Positive'Image(Inventory_Container.To_Index(I)) &
            " -values [list {" &
            GetItemName(PlayerShip.Cargo(I), False, False) & "} {" &
            To_String(ItemDurability) & "} {" & To_String(ItemType) & "}" &
            Positive'Image(PlayerShip.Cargo(I).Amount) & " " &
            Positive'Image(ItemWeight) & "]");
         <<End_Of_Loop>>
      end loop;
      Selection_Set(ItemsView, "[list" & Natural'Image(FirstIndex) & "]");
      configure(ComboBox, "-values [list " & To_String(ItemsTypes) & "]");
      if Argc = 1 then
         Current(ComboBox, "0");
      end if;
      CargoFrame.Name := New_String(Widget_Image(CargoCanvas) & ".cargo.item");
      if GameSettings.ShowCargoInfo then
         Tcl.Tk.Ada.Grid.Grid(CargoFrame);
         ComboBox.Name :=
           New_String(Widget_Image(CargoFrame) & ".giveframe.member");
         for Member of PlayerShip.Crew loop
            Append(MembersNames, " " & Member.Name);
         end loop;
         configure(ComboBox, "-values [list" & To_String(MembersNames) & "]");
         Current(ComboBox, "0");
         Label.Name :=
           New_String(Widget_Image(CargoFrame) & ".dropframe.error");
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         Label.Name :=
           New_String(Widget_Image(CargoFrame) & ".giveframe.error");
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(CargoFrame);
      end if;
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      CargoFrame.Name := New_String(Widget_Image(CargoCanvas) & ".cargo");
      configure
        (CargoCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (CargoCanvas, "window",
         "[expr " & Winfo_Get(CargoFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(CargoFrame, "reqheight") & " / 2] -window " &
         Widget_Image(CargoFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (CargoCanvas, "-scrollregion [list " & BBox(CargoCanvas, "all") & "]");
      ShowScreen("cargoframe");
      return TCL_OK;
   end Show_Cargo_Info_Command;

   -- ****if* CUI2/ItemIndex
   -- FUNCTION
   -- Index of the currently selected item
   -- SOURCE
   ItemIndex: Positive;
   -- ****

   -- ****f* CUI2/Show_Cargo_Item_Info_Command
   -- FUNCTION
   -- Show information about the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Cargo_Item_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Item_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      CargoView: Ttk_Tree_View;
      GiveFrame: Ttk_Frame;
      SpinBox: Ttk_SpinBox;
   begin
      if not GameSettings.ShowCargoInfo then
         return TCL_OK;
      end if;
      CargoView.Interp := Interp;
      CargoView.Name :=
        New_String(".paned.cargoframe.canvas.cargo.cargo.view");
      ItemIndex := Positive'Value(Selection(CargoView));
      ShowInventoryItemInfo
        (".paned.cargoframe.canvas.cargo.item.info.text", ItemIndex, 0);
      GiveFrame.Interp := Interp;
      GiveFrame.Name :=
        New_String(".paned.cargoframe.canvas.cargo.item.giveframe");
      if Items_List(PlayerShip.Cargo(ItemIndex).ProtoIndex).IType =
        MissionItemsType then
         Tcl.Tk.Ada.Grid.Grid_Remove(GiveFrame);
      else
         Tcl.Tk.Ada.Grid.Grid(GiveFrame);
      end if;
      SpinBox.Interp := Interp;
      SpinBox.Name :=
        New_String(".paned.cargoframe.canvas.cargo.item.dropframe.amount");
      Set(SpinBox, "1");
      configure
        (SpinBox,
         "-to" & Positive'Image(PlayerShip.Cargo(ItemIndex).Amount) &
         " -validatecommand {CheckAmount %W" & Positive'Image(ItemIndex) &
         " %P} -command {ValidateAmount " & Widget_Image(SpinBox) &
         Positive'Image(ItemIndex) & "}");
      SpinBox.Name := New_String(Widget_Image(GiveFrame) & ".amount");
      Set(SpinBox, "1");
      configure
        (SpinBox,
         "-to" & Positive'Image(PlayerShip.Cargo(ItemIndex).Amount) &
         " -validatecommand {CheckAmount %W" & Positive'Image(ItemIndex) &
         " %P} -command {ValidateAmount " & Widget_Image(SpinBox) &
         Positive'Image(ItemIndex) & "}");
      return TCL_OK;
   end Show_Cargo_Item_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowCargoInfo", Show_Cargo_Info_Command'Access);
      AddCommand("ShowCargoItemInfo", Show_Cargo_Item_Info_Command'Access);
   end AddCommands;

end Ships.Cargo.UI;
