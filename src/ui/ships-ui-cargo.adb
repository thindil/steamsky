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
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Utils.UI; use Utils.UI;

package body Ships.UI.Cargo is

   -- ****o* SUCargo/Show_Cargo_Command
   -- FUNCTION
   -- Show the cargo of the player ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateCargo
   -- SOURCE
   function Show_Cargo_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Cargo_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ShipCanvas: constant Tk_Canvas :=
        Get_Widget(".paned.shipinfoframe.cargo.canvas", Interp);
      CargoInfoFrame: constant Ttk_Frame := Get_Widget(ShipCanvas & ".frame", Interp);
      Item: Ttk_Frame;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Row: Positive := 3;
      ItemType, ProtoIndex, ProgressBarStyle: Unbounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      CargoButton: Ttk_MenuButton;
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget(CargoInfoFrame & ".selecttype.combo", Interp);
      DurabilityBar: Ttk_ProgressBar;
      ItemLabel: Ttk_Label;
      ItemsType: constant String := Get(TypeBox);
   begin
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(CargoInfoFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 3 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (CargoInfoFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item := Get_Widget(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      for I in PlayerShip.Cargo.Iterate loop
         ProtoIndex := PlayerShip.Cargo(I).ProtoIndex;
         if Items_List(ProtoIndex).ShowType /= Null_Unbounded_String then
            ItemType := Items_List(ProtoIndex).ShowType;
         else
            ItemType := Items_List(ProtoIndex).IType;
         end if;
         if Index(ItemsTypes, "{" & To_String(ItemType) & "}") = 0 then
            Append(ItemsTypes, " {" & To_String(ItemType) & "}");
         end if;
         if ItemsType /= "All" and then To_String(ItemType) /= ItemsType then
            goto End_Of_Loop;
         end if;
         CargoButton :=
           Create
             (CargoInfoFrame & ".name" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & GetItemName(PlayerShip.Cargo(I)) & "}");
         Add(CargoButton, "Show available item's options");
         Tcl.Tk.Ada.Grid.Grid
           (CargoButton, "-row" & Natural'Image(Row) & " -sticky w");
         if PlayerShip.Cargo(I).Durability > 74 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif PlayerShip.Cargo(I).Durability > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         DurabilityBar :=
           Create
             (CargoInfoFrame & ".durability" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(PlayerShip.Cargo(I).Durability) &
              "}" & To_String(ProgressBarStyle));
         Add(DurabilityBar, "The current durability of the selected item.");
         Tcl.Tk.Ada.Grid.Grid
           (DurabilityBar, "-row" & Natural'Image(Row) & " -column 1");
         ItemLabel :=
           Create
             (CargoInfoFrame & ".type" & Trim(Natural'Image(Row), Left),
              "-text {" & To_String(ItemType) & "}");
         Add(ItemLabel, "The type of the selected item.");
         Tcl.Tk.Ada.Grid.Grid
           (ItemLabel, "-row" & Natural'Image(Row) & " -column 2");
         ItemLabel :=
           Create
             (CargoInfoFrame & ".amount" & Trim(Natural'Image(Row), Left),
              "-text {" & Positive'Image(PlayerShip.Cargo(I).Amount) & "}");
         Add(ItemLabel, "The amount of the selected item.");
         Tcl.Tk.Ada.Grid.Grid
           (ItemLabel, "-row" & Natural'Image(Row) & " -column 3");
         ItemLabel :=
           Create
             (CargoInfoFrame & ".weight" & Trim(Natural'Image(Row), Left),
              "-text {" &
              Positive'Image
                (PlayerShip.Cargo(I).Amount * Items_List(ProtoIndex).Weight) &
              " kg}");
         Add(ItemLabel, "The total weight of the selected item.");
         Tcl.Tk.Ada.Grid.Grid
           (ItemLabel, "-row" & Natural'Image(Row) & " -column 4");
         <<End_Of_Loop>>
         Row := Row + 1;
      end loop;
      configure(TypeBox, "-values [list " & To_String(ItemsTypes) & "]");
      Tcl_Eval(Get_Context, "update");
      configure
        (ShipCanvas, "-scrollregion [list " & BBox(ShipCanvas, "all") & "]");
      Xview_Move_To(ShipCanvas, "0.0");
      Yview_Move_To(ShipCanvas, "0.0");
      return TCL_OK;
   end Show_Cargo_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowCargo", Show_Cargo_Command'Access);
   end AddCommands;

end Ships.UI.Cargo;
