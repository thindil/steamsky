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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Crew.Inventory; use Crew.Inventory;
with Factions; use Factions;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Ships.UI.Crew.Inventory is

   -- ****o* SUCI/Show_Member_Inventory_Command
   -- FUNCTION
   -- Show inventory of the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberInventory memberindex
   -- MemberIndex is the index of the crew member to show inventory
   -- SOURCE
   function Show_Member_Inventory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Member_Inventory_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
      Member: constant Member_Data := PlayerShip.Crew(MemberIndex);
      MemberDialog: constant Tk_Toplevel :=
        Create
          (".memberdialog",
           "-class Dialog -background [ttk::style lookup . -background] -relief solid -borderwidth 2");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Interp);
      XScroll: constant Ttk_Scrollbar :=
        Create
          (MemberDialog & ".xscroll",
           "-orient horizontal -command [list .memberdialog.canvas xview]");
      YScroll: constant Ttk_Scrollbar :=
        Create
          (MemberDialog & ".yscroll",
           "-orient vertical -command [list .memberdialog.canvas yview]");
      MemberCanvas: constant Tk_Canvas :=
        Create
          (MemberDialog & ".canvas",
           "-yscrollcommand [list " & YScroll &
           " set] -xscrollcommand [list " & XScroll & " set]");
      MemberFrame: constant Ttk_Frame := Create(MemberCanvas & ".frame");
      CloseButton: constant Ttk_Button :=
        Create
          (MemberFrame & ".button",
           "-text Close -command {CloseDialog " & MemberDialog & "}");
      Height, Width, NewWidth: Positive := 10;
      Label, AmountLabel, WeightLabel, ItemLabel: Ttk_Label;
      ItemButton: Ttk_MenuButton;
      DamageBar: Ttk_ProgressBar;
      ProgressBarStyle: Unbounded_String;
      ItemMenu: Tk_Menu;
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      Wm_Set
        (MemberDialog, "title",
         "{Steam Sky - " & To_String(Member.Name) & " Orders Priorities}");
      Wm_Set(MemberDialog, "transient", ".");
      if Tcl_GetVar(Interp, "tcl_platform(os)") = "Linux" then
         Wm_Set(MemberDialog, "attributes", "-type dialog");
      end if;
      Tcl.Tk.Ada.Pack.Pack(YScroll, " -side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(MemberCanvas, "-expand true -fill both");
      Tcl.Tk.Ada.Pack.Pack(XScroll, "-fill x");
      Autoscroll(YScroll);
      Autoscroll(XScroll);
      Label := Create(MemberFrame & ".name", "-text {Name}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Label := Create(MemberFrame & ".durability", "-text {Durability}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 1 -row 0");
      Label := Create(MemberFrame & ".used", "-text {Used}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-column 2 -row 0");
      AmountLabel := Create(MemberFrame & ".amount", "-text {Amount}");
      Tcl.Tk.Ada.Grid.Grid(AmountLabel, "-column 3 -row 0");
      WeightLabel := Create(MemberFrame & ".weight", "-text {Weight}");
      Tcl.Tk.Ada.Grid.Grid(WeightLabel, "-column 4 -row 0");
      Height := Height + Positive'Value(Winfo_Get(Label, "reqheight"));
      ItemMenu.Interp := Interp;
      for I in Member.Inventory.Iterate loop
         ItemMenu.Name :=
           New_String
             (".itemmenu" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left));
         if (Winfo_Get(ItemMenu, "exists")) = "0" then
            ItemMenu :=
              Create
                (".itemmenu" &
                 Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
                 "-tearoff false");
         end if;
         Delete(ItemMenu, "0", "end");
         if ItemIsUsed(MemberIndex, Inventory_Container.To_Index(I)) then
            Menu.Add(ItemMenu, "command", "-label {Unequip}");
            Label :=
              Create
                (MemberFrame & ".used" &
                 Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
                 "-text {Yes}");
         else
            Menu.Add(ItemMenu, "command", "-label {Equip}");
            Label :=
              Create
                (MemberFrame & ".used" &
                 Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
                 "-text {No}");
         end if;
         Menu.Add
           (ItemMenu, "command", "-label {Move the item to the ship cargo}");
         Menu.Add
           (ItemMenu, "command", "-label {Show more info about the item}");
         ItemButton :=
           Create
             (MemberFrame & ".name" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & GetItemName(Member.Inventory(I), False, False) &
              "} -menu " & ItemMenu);
         Add(ItemButton, "Show available item's options");
         Tcl.Tk.Ada.Grid.Grid(ItemButton, "-sticky w");
         Height := Height + Positive'Value(Winfo_Get(ItemButton, "reqheight"));
         if Member.Inventory(I).Durability > 74 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif Member.Inventory(I).Durability > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         DamageBar :=
           Create
             (MemberFrame & ".durability" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-value {" & Positive'Image(Member.Inventory(I).Durability) &
              "}" & To_String(ProgressBarStyle));
         Add(DamageBar, "The current durability level of the selected item.");
         Tcl.Tk.Ada.Grid.Grid
           (DamageBar,
            "-row" & Positive'Image(Inventory_Container.To_Index(I)) &
            " -column 1");
         Tcl.Tk.Ada.Grid.Grid
           (Label,
            "-row" & Positive'Image(Inventory_Container.To_Index(I)) &
            " -column 2");
         ItemLabel :=
           Create
             (MemberFrame & ".amount" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" & Positive'Image(Member.Inventory(I).Amount) & "}");
         Tcl.Tk.Ada.Grid.Grid
           (ItemLabel,
            "-row" & Positive'Image(Inventory_Container.To_Index(I)) &
            " -column 3");
         ItemLabel :=
           Create
             (MemberFrame & ".weight" &
              Trim(Positive'Image(Inventory_Container.To_Index(I)), Left),
              "-text {" &
              Positive'Image
                (Member.Inventory(I).Amount *
                 Items_List(Member.Inventory(I).ProtoIndex).Weight) &
              "}");
         Tcl.Tk.Ada.Grid.Grid
           (ItemLabel,
            "-row" & Positive'Image(Inventory_Container.To_Index(I)) &
            " -column 4");
         NewWidth :=
           Positive'Value(Winfo_Get(ItemButton, "reqwidth")) +
           Positive'Value(Winfo_Get(DamageBar, "reqwidth")) +
           Positive'Value(Winfo_Get(Label, "reqwidth")) +
           Positive'Value(Winfo_Get(AmountLabel, "reqwidth")) +
           Positive'Value(Winfo_Get(WeightLabel, "reqwidth"));
         if NewWidth > Width then
            Width := NewWidth;
         end if;
      end loop;
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-columnspan 5");
      Height := Height + Positive'Value(Winfo_Get(CloseButton, "reqheight"));
      Focus(CloseButton);
      if Height > 500 then
         Height := 500;
      end if;
      configure
        (MemberFrame,
         "-height" & Positive'Image(Height) & " -width" &
         Positive'Image(Width));
      Canvas_Create
        (MemberCanvas, "window", "0 0 -anchor nw -window " & MemberFrame);
      configure
        (MemberCanvas,
         "-scrollregion [list " & BBox(MemberCanvas, "all") & "]");
      Height := Height + 30;
      Width := Width + 30;
      declare
         X, Y: Integer;
      begin
         X :=
           (Positive'Value(Winfo_Get(MemberDialog, "vrootwidth")) - Width) / 2;
         if X < 0 then
            X := 0;
         end if;
         Y :=
           (Positive'Value(Winfo_Get(MemberDialog, "vrootheight")) - Height) /
           2;
         if Y < 0 then
            Y := 0;
         end if;
         Wm_Set
           (MemberDialog, "geometry",
            Trim(Positive'Image(Width), Left) & "x" &
            Trim(Positive'Image(Height), Left) & "+" &
            Trim(Positive'Image(X), Left) & "+" &
            Trim(Positive'Image(Y), Left));
         Bind(MemberDialog, "<Destroy>", "{CloseDialog " & MemberDialog & "}");
         Bind(MemberDialog, "<Escape>", "{CloseDialog " & MemberDialog & "}");
         Tcl_Eval(Interp, "update");
      end;
      return TCL_OK;
   end Show_Member_Inventory_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowMemberInventory", Show_Member_Inventory_Command'Access);
   end AddCommands;

end Ships.UI.Crew.Inventory;
