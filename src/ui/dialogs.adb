-- Copyright (c) 2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with CoreUI; use CoreUI;

package body Dialogs is

   function Create_Dialog
     (Name, Title: String; Title_Width: Positive := 275;
      Columns: Positive := 1; Parent_Name: String := ".gameframe")
      return Ttk_Frame is
      New_Dialog: constant Ttk_Frame := Create(Name, "-style Dialog.TFrame");
      Dialog_Header: constant Ttk_Label :=
        Create
          (New_Dialog & ".header",
           "-text {" & Title & "} -wraplength" & Positive'Image(Title_Width) &
           " -style Header.TLabel");
   begin
      if Parent_Name = ".gameframe" then
         Tcl.Tk.Ada.Busy.Busy(Game_Header);
         Tcl.Tk.Ada.Busy.Busy(Main_Paned);
      else
         Tcl.Tk.Ada.Busy.Busy(Ttk_Frame'(Get_Widget(Parent_Name)));
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (Dialog_Header,
         "-sticky we -padx 2 -pady {2 0}" &
         (if Columns > 1 then " -columnspan" & Positive'Image(Columns)
          else ""));
      return New_Dialog;
   end Create_Dialog;

   procedure Add_Close_Button(Name, Text, Command: String) is
      Button: constant Ttk_Button :=
        Create
          (Name,
           "-text {" & Text &
           "} -command {" & Command & "}");
   begin
      Tcl.Tk.Ada.Grid.Grid(Button, "-pady 5");
      Focus(Button);
      Bind(Button, "<Tab>", "{break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
   end Add_Close_Button;

end Dialogs;
