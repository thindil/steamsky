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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;

package body Table is

   function CreateTable
     (Parent: String; Headers: Headers_Array) return Table_Widget is
      Canvas: constant Tk_Canvas :=
        Create
          (Parent & ".table",
           "-yscrollcommand [list " & Parent &
           ".scrolly set] -xscrollcommand [list " & Parent & ".scrollx set]");
      YScroll: constant Ttk_Scrollbar :=
        Create
          (Parent & ".scrolly",
           "-orient vertical -command [list " & Canvas & " yview]");
      XScroll: constant Ttk_Scrollbar :=
        Create
          (Parent & ".scrollx",
           "-orient horizontal -command [list " & Canvas & " xview]");
      Table: Table_Widget (Headers'Length);
      X: Natural := 0;
      Tokens: Slice_Set;
   begin
      Tcl.Tk.Ada.Pack.Pack(YScroll, "-side right -fill y");
      Tcl.Tk.Ada.Pack.Pack(Canvas, "-side top -fill both");
      Tcl.Tk.Ada.Pack.Pack(XScroll, "-fill x");
      Autoscroll(YScroll);
      Autoscroll(XScroll);
      for I in Headers'Range loop
         Canvas_Create
           (Canvas, "text",
            Trim(Natural'Image(X), Left) & " 0 -anchor nw -text {" &
            To_String(Headers(I)) &
            "} -font InterfaceFont -fill [ttk::style lookup " &
            To_String(GameSettings.InterfaceTheme) &
            " -foreground] -tags [list header" &
            Trim(Positive'Image(I), Left) & "]");
         Create
           (Tokens, BBox(Canvas, "header" & Trim(Positive'Image(I), Left)),
            " ");
         X := Positive'Value(Slice(Tokens, 3)) + 10;
         Table.Columns_Width(I) := X - Positive'Value(Slice(Tokens, 1));
         if I = 1 then
            Table.Row_Height := Positive'Value(Slice(Tokens, 4)) + 5;
         end if;
      end loop;
      Table.Canvas := Canvas;
      return Table;
   end CreateTable;

   procedure ClearTable(Table: in out Table_Widget) is
   begin
      for I in 1 .. Table.Row loop
         Delete(Table.Canvas, "row" & Trim(Positive'Image(I), Left));
      end loop;
      Table.Row := 1;
   end ClearTable;

   procedure AddButton
     (Table: in out Table_Widget; Text, Tooltip, Command: String;
      Column: Positive; NewRow: Boolean := False) is
      X: Natural := 0;
      ItemId: Unbounded_String;
      Tokens: Slice_Set;
      Tag: constant String :=
        "row" & Trim(Positive'Image(Table.Row), Left) & "col" &
        Trim(Positive'Image(Column), Left);
   begin
      for I in 1 .. Column - 1 loop
         X := X + Table.Columns_Width(I);
      end loop;
      ItemId :=
        To_Unbounded_String
          (Canvas_Create
             (Table.Canvas, "text",
              Trim(Natural'Image(X), Left) &
              Positive'Image(Table.Row * Table.Row_Height) &
              " -anchor nw -text {" & Text &
              "} -font InterfaceFont -fill [ttk::style lookup " &
              To_String(GameSettings.InterfaceTheme) &
              " -selectforeground] -justify center -tags [list " & Tag & "]"));
      Add(Table.Canvas, Tooltip, "-item " & To_String(ItemId));
      Bind
        (Table.Canvas, Tag, "<Enter>",
         "{" & Table.Canvas & " configure -cursor hand1}");
      Bind
        (Table.Canvas, Tag, "<Leave>",
         "{" & Table.Canvas & " configure -cursor left_ptr}");
      Create(Tokens, BBox(Table.Canvas, To_String(ItemId)), " ");
      X :=
        (Positive'Value(Slice(Tokens, 3)) + 10) -
        Positive'Value(Slice(Tokens, 1));
      if X > Table.Columns_Width(Column) then
         Table.Columns_Width(Column) := X;
      end if;
      if NewRow then
         Table.Row := Table.Row + 1;
      end if;
   end AddButton;

end Table;
