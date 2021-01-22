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
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;

package body Table is

   function CreateTable
     (Parent: String; Headers: Headers_Array; With_Scrollbars: Boolean := True)
      return Table_Widget is
      Canvas: Tk_Canvas;
      YScroll: constant Ttk_Scrollbar :=
        Create
          (Parent & ".scrolly",
           "-orient vertical -command [list " & Parent & ".table yview]");
      XScroll: constant Ttk_Scrollbar :=
        Create
          (Parent & ".scrollx",
           "-orient horizontal -command [list " & Parent & ".table xview]");
      Table: Table_Widget (Headers'Length);
      X: Natural := 0;
      Tokens: Slice_Set;
      Master: constant Tk_Canvas := Get_Widget(Parent);
   begin
      if With_Scrollbars then
         Canvas :=
           Create
             (Parent & ".table",
              "-yscrollcommand [list " & Parent &
              ".scrolly set] -xscrollcommand [list " & Parent &
              ".scrollx set]");
         Tcl.Tk.Ada.Pack.Pack(YScroll, "-side right -fill y");
         Tcl.Tk.Ada.Pack.Pack(Canvas, "-side top -fill both -padx {5 0}");
         Tcl.Tk.Ada.Pack.Pack(XScroll, "-side bottom -fill x");
         Autoscroll(XScroll);
         Autoscroll(YScroll);
      else
         Canvas := Create(Parent & ".table");
         Tcl.Tk.Ada.Grid.Grid(Canvas, "-sticky nwes -padx {5 0}");
         Tcl.Tk.Ada.Grid.Column_Configure(Master, Canvas, "-weight 1");
         Tcl.Tk.Ada.Grid.Row_Configure(Master, Canvas, "-weight 1");
      end if;
      for I in Headers'Range loop
         Canvas_Create
           (Canvas, "text",
            Trim(Natural'Image(X), Left) & " 0 -anchor nw -text {" &
            To_String(Headers(I)) &
            "} -font InterfaceFont -justify center -fill [ttk::style lookup " &
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
      for Row in 1 .. Table.Row loop
         for Column in 1 .. Table.Amount loop
            Delete
              (Table.Canvas,
               "row" & Trim(Positive'Image(Row), Left) & "col" &
               Trim(Positive'Image(Column), Left));
         end loop;
      end loop;
      Table.Row := 1;
   end ClearTable;

   procedure AddText
     (Table: in out Table_Widget; Text, Tooltip: String; Column: Positive;
      NewRow: Boolean := False) is
      X: Natural := 0;
      ItemId: Unbounded_String;
      Tokens: Slice_Set;
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
              " -foreground] -tags [list row" &
              Trim(Positive'Image(Table.Row), Left) & "col" &
              Trim(Positive'Image(Column), Left) & "]"));
      if Tooltip'Length > 0 then
         Add(Table.Canvas, Tooltip, "-item " & To_String(ItemId));
      end if;
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
   end AddText;

   procedure AddButton
     (Table: in out Table_Widget; Text, Tooltip, Command: String;
      Column: Positive; NewRow: Boolean := False) is
      Tag: constant String :=
        "row" & Trim(Positive'Image(Table.Row), Left) & "col" &
        Trim(Positive'Image(Column), Left);
   begin
      AddText(Table, Text, Tooltip, Column, NewRow);
      Item_Configure
        (Table.Canvas, Tag,
         "-fill [ttk::style lookup " & To_String(GameSettings.InterfaceTheme) &
         " -selectforeground]");
      Bind
        (Table.Canvas, Tag, "<Enter>",
         "{" & Table.Canvas & " configure -cursor hand1}");
      Bind
        (Table.Canvas, Tag, "<Leave>",
         "{" & Table.Canvas & " configure -cursor left_ptr}");
      Bind(Table.Canvas, Tag, "<1>", "{" & Command & "}");
   end AddButton;

   procedure UpdateTable(Table: in out Table_Widget) is
      Tag: Unbounded_String;
      NewX: Natural := Table.Columns_Width(1);
      NewY: Natural := 0;
   begin
      for Column in 2 .. Table.Amount loop
         Tag :=
           To_Unbounded_String("header" & Trim(Natural'Image(Column), Left));
         Coords
           (Table.Canvas, To_String(Tag),
            Trim(Positive'Image(NewX), Left) & Positive'Image(NewY));
         for Row in 1 .. Table.Row loop
            NewY := NewY + Table.Row_Height;
            Tag :=
              To_Unbounded_String
                ("row" & Trim(Positive'Image(Row), Left) & "col" &
                 Trim(Natural'Image(Column), Left));
            Coords
              (Table.Canvas, To_String(Tag),
               Trim(Positive'Image(NewX), Left) & Positive'Image(NewY));
         end loop;
         NewX := NewX + Table.Columns_Width(Column);
         NewY := 0;
      end loop;
   end UpdateTable;

end Table;
