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
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
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
      X: Natural := 5;
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
         Table.Scrollbars := True;
      else
         Canvas := Create(Parent & ".table");
         Tcl.Tk.Ada.Grid.Grid(Canvas, "-sticky nwes -padx {5 0}");
         Tcl.Tk.Ada.Grid.Column_Configure(Master, Canvas, "-weight 1");
         Tcl.Tk.Ada.Grid.Row_Configure(Master, Canvas, "-weight 1");
         Table.Scrollbars := False;
      end if;
      for I in Headers'Range loop
         Canvas_Create
           (Canvas, "text",
            Trim(Natural'Image(X), Left) & " 2 -anchor nw -text {" &
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
      Canvas_Create
        (Canvas, "rectangle",
         "0 0" & Positive'Image(X) & Positive'Image(Table.Row_Height - 3) &
         " -fill " & Style_Lookup("Table", "-headercolor") & " -outline " &
         Style_Lookup("Table", "-rowcolor") &
         " -width 2 -tags [list headerback]");
      Lower(Canvas, "headerback");
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
            Delete(Table.Canvas, "row" & Trim(Positive'Image(Row), Left));
            Delete
              (Table.Canvas,
               "progressbar" & Trim(Positive'Image(Row), Left) & "back" &
               Trim(Positive'Image(Column), Left));
            Delete
              (Table.Canvas,
               "progressbar" & Trim(Positive'Image(Row), Left) & "bar" &
               Trim(Positive'Image(Column), Left));
         end loop;
      end loop;
      Table.Row := 1;
   end ClearTable;

   function AddBackground
     (Table: Table_Widget; NewRow: Boolean) return String is
      ItemId: Unbounded_String;
      Color: constant String :=
        (if Table.Row rem 2 > 0 then
           Style_Lookup(To_String(GameSettings.InterfaceTheme), "-troughcolor")
         else Style_Lookup
             (To_String(GameSettings.InterfaceTheme), "-background"));
   begin
      if not NewRow then
         return Color;
      end if;
      ItemId :=
        To_Unbounded_String
          (Canvas_Create
             (Table.Canvas, "rectangle",
              " 0" & Positive'Image((Table.Row * Table.Row_Height)) & " 10" &
              Positive'Image
                ((Table.Row * Table.Row_Height) + (Table.Row_Height)) &
              " -fill " & Color & " -width 0 -tags [list row" &
              Trim(Positive'Image(Table.Row), Left) & "]"));
      Lower(Table.Canvas, To_String(ItemId));
      Bind
        (Table.Canvas, To_String(ItemId), "<Enter>",
         "{" & Table.Canvas & " itemconfigure " & To_String(ItemId) &
         " -fill " &
         Style_Lookup
           (To_String(GameSettings.InterfaceTheme), "-selectbackground") &
         "}");
      Bind
        (Table.Canvas, To_String(ItemId), "<Leave>",
         "{" & Table.Canvas & " itemconfigure " & To_String(ItemId) &
         " -fill " & Color & "}");
      return Color;
   end AddBackground;

   procedure AddText
     (Table: in out Table_Widget; Text, Tooltip: String; Column: Positive;
      NewRow: Boolean := False; Color: String := "") is
      X: Natural := 5;
      ItemId: Unbounded_String;
      Tokens: Slice_Set;
      Text_Color: constant String :=
        (if Color'Length > 0 then Color
         else Style_Lookup
             (To_String(GameSettings.InterfaceTheme), "-foreground"));
      Background_Color: constant String := AddBackground(Table, NewRow);
   begin
      for I in 1 .. Column - 1 loop
         X := X + Table.Columns_Width(I);
      end loop;
      ItemId :=
        To_Unbounded_String
          (Canvas_Create
             (Table.Canvas, "text",
              Trim(Natural'Image(X), Left) &
              Positive'Image((Table.Row * Table.Row_Height) + 2) &
              " -anchor nw -text {" & Text & "} -font InterfaceFont -fill " &
              Text_Color & " -tags [list row" &
              Trim(Positive'Image(Table.Row), Left) & "col" &
              Trim(Positive'Image(Column), Left) & "]"));
      if Tooltip'Length > 0 then
         Add(Table.Canvas, Tooltip, "-item " & To_String(ItemId));
      end if;
      Bind
        (Table.Canvas, To_String(ItemId), "<Enter>",
         "{" & Table.Canvas & " itemconfigure row" &
         Trim(Positive'Image(Table.Row), Left) & " -fill " &
         Style_Lookup
           (To_String(GameSettings.InterfaceTheme), "-selectbackground") &
         "}");
      Bind
        (Table.Canvas, To_String(ItemId), "<Leave>",
         "{" & Table.Canvas & " itemconfigure row" &
         Trim(Positive'Image(Table.Row), Left) & " -fill " & Background_Color &
         "}");
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
      Column: Positive; NewRow: Boolean := False; Color: String := "") is
      Tag: constant String :=
        "row" & Trim(Positive'Image(Table.Row), Left) & "col" &
        Trim(Positive'Image(Column), Left);
   begin
      AddText(Table, Text, Tooltip, Column, NewRow, Color);
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
      NewY: Natural := 2;
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
            MoveTo
              (Table.Canvas, To_String(Tag), Trim(Positive'Image(NewX), Left),
               Trim(Positive'Image(NewY), Left));
            Tag :=
              To_Unbounded_String
                ("progressbar" & Trim(Positive'Image(Row), Left) & "back" &
                 Trim(Natural'Image(Column), Left));
            MoveTo
              (Table.Canvas, To_String(Tag), Trim(Positive'Image(NewX), Left),
               Trim(Positive'Image(NewY + 5), Left));
            Tag :=
              To_Unbounded_String
                ("progressbar" & Trim(Positive'Image(Row), Left) & "bar" &
                 Trim(Natural'Image(Column), Left));
            MoveTo
              (Table.Canvas, To_String(Tag),
               Trim(Positive'Image(NewX + 2), Left),
               Trim(Positive'Image(NewY + 7), Left));
         end loop;
         NewX := NewX + Table.Columns_Width(Column);
         NewY := 2;
      end loop;
      declare
         Tokens: Slice_Set;
      begin
         Create(Tokens, BBox(Table.Canvas, "all"), " ");
            -- if no scrollbars, resize the table
         if not Table.Scrollbars then
            configure
              (Table.Canvas,
               "-height [expr " & Slice(Tokens, 4) & " - " & Slice(Tokens, 2) &
               "] -width [expr " & Slice(Tokens, 3) & " - " &
               Slice(Tokens, 1) & " + 5]");
         end if;
         Coords
           (Table.Canvas, "headerback",
            "0 0" & Positive'Image(Positive'Value(Slice(Tokens, 3)) + 5) &
            Positive'Image(Table.Row_Height - 3));
         NewY := Table.Row_Height;
         Resize_Background_Loop :
         for Row in 1 .. Table.Row loop
            NewY := NewY + Table.Row_Height;
            Tag :=
              To_Unbounded_String("row" & Trim(Positive'Image(Row), Left));
            Coords
              (Table.Canvas, To_String(Tag),
               "0" & Positive'Image(NewY - Table.Row_Height) &
               Positive'Image(Positive'Value(Slice(Tokens, 3)) + 5) &
               Positive'Image(NewY));
         end loop Resize_Background_Loop;
      end;
   end UpdateTable;

   procedure AddProgressBar
     (Table: in out Table_Widget; Value: Natural; MaxValue: Positive;
      Tooltip: String; Column: Positive; NewRow: Boolean := False) is
      X: Natural := 0;
      ItemId: Unbounded_String;
      Tokens: Slice_Set;
      Length: constant Natural :=
        Natural((Float(Value) / Float(MaxValue)) * Float(MaxValue));
      Color: constant String :=
        (if Length > 74 then
           Style_Lookup("green.Horizontal.TProgressbar", "-background")
         elsif Length > 24 then
           Style_Lookup("yellow.Horizontal.TProgressbar", "-background")
         else Style_Lookup("TProgressbar", "-background"));
      Background_Color: constant String := AddBackground(Table, NewRow);
   begin
      for I in 1 .. Column - 1 loop
         X := X + Table.Columns_Width(I);
      end loop;
      ItemId :=
        To_Unbounded_String
          (Canvas_Create
             (Table.Canvas, "rectangle",
              Trim(Natural'Image(X), Left) &
              Positive'Image((Table.Row * Table.Row_Height) + 5) &
              Positive'Image(X + 102) &
              Positive'Image
                ((Table.Row * Table.Row_Height) + (Table.Row_Height - 10)) &
              " -fill " & Style_Lookup("TProgressbar", "-troughcolor") &
              " -outline " & Style_Lookup("TProgressbar", "-bordercolor") &
              " -tags [list progressbar" &
              Trim(Positive'Image(Table.Row), Left) & "back" &
              Trim(Positive'Image(Column), Left) & "]"));
      Bind
        (Table.Canvas, To_String(ItemId), "<Enter>",
         "{" & Table.Canvas & " itemconfigure row" &
         Trim(Positive'Image(Table.Row), Left) & " -fill " &
         Style_Lookup
           (To_String(GameSettings.InterfaceTheme), "-selectbackground") &
         "}");
      Bind
        (Table.Canvas, To_String(ItemId), "<Leave>",
         "{" & Table.Canvas & " itemconfigure row" &
         Trim(Positive'Image(Table.Row), Left) & " -fill " & Background_Color &
         "}");
      if Tooltip'Length > 0 then
         Add(Table.Canvas, Tooltip, "-item " & To_String(ItemId));
      end if;
      ItemId :=
        To_Unbounded_String
          (Canvas_Create
             (Table.Canvas, "rectangle",
              Trim(Natural'Image(X + 2), Left) &
              Positive'Image((Table.Row * Table.Row_Height) + 7) &
              Positive'Image(X + Length) &
              Positive'Image
                ((Table.Row * Table.Row_Height) + (Table.Row_Height - 12)) &
              " -fill " & Color & " -tags [list progressbar" &
              Trim(Positive'Image(Table.Row), Left) & "bar" &
              Trim(Positive'Image(Column), Left) & "]"));
      Bind
        (Table.Canvas, To_String(ItemId), "<Enter>",
         "{" & Table.Canvas & " itemconfigure row" &
         Trim(Positive'Image(Table.Row), Left) & " -fill " &
         Style_Lookup
           (To_String(GameSettings.InterfaceTheme), "-selectbackground") &
         "}");
      Bind
        (Table.Canvas, To_String(ItemId), "<Leave>",
         "{" & Table.Canvas & " itemconfigure row" &
         Trim(Positive'Image(Table.Row), Left) & " -fill " & Background_Color &
         "}");
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
   end AddProgressBar;

end Table;
