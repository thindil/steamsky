-- Copyright (c) 2021-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Interfaces.C;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with Utils.UI; use Utils.UI;

package body Table is

   function Create_Table
     (Parent: String; Headers: Headers_Array;
      Scrollbar: Ttk_Scrollbar := Get_Widget(pathName => ".");
      Command, Tooltip: String := "") return Table_Widget is
      Canvas: Tk_Canvas;
      Y_Scroll: Ttk_Scrollbar;
      X_Scroll: Ttk_Scrollbar;
      Table: Table_Widget (Amount => Headers'Length);
      X: Natural := 5;
      Tokens: Slice_Set;
      Master: constant Tk_Canvas := Get_Widget(pathName => Parent);
      Header_Id: Unbounded_String;
   begin
      if Widget_Image(Win => Scrollbar) = "." then
         Y_Scroll :=
           Create
             (pathName => Parent & ".scrolly",
              options =>
                "-orient vertical -command [list " & Parent & ".table yview]");
         X_Scroll :=
           Create
             (pathName => Parent & ".scrollx",
              options =>
                "-orient horizontal -command [list " & Parent &
                ".table xview]");
         Canvas :=
           Create
             (pathName => Parent & ".table",
              options =>
                "-yscrollcommand [list " & Parent &
                ".scrolly set] -xscrollcommand [list " & Parent &
                ".scrollx set]");
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Y_Scroll, Options => "-side right -fill y");
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Canvas, Options => "-side top -fill both -padx {5 0}");
         Tcl.Tk.Ada.Pack.Pack
           (Slave => X_Scroll, Options => "-side bottom -fill x");
         Autoscroll(Scroll => X_Scroll);
         Autoscroll(Scroll => Y_Scroll);
         Table.Scrollbar := Y_Scroll;
      else
         Canvas := Create(pathName => Parent & ".table");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Canvas, Options => "-sticky nwes -padx {5 0}");
         Tcl.Tk.Ada.Grid.Column_Configure
           (Master => Master, Slave => Canvas, Options => "-weight 1");
         Tcl.Tk.Ada.Grid.Row_Configure
           (Master => Master, Slave => Canvas, Options => "-weight 1");
         Table.Scrollbar := Scrollbar;
      end if;
      Create_Headers_Loop :
      for I in Headers'Range loop
         Header_Id :=
           To_Unbounded_String
             (Source =>
                Canvas_Create
                  (Parent => Canvas, Child_Type => "text",
                   Options =>
                     Trim(Source => Natural'Image(X), Side => Left) &
                     " 2 -anchor nw -text {" &
                     To_String(Source => Headers(I)) &
                     "} -font InterfaceFont -justify center -fill [ttk::style lookup " &
                     To_String(Source => Game_Settings.Interface_Theme) &
                     " -foreground] -tags [list header" &
                     Trim(Source => Positive'Image(I), Side => Left) & "]"));
         if Command'Length > 0 then
            Bind
              (CanvasWidget => Canvas,
               TagOrId => To_String(Source => Header_Id),
               Sequence => "<Enter>",
               Command => "{" & Canvas & " configure -cursor hand1}");
            Bind
              (CanvasWidget => Canvas,
               TagOrId => To_String(Source => Header_Id),
               Sequence => "<Leave>",
               Command => "{" & Canvas & " configure -cursor left_ptr}");
            Bind
              (CanvasWidget => Canvas,
               TagOrId => To_String(Source => Header_Id),
               Sequence => "<Button-1>", Command => "{" & Command & " %x}");
         end if;
         if Tooltip'Length > 0 then
            Add
              (Widget => Canvas, Message => Tooltip,
               Options => "-item " & To_String(Source => Header_Id));
         end if;
         Create
           (S => Tokens,
            From =>
              BBox
                (CanvasWidget => Canvas,
                 TagOrId =>
                   "header" & Trim(Source => Positive'Image(I), Side => Left)),
            Separators => " ");
         X := Positive'Value(Slice(S => Tokens, Index => 3)) + 10;
         Table.Columns_Width(I) :=
           X - Positive'Value(Slice(S => Tokens, Index => 1));
         if I = 1 then
            Table.Row_Height :=
              Positive'Value(Slice(S => Tokens, Index => 4)) + 5;
         end if;
      end loop Create_Headers_Loop;
      Header_Id :=
        To_Unbounded_String
          (Source =>
             Canvas_Create
               (Parent => Canvas, Child_Type => "rectangle",
                Options =>
                  "0 0" & Positive'Image(X) &
                  Positive'Image(Table.Row_Height - 3) & " -fill " &
                  Style_Lookup(Name => "Table", Option => "-headercolor") &
                  " -outline " &
                  Style_Lookup(Name => "Table", Option => "-rowcolor") &
                  " -width 2 -tags [list headerback]"));
      Lower(CanvasWidget => Canvas, TagOrId => "headerback");
      if Command'Length > 0 then
         Bind
           (CanvasWidget => Canvas, TagOrId => To_String(Source => Header_Id),
            Sequence => "<Enter>",
            Command => "{" & Canvas & " configure -cursor hand1}");
         Bind
           (CanvasWidget => Canvas, TagOrId => To_String(Source => Header_Id),
            Sequence => "<Leave>",
            Command => "{" & Canvas & " configure -cursor left_ptr}");
         Bind
           (CanvasWidget => Canvas, TagOrId => To_String(Source => Header_Id),
            Sequence => "<Button-1>", Command => "{" & Command & " %x}");
      end if;
      if Tooltip'Length > 0 then
         Add
           (Widget => Canvas, Message => Tooltip,
            Options => "-item " & To_String(Source => Header_Id));
      end if;
      Table.Canvas := Canvas;
      Tcl_Eval
        (interp => Get_Context,
         strng =>
           "SetScrollbarBindings " & Table.Canvas & " " & Table.Scrollbar);
      Bind
        (Widgt => Table.Canvas, Sequence => "<Up>",
         Script => "{UpdateCurrentRow " & Table.Canvas & " lower}");
      Bind
        (Widgt => Table.Canvas, Sequence => "<Down>",
         Script => "{UpdateCurrentRow " & Table.Canvas & " raise}");
      Bind
        (Widgt => Table.Canvas, Sequence => "<Key-space>",
         Script => "{ExecuteCurrentRow " & Table.Canvas & "}");
      Bind
        (Widgt => Table.Canvas, Sequence => "<FocusOut>",
         Script => "{HideCurrentRow " & Table.Canvas & "}");
      Bind
        (Widgt => Table.Canvas, Sequence => "<Leave>",
         Script => "{HideCurrentRow " & Table.Canvas & "}");
      return Table;
   end Create_Table;

   procedure Clear_Table(Table: in out Table_Widget) is
      Buttons_Frame: Ttk_Frame :=
        Get_Widget(pathName => Table.Canvas & ".buttonframe");
      Button: Ttk_Button;
   begin
      if Winfo_Get(Widgt => Buttons_Frame, Info => "exists") = "1" then
         Button := Get_Widget(pathName => Buttons_Frame & ".previous");
         Destroy(Widgt => Button);
         Button := Get_Widget(pathName => Buttons_Frame & ".next");
         Destroy(Widgt => Button);
         Destroy(Widgt => Buttons_Frame);
      end if;
      Clear_Rows_Loop :
      for Row in 1 .. Table.Row loop
         Clear_Columns_Loop :
         for Column in 1 .. Table.Amount loop
            Delete
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "row" & Trim(Source => Positive'Image(Row), Side => Left) &
                 "col" & Trim(Source => Positive'Image(Column), Side => Left));
            Delete
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "row" & Trim(Source => Positive'Image(Row), Side => Left));
            Delete
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "progressbar" &
                 Trim(Source => Positive'Image(Row), Side => Left) & "back" &
                 Trim(Source => Positive'Image(Column), Side => Left));
            Delete
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "progressbar" &
                 Trim(Source => Positive'Image(Row), Side => Left) & "bar" &
                 Trim(Source => Positive'Image(Column), Side => Left));
         end loop Clear_Columns_Loop;
      end loop Clear_Rows_Loop;
      Table.Row := 1;
   end Clear_Table;

   -- ****if* Table/Add_Bindings
   -- FUNCTION
   -- Add events to the selected element of the Table_Widget
   -- PARAMETERS
   -- Canvas   - Tk_Canvas in which the events will be added
   -- Item_Id  - The id of the item to which the events will be added
   -- Row      - The number of row in which the events will be added
   -- Command  - The Tcl command which will be executed on mouse button event
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   procedure Add_Bindings
     (Canvas: Tk_Canvas; Item_Id, Row, Command, Color: String) is
     -- ****
   begin
      Bind
        (CanvasWidget => Canvas, TagOrId => Item_Id, Sequence => "<Enter>",
         Command =>
           "{" & Canvas & " itemconfigure row$currentrow -fill " & Color &
           ";" & Canvas & " itemconfigure row" & Row & " -fill " &
           Style_Lookup
             (Name => To_String(Source => Game_Settings.Interface_Theme),
              Option => "-selectbackground") &
           (if Command'Length > 0 then
              ";" & Canvas & " configure -cursor hand1"
            else "") &
           ";set currentrow " & Row & "}");
      Bind
        (CanvasWidget => Canvas, TagOrId => Item_Id, Sequence => "<Leave>",
         Command => "{" & Canvas & " configure -cursor left_ptr}");
      if Command'Length > 0 then
         Bind
           (CanvasWidget => Canvas, TagOrId => Item_Id,
            Sequence =>
              "<Button-" & (if Game_Settings.Right_Button then "3" else "1") &
              ">",
            Command => "{" & Command & "}");
      end if;
   end Add_Bindings;

   -- ****if* Table/Table.Add_Background
   -- FUNCTION
   -- Add a proper background color to the item in the table and return the
   -- name of used color
   -- PARAMETERS
   -- Table    - The Table_Widget in which background will be added
   -- New_Row  - If True, add the background, otherwise just return the color
   --            which will be used
   -- Command  - Tcl command which will be executed when the background was
   --            clicked
   -- RESULT
   -- The String with the name of the color used for set background for the
   -- item
   -- SOURCE
   function Add_Background
     (Table: Table_Widget; New_Row: Boolean; Command: String) return String is
     -- ****
      Item_Id: Unbounded_String;
      Color: constant String :=
        (if Table.Row rem 2 > 0 then Style_Lookup(Name => "Table", Option => "-rowcolor")
         else Style_Lookup
             (Name => To_String(Source => Game_Settings.Interface_Theme), Option => "-background"));
   begin
      if not New_Row then
         return Color;
      end if;
      Item_Id :=
        To_Unbounded_String
          (Source => Canvas_Create
             (Parent => Table.Canvas, Child_Type => "rectangle",
              Options => " 0" & Positive'Image((Table.Row * Table.Row_Height)) & " 10" &
              Positive'Image
                ((Table.Row * Table.Row_Height) + (Table.Row_Height)) &
              " -fill " & Color & " -width 0 -tags [list row" &
              Trim(Source => Positive'Image(Table.Row), Side => Left) & "]"));
      Lower(CanvasWidget => Table.Canvas, TagOrId => To_String(Source => Item_Id));
      Add_Bindings
        (Canvas => Table.Canvas, Item_Id => "row" & Trim(Source => Positive'Image(Table.Row), Side => Left),
         Row => Trim(Source => Positive'Image(Table.Row), Side => Left), Command => Command, Color => Color);
      return Color;
   end Add_Background;

   procedure Add_Button
     (Table: in out Table_Widget; Text, Tooltip, Command: String;
      Column: Positive; New_Row: Boolean := False; Color: String := "") is
      X: Natural := 5;
      ItemId: Unbounded_String;
      Tokens: Slice_Set;
      Text_Color: constant String :=
        (if Color'Length > 0 then Color
         else Style_Lookup
             (To_String(Game_Settings.Interface_Theme), "-foreground"));
      Background_Color: constant String :=
        Add_Background(Table, New_Row, Command);
   begin
      Count_X_Loop :
      for I in 1 .. Column - 1 loop
         X := X + Table.Columns_Width(I);
      end loop Count_X_Loop;
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
      Add_Bindings
        (Table.Canvas, To_String(ItemId),
         Trim(Positive'Image(Table.Row), Left), Command, Background_Color);
      Create(Tokens, BBox(Table.Canvas, To_String(ItemId)), " ");
      X :=
        (Positive'Value(Slice(Tokens, 3)) + 10) -
        Positive'Value(Slice(Tokens, 1));
      if X > Table.Columns_Width(Column) then
         Table.Columns_Width(Column) := X;
      end if;
      if New_Row then
         Table.Row := Table.Row + 1;
      end if;
   end Add_Button;

   procedure Update_Table
     (Table: in out Table_Widget; Grab_Focus: Boolean := True) is
      Tag: Unbounded_String;
      NewX: Natural := Table.Columns_Width(1) + 20;
      NewY: Natural := 2;
   begin
      Update_Columns_Loop :
      for Column in 2 .. Table.Amount loop
         Tag :=
           To_Unbounded_String("header" & Trim(Natural'Image(Column), Left));
         Coords
           (Table.Canvas, To_String(Tag),
            Trim(Positive'Image(NewX), Left) & Positive'Image(NewY));
         Update_Rows_Loop :
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
         end loop Update_Rows_Loop;
         NewX := NewX + Table.Columns_Width(Column) + 20;
         NewY := 2;
      end loop Update_Columns_Loop;
      declare
         Tokens: Slice_Set;
      begin
         Create(Tokens, BBox(Table.Canvas, "all"), " ");
            -- if no scrollbars, resize the table
         if Winfo_Get(Table.Canvas, "parent") /=
           Winfo_Get(Table.Scrollbar, "parent") then
            configure
              (Table.Canvas,
               "-height [expr " & Slice(Tokens, 4) & " - " & Slice(Tokens, 2) &
               "] -width [expr " & Slice(Tokens, 3) & " - " &
               Slice(Tokens, 1) & " + 5]");
         end if;
         Coords
           (Table.Canvas, "headerback",
            "0 0" & Positive'Image(Positive'Value(Slice(Tokens, 3)) - 1) &
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
               Positive'Image(Positive'Value(Slice(Tokens, 3)) - 1) &
               Positive'Image(NewY));
         end loop Resize_Background_Loop;
      end;
      Tcl_SetVar(Get_Context, "currentrow", "1");
      Bind
        (Table.Canvas, "<FocusIn>",
         "{set maxrows" & Natural'Image(Table.Row) &
         ";if {$currentrow > $maxrows} {set currentrow 1};" & Table.Canvas &
         " itemconfigure row$currentrow -fill [ttk::style lookup " &
         To_String(Game_Settings.Interface_Theme) & " -selectbackground]}");
      if Grab_Focus then
         Widgets.Focus(Table.Canvas);
      end if;
   end Update_Table;

   procedure Add_Progress_Bar
     (Table: in out Table_Widget; Value: Natural; Max_Value: Positive;
      Tooltip, Command: String; Column: Positive;
      New_Row, Invert_Colors: Boolean := False) is
      X: Natural := 0;
      ItemId: Unbounded_String;
      Tokens: Slice_Set;
      Length: constant Natural :=
        Natural
          (100.0 +
           ((Float(Value) - Float(Max_Value)) / Float(Max_Value) * 100.0));
      Color: Unbounded_String;
      Background_Color: constant String :=
        Add_Background(Table, New_Row, Command);
   begin
      Count_X_Loop :
      for I in 1 .. Column - 1 loop
         X := X + Table.Columns_Width(I);
      end loop Count_X_Loop;
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
      Add_Bindings
        (Table.Canvas, To_String(ItemId),
         Trim(Positive'Image(Table.Row), Left), Command, Background_Color);
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
      if not Invert_Colors then
         Color :=
           To_Unbounded_String
             (if Length > 74 then
                Style_Lookup("green.Horizontal.TProgressbar", "-background")
              elsif Length > 24 then
                Style_Lookup("yellow.Horizontal.TProgressbar", "-background")
              elsif Length > 0 then Style_Lookup("TProgressbar", "-background")
              else Style_Lookup("TProgressbar", "-troughcolor"));
      else
         Color :=
           To_Unbounded_String
             (if Length < 25 then
                Style_Lookup("green.Horizontal.TProgressbar", "-background")
              elsif Length > 24 and Length < 75 then
                Style_Lookup("yellow.Horizontal.TProgressbar", "-background")
              else Style_Lookup("TProgressbar", "-background"));
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
              " -fill " & To_String(Color) & " -tags [list progressbar" &
              Trim(Positive'Image(Table.Row), Left) & "bar" &
              Trim(Positive'Image(Column), Left) & "]"));
      Add_Bindings
        (Table.Canvas, To_String(ItemId),
         Trim(Positive'Image(Table.Row), Left), Command, Background_Color);
      if Tooltip'Length > 0 then
         Add(Table.Canvas, Tooltip, "-item " & To_String(ItemId));
      end if;
      if New_Row then
         Table.Row := Table.Row + 1;
      end if;
   end Add_Progress_Bar;

   procedure Add_Pagination
     (Table: in out Table_Widget; Previous_Command, Next_Command: String) is
      ButtonsFrame: constant Ttk_Frame :=
        Create(Table.Canvas & ".buttonframe");
      Button: Ttk_Button;
   begin
      if Previous_Command'Length > 0 then
         Button :=
           Create
             (ButtonsFrame & ".previous",
              "-text Previous -command {" & Previous_Command & "}");
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky w");
         Add(Button, "Previous page");
      end if;
      if Next_Command'Length > 0 then
         Button :=
           Create
             (ButtonsFrame & ".next",
              "-text Next -command {" & Next_Command & "}");
         Tcl.Tk.Ada.Grid.Grid(Button, "-sticky e -row 0 -column 1");
         Add(Button, "Next page");
      end if;
      Tcl_Eval(Get_Interp(Table.Canvas), "update");
      Canvas_Create
        (Table.Canvas, "window",
         "0" & Positive'Image(Table.Row * Table.Row_Height) &
         " -anchor nw -window " & ButtonsFrame);
   end Add_Pagination;

   procedure Add_Check_Button
     (Table: in out Table_Widget; Tooltip, Command: String; Checked: Boolean;
      Column: Positive; New_Row: Boolean := False) is
      X: Natural := 5;
      ItemId: Unbounded_String;
      Tokens: Slice_Set;
      Background_Color: constant String :=
        Add_Background(Table, New_Row, Command);
      ImageName: constant String :=
        "${ttk::theme::" & Theme_Use & "::I(checkbox-" &
        (if Checked then "checked" else "unchecked") & ")}";
   begin
      Count_X_Loop :
      for I in 1 .. Column - 1 loop
         X := X + Table.Columns_Width(I);
      end loop Count_X_Loop;
      ItemId :=
        To_Unbounded_String
          (Canvas_Create
             (Table.Canvas, "image",
              Trim(Natural'Image(X), Left) &
              Positive'Image((Table.Row * Table.Row_Height) + 2) &
              " -anchor nw -image " & ImageName & " -tags [list row" &
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
      if Command'Length > 0 then
         Add_Bindings
           (Table.Canvas, To_String(ItemId),
            Trim(Positive'Image(Table.Row), Left), Command, Background_Color);
      end if;
      if New_Row then
         Table.Row := Table.Row + 1;
      end if;
   end Add_Check_Button;

   function Get_Column_Number
     (Table: Table_Widget; X_Position: Natural) return Positive is
      Position: Positive := X_Position;
   begin
      for I in Table.Columns_Width'Range loop
         if Position < Table.Columns_Width(I) + 20 then
            return I;
         end if;
         Position := Position - Table.Columns_Width(I) - 20;
      end loop;
      return 1;
   end Get_Column_Number;

   procedure Update_Headers_Command(Table: Table_Widget; Command: String) is
   begin
      if Command'Length > 0 then
         for I in Table.Columns_Width'Range loop
            Bind
              (Table.Canvas, "header" & Trim(Positive'Image(I), Left),
               "<Enter>", "{" & Table.Canvas & " configure -cursor hand1}");
            Bind
              (Table.Canvas, "header" & Trim(Positive'Image(I), Left),
               "<Leave>", "{" & Table.Canvas & " configure -cursor left_ptr}");
            Bind
              (Table.Canvas, "header" & Trim(Positive'Image(I), Left),
               "<Button-1>", "{" & Command & " %x}");
         end loop;
         Bind
           (Table.Canvas, "headerback", "<Enter>",
            "{" & Table.Canvas & " configure -cursor hand1}");
         Bind
           (Table.Canvas, "headerback", "<Leave>",
            "{" & Table.Canvas & " configure -cursor left_ptr}");
         Bind
           (Table.Canvas, "headerback", "<Button-1>", "{" & Command & " %x}");
      else
         for I in Table.Columns_Width'Range loop
            Bind
              (Table.Canvas, "header" & Trim(Positive'Image(I), Left),
               "<Enter>", "{}");
            Bind
              (Table.Canvas, "header" & Trim(Positive'Image(I), Left),
               "<Leave>", "{}");
            Bind
              (Table.Canvas, "header" & Trim(Positive'Image(I), Left),
               "<Button-1>", "{}");
         end loop;
         Bind(Table.Canvas, "headerback", "<Enter>", "{}");
         Bind(Table.Canvas, "headerback", "<Leave>", "{}");
         Bind(Table.Canvas, "headerback", "<Button-1>", "{}");
      end if;
   end Update_Headers_Command;

   -- ****o* Table/Table.Update_Current_Row_Command
   -- FUNCTION
   -- Update Tcl variable currentrow and show the currently selected row in
   -- the table
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateCurrentRow canvas action
   -- Canvas is the name of Table Tk_Canvas in which the current row will
   -- be updated, action is the name of action which will be taken. Can be
   -- raise or lower
   -- SOURCE
   function Update_Current_Row_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Current_Row_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      CurrentRow: Natural := Natural'Value(Tcl_GetVar(Interp, "currentrow"));
      MaxRows: constant Natural :=
        Natural'Value(Tcl_GetVar(Interp, "maxrows")) - 1;
      Color: constant String :=
        (if CurrentRow rem 2 > 0 then Style_Lookup("Table", "-rowcolor")
         else Style_Lookup
             (To_String(Game_Settings.Interface_Theme), "-background"));
      Canvas: constant Tk_Canvas := Get_Widget(CArgv.Arg(Argv, 1), Interp);
   begin
      if CArgv.Arg(Argv, 2) = "lower" then
         CurrentRow := CurrentRow - 1;
         if CurrentRow = 0 then
            CurrentRow := 1;
         end if;
      else
         CurrentRow := CurrentRow + 1;
         if CurrentRow > MaxRows then
            CurrentRow := MaxRows;
         end if;
      end if;
      Item_Configure(Canvas, "row$currentrow", "-fill " & Color);
      Item_Configure
        (Canvas, "row" & Trim(Natural'Image(CurrentRow), Left),
         "-fill " &
         Style_Lookup
           (To_String(Game_Settings.Interface_Theme), "-selectbackground"));
      Tcl_SetVar(Interp, "currentrow", Trim(Natural'Image(CurrentRow), Left));
      return TCL_OK;
   end Update_Current_Row_Command;

   -- ****o* Table/Table.Execute_Current_Row_Command
   -- FUNCTION
   -- Execute the Tcl command associated with the current row in the selected
   -- Table_Widget
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ExecuteCurrentRow canvas
   -- Canvas is the name of Table Tk_Canvas in which the Tcl command related
   -- to the current row will be executed
   -- SOURCE
   function Execute_Current_Row_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Execute_Current_Row_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Canvas: constant Tk_Canvas := Get_Widget(CArgv.Arg(Argv, 1), Interp);
   begin
      Tcl_Eval
        (Interp,
         Bind
           (Canvas, "row$currentrow",
            "<Button-" & (if Game_Settings.Right_Button then "3" else "1") &
            ">"));
      return TCL_OK;
   end Execute_Current_Row_Command;

   -- ****o* Table/Table.Hide_Current_Row_Command
   -- FUNCTION
   -- Set the normal background color for the current row in the selected
   -- Table_Widget
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- HideCurrentRow canvas
   -- Canvas is the name of Table Tk_Canvas in which the selected row
   -- background will be recolored
   -- SOURCE
   function Hide_Current_Row_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Hide_Current_Row_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Canvas: constant Tk_Canvas := Get_Widget(CArgv.Arg(Argv, 1), Interp);
      Color: constant String :=
        (if Natural'Value(Tcl_GetVar(Interp, "currentrow")) rem 2 > 0 then
           Style_Lookup("Table", "-rowcolor")
         else Style_Lookup
             (To_String(Game_Settings.Interface_Theme), "-background"));
   begin
      Item_Configure(Canvas, "row$currentrow", "-fill " & Color);
      return TCL_OK;
   end Hide_Current_Row_Command;

   procedure Add_Commands is
   begin
      Add_Command("UpdateCurrentRow", Update_Current_Row_Command'Access);
      Add_Command("ExecuteCurrentRow", Execute_Current_Row_Command'Access);
      Add_Command("HideCurrentRow", Hide_Current_Row_Command'Access);
   end Add_Commands;

end Table;
