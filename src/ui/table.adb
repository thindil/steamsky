-- Copyright (c) 2021-2023 Bartek thindil Jasicki
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
with Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
-- with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.TtkStyle; use Tcl.Tk.Ada.TtkStyle;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
-- with Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with Utils.UI;

package body Table is

   function Create_Table
     (Parent: String; Headers: Headers_Array;
      Scrollbar: Ttk_Scrollbar := Get_Widget(pathName => ".");
      Command, Tooltip_Text: String := "") return Table_Widget is
      use Interfaces.C.Strings;
--      use Tcl.Tklib.Ada.Autoscroll;

--      Canvas_Widget: Tk_Canvas;
--      --## rule off IMPROPER_INITIALIZATION
--      Y_Scroll: Ttk_Scrollbar;
--      X_Scroll: Ttk_Scrollbar;
--      Tokens: Slice_Set;
      New_Table: Table_Widget (Amount => Headers'Length);
--      --## rule on IMPROPER_INITIALIZATION
--      X, Old_X: Natural := 5;
--      Master: constant Tk_Canvas := Get_Widget(pathName => Parent);
--      Header_Id: Unbounded_String := Null_Unbounded_String;

      --## rule off TYPE_INITIAL_VALUES
      type Nim_Headers is array(0 .. 10) of chars_ptr;
      type Nim_Width is array(0 .. 10) of Integer;
      --## rule on TYPE_INITIAL_VALUES
      N_Headers: Nim_Headers;
      Nim_Canvas, Nim_Scrollbar: chars_ptr;
      N_Width: Nim_Width := (others => 0);
      Index, Nim_Height: Natural := 0;
      procedure Create_Ada_Table
        (P: chars_ptr; H: Nim_Headers; S, Com, T_Text: chars_ptr;
         N_Canvas, N_Scrollbar: out chars_ptr; Height: out Integer;
         N_W: out Nim_Width) with
         Import => True,
         Convention => C,
         External_Name => "createAdaTable";
   begin
      Convert_Headers_To_Nim_Loop :
      for Header of Headers loop
         N_Headers(Index) := New_String(Str => To_String(Source => Header));
         Index := Index + 1;
      end loop Convert_Headers_To_Nim_Loop;
      Create_Ada_Table
        (P => New_String(Str => Parent), H => N_Headers,
         S => New_String(Str => Widget_Image(Win => Scrollbar)),
         Com => New_String(Str => Command),
         T_Text => New_String(Str => Tooltip_Text), N_Canvas => Nim_Canvas,
         N_Scrollbar => Nim_Scrollbar, Height => Nim_Height, N_W => N_Width);
      New_Table.Canvas := Get_Widget(pathName => Value(Item => Nim_Canvas));
      Index := 1;
      Convert_Headers_Width_Loop :
      for Width of N_Width loop
         exit Convert_Headers_Width_Loop when Width = 0;
         New_Table.Columns_Width(Index) := Width;
         Index := Index + 1;
      end loop Convert_Headers_Width_Loop;
      New_Table.Row := 1;
      New_Table.Row_Height := Nim_Height;
      New_Table.Scrollbar :=
        Get_Widget(pathName => Value(Item => Nim_Scrollbar));
      Tcl_Eval
        (interp => Get_Context,
         strng =>
           "SetScrollbarBindings " & New_Table.Canvas & " " &
           New_Table.Scrollbar);
--      if Widget_Image(Win => Scrollbar) = "." then
--         Y_Scroll :=
--           Create
--             (pathName => Parent & ".scrolly",
--              options =>
--                "-orient vertical -command [list " & Parent & ".table yview]");
--         X_Scroll :=
--           Create
--             (pathName => Parent & ".scrollx",
--              options =>
--                "-orient horizontal -command [list " & Parent &
--                ".table xview]");
--         Canvas_Widget :=
--           Create
--             (pathName => Parent & ".table",
--              options =>
--                "-yscrollcommand [list " & Parent &
--                ".scrolly set] -xscrollcommand [list " & Parent &
--                ".scrollx set]");
--         Tcl.Tk.Ada.Pack.Pack
--           (Slave => Y_Scroll, Options => "-side right -fill y");
--         Tcl.Tk.Ada.Pack.Pack
--           (Slave => Canvas_Widget,
--            Options => "-side top -fill both -padx {5 0}");
--         Tcl.Tk.Ada.Pack.Pack
--           (Slave => X_Scroll, Options => "-side bottom -fill x");
--         Autoscroll(Scroll => X_Scroll);
--         Autoscroll(Scroll => Y_Scroll);
--         New_Table.Scrollbar := Y_Scroll;
--      else
--         Canvas_Widget := Create(pathName => Parent & ".table");
--         Tcl.Tk.Ada.Grid.Grid
--           (Slave => Canvas_Widget, Options => "-sticky nwes -padx {5 0}");
--         Tcl.Tk.Ada.Grid.Column_Configure
--           (Master => Master, Slave => Canvas_Widget, Options => "-weight 1");
--         Tcl.Tk.Ada.Grid.Row_Configure
--           (Master => Master, Slave => Canvas_Widget, Options => "-weight 1");
--         New_Table.Scrollbar := Scrollbar;
--      end if;
--      Create_Headers_Loop :
--      for I in Headers'Range loop
--         exit Create_Headers_Loop when Length(Source => Headers(I)) = 0 and
--           I = Headers'Last;
--         Header_Id :=
--           To_Unbounded_String
--             (Source =>
--                Canvas_Create
--                  (Parent => Canvas_Widget, Child_Type => "text",
--                   Options =>
--                     Trim(Source => Natural'Image(X), Side => Left) &
--                     " 2 -anchor nw -text {" &
--                     To_String(Source => Headers(I)) &
--                     "} -font InterfaceFont -justify center -fill " &
--                     Style_Lookup
--                       (Name => "Table", Option => "-headerforecolor") &
--                     " -tags [list header" &
--                     Trim(Source => Positive'Image(I), Side => Left) & "]"));
--         if Command'Length > 0 then
--            Bind
--              (CanvasWidget => Canvas_Widget,
--               TagOrId => To_String(Source => Header_Id),
--               Sequence => "<Enter>",
--               Command => "{" & Canvas_Widget & " configure -cursor hand1}");
--            Bind
--              (CanvasWidget => Canvas_Widget,
--               TagOrId => To_String(Source => Header_Id),
--               Sequence => "<Leave>",
--               Command =>
--                 "{" & Canvas_Widget & " configure -cursor left_ptr}");
--            Bind
--              (CanvasWidget => Canvas_Widget,
--               TagOrId => To_String(Source => Header_Id),
--               Sequence => "<Button-1>", Command => "{" & Command & " %x}");
--         end if;
--         if Tooltip_Text'Length > 0 then
--            Add
--              (Widget => Canvas_Widget, Message => Tooltip_Text,
--               Options => "-item " & To_String(Source => Header_Id));
--         end if;
--         Create
--           (S => Tokens,
--            From =>
--              BBox
--                (CanvasWidget => Canvas_Widget,
--                 TagOrId =>
--                   "header" & Trim(Source => Positive'Image(I), Side => Left)),
--            Separators => " ");
--         Old_X := X - 5;
--         X := Positive'Value(Slice(S => Tokens, Index => 3)) + 5;
--         New_Table.Columns_Width(I) :=
--           X - Positive'Value(Slice(S => Tokens, Index => 1));
--         if I = 1 then
--            New_Table.Row_Height :=
--              Positive'Value(Slice(S => Tokens, Index => 4)) + 5;
--         end if;
--         Header_Id :=
--           To_Unbounded_String
--             (Source =>
--                Canvas_Create
--                  (Parent => Canvas_Widget, Child_Type => "rectangle",
--                   Options =>
--                     Trim(Source => Natural'Image(Old_X), Side => Left) &
--                     " 0" & Positive'Image(X - 2) &
--                     Positive'Image(New_Table.Row_Height - 3) & " -fill " &
--                     Style_Lookup
--                       (Name => "Table", Option => "-headerbackcolor") &
--                     " -outline " &
--                     Style_Lookup
--                       (Name => "Table", Option => "-headerbordercolor") &
--                     " -width 2 -tags [list headerback" &
--                     Trim(Source => Positive'Image(I), Side => Left) & "]"));
--         Lower
--           (CanvasWidget => Canvas_Widget,
--            TagOrId =>
--              "headerback" & Trim(Source => Positive'Image(I), Side => Left));
--         if Command'Length > 0 then
--            Bind
--              (CanvasWidget => Canvas_Widget,
--               TagOrId => To_String(Source => Header_Id),
--               Sequence => "<Enter>",
--               Command => "{" & Canvas_Widget & " configure -cursor hand1}");
--            Bind
--              (CanvasWidget => Canvas_Widget,
--               TagOrId => To_String(Source => Header_Id),
--               Sequence => "<Leave>",
--               Command =>
--                 "{" & Canvas_Widget & " configure -cursor left_ptr}");
--            Bind
--              (CanvasWidget => Canvas_Widget,
--               TagOrId => To_String(Source => Header_Id),
--               Sequence => "<Button-1>", Command => "{" & Command & " %x}");
--         end if;
--         if Tooltip_Text'Length > 0 then
--            Add
--              (Widget => Canvas_Widget, Message => Tooltip_Text,
--               Options => "-item " & To_String(Source => Header_Id));
--         end if;
--      end loop Create_Headers_Loop;
--      New_Table.Canvas := Canvas_Widget;
--      Bind
--        (Widgt => New_Table.Canvas, Sequence => "<Up>",
--         Script => "{UpdateCurrentRow " & New_Table.Canvas & " lower}");
--      Bind
--        (Widgt => New_Table.Canvas, Sequence => "<Down>",
--         Script => "{UpdateCurrentRow " & New_Table.Canvas & " raise}");
--      Bind
--        (Widgt => New_Table.Canvas, Sequence => "<Key-space>",
--         Script => "{ExecuteCurrentRow " & New_Table.Canvas & "}");
--      Bind
--        (Widgt => New_Table.Canvas, Sequence => "<FocusOut>",
--         Script => "{HideCurrentRow " & New_Table.Canvas & "}");
--      Bind
--        (Widgt => New_Table.Canvas, Sequence => "<Leave>",
--         Script => "{HideCurrentRow " & New_Table.Canvas & "}");
      return New_Table;
   end Create_Table;

   --## rule off LOCAL_HIDING
   procedure Clear_Table(Table: in out Table_Widget) is
      --## rule on LOCAL_HIDING
      Buttons_Frame: Ttk_Frame :=
        Get_Widget(pathName => Table.Canvas & ".buttonframe");
      --## rule off IMPROPER_INITIALIZATION
      Button: Ttk_Button;
      --## rule on IMPROPER_INITIALIZATION
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

   --## rule off LOCAL_HIDING
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
      --## rule on LOCAL_HIDING
   begin
      Bind
        (CanvasWidget => Canvas, TagOrId => Item_Id, Sequence => "<Enter>",
         Command =>
           "{" & Canvas & " itemconfigure row$currentrow -fill " & Color &
           ";" & Canvas & " itemconfigure row" & Row & " -fill " &
           Style_Lookup
             (Name => To_String(Source => Get_Interface_Theme),
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
              "<Button-" &
              (if Get_Boolean_Setting(Name => "rightButton") then "3"
               else "1") &
              ">",
            Command => "{" & Command & "}");
      end if;
   end Add_Bindings;

   --## rule off LOCAL_HIDING
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
      --## rule on LOCAL_HIDING
      Item_Id: Unbounded_String;
      Color: constant String :=
        (if Table.Row rem 2 > 0 then
           Style_Lookup(Name => "Table", Option => "-rowcolor")
         else Style_Lookup
             (Name => To_String(Source => Get_Interface_Theme),
              Option => "-background"));
   begin
      if not New_Row then
         return Color;
      end if;
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Item_Id :=
        To_Unbounded_String
          (Source =>
             Canvas_Create
               (Parent => Table.Canvas, Child_Type => "rectangle",
                Options =>
                  " 0" & Positive'Image((Table.Row * Table.Row_Height)) &
                  " 10" &
                  Positive'Image
                    ((Table.Row * Table.Row_Height) + (Table.Row_Height)) &
                  " -fill " & Color & " -width 0 -tags [list row" &
                  Trim(Source => Positive'Image(Table.Row), Side => Left) &
                  "]"));
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      Lower
        (CanvasWidget => Table.Canvas,
         TagOrId => To_String(Source => Item_Id));
      Add_Bindings
        (Canvas => Table.Canvas,
         Item_Id =>
           "row" & Trim(Source => Positive'Image(Table.Row), Side => Left),
         Row => Trim(Source => Positive'Image(Table.Row), Side => Left),
         Command => Command, Color => Color);
      return Color;
   end Add_Background;

   --## rule off LOCAL_HIDING
   procedure Add_Button
     (Table: in out Table_Widget; Text, Tooltip, Command: String;
      Column: Positive; New_Row: Boolean := False; Color: String := "") is
      --## rule on LOCAL_HIDING
      X: Natural := 5;
      Item_Id: Unbounded_String := Null_Unbounded_String;
      --## rule off IMPROPER_INITIALIZATION
      Tokens: Slice_Set;
      --## rule on IMPROPER_INITIALIZATION
      Text_Color: constant String :=
        (if Color'Length > 0 then Color
         else Style_Lookup
             (Name => To_String(Source => Get_Interface_Theme),
              Option => "-foreground"));
      Background_Color: constant String :=
        Add_Background(Table => Table, New_Row => New_Row, Command => Command);
   begin
      --## rule off SIMPLIFIABLE_STATEMENTS
      Count_X_Loop :
      for I in 1 .. Column - 1 loop
         X := X + Table.Columns_Width(I);
      end loop Count_X_Loop;
      --## rule on SIMPLIFIABLE_STATEMENTS
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Item_Id :=
        To_Unbounded_String
          (Source =>
             Canvas_Create
               (Parent => Table.Canvas, Child_Type => "text",
                Options =>
                  Trim(Source => Natural'Image(X), Side => Left) &
                  Positive'Image((Table.Row * Table.Row_Height) + 2) &
                  " -anchor nw -text {" & Text &
                  "} -font InterfaceFont -fill " & Text_Color &
                  " -tags [list row" &
                  Trim(Source => Positive'Image(Table.Row), Side => Left) &
                  "col" &
                  Trim(Source => Positive'Image(Column), Side => Left) & "]"));
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      if Tooltip'Length > 0 then
         Add
           (Widget => Table.Canvas, Message => Tooltip,
            Options => "-item " & To_String(Source => Item_Id));
      end if;
      Add_Bindings
        (Canvas => Table.Canvas, Item_Id => To_String(Source => Item_Id),
         Row => Trim(Source => Positive'Image(Table.Row), Side => Left),
         Command => Command, Color => Background_Color);
      Create
        (S => Tokens,
         From =>
           BBox
             (CanvasWidget => Table.Canvas,
              TagOrId => To_String(Source => Item_Id)),
         Separators => " ");
      X :=
        (Positive'Value(Slice(S => Tokens, Index => 3)) + 10) -
        Positive'Value(Slice(S => Tokens, Index => 1));
      if X > Table.Columns_Width(Column) then
         Table.Columns_Width(Column) := X;
      end if;
      if New_Row then
         Table.Row := Table.Row + 1;
      end if;
   end Add_Button;

   --## rule off LOCAL_HIDING
   procedure Update_Table
     (Table: in out Table_Widget; Grab_Focus: Boolean := True) is
      --## rule off LOCAL_HIDING
      Tag: Unbounded_String;
      New_X: Natural := Table.Columns_Width(1) + 20;
      New_Y: Natural := 2;
   begin
      Tag := To_Unbounded_String(Source => "headerback1");
      Coords
        (CanvasWidget => Table.Canvas, TagOrId => To_String(Source => Tag),
         Coordinates =>
           "0 0" & Positive'Image(Table.Columns_Width(1) + 10) &
           Positive'Image(Table.Row_Height - 3));
      Update_Columns_Loop :
      for Column in 2 .. Table.Amount loop
         Tag :=
           To_Unbounded_String
             (Source =>
                "header" &
                Trim(Source => Natural'Image(Column), Side => Left));
         Coords
           (CanvasWidget => Table.Canvas, TagOrId => To_String(Source => Tag),
            Coordinates =>
              Trim(Source => Positive'Image(New_X), Side => Left) &
              Positive'Image(New_Y));
         Tag :=
           To_Unbounded_String
             (Source =>
                "headerback" &
                Trim(Source => Natural'Image(Column), Side => Left));
         Coords
           (CanvasWidget => Table.Canvas, TagOrId => To_String(Source => Tag),
            Coordinates =>
              Trim(Source => Positive'Image(New_X - 10), Side => Left) & " 0" &
              Positive'Image(New_X + Table.Columns_Width(Column) + 10) &
              Positive'Image(Table.Row_Height - 3));
         Update_Rows_Loop :
         for Row in 1 .. Table.Row loop
            New_Y := New_Y + Table.Row_Height;
            Tag :=
              To_Unbounded_String
                (Source =>
                   "row" & Trim(Source => Positive'Image(Row), Side => Left) &
                   "col" &
                   Trim(Source => Natural'Image(Column), Side => Left));
            MoveTo
              (CanvasWidget => Table.Canvas,
               TagOrId => To_String(Source => Tag),
               XPos => Trim(Source => Positive'Image(New_X), Side => Left),
               YPos => Trim(Source => Positive'Image(New_Y), Side => Left));
            Tag :=
              To_Unbounded_String
                (Source =>
                   "progressbar" &
                   Trim(Source => Positive'Image(Row), Side => Left) & "back" &
                   Trim(Source => Natural'Image(Column), Side => Left));
            MoveTo
              (CanvasWidget => Table.Canvas,
               TagOrId => To_String(Source => Tag),
               XPos => Trim(Source => Positive'Image(New_X), Side => Left),
               YPos =>
                 Trim(Source => Positive'Image(New_Y + 5), Side => Left));
            Tag :=
              To_Unbounded_String
                (Source =>
                   "progressbar" &
                   Trim(Source => Positive'Image(Row), Side => Left) & "bar" &
                   Trim(Source => Natural'Image(Column), Side => Left));
            MoveTo
              (CanvasWidget => Table.Canvas,
               TagOrId => To_String(Source => Tag),
               XPos => Trim(Source => Positive'Image(New_X + 2), Side => Left),
               YPos =>
                 Trim(Source => Positive'Image(New_Y + 7), Side => Left));
         end loop Update_Rows_Loop;
         New_X := New_X + Table.Columns_Width(Column) + 20;
         New_Y := 2;
      end loop Update_Columns_Loop;
      Resize_Table_Block :
      declare
         Tokens: Slice_Set;
      begin
         Create
           (S => Tokens,
            From => BBox(CanvasWidget => Table.Canvas, TagOrId => "all"),
            Separators => " ");
            -- if no scrollbars, resize the table
         if Winfo_Get(Widgt => Table.Canvas, Info => "parent") /=
           Winfo_Get(Widgt => Table.Scrollbar, Info => "parent") then
            configure
              (Widgt => Table.Canvas,
               options =>
                 "-height [expr " & Slice(S => Tokens, Index => 4) & " - " &
                 Slice(S => Tokens, Index => 2) & "] -width [expr " &
                 Slice(S => Tokens, Index => 3) & " - " &
                 Slice(S => Tokens, Index => 1) & " + 5]");
         end if;
         New_Y := Table.Row_Height;
         Resize_Background_Loop :
         for Row in 1 .. Table.Row loop
            New_Y := New_Y + Table.Row_Height;
            Tag :=
              To_Unbounded_String
                (Source =>
                   "row" & Trim(Source => Positive'Image(Row), Side => Left));
            Coords
              (CanvasWidget => Table.Canvas,
               TagOrId => To_String(Source => Tag),
               Coordinates =>
                 "0" & Positive'Image(New_Y - Table.Row_Height) &
                 Positive'Image
                   (Positive'Value(Slice(S => Tokens, Index => 3)) - 1) &
                 Positive'Image(New_Y));
         end loop Resize_Background_Loop;
      end Resize_Table_Block;
      Tcl_SetVar
        (interp => Get_Context, varName => "currentrow", newValue => "1");
      Bind
        (Widgt => Table.Canvas, Sequence => "<FocusIn>",
         Script =>
           "{set maxrows" & Natural'Image(Table.Row) &
           ";if {$currentrow > $maxrows} {set currentrow 1};" & Table.Canvas &
           " itemconfigure row$currentrow -fill [ttk::style lookup " &
           To_String(Source => Get_Interface_Theme) & " -selectbackground]}");
      if Grab_Focus then
         Widgets.Focus(Widgt => Table.Canvas);
      end if;
   end Update_Table;

   procedure Add_Progress_Bar
     (Table: in out Table_Widget; Value: Natural; Max_Value: Positive;
      Tooltip, Command: String; Column: Positive;
      New_Row, Invert_Colors: Boolean := False) is
      X: Natural := 0;
      Item_Id, Color: Unbounded_String := Null_Unbounded_String;
      --## rule off IMPROPER_INITIALIZATION
      Tokens: Slice_Set;
      --## rule on IMPROPER_INITIALIZATION
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Length: constant Natural :=
        Natural
          (100.0 +
           ((Float(Value) - Float(Max_Value)) / Float(Max_Value) * 100.0));
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      Background_Color: constant String :=
        Add_Background(Table => Table, New_Row => New_Row, Command => Command);
   begin
      --## rule off SIMPLIFIABLE_STATEMENTS
      Count_X_Loop :
      for I in 1 .. Column - 1 loop
         X := X + Table.Columns_Width(I);
      end loop Count_X_Loop;
      --## rule on SIMPLIFIABLE_STATEMENTS
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Item_Id :=
        To_Unbounded_String
          (Source =>
             Canvas_Create
               (Parent => Table.Canvas, Child_Type => "rectangle",
                Options =>
                  Trim(Source => Natural'Image(X), Side => Left) &
                  Positive'Image((Table.Row * Table.Row_Height) + 5) &
                  Positive'Image(X + 102) &
                  Positive'Image
                    ((Table.Row * Table.Row_Height) +
                     (Table.Row_Height - 10)) &
                  " -fill " &
                  Style_Lookup
                    (Name => "TProgressbar", Option => "-troughcolor") &
                  " -outline " &
                  Style_Lookup
                    (Name => "TProgressbar", Option => "-bordercolor") &
                  " -tags [list progressbar" &
                  Trim(Source => Positive'Image(Table.Row), Side => Left) &
                  "back" &
                  Trim(Source => Positive'Image(Column), Side => Left) & "]"));
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      Add_Bindings
        (Canvas => Table.Canvas, Item_Id => To_String(Source => Item_Id),
         Row => Trim(Source => Positive'Image(Table.Row), Side => Left),
         Command => Command, Color => Background_Color);
      if Tooltip'Length > 0 then
         Add
           (Widget => Table.Canvas, Message => Tooltip,
            Options => "-item " & To_String(Source => Item_Id));
      end if;
      Create
        (S => Tokens,
         From =>
           BBox
             (CanvasWidget => Table.Canvas,
              TagOrId => To_String(Source => Item_Id)),
         Separators => " ");
      X :=
        (Positive'Value(Slice(S => Tokens, Index => 3)) + 10) -
        Positive'Value(Slice(S => Tokens, Index => 1));
      if X > Table.Columns_Width(Column) then
         Table.Columns_Width(Column) := X;
      end if;
      if Invert_Colors then
         Color :=
           To_Unbounded_String
             (Source =>
                (if Length < 25 then
                   Style_Lookup
                     (Name => "green.Horizontal.TProgressbar",
                      Option => "-background")
                 elsif Length > 24 and Length < 75 then
                   Style_Lookup
                     (Name => "yellow.Horizontal.TProgressbar",
                      Option => "-background")
                 else Style_Lookup
                     (Name => "TProgressbar", Option => "-background")));
      else
         Color :=
           To_Unbounded_String
             (Source =>
                (if Length > 74 then
                   Style_Lookup
                     (Name => "green.Horizontal.TProgressbar",
                      Option => "-background")
                 elsif Length > 24 then
                   Style_Lookup
                     (Name => "yellow.Horizontal.TProgressbar",
                      Option => "-background")
                 elsif Length > 0 then
                   Style_Lookup
                     (Name => "TProgressbar", Option => "-background")
                 else Style_Lookup
                     (Name => "TProgressbar", Option => "-troughcolor")));
      end if;
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Item_Id :=
        To_Unbounded_String
          (Source =>
             Canvas_Create
               (Parent => Table.Canvas, Child_Type => "rectangle",
                Options =>
                  Trim(Source => Natural'Image(X + 2), Side => Left) &
                  Positive'Image((Table.Row * Table.Row_Height) + 7) &
                  Positive'Image(X + Length) &
                  Positive'Image
                    ((Table.Row * Table.Row_Height) +
                     (Table.Row_Height - 12)) &
                  " -fill " & To_String(Source => Color) &
                  " -tags [list progressbar" &
                  Trim(Source => Positive'Image(Table.Row), Side => Left) &
                  "bar" &
                  Trim(Source => Positive'Image(Column), Side => Left) & "]"));
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      Add_Bindings
        (Canvas => Table.Canvas, Item_Id => To_String(Source => Item_Id),
         Row => Trim(Source => Positive'Image(Table.Row), Side => Left),
         Command => Command, Color => Background_Color);
      if Tooltip'Length > 0 then
         Add
           (Widget => Table.Canvas, Message => Tooltip,
            Options => "-item " & To_String(Source => Item_Id));
      end if;
      if New_Row then
         Table.Row := Table.Row + 1;
      end if;
   end Add_Progress_Bar;

   procedure Add_Pagination
     (Table: in out Table_Widget;
      Previous_Command, Next_Command: String := "") is
      Buttons_Frame: constant Ttk_Frame :=
        Create(pathName => Table.Canvas & ".buttonframe");
      --## rule off IMPROPER_INITIALIZATION
      Button: Ttk_Button;
      --## rule on IMPROPER_INITIALIZATION
   begin
      if Previous_Command'Length > 0 then
         Button :=
           Create
             (pathName => Buttons_Frame & ".previous",
              options => "-text Previous -command {" & Previous_Command & "}");
         Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-sticky w");
         Add(Widget => Button, Message => "Previous page");
      end if;
      if Next_Command'Length > 0 then
         Button :=
           Create
             (pathName => Buttons_Frame & ".next",
              options => "-text Next -command {" & Next_Command & "}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button, Options => "-sticky e -row 0 -column 1");
         Add(Widget => Button, Message => "Next page");
      end if;
      Tcl_Eval(interp => Get_Interp(Widgt => Table.Canvas), strng => "update");
      Canvas_Create
        (Parent => Table.Canvas, Child_Type => "window",
         Options =>
           "0" & Positive'Image(Table.Row * Table.Row_Height) &
           " -anchor nw -window " & Buttons_Frame);
   end Add_Pagination;

   procedure Add_Check_Button
     (Table: in out Table_Widget; Tooltip, Command: String; Checked: Boolean;
      Column: Positive; New_Row, Empty_Unchecked: Boolean := False) is
      X: Natural := 5;
      Item_Id: Unbounded_String := Null_Unbounded_String;
      --## rule off IMPROPER_INITIALIZATION
      Tokens: Slice_Set;
      --## rule on IMPROPER_INITIALIZATION
      Background_Color: constant String :=
        Add_Background(Table => Table, New_Row => New_Row, Command => Command);
      Image_Name: constant String :=
        "${ttk::theme::" & Theme_Use & "::Images(checkbox-" &
        (if Checked then "checked"
         else (if Empty_Unchecked then "unchecked-empty" else "unchecked")) &
        ")}";
   begin
      --## rule off SIMPLIFIABLE_STATEMENTS
      Count_X_Loop :
      for I in 1 .. Column - 1 loop
         X := X + Table.Columns_Width(I);
      end loop Count_X_Loop;
      --## rule on SIMPLIFIABLE_STATEMENTS
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Item_Id :=
        To_Unbounded_String
          (Source =>
             Canvas_Create
               (Parent => Table.Canvas, Child_Type => "image",
                Options =>
                  Trim(Source => Natural'Image(X), Side => Left) &
                  Positive'Image((Table.Row * Table.Row_Height) + 2) &
                  " -anchor nw -image " & Image_Name & " -tags [list row" &
                  Trim(Source => Positive'Image(Table.Row), Side => Left) &
                  "col" &
                  Trim(Source => Positive'Image(Column), Side => Left) & "]"));
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      if Tooltip'Length > 0 then
         Add
           (Widget => Table.Canvas, Message => Tooltip,
            Options => "-item " & To_String(Source => Item_Id));
      end if;
      Create
        (S => Tokens,
         From =>
           BBox
             (CanvasWidget => Table.Canvas,
              TagOrId => To_String(Source => Item_Id)),
         Separators => " ");
      X :=
        (Positive'Value(Slice(S => Tokens, Index => 3)) + 10) -
        Positive'Value(Slice(S => Tokens, Index => 1));
      if X > Table.Columns_Width(Column) then
         Table.Columns_Width(Column) := X;
      end if;
      if Command'Length > 0 then
         Add_Bindings
           (Canvas => Table.Canvas, Item_Id => To_String(Source => Item_Id),
            Row => Trim(Source => Positive'Image(Table.Row), Side => Left),
            Command => Command, Color => Background_Color);
      end if;
      if New_Row then
         Table.Row := Table.Row + 1;
      end if;
   end Add_Check_Button;

   function Get_Column_Number
     (Table: Table_Widget; X_Position: Natural) return Positive is
      Position: Positive := X_Position;
   begin
      Find_Number_Loop :
      for I in Table.Columns_Width'Range loop
         if Position < Table.Columns_Width(I) + 20 then
            return I;
         end if;
         Position := Position - Table.Columns_Width(I) - 20;
      end loop Find_Number_Loop;
      return 1;
   end Get_Column_Number;

   procedure Update_Headers_Command(Table: Table_Widget; Command: String) is
   begin
      if Command'Length > 0 then
         Update_Headers_Loop :
         for I in Table.Columns_Width'Range loop
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "header" & Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Enter>",
               Command => "{" & Table.Canvas & " configure -cursor hand1}");
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "header" & Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Leave>",
               Command => "{" & Table.Canvas & " configure -cursor left_ptr}");
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "header" & Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Button-1>", Command => "{" & Command & " %x}");
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "headerback" &
                 Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Enter>",
               Command => "{" & Table.Canvas & " configure -cursor hand1}");
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "headerback" &
                 Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Leave>",
               Command => "{" & Table.Canvas & " configure -cursor left_ptr}");
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "headerback" &
                 Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Button-1>", Command => "{" & Command & " %x}");
         end loop Update_Headers_Loop;
      else
         Reset_Headers_Command_Loop :
         for I in Table.Columns_Width'Range loop
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "header" & Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Enter>", Command => "{}");
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "header" & Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Leave>", Command => "{}");
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "header" & Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Button-1>", Command => "{}");
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "headerback" &
                 Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Enter>", Command => "{}");
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "headerback" &
                 Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Leave>", Command => "{}");
            Bind
              (CanvasWidget => Table.Canvas,
               TagOrId =>
                 "headerback" &
                 Trim(Source => Positive'Image(I), Side => Left),
               Sequence => "<Button-1>", Command => "{}");
         end loop Reset_Headers_Command_Loop;
      end if;
   end Update_Headers_Command;

   -- ****o* Table/Table.Update_Current_Row_Command
   -- FUNCTION
   -- Update Tcl variable currentrow and show the currently selected row in
   -- the table
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateCurrentRow canvas action
   -- Canvas is the name of Table Tk_Canvas in which the current row will
   -- be updated, action is the name of action which will be taken. Can be
   -- raise or lower
   -- SOURCE
   function Update_Current_Row_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Current_Row_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Current_Row: Natural :=
        Natural'Value(Tcl_GetVar(interp => Interp, varName => "currentrow"));
      Max_Rows: constant Natural :=
        Natural'Value(Tcl_GetVar(interp => Interp, varName => "maxrows")) - 1;
      Color: constant String :=
        (if Current_Row rem 2 > 0 then
           Style_Lookup(Name => "Table", Option => "-rowcolor")
         else Style_Lookup
             (Name => To_String(Source => Get_Interface_Theme),
              Option => "-background"));
      Canvas: constant Tk_Canvas :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
   begin
      if CArgv.Arg(Argv => Argv, N => 2) = "lower" then
         Current_Row := Current_Row - 1;
         if Current_Row = 0 then
            Current_Row := 1;
         end if;
      else
         Current_Row := Current_Row + 1;
         if Current_Row > Max_Rows then
            Current_Row := Max_Rows;
         end if;
      end if;
      Item_Configure
        (CanvasWidget => Canvas, TagOrId => "row$currentrow",
         Options => "-fill " & Color);
      Item_Configure
        (CanvasWidget => Canvas,
         TagOrId =>
           "row" & Trim(Source => Natural'Image(Current_Row), Side => Left),
         Options =>
           "-fill " &
           Style_Lookup
             (Name => To_String(Source => Get_Interface_Theme),
              Option => "-selectbackground"));
      Tcl_SetVar
        (interp => Interp, varName => "currentrow",
         newValue => Trim(Source => Natural'Image(Current_Row), Side => Left));
      return TCL_OK;
   end Update_Current_Row_Command;

   -- ****o* Table/Table.Execute_Current_Row_Command
   -- FUNCTION
   -- Execute the Tcl command associated with the current row in the selected
   -- Table_Widget
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ExecuteCurrentRow canvas
   -- Canvas is the name of Table Tk_Canvas in which the Tcl command related
   -- to the current row will be executed
   -- SOURCE
   function Execute_Current_Row_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Execute_Current_Row_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Canvas: constant Tk_Canvas :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
   begin
      Tcl_Eval
        (interp => Interp,
         strng =>
           Bind
             (CanvasWidget => Canvas, TagOrId => "row$currentrow",
              Sequence =>
                "<Button-" &
                (if Get_Boolean_Setting(Name => "rightButton") then "3"
                 else "1") &
                ">"));
      return TCL_OK;
   end Execute_Current_Row_Command;

   -- ****o* Table/Table.Hide_Current_Row_Command
   -- FUNCTION
   -- Set the normal background color for the current row in the selected
   -- Table_Widget
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- HideCurrentRow canvas
   -- Canvas is the name of Table Tk_Canvas in which the selected row
   -- background will be recolored
   -- SOURCE
   function Hide_Current_Row_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Hide_Current_Row_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Canvas: constant Tk_Canvas :=
        Get_Widget
          (pathName => CArgv.Arg(Argv => Argv, N => 1), Interp => Interp);
      Color: constant String :=
        (if
           Natural'Value
             (Tcl_GetVar(interp => Interp, varName => "currentrow")) rem
           2 >
           0
         then Style_Lookup(Name => "Table", Option => "-rowcolor")
         else Style_Lookup
             (Name => To_String(Source => Get_Interface_Theme),
              Option => "-background"));
   begin
      Item_Configure
        (CanvasWidget => Canvas, TagOrId => "row$currentrow",
         Options => "-fill " & Color);
      return TCL_OK;
   end Hide_Current_Row_Command;

   function Is_Checked
     (Table: Table_Widget; Row, Column: Natural) return Boolean is
   begin
      if Item_Cget
          (CanvasWidget => Table.Canvas,
           TagOrId =>
             "row" & Trim(Source => Positive'Image(Row), Side => Left) &
             "col" & Trim(Source => Positive'Image(Column), Side => Left),
           Option => "-image") =
        "checkbox-checked" then
         return True;
      end if;
      return False;
   end Is_Checked;

   procedure Toggle_Checked_Button
     (Table: Table_Widget; Row, Column: Natural) is
   begin
      if Is_Checked(Table => Table, Row => Row, Column => Column) then
         Item_Configure
           (CanvasWidget => Table.Canvas,
            TagOrId =>
              "row" & Trim(Source => Positive'Image(Row), Side => Left) &
              "col" & Trim(Source => Positive'Image(Column), Side => Left),
            Options => "-image checkbox-unchecked-empty");
      else
         Item_Configure
           (CanvasWidget => Table.Canvas,
            TagOrId =>
              "row" & Trim(Source => Positive'Image(Row), Side => Left) &
              "col" & Trim(Source => Positive'Image(Column), Side => Left),
            Options => "-image checkbox-checked");
      end if;
   end Toggle_Checked_Button;

   procedure Add_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "UpdateCurrentRow",
         Ada_Command => Update_Current_Row_Command'Access);
      Add_Command
        (Name => "ExecuteCurrentRow",
         Ada_Command => Execute_Current_Row_Command'Access);
      Add_Command
        (Name => "HideCurrentRow",
         Ada_Command => Hide_Current_Row_Command'Access);
   end Add_Commands;

end Table;
