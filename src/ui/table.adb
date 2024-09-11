-- Copyright (c) 2021-2024 Bartek thindil Jasicki
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

with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Utils.UI;

package body Table is

   function Create_Table
     (Parent: String; Headers: Headers_Array;
      Scrollbar: Ttk_Scrollbar := Get_Widget(pathName => ".");
      Command, Tooltip_Text: String := "") return Table_Widget is
      use Tcl;
      use Tcl.Ada;
      use Tcl.Tk.Ada;

      --## rule off IMPROPER_INITIALIZATION
      New_Table: Table_Widget (Amount => Headers'Length);
      --## rule on IMPROPER_INITIALIZATION
      --## rule off TYPE_INITIAL_VALUES
      type Nim_Headers is array(0 .. 10) of chars_ptr;
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
      return New_Table;
   end Create_Table;

   --## rule off LOCAL_HIDING
   procedure Clear_Table(Table: in out Table_Widget) is
      --## rule on LOCAL_HIDING
      procedure Clear_Ada_Table(Columns, Rows: Positive; Canv: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "clearAdaTable";
   begin
      Clear_Ada_Table
        (Columns => Table.Amount, Rows => Table.Row,
         Canv => New_String(Str => Widget_Image(Win => Table.Canvas)));
      Table.Row := 1;
   end Clear_Table;

   --## rule off LOCAL_HIDING
   function Get_Column_Number
     (Table: Table_Widget; X_Position: Natural) return Positive is
      --## rule on LOCAL_HIDING
      N_Width: Nim_Width := (others => 0);
      Index: Natural := 0;
      function Get_Ada_Column_Number
        (Width: Nim_Width; X_Pos: Integer) return Positive with
         Import => True,
         Convention => C,
         External_Name => "getAdaColumnNumber";
   begin
      Convert_Width_Loop :
      for Width of Table.Columns_Width loop
         N_Width(Index) := Width;
         Index := Index + 1;
      end loop Convert_Width_Loop;
      return Get_Ada_Column_Number(Width => N_Width, X_Pos => X_Position);
   end Get_Column_Number;

   function Update_Current_Row_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "updateCurrentRowCommand";

   function Execute_Current_Row_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "executeCurrentRowCommand";

   function Hide_Current_Row_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "hideCurrentRowCommand";

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
