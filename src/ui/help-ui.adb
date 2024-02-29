-- Copyright (c) 2020-2024 Bartek thindil Jasicki
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

with Ada.Strings;
with Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm;
with Config; use Config;
with Dialogs;
with Game;
with Themes;
with Utils.UI;

package body Help.UI is

   --## rule off REDUCEABLE_SCOPE
   -- ****o* HUI/HUI.Close_Help_Command
   -- FUNCTION
   -- Destroy help window and save sash position to the game configuration
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseHelp
   -- SOURCE
   function Close_Help_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****
      --## rule on REDUCEABLE_SCOPE

   function Close_Help_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Help_Window: Tk_Toplevel :=
        Get_Widget(pathName => ".help", Interp => Interp);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(pathName => Help_Window & ".paned", Interp => Interp);
   begin
      Set_Integer_Setting
        (Name => "topicsPosition",
         Value => Natural'Value(SashPos(Paned => Paned, Index => "0")));
      Destroy(Widgt => Help_Window);
      return TCL_OK;
   end Close_Help_Command;

   -- ****o* HUI/HUI.Show_Help_Command
   -- FUNCTION
   -- Show help window to the player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowHelp topicindex
   -- Topicindex is the index of the help topic which content will be show
   -- SOURCE
   function Show_Help_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Help_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use GNAT.Directory_Operations;
      use Tcl.Ada;
      use Tcl.Tk.Ada.Widgets.Text;
      use Tcl.Tk.Ada.Widgets.TtkTreeView;
      use Tcl.Tk.Ada.Winfo;
      use Tcl.Tk.Ada.Wm;
      use Dialogs;
      use Game;
      use Themes;

      Help_Window: constant Tk_Toplevel :=
        Get_Widget(pathName => ".help", Interp => Interp);
      X, Y: Integer;
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(pathName => Help_Window & ".paned", Interp => Interp);
      Topics_View: constant Ttk_Tree_View :=
        Get_Widget(pathName => Paned & ".topics.view", Interp => Interp);
      Topic_Index: constant String :=
        (if Argc = 1 then Tcl_GetVar(interp => Interp, varName => "gamestate")
         else CArgv.Arg(Argv => Argv, N => 1));
      Help_View: constant Tk_Text :=
        Get_Widget(pathName => Paned & ".content.view", Interp => Interp);
      Local_Help: Help_Data; --## rule line off IMPROPER_INITIALIZATION
      Help_Title: Unbounded_String := Null_Unbounded_String;
   begin
      if Winfo_Get(Widgt => Help_Window, Info => "exists") = "1" then
         return
           Close_Help_Command
             (Client_Data => Client_Data, Interp => Interp, Argc => Argc,
              Argv => Argv);
      end if;
      Tcl_EvalFile
        (interp => Interp,
         fileName =>
           To_String(Source => Data_Directory) & "ui" & Dir_Separator &
           "help.tcl");
      Tag_Configure
        (TextWidget => Help_View, TagName => "special",
         Options =>
           "-foreground {" & Get_Icon(Name => "specialHelpColor") &
           "} -font BoldHelpFont");
      Tag_Configure
        (TextWidget => Help_View, TagName => "underline",
         Options =>
           "-foreground {" & Get_Icon(Name => "underlineHelpColor") &
           "} -font UnderlineHelpFont");
      Tag_Configure
        (TextWidget => Help_View, TagName => "bold",
         Options =>
           "-foreground {" & Get_Icon(Name => "boldHelpColor") &
           "} -font BoldHelpFont");
      Tag_Configure
        (TextWidget => Help_View, TagName => "italic",
         Options =>
           "-foreground {" & Get_Icon(Name => "italicHelpColor") &
           "} -font ItalicHelpFont");
      X :=
        (Positive'Value
           (Winfo_Get(Widgt => Help_Window, Info => "vrootwidth")) -
         Get_Integer_Setting(Name => "windowWidth")) /
        2;
      if X < 0 then
         X := 0;
      end if;
      Y :=
        (Positive'Value
           (Winfo_Get(Widgt => Help_Window, Info => "vrootheight")) -
         Get_Integer_Setting(Name => "windowHeight")) /
        2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (Widgt => Help_Window, Action => "geometry",
         Options =>
           Trim
             (Source =>
                Positive'Image(Get_Integer_Setting(Name => "windowWidth")),
              Side => Left) &
           "x" &
           Trim
             (Source =>
                Positive'Image(Get_Integer_Setting(Name => "windowHeight")),
              Side => Left) &
           "+" & Trim(Source => Positive'Image(X), Side => Left) & "+" &
           Trim(Source => Positive'Image(Y), Side => Left));
      Tcl_Eval(interp => Interp, strng => "update");
      SashPos
        (Paned => Paned, Index => "0",
         NewPos =>
           Natural'Image(Get_Integer_Setting(Name => "topicsPosition")));
      Insert_Topics_Loop :
      for I in 0 .. 100 loop
         Local_Help := Get_Help(Title => Help_Title, Help_Index => I);
         exit Insert_Topics_Loop when Length(Source => Local_Help.Index) = 0;
         Insert
           (TreeViewWidget => Topics_View,
            Options =>
              "{} end -id {" & To_String(Source => Local_Help.Index) &
              "} -text {" & To_String(Source => Help_Title) & "}");
      end loop Insert_Topics_Loop;
      Bind
        (Widgt => Topics_View, Sequence => "<<TreeviewSelect>>",
         Script => "ShowTopic");
      if Exists(TreeViewWidget => Topics_View, Item => Topic_Index) = "0" then
         Show_Message
           (Text =>
              "The selected help topic doesn't exist. Showing the first available instead.",
            Parent_Frame => ".help", Title => "Can't find help topic");
         Selection_Set
           (TreeViewWidget => Topics_View,
            Items =>
              To_String
                (Source =>
                   Get_Help(Title => Help_Title, Help_Index => 0).Index));
         return TCL_OK;
      end if;
      Selection_Set(TreeViewWidget => Topics_View, Items => Topic_Index);
      Tcl_Eval(interp => Interp, strng => "update");
      See(TreeViewWidget => Topics_View, Item => Topic_Index);
      return TCL_OK;
   end Show_Help_Command;

   procedure Add_Commands is
      use Utils.UI;
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaHelpCommands";
   begin
      Add_Ada_Commands;
      Add_Command(Name => "ShowHelp", Ada_Command => Show_Help_Command'Access);
--      Add_Command
--        (Name => "CloseHelp", Ada_Command => Close_Help_Command'Access);
   end Add_Commands;

end Help.UI;
