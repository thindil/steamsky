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
with Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;
with GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkEntry;
with Utils.UI;

package body GameOptions is

   -- ****o* GameOptions/GameOptions.Show_Options_Tab_Command
   -- FUNCTION
   -- Show the selected options tab
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowOptionsTab
   -- SOURCE
   function Show_Options_Tab_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showOptionsTabCommand";
      -- ****

   -- ****o* GameOptions/GameOptions.Show_Options_Command
   -- FUNCTION
   -- Show the game options to the player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowOptions
   -- SOURCE
   function Show_Options_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showOptionsCommand";
      -- ****

   -- ****o* GameOptions/GameOptions.Set_Fonts_Command
   -- FUNCTION
   -- Set the selected font
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetFonts fontfield
   -- Fontfield is the name of the spinbox which value changed.
   -- SOURCE
   function Set_Fonts_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setFontsCommand";
      -- ****

   -- ****o* GameOptions/GameOptions.Set_Default_Fonts_Command
   -- FUNCTION
   -- Set the default values for fonts
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetDefaultFonts
   -- SOURCE
   function Set_Default_Fonts_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setDefaultFontsCommand";
      -- ****

   -- ****o* GameOptions/GameOptions.Close_Options_Command
   -- FUNCTION
   -- Save all options and back to the map
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseOptions oldscreen
   -- Oldscreen is name of the screen to which the game should return.
   -- Can be 'map' or 'combat'.
   -- SOURCE
   function Close_Options_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "closeOptionsCommand";
      -- ****

   -- ****o* GameOptions/GameOptions.Reset_Keys_Command
   -- FUNCTION
   -- Reset the selected group of keys to their default values
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ResetKeys group
   -- Group is the group of keys which will be resetted. Possible values are
   -- movement, map, menu
   -- SOURCE
   function Reset_Keys_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Reset_Keys_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Ada.Strings.Unbounded;
      use Interfaces.C.Strings;
      use GNAT.Directory_Operations;
      use Tcl.Tk.Ada.Widgets.TtkEntry;

      --## rule off TYPE_INITIAL_VALUES
      type Accel_Data is record
         Shortcut: Unbounded_String;
         Entry_Name: Unbounded_String;
         Config_Name: Unbounded_String;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      Default_Movement_Accels: constant array(1 .. 14) of Accel_Data :=
        (1 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Home" else "KP_Home")),
            Entry_Name => To_Unbounded_String(Source => ".movement.upleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         2 =>
           (Shortcut =>
              To_Unbounded_String
                (Source => (if Dir_Separator = '\' then "Up" else "KP_Up")),
            Entry_Name => To_Unbounded_String(Source => ".movement.up"),
            Config_Name => To_Unbounded_String(Source => "")),
         3 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Prior" else "KP_Prior")),
            Entry_Name => To_Unbounded_String(Source => ".movement.upright"),
            Config_Name => To_Unbounded_String(Source => "")),
         4 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Left" else "KP_Left")),
            Entry_Name => To_Unbounded_String(Source => ".movement.left"),
            Config_Name => To_Unbounded_String(Source => "")),
         5 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Clear" else "KP_Begin")),
            Entry_Name => To_Unbounded_String(Source => ".movement.wait"),
            Config_Name => To_Unbounded_String(Source => "")),
         6 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Right" else "KP_Right")),
            Entry_Name => To_Unbounded_String(Source => ".movement.right"),
            Config_Name => To_Unbounded_String(Source => "")),
         7 =>
           (Shortcut =>
              To_Unbounded_String
                (Source => (if Dir_Separator = '\' then "End" else "KP_End")),
            Entry_Name => To_Unbounded_String(Source => ".movement.downleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         8 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Down" else "KP_Down")),
            Entry_Name => To_Unbounded_String(Source => ".movement.down"),
            Config_Name => To_Unbounded_String(Source => "")),
         9 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "Next" else "KP_Next")),
            Entry_Name => To_Unbounded_String(Source => ".movement.downright"),
            Config_Name => To_Unbounded_String(Source => "")),
         10 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   (if Dir_Separator = '\' then "slash" else "KP_Divide")),
            Entry_Name => To_Unbounded_String(Source => ".movement.moveto"),
            Config_Name => To_Unbounded_String(Source => "")),
         11 =>
           (Shortcut => To_Unbounded_String(Source => "Control-a"),
            Entry_Name => To_Unbounded_String(Source => ".movement.fullstop"),
            Config_Name => To_Unbounded_String(Source => "")),
         12 =>
           (Shortcut => To_Unbounded_String(Source => "Control-b"),
            Entry_Name =>
              To_Unbounded_String(Source => ".movement.quarterspeed"),
            Config_Name => To_Unbounded_String(Source => "")),
         13 =>
           (Shortcut => To_Unbounded_String(Source => "Control-c"),
            Entry_Name => To_Unbounded_String(Source => ".movement.halfspeed"),
            Config_Name => To_Unbounded_String(Source => "")),
         14 =>
           (Shortcut => To_Unbounded_String(Source => "Control-d"),
            Entry_Name => To_Unbounded_String(Source => ".movement.fullspeed"),
            Config_Name => To_Unbounded_String(Source => "")));
      Default_Menu_Accels: constant array(1 .. 12) of Accel_Data :=
        (1 =>
           (Shortcut => To_Unbounded_String(Source => "s"),
            Entry_Name => To_Unbounded_String(Source => ".menu.shipinfo"),
            Config_Name => To_Unbounded_String(Source => "")),
         2 =>
           (Shortcut => To_Unbounded_String(Source => "o"),
            Entry_Name => To_Unbounded_String(Source => ".menu.orders"),
            Config_Name => To_Unbounded_String(Source => "")),
         3 =>
           (Shortcut => To_Unbounded_String(Source => "r"),
            Entry_Name => To_Unbounded_String(Source => ".menu.crafts"),
            Config_Name => To_Unbounded_String(Source => "")),
         4 =>
           (Shortcut => To_Unbounded_String(Source => "m"),
            Entry_Name => To_Unbounded_String(Source => ".menu.messages"),
            Config_Name => To_Unbounded_String(Source => "")),
         5 =>
           (Shortcut => To_Unbounded_String(Source => "k"),
            Entry_Name => To_Unbounded_String(Source => ".menu.knowledge"),
            Config_Name => To_Unbounded_String(Source => "")),
         6 =>
           (Shortcut => To_Unbounded_String(Source => "w"),
            Entry_Name => To_Unbounded_String(Source => ".menu.waitorders"),
            Config_Name => To_Unbounded_String(Source => "")),
         7 =>
           (Shortcut => To_Unbounded_String(Source => "g"),
            Entry_Name => To_Unbounded_String(Source => ".menu.gamestats"),
            Config_Name => To_Unbounded_String(Source => "")),
         8 =>
           (Shortcut => To_Unbounded_String(Source => "F1"),
            Entry_Name => To_Unbounded_String(Source => ".menu.help"),
            Config_Name => To_Unbounded_String(Source => "")),
         9 =>
           (Shortcut => To_Unbounded_String(Source => "p"),
            Entry_Name => To_Unbounded_String(Source => ".menu.gameoptions"),
            Config_Name => To_Unbounded_String(Source => "")),
         10 =>
           (Shortcut => To_Unbounded_String(Source => "q"),
            Entry_Name => To_Unbounded_String(Source => ".menu.quit"),
            Config_Name => To_Unbounded_String(Source => "")),
         11 =>
           (Shortcut => To_Unbounded_String(Source => "x"),
            Entry_Name => To_Unbounded_String(Source => ".menu.resign"),
            Config_Name => To_Unbounded_String(Source => "")),
         12 =>
           (Shortcut => To_Unbounded_String(Source => "e"),
            Entry_Name => To_Unbounded_String(Source => ".menu.menu"),
            Config_Name => To_Unbounded_String(Source => "")));
      Default_Map_Accels: constant array(1 .. 23) of Accel_Data :=
        (1 =>
           (Shortcut => To_Unbounded_String(Source => "Shift-Return"),
            Entry_Name => To_Unbounded_String(Source => ".map.center"),
            Config_Name => To_Unbounded_String(Source => "")),
         2 =>
           (Shortcut => To_Unbounded_String(Source => "Shift-h"),
            Entry_Name => To_Unbounded_String(Source => ".map.centerhomebase"),
            Config_Name => To_Unbounded_String(Source => "")),
         3 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Home" else "KP_7")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapupleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         4 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" & (if Dir_Separator = '\' then "Up" else "KP_8")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapup"),
            Config_Name => To_Unbounded_String(Source => "")),
         5 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Prior" else "KP_9")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapupright"),
            Config_Name => To_Unbounded_String(Source => "")),
         6 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Left" else "KP_4")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         7 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Right" else "KP_6")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapright"),
            Config_Name => To_Unbounded_String(Source => "")),
         8 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" & (if Dir_Separator = '\' then "End" else "KP_1")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapdownleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         9 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Down" else "KP_2")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapdown"),
            Config_Name => To_Unbounded_String(Source => "")),
         10 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Shift-" &
                   (if Dir_Separator = '\' then "Next" else "KP_3")),
            Entry_Name => To_Unbounded_String(Source => ".map.mapdownright"),
            Config_Name => To_Unbounded_String(Source => "")),
         11 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Home" else "KP_Home")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursorupleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         12 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Up" else "KP_Up")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursorup"),
            Config_Name => To_Unbounded_String(Source => "")),
         13 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Prior" else "KP_Prior")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursorupright"),
            Config_Name => To_Unbounded_String(Source => "")),
         14 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Left" else "KP_Left")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursorleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         15 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Right" else "KP_Right")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursorright"),
            Config_Name => To_Unbounded_String(Source => "")),
         16 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "End" else "KP_End")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursordownleft"),
            Config_Name => To_Unbounded_String(Source => "")),
         17 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Down" else "KP_Down")),
            Entry_Name => To_Unbounded_String(Source => ".map.cursordown"),
            Config_Name => To_Unbounded_String(Source => "")),
         18 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Next" else "KP_Next")),
            Entry_Name =>
              To_Unbounded_String(Source => ".map.cursordownright"),
            Config_Name => To_Unbounded_String(Source => "")),
         19 =>
           (Shortcut =>
              To_Unbounded_String
                (Source =>
                   "Control-" &
                   (if Dir_Separator = '\' then "Begin" else "Return")),
            Entry_Name => To_Unbounded_String(Source => ".map.clickmouse"),
            Config_Name => To_Unbounded_String(Source => "")),
         20 =>
           (Shortcut => To_Unbounded_String(Source => "Control-a"),
            Entry_Name => To_Unbounded_String(Source => ".movement.fullstop"),
            Config_Name => To_Unbounded_String(Source => "")),
         21 =>
           (Shortcut => To_Unbounded_String(Source => "Control-b"),
            Entry_Name =>
              To_Unbounded_String(Source => ".movement.quarterspeed"),
            Config_Name => To_Unbounded_String(Source => "")),
         22 =>
           (Shortcut => To_Unbounded_String(Source => "Control-c"),
            Entry_Name => To_Unbounded_String(Source => ".movement.halfspeed"),
            Config_Name => To_Unbounded_String(Source => "")),
         23 =>
           (Shortcut => To_Unbounded_String(Source => "Control-d"),
            Entry_Name => To_Unbounded_String(Source => ".movement.fullspeed"),
            Config_Name => To_Unbounded_String(Source => "")));
      Default_General_Accels: constant array(1 .. 4) of Accel_Data :=
        (1 =>
           (Shortcut => To_Unbounded_String(Source => "Alt-a"),
            Entry_Name => To_Unbounded_String(Source => ".ui.resizefirst"),
            Config_Name => To_Unbounded_String(Source => "")),
         2 =>
           (Shortcut => To_Unbounded_String(Source => "Alt-b"),
            Entry_Name => To_Unbounded_String(Source => ".ui.resizesecond"),
            Config_Name => To_Unbounded_String(Source => "")),
         3 =>
           (Shortcut => To_Unbounded_String(Source => "Alt-c"),
            Entry_Name => To_Unbounded_String(Source => ".ui.resizethird"),
            Config_Name => To_Unbounded_String(Source => "")),
         4 =>
           (Shortcut => To_Unbounded_String(Source => "Alt-d"),
            Entry_Name => To_Unbounded_String(Source => ".ui.resizefourth"),
            Config_Name => To_Unbounded_String(Source => "")));
      Key_Entry: Ttk_Entry; --## rule line off IMPROPER_INITIALIZATION
   begin
      Key_Entry.Interp := Interp;
      if CArgv.Arg(Argv => Argv, N => 1) = "movement" then
         Reset_Movement_Keys_Loop :
         for Accel of Default_Movement_Accels loop
            Key_Entry.Name :=
              New_String
                (Str =>
                   ".gameframe.paned.optionsframe.canvas.options" &
                   To_String(Source => Accel.Entry_Name));
            Delete
              (TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
            Insert
              (TextEntry => Key_Entry, Index => "0",
               Text => To_String(Source => Accel.Shortcut));
         end loop Reset_Movement_Keys_Loop;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "menu" then
         Reset_Menu_Keys_Loop :
         for Accel of Default_Menu_Accels loop
            Key_Entry.Name :=
              New_String
                (Str =>
                   ".gameframe.paned.optionsframe.canvas.options" &
                   To_String(Source => Accel.Entry_Name));
            Delete
              (TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
            Insert
              (TextEntry => Key_Entry, Index => "0",
               Text => To_String(Source => Accel.Shortcut));
         end loop Reset_Menu_Keys_Loop;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "map" then
         Reset_Map_Keys_Loop :
         for Accel of Default_Map_Accels loop
            Key_Entry.Name :=
              New_String
                (Str =>
                   ".gameframe.paned.optionsframe.canvas.options" &
                   To_String(Source => Accel.Entry_Name));
            Delete
              (TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
            Insert
              (TextEntry => Key_Entry, Index => "0",
               Text => To_String(Source => Accel.Shortcut));
         end loop Reset_Map_Keys_Loop;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "general" then
         Reset_General_Keys_Loop :
         for Accel of Default_General_Accels loop
            Key_Entry.Name :=
              New_String
                (Str =>
                   ".gameframe.paned.optionsframe.canvas.options" &
                   To_String(Source => Accel.Entry_Name));
            Delete
              (TextEntry => Key_Entry, FirstIndex => "0", LastIndex => "end");
            Insert
              (TextEntry => Key_Entry, Index => "0",
               Text => To_String(Source => Accel.Shortcut));
         end loop Reset_General_Keys_Loop;
      end if;
      return TCL_OK;
   end Reset_Keys_Command;

   procedure Add_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "ShowOptions", Ada_Command => Show_Options_Command'Access);
      Add_Command(Name => "SetFonts", Ada_Command => Set_Fonts_Command'Access);
      Add_Command
        (Name => "SetDefaultFonts",
         Ada_Command => Set_Default_Fonts_Command'Access);
      Add_Command
        (Name => "CloseOptions", Ada_Command => Close_Options_Command'Access);
      Add_Command
        (Name => "ShowOptionsTab",
         Ada_Command => Show_Options_Tab_Command'Access);
      Add_Command
        (Name => "ResetKeys", Ada_Command => Reset_Keys_Command'Access);
   end Add_Commands;

end GameOptions;
