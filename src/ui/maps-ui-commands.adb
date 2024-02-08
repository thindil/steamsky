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

with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
-- with Tcl.Ada; use Tcl.Ada;
-- with Tcl.Tk.Ada.Busy;
-- with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
-- with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
-- with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
-- with CoreUI;
with Dialogs; use Dialogs;
with Ships;
with Ships.Movement;
with Utils.UI;

package body Maps.UI.Commands is

   -- ****o* MapCommands/MapCommands.Invoke_Menu_Command
   -- FUNCTION
   -- Invoke the selected game menu option with the selected keyboard shortcut
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- InvokeMenu shortcut
   -- Shortcut, the keyboard shortcut which was pressed
   -- SOURCE
--   function Invoke_Menu_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
--      Convention => C;
--      -- ****
--
--   function Invoke_Menu_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Client_Data, Argc);
--      use CoreUI;
--
--      Focused_Widget: constant Ttk_Frame :=
--        Get_Widget(pathName => Focus(Interp => Interp), Interp => Interp);
--      Menu_Commands: constant array(1 .. 11) of Unbounded_String :=
--        (1 => To_Unbounded_String(Source => "ShowShipInfo"),
--         2 => To_Unbounded_String(Source => "ShowOrders"),
--         3 => To_Unbounded_String(Source => "ShowCrafting"),
--         4 => To_Unbounded_String(Source => "ShowLastMessages"),
--         5 => To_Unbounded_String(Source => "ShowKnowledge"),
--         6 => To_Unbounded_String(Source => "ShowWait"),
--         7 => To_Unbounded_String(Source => "ShowStats"),
--         8 => To_Unbounded_String(Source => "ShowHelp"),
--         9 => To_Unbounded_String(Source => "ShowOptions"),
--         10 => To_Unbounded_String(Source => "QuitGame"),
--         11 => To_Unbounded_String(Source => "ResignGame"));
--   begin
--      if Winfo_Get(Widgt => Focused_Widget, Info => "class") = "TEntry" or
--        Tcl.Tk.Ada.Busy.Status(Window => Game_Header) = "1" then
--         return TCL_OK;
--      end if;
--      Invoke_Button_Loop :
--      for I in 1 .. 11 loop
--         if Get_Menu_Accelerator(Index => I) =
--           CArgv.Arg(Argv => Argv, N => 1) then
--            Tcl_Eval
--              (interp => Interp,
--               strng => To_String(Source => Menu_Commands(I)));
--            return TCL_OK;
--         end if;
--      end loop Invoke_Button_Loop;
--      return TCL_OK;
--   end Invoke_Menu_Command;

   -- ****o* MapCommands/MapCommands.Set_Ship_Speed_Command
   -- FUNCTION
   -- Set the new speed for the player's ship
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetShipSpeed speed
   -- Speed is the new speed order for the player's ship.
   -- SOURCE
   function Set_Ship_Speed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Ship_Speed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Ships;
      use Ships.Movement;

      Message: constant String :=
        Change_Ship_Speed
          (Speed_Value =>
             Ship_Speed'Val
               (Natural'Value(CArgv.Arg(Argv => Argv, N => 1)) + 1));
   begin
      if Message'Length > 0 then
         Show_Message(Text => Message, Title => "Changing the ship's speed.");
      end if;
      return TCL_OK;
   end Set_Ship_Speed_Command;

   procedure Add_Commands is
      use Utils.UI;
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaMapsCommands";
   begin
      Add_Ada_Commands;
--      Add_Command
--        (Name => "InvokeMenu", Ada_Command => Invoke_Menu_Command'Access);
      Add_Command
        (Name => "SetShipSpeed", Ada_Command => Set_Ship_Speed_Command'Access);
   end Add_Commands;

end Maps.UI.Commands;
