-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Game; use Game;
with OrdersMenu;
with Ships; use Ships;

package body Maps.UI is

   -- ****iv* MUI/GameMenu
   -- FUNCTION
   -- The main game menu
   -- SOURCE
   GameMenu: Tk_Menu;
   -- ****

   -- ****if* MUI/CreateGameMenu
   -- FUNCTION
   -- Create the main game menu. Clear old elements and add all default
   -- SOURCE
   procedure CreateGameMenu is
      -- ****
   begin
      Delete(GameMenu, "0", "end");
      Add(GameMenu, "command", "-label {Ship information}");
      Add(GameMenu, "command", "-label {Ship cargo}");
      Add(GameMenu, "command", "-label {Crew information}");
      Add
        (GameMenu, "command",
         "-label {Ship orders} -command {tk_popup .orders [expr [winfo width .paned.mapframe] / 3] [expr [winfo height .paned.mapframe] / 3]}");
      Add(GameMenu, "command", "-label {Crafting}");
      Add(GameMenu, "command", "-label {Last messages}");
      Add(GameMenu, "command", "-label {List of known bases}");
      Add(GameMenu, "command", "-label {List of known events}");
      Add(GameMenu, "command", "-label {Accepted missions}");
      Add(GameMenu, "command", "-label {Stories}");
      Add(GameMenu, "command", "-label {Wait orders}");
      Add(GameMenu, "command", "-label {Game statistics}");
      Add(GameMenu, "command", "-label {Help}");
      Add(GameMenu, "command", "-label {Game options}");
      Add(GameMenu, "command", "-label {Quit from game}");
      Add(GameMenu, "command", "-label {Resign from game}");
   end CreateGameMenu;

   procedure CreateGameUI is
   begin
      GameMenu.Interp := Get_Context;
      GameMenu.Name := New_String(".gamemenu");
      if Winfo_Get(GameMenu, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "game.tcl");
         OrdersMenu.AddCommands;
      end if;
      CreateGameMenu;
      for I in MenuAccelerators'Range loop
         Entry_Configure
           (GameMenu, Positive'Image(I),
            "-accelerator {" & To_String(MenuAccelerators(I)) & "}");
         Bind_To_Main_Window
           (Get_Context, "<" & To_String(MenuAccelerators(I)) & ">",
            "{.gamemenu invoke" & Positive'Image(I) & "}");
      end loop;
   end CreateGameUI;

end Maps.UI;
