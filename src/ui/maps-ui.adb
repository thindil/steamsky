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

with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo;
with Crafts.UI;
with CoreUI;
with DebugUI;
with GameOptions;
with Help.UI;
with Knowledge;
with Log;
with Messages;
with Messages.UI;
with Missions.UI;
with OrdersMenu;
with Ships;
with Ships.UI;
with Statistics;
with Statistics.UI;
with Trades.UI;
with WaitMenu;
with Maps.UI.Commands;

package body Maps.UI is

   -- ****iv* MUI/MUI.MapView
   -- FUNCTION
   -- Text widget with the sky map
   -- SOURCE
   Map_View: Tk_Text;
   -- ****

   --## rule off REDUCEABLE_SCOPE
   -- ****if* MUI/MUI.Get_Map_View
   -- FUNCTION
   -- Get the text widget with the sky map
   -- RESULT
   -- Returns text widget with the sky map
   -- SOURCE
   function Get_Map_View return Tk_Text is
      -- ****
   begin
      return Map_View;
   end Get_Map_View;
   --## rule on REDUCEABLE_SCOPE

   procedure Create_Game_Ui is
      use Tcl.Tk.Ada;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
      use Tcl.Tk.Ada.Winfo;
      use CoreUI;
      use DebugUI;
      use Log;
      use Maps.UI.Commands;

      Game_Frame: constant Ttk_Frame := Get_Widget(pathName => ".gameframe");
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(pathName => Game_Frame & ".paned");
      Header: constant Ttk_Frame :=
        Get_Widget(pathName => Game_Frame & ".header");
      procedure Create_Ada_Game_Ui with
         Import => True,
         Convention => C,
         External_Name => "createGameUi";
   begin
      Map_View := Get_Widget(pathName => Paned & ".mapframe.map");
      if Winfo_Get(Widgt => Get_Map_View, Info => "exists") = "0" then
         Add_Maps_Commands;
         Main_Paned := Paned;
         Game_Header := Header;
         Close_Button := Get_Widget(pathName => Game_Header & ".closebutton");
         OrdersMenu.Add_Commands;
         WaitMenu.Add_Commands;
         Help.UI.Add_Commands;
         Ships.UI.Add_Commands;
         Crafts.UI.Add_Commands;
         Messages.UI.Add_Commands;
         GameOptions.Add_Commands;
         Trades.UI.Add_Commands;
         Knowledge.Add_Commands;
         Missions.UI.Add_Commands;
         Statistics.UI.Add_Commands;
         if Log.Debug_Mode = Log.MENU then
            Show_Debug_Ui;
         end if;
      end if;
      Create_Ada_Game_Ui;
   end Create_Game_Ui;

end Maps.UI;
