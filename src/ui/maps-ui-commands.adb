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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Bases; use Bases;
with Messages; use Messages;
with OrdersMenu; use OrdersMenu;
with Utils.UI; use Utils.UI;

package body Maps.UI.Commands is

   ButtonNames: constant array(1 .. 13) of Unbounded_String :=
     (To_Unbounded_String("show"), To_Unbounded_String("nw"),
      To_Unbounded_String("n"), To_Unbounded_String("ne"),
      To_Unbounded_String("w"), To_Unbounded_String("wait"),
      To_Unbounded_String("e"), To_Unbounded_String("sw"),
      To_Unbounded_String("s"), To_Unbounded_String("se"),
      To_Unbounded_String("hide"), To_Unbounded_String("left"),
      To_Unbounded_String("right"));

   -- ****if* MapCommands/Hide_Map_Buttons_Command
   -- FUNCTION
   -- Hide map movement buttons
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Hide_Map_Buttons_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Hide_Map_Buttons_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Button: Ttk_Button;
   begin
      Button.Interp := Interp;
      for I in 2 .. 13 loop
         Button.Name :=
           New_String(".paned.mapframe.buttons." & To_String(ButtonNames(I)));
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
      end loop;
      Button.Name := New_String(".paned.mapframe.buttons.show");
      Tcl.Tk.Ada.Grid.Grid(Button);
      return TCL_OK;
   end Hide_Map_Buttons_Command;

   -- ****if* MapCommands/Show_Map_Buttons_Command
   -- FUNCTION
   -- Show map movement buttons
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Map_Buttons_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Map_Buttons_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Button: Ttk_Button;
      ButtonsBox: Ttk_Frame;
   begin
      Button.Interp := Interp;
      ButtonsBox.Interp := Interp;
      ButtonsBox.Name := New_String(".paned.mapframe.buttons");
      for I in 2 .. 11 loop
         Button.Name :=
           New_String
             (Widget_Image(ButtonsBox) & "." & To_String(ButtonNames(I)));
         Tcl.Tk.Ada.Grid.Grid(Button);
      end loop;
      Button.Name := New_String(Widget_Image(ButtonsBox) & ".show");
      Tcl.Tk.Ada.Grid.Grid_Remove(Button);
      if Index(Tcl.Tk.Ada.Grid.Grid_Info(ButtonsBox), "-sticky es") = 0 then
         Button.Name := New_String(Widget_Image(ButtonsBox) & ".right");
      else
         Button.Name := New_String(Widget_Image(ButtonsBox) & ".left");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Button);
      return TCL_OK;
   end Show_Map_Buttons_Command;

   -- ****if* MapCommands/Move_Map_Buttons_Command
   -- FUNCTION
   -- Move map movement buttons left of right
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Move_Map_Buttons_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Map_Buttons_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Button: Ttk_Button;
      ButtonsBox: Ttk_Frame;
   begin
      ButtonsBox.Interp := Interp;
      ButtonsBox.Name := New_String(".paned.mapframe.buttons");
      Button.Interp := Interp;
      Button.Name :=
        New_String(Widget_Image(ButtonsBox) & "." & CArgv.Arg(Argv, 1));
      Tcl.Tk.Ada.Grid.Grid_Remove(Button);
      if CArgv.Arg(Argv, 1) = "left" then
         Button.Name := New_String(Widget_Image(ButtonsBox) & ".right");
         Tcl.Tk.Ada.Grid.Grid_Configure(ButtonsBox, "-sticky sw");
      else
         Button.Name := New_String(Widget_Image(ButtonsBox) & ".left");
         Tcl.Tk.Ada.Grid.Grid_Configure(ButtonsBox, "-sticky se");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Button);
      return TCL_OK;
   end Move_Map_Buttons_Command;

   -- ****if* MapCommands/Draw_Map_Command
   -- FUNCTION
   -- Draw the sky map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Draw_Map_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Draw_Map_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MapView: Tk_Text;
   begin
      MapView.Interp := Interp;
      MapView.Name := New_String(".paned.mapframe.map");
      configure
        (MapView,
         "-width [expr [winfo width $mapview] / [font measure MapFont { }] - 2]");
      configure
        (MapView,
         "-height [expr [winfo height $mapview] / [font metrics MapFont -linespace] - 1]");
      DrawMap;
      return TCL_OK;
   end Draw_Map_Command;

   -- ****iv* Maps.UI.Commands/MapX
   -- FUNCTION
   -- Current map cell X coordinate (where mouse is hovering)
   -- SOURCE
   MapX: Positive;
   -- ****

   -- ****iv* Maps.UI.Commands/MapX
   -- FUNCTION
   -- Current map cell Y coordinate (where mouse is hovering)
   -- SOURCE
   MapY: Positive;
   -- ****

   -- ****if* MapCommands/Update_Map_Info_Command
   -- FUNCTION
   -- Update map cell info
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Update_Map_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Map_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      MapView: Tk_Text;
      MapIndex: Unbounded_String;
   begin
      MapView.Interp := Interp;
      MapView.Name := New_String(".paned.mapframe.map");
      MapIndex :=
        To_Unbounded_String
          (Index
             (MapView, "@" & CArgv.Arg(Argv, 1) & "," & CArgv.Arg(Argv, 2)));
      MapY :=
        StartY + Positive'Value(Slice(MapIndex, 1, Index(MapIndex, ".") - 1)) -
        1;
      MapX :=
        StartX +
        Positive'Value
          (Slice(MapIndex, Index(MapIndex, ".") + 1, Length(MapIndex)));
      UpdateMapInfo(MapX, MapY);
      return TCL_OK;
   end Update_Map_Info_Command;

   -- ****if* MapCommands/Move_Map_Info_Command
   -- FUNCTION
   -- Move map info frame when mouse enter it
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Move_Map_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Map_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MapInfoFrame: Ttk_Frame;
   begin
      MapInfoFrame.Interp := Interp;
      MapInfoFrame.Name := New_String(".paned.mapframe.info");
      if Index(Tcl.Tk.Ada.Grid.Grid_Info(MapInfoFrame), "-sticky ne") = 0 then
         Tcl.Tk.Ada.Grid.Grid_Configure(MapInfoFrame, "-sticky ne");
      else
         Tcl.Tk.Ada.Grid.Grid_Configure(MapInfoFrame, "-sticky nw");
      end if;
      return TCL_OK;
   end Move_Map_Info_Command;

   -- ****if* MapCommands/Show_Destination_Menu_Command
   -- FUNCTION
   -- Create and show destination menu
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Destination_Menu_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Destination_Menu_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      DestinationMenu: Tk_Menu;
   begin
      if PlayerShip.SkyX = MapX and PlayerShip.SkyY = MapY then
         return Show_Orders_Command(ClientData, Interp, Argc, Argv);
      end if;
      DestinationMenu.Interp := Interp;
      DestinationMenu.Name := New_String(".destination");
      Delete(DestinationMenu, "0", "end");
      Add
        (DestinationMenu, "command",
         "-label {Set destination} -command SetDestination");
      if PlayerShip.Speed /= DOCKED then
         Add(DestinationMenu, "command", "-label {Set destination and move}");
         Add(DestinationMenu, "command", "-label {Move to}");
      end if;
      Add(DestinationMenu, "command", "-label {Close}");
      Tcl_Eval
        (Interp,
         "tk_popup .destination " & CArgv.Arg(Argv, 1) & " " &
         CArgv.Arg(Argv, 2));
      return TCL_OK;
   end Show_Destination_Menu_Command;

   -- ****if* MapCommands/Set_Destination_Command
   -- FUNCTION
   -- Set current map cell as destination for the player's ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Set_Destination_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Destination_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      PlayerShip.DestinationX := MapX;
      PlayerShip.DestinationY := MapY;
      AddMessage
        ("You set the travel destination for your ship.", OrderMessage);
      DrawMap;
      return TCL_OK;
   end Set_Destination_Command;

   -- ****if* MapCommands/Move_Map_Command
   -- FUNCTION
   -- Move map in the selected direction
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Move_Map_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Map_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      MapView: Tk_Text;
      MapHeight, MapWidth: Positive;
      SpinBox: Ttk_SpinBox;
   begin
      MapView.Interp := Interp;
      MapView.Name := New_String(".paned.mapframe.map");
      MapHeight := Positive'Value(cget(MapView, "-height"));
      MapWidth := Positive'Value(cget(MapView, "-width"));
      if CArgv.Arg(Argv, 1) = "centeronship" then
         CenterX := PlayerShip.SkyX;
         CenterY := PlayerShip.SkyY;
      elsif CArgv.Arg(Argv, 1) = "movemapto" then
         SpinBox.Interp := Interp;
         SpinBox.Name := New_String(".movemapdialog.frame.x");
         CenterX := Positive'Value(Get(SpinBox));
         SpinBox.Name := New_String(".movemapdialog.frame.y");
         CenterY := Positive'Value(Get(SpinBox));
      elsif CArgv.Arg(Argv, 1) = "n" then
         if CenterY - (MapHeight / 3) < 1 then
            CenterY := MapHeight / 3;
         else
            CenterY := CenterY - (MapHeight / 3);
         end if;
      elsif CArgv.Arg(Argv, 1) = "s" then
         if CenterY + (MapHeight / 3) > 1024 then
            CenterY := 1024 - (MapHeight / 3);
         else
            CenterY := CenterY + (MapHeight / 3);
         end if;
      elsif CArgv.Arg(Argv, 1) = "w" then
         if CenterX - (MapWidth / 3) < 1 then
            CenterX := MapWidth / 3;
         else
            CenterX := CenterX - (MapWidth / 3);
         end if;
      elsif CArgv.Arg(Argv, 1) = "e" then
         if CenterX + (MapWidth / 3) > 1024 then
            CenterX := 1024 - (MapWidth / 3);
         else
            CenterX := CenterX + (MapWidth / 3);
         end if;
      elsif CArgv.Arg(Argv, 1) = "nw" then
         if CenterY - (MapHeight / 3) < 1 then
            CenterY := MapHeight / 3;
         else
            CenterY := CenterY - (MapHeight / 3);
         end if;
         if CenterX - (MapWidth / 3) < 1 then
            CenterX := MapWidth / 3;
         else
            CenterX := CenterX - (MapWidth / 3);
         end if;
      elsif CArgv.Arg(Argv, 1) = "ne" then
         if CenterY - (MapHeight / 3) < 1 then
            CenterY := MapHeight / 3;
         else
            CenterY := CenterY - (MapHeight / 3);
         end if;
         if CenterX + (MapWidth / 3) > 1024 then
            CenterX := 1024 - (MapWidth / 3);
         else
            CenterX := CenterX + (MapWidth / 3);
         end if;
      elsif CArgv.Arg(Argv, 1) = "sw" then
         if CenterY + (MapHeight / 3) > 1024 then
            CenterY := 1024 - (MapHeight / 3);
         else
            CenterY := CenterY + (MapHeight / 3);
         end if;
         if CenterX - (MapWidth / 3) < 1 then
            CenterX := MapWidth / 3;
         else
            CenterX := CenterX - (MapWidth / 3);
         end if;
      elsif CArgv.Arg(Argv, 1) = "se" then
         if CenterY + (MapHeight / 3) > 1024 then
            CenterY := 1024 - (MapHeight / 3);
         else
            CenterY := CenterY + (MapHeight / 3);
         end if;
         if CenterX + (MapWidth / 3) > 1024 then
            CenterX := 1024 - (MapWidth / 3);
         else
            CenterX := CenterX + (MapWidth / 3);
         end if;
      elsif CArgv.Arg(Argv, 1) = "centeronhome" then
         CenterX := SkyBases(PlayerShip.HomeBase).SkyX;
         CenterY := SkyBases(PlayerShip.HomeBase).SkyY;
      end if;
      DrawMap;
      return TCL_OK;
   end Move_Map_Command;

   procedure AddCommands is
   begin
      AddCommand("HideMapButtons", Hide_Map_Buttons_Command'Access);
      AddCommand("ShowMapButtons", Show_Map_Buttons_Command'Access);
      AddCommand("MoveMapButtons", Move_Map_Buttons_Command'Access);
      AddCommand("DrawMap", Draw_Map_Command'Access);
      AddCommand("UpdateMapInfo", Update_Map_Info_Command'Access);
      AddCommand("MoveMapInfo", Move_Map_Info_Command'Access);
      AddCommand("ShowDestinationMenu", Show_Destination_Menu_Command'Access);
      AddCommand("SetDestination", Set_Destination_Command'Access);
      AddCommand("MoveMap", Move_Map_Command'Access);
   end AddCommands;

end Maps.UI.Commands;
