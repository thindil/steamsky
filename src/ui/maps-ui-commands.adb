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

with Ada.Containers; use Ada.Containers;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Event; use Tcl.Tk.Ada.Event;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Combat.UI; use Combat.UI;
with Config; use Config;
with Crew; use Crew;
with Events; use Events;
with Events.UI; use Events.UI;
with Factions; use Factions;
with Game; use Game;
with MainMenu; use MainMenu;
with Messages; use Messages;
with Missions; use Missions;
with Missions.UI; use Missions.UI;
with OrdersMenu; use OrdersMenu;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Statistics.UI; use Statistics.UI;
with Stories; use Stories;
with Stories.UI; use Stories.UI;
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
      if StartY + Integer'Value(Slice(MapIndex, 1, Index(MapIndex, ".") - 1)) -
        1 <
        1 then
         return TCL_OK;
      end if;
      MapY :=
        StartY + Integer'Value(Slice(MapIndex, 1, Index(MapIndex, ".") - 1)) -
        1;
      if StartX +
        Integer'Value
          (Slice(MapIndex, Index(MapIndex, ".") + 1, Length(MapIndex))) <
        1 then
         return TCL_OK;
      end if;
      MapX :=
        StartX +
        Integer'Value
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
      UpdateMoveButtons;
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
      if Winfo_Get(MapView, "ismapped") = "0" then
         return TCL_OK;
      end if;
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

   -- ****if* MapCommands/Zoom_Map_Command
   -- FUNCTION
   -- Zoom the sky map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Zoom_Map_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Zoom_Map_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
   begin
      if CArgv.Arg(Argv, 1) = "raise" then
         GameSettings.MapFontSize := GameSettings.MapFontSize + 1;
      else
         GameSettings.MapFontSize := GameSettings.MapFontSize - 1;
      end if;
      if GameSettings.MapFontSize < 3 then
         GameSettings.MapFontSize := 3;
      elsif GameSettings.MapFontSize > 50 then
         GameSettings.MapFontSize := 50;
      end if;
      Tcl_Eval
        (Interp,
         "font configure MapFont -size" &
         Positive'Image(GameSettings.MapFontSize));
      return Draw_Map_Command(ClientData, Interp, Argc, Argv);
   end Zoom_Map_Command;

   -- ****if* MapCommands/Move_Command
   -- FUNCTION
   -- Move the player ship in the selected location and check what happens
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Move_Ship_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Ship_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Message: Unbounded_String;
      Result: Natural;
      StartsCombat: Boolean := False;
      NewX, NewY: Integer := 0;
   begin
      if CArgv.Arg(Argv, 1) = "n" then -- Move up
         Result := MoveShip(0, -1, Message);
      elsif CArgv.Arg(Argv, 1) = "s" then -- Move down
         Result := MoveShip(0, 1, Message);
      elsif CArgv.Arg(Argv, 1) = "e" then -- Move right
         Result := MoveShip(1, 0, Message);
      elsif CArgv.Arg(Argv, 1) = "w" then -- Move left
         Result := MoveShip(-1, 0, Message);
      elsif CArgv.Arg(Argv, 1) = "sw" then -- Move down/left
         Result := MoveShip(-1, 1, Message);
      elsif CArgv.Arg(Argv, 1) = "se" then -- Move down/right
         Result := MoveShip(1, 1, Message);
      elsif CArgv.Arg(Argv, 1) = "nw" then -- Move up/left
         Result := MoveShip(-1, -1, Message);
      elsif CArgv.Arg(Argv, 1) = "ne" then -- Move up/right
         Result := MoveShip(1, -1, Message);
      elsif CArgv.Arg(Argv, 1) =
        "waitormove" then -- Move to destination or wait 1 game minute
         if PlayerShip.DestinationX = 0 and PlayerShip.DestinationY = 0 then
            Result := 1;
            UpdateGame(1);
            WaitInPlace(1);
         else
            if PlayerShip.DestinationX > PlayerShip.SkyX then
               NewX := 1;
            elsif PlayerShip.DestinationX < PlayerShip.SkyX then
               NewX := -1;
            end if;
            if PlayerShip.DestinationY > PlayerShip.SkyY then
               NewY := 1;
            elsif PlayerShip.DestinationY < PlayerShip.SkyY then
               NewY := -1;
            end if;
            Result := MoveShip(NewX, NewY, Message);
            if PlayerShip.DestinationX = PlayerShip.SkyX and
              PlayerShip.DestinationY = PlayerShip.SkyY then
               AddMessage
                 ("You reached your travel destination.", OrderMessage);
               PlayerShip.DestinationX := 0;
               PlayerShip.DestinationY := 0;
               if GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
               Result := 4;
            end if;
         end if;
      elsif CArgv.Arg(Argv, 1) = "moveto" then -- Move to destination
         loop
            NewX := 0;
            NewY := 0;
            if PlayerShip.DestinationX > PlayerShip.SkyX then
               NewX := 1;
            elsif PlayerShip.DestinationX < PlayerShip.SkyX then
               NewX := -1;
            end if;
            if PlayerShip.DestinationY > PlayerShip.SkyY then
               NewY := 1;
            elsif PlayerShip.DestinationY < PlayerShip.SkyY then
               NewY := -1;
            end if;
            Result := MoveShip(NewX, NewY, Message);
            exit when Result = 0;
            StartsCombat := CheckForEvent;
            if StartsCombat then
               Result := 4;
               exit;
            end if;
            if Result = 8 then
               WaitForRest;
               if not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
                   (To_Unbounded_String("sentientships"))
                 and then
                 (FindMember(Pilot) = 0 or FindMember(Engineer) = 0) then
                  WaitForRest;
               end if;
               Result := 1;
               StartsCombat := CheckForEvent;
               if StartsCombat then
                  Result := 4;
                  exit;
               end if;
            end if;
            if GameSettings.AutoMoveStop /= NEVER and
              SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
               declare
                  EventIndex: constant Positive :=
                    SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex;
               begin
                  case GameSettings.AutoMoveStop is
                     when ANY =>
                        if Events_List(EventIndex).EType = EnemyShip or
                          Events_List(EventIndex).EType = Trader or
                          Events_List(EventIndex).EType = FriendlyShip or
                          Events_List(EventIndex).EType = EnemyPatrol then
                           Result := 0;
                           exit;
                        end if;
                     when FRIENDLY =>
                        if Events_List(EventIndex).EType = Trader or
                          Events_List(EventIndex).EType = FriendlyShip then
                           Result := 0;
                           exit;
                        end if;
                     when Config.ENEMY =>
                        if Events_List(EventIndex).EType = EnemyShip or
                          Events_List(EventIndex).EType = EnemyPatrol then
                           Result := 0;
                           exit;
                        end if;
                     when NEVER =>
                        null;
                  end case;
               end;
            end if;
            declare
               MessageDialog: Ttk_Frame;
            begin
               MessageDialog.Interp := Interp;
               MessageDialog.Name := New_String(".message");
               if Winfo_Get(MessageDialog, "exists") = "0" then
                  if GetItemAmount(FuelType) <= GameSettings.LowFuel then
                     ShowMessage("Your fuel level is dangerously low.");
                     Result := 4;
                     exit;
                  elsif GetItemsAmount("Food") <= GameSettings.LowFood then
                     ShowMessage("Your food level is dangerously low.");
                     Result := 4;
                     exit;
                  elsif GetItemsAmount("Drinks") <= GameSettings.LowDrinks then
                     ShowMessage("Your drinks level is dangerously low.");
                     Result := 4;
                     exit;
                  end if;
               end if;
            end;
            if PlayerShip.DestinationX = PlayerShip.SkyX and
              PlayerShip.DestinationY = PlayerShip.SkyY then
               AddMessage
                 ("You reached your travel destination.", OrderMessage);
               PlayerShip.DestinationX := 0;
               PlayerShip.DestinationY := 0;
               if GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
               Result := 4;
               exit;
            end if;
            exit when Result = 6 or Result = 7;
         end loop;
      end if;
      case Result is
         when 1 => -- Ship moved, check for events
            StartsCombat := CheckForEvent;
            if not StartsCombat and GameSettings.AutoFinish then
               Message := To_Unbounded_String(AutoFinishMissions);
            end if;
         when 6 => -- Ship moved, but pilot needs rest, confirm
            if MessageBox
                ("-message {You don't have pilot on duty. Did you want to wait until your pilot rest?} -icon question -type yesno") =
              "yes" then
               WaitForRest;
               StartsCombat := CheckForEvent;
               if not StartsCombat and GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
            end if;
         when 7 => -- Ship moved, but engineer needs rest, confirm
            if MessageBox
                ("-message {You don't have engineer on duty. Did you want to wait until your engineer rest?} -icon question -type yesno") =
              "yes" then
               WaitForRest;
               StartsCombat := CheckForEvent;
               if not StartsCombat and GameSettings.AutoFinish then
                  Message := To_Unbounded_String(AutoFinishMissions);
               end if;
            end if;
         when 8 => -- Ship moved, but crew needs rest, autorest
            StartsCombat := CheckForEvent;
            if not StartsCombat then
               WaitForRest;
               if not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
                   (To_Unbounded_String("sentientships"))
                 and then
                 (FindMember(Pilot) = 0 or FindMember(Engineer) = 0) then
                  WaitForRest;
               end if;
               StartsCombat := CheckForEvent;
            end if;
            if not StartsCombat and GameSettings.AutoFinish then
               Message := To_Unbounded_String(AutoFinishMissions);
            end if;
         when others =>
            null;
      end case;
      if Message /= Null_Unbounded_String then
         ShowMessage(To_String(Message));
      end if;
      CenterX := PlayerShip.SkyX;
      CenterY := PlayerShip.SkyY;
      if StartsCombat then
         ShowCombatUI;
      else
         ShowSkyMap;
      end if;
      return TCL_OK;
   end Move_Ship_Command;

   -- ****if* MapCommands/Quit_Game_Command
   -- FUNCTION
   -- Ask player if he/she wants to quit from the game and if yes, save it and
   -- show main menu
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Quit_Game_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Quit_Game_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Paned: Ttk_PanedWindow;
   begin
      if MessageBox
          ("-message {Are you sure want to quit?} -icon question -type yesno") =
        "yes" then
         Paned.Interp := Interp;
         Paned.Name := New_String(".paned");
         GameSettings.MessagesPosition := Natural'Value(SashPos(Paned, "0"));
         EndGame(True);
         ShowMainMenu;
      end if;
      return TCL_OK;
   end Quit_Game_Command;

   -- ****if* MapCommands/Resign_Game_Command
   -- FUNCTION
   -- Resing from the game - if player resigned, kill he/she character and
   -- follow as for death of the player's character
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Resign_Game_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Resign_Game_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      if MessageBox
          ("-message {Are you sure want to resign from game?} -icon question -type yesno") =
        "yes" then
         Death(1, To_Unbounded_String("resignation"), PlayerShip);
         DeathConfirm;
      end if;
      return TCL_OK;
   end Resign_Game_Command;

   -- ****if* MapCommands/Show_Stats_Command
   -- FUNCTION
   -- Show the player's game statistics
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Stats_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Stats_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      CloseButton: Ttk_Button;
   begin
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      ShowStatistics;
      return TCL_OK;
   end Show_Stats_Command;

   -- ****if* MapCommands/Show_Sky_Map_Command
   -- FUNCTION
   -- Show sky map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Sky_Map_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Sky_Map_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      CloseButton: Ttk_Button;
   begin
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
      ShowSkyMap(True);
      return TCL_OK;
   end Show_Sky_Map_Command;

   -- ****if* MapCommands/Show_Events_Command
   -- FUNCTION
   -- Show the list of known events
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Events_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Events_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      CloseButton: Ttk_Button;
   begin
      if Events_List.Length = 0 then
         ShowMessage
           ("You dont know any event yet. You may ask for events in bases. When your ship is docked to base, select Ask for Events from ship orders menu.");
         return TCL_OK;
      end if;
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      ShowEventsList;
      return TCL_OK;
   end Show_Events_Command;

   -- ****if* MapCommands/Show_Missions_Command
   -- FUNCTION
   -- Show the list of accepted missions
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Show_Missions_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Missions_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      CloseButton: Ttk_Button;
   begin
      if CArgv.Arg(Argv, 1) = "accepted" then
         if AcceptedMissions.Length = 0 then
            ShowMessage
              ("You didn't accepted any mission yet. You may ask for missions in bases. When your ship is docked to base, check Missions from ship orders menu.");
            return TCL_OK;
         end if;
         ShowMissionsList;
      else
         ShowMissionsList(False);
      end if;
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      return TCL_OK;
   end Show_Missions_Command;

   -- ****if* MapCommands/Show_Stories_Command
   -- FUNCTION
   -- Show the list of know stories
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Show_Stories_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Stories_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      CloseButton: Ttk_Button;
   begin
      if FinishedStories.Length = 0 then
         ShowMessage("You didn't discovered any story yet.");
         return TCL_OK;
      end if;
      ShowStories;
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      return TCL_OK;
   end Show_Stories_Command;

   -- ****if* MapCommands/Move_Cursor_Command
   -- FUNCTION
   -- Move mouse cursor with keyboard
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Move_Mouse_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Move_Mouse_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      MapView: Tk_Text;
   begin
      MapView.Interp := Interp;
      MapView.Name := New_String(".paned.mapframe.map");
      if CArgv.Arg(Argv, 1) = "click" then
         Generate
           (MapView, "<1>",
            "-x " & CArgv.Arg(Argv, 2) & " -y " & CArgv.Arg(Argv, 3));
      elsif CArgv.Arg(Argv, 1) = "nw" then
         Generate
           (MapView, "<Motion>",
            "-warp 1 -x [expr " & CArgv.Arg(Argv, 2) & "-1] -y [expr " &
            CArgv.Arg(Argv, 3) & "-1]");
      elsif CArgv.Arg(Argv, 1) = "n" then
         Generate
           (MapView, "<Motion>",
            "-warp 1 -x " & CArgv.Arg(Argv, 2) & " -y [expr " &
            CArgv.Arg(Argv, 3) & "-1]");
      elsif CArgv.Arg(Argv, 1) = "ne" then
         Generate
           (MapView, "<Motion>",
            "-warp 1 -x [expr " & CArgv.Arg(Argv, 2) & "+1] -y [expr " &
            CArgv.Arg(Argv, 3) & "-1]");
      elsif CArgv.Arg(Argv, 1) = "w" then
         Generate
           (MapView, "<Motion>",
            "-warp 1 -x [expr " & CArgv.Arg(Argv, 2) & "-1] -y " &
            CArgv.Arg(Argv, 3));
      elsif CArgv.Arg(Argv, 1) = "e" then
         Generate
           (MapView, "<Motion>",
            "-warp 1 -x [expr " & CArgv.Arg(Argv, 2) & "+1] -y " &
            CArgv.Arg(Argv, 3));
      elsif CArgv.Arg(Argv, 1) = "sw" then
         Generate
           (MapView, "<Motion>",
            "-warp 1 -x [expr " & CArgv.Arg(Argv, 2) & "-1] -y [expr " &
            CArgv.Arg(Argv, 3) & "+1]");
      elsif CArgv.Arg(Argv, 1) = "s" then
         Generate
           (MapView, "<Motion>",
            "-warp 1 -x " & CArgv.Arg(Argv, 2) & " -y [expr " &
            CArgv.Arg(Argv, 3) & "+1]");
      elsif CArgv.Arg(Argv, 1) = "se" then
         Generate
           (MapView, "<Motion>",
            "-warp 1 -x [expr " & CArgv.Arg(Argv, 2) & "+1] -y [expr " &
            CArgv.Arg(Argv, 3) & "+1]");
      end if;
      return TCL_OK;
   end Move_Mouse_Command;

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
      AddCommand("ZoomMap", Zoom_Map_Command'Access);
      AddCommand("MoveShip", Move_Ship_Command'Access);
      AddCommand("QuitGame", Quit_Game_Command'Access);
      AddCommand("ResignGame", Resign_Game_Command'Access);
      AddCommand("ShowStats", Show_Stats_Command'Access);
      AddCommand("ShowSkyMap", Show_Sky_Map_Command'Access);
      AddCommand("ShowEvents", Show_Events_Command'Access);
      AddCommand("ShowMissions", Show_Missions_Command'Access);
      AddCommand("ShowStories", Show_Stories_Command'Access);
      AddCommand("MoveCursor", Move_Mouse_Command'Access);
   end AddCommands;

end Maps.UI.Commands;
