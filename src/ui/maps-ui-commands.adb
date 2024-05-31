-- Copyright (c) 2024 Bartek thindil Jasicki
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
with CArgv;
with Tcl;
with Utils.UI;
with Ships;

package body Maps.UI.Commands is

   function Hide_Map_Buttons_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "hideMapButtonsCommand";

   function Show_Map_Buttons_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showMapButtonsCommand";

   function Move_Map_Buttons_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "moveMapButtonsCommand";

   function Move_Map_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "moveMapInfoCommand";

   function Draw_Map_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "drawMapCommand";

   function Zoom_Map_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "zoomMapCommand";

   function Update_Map_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "updateMapInfoCommand";

   function Show_Destination_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showDestinationMenuCommand";

   function Set_Ship_Destination_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setShipDestinationCommand";

   function Move_Map_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "moveMapCommand";

   function Move_Ship_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;

   function Move_Ship_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      use Tcl;
      use Ships;

      function Move_Ada_Ship_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Import => True,
         Convention => C,
         External_Name => "moveShipCommand";
   begin
      if Move_Ada_Ship_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv) =
        TCL_OK then
         Get_Ship_From_Nim(Ship => Player_Ship);
         Get_Map_Y_Loop :
         for Y in 1 .. 1_024 loop
            Get_Map_X_Loop :
            for X in 1 .. 1_024 loop
               Set_Map_Cell(X => X, Y => Y);
            end loop Get_Map_X_Loop;
         end loop Get_Map_Y_Loop;
      end if;
      return TCL_OK;
   end Move_Ship_Command;

   function Quit_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "quitGameCommand";

   function Resign_Game_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "resignGameCommand";

   function Show_Stats_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showStatsCommand";

   function Show_Sky_Map_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showSkyMapCommand";

   function Move_Mouse_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "moveMouseCommand";

   function Toggle_Full_Screen_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "toggleFullScreenCommand";

   function Resize_Last_Messages_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "resizeLastMessagesCommand";

   function Show_Game_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showGameMenuCommand";

   function Invoke_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "invokeMenuCommand";

   function Set_Ship_Speed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setShipSpeedCommand";

   procedure Add_Maps_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "HideMapButtons",
         Ada_Command => Hide_Map_Buttons_Command'Access);
      Add_Command
        (Name => "ShowMapButtons",
         Ada_Command => Show_Map_Buttons_Command'Access);
      Add_Command
        (Name => "MoveMapButtons",
         Ada_Command => Move_Map_Buttons_Command'Access);
      Add_Command
        (Name => "MoveMapInfo", Ada_Command => Move_Map_Info_Command'Access);
      Add_Command(Name => "DrawMap", Ada_Command => Draw_Map_Command'Access);
      Add_Command(Name => "ZoomMap", Ada_Command => Zoom_Map_Command'Access);
      Add_Command
        (Name => "UpdateMapInfo",
         Ada_Command => Update_Map_Info_Command'Access);
      Add_Command
        (Name => "ShowDestinationMenu",
         Ada_Command => Show_Destination_Menu_Command'Access);
      Add_Command
        (Name => "SetDestination",
         Ada_Command => Set_Ship_Destination_Command'Access);
      Add_Command(Name => "MoveMap", Ada_Command => Move_Map_Command'Access);
      Add_Command(Name => "MoveShip", Ada_Command => Move_Ship_Command'Access);
      Add_Command(Name => "QuitGame", Ada_Command => Quit_Game_Command'Access);
      Add_Command
        (Name => "ResignGame", Ada_Command => Resign_Game_Command'Access);
      Add_Command
        (Name => "ShowStats", Ada_Command => Show_Stats_Command'Access);
      Add_Command
        (Name => "ShowSkyMap", Ada_Command => Show_Sky_Map_Command'Access);
      Add_Command
        (Name => "MoveCursor", Ada_Command => Move_Mouse_Command'Access);
      Add_Command
        (Name => "ToggleFullScreen",
         Ada_Command => Toggle_Full_Screen_Command'Access);
      Add_Command
        (Name => "ResizeLastMessages",
         Ada_Command => Resize_Last_Messages_Command'Access);
      Add_Command
        (Name => "ShowGameMenu", Ada_Command => Show_Game_Menu_Command'Access);
      Add_Command
        (Name => "InvokeMenu", Ada_Command => Invoke_Menu_Command'Access);
      Add_Command
        (Name => "SetShipSpeed", Ada_Command => Set_Ship_Speed_Command'Access);
   end Add_Maps_Commands;

end Maps.UI.Commands;
