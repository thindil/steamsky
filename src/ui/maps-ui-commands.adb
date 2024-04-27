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
      External_Name => "hideMapButtonsCommand";

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
      Import => True,
      Convention => C,
      External_Name => "moveShipCommand";

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
   end Add_Maps_Commands;

end Maps.UI.Commands;
