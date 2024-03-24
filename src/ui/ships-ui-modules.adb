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
with Interfaces.C;
with CArgv;
with Tcl;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Dialogs;
with Utils.UI;
with ShipModules;

package body Ships.UI.Modules is

   -- ****o* SUModules/SUModules.Show_Assign_Ammo_Command
   -- FUNCTION
   -- Show the list of available ammo for the selected gun
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowAssingAmmo index
   -- Index is the module index of the selected gun which will be have
   -- assigned a new ammo
   -- SOURCE
   function Show_Assign_Ammo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Assign_Ammo_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Tcl;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Dialogs;
      use ShipModules;
      use Tiny_String;

      Module_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Ammo_Index: constant Natural :=
        (if Player_Ship.Modules(Module_Index).M_Type = GUN then
           Player_Ship.Modules(Module_Index).Ammo_Index
         else Player_Ship.Modules(Module_Index).Harpoon_Index);
      Ammo_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".ammomenu", Title => "Available ammo", Parent_Name => ".");
      Row: Positive := 1;
      procedure Add_Button(Name, Label, Command: String) is
         use Tcl.Tk.Ada.Widgets.TtkButton;

         Button: constant Ttk_Button :=
           Create
             (pathName => Ammo_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Ammo_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Ammo_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Ammo_Menu & ".ammo1;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Find_Ammo_Loop :
      for I in
        Inventory_Container.First_Index(Container => Player_Ship.Cargo) ..
          Inventory_Container.Last_Index(Container => Player_Ship.Cargo) loop
         if Get_Proto_Item
             (Index =>
                Inventory_Container.Element
                  (Container => Player_Ship.Cargo, Index => I)
                  .Proto_Index)
             .I_Type =
           Get_Ada_Item_Type
             (Item_Index =>
                Get_Module
                  (Index => Player_Ship.Modules(Module_Index).Proto_Index)
                  .Value -
                1) and
           I /= Ammo_Index then
            Add_Button
              (Name =>
                 ".ammo" & Trim(Source => Positive'Image(Row), Side => Left),
               Label =>
                 To_String
                   (Source =>
                      Get_Proto_Item
                        (Index =>
                           Inventory_Container.Element
                             (Container => Player_Ship.Cargo, Index => I)
                             .Proto_Index)
                        .Name),
               Command =>
                 "AssignModule ammo " & CArgv.Arg(Argv => Argv, N => 1) &
                 Positive'Image(I));
            Row := Row + 1;
         end if;
      end loop Find_Ammo_Loop;
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Ammo_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Assign_Ammo_Command;

   procedure Add_Modules_Commands is
      use Utils.UI;
      procedure Add_Ada_Commands with
         Import => True,
         Convention => C,
         External_Name => "addAdaModulesCommands";
   begin
      Add_Ada_Commands;
      Add_Command
        (Name => "ShowAssignAmmo",
         Ada_Command => Show_Assign_Ammo_Command'Access);
   end Add_Modules_Commands;

end Ships.UI.Modules;
