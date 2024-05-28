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
with Utils.UI;

package body Crafts.UI is

   -- ****o* CUI4/CUI4.Show_Crafting_Command
   -- FUNCTION
   -- Show information about available crafting recipes
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCrafting page recipename
   -- Page is the current page of recipes list to show, recipename is the
   -- text which will be searching in the recipes names. Can be empty, then
   -- show all recipes.
   -- SOURCE
   function Show_Crafting_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showCraftingCommand";
      -- ****

   -- ****o* CUI4/CUI4.Show_Set_Recipe_Command
   -- FUNCTION
   -- Show dialog to set the selected recipe as crafting order
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetRecipe index
   -- Index is the index of the recipe to craft.
   -- SOURCE
   function Show_Set_Recipe_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showSetRecipeCommand";
      -- ****

   -- ****o* CUI4/CUI4.Show_Recipe_Info_Command
   -- FUNCTION
   -- Show information about the selected recipe
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecipeInfo index cancraft
   -- Index is the index of the crafting recipe to show, cancraft if TRUE
   -- then recipe can be crafted (show craft button)
   -- SOURCE
   function Show_Recipe_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showRecipeInfoCommand";
      -- ****

   -- ****o* CUI4/CUI4.Set_Crafting_Command
   -- FUNCTION
   -- Set the selected recipe as a crafting order in the selected workshop
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCrafting index
   -- Index is the index of the crafting recipe to set
   -- SOURCE
   function Set_Crafting_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "setCraftingCommand";
      -- ****

   -- ****o* CUI4/CUI4.Sort_Crafting_Command
   -- FUNCTION
   -- Sort the list of crafting recipes
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortCrafting x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Crafting_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "sortCraftingCommand";
      -- ****

   procedure Add_Commands is
      use Utils.UI;
   begin
      Add_Command
        (Name => "ShowCrafting", Ada_Command => Show_Crafting_Command'Access);
      Add_Command
        (Name => "ShowSetRecipe",
         Ada_Command => Show_Set_Recipe_Command'Access);
      Add_Command
        (Name => "ShowRecipeInfo",
         Ada_Command => Show_Recipe_Info_Command'Access);
      Add_Command
        (Name => "SetCrafting", Ada_Command => Set_Crafting_Command'Access);
      Add_Command
        (Name => "SortCrafting", Ada_Command => Sort_Crafting_Command'Access);
   end Add_Commands;

end Crafts.UI;
