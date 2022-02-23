-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers.Generic_Array_Sort;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with CoreUI; use CoreUI;
with Crew; use Crew;
with Dialogs; use Dialogs;
with Items; use Items;
with Maps.UI; use Maps.UI;
with Ships.Crew; use Ships.Crew;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Crafts.UI is

   -- ****iv* CUI4/CUI4.RecipesTable
   -- FUNCTION
   -- Table with info about available crafting recipes
   -- SOURCE
   RecipesTable: Table_Widget (5);
   -- ****

   -- ****iv* CUI4/CUI4.Modules_Indexes
   -- FUNCTION
   -- Indexes of the player ship modules
   -- SOURCE
   Recipes_Indexes: UnboundedString_Container.Vector;
   -- ****

   -- ****iv* CUI4/CUI4.Studies
   -- FUNCTION
   -- The list of available study recipes
   -- SOURCE
   Studies: TinyString_Container.Vector;
   -- ****

   -- ****iv* CUI4/CUI4.Deconstructs
   -- FUNCTION
   -- The list of available deconstruct recipes
   -- SOURCE
   Deconstructs: TinyString_Container.Vector;
   -- ****

   -- ****if* CUI4/CUI4.CheckTool
   -- FUNCTION
   -- Check if the player has needed tool for the crafting recipe
   -- PARAMETERS
   -- ToolNeeded - The type of tool needed for the recipe
   -- RESULT
   -- True if the tool is in the player ship cargo, otherwise False
   -- SOURCE
   function CheckTool(ToolNeeded: Unbounded_String) return Boolean is
      -- ****
      CargoIndex: Natural;
      Has_Tool: Boolean := True;
   begin
      if ToolNeeded /= To_Unbounded_String("None") then
         Has_Tool := False;
         Check_Tool_Loop :
         for I in Items_List.Iterate loop
            if Items_List(I).I_Type = ToolNeeded then
               CargoIndex :=
                 Find_Item(Player_Ship.Cargo, Objects_Container.Key(I));
               if CargoIndex > 0 then
                  Has_Tool := True;
                  exit Check_Tool_Loop;
               end if;
            end if;
         end loop Check_Tool_Loop;
      end if;
      return Has_Tool;
   end CheckTool;

   -- ****if* CUI4/CUI4.Is_Craftable
   -- FUNCTION
   -- Check if the selected recipe can be crafted (has all requirements meet)
   -- PARAMETERS
   -- Recipe        - The crafting recipe to check
   -- CanCraft      - If recipe can be crafted, then it will be True, otherwise
   --                 False
   -- Has_Workplace - If there is workplace for the recipe, will be True,
   --                 otherwise False
   -- Has_Tool      - If there is available tool for the recipe, will be True,
   --                 otherwise False
   -- Has_Materials - If there are available materials for the recipe, will be
   --                 True, otherwise False
   -- OUTPUT
   -- Parameters CanCraft, Has_Workplace, Has_Tool and Has_Materials
   -- SOURCE
   procedure Is_Craftable
     (Recipe: Craft_Data;
      CanCraft, Has_Workplace, Has_Tool, Has_Materials: out Boolean) is
      -- ****
      CargoIndex: Natural;
   begin
      CanCraft := False;
      Has_Workplace := False;
      Find_Workshop_Loop :
      for Module of Player_Ship.Modules loop
         if Modules_List(Module.Proto_Index).M_Type = Recipe.Workplace
           and then Module.Durability > 0 then
            Has_Workplace := True;
            exit Find_Workshop_Loop;
         end if;
      end loop Find_Workshop_Loop;
      Has_Tool := CheckTool(Recipe.Tool);
      declare
         Materials: array
           (Recipe.Material_Types.First_Index ..
                Recipe.Material_Types.Last_Index) of Boolean :=
           (others => False);
      begin
         Find_Materials_Loop :
         for K in
           Recipe.Material_Types.First_Index ..
             Recipe.Material_Types.Last_Index loop
            Find_Cargo_Index_Loop :
            for J in Items_List.Iterate loop
               if Items_List(J).I_Type = Recipe.Material_Types(K) then
                  CargoIndex :=
                    Find_Item(Player_Ship.Cargo, Objects_Container.Key(J));
                  if CargoIndex > 0
                    and then Player_Ship.Cargo(CargoIndex).Amount >=
                      Recipe.Material_Amounts(K) then
                     Materials(K) := True;
                  end if;
               end if;
            end loop Find_Cargo_Index_Loop;
         end loop Find_Materials_Loop;
         Has_Materials := True;
         Set_Can_Craft_Loop :
         for J in Materials'Range loop
            if not Materials(J) then
               Has_Materials := False;
               exit Set_Can_Craft_Loop;
            end if;
         end loop Set_Can_Craft_Loop;
      end;
      if Has_Tool and Has_Materials and Has_Workplace then
         CanCraft := True;
      end if;
   end Is_Craftable;

   -- ****if* CUI4/CUI4.Check_Study_Prerequisites
   -- FUNCTION
   -- Check if the study and decontruct recipes can be crafted
   -- PARAMETERS
   -- CanCraft      - If recipe can be crafter then it will be True, otherwise
   --                 False
   -- Has_Tool      - If there is tool for the study and deconstruct recipes
   --                 then True, otherwise False
   -- Has_Workplace - If there is workplace for study and deconstruct recipes
   --                 then True, otherwise False
   -- OUTPUT
   -- Parameters CanCraft, Has_Tool and Has_Workplace
   -- SOURCE
   procedure Check_Study_Prerequisites
     (CanCraft, Has_Tool, Has_Workplace: out Boolean) is
     -- ****
   begin
      Has_Tool := CheckTool(Alchemy_Tools);
      CanCraft := False;
      Has_Workplace := False;
      Find_Alchemy_Lab_Loop :
      for Module of Player_Ship.Modules loop
         if Modules_List(Module.Proto_Index).M_Type = ALCHEMY_LAB
           and then Module.Durability > 0 then
            Has_Workplace := True;
            exit Find_Alchemy_Lab_Loop;
         end if;
      end loop Find_Alchemy_Lab_Loop;
      if Has_Workplace then
         CanCraft := True;
      end if;
   end Check_Study_Prerequisites;

   -- ****o* CUI4/CUI4.Show_Crafting_Command
   -- FUNCTION
   -- Show information about available crafting recipes
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCrafting page recipename
   -- Page is the current page of recipes list to show, recipename is the
   -- text which will be searching in the recipes names. Can be empty, then
   -- show all recipes.
   -- SOURCE
   function Show_Crafting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Crafting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      use Tiny_String;

      CraftsFrame: Ttk_Frame := Get_Widget(Main_Paned & ".craftframe", Interp);
      CraftsCanvas: constant Tk_Canvas :=
        Get_Widget(CraftsFrame & ".canvas", Interp);
      CanCraft, Has_Tool, Has_Workplace, Has_Materials: Boolean := True;
      Recipe: Craft_Data;
      Page: constant Positive :=
        (if Argc = 2 then Positive'Value(CArgv.Arg(Argv, 1)) else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      RecipeName: constant String :=
        (if Argc = 3 then CArgv.Arg(Argv, 2) else "");
      SearchEntry: constant Ttk_Entry :=
        Get_Widget(CraftsCanvas & ".craft.sframe.search");
   begin
      if Winfo_Get(CraftsCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "crafts.tcl");
         Bind(CraftsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(CraftsCanvas, "ismapped") = "1" and Argc = 1 then
         Tcl_Eval(Interp, "InvokeButton " & Close_Button);
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         return TCL_OK;
      end if;
      Tcl_SetVar(Interp, "gamestate", "crafts");
      if RecipeName'Length = 0 then
         configure(SearchEntry, "-validatecommand {}");
         Delete(SearchEntry, "0", "end");
         configure(SearchEntry, "-validatecommand {ShowCrafting 1 %P}");
      end if;
      Studies.Clear;
      Deconstructs.Clear;
      Find_Possible_Recipes_Loop :
      for Item of Player_Ship.Cargo loop
         Add_Recipes_Loop :
         for J in Recipes_List.Iterate loop
            if Recipes_List(J).Result_Index = Item.Proto_Index then
               if Known_Recipes.Find_Index(Item => Recipes_Container.Key(J)) =
                 Positive_Container.No_Index and
                 Studies.Find_Index(Item => Item.Proto_Index) =
                   Positive_Container.No_Index then
                  Studies.Append(New_Item => Item.Proto_Index);
               end if;
               if Recipes_List(J).Material_Amounts(1) > 1 and
                 Recipes_List(J).Result_Amount = 1 then
                  Deconstructs.Append(New_Item => Item.Proto_Index);
               end if;
            end if;
         end loop Add_Recipes_Loop;
      end loop Find_Possible_Recipes_Loop;
      if Recipes_Indexes.Length /=
        Known_Recipes.Length + Studies.Length + Deconstructs.Length then
         Recipes_Indexes.Clear;
         for I in Known_Recipes.Iterate loop
            Recipes_Indexes.Append(Known_Recipes(I));
         end loop;
         for I in Studies.Iterate loop
            Recipes_Indexes.Append
              (To_Unbounded_String(Source => To_String(Source => Studies(I))));
         end loop;
         for I in Deconstructs.Iterate loop
            Recipes_Indexes.Append
              (To_Unbounded_String
                 (Source => To_String(Source => Deconstructs(I))));
         end loop;
      end if;
      if RecipesTable.Row_Height = 1 then
         RecipesTable :=
           Create_Table
             (CraftsCanvas & ".craft",
              (To_Unbounded_String("Name"), To_Unbounded_String("Craftable"),
               To_Unbounded_String("Workshop"), To_Unbounded_String("Tools"),
               To_Unbounded_String("Materials")),
              Get_Widget(CraftsFrame & ".scrolly"), "SortCrafting",
              "Press mouse button to sort the crafting recipes.");
      else
         Clear_Table(RecipesTable);
      end if;
      Show_Recipes_Loop :
      for I in Recipes_Indexes.First_Index .. Recipes_Indexes.Last_Index loop
         exit Show_Recipes_Loop when I > Positive(Known_Recipes.Length);
         if RecipeName'Length > 0
           and then
             Index
               (To_Lower
                  (To_String
                     (Items_List(Recipes_List(Recipes_Indexes(I)).Result_Index)
                        .Name)),
                To_Lower(RecipeName), 1) =
             0 then
            goto End_Of_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         Recipe := Recipes_List(Recipes_Indexes(I));
         Is_Craftable
           (Recipe, CanCraft, Has_Workplace, Has_Tool, Has_Materials);
         Add_Button
           (RecipesTable,
            To_String
              (Items_List(Recipes_List(Recipes_Indexes(I)).Result_Index).Name),
            "Show available recipe's options",
            "ShowRecipeMenu {" & To_String(Recipes_Indexes(I)) & "} " &
            Boolean'Image(CanCraft),
            1);
         Add_Check_Button
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {" & To_String(Recipes_Indexes(I)) & "} " &
            Boolean'Image(CanCraft),
            CanCraft, 2);
         Add_Check_Button
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {" & To_String(Recipes_Indexes(I)) & "} " &
            Boolean'Image(CanCraft),
            Has_Workplace, 3);
         Add_Check_Button
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {" & To_String(Recipes_Indexes(I)) & "} " &
            Boolean'Image(CanCraft),
            Has_Tool, 4);
         Add_Check_Button
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {" & To_String(Recipes_Indexes(I)) & "} " &
            Boolean'Image(CanCraft),
            Has_Materials, 5, True);
         exit Show_Recipes_Loop when RecipesTable.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Loop>>
      end loop Show_Recipes_Loop;
      Check_Study_Prerequisites(CanCraft, Has_Tool, Has_Workplace);
      Set_Study_Recipes_Loop :
      for I in
        Positive(Known_Recipes.Length + 1) .. Recipes_Indexes.Last_Index loop
         exit Set_Study_Recipes_Loop when RecipesTable.Row =
           Game_Settings.Lists_Limit + 1 or
           I > Positive(Studies.Length);
         if RecipeName'Length > 0
           and then
             Index
               (To_Lower
                  ("Study " &
                   To_String
                     (Items_List
                        (To_Bounded_String
                           (Source => To_String(Source => Recipes_Indexes(I))))
                        .Name)),
                To_Lower(RecipeName), 1) =
             0 then
            goto End_Of_Study_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Study_Loop;
         end if;
         Add_Button
           (RecipesTable,
            "Study " &
            To_String
              (Items_List
                 (To_Bounded_String
                    (Source => To_String(Source => Recipes_Indexes(I))))
                 .Name),
            "Show available recipe's options",
            "ShowRecipeMenu {Study " & To_String(Recipes_Indexes(I)) & "} " &
            Boolean'Image(CanCraft),
            1);
         Add_Check_Button
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Study " & To_String(Recipes_Indexes(I)) & "} " &
            Boolean'Image(CanCraft),
            CanCraft, 2);
         Add_Check_Button
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Study " & To_String(Recipes_Indexes(I)) & "} " &
            Boolean'Image(CanCraft),
            Has_Workplace, 3);
         Add_Check_Button
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Study " & To_String(Recipes_Indexes(I)) & "} " &
            Boolean'Image(CanCraft),
            Has_Tool, 4, True);
         <<End_Of_Study_Loop>>
      end loop Set_Study_Recipes_Loop;
      Set_Deconstruct_Recipes_Loop :
      for I in
        Positive(Known_Recipes.Length + Studies.Length + 1) ..
          Recipes_Indexes.Last_Index loop
         exit Set_Deconstruct_Recipes_Loop when RecipesTable.Row =
           Game_Settings.Lists_Limit + 1;
         if RecipeName'Length > 0
           and then
             Index
               (To_Lower
                  ("Deconstruct " &
                   To_String
                     (Items_List
                        (To_Bounded_String
                           (Source => To_String(Source => Recipes_Indexes(I))))
                        .Name)),
                To_Lower(RecipeName), 1) =
             0 then
            goto End_Of_Deconstruct_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Deconstruct_Loop;
         end if;
         Add_Button
           (RecipesTable,
            "Decontruct " &
            To_String
              (Items_List
                 (To_Bounded_String
                    (Source => To_String(Source => Recipes_Indexes(I))))
                 .Name),
            "Show available recipe's options",
            "ShowRecipeMenu {Deconstruct " & To_String(Recipes_Indexes(I)) &
            "} " & Boolean'Image(CanCraft),
            1);
         Add_Check_Button
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Deconstruct " & To_String(Recipes_Indexes(I)) &
            "} " & Boolean'Image(CanCraft),
            CanCraft, 2);
         Add_Check_Button
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Deconstruct " & To_String(Recipes_Indexes(I)) &
            "} " & Boolean'Image(CanCraft),
            Has_Workplace, 3);
         Add_Check_Button
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Deconstruct " & To_String(Recipes_Indexes(I)) &
            "} " & Boolean'Image(CanCraft),
            Has_Tool, 4, True);
         <<End_Of_Deconstruct_Loop>>
      end loop Set_Deconstruct_Recipes_Loop;
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      if Page > 1 then
         if RecipesTable.Row < Game_Settings.Lists_Limit + 1 then
            Add_Pagination
              (RecipesTable,
               "ShowCrafting" & Positive'Image(Page - 1) &
               (if RecipeName'Length > 0 then " {" & RecipeName & "}" else ""),
               "");
         else
            Add_Pagination
              (RecipesTable,
               "ShowCrafting" & Positive'Image(Page - 1) &
               (if RecipeName'Length > 0 then " {" & RecipeName & "}" else ""),
               "ShowCrafting" & Positive'Image(Page + 1) &
               (if RecipeName'Length > 0 then " {" & RecipeName & "}"
                else ""));
         end if;
      elsif RecipesTable.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (RecipesTable, "",
            "ShowCrafting" & Positive'Image(Page + 1) &
            (if RecipeName'Length > 0 then " {" & RecipeName & "}" else ""));
      end if;
      Update_Table
        (RecipesTable, (if Focus = Widget_Image(SearchEntry) then False));
      CraftsFrame.Name := New_String(Widget_Image(CraftsCanvas) & ".craft");
      configure
        (CraftsCanvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (CraftsCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(CraftsFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (CraftsCanvas,
         "-scrollregion [list " & BBox(CraftsCanvas, "all") & "]");
      Show_Screen("craftframe");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Crafting_Command;

   -- ****o* CUI4/CUI4.Show_Recipe_Menu_Command
   -- FUNCTION
   -- Show menu with available actions for the selected recipe
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecipeMenu index craftable
   -- Index is the index of the recipe to craft. If craftable is TRUE,
   -- then the recipe can be crafted, otherwise FALSE
   -- SOURCE
   function Show_Recipe_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recipe_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Recipe_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".recipemenu", Title => "Actions", Parent_Name => ".");
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Recipe_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Recipe_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Recipe_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script =>
                 "{focus " & Recipe_Menu & "." &
                 (if CArgv.Arg(Argv, 2) = "TRUE" then "set" else "info") &
                 ";break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      if CArgv.Arg(Argv, 2) = "TRUE" then
         Add_Button
           (Name => ".set", Label => "Set crafting order",
            Command =>
              "ShowSetRecipe {" & CArgv.Arg(Argv => Argv, N => 1) & "}");
      end if;
      Add_Button
        (Name => ".info", Label => "Show more info about the recipe",
         Command =>
           "ShowRecipeInfo {" & CArgv.Arg(Argv => Argv, N => 1) & "} " &
           CArgv.Arg(Argv, 2));
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Recipe_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Recipe_Menu_Command;

   -- ****o* CUI4/CUI4.Show_Set_Recipe_Command
   -- FUNCTION
   -- Show dialog to set the selected recipe as crafting order
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetRecipe index
   -- Index is the index of the recipe to craft.
   -- SOURCE
   function Show_Set_Recipe_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Set_Recipe_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      use Tiny_String;

      MType: Module_Type;
      ModulesList, CrewList: Unbounded_String;
      RecipeIndex: constant Unbounded_String :=
        To_Unbounded_String(CArgv.Arg(Argv, 1));
      Recipe: constant Craft_Data := Set_Recipe_Data(RecipeIndex);
      RecipeLength: constant Positive := Length(RecipeIndex);
      RecipeType: constant String :=
        (if RecipeLength > 6 and then Slice(RecipeIndex, 1, 5) = "Study" then
           "Study"
         elsif RecipeLength > 6 and then Slice(RecipeIndex, 1, 5) = "Decon"
         then "Deconstruct"
         else "Craft");
      CraftDialog: constant Ttk_Frame :=
        Create_Dialog
          (".craftdialog",
           RecipeType & " " &
           (if RecipeType = "Study" then
              To_String
                (Items_List
                   (To_Bounded_String
                      (Source => Slice(RecipeIndex, 7, RecipeLength)))
                   .Name)
            elsif RecipeType = "Deconstruct" then
              To_String
                (Items_List
                   (To_Bounded_String
                      (Source => Slice(RecipeIndex, 13, RecipeLength)))
                   .Name)
            else To_String
                (Items_List(Recipes_List(RecipeIndex).Result_Index).Name)),
           275, 2);
      MaxAmount: constant Positive := Check_Recipe(RecipeIndex);
      Label: Ttk_Label :=
        Create(CraftDialog & ".amountlabel", "-text {Amount:}");
      ModulesBox: constant Ttk_ComboBox :=
        Create(CraftDialog & ".workshop", "-state readonly");
      AmountBox: constant Ttk_SpinBox :=
        Create
          (CraftDialog & ".amount",
           "-to" & Positive'Image(MaxAmount) &
           " -validatecommand {ValidateSpinbox %W %P} -width 20");
      Button: Ttk_Button :=
        Create
          (CraftDialog & ".maxamount",
           "-text {max" & Positive'Image(MaxAmount) & "} -command {" &
           AmountBox & " set" & Positive'Image(MaxAmount) & "}");
      ButtonRow: Positive := 1;
      Modules_Amount: Natural := 0;
      Crafter_Button: Ttk_RadioButton :=
        Create
          (CraftDialog & ".noworker",
           "-text {Don't assign anyone} -variable craftworker -value noone");
      CrewBox: constant Ttk_ComboBox :=
        Create(CraftDialog & ".members", "-state readonly");
      FirstFocus: Unbounded_String := Null_Unbounded_String;
   begin
      Set(AmountBox, "1");
      Tcl_SetVar(Interp, "craftworker", "noone");
      if RecipeType /= "Study" then
         if MaxAmount > 1 then
            Tcl.Tk.Ada.Grid.Grid(Label);
            Tcl.Tk.Ada.Grid.Grid(Button, "-row 1 -column 1 -padx {0 5}");
            Add
              (Button,
               "Set maximum possible amount of how many times\nthe crafting order should be done.");
            Bind(Button, "<Tab>", "{focus " & AmountBox & ";break}");
            Bind
              (Button, "<Escape>",
               "{" & CraftDialog & ".cancel invoke;break}");
            FirstFocus := To_Unbounded_String(".maxamount");
         else
            Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2");
         end if;
         Tcl.Tk.Ada.Grid.Grid(AmountBox, "-columnspan 2 -padx 5");
         Add
           (AmountBox,
            "Set amount of how many times the crafting order\nshould be done.");
         Bind
           (AmountBox, "<Tab>", "{focus " & CraftDialog & ".noworker;break}");
         Bind
           (AmountBox, "<Escape>",
            "{" & CraftDialog & ".cancel invoke;break}");
         if FirstFocus = Null_Unbounded_String then
            FirstFocus := To_Unbounded_String(".amount");
         end if;
         ButtonRow := ButtonRow + 2;
      end if;
      if RecipeType in "Study" | "Deconstruct" then
         MType := ALCHEMY_LAB;
      else
         MType := Recipes_List(RecipeIndex).Workplace;
      end if;
      Show_Workshops_List_Loop :
      for Module of Player_Ship.Modules loop
         if Modules_List(Module.Proto_Index).M_Type = MType then
            Append(ModulesList, " {" & Module.Name & "}");
            Modules_Amount := Modules_Amount + 1;
         end if;
      end loop Show_Workshops_List_Loop;
      configure(ModulesBox, "-values [list" & To_String(ModulesList) & "]");
      Current(ModulesBox, "0");
      if Modules_Amount > 1 then
         Label := Create(CraftDialog & ".workshoplabel", "-text {Wokshop:}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2 -padx 5");
         Tcl.Tk.Ada.Grid.Grid(ModulesBox, "-columnspan 2 -padx 5");
         Bind
           (ModulesBox, "<Escape>",
            "{" & CraftDialog & ".cancel invoke;break}");
         ButtonRow := ButtonRow + 2;
         if FirstFocus = Null_Unbounded_String then
            FirstFocus := To_Unbounded_String(".workshop");
         end if;
      end if;
      Tcl.Tk.Ada.Grid.Grid(Crafter_Button, "-columnspan 2 -padx 5 -sticky w");
      Add
        (Crafter_Button,
         "Don't assign anyone to the order. You can\nmanually do it later, in ship info screen.");
      Bind
        (Crafter_Button, "<Tab>",
         "{focus " & CraftDialog & ".bestworker;break}");
      Bind
        (Crafter_Button, "<Escape>",
         "{" & CraftDialog & ".cancel invoke;break}");
      if FirstFocus = Null_Unbounded_String then
         FirstFocus := To_Unbounded_String(".noworker");
      end if;
      Crafter_Button :=
        Create
          (CraftDialog & ".bestworker",
           "-text {Assign the best worker} -variable craftworker -value best");
      Tcl.Tk.Ada.Grid.Grid(Crafter_Button, "-columnspan 2 -padx 5 -sticky w");
      Add
        (Crafter_Button,
         "Assign the crew member with the highest skill\nneeded for the recipe, even if the crew member\nis busy.");
      Bind
        (Crafter_Button, "<Escape>",
         "{" & CraftDialog & ".cancel invoke;break}");
      Crafter_Button :=
        Create
          (CraftDialog & ".selectedworker",
           "-text {Assign selected member} -variable craftworker -value fromlist");
      Tcl.Tk.Ada.Grid.Grid(Crafter_Button, "-columnspan 2 -padx 5 -sticky w");
      Add
        (Crafter_Button,
         "Assign the crew member from the list.\nThe sign + after name means that this crew member has\nneeded skill, the sign ++ after name means that his/her\nneeded skill is the best in the crew.");
      Bind(Crafter_Button, "<Tab>", "{focus " & CrewBox & ";break}");
      Bind
        (Crafter_Button, "<Escape>",
         "{" & CraftDialog & ".cancel invoke;break}");
      Show_Members_List_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Append
           (CrewList,
            " {" & To_String(Source => Player_Ship.Crew(I).Name) &
            Get_Skill_Marks(Recipe.Skill, Crew_Container.To_Index(I)) & "}");
      end loop Show_Members_List_Loop;
      configure(CrewBox, "-values [list" & To_String(CrewList) & "]");
      Current(CrewBox, "0");
      Tcl.Tk.Ada.Grid.Grid(CrewBox, "-columnspan 2 -padx 5");
      Add
        (CrewBox,
         "Assign the crew member from the list.\nThe sign + after name means that this crew member has\nneeded skill, the sign ++ after name means that his/her\nneeded skill is the best in the crew.");
      Bind(CrewBox, "<Tab>", "{focus " & CraftDialog & ".craft;break}");
      Bind(CrewBox, "<Escape>", "{" & CraftDialog & ".cancel invoke;break}");
      ButtonRow := ButtonRow + 4;
      Button :=
        Create
          (CraftDialog & ".craft",
           "-text {" & RecipeType & "} -command {SetCrafting {" &
           CArgv.Arg(Argv, 1) & "};CloseDialog " & CraftDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-pady 5 -padx 5");
      Add(Button, "Set the crafting order.");
      Bind(Button, "<Escape>", "{" & CraftDialog & ".cancel invoke;break}");
      Button :=
        Create
          (CraftDialog & ".cancel",
           "-text {Cancel} -command {CloseDialog " & CraftDialog & "}");
      Tcl.Tk.Ada.Grid.Grid
        (Button, "-pady 5 -padx 5 -column 1 -row" & Positive'Image(ButtonRow));
      Add(Button, "Cancel setting the order and close dialog.");
      Bind
        (Button, "<Tab>",
         "{focus " & CraftDialog & To_String(FirstFocus) & ";break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      Show_Dialog(CraftDialog);
      Focus(Button);
      return TCL_OK;
   end Show_Set_Recipe_Command;

   -- ****o* CUI4/CUI4.Show_Recipe_Info_Command
   -- FUNCTION
   -- Show information about the selected recipe
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecipeInfo index cancraft
   -- Index is the index of the crafting recipe to show, cancraft if TRUE
   -- then recipe can be crafted (show craft button)
   -- SOURCE
   function Show_Recipe_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recipe_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      use Tiny_String;

      RecipeIndex: constant Unbounded_String :=
        To_Unbounded_String(CArgv.Arg(Argv, 1));
      RecipeLength: constant Positive := Length(RecipeIndex);
      RecipeType: constant String :=
        (if RecipeLength > 6 and then Slice(RecipeIndex, 1, 5) = "Study" then
           "Study"
         elsif RecipeLength > 6 and then Slice(RecipeIndex, 1, 5) = "Decon"
         then "Deconstruct"
         else "Craft");
      RecipeDialog: constant Ttk_Frame :=
        Create_Dialog
          (".recipedialog",
           (if RecipeType = "Study" then
              "Study " &
              To_String
                (Items_List
                   (To_Bounded_String
                      (Source => Slice(RecipeIndex, 7, RecipeLength)))
                   .Name)
            elsif RecipeType = "Deconstruct" then
              "Deconstruct " &
              To_String
                (Items_List
                   (To_Bounded_String
                      (Source => Slice(RecipeIndex, 13, RecipeLength)))
                   .Name)
            else "Craft " &
              To_String
                (Items_List(Recipes_List(RecipeIndex).Result_Index).Name)),
           275);
      WorkplaceName: Unbounded_String := Null_Unbounded_String;
      Recipe: Craft_Data;
      MAmount, CargoIndex: Natural := 0;
      HaveWorkplace, IsMaterial: Boolean := True;
      HaveTool: Boolean := False;
      TextLength: Positive;
      RecipeText: constant Tk_Text :=
        Create
          (RecipeDialog & ".text", "-wrap char -height 15 -width 40", Interp);
   begin
      Tag_Configure(RecipeText, "red", "-foreground red");
      if RecipeType = "Study" then
         Recipe.Material_Types.Append
           (New_Item =>
              Items_List
                (To_Bounded_String
                   (Source => Slice(RecipeIndex, 7, Length(RecipeIndex))))
                .I_Type);
         Recipe.Result_Index :=
           To_Bounded_String
             (Source => Slice(RecipeIndex, 7, Length(RecipeIndex)));
         Recipe.Material_Amounts.Append(New_Item => 1);
         Recipe.Result_Amount := 0;
         Recipe.Workplace := ALCHEMY_LAB;
         Set_Study_Recipe_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.Result_Index = Recipe.Result_Index then
               Recipe.Skill := ProtoRecipe.Skill;
               Recipe.Time := ProtoRecipe.Difficulty * 15;
               exit Set_Study_Recipe_Loop;
            end if;
         end loop Set_Study_Recipe_Loop;
         Recipe.Difficulty := 1;
         Recipe.Tool := Alchemy_Tools;
         Recipe.Tool_Quality := 100;
      elsif RecipeType = "Deconstruct" then
         Recipe.Material_Types.Append
           (New_Item =>
              Items_List
                (To_Bounded_String
                   (Source => Slice(RecipeIndex, 13, Length(RecipeIndex))))
                .I_Type);
         Recipe.Result_Index :=
           To_Bounded_String
             (Source => Slice(RecipeIndex, 13, Length(RecipeIndex)));
         Recipe.Material_Amounts.Append(New_Item => 1);
         Recipe.Result_Amount := 0;
         Recipe.Workplace := ALCHEMY_LAB;
         Set_Deconstruct_Recipe_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.Result_Index = Recipe.Result_Index then
               Recipe.Skill := ProtoRecipe.Skill;
               Recipe.Time := ProtoRecipe.Difficulty * 15;
               Recipe.Difficulty := ProtoRecipe.Difficulty;
               Recipe.Result_Index :=
                 Find_Proto_Item(ProtoRecipe.Material_Types(1));
               Recipe.Result_Amount :=
                 Positive
                   (Float'Ceiling
                      (Float(ProtoRecipe.Material_Amounts.Element(1)) * 0.8));
               exit Set_Deconstruct_Recipe_Loop;
            end if;
         end loop Set_Deconstruct_Recipe_Loop;
         Recipe.Tool := Alchemy_Tools;
         Recipe.Tool_Quality := 100;
      else
         Recipe := Recipes_List(RecipeIndex);
         Insert
           (RecipeText, "end",
            "{Amount:" & Integer'Image(Recipe.Result_Amount) & LF & "}");
      end if;
      Insert(RecipeText, "end", "{Materials needed: }");
      Check_Materials_Loop :
      for I in
        Recipe.Material_Types.First_Index ..
          Recipe.Material_Types.Last_Index loop
         Insert(RecipeText, "end", "{" & LF & "-}");
         MAmount := 0;
         Find_Materials_Loop :
         for J in Items_List.Iterate loop
            IsMaterial := False;
            if Length(RecipeIndex) > 6
              and then Slice(RecipeIndex, 1, 5) = "Study" then
               if Items_List(J).Name =
                 Items_List(Recipe.Result_Index).Name then
                  IsMaterial := True;
               end if;
            elsif Length(RecipeIndex) > 12
              and then Slice(RecipeIndex, 1, 11) = "Deconstruct" then
               if Objects_Container.Key(J) =
                 To_Bounded_String
                   (Source => Slice(RecipeIndex, 13, Length(RecipeIndex))) then
                  IsMaterial := True;
               end if;
            else
               if Items_List(J).I_Type = Recipe.Material_Types(I) then
                  IsMaterial := True;
               end if;
            end if;
            if IsMaterial then
               if MAmount > 0 then
                  Insert(RecipeText, "end", "{ or}");
               end if;
               CargoIndex :=
                 Find_Item(Player_Ship.Cargo, Objects_Container.Key(J));
               if CargoIndex > 0
                 and then Player_Ship.Cargo(CargoIndex).Amount >=
                   Recipe.Material_Amounts(I) then
                  TextLength :=
                    Positive'Image(Player_Ship.Cargo(CargoIndex).Amount)'
                      Length;
                  Insert
                    (RecipeText, "end",
                     "{" & Integer'Image(Recipe.Material_Amounts(I)) & "x" &
                     To_String(Items_List(J).Name) & "(owned: " &
                     Positive'Image(Player_Ship.Cargo(CargoIndex).Amount)
                       (2 .. TextLength) &
                     ")}");
               else
                  Insert
                    (RecipeText, "end",
                     "{" & Integer'Image(Recipe.Material_Amounts(I)) & "x" &
                     To_String(Items_List(J).Name) & "} [list red]");
               end if;
               MAmount := MAmount + 1;
            end if;
         end loop Find_Materials_Loop;
      end loop Check_Materials_Loop;
      if Recipe.Tool /= To_Unbounded_String("None") then
         Insert(RecipeText, "end", "{" & LF & "Tool: }");
         MAmount := 0;
         Check_Tool_Loop :
         for I in Items_List.Iterate loop
            HaveTool := False;
            if Items_List(I).I_Type = Recipe.Tool
              and then
              (Items_List(I).Value.Length > 0
               and then Items_List(I).Value(1) <= Recipe.Tool_Quality) then
               if MAmount > 0 then
                  Insert(RecipeText, "end", "{ or }");
               end if;
               CargoIndex :=
                 Find_Item
                   (Inventory => Player_Ship.Cargo,
                    Proto_Index => Objects_Container.Key(I),
                    Quality => Recipe.Tool_Quality);
               if CargoIndex > 0 then
                  HaveTool := True;
               end if;
               Insert
                 (RecipeText, "end",
                  "{" & To_String(Items_List(I).Name) & "}" &
                  (if not HaveTool then " [list red]" else ""));
               MAmount := MAmount + 1;
            end if;
         end loop Check_Tool_Loop;
      else
         HaveTool := True;
      end if;
      Insert(RecipeText, "end", "{" & LF & "Workplace: }");
      HaveWorkplace := False;
      Have_Workplace_Loop :
      for Module of Player_Ship.Modules loop
         if Modules_List(Module.Proto_Index).M_Type = Recipe.Workplace then
            WorkplaceName := Module.Name;
            if Module.Durability > 0 then
               HaveWorkplace := True;
               exit Have_Workplace_Loop;
            end if;
         end if;
      end loop Have_Workplace_Loop;
      if WorkplaceName = Null_Unbounded_String then
         Find_Workshop_Name_Loop :
         for I in Modules_List.Iterate loop
            if Modules_List(I).M_Type = Recipe.Workplace then
               WorkplaceName :=
                 To_Unbounded_String
                   (Get_Module_Type(BaseModules_Container.Key(I)));
               exit Find_Workshop_Name_Loop;
            end if;
         end loop Find_Workshop_Name_Loop;
      end if;
      Insert
        (RecipeText, "end",
         "{" & To_String(WorkplaceName) & "}" &
         (if not HaveWorkplace then " [list red]" else ""));
      Insert
        (RecipeText, "end",
         "{" & LF & "Skill: " &
         To_String
           (SkillsData_Container.Element(Skills_List, Recipe.Skill).Name) &
         "/" &
         To_String
           (AttributesData_Container.Element
              (Attributes_List,
               SkillsData_Container.Element(Skills_List, Recipe.Skill)
                 .Attribute)
              .Name) &
         LF & "Time needed:" & Positive'Image(Recipe.Time) & " minutes}");
      configure(RecipeText, "-state disabled");
      Tcl.Tk.Ada.Grid.Grid(RecipeText, "-padx 5");
      if CArgv.Arg(Argv, 2) = "TRUE" then
         declare
            ButtonBox: constant Ttk_Frame := Create(RecipeDialog & ".buttons");
            Button: Ttk_Button;
         begin
            Button :=
              Create
                (ButtonBox & ".craft",
                 "-text " & RecipeType & " -command {ShowSetRecipe {" &
                 CArgv.Arg(Argv, 1) & "};CloseDialog " & RecipeDialog & "}");
            Tcl.Tk.Ada.Grid.Grid(Button);
            Bind(Button, "<Escape>", "{" & ButtonBox & ".close invoke;break}");
            Button :=
              Create
                (ButtonBox & ".close",
                 "-text Close -command {CloseDialog " & RecipeDialog & "}");
            Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 1 -padx {5 0}");
            Focus(Button);
            Bind(Button, "<Tab>", "{focus " & ButtonBox & ".craft;break}");
            Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
            Tcl.Tk.Ada.Grid.Grid(ButtonBox, "-pady 5");
         end;
      else
         Add_Close_Button
           (RecipeDialog & ".close", "Close", "CloseDialog " & RecipeDialog);
      end if;
      Show_Dialog
        (Dialog => RecipeDialog, Relative_X => 0.2, Relative_Y => 0.1);
      return TCL_OK;
   end Show_Recipe_Info_Command;

   -- ****o* CUI4/CUI4.Set_Crafting_Command
   -- FUNCTION
   -- Set the selected recipe as a crafting order in the selected workshop
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCrafting index
   -- Index is the index of the crafting recipe to set
   -- SOURCE
   function Set_Crafting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Crafting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      RecipeIndex: Unbounded_String := To_Unbounded_String(CArgv.Arg(Argv, 1));
      ModulesBox: constant Ttk_ComboBox := Get_Widget(".craftdialog.workshop");
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".craftdialog.amount", Interp);
      MembersBox: constant Ttk_ComboBox :=
        Get_Widget(".craftdialog.members", Interp);
      AssignWorker: constant String := Tcl_GetVar(Interp, "craftworker");
   begin
      if Element(RecipeIndex, 1) = '{' then
         RecipeIndex :=
           Unbounded_Slice(RecipeIndex, 2, Length(RecipeIndex) - 1);
      end if;
      Set_Module_Loop :
      for I in Player_Ship.Modules.Iterate loop
         if Player_Ship.Modules(I).Name =
           To_Unbounded_String(Get(ModulesBox)) then
            Set_Recipe
              (Modules_Container.To_Index(I), Positive'Value(Get(AmountBox)),
               RecipeIndex);
            if AssignWorker = "fromlist" then
               Give_Orders
                 (Player_Ship, Positive'Value(Current(MembersBox)) + 1, CRAFT,
                  Modules_Container.To_Index(I));
            elsif AssignWorker = "best" then
               declare
                  Recipe: constant Craft_Data := Set_Recipe_Data(RecipeIndex);
                  WorkerAssigned: Boolean := False;
               begin
                  Set_Best_Worker_Loop :
                  for J in Player_Ship.Crew.Iterate loop
                     if Get_Skill_Marks
                         (Recipe.Skill, Crew_Container.To_Index(J)) =
                       " ++" then
                        Give_Orders
                          (Player_Ship, Crew_Container.To_Index(J), CRAFT,
                           Modules_Container.To_Index(I));
                        WorkerAssigned := True;
                        exit Set_Best_Worker_Loop;
                     end if;
                  end loop Set_Best_Worker_Loop;
                  if not WorkerAssigned then
                     Give_Orders
                       (Player_Ship, 1, CRAFT, Modules_Container.To_Index(I));
                  end if;
               end;
            end if;
            Update_Header;
            Update_Messages;
            exit Set_Module_Loop;
         end if;
      end loop Set_Module_Loop;
      return TCL_OK;
   end Set_Crafting_Command;

   -- ****it* CUI4/CUI4.Recipes_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the crafting recipes list
   -- OPTIONS
   -- NAMEASC       - Sort recipes by name ascending
   -- NAMEDESC      - Sort recipes by name descending
   -- CRAFTABLEASC  - Sort recipes by craftable ascending
   -- CRAFTABLEDESC - Sort recipes by craftable descending
   -- WORKPLACEASC  - Sort recipes by workshop state ascending
   -- WORKPLACEDESC - Sort recipes by workshop state descending
   -- TOOLSASC      - Sort recipes by available tool ascending
   -- TOOLSDESC     - Sort recipes by available tool descending
   -- MATERIALSASC  - Sort recipes by available materials ascending
   -- MATERIALSDESC - Sort recipes by available materials descending
   -- NONE          - No sorting recipes (default)
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   type Recipes_Sort_Orders is
     (NAMEASC, NAMEDESC, CRAFTABLEASC, CRAFTABLEDESC, WORKPLACEASC,
      WORKPLACEDESC, TOOLSASC, TOOLSDESC, MATERIALSASC, MATERIALSDESC,
      NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* CUI4/CUI4.Default_Recipes_Sort_Order
      -- FUNCTION
      -- Default sorting order for the crafting recipes
      -- HISTORY
      -- 6.5 - Added
      -- SOURCE
   Default_Recipes_Sort_Order: constant Recipes_Sort_Orders := NONE;
   -- ****

   -- ****iv* CUI4/CUI4.Recipes_Sort_Order
   -- FUNCTION
   -- The current sorting order for crafting recipes list
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Recipes_Sort_Order: Recipes_Sort_Orders := Default_Recipes_Sort_Order;
   -- ****

   -- ****o* CUI4/CUI4.Sort_Crafting_Command
   -- FUNCTION
   -- Sort the list of crafting recipes
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortCrafting x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Crafting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Crafting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      Column: constant Positive :=
        Get_Column_Number(RecipesTable, Natural'Value(CArgv.Arg(Argv, 1)));
      type Local_Module_Data is record
         Name: Unbounded_String;
         Craftable: Boolean;
         Workplace: Boolean;
         Tool: Boolean;
         Materials: Boolean;
         Id: Unbounded_String;
      end record;
      type Recipes_Array is array(Positive range <>) of Local_Module_Data;
      Can_Craft, Has_Tool, Has_Materials, Has_Workplace: Boolean;
      function "<"(Left, Right: Local_Module_Data) return Boolean is
      begin
         if Recipes_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Recipes_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Recipes_Sort_Order = CRAFTABLEASC
           and then Left.Craftable < Right.Craftable then
            return True;
         end if;
         if Recipes_Sort_Order = CRAFTABLEDESC
           and then Left.Craftable > Right.Craftable then
            return True;
         end if;
         if Recipes_Sort_Order = WORKPLACEASC
           and then Left.Workplace < Right.Workplace then
            return True;
         end if;
         if Recipes_Sort_Order = WORKPLACEDESC
           and then Left.Workplace > Right.Workplace then
            return True;
         end if;
         if Recipes_Sort_Order = TOOLSASC and then Left.Tool < Right.Tool then
            return True;
         end if;
         if Recipes_Sort_Order = TOOLSDESC and then Left.Tool > Right.Tool then
            return True;
         end if;
         if Recipes_Sort_Order = MATERIALSASC
           and then Left.Materials < Right.Materials then
            return True;
         end if;
         if Recipes_Sort_Order = MATERIALSDESC
           and then Left.Materials > Right.Materials then
            return True;
         end if;
         return False;
      end "<";
   begin
      case Column is
         when 1 =>
            if Recipes_Sort_Order = NAMEASC then
               Recipes_Sort_Order := NAMEDESC;
            else
               Recipes_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Recipes_Sort_Order = CRAFTABLEASC then
               Recipes_Sort_Order := CRAFTABLEDESC;
            else
               Recipes_Sort_Order := CRAFTABLEASC;
            end if;
         when 3 =>
            if Recipes_Sort_Order = WORKPLACEASC then
               Recipes_Sort_Order := WORKPLACEDESC;
            else
               Recipes_Sort_Order := WORKPLACEASC;
            end if;
         when 4 =>
            if Recipes_Sort_Order = TOOLSASC then
               Recipes_Sort_Order := TOOLSDESC;
            else
               Recipes_Sort_Order := TOOLSASC;
            end if;
         when 5 =>
            if Recipes_Sort_Order = MATERIALSASC then
               Recipes_Sort_Order := MATERIALSDESC;
            else
               Recipes_Sort_Order := MATERIALSASC;
            end if;
         when others =>
            null;
      end case;
      if Recipes_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Sort_Known_Recipes_Block :
      declare
         Local_Recipes: Recipes_Array(1 .. Positive(Known_Recipes.Length));
         procedure Sort_Recipes is new Ada.Containers.Generic_Array_Sort
           (Index_Type => Positive, Element_Type => Local_Module_Data,
            Array_Type => Recipes_Array);
      begin
         for I in Known_Recipes.Iterate loop
            Is_Craftable
              (Recipes_List(Known_Recipes(I)), Can_Craft, Has_Workplace,
               Has_Tool, Has_Materials);
            Local_Recipes(UnboundedString_Container.To_Index(I)) :=
              (Name =>
                 Items_List(Recipes_List(Known_Recipes(I)).Result_Index).Name,
               Craftable => Can_Craft, Workplace => Has_Workplace,
               Tool => Has_Tool, Materials => Has_Materials,
               Id => Known_Recipes(I));
         end loop;
         Sort_Recipes(Local_Recipes);
         Recipes_Indexes.Clear;
         for Recipe of Local_Recipes loop
            Recipes_Indexes.Append(Recipe.Id);
         end loop;
      end Sort_Known_Recipes_Block;
      Check_Study_Prerequisites(Can_Craft, Has_Tool, Has_Workplace);
      Sort_Studying_Recipes_Block :
      declare
         Local_Recipes: Recipes_Array(1 .. Positive(Studies.Length));
         procedure Sort_Recipes is new Ada.Containers.Generic_Array_Sort
           (Index_Type => Positive, Element_Type => Local_Module_Data,
            Array_Type => Recipes_Array);
      begin
         for I in Studies.Iterate loop
            Local_Recipes(TinyString_Container.To_Index(I)) :=
              (Name => Items_List(Studies(I)).Name, Craftable => Can_Craft,
               Tool => Has_Tool, Workplace => Has_Workplace, Materials => True,
               Id =>
                 To_Unbounded_String
                   (Source => Tiny_String.To_String(Source => Studies(I))));
         end loop;
         Sort_Recipes(Local_Recipes);
         for Recipe of Local_Recipes loop
            Recipes_Indexes.Append(Recipe.Id);
         end loop;
      end Sort_Studying_Recipes_Block;
      Sort_Deconstruct_Recipes_Block :
      declare
         Local_Recipes: Recipes_Array(1 .. Positive(Deconstructs.Length));
         procedure Sort_Recipes is new Ada.Containers.Generic_Array_Sort
           (Index_Type => Positive, Element_Type => Local_Module_Data,
            Array_Type => Recipes_Array);
      begin
         for I in Deconstructs.Iterate loop
            Local_Recipes(TinyString_Container.To_Index(I)) :=
              (Name => Items_List(Deconstructs(I)).Name,
               Craftable => Can_Craft, Workplace => Has_Workplace,
               Tool => Has_Tool, Materials => True,
               Id =>
                 To_Unbounded_String
                   (Source =>
                      Tiny_String.To_String(Source => Deconstructs(I))));
         end loop;
         Sort_Recipes(Local_Recipes);
         for Recipe of Local_Recipes loop
            Recipes_Indexes.Append(Recipe.Id);
         end loop;
      end Sort_Deconstruct_Recipes_Block;
      return
        Show_Crafting_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowCrafting" & "1");
   end Sort_Crafting_Command;

   procedure AddCommands is
   begin
      Add_Command("ShowCrafting", Show_Crafting_Command'Access);
      Add_Command("ShowRecipeMenu", Show_Recipe_Menu_Command'Access);
      Add_Command("ShowSetRecipe", Show_Set_Recipe_Command'Access);
      Add_Command("ShowRecipeInfo", Show_Recipe_Info_Command'Access);
      Add_Command("SetCrafting", Set_Crafting_Command'Access);
      Add_Command("SortCrafting", Sort_Crafting_Command'Access);
   end AddCommands;

end Crafts.UI;
