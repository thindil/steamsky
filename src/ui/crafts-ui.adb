-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with CoreUI; use CoreUI;
with Items; use Items;
with Maps.UI; use Maps.UI;
with Table; use Table;
with Trades; use Trades;
with Utils.UI; use Utils.UI;

package body Crafts.UI is

   -- ****iv* CUI4/CUI$.BasesTable
   -- FUNCTION
   -- Table with info about available crafting recipes
   -- SOURCE
   RecipesTable: Table_Widget (5);
   -- ****

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
   -- ShowCrafting page
   -- Page is the current page of recipes list to show
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
      CraftsFrame: Ttk_Frame := Get_Widget(Main_Paned & ".craftframe", Interp);
      CraftsCanvas: constant Tk_Canvas :=
        Get_Widget(CraftsFrame & ".canvas", Interp);
      Studies, Deconstructs: UnboundedString_Container.Vector;
      CanCraft, Has_Tool, Has_Workplace, Has_Materials: Boolean := True;
      Recipe: Craft_Data;
      CargoIndex: Natural;
      RecipesView: constant Ttk_Tree_View :=
        Get_Widget(CraftsCanvas & ".craft.list.view", Interp);
      Row: Positive := 2;
      Page: constant Positive :=
        (if Argc = 2 then Positive'Value(CArgv.Arg(Argv, 1)) else 1);
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
      Current_Row: Positive := 1;
      procedure CheckTool(ToolNeeded: Unbounded_String) is
      begin
         if ToolNeeded /= To_Unbounded_String("None") then
            Has_Tool := False;
            Check_Tool_Loop :
            for I in Items_List.Iterate loop
               if Items_List(I).IType = ToolNeeded then
                  CargoIndex :=
                    FindItem(PlayerShip.Cargo, Objects_Container.Key(I));
                  if CargoIndex > 0 then
                     Has_Tool := True;
                     exit Check_Tool_Loop;
                  end if;
               end if;
            end loop Check_Tool_Loop;
         end if;
      end CheckTool;
   begin
      if Winfo_Get(CraftsCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "crafts.tcl");
         Bind(CraftsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(CraftsCanvas, "ismapped") = "1" and Argc = 1 then
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         Tcl_Eval(Interp, "InvokeButton " & Close_Button);
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp craft}");
      Find_Possible_Recipes_Loop :
      for Item of PlayerShip.Cargo loop
         Add_Recipes_Loop :
         for J in Recipes_List.Iterate loop
            if Recipes_List(J).ResultIndex = Item.ProtoIndex then
               if Known_Recipes.Find_Index(Item => Recipes_Container.Key(J)) =
                 Positive_Container.No_Index and
                 Studies.Find_Index(Item => Item.ProtoIndex) =
                   Positive_Container.No_Index then
                  Studies.Append(New_Item => Item.ProtoIndex);
               end if;
               if Recipes_List(J).MaterialAmounts(1) > 1 and
                 Recipes_List(J).ResultAmount = 1 then
                  Deconstructs.Append(New_Item => Item.ProtoIndex);
               end if;
            end if;
         end loop Add_Recipes_Loop;
      end loop Find_Possible_Recipes_Loop;
      if RecipesTable.Row_Height = 1 then
         RecipesTable :=
           CreateTable
             (CraftsCanvas & ".craft",
              (To_Unbounded_String("Name"), To_Unbounded_String("Craftable"),
               To_Unbounded_String("Workshop"), To_Unbounded_String("Tools"),
               To_Unbounded_String("Materials")),
              Get_Widget(CraftsFrame & ".scrolly"));
      end if;
      Delete(RecipesView, "[list " & Children(RecipesView, "{}") & "]");
      Show_Recipes_Loop :
      for I in Known_Recipes.Iterate loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         CanCraft := False;
         Has_Workplace := False;
         Recipe := Recipes_List(Known_Recipes(I));
         Find_Workshop_Loop :
         for Module of PlayerShip.Modules loop
            if Modules_List(Module.ProtoIndex).MType = Recipe.Workplace
              and then Module.Durability > 0 then
               Has_Workplace := True;
               exit Find_Workshop_Loop;
            end if;
         end loop Find_Workshop_Loop;
         CheckTool(Recipe.Tool);
         declare
            Materials: array
              (Recipe.MaterialTypes.First_Index ..
                   Recipe.MaterialTypes.Last_Index) of Boolean :=
              (others => False);
         begin
            Find_Materials_Loop :
            for K in
              Recipe.MaterialTypes.First_Index ..
                Recipe.MaterialTypes.Last_Index loop
               Find_Cargo_Index_Loop :
               for J in Items_List.Iterate loop
                  if Items_List(J).IType = Recipe.MaterialTypes(K) then
                     CargoIndex :=
                       FindItem(PlayerShip.Cargo, Objects_Container.Key(J));
                     if CargoIndex > 0
                       and then PlayerShip.Cargo(CargoIndex).Amount >=
                         Recipe.MaterialAmounts(K) then
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
         AddButton
           (RecipesTable,
            To_String
              (Items_List(Recipes_List(Known_Recipes(I)).ResultIndex).Name),
            "Show available recipe's options",
            "ShowRecipeMenu" & Positive'Image(Row - 1), 1);
         AddCheckButton
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu" & Positive'Image(Row - 1), CanCraft, 2);
         AddCheckButton
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu" & Positive'Image(Row - 1), Has_Workplace, 3);
         AddCheckButton
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu" & Positive'Image(Row - 1), Has_Tool, 4);
         AddCheckButton
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu" & Positive'Image(Row - 1), Has_Materials, 5,
            True);
         Row := Row + 1;
         exit Show_Recipes_Loop when RecipesTable.Row = 26;
         <<End_Of_Loop>>
      end loop Show_Recipes_Loop;
      CheckTool(Alchemy_Tools);
      CanCraft := False;
      Has_Workplace := False;
      Find_Alchemy_Lab_Loop :
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = ALCHEMY_LAB
           and then Module.Durability > 0 then
            Has_Workplace := True;
            exit Find_Alchemy_Lab_Loop;
         end if;
      end loop Find_Alchemy_Lab_Loop;
      if Has_Workplace then
         CanCraft := True;
      end if;
      Set_Study_Recipes_Loop :
      for I in Studies.Iterate loop
         exit Set_Study_Recipes_Loop when RecipesTable.Row = 26;
         AddButton
           (RecipesTable, "Study " & To_String(Items_List(Studies(I)).Name),
            "Show available recipe's options",
            "ShowRecipeMenu {Study " & To_String(Studies(I)) & "}", 1);
         AddCheckButton
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Study " & To_String(Studies(I)) & "}", CanCraft,
            2);
         AddCheckButton
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Study " & To_String(Studies(I)) & "}",
            Has_Workplace, 3);
         AddCheckButton
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Study " & To_String(Studies(I)) & "}", Has_Tool,
            4, True);
         Row := Row + 1;
      end loop Set_Study_Recipes_Loop;
      Set_Deconstruct_Recipes_Loop :
      for I in Deconstructs.Iterate loop
         exit Set_Deconstruct_Recipes_Loop when RecipesTable.Row = 26;
         AddButton
           (RecipesTable,
            "Decontruct " & To_String(Items_List(Deconstructs(I)).Name),
            "Show available recipe's options",
            "ShowRecipeMenu {Decontruct " & To_String(Deconstructs(I)) & "}", 1);
         AddCheckButton
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Deconstruct " & To_String(Deconstructs(I)) & "}",
            CanCraft, 2);
         AddCheckButton
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Deconstruct " & To_String(Deconstructs(I)) & "}",
            Has_Workplace, 3);
         AddCheckButton
           (RecipesTable, "Show available recipe's options",
            "ShowRecipeMenu {Deconstruct " & To_String(Deconstructs(I)) & "}",
            Has_Tool, 4, True);
         Row := Row + 1;
      end loop Set_Deconstruct_Recipes_Loop;
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      if Page > 1 then
         if RecipesTable.Row < 26 then
            AddPagination
              (RecipesTable, "ShowCrafting" & Positive'Image(Page - 1), "");
         else
            AddPagination
              (RecipesTable, "ShowCrafting" & Positive'Image(Page - 1),
               "ShowModules" & Positive'Image(Page + 1));
         end if;
      elsif RecipesTable.Row = 26 then
         AddPagination
           (RecipesTable, "", "ShowCrafting" & Positive'Image(Page + 1));
      end if;
      UpdateTable(RecipesTable);
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
      ShowScreen("craftframe");
      return TCL_OK;
   end Show_Crafting_Command;

   -- ****if* CUI4/CUI4.ShowSetRecipe
   -- FUNCTION
   -- Show UI to set selected recipe as crafting order
   -- SOURCE
   procedure ShowSetRecipe(Index: Unbounded_String) is
      -- ****
      MaxAmount: Positive;
      MType: ModuleType;
      ModulesList: Unbounded_String;
      FrameName: constant String :=
        Main_Paned & ".craftframe.canvas.craft.item.set";
      AmountBox: constant Ttk_SpinBox := Get_Widget(FrameName & ".amount");
      MaxLabel: constant Ttk_Label := Get_Widget(FrameName & ".maxamount");
      ModulesBox: constant Ttk_ComboBox := Get_Widget(FrameName & ".workshop");
      RecipeIndex: constant Unbounded_String :=
        (if Element(Index, 1) = '{' then
           Unbounded_Slice(Index, 2, Length(Index) - 1)
         else Index);
   begin
      MaxAmount := CheckRecipe(RecipeIndex);
      Set(AmountBox, "1");
      configure
        (AmountBox,
         "-to" & Positive'Image(MaxAmount) &
         " -validatecommand {ValidateSpinbox %W %P}");
      if MaxAmount > 1 then
         configure(MaxLabel, "-text {max" & Positive'Image(MaxAmount) & "}");
         Tcl.Tk.Ada.Grid.Grid(MaxLabel);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(MaxLabel);
      end if;
      if Length(RecipeIndex) > 6
        and then Slice(RecipeIndex, 1, 5) = "Study" then
         Tcl.Tk.Ada.Grid.Grid_Remove(AmountBox);
      else
         Tcl.Tk.Ada.Grid.Grid(AmountBox);
      end if;
      if Length(RecipeIndex) > 6
        and then Slice(RecipeIndex, 1, 5) = "Study" then
         MType := ALCHEMY_LAB;
      elsif Length(RecipeIndex) > 12
        and then Slice(RecipeIndex, 1, 11) = "Deconstruct" then
         MType := ALCHEMY_LAB;
      else
         MType := Recipes_List(RecipeIndex).Workplace;
      end if;
      Show_Workshops_List_Loop :
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = MType then
            Append(ModulesList, " {" & Module.Name & "}");
         end if;
      end loop Show_Workshops_List_Loop;
      configure(ModulesBox, "-values [list" & To_String(ModulesList) & "]");
      Current(ModulesBox, "0");
   exception
      when An_Exception : Crafting_No_Materials =>
         ShowMessage
           (Text =>
              "You don't have enough materials to start " &
              Exception_Message(An_Exception) & ".",
            Title => "Can't set crafting recipe");
      when An_Exception : Crafting_No_Tools =>
         ShowMessage
           (Text =>
              "You don't have the proper tool to start " &
              Exception_Message(An_Exception) & ".",
            Title => "Can't set crafting recipe");
      when Trade_No_Free_Cargo =>
         ShowMessage
           (Text =>
              "You don't have that much free space in your ship's cargo.",
            Title => "Can't set crafting recipe");
      when An_Exception : Crafting_No_Workshop =>
         ShowMessage
           (Text =>
              "You don't have proper a workplace to start " &
              Exception_Message(An_Exception) & ".",
            Title => "Can't set crafting recipe");
   end ShowSetRecipe;

   -- ****o* CUI4/CUI4.Show_Recipe_Info_Command
   -- FUNCTION
   -- Show information about the selected recipe
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecipeInfo
   -- SOURCE
   function Show_Recipe_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recipe_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      FrameName: constant String := Main_Paned & ".craftframe.canvas.craft";
      RecipesView: constant Ttk_Tree_View :=
        Get_Widget(FrameName & ".list.view", Interp);
      WorkplaceName: Unbounded_String := Null_Unbounded_String;
      Recipe: Craft_Data;
      MAmount, CargoIndex: Natural := 0;
      HaveWorkplace, IsMaterial, HaveMaterials: Boolean := True;
      HaveTool: Boolean := False;
      TextLength: Positive;
      RecipeText: constant Tk_Text :=
        Get_Widget(FrameName & ".item.info.text", Interp);
      CraftFrame: constant Ttk_Frame :=
        Get_Widget(FrameName & ".item.set", Interp);
      ErrorLabel: constant Ttk_Label :=
        Get_Widget(FrameName & ".item.error", Interp);
      RecipeIndex: constant Unbounded_String :=
        To_Unbounded_String(Selection(RecipesView));
   begin
      configure(RecipeText, "-state normal");
      Delete(RecipeText, "1.0", "end");
      if Length(RecipeIndex) > 6
        and then Slice(RecipeIndex, 1, 5) = "{Stud" then
         Recipe.MaterialTypes.Append
           (New_Item =>
              Items_List
                (Unbounded_Slice(RecipeIndex, 8, Length(RecipeIndex) - 1))
                .IType);
         Recipe.ResultIndex :=
           Unbounded_Slice(RecipeIndex, 8, Length(RecipeIndex) - 1);
         Recipe.MaterialAmounts.Append(New_Item => 1);
         Recipe.ResultAmount := 0;
         Recipe.Workplace := ALCHEMY_LAB;
         Set_Study_Recipe_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.ResultIndex = Recipe.ResultIndex then
               Recipe.Skill := ProtoRecipe.Skill;
               Recipe.Time := ProtoRecipe.Difficulty * 15;
               exit Set_Study_Recipe_Loop;
            end if;
         end loop Set_Study_Recipe_Loop;
         Recipe.Difficulty := 1;
         Recipe.Tool := Alchemy_Tools;
         Recipe.ToolQuality := 100;
      elsif Length(RecipeIndex) > 12
        and then Slice(RecipeIndex, 1, 11) = "{Deconstruc" then
         Recipe.MaterialTypes.Append
           (New_Item =>
              Items_List
                (Unbounded_Slice(RecipeIndex, 14, Length(RecipeIndex) - 1))
                .IType);
         Recipe.ResultIndex :=
           Unbounded_Slice(RecipeIndex, 14, Length(RecipeIndex) - 1);
         Recipe.MaterialAmounts.Append(New_Item => 1);
         Recipe.ResultAmount := 0;
         Recipe.Workplace := ALCHEMY_LAB;
         Set_Deconstruct_Recipe_Loop :
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.ResultIndex = Recipe.ResultIndex then
               Recipe.Skill := ProtoRecipe.Skill;
               Recipe.Time := ProtoRecipe.Difficulty * 15;
               Recipe.Difficulty := ProtoRecipe.Difficulty;
               Recipe.ResultIndex :=
                 FindProtoItem(ProtoRecipe.MaterialTypes(1));
               Recipe.ResultAmount :=
                 Positive
                   (Float'Ceiling
                      (Float(ProtoRecipe.MaterialAmounts.Element(1)) * 0.8));
               exit Set_Deconstruct_Recipe_Loop;
            end if;
         end loop Set_Deconstruct_Recipe_Loop;
         Recipe.Tool := Alchemy_Tools;
         Recipe.ToolQuality := 100;
      else
         Recipe := Recipes_List(RecipeIndex);
         Insert
           (RecipeText, "end",
            "{Amount:" & Integer'Image(Recipe.ResultAmount) & LF & "}");
      end if;
      Insert(RecipeText, "end", "{Materials needed: }");
      declare
         Materials: array
           (Recipe.MaterialTypes.First_Index ..
                Recipe.MaterialTypes.Last_Index) of Boolean :=
           (others => False);
      begin
         Check_Materials_Loop :
         for I in
           Recipe.MaterialTypes.First_Index ..
             Recipe.MaterialTypes.Last_Index loop
            Insert(RecipeText, "end", "{" & LF & "-}");
            MAmount := 0;
            Find_Materials_Loop :
            for J in Items_List.Iterate loop
               IsMaterial := False;
               if Length(RecipeIndex) > 6
                 and then Slice(RecipeIndex, 1, 5) = "{Stud" then
                  if Items_List(J).Name =
                    Items_List(Recipe.ResultIndex).Name then
                     IsMaterial := True;
                  end if;
               elsif Length(RecipeIndex) > 12
                 and then Slice(RecipeIndex, 1, 11) = "{Deconstruc" then
                  if Objects_Container.Key(J) =
                    Unbounded_Slice
                      (RecipeIndex, 14, Length(RecipeIndex) - 1) then
                     IsMaterial := True;
                  end if;
               else
                  if Items_List(J).IType = Recipe.MaterialTypes(I) then
                     IsMaterial := True;
                  end if;
               end if;
               if IsMaterial then
                  if MAmount > 0 then
                     Insert(RecipeText, "end", "{ or}");
                  end if;
                  CargoIndex :=
                    FindItem(PlayerShip.Cargo, Objects_Container.Key(J));
                  if CargoIndex > 0
                    and then PlayerShip.Cargo(CargoIndex).Amount >=
                      Recipe.MaterialAmounts(I) then
                     Materials(I) := True;
                  end if;
                  if CargoIndex > 0
                    and then PlayerShip.Cargo(CargoIndex).Amount >=
                      Recipe.MaterialAmounts(I) then
                     TextLength :=
                       Positive'Image(PlayerShip.Cargo(CargoIndex).Amount)'
                         Length;
                     Insert
                       (RecipeText, "end",
                        "{" & Integer'Image(Recipe.MaterialAmounts(I)) & "x" &
                        To_String(Items_List(J).Name) & "(owned: " &
                        Positive'Image(PlayerShip.Cargo(CargoIndex).Amount)
                          (2 .. TextLength) &
                        ")}");
                  else
                     Insert
                       (RecipeText, "end",
                        "{" & Integer'Image(Recipe.MaterialAmounts(I)) & "x" &
                        To_String(Items_List(J).Name) & "} [list red]");
                  end if;
                  MAmount := MAmount + 1;
               end if;
            end loop Find_Materials_Loop;
         end loop Check_Materials_Loop;
         HaveMaterials := True;
         Have_Materials_Loop :
         for I in Materials'Range loop
            if not Materials(I) then
               HaveMaterials := False;
               exit Have_Materials_Loop;
            end if;
         end loop Have_Materials_Loop;
      end;
      if Recipe.Tool /= To_Unbounded_String("None") then
         Insert(RecipeText, "end", "{" & LF & "Tool: }");
         MAmount := 0;
         Check_Tool_Loop :
         for I in Items_List.Iterate loop
            if Items_List(I).IType = Recipe.Tool
              and then
              (Items_List(I).Value.Length > 0
               and then Items_List(I).Value(1) <= Recipe.ToolQuality) then
               if MAmount > 0 then
                  Insert(RecipeText, "end", "{ or }");
               end if;
               CargoIndex :=
                 FindItem
                   (Inventory => PlayerShip.Cargo,
                    ProtoIndex => Objects_Container.Key(I),
                    Quality => Recipe.ToolQuality);
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
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = Recipe.Workplace then
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
            if Modules_List(I).MType = Recipe.Workplace then
               WorkplaceName :=
                 To_Unbounded_String
                   (GetModuleType(BaseModules_Container.Key(I)));
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
         "{" & LF & "Skill: " & To_String(Skills_List(Recipe.Skill).Name) &
         "/" &
         To_String(Attributes_List(Skills_List(Recipe.Skill).Attribute).Name) &
         LF & "Time needed:" & Positive'Image(Recipe.Time) & " minutes}");
      if HaveMaterials and HaveTool and HaveWorkplace then
         ShowSetRecipe(RecipeIndex);
         Tcl.Tk.Ada.Grid.Grid(CraftFrame);
         Tcl.Tk.Ada.Grid.Grid_Remove(ErrorLabel);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(CraftFrame);
         Tcl.Tk.Ada.Grid.Grid(ErrorLabel);
         if not HaveMaterials then
            configure
              (ErrorLabel,
               "-text {You can't craft this recipe because you don't have the proper materials.}");
         end if;
         if not HaveTool then
            configure
              (ErrorLabel,
               "-text {You can't craft this recipe because you don't have the proper tool.}");
         end if;
         if not HaveWorkplace then
            configure
              (ErrorLabel,
               "-text {You can't craft this recipe because you don't have the proper workshop.}");
         end if;
      end if;
      configure(RecipeText, "-state disabled");
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
   -- SetCrafting
   -- SOURCE
   function Set_Crafting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Crafting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      FrameName: constant String := Main_Paned & ".craftframe.canvas.craft";
      RecipesView: constant Ttk_Tree_View :=
        Get_Widget(FrameName & ".list.view", Interp);
      RecipeIndex: Unbounded_String :=
        To_Unbounded_String(Selection(RecipesView));
      ModulesBox: constant Ttk_ComboBox :=
        Get_Widget(FrameName & ".item.set.workshop");
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(FrameName & ".item.set.amount", Interp);
   begin
      if Element(RecipeIndex, 1) = '{' then
         RecipeIndex :=
           Unbounded_Slice(RecipeIndex, 2, Length(RecipeIndex) - 1);
      end if;
      Set_Module_Loop :
      for I in PlayerShip.Modules.Iterate loop
         if PlayerShip.Modules(I).Name =
           To_Unbounded_String(Get(ModulesBox)) then
            SetRecipe
              (Modules_Container.To_Index(I), Positive'Value(Get(AmountBox)),
               RecipeIndex);
            UpdateMessages;
            exit Set_Module_Loop;
         end if;
      end loop Set_Module_Loop;
      return TCL_OK;
   end Set_Crafting_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowCrafting", Show_Crafting_Command'Access);
      AddCommand("ShowRecipeInfo", Show_Recipe_Info_Command'Access);
      AddCommand("SetCrafting", Set_Crafting_Command'Access);
   end AddCommands;

end Crafts.UI;
