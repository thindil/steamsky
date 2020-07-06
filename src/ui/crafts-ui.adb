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

with Ada.Characters.Handling; use Ada.Characters.Handling;
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
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Items; use Items;
with Maps.UI; use Maps.UI;
with Trades; use Trades;
with Utils.UI; use Utils.UI;

package body Crafts.UI is

   -- ****f* CUI4/Show_Crafting_Command
   -- FUNCTION
   -- Show information about available crafting recipes
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Crafting_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Crafting_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Paned: Ttk_PanedWindow;
      CraftsCanvas: Tk_Canvas;
      CraftsFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      Studies, Deconstructs: UnboundedString_Container.Vector;
      CanCraft: Boolean;
      Recipe: Craft_Data;
      CargoIndex: Natural;
      RecipesView: Ttk_Tree_View;
      FirstIndex: Unbounded_String;
      procedure CheckTool(ToolNeeded: Unbounded_String) is
      begin
         if ToolNeeded /= To_Unbounded_String("None") then
            CanCraft := False;
            for I in Items_List.Iterate loop
               if Items_List(I).IType = ToolNeeded then
                  CargoIndex :=
                    FindItem(PlayerShip.Cargo, Objects_Container.Key(I));
                  if CargoIndex > 0 then
                     CanCraft := True;
                     exit;
                  end if;
               end if;
            end loop;
         end if;
      end CheckTool;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      CraftsFrame.Interp := Interp;
      CraftsFrame.Name := New_String(Widget_Image(Paned) & ".craftframe");
      CraftsCanvas.Interp := Interp;
      CraftsCanvas.Name := New_String(Widget_Image(CraftsFrame) & ".canvas");
      if Winfo_Get(CraftsCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "crafts.tcl");
         Bind(CraftsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(CraftsCanvas, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp craft}");
      for Item of PlayerShip.Cargo loop
         for J in Recipes_List.Iterate loop
            if Recipes_List(J).ResultIndex = Item.ProtoIndex then
               if
                 (Known_Recipes.Find_Index(Item => Recipes_Container.Key(J)) =
                  Positive_Container.No_Index and
                  Studies.Find_Index(Item => Item.ProtoIndex) =
                    Positive_Container.No_Index) then
                  Studies.Append(New_Item => Item.ProtoIndex);
               end if;
               if Recipes_List(J).MaterialAmounts(1) > 1 and
                 Recipes_List(J).ResultAmount = 1 then
                  Deconstructs.Append(New_Item => Item.ProtoIndex);
               end if;
            end if;
         end loop;
      end loop;
      RecipesView.Interp := Interp;
      RecipesView.Name :=
        New_String(Widget_Image(CraftsCanvas) & ".craft.list.view");
      Delete(RecipesView, "[list " & Children(RecipesView, "{}") & "]");
      for I in Known_Recipes.First_Index .. Known_Recipes.Last_Index loop
         CanCraft := False;
         Recipe := Recipes_List(Known_Recipes(I));
         for Module of PlayerShip.Modules loop
            if Modules_List(Module.ProtoIndex).MType = Recipe.Workplace
              and then Module.Durability > 0 then
               CanCraft := True;
               exit;
            end if;
         end loop;
         if CanCraft then
            CheckTool(Recipe.Tool);
         end if;
         if CanCraft then
            declare
               Materials: array
                 (Recipe.MaterialTypes.First_Index ..
                      Recipe.MaterialTypes.Last_Index) of Boolean :=
                 (others => False);
            begin
               for K in
                 Recipe.MaterialTypes.First_Index ..
                   Recipe.MaterialTypes.Last_Index loop
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
                  end loop;
               end loop;
               CanCraft := True;
               for I in Materials'Range loop
                  if not Materials(I) then
                     CanCraft := False;
                     exit;
                  end if;
               end loop;
            end;
         end if;
         if CanCraft then
            Insert
              (RecipesView,
               "{} end -id {" & To_String(Known_Recipes(I)) & "} -text {" &
               To_String
                 (Items_List(Recipes_List(Known_Recipes(I)).ResultIndex)
                    .Name) &
               "}");
         else
            Insert
              (RecipesView,
               "{} end -id {" & To_String(Known_Recipes(I)) & "} -text {" &
               To_String
                 (Items_List(Recipes_List(Known_Recipes(I)).ResultIndex)
                    .Name) &
               "} -tags [list gray]");
         end if;
         if FirstIndex = Null_Unbounded_String then
            FirstIndex := Known_Recipes(I);
         end if;
      end loop;
      CheckTool(AlchemyTools);
      for I in Studies.First_Index .. Studies.Last_Index loop
         if CanCraft then
            Insert
              (RecipesView,
               "{} end -id {Study " & To_String(Studies(I)) &
               "} -text {Study " & To_String(Items_List(Studies(I)).Name) &
               "}");
         else
            Insert
              (RecipesView,
               "{} end -id {Study " & To_String(Studies(I)) &
               "} -text {Study " & To_String(Items_List(Studies(I)).Name) &
               "} -tag [list gray]");
         end if;
      end loop;
      for I in Deconstructs.First_Index .. Deconstructs.Last_Index loop
         if CanCraft then
            Insert
              (RecipesView,
               "{} end -id {Deconstruct " & To_String(Deconstructs(I)) &
               "} -text {Decontruct " &
               To_String(Items_List(Deconstructs(I)).Name) & "}");
         else
            Insert
              (RecipesView,
               "{} end -id {Deconstruct " & To_String(Deconstructs(I)) &
               "} -text {Decontruct " &
               To_String(Items_List(Deconstructs(I)).Name) &
               "} -tag [list gray]");
         end if;
      end loop;
      Selection_Set(RecipesView, "[list " & To_String(FirstIndex) & "]");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      CraftsFrame.Name := New_String(Widget_Image(CraftsCanvas) & ".craft");
      configure
        (CraftsCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (CraftsCanvas, "window",
         "[expr " & Winfo_Get(CraftsFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(CraftsFrame, "reqheight") & " / 2] -window " &
         Widget_Image(CraftsFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (CraftsCanvas,
         "-scrollregion [list " & BBox(CraftsCanvas, "all") & "]");
      ShowScreen("craftframe");
      return TCL_OK;
   end Show_Crafting_Command;

   -- ****if* CUI4/ShowSetRecipe
   -- FUNCTION
   -- Show UI to set selected recipe as crafting order
   -- SOURCE
   procedure ShowSetRecipe(Index: Unbounded_String) is
      -- ****
      MaxAmount: Positive;
      MType: ModuleType;
      RecipeIndex: Unbounded_String;
   begin
      if Element(Index, 1) = '{' then
         RecipeIndex := Unbounded_Slice(Index, 2, Length(Index) - 1);
      else
         RecipeIndex := Index;
      end if;
      MaxAmount := CheckRecipe(RecipeIndex);
--      Set_Value(AmountAdj, 1.0);
--      Set_Upper(AmountAdj, Gdouble(MaxAmount));
--      Set_Label(LabelTimes, "(max" & Positive'Image(MaxAmount) & "):");
      if Length(RecipeIndex) > 6
        and then Slice(RecipeIndex, 1, 5) = "Study" then
         MType := ALCHEMY_LAB;
      elsif Length(RecipeIndex) > 12
        and then Slice(RecipeIndex, 1, 11) = "Deconstruct" then
         MType := ALCHEMY_LAB;
      else
         MType := Recipes_List(RecipeIndex).Workplace;
      end if;
--      Remove_All(CmbModules);
--      for Module of PlayerShip.Modules loop
--         if Modules_List(Module.ProtoIndex).MType = MType then
--            Append_Text(CmbModules, To_String(Module.Name));
--         end if;
--      end loop;
--      if Length(RecipeIndex) > 6
--        and then Slice(RecipeIndex, 1, 5) = "Study" then
--         Hide(Gtk_Widget(Get_Object(Object, "spincraftamount")));
--         Hide(Gtk_Widget(LabelTimes));
--      else
--         Show_All(Gtk_Widget(Get_Object(Object, "spincraftamount")));
--         Show_All(Gtk_Widget(LabelTimes));
--      end if;
--      Set_Active(Gtk_Combo_Box(Get_Object(Object, "cmbmodules")), 0);
   exception
      when An_Exception : Crafting_No_Materials =>
         ShowMessage
           ("You don't have enough materials to start " &
            Exception_Message(An_Exception) & ".");
      when An_Exception : Crafting_No_Tools =>
         ShowMessage
           ("You don't have the proper tool to start " &
            Exception_Message(An_Exception) & ".");
      when Trade_No_Free_Cargo =>
         ShowMessage
           ("You don't have that much free space in your ship's cargo.");
      when An_Exception : Crafting_No_Workshop =>
         ShowMessage
           ("You don't have proper a workplace to start " &
            Exception_Message(An_Exception) & ".");
   end ShowSetRecipe;

   -- ****f* CUI4/Show_Recipe_Info_Command
   -- FUNCTION
   -- Show information about the selected recipe
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Recipe_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recipe_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      RecipesView: Ttk_Tree_View;
      WorkplaceName, RecipeIndex: Unbounded_String := Null_Unbounded_String;
      Recipe: Craft_Data;
      MAmount, CargoIndex: Natural := 0;
      HaveWorkplace, IsMaterial, HaveMaterials: Boolean := True;
      HaveTool: Boolean := False;
      TextLength: Positive;
      RecipeText: Tk_Text;
      CraftFrame: Ttk_Frame;
      ErrorLabel: Ttk_Label;
   begin
      RecipesView.Interp := Interp;
      RecipesView.Name :=
        New_String(".paned.craftframe.canvas.craft.list.view");
      RecipeIndex := To_Unbounded_String(Selection(RecipesView));
      RecipeText.Interp := Interp;
      RecipeText.Name :=
        New_String(".paned.craftframe.canvas.craft.item.info.text");
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
         for ProtoRecipe of Recipes_List loop
            if ProtoRecipe.ResultIndex = Recipe.ResultIndex then
               Recipe.Skill := ProtoRecipe.Skill;
               Recipe.Time := ProtoRecipe.Difficulty * 15;
               exit;
            end if;
         end loop;
         Recipe.Difficulty := 1;
         Recipe.Tool := AlchemyTools;
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
               exit;
            end if;
         end loop;
         Recipe.Tool := AlchemyTools;
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
         for I in
           Recipe.MaterialTypes.First_Index ..
             Recipe.MaterialTypes.Last_Index loop
            Insert(RecipeText, "end", "{" & LF & "-}");
            MAmount := 0;
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
                    Unbounded_Slice(RecipeIndex, 13, Length(RecipeIndex)) then
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
            end loop;
         end loop;
         HaveMaterials := True;
         for I in Materials'Range loop
            if not Materials(I) then
               HaveMaterials := False;
               exit;
            end if;
         end loop;
      end;
      if Recipe.Tool /= To_Unbounded_String("None") then
         Insert(RecipeText, "end", "{" & LF & "Tool: }");
         MAmount := 0;
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
                  Insert
                    (RecipeText, "end",
                     "{" & To_String(Items_List(I).Name) & "}");
               else
                  Insert
                    (RecipeText, "end",
                     "{" & To_String(Items_List(I).Name) & "} [list red]");
               end if;
               MAmount := MAmount + 1;
            end if;
         end loop;
      else
         HaveTool := True;
      end if;
      Insert(RecipeText, "end", "{" & LF & "Workplace: }");
      HaveWorkplace := False;
      for Module of PlayerShip.Modules loop
         if Modules_List(Module.ProtoIndex).MType = Recipe.Workplace then
            WorkplaceName := Module.Name;
            if Module.Durability > 0 then
               HaveWorkplace := True;
               exit;
            end if;
         end if;
      end loop;
      if WorkplaceName = Null_Unbounded_String then
         for Module of Modules_List loop
            if Module.MType = Recipe.Workplace then
               WorkplaceName :=
                 To_Unbounded_String(To_Lower(ModuleType'Image(Module.MType)));
               while Index(WorkplaceName, "_", 1) > 0 loop
                  Replace_Element
                    (WorkplaceName, Index(WorkplaceName, "_", 1), ' ');
               end loop;
               exit;
            end if;
         end loop;
      end if;
      if not HaveWorkplace then
         Insert
           (RecipeText, "end",
            "{" & To_String(WorkplaceName) & "} [list red]");
      else
         Insert(RecipeText, "end", "{" & To_String(WorkplaceName) & "}");
      end if;
      Insert
        (RecipeText, "end",
         "{" & LF & "Skill: " & To_String(Skills_List(Recipe.Skill).Name) &
         "/" &
         To_String(Attributes_List(Skills_List(Recipe.Skill).Attribute).Name) &
         "}");
      Insert
        (RecipeText, "end",
         "{" & LF & "Time needed:" & Positive'Image(Recipe.Time) &
         " minutes}");
      CraftFrame.Interp := Interp;
      CraftFrame.Name := New_String(".paned.craftframe.canvas.craft.item.set");
      ErrorLabel.Interp := Interp;
      ErrorLabel.Name :=
        New_String(".paned.craftframe.canvas.craft.item.error");
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

   procedure AddCommands is
   begin
      AddCommand("ShowCrafting", Show_Crafting_Command'Access);
      AddCommand("ShowRecipeInfo", Show_Recipe_Info_Command'Access);
   end AddCommands;

end Crafts.UI;
