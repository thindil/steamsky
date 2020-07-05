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
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Items; use Items;
with Maps.UI; use Maps.UI;
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

   procedure AddCommands is
   begin
      AddCommand("ShowCrafting", Show_Crafting_Command'Access);
   end AddCommands;

end Crafts.UI;
