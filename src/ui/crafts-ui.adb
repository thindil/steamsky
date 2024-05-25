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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tklib.Ada.Tooltip;
with Config;
with Dialogs;
with Items;
with Ships;
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
      Convention => C;
      -- ****

   function Show_Recipe_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Ada.Characters.Latin_1;
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      use Tcl.Ada;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.Text;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Config;
      use Dialogs;
      use Items;
      use Ships;
      use Tiny_String;

      Recipe_Index: constant Bounded_String :=
        To_Bounded_String(Source => CArgv.Arg(Argv => Argv, N => 1));
      Recipe_Length: constant Positive := Length(Source => Recipe_Index);
      Recipe_Type: constant String :=
        (if
           Recipe_Length > 6
           and then Slice(Source => Recipe_Index, Low => 1, High => 5) =
             "Study"
         then "Study"
         elsif
           Recipe_Length > 6
           and then Slice(Source => Recipe_Index, Low => 1, High => 5) =
             "Decon"
         then "Deconstruct"
         else "Craft");
      Recipe_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".recipedialog",
           Title =>
             (if Recipe_Type = "Study" then
                "Study " &
                To_String
                  (Source =>
                     Get_Proto_Item
                       (Index =>
                          Positive'Value
                            (Slice
                               (Source => Recipe_Index, Low => 7,
                                High => Recipe_Length)))
                       .Name)
              elsif Recipe_Type = "Deconstruct" then
                "Deconstruct " &
                To_String
                  (Source =>
                     Get_Proto_Item
                       (Index =>
                          Positive'Value
                            (Slice
                               (Source => Recipe_Index, Low => 13,
                                High => Recipe_Length)))
                       .Name)
              else "Craft " &
                To_String
                  (Source =>
                     Get_Proto_Item
                       (Index =>
                          Get_Recipe(Recipe_Index => Recipe_Index)
                            .Result_Index)
                       .Name)),
           Title_Width => 275);
      Workplace_Name: Bounded_String := Null_Bounded_String;
      Recipe: Craft_Data; --## rule line off IMPROPER_INITIALIZATION
      M_Amount, Cargo_Index: Natural := 0;
      Have_Workplace, Is_Material: Boolean := True;
      Have_Tool: Boolean := False;
      Text_Length: Positive := 1;
      Recipe_Text: constant Tk_Text :=
        Create
          (pathName => Recipe_Dialog & ".text",
           options => "-wrap char -height 15 -width 40", Interp => Interp);
   begin
      Tag_Configure
        (TextWidget => Recipe_Text, TagName => "red",
         Options =>
           "-foreground " &
           Tcl_GetVar
             (interp => Interp,
              varName =>
                "ttk::theme::" & To_String(Source => Get_Interface_Theme) &
                "::colors(-red)"));
      Tag_Configure
        (TextWidget => Recipe_Text, TagName => "gold",
         Options =>
           "-foreground " &
           Tcl_GetVar
             (interp => Interp,
              varName =>
                "ttk::theme::" & To_String(Source => Get_Interface_Theme) &
                "::colors(-goldenyellow)"));
      if Recipe_Type = "Study" then
         --## rule off IMPROPER_INITIALIZATION
         Recipe.Material_Types.Append
           (New_Item =>
              Get_Proto_Item
                (Index =>
                   Positive'Value
                     (Slice
                        (Source => Recipe_Index, Low => 7,
                         High => Length(Source => Recipe_Index))))
                .I_Type);
         Recipe.Result_Index :=
           Positive'Value
             (Slice
                (Source => Recipe_Index, Low => 7,
                 High => Length(Source => Recipe_Index)));
         Recipe.Material_Amounts.Append(New_Item => 1);
         --## rule on IMPROPER_INITIALIZATION
         Recipe.Result_Amount := 0;
         Recipe.Workplace := ALCHEMY_LAB;
         Set_Study_Recipe_Loop :
         for I in 1 .. Get_Recipes_Amount loop
            if Get_Recipe
                (Recipe_Index =>
                   To_Bounded_String
                     (Source => Trim(Source => I'Img, Side => Both)))
                .Result_Index =
              Recipe.Result_Index then
               Recipe.Skill :=
                 Get_Recipe
                   (Recipe_Index =>
                      To_Bounded_String
                        (Source => Trim(Source => I'Img, Side => Both)))
                   .Skill;
               Recipe.Time :=
                 Get_Recipe
                   (Recipe_Index =>
                      To_Bounded_String
                        (Source => Trim(Source => I'Img, Side => Both)))
                   .Difficulty *
                 15;
               exit Set_Study_Recipe_Loop;
            end if;
         end loop Set_Study_Recipe_Loop;
         Recipe.Difficulty := 1;
         Recipe.Tool := Alchemy_Tools;
         Recipe.Tool_Quality := 100;
      elsif Recipe_Type = "Deconstruct" then
         Recipe.Material_Types.Append
           (New_Item =>
              Get_Proto_Item
                (Index =>
                   Positive'Value
                     (Slice
                        (Source => Recipe_Index, Low => 13,
                         High => Length(Source => Recipe_Index))))
                .I_Type);
         Recipe.Result_Index :=
           Positive'Value
             (Slice
                (Source => Recipe_Index, Low => 13,
                 High => Length(Source => Recipe_Index)));
         Recipe.Material_Amounts.Append(New_Item => 1);
         Recipe.Result_Amount := 0;
         Recipe.Workplace := ALCHEMY_LAB;
         Set_Deconstruct_Recipe_Loop :
         for I in 1 .. Get_Recipes_Amount loop
            if Get_Recipe
                (Recipe_Index =>
                   To_Bounded_String
                     (Source => Trim(Source => I'Img, Side => Both)))
                .Result_Index =
              Recipe.Result_Index then
               Recipe.Skill :=
                 Get_Recipe
                   (Recipe_Index =>
                      To_Bounded_String
                        (Source => Trim(Source => I'Img, Side => Both)))
                   .Skill;
               Recipe.Time :=
                 Get_Recipe
                   (Recipe_Index =>
                      To_Bounded_String
                        (Source => Trim(Source => I'Img, Side => Both)))
                   .Difficulty *
                 15;
               Recipe.Difficulty :=
                 Get_Recipe
                   (Recipe_Index =>
                      To_Bounded_String
                        (Source => Trim(Source => I'Img, Side => Both)))
                   .Difficulty;
               Recipe.Result_Index :=
                 Find_Proto_Item
                   (Item_Type =>
                      Get_Recipe
                        (Recipe_Index =>
                           To_Bounded_String
                             (Source => Trim(Source => I'Img, Side => Both)))
                        .Material_Types
                        (1));
               Recipe.Result_Amount :=
                 Positive
                   (Float'Ceiling
                      (Float
                         (Get_Recipe
                            (Recipe_Index =>
                               To_Bounded_String
                                 (Source =>
                                    Trim(Source => I'Img, Side => Both)))
                            .Material_Amounts
                            .Element
                            (Index => 1)) *
                       0.8));
               exit Set_Deconstruct_Recipe_Loop;
            end if;
         end loop Set_Deconstruct_Recipe_Loop;
         Recipe.Tool := Alchemy_Tools;
         Recipe.Tool_Quality := 100;
      else
         Recipe := Get_Recipe(Recipe_Index => Recipe_Index);
         Insert
           (TextWidget => Recipe_Text, Index => "end", Text => "{Amount:}");
         Insert
           (TextWidget => Recipe_Text, Index => "end",
            Text =>
              "{" & Integer'Image(Recipe.Result_Amount) & LF &
              "} [list gold]");
      end if;
      Insert
        (TextWidget => Recipe_Text, Index => "end",
         Text => "{Materials needed: }");
      Check_Materials_Loop :
      for I in
        Recipe.Material_Types.First_Index ..
          Recipe.Material_Types.Last_Index loop
         Insert
           (TextWidget => Recipe_Text, Index => "end",
            Text => "{" & LF & "-} [list gold]");
         M_Amount := 0;
         Find_Materials_Loop :
         for J in 1 .. Get_Proto_Amount loop
            Is_Material := False;
            if Length(Source => Recipe_Index) > 6
              and then Slice(Source => Recipe_Index, Low => 1, High => 5) =
                "Study" then
               if Get_Proto_Item(Index => J).Name =
                 Get_Proto_Item(Index => Recipe.Result_Index).Name then
                  Is_Material := True;
               end if;
            elsif Length(Source => Recipe_Index) > 12
              and then Slice(Source => Recipe_Index, Low => 1, High => 11) =
                "Deconstruct" then
               if J =
                 Positive'Value
                   (Slice
                      (Source => Recipe_Index, Low => 13,
                       High => Length(Source => Recipe_Index))) then
                  Is_Material := True;
               end if;
            else
               if Get_Proto_Item(Index => J).I_Type =
                 Recipe.Material_Types(I) then
                  Is_Material := True;
               end if;
            end if;
            if Is_Material then
               if M_Amount > 0 then
                  Insert
                    (TextWidget => Recipe_Text, Index => "end",
                     Text => "{ or} [list gold]");
               end if;
               Cargo_Index :=
                 Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => J);
               if Cargo_Index > 0
                 and then
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => Cargo_Index)
                     .Amount >=
                   Recipe.Material_Amounts(I) then
                  Text_Length :=
                    Positive'Image
                      (Inventory_Container.Element
                         (Container => Player_Ship.Cargo, Index => Cargo_Index)
                         .Amount)'
                      Length;
                  Insert
                    (TextWidget => Recipe_Text, Index => "end",
                     Text =>
                       "{" & Integer'Image(Recipe.Material_Amounts(I)) & "x" &
                       To_String(Source => Get_Proto_Item(Index => J).Name) &
                       "(owned: " &
                       Positive'Image
                         (Inventory_Container.Element
                            (Container => Player_Ship.Cargo,
                             Index => Cargo_Index)
                            .Amount)
                         (2 .. Text_Length) &
                       ")} [list gold]");
               else
                  Insert
                    (TextWidget => Recipe_Text, Index => "end",
                     Text =>
                       "{" & Integer'Image(Recipe.Material_Amounts(I)) & "x" &
                       To_String(Source => Get_Proto_Item(Index => J).Name) &
                       "} [list red]");
               end if;
               M_Amount := M_Amount + 1;
            end if;
         end loop Find_Materials_Loop;
      end loop Check_Materials_Loop;
      if Recipe.Tool = To_Bounded_String(Source => "None") then
         Have_Tool := True;
      else
         Insert
           (TextWidget => Recipe_Text, Index => "end",
            Text => "{" & LF & "Tool: }");
         M_Amount := 0;
         Check_Tool_Loop :
         for I in 1 .. Get_Proto_Amount loop
            Have_Tool := False;
            if Get_Proto_Item(Index => I).I_Type = Recipe.Tool
              and then Get_Proto_Item(Index => I).Value(1) <=
                Recipe.Tool_Quality then
               if M_Amount > 0 then
                  Insert
                    (TextWidget => Recipe_Text, Index => "end",
                     Text => "{ or } [list gold]");
               end if;
               Cargo_Index :=
                 Find_Item
                   (Inventory => Player_Ship.Cargo, Proto_Index => I,
                    Quality => Recipe.Tool_Quality);
               if Cargo_Index > 0 then
                  Have_Tool := True;
               end if;
               Insert
                 (TextWidget => Recipe_Text, Index => "end",
                  Text =>
                    "{" &
                    To_String(Source => Get_Proto_Item(Index => I).Name) &
                    "}" &
                    (if not Have_Tool then " [list red]" else " [list gold]"));
               M_Amount := M_Amount + 1;
            end if;
         end loop Check_Tool_Loop;
      end if;
      Insert
        (TextWidget => Recipe_Text, Index => "end",
         Text => "{" & LF & "Workplace: }");
      Have_Workplace := False;
      Have_Workplace_Loop :
      for Module of Player_Ship.Modules loop
         if Get_Module(Index => Module.Proto_Index).M_Type =
           Recipe.Workplace then
            Workplace_Name := Module.Name;
            if Module.Durability > 0 then
               Have_Workplace := True;
               exit Have_Workplace_Loop;
            end if;
         end if;
      end loop Have_Workplace_Loop;
      if Workplace_Name = Null_Bounded_String then
         Find_Workshop_Name_Loop :
         for I in 1 .. Get_Modules_Amount loop
            if Get_Module(Index => I).M_Type = Recipe.Workplace then
               Workplace_Name :=
                 To_Bounded_String
                   (Source => Get_Module_Type(Module_Index => I));
               exit Find_Workshop_Name_Loop;
            end if;
         end loop Find_Workshop_Name_Loop;
      end if;
      Insert
        (TextWidget => Recipe_Text, Index => "end",
         Text =>
           "{" & To_String(Source => Workplace_Name) & "}" &
           (if not Have_Workplace then " [list red]" else " [list gold]"));
      Insert
        (TextWidget => Recipe_Text, Index => "end",
         Text => "{" & LF & "Skill: }");
      Insert
        (TextWidget => Recipe_Text, Index => "end",
         Text =>
           "{" &
           To_String
             (Source =>
                SkillsData_Container.Element
                  (Container => Skills_List, Index => Recipe.Skill)
                  .Name) &
           "/" &
           To_String
             (Source =>
                AttributesData_Container.Element
                  (Container => Attributes_List,
                   Index =>
                     SkillsData_Container.Element
                       (Container => Skills_List, Index => Recipe.Skill)
                       .Attribute)
                  .Name) &
           "} [list gold]");
      Insert
        (TextWidget => Recipe_Text, Index => "end",
         Text => "{" & LF & "Time needed:}");
      Insert
        (TextWidget => Recipe_Text, Index => "end",
         Text => "{" & Positive'Image(Recipe.Time) & " minutes} [list gold]");
      configure(Widgt => Recipe_Text, options => "-state disabled");
      Tcl.Tk.Ada.Grid.Grid(Slave => Recipe_Text, Options => "-padx 5");
      if CArgv.Arg(Argv => Argv, N => 2) = "true" then
         Add_Buttons_Block :
         declare
            use Ada.Characters.Handling;
            use Tcl.Tk.Ada.Widgets.TtkButton;
            use Tcl.Tklib.Ada.Tooltip;

            Button_Box: constant Ttk_Frame :=
              Create(pathName => Recipe_Dialog & ".buttons");
            Button: Ttk_Button;
         begin
            Button :=
              Create
                (pathName => Button_Box & ".craft",
                 options =>
                   "-image " & To_Lower(Item => Recipe_Type) & "icon" &
                   " -command {ShowSetRecipe {" &
                   CArgv.Arg(Argv => Argv, N => 1) & "};CloseDialog " &
                   Recipe_Dialog & "} -style Dialog.TButton -text {" &
                   Recipe_Type & "}");
            Tcl.Tk.Ada.Grid.Grid(Slave => Button);
            Add
              (Widget => Button,
               Message => "Set crafting order (" & Recipe_Type & ").");
            Bind
              (Widgt => Button, Sequence => "<Escape>",
               Script => "{" & Button_Box & ".close invoke;break}");
            Button :=
              Create
                (pathName => Button_Box & ".close",
                 options =>
                   "-image exiticon -command {CloseDialog " & Recipe_Dialog &
                   "} -style Dialog.TButton -text Close");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Button, Options => "-row 0 -column 1 -padx {5 0}");
            Add(Widget => Button, Message => "Close dialog \[Escape key\]");
            Focus(Widgt => Button);
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Button_Box & ".craft;break}");
            Bind
              (Widgt => Button, Sequence => "<Escape>",
               Script => "{" & Button & " invoke;break}");
            Tcl.Tk.Ada.Grid.Grid(Slave => Button_Box, Options => "-pady 5");
         end Add_Buttons_Block;
      else
         Add_Close_Button
           (Name => Recipe_Dialog & ".close", Text => "Close",
            Command => "CloseDialog " & Recipe_Dialog, Row => 2,
            Icon => "exiticon");
      end if;
      Show_Dialog
        (Dialog => Recipe_Dialog, Relative_X => 0.2, Relative_Y => 0.1);
      return TCL_OK;
   end Show_Recipe_Info_Command;

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
