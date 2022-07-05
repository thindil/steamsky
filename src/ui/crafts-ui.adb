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
with Ada.Strings; use Ada.Strings;
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
with Maps.UI; use Maps.UI;
with Ships.Crew; use Ships.Crew;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Crafts.UI is

   -- ****iv* CUI4/CUI4.Recipes_Table
   -- FUNCTION
   -- Table with info about available crafting recipes
   -- SOURCE
   Recipes_Table: Table_Widget (Amount => 5);
   -- ****

   -- ****iv* CUI4/CUI4.Recipes_Indexes
   -- FUNCTION
   -- Indexes of available indexes of crafting recipes
   -- SOURCE
   Recipes_Indexes: TinyString_Container.Vector;
   -- ****

   -- ****iv* CUI4/CUI4.Studies
   -- FUNCTION
   -- The list of available study recipes
   -- SOURCE
   Studies: Positive_Container.Vector;
   -- ****

   -- ****iv* CUI4/CUI4.Deconstructs
   -- FUNCTION
   -- The list of available deconstruct recipes
   -- SOURCE
   Deconstructs: Positive_Container.Vector;
   -- ****

   -- ****if* CUI4/CUI4.Check_Tool
   -- FUNCTION
   -- Check if the player has needed tool for the crafting recipe
   -- PARAMETERS
   -- Tool_Needed - The type of tool needed for the recipe
   -- RESULT
   -- True if the tool is in the player ship cargo, otherwise False
   -- SOURCE
   function Check_Tool
     (Tool_Needed: Tiny_String.Bounded_String) return Boolean is
      -- ****
      use Tiny_String;

      Cargo_Index: Natural;
      Has_Tool: Boolean := True;
   begin
      if Tool_Needed /= To_Bounded_String(Source => "None") then
         Has_Tool := False;
         Check_Tool_Loop :
         for I in
           Objects_Container.First_Index(Container => Items_List) ..
             Objects_Container.Last_Index(Container => Items_List) loop
            if To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List, Index => I)
                     .I_Type) =
              To_String(Source => Tool_Needed) then
               Cargo_Index :=
                 Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => I);
               if Cargo_Index > 0 then
                  Has_Tool := True;
                  exit Check_Tool_Loop;
               end if;
            end if;
         end loop Check_Tool_Loop;
      end if;
      return Has_Tool;
   end Check_Tool;

   -- ****if* CUI4/CUI4.Is_Craftable
   -- FUNCTION
   -- Check if the selected recipe can be crafted (has all requirements meet)
   -- PARAMETERS
   -- Recipe         - The crafting recipe to check
   -- Can_Craft      - If recipe can be crafted, then it will be True, otherwise
   --                  False
   -- Has_Workplace  - If there is workplace for the recipe, will be True,
   --                  otherwise False
   -- Has_Tool       - If there is available tool for the recipe, will be True,
   --                  otherwise False
   -- Has_Materials  - If there are available materials for the recipe, will be
   --                  True, otherwise False
   -- OUTPUT
   -- Parameters Can_Craft, Has_Workplace, Has_Tool and Has_Materials
   -- SOURCE
   procedure Is_Craftable
     (Recipe: Craft_Data;
      Can_Craft, Has_Workplace, Has_Tool, Has_Materials: out Boolean) is
      -- ****
      use Tiny_String;

      Cargo_Index: Natural;
   begin
      Can_Craft := False;
      Has_Workplace := False;
      Find_Workshop_Loop :
      for Module of Player_Ship.Modules loop
         if BaseModules_Container.Element
             (Container => Modules_List, Index => Module.Proto_Index)
             .M_Type =
           Recipe.Workplace
           and then Module.Durability > 0 then
            Has_Workplace := True;
            exit Find_Workshop_Loop;
         end if;
      end loop Find_Workshop_Loop;
      Has_Tool := Check_Tool(Tool_Needed => Recipe.Tool);
      Check_Recipe_Block :
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
            for J in
              Objects_Container.First_Index(Container => Items_List) ..
                Objects_Container.Last_Index(Container => Items_List) loop
               if Objects_Container.Element
                   (Container => Items_List, Index => J)
                   .I_Type =
                 Recipe.Material_Types(K) then
                  Cargo_Index :=
                    Find_Item
                      (Inventory => Player_Ship.Cargo, Proto_Index => J);
                  if Cargo_Index > 0
                    and then
                      Inventory_Container.Element
                        (Container => Player_Ship.Cargo, Index => Cargo_Index)
                        .Amount >=
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
      end Check_Recipe_Block;
      if Has_Tool and Has_Materials and Has_Workplace then
         Can_Craft := True;
      end if;
   end Is_Craftable;

   -- ****if* CUI4/CUI4.Check_Study_Prerequisites
   -- FUNCTION
   -- Check if the study and decontruct recipes can be crafted
   -- PARAMETERS
   -- Can_Craft      - If recipe can be crafter then it will be True, otherwise
   --                  False
   -- Has_Tool       - If there is tool for the study and deconstruct recipes
   --                  then True, otherwise False
   -- Has_Workplace  - If there is workplace for study and deconstruct recipes
   --                  then True, otherwise False
   -- OUTPUT
   -- Parameters Can_Craft, Has_Tool and Has_Workplace
   -- SOURCE
   procedure Check_Study_Prerequisites
     (Can_Craft, Has_Tool, Has_Workplace: out Boolean) is
     -- ****
   begin
      Has_Tool := Check_Tool(Tool_Needed => Alchemy_Tools);
      Can_Craft := False;
      Has_Workplace := False;
      Find_Alchemy_Lab_Loop :
      for Module of Player_Ship.Modules loop
         if BaseModules_Container.Element
             (Container => Modules_List, Index => Module.Proto_Index)
             .M_Type =
           ALCHEMY_LAB
           and then Module.Durability > 0 then
            Has_Workplace := True;
            exit Find_Alchemy_Lab_Loop;
         end if;
      end loop Find_Alchemy_Lab_Loop;
      if Has_Workplace then
         Can_Craft := True;
      end if;
   end Check_Study_Prerequisites;

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
      Convention => C;
      -- ****

   function Show_Crafting_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
      use Tiny_String;

      Crafts_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".craftframe", Interp => Interp);
      Crafts_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Crafts_Frame & ".canvas", Interp => Interp);
      Can_Craft, Has_Tool, Has_Workplace, Has_Materials: Boolean := True;
      Recipe: Craft_Data;
      Page: constant Positive :=
        (if Argc = 2 then Positive'Value(CArgv.Arg(Argv => Argv, N => 1))
         else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      Recipe_Name: constant String :=
        (if Argc = 3 then CArgv.Arg(Argv => Argv, N => 2) else "");
      Search_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Crafts_Canvas & ".craft.sframe.search");
   begin
      if Winfo_Get(Widgt => Crafts_Canvas, Info => "exists") = "0" then
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "crafts.tcl");
         Bind
           (Widgt => Crafts_Frame, Sequence => "<Configure>",
            Script => "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Widgt => Crafts_Canvas, Info => "ismapped") = "1" and
        Argc = 1 then
         Tcl_Eval(interp => Interp, strng => "InvokeButton " & Close_Button);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         return TCL_OK;
      end if;
      Tcl_SetVar
        (interp => Interp, varName => "gamestate", newValue => "crafts");
      if Recipe_Name'Length = 0 then
         configure(Widgt => Search_Entry, options => "-validatecommand {}");
         Delete
           (TextEntry => Search_Entry, FirstIndex => "0", LastIndex => "end");
         configure
           (Widgt => Search_Entry,
            options => "-validatecommand {ShowCrafting 1 %P}");
      end if;
      Studies.Clear;
      Deconstructs.Clear;
      Find_Possible_Recipes_Loop :
      for Item of Player_Ship.Cargo loop
         Add_Recipes_Loop :
         for J in Recipes_List.Iterate loop
            if Recipes_List(J).Result_Index = Item.Proto_Index then
               if Known_Recipes.Find_Index
                   (Item => Recipes_Container.Key(Position => J)) =
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
         Fill_Known_Recipes_Loop :
         for I in Known_Recipes.Iterate loop
            Recipes_Indexes.Append(New_Item => Known_Recipes(I));
         end loop Fill_Known_Recipes_Loop;
         Fill_Studies_Loop :
         for I in Studies.Iterate loop
            Recipes_Indexes.Append
              (New_Item =>
                 To_Bounded_String(Source => Positive'Image(Studies(I))));
         end loop Fill_Studies_Loop;
         Fill_Deconstructs_Loop :
         for I in Deconstructs.Iterate loop
            Recipes_Indexes.Append
              (New_Item =>
                 To_Bounded_String(Source => Positive'Image(Deconstructs(I))));
         end loop Fill_Deconstructs_Loop;
      end if;
      if Recipes_Table.Row_Height = 1 then
         Recipes_Table :=
           Create_Table
             (Parent => Crafts_Canvas & ".craft",
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Craftable"),
                 3 => To_Unbounded_String(Source => "Workshop"),
                 4 => To_Unbounded_String(Source => "Tools"),
                 5 => To_Unbounded_String(Source => "Materials")),
              Scrollbar => Get_Widget(pathName => Crafts_Frame & ".scrolly"),
              Command => "SortCrafting",
              Tooltip => "Press mouse button to sort the crafting recipes.");
      else
         Clear_Table(Table => Recipes_Table);
      end if;
      Show_Recipes_Loop :
      for I in Recipes_Indexes.First_Index .. Recipes_Indexes.Last_Index loop
         exit Show_Recipes_Loop when I > Positive(Known_Recipes.Length);
         if Recipe_Name'Length > 0
           and then
             Index
               (Source =>
                  To_Lower
                    (Item =>
                       To_String
                         (Source =>
                            Objects_Container.Element
                              (Container => Items_List,
                               Index =>
                                 Recipes_List
                                   (To_Bounded_String
                                      (Source =>
                                         To_String
                                           (Source => Recipes_Indexes(I))))
                                   .Result_Index)
                              .Name)),
                Pattern => To_Lower(Item => Recipe_Name), From => 1) =
             0 then
            goto End_Of_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         Recipe :=
           Recipes_List
             (To_Bounded_String
                (Source => To_String(Source => Recipes_Indexes(I))));
         Is_Craftable
           (Recipe => Recipe, Can_Craft => Can_Craft,
            Has_Workplace => Has_Workplace, Has_Tool => Has_Tool,
            Has_Materials => Has_Materials);
         Add_Button
           (Table => Recipes_Table,
            Text =>
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List,
                      Index =>
                        Recipes_List
                          (To_Bounded_String
                             (Source =>
                                To_String(Source => Recipes_Indexes(I))))
                          .Result_Index)
                     .Name),
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {" & To_String(Source => Recipes_Indexes(I)) &
              "} " & Boolean'Image(Can_Craft),
            Column => 1);
         Add_Check_Button
           (Table => Recipes_Table,
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {" & To_String(Source => Recipes_Indexes(I)) &
              "} " & Boolean'Image(Can_Craft),
            Checked => Can_Craft, Column => 2);
         Add_Check_Button
           (Table => Recipes_Table,
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {" & To_String(Source => Recipes_Indexes(I)) &
              "} " & Boolean'Image(Can_Craft),
            Checked => Has_Workplace, Column => 3);
         Add_Check_Button
           (Table => Recipes_Table,
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {" & To_String(Source => Recipes_Indexes(I)) &
              "} " & Boolean'Image(Can_Craft),
            Checked => Has_Tool, Column => 4);
         Add_Check_Button
           (Table => Recipes_Table,
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {" & To_String(Source => Recipes_Indexes(I)) &
              "} " & Boolean'Image(Can_Craft),
            Checked => Has_Materials, Column => 5, New_Row => True);
         exit Show_Recipes_Loop when Recipes_Table.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Loop>>
      end loop Show_Recipes_Loop;
      Check_Study_Prerequisites
        (Can_Craft => Can_Craft, Has_Tool => Has_Tool,
         Has_Workplace => Has_Workplace);
      Set_Study_Recipes_Loop :
      for I in
        Positive(Known_Recipes.Length + 1) .. Recipes_Indexes.Last_Index loop
         exit Set_Study_Recipes_Loop when Recipes_Table.Row =
           Game_Settings.Lists_Limit + 1 or
           I > Positive(Studies.Length);
         if Recipe_Name'Length > 0
           and then
             Index
               (Source =>
                  To_Lower
                    (Item =>
                       "Study " &
                       To_String
                         (Source =>
                            Objects_Container.Element
                              (Container => Items_List,
                               Index =>
                                 Positive'Value
                                   (To_String(Source => Recipes_Indexes(I))))
                              .Name)),
                Pattern => To_Lower(Item => Recipe_Name), From => 1) =
             0 then
            goto End_Of_Study_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Study_Loop;
         end if;
         Add_Button
           (Table => Recipes_Table,
            Text =>
              "Study " &
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List,
                      Index =>
                        Positive'Value
                          (To_String(Source => Recipes_Indexes(I))))
                     .Name),
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {Study " &
              To_String(Source => Recipes_Indexes(I)) & "} " &
              Boolean'Image(Can_Craft),
            Column => 1);
         Add_Check_Button
           (Table => Recipes_Table,
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {Study " &
              To_String(Source => Recipes_Indexes(I)) & "} " &
              Boolean'Image(Can_Craft),
            Checked => Can_Craft, Column => 2);
         Add_Check_Button
           (Table => Recipes_Table,
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {Study " &
              To_String(Source => Recipes_Indexes(I)) & "} " &
              Boolean'Image(Can_Craft),
            Checked => Has_Workplace, Column => 3);
         Add_Check_Button
           (Table => Recipes_Table,
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {Study " &
              To_String(Source => Recipes_Indexes(I)) & "} " &
              Boolean'Image(Can_Craft),
            Checked => Has_Tool, Column => 4, New_Row => True);
         <<End_Of_Study_Loop>>
      end loop Set_Study_Recipes_Loop;
      Set_Deconstruct_Recipes_Loop :
      for I in
        Positive(Known_Recipes.Length + Studies.Length + 1) ..
          Recipes_Indexes.Last_Index loop
         exit Set_Deconstruct_Recipes_Loop when Recipes_Table.Row =
           Game_Settings.Lists_Limit + 1;
         if Recipe_Name'Length > 0
           and then
             Index
               (Source =>
                  To_Lower
                    (Item =>
                       "Deconstruct " &
                       To_String
                         (Source =>
                            Objects_Container.Element
                              (Container => Items_List,
                               Index =>
                                 Positive'Value
                                   (To_String(Source => Recipes_Indexes(I))))
                              .Name)),
                Pattern => To_Lower(Item => Recipe_Name), From => 1) =
             0 then
            goto End_Of_Deconstruct_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Deconstruct_Loop;
         end if;
         Add_Button
           (Table => Recipes_Table,
            Text =>
              "Decontruct " &
              To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List,
                      Index =>
                        Positive'Value
                          (To_String(Source => Recipes_Indexes(I))))
                     .Name),
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {Deconstruct " &
              To_String(Source => Recipes_Indexes(I)) & "} " &
              Boolean'Image(Can_Craft),
            Column => 1);
         Add_Check_Button
           (Table => Recipes_Table,
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {Deconstruct " &
              To_String(Source => Recipes_Indexes(I)) & "} " &
              Boolean'Image(Can_Craft),
            Checked => Can_Craft, Column => 2);
         Add_Check_Button
           (Table => Recipes_Table,
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {Deconstruct " &
              To_String(Source => Recipes_Indexes(I)) & "} " &
              Boolean'Image(Can_Craft),
            Checked => Has_Workplace, Column => 3);
         Add_Check_Button
           (Table => Recipes_Table,
            Tooltip => "Show available recipe's options",
            Command =>
              "ShowRecipeMenu {Deconstruct " &
              To_String(Source => Recipes_Indexes(I)) & "} " &
              Boolean'Image(Can_Craft),
            Checked => Has_Tool, Column => 4, New_Row => True);
         <<End_Of_Deconstruct_Loop>>
      end loop Set_Deconstruct_Recipes_Loop;
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Button, Options => "-row 0 -column 1");
      if Page > 1 then
         if Recipes_Table.Row < Game_Settings.Lists_Limit + 1 then
            Add_Pagination
              (Table => Recipes_Table,
               Previous_Command =>
                 "ShowCrafting" & Positive'Image(Page - 1) &
                 (if Recipe_Name'Length > 0 then " {" & Recipe_Name & "}"
                  else ""),
               Next_Command => "");
         else
            Add_Pagination
              (Table => Recipes_Table,
               Previous_Command =>
                 "ShowCrafting" & Positive'Image(Page - 1) &
                 (if Recipe_Name'Length > 0 then " {" & Recipe_Name & "}"
                  else ""),
               Next_Command =>
                 "ShowCrafting" & Positive'Image(Page + 1) &
                 (if Recipe_Name'Length > 0 then " {" & Recipe_Name & "}"
                  else ""));
         end if;
      elsif Recipes_Table.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (Table => Recipes_Table, Previous_Command => "",
            Next_Command =>
              "ShowCrafting" & Positive'Image(Page + 1) &
              (if Recipe_Name'Length > 0 then " {" & Recipe_Name & "}"
               else ""));
      end if;
      Update_Table
        (Table => Recipes_Table,
         Grab_Focus =>
           (if Focus = Widget_Image(Win => Search_Entry) then False));
      Crafts_Frame.Name :=
        New_String(Str => Widget_Image(Win => Crafts_Canvas) & ".craft");
      configure
        (Widgt => Crafts_Canvas,
         options =>
           "-height [expr " & SashPos(Paned => Main_Paned, Index => "0") &
           " - 20] -width " & cget(Widgt => Main_Paned, option => "-width"));
      Tcl_Eval(interp => Get_Context, strng => "update");
      Canvas_Create
        (Parent => Crafts_Canvas, Child_Type => "window",
         Options =>
           "0 0 -anchor nw -window " & Widget_Image(Win => Crafts_Frame));
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Crafts_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Crafts_Canvas, TagOrId => "all") & "]");
      Show_Screen(New_Screen_Name => "craftframe");
      Tcl_SetResult(interp => Interp, str => "1");
      return TCL_OK;
   end Show_Crafting_Command;

   -- ****o* CUI4/CUI4.Show_Recipe_Menu_Command
   -- FUNCTION
   -- Show menu with available actions for the selected recipe
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecipeMenu index craftable
   -- Index is the index of the recipe to craft. If craftable is TRUE,
   -- then the recipe can be crafted, otherwise FALSE
   -- SOURCE
   function Show_Recipe_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recipe_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
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
                 (if CArgv.Arg(Argv => Argv, N => 2) = "TRUE" then "set"
                  else "info") &
                 ";break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      if CArgv.Arg(Argv => Argv, N => 2) = "TRUE" then
         Add_Button
           (Name => ".set", Label => "Set crafting order",
            Command =>
              "ShowSetRecipe {" & CArgv.Arg(Argv => Argv, N => 1) & "}");
      end if;
      Add_Button
        (Name => ".info", Label => "Show more info about the recipe",
         Command =>
           "ShowRecipeInfo {" & CArgv.Arg(Argv => Argv, N => 1) & "} " &
           CArgv.Arg(Argv => Argv, N => 2));
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Recipe_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Recipe_Menu_Command;

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
      Convention => C;
      -- ****

   function Show_Set_Recipe_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Tiny_String;

      M_Type: Module_Type;
      Modules_List_2, Crew_List: Unbounded_String;
      Recipe_Index: constant Bounded_String :=
        To_Bounded_String(Source => CArgv.Arg(Argv => Argv, N => 1));
      Recipe: constant Craft_Data :=
        Set_Recipe_Data(Recipe_Index => Recipe_Index);
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
      Craft_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".craftdialog",
           Title =>
             Recipe_Type & " " &
             (if Recipe_Type = "Study" then
                To_String
                  (Source =>
                     Objects_Container.Element
                       (Container => Items_List,
                        Index =>
                          Positive'Value
                            (Slice
                               (Source => Recipe_Index, Low => 7,
                                High => Recipe_Length)))
                       .Name)
              elsif Recipe_Type = "Deconstruct" then
                To_String
                  (Source =>
                     Objects_Container.Element
                       (Container => Items_List,
                        Index =>
                          Positive'Value
                            (Slice
                               (Source => Recipe_Index, Low => 13,
                                High => Recipe_Length)))
                       .Name)
              else To_String
                  (Source =>
                     Objects_Container.Element
                       (Container => Items_List,
                        Index => Recipes_List(Recipe_Index).Result_Index)
                       .Name)),
           Title_Width => 275, Columns => 2);
      Max_Amount: constant Positive :=
        Check_Recipe(Recipe_Index => Recipe_Index);
      Label: Ttk_Label :=
        Create
          (pathName => Craft_Dialog & ".amountlabel",
           options => "-text {Amount:}");
      Modules_Box: constant Ttk_ComboBox :=
        Create
          (pathName => Craft_Dialog & ".workshop",
           options => "-state readonly");
      Amount_Box: constant Ttk_SpinBox :=
        Create
          (pathName => Craft_Dialog & ".amount",
           options =>
             "-to" & Positive'Image(Max_Amount) &
             " -validatecommand {ValidateSpinbox %W %P} -width 20");
      Button: Ttk_Button :=
        Create
          (pathName => Craft_Dialog & ".maxamount",
           options =>
             "-text {max" & Positive'Image(Max_Amount) & "} -command {" &
             Amount_Box & " set" & Positive'Image(Max_Amount) & "}");
      Button_Row: Positive := 1;
      Modules_Amount: Natural := 0;
      Crafter_Button: Ttk_RadioButton :=
        Create
          (pathName => Craft_Dialog & ".noworker",
           options =>
             "-text {Don't assign anyone} -variable craftworker -value noone");
      Crew_Box: constant Ttk_ComboBox :=
        Create
          (pathName => Craft_Dialog & ".members",
           options => "-state readonly");
      First_Focus: Unbounded_String := Null_Unbounded_String;
   begin
      Set(SpinBox => Amount_Box, Value => "1");
      Tcl_SetVar
        (interp => Interp, varName => "craftworker", newValue => "noone");
      if Recipe_Type /= "Study" then
         if Max_Amount > 1 then
            Tcl.Tk.Ada.Grid.Grid(Slave => Label);
            Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-row 1 -column 1 -padx {0 5}");
            Add
              (Widget => Button,
               Message => "Set maximum possible amount of how many times\nthe crafting order should be done.");
            Bind(Widgt => Button, Sequence => "<Tab>", Script => "{focus " & Amount_Box & ";break}");
            Bind
              (Widgt => Button, Sequence => "<Escape>",
               Script => "{" & Craft_Dialog & ".cancel invoke;break}");
            First_Focus := To_Unbounded_String(Source => ".maxamount");
         else
            Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-columnspan 2");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Slave => Amount_Box, Options => "-columnspan 2 -padx 5");
         Add
           (Widget => Amount_Box,
            Message => "Set amount of how many times the crafting order\nshould be done.");
         Bind
           (Widgt => Amount_Box, Sequence => "<Tab>",
            Script => "{focus " & Craft_Dialog & ".noworker;break}");
         Bind
           (Widgt => Amount_Box, Sequence => "<Escape>",
            Script => "{" & Craft_Dialog & ".cancel invoke;break}");
         if First_Focus = Null_Unbounded_String then
            First_Focus := To_Unbounded_String(Source => ".amount");
         end if;
         Button_Row := Button_Row + 2;
      end if;
      if Recipe_Type in "Study" | "Deconstruct" then
         M_Type := ALCHEMY_LAB;
      else
         M_Type := Recipes_List(Recipe_Index).Workplace;
      end if;
      Show_Workshops_List_Loop :
      for Module of Player_Ship.Modules loop
         if BaseModules_Container.Element
             (Container => Modules_List, Index => Module.Proto_Index)
             .M_Type =
           M_Type then
            Append
              (Source => Modules_List_2, New_Item => " {" & To_String(Source => Module.Name) & "}");
            Modules_Amount := Modules_Amount + 1;
         end if;
      end loop Show_Workshops_List_Loop;
      configure
        (Widgt => Modules_Box, options => "-values [list" & To_String(Source => Modules_List_2) & "]");
      Current(ComboBox => Modules_Box, NewIndex => "0");
      if Modules_Amount > 1 then
         Label := Create(pathName => Craft_Dialog & ".workshoplabel", options => "-text {Wokshop:}");
         Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-columnspan 2 -padx 5");
         Tcl.Tk.Ada.Grid.Grid(Slave => Modules_Box, Options => "-columnspan 2 -padx 5");
         Bind
           (Widgt => Modules_Box, Sequence => "<Escape>",
            Script => "{" & Craft_Dialog & ".cancel invoke;break}");
         Button_Row := Button_Row + 2;
         if First_Focus = Null_Unbounded_String then
            First_Focus := To_Unbounded_String(Source => ".workshop");
         end if;
      end if;
      Tcl.Tk.Ada.Grid.Grid(Slave => Crafter_Button, Options => "-columnspan 2 -padx 5 -sticky w");
      Add
        (Widget => Crafter_Button,
         Message => "Don't assign anyone to the order. You can\nmanually do it later, in ship info screen.");
      Bind
        (Widgt => Crafter_Button, Sequence => "<Tab>",
         Script => "{focus " & Craft_Dialog & ".bestworker;break}");
      Bind
        (Widgt => Crafter_Button, Sequence => "<Escape>",
         Script => "{" & Craft_Dialog & ".cancel invoke;break}");
      if First_Focus = Null_Unbounded_String then
         First_Focus := To_Unbounded_String(Source => ".noworker");
      end if;
      Crafter_Button :=
        Create
          (pathName => Craft_Dialog & ".bestworker",
           options => "-text {Assign the best worker} -variable craftworker -value best");
      Tcl.Tk.Ada.Grid.Grid(Slave => Crafter_Button, Options => "-columnspan 2 -padx 5 -sticky w");
      Add
        (Crafter_Button,
         "Assign the crew member with the highest skill\nneeded for the recipe, even if the crew member\nis busy.");
      Bind
        (Crafter_Button, "<Escape>",
         "{" & Craft_Dialog & ".cancel invoke;break}");
      Crafter_Button :=
        Create
          (Craft_Dialog & ".selectedworker",
           "-text {Assign selected member} -variable craftworker -value fromlist");
      Tcl.Tk.Ada.Grid.Grid(Crafter_Button, "-columnspan 2 -padx 5 -sticky w");
      Add
        (Crafter_Button,
         "Assign the crew member from the list.\nThe sign + after name means that this crew member has\nneeded skill, the sign ++ after name means that his/her\nneeded skill is the best in the crew.");
      Bind(Crafter_Button, "<Tab>", "{focus " & Crew_Box & ";break}");
      Bind
        (Crafter_Button, "<Escape>",
         "{" & Craft_Dialog & ".cancel invoke;break}");
      Show_Members_List_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Append
           (Crew_List,
            " {" & To_String(Source => Player_Ship.Crew(I).Name) &
            Get_Skill_Marks(Recipe.Skill, Crew_Container.To_Index(I)) & "}");
      end loop Show_Members_List_Loop;
      configure(Crew_Box, "-values [list" & To_String(Crew_List) & "]");
      Current(Crew_Box, "0");
      Tcl.Tk.Ada.Grid.Grid(Crew_Box, "-columnspan 2 -padx 5");
      Add
        (Crew_Box,
         "Assign the crew member from the list.\nThe sign + after name means that this crew member has\nneeded skill, the sign ++ after name means that his/her\nneeded skill is the best in the crew.");
      Bind(Crew_Box, "<Tab>", "{focus " & Craft_Dialog & ".craft;break}");
      Bind(Crew_Box, "<Escape>", "{" & Craft_Dialog & ".cancel invoke;break}");
      Button_Row := Button_Row + 4;
      Button :=
        Create
          (Craft_Dialog & ".craft",
           "-text {" & Recipe_Type & "} -command {SetCrafting {" &
           CArgv.Arg(Argv, 1) & "};CloseDialog " & Craft_Dialog & "}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-pady 5 -padx 5");
      Add(Button, "Set the crafting order.");
      Bind(Button, "<Escape>", "{" & Craft_Dialog & ".cancel invoke;break}");
      Button :=
        Create
          (Craft_Dialog & ".cancel",
           "-text {Cancel} -command {CloseDialog " & Craft_Dialog & "}");
      Tcl.Tk.Ada.Grid.Grid
        (Button,
         "-pady 5 -padx 5 -column 1 -row" & Positive'Image(Button_Row));
      Add(Button, "Cancel setting the order and close dialog.");
      Bind
        (Button, "<Tab>",
         "{focus " & Craft_Dialog & To_String(First_Focus) & ";break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      Show_Dialog(Craft_Dialog);
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

      RecipeIndex: constant Bounded_String :=
        To_Bounded_String(CArgv.Arg(Argv, 1));
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
                (Objects_Container.Element
                   (Container => Items_List,
                    Index =>
                      Positive'Value(Slice(RecipeIndex, 7, RecipeLength)))
                   .Name)
            elsif RecipeType = "Deconstruct" then
              "Deconstruct " &
              To_String
                (Objects_Container.Element
                   (Container => Items_List,
                    Index =>
                      Positive'Value(Slice(RecipeIndex, 13, RecipeLength)))
                   .Name)
            else "Craft " &
              To_String
                (Objects_Container.Element
                   (Container => Items_List,
                    Index => Recipes_List(RecipeIndex).Result_Index)
                   .Name)),
           275);
      WorkplaceName: Bounded_String := Null_Bounded_String;
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
              Objects_Container.Element
                (Container => Items_List,
                 Index =>
                   Positive'Value(Slice(RecipeIndex, 7, Length(RecipeIndex))))
                .I_Type);
         Recipe.Result_Index :=
           Positive'Value(Slice(RecipeIndex, 7, Length(RecipeIndex)));
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
              Objects_Container.Element
                (Container => Items_List,
                 Index =>
                   Positive'Value(Slice(RecipeIndex, 13, Length(RecipeIndex))))
                .I_Type);
         Recipe.Result_Index :=
           Positive'Value(Slice(RecipeIndex, 13, Length(RecipeIndex)));
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
         for J in
           Objects_Container.First_Index(Container => Items_List) ..
             Objects_Container.Last_Index(Container => Items_List) loop
            IsMaterial := False;
            if Length(RecipeIndex) > 6
              and then Slice(RecipeIndex, 1, 5) = "Study" then
               if Objects_Container.Element
                   (Container => Items_List, Index => J)
                   .Name =
                 Objects_Container.Element
                   (Container => Items_List, Index => Recipe.Result_Index)
                   .Name then
                  IsMaterial := True;
               end if;
            elsif Length(RecipeIndex) > 12
              and then Slice(RecipeIndex, 1, 11) = "Deconstruct" then
               if J =
                 Positive'Value
                   (Slice(RecipeIndex, 13, Length(RecipeIndex))) then
                  IsMaterial := True;
               end if;
            else
               if Objects_Container.Element
                   (Container => Items_List, Index => J)
                   .I_Type =
                 Recipe.Material_Types(I) then
                  IsMaterial := True;
               end if;
            end if;
            if IsMaterial then
               if MAmount > 0 then
                  Insert(RecipeText, "end", "{ or}");
               end if;
               CargoIndex := Find_Item(Player_Ship.Cargo, J);
               if CargoIndex > 0
                 and then
                   Inventory_Container.Element
                     (Container => Player_Ship.Cargo, Index => CargoIndex)
                     .Amount >=
                   Recipe.Material_Amounts(I) then
                  TextLength :=
                    Positive'Image
                      (Inventory_Container.Element
                         (Container => Player_Ship.Cargo, Index => CargoIndex)
                         .Amount)'
                      Length;
                  Insert
                    (RecipeText, "end",
                     "{" & Integer'Image(Recipe.Material_Amounts(I)) & "x" &
                     To_String
                       (Objects_Container.Element
                          (Container => Items_List, Index => J)
                          .Name) &
                     "(owned: " &
                     Positive'Image
                       (Inventory_Container.Element
                          (Container => Player_Ship.Cargo, Index => CargoIndex)
                          .Amount)
                       (2 .. TextLength) &
                     ")}");
               else
                  Insert
                    (RecipeText, "end",
                     "{" & Integer'Image(Recipe.Material_Amounts(I)) & "x" &
                     To_String
                       (Objects_Container.Element
                          (Container => Items_List, Index => J)
                          .Name) &
                     "} [list red]");
               end if;
               MAmount := MAmount + 1;
            end if;
         end loop Find_Materials_Loop;
      end loop Check_Materials_Loop;
      if Recipe.Tool /= To_Bounded_String("None") then
         Insert(RecipeText, "end", "{" & LF & "Tool: }");
         MAmount := 0;
         Check_Tool_Loop :
         for I in
           Objects_Container.First_Index(Container => Items_List) ..
             Objects_Container.Last_Index(Container => Items_List) loop
            HaveTool := False;
            if Objects_Container.Element(Container => Items_List, Index => I)
                .I_Type =
              Recipe.Tool
              and then
              (Objects_Container.Element(Container => Items_List, Index => I)
                 .Value
                 (1) <=
               Recipe.Tool_Quality) then
               if MAmount > 0 then
                  Insert(RecipeText, "end", "{ or }");
               end if;
               CargoIndex :=
                 Find_Item
                   (Inventory => Player_Ship.Cargo, Proto_Index => I,
                    Quality => Recipe.Tool_Quality);
               if CargoIndex > 0 then
                  HaveTool := True;
               end if;
               Insert
                 (RecipeText, "end",
                  "{" &
                  To_String
                    (Objects_Container.Element
                       (Container => Items_List, Index => I)
                       .Name) &
                  "}" & (if not HaveTool then " [list red]" else ""));
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
         if BaseModules_Container.Element
             (Container => Modules_List, Index => Module.Proto_Index)
             .M_Type =
           Recipe.Workplace then
            WorkplaceName := Module.Name;
            if Module.Durability > 0 then
               HaveWorkplace := True;
               exit Have_Workplace_Loop;
            end if;
         end if;
      end loop Have_Workplace_Loop;
      if WorkplaceName = Null_Bounded_String then
         Find_Workshop_Name_Loop :
         for I in
           BaseModules_Container.First_Index(Container => Modules_List) ..
             BaseModules_Container.Last_Index(Container => Modules_List) loop
            if BaseModules_Container.Element
                (Container => Modules_List, Index => I)
                .M_Type =
              Recipe.Workplace then
               WorkplaceName := To_Bounded_String(Get_Module_Type(I));
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
      use Tiny_String;

      RecipeIndex: Bounded_String := To_Bounded_String(CArgv.Arg(Argv, 1));
      ModulesBox: constant Ttk_ComboBox := Get_Widget(".craftdialog.workshop");
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".craftdialog.amount", Interp);
      MembersBox: constant Ttk_ComboBox :=
        Get_Widget(".craftdialog.members", Interp);
      AssignWorker: constant String := Tcl_GetVar(Interp, "craftworker");
      WorkshopIndex: Natural := Natural'Value(Current(ModulesBox)) + 1;
   begin
      if Element(RecipeIndex, 1) = '{' then
         RecipeIndex := Bounded_Slice(RecipeIndex, 2, Length(RecipeIndex) - 1);
      end if;
      Set_Module_Loop :
      for I in
        Player_Ship.Modules.First_Index .. Player_Ship.Modules.Last_Index loop
         if Player_Ship.Modules(I).Name =
           To_Bounded_String(Get(ModulesBox)) then
            WorkshopIndex := WorkshopIndex - 1;
         end if;
         if WorkshopIndex = 0 then
            Set_Recipe(I, Positive'Value(Get(AmountBox)), RecipeIndex);
            if AssignWorker = "fromlist" then
               Give_Orders
                 (Player_Ship, Positive'Value(Current(MembersBox)) + 1, CRAFT,
                  I);
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
                          (Player_Ship, Crew_Container.To_Index(J), CRAFT, I);
                        WorkerAssigned := True;
                        exit Set_Best_Worker_Loop;
                     end if;
                  end loop Set_Best_Worker_Loop;
                  if not WorkerAssigned then
                     Give_Orders(Player_Ship, 1, CRAFT, I);
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
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number(Recipes_Table, Natural'Value(CArgv.Arg(Argv, 1)));
      type Local_Module_Data is record
         Name: Unbounded_String;
         Craftable: Boolean;
         Workplace: Boolean;
         Tool: Boolean;
         Materials: Boolean;
         Id: Bounded_String;
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
              (Recipes_List
                 (To_Bounded_String
                    (Source => To_String(Source => Known_Recipes(I)))),
               Can_Craft, Has_Workplace, Has_Tool, Has_Materials);
            Local_Recipes(TinyString_Container.To_Index(I)) :=
              (Name =>
                 To_Unbounded_String
                   (Source =>
                      To_String
                        (Source =>
                           Objects_Container.Element
                             (Container => Items_List,
                              Index =>
                                Recipes_List
                                  (To_Bounded_String
                                     (Source =>
                                        To_String(Source => Known_Recipes(I))))
                                  .Result_Index)
                             .Name)),
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
            Local_Recipes(Positive_Container.To_Index(I)) :=
              (Name =>
                 To_Unbounded_String
                   (Source =>
                      To_String
                        (Source =>
                           Objects_Container.Element
                             (Container => Items_List, Index => Studies(I))
                             .Name)),
               Craftable => Can_Craft, Tool => Has_Tool,
               Workplace => Has_Workplace, Materials => True,
               Id =>
                 To_Bounded_String
                   (Source =>
                      Trim
                        (Source => Positive'Image(Studies(I)), Side => Left)));
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
            Local_Recipes(Positive_Container.To_Index(I)) :=
              (Name =>
                 To_Unbounded_String
                   (Source =>
                      To_String
                        (Source =>
                           Objects_Container.Element
                             (Container => Items_List,
                              Index => Deconstructs(I))
                             .Name)),
               Craftable => Can_Craft, Workplace => Has_Workplace,
               Tool => Has_Tool, Materials => True,
               Id =>
                 To_Bounded_String
                   (Source =>
                      Trim
                        (Source => Positive'Image(Deconstructs(I)),
                         Side => Left)));
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

   procedure Add_Commands is
   begin
      Add_Command("ShowCrafting", Show_Crafting_Command'Access);
      Add_Command("ShowRecipeMenu", Show_Recipe_Menu_Command'Access);
      Add_Command("ShowSetRecipe", Show_Set_Recipe_Command'Access);
      Add_Command("ShowRecipeInfo", Show_Recipe_Info_Command'Access);
      Add_Command("SetCrafting", Set_Crafting_Command'Access);
      Add_Command("SortCrafting", Sort_Crafting_Command'Access);
   end Add_Commands;

end Crafts.UI;
