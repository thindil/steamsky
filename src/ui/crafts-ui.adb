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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Containers.Generic_Array_Sort; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
with Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with CoreUI;
with Crew;
with Dialogs; use Dialogs;
with Items; use Items;
with Maps.UI;
with Ships.Crew;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Crafts.UI is

   -- ****iv* CUI4/CUI4.Recipes_Table
   -- FUNCTION
   -- Table with info about available crafting recipes
   -- SOURCE
   Recipes_Table: Table_Widget (Amount => 4);
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

   --## rule off IMPROPER_INITIALIZATION
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
      use Game.Tiny_String;

      Nim_Recipe: Craft_Nim_Data;
      M_Types: Material_Types_Array := (others => New_String(Str => ""));
      M_Amounts: Material_Amounts_Array := (others => 0);
      Craft, Workplace, Tool, Materials, Index: Integer := 0;
      procedure Is_Ada_Craftable
        (R: Craft_Nim_Data;
         C_Craft, H_Workplace, H_Tool, H_Materials: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "isAdaCraftable";
   begin
      Convert_Materials_Types_Loop :
      for Material of Recipe.Material_Types loop
         M_Types(Index) := New_String(Str => To_String(Source => Material));
         Index := Index + 1;
      end loop Convert_Materials_Types_Loop;
      Index := 0;
      Convert_Materials_Amounts_Loop :
      for Amount of Recipe.Material_Amounts loop
         M_Amounts(Index) := Amount;
         Index := Index + 1;
      end loop Convert_Materials_Amounts_Loop;
      Nim_Recipe :=
        (Workplace => Module_Type'Pos(Recipe.Workplace),
         Tool => New_String(Str => To_String(Source => Recipe.Tool)),
         Tool_Quality => 1, Reputation => 0, Difficulty => 1, Time => 1,
         Skill => 0, Result_Index => 0, Result_Amount => 1,
         Material_Types => M_Types, Material_Amounts => M_Amounts);
      Is_Ada_Craftable
        (R => Nim_Recipe, C_Craft => Craft, H_Workplace => Workplace,
         H_Tool => Tool, H_Materials => Materials);
      Can_Craft := (if Craft = 1 then True else False);
      Has_Workplace := (if Workplace = 1 then True else False);
      Has_Materials := (if Materials = 1 then True else False);
      Has_Tool := (if Tool = 1 then True else False);
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
      Craft, Workplace, Tool: Integer := 0;
      procedure Check_Ada_Study_Prerequisites
        (C_Craft, H_Workplace, H_Tool: out Integer) with
         Import => True,
         Convention => C,
         External_Name => "checkAdaStudyPrerequisities";
   begin
      Check_Ada_Study_Prerequisites
        (C_Craft => Craft, H_Workplace => Workplace, H_Tool => Tool);
      Can_Craft := (if Craft = 1 then True else False);
      Has_Workplace := (if Workplace = 1 then True else False);
      Has_Tool := (if Tool = 1 then True else False);
   end Check_Study_Prerequisites;
   --## rule on IMPROPER_INITIALIZATION

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
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use CoreUI;
--
      Crafts_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".craftframe", Interp => Interp);
      function Show_Ada_Crafting_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Import => True,
         Convention => C,
         External_Name => "showCraftingCommand";
   begin
      if Show_Ada_Crafting_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv) /=
        TCL_OK then
         return TCL_ERROR;
      end if;
      if Recipes_Table.Row_Height = 1 then
         Recipes_Table.Scrollbar :=
              Get_Widget(pathName => Crafts_Frame & ".scrolly");
      end if;
      return TCL_OK;
   end Show_Crafting_Command;

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
      use Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use Tiny_String;

      M_Type: Module_Type;
      Modules_List_2, Crew_List: Unbounded_String := Null_Unbounded_String;
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
                     Get_Proto_Item
                       (Index =>
                          Positive'Value
                            (Slice
                               (Source => Recipe_Index, Low => 7,
                                High => Recipe_Length)))
                       .Name)
              elsif Recipe_Type = "Deconstruct" then
                To_String
                  (Source =>
                     Get_Proto_Item
                       (Index =>
                          Positive'Value
                            (Slice
                               (Source => Recipe_Index, Low => 13,
                                High => Recipe_Length)))
                       .Name)
              else To_String
                  (Source =>
                     Get_Proto_Item
                       (Index =>
                          Get_Recipe(Recipe_Index => Recipe_Index)
                            .Result_Index)
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
             "-from 1 -to" & Positive'Image(Max_Amount) &
             " -validate key -validatecommand {ValidateSpinbox %W %P " &
             Craft_Dialog & ".craft} -width 20");
      Button: Ttk_Button :=
        Create
          (pathName => Craft_Dialog & ".maxamount",
           options =>
             "-text {max" & Positive'Image(Max_Amount) & "} -command {" &
             Amount_Box & " set" & Positive'Image(Max_Amount) & ";" &
             Amount_Box & " validate}");
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
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Button, Options => "-row 1 -column 1 -padx {0 5}");
            Add
              (Widget => Button,
               Message =>
                 "Set maximum possible amount of how many times\nthe crafting order should be done.");
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Amount_Box & ";break}");
            Bind
              (Widgt => Button, Sequence => "<Escape>",
               Script => "{" & Craft_Dialog & ".cancel invoke;break}");
            First_Focus := To_Unbounded_String(Source => ".maxamount");
         else
            Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-columnspan 2");
         end if;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Amount_Box, Options => "-columnspan 2 -padx 5");
         Add
           (Widget => Amount_Box,
            Message =>
              "Set amount of how many times the crafting order\nshould be done.");
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
         M_Type := Get_Recipe(Recipe_Index => Recipe_Index).Workplace;
      end if;
      Show_Workshops_List_Loop :
      for Module of Player_Ship.Modules loop
         if Get_Module(Index => Module.Proto_Index).M_Type = M_Type then
            Append
              (Source => Modules_List_2,
               New_Item => " {" & To_String(Source => Module.Name) & "}");
            Modules_Amount := Modules_Amount + 1;
         end if;
      end loop Show_Workshops_List_Loop;
      configure
        (Widgt => Modules_Box,
         options =>
           "-values [list" & To_String(Source => Modules_List_2) & "]");
      Current(ComboBox => Modules_Box, NewIndex => "0");
      if Modules_Amount > 1 then
         Label :=
           Create
             (pathName => Craft_Dialog & ".workshoplabel",
              options => "-text {Wokshop:}");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Label, Options => "-columnspan 2 -padx 5");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Modules_Box, Options => "-columnspan 2 -padx 5");
         Bind
           (Widgt => Modules_Box, Sequence => "<Escape>",
            Script => "{" & Craft_Dialog & ".cancel invoke;break}");
         Button_Row := Button_Row + 2;
         if First_Focus = Null_Unbounded_String then
            First_Focus := To_Unbounded_String(Source => ".workshop");
         end if;
      end if;
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Crafter_Button,
         Options => "-columnspan 2 -padx 5 -sticky w");
      Add
        (Widget => Crafter_Button,
         Message =>
           "Don't assign anyone to the order. You can\nmanually do it later, in ship info screen.");
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
           options =>
             "-text {Assign the best worker} -variable craftworker -value best");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Crafter_Button,
         Options => "-columnspan 2 -padx 5 -sticky w");
      Add
        (Widget => Crafter_Button,
         Message =>
           "Assign the crew member with the highest skill\nneeded for the recipe, even if the crew member\nis busy.");
      Bind
        (Widgt => Crafter_Button, Sequence => "<Escape>",
         Script => "{" & Craft_Dialog & ".cancel invoke;break}");
      Crafter_Button :=
        Create
          (pathName => Craft_Dialog & ".selectedworker",
           options =>
             "-text {Assign selected member} -variable craftworker -value fromlist");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Crafter_Button,
         Options => "-columnspan 2 -padx 5 -sticky w");
      Add
        (Widget => Crafter_Button,
         Message =>
           "Assign the crew member from the list.\nThe sign + after name means that this crew member has\nneeded skill, the sign ++ after name means that his/her\nneeded skill is the best in the crew.");
      Bind
        (Widgt => Crafter_Button, Sequence => "<Tab>",
         Script => "{focus " & Crew_Box & ";break}");
      Bind
        (Widgt => Crafter_Button, Sequence => "<Escape>",
         Script => "{" & Craft_Dialog & ".cancel invoke;break}");
      Show_Members_List_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Append
           (Source => Crew_List,
            New_Item =>
              " {" & To_String(Source => Player_Ship.Crew(I).Name) &
              Get_Skill_Marks
                (Skill_Index => Recipe.Skill,
                 Member_Index => Crew_Container.To_Index(Position => I)) &
              "}");
      end loop Show_Members_List_Loop;
      configure
        (Widgt => Crew_Box,
         options => "-values [list" & To_String(Source => Crew_List) & "]");
      Current(ComboBox => Crew_Box, NewIndex => "0");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Crew_Box, Options => "-columnspan 2 -padx 5");
      Add
        (Widget => Crew_Box,
         Message =>
           "Assign the crew member from the list.\nThe sign + after name means that this crew member has\nneeded skill, the sign ++ after name means that his/her\nneeded skill is the best in the crew.");
      Bind
        (Widgt => Crew_Box, Sequence => "<Tab>",
         Script => "{focus " & Craft_Dialog & ".craft;break}");
      Bind
        (Widgt => Crew_Box, Sequence => "<Escape>",
         Script => "{" & Craft_Dialog & ".cancel invoke;break}");
      Button_Row := Button_Row + 4;
      Button :=
        Create
          (pathName => Craft_Dialog & ".craft",
           options =>
             "-text {" & Recipe_Type & "} -command {SetCrafting {" &
             CArgv.Arg(Argv => Argv, N => 1) & "};CloseDialog " &
             Craft_Dialog & "} -image " & To_Lower(Item => Recipe_Type) &
             "2icon -style Dialoggreen.TButton");
      Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-pady 5 -padx 5");
      Add(Widget => Button, Message => "Set the crafting order.");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Craft_Dialog & ".cancel invoke;break}");
      Button :=
        Create
          (pathName => Craft_Dialog & ".cancel",
           options =>
             "-text {Cancel} -command {CloseDialog " & Craft_Dialog &
             "} -image cancelicon -style Dialogred.TButton");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button,
         Options =>
           "-pady 5 -padx 5 -column 1 -row" & Positive'Image(Button_Row));
      Add
        (Widget => Button,
         Message =>
           "Cancel setting the order and close dialog. \[Escape key\]");
      Bind
        (Widgt => Button, Sequence => "<Tab>",
         Script =>
           "{focus " & Craft_Dialog & To_String(Source => First_Focus) &
           ";break}");
      Bind
        (Widgt => Button, Sequence => "<Escape>",
         Script => "{" & Button & " invoke;break}");
      Show_Dialog(Dialog => Craft_Dialog);
      Focus(Widgt => Button);
      return TCL_OK;
   end Show_Set_Recipe_Command;

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
      use Tcl.Tk.Ada.Widgets.Text;
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
      if CArgv.Arg(Argv => Argv, N => 2) = "TRUE" then
         Add_Buttons_Block :
         declare
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
      Convention => C;
      -- ****

   function Set_Crafting_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Crew;
      use Maps.UI;
      use Ships.Crew;
      use Tiny_String;

      Recipe_Index: Bounded_String :=
        To_Bounded_String(Source => CArgv.Arg(Argv => Argv, N => 1));
      Modules_Box: constant Ttk_ComboBox :=
        Get_Widget(pathName => ".craftdialog.workshop");
      Amount_Box: constant Ttk_SpinBox :=
        Get_Widget(pathName => ".craftdialog.amount", Interp => Interp);
      Members_Box: constant Ttk_ComboBox :=
        Get_Widget(pathName => ".craftdialog.members", Interp => Interp);
      Assign_Worker: constant String :=
        Tcl_GetVar(interp => Interp, varName => "craftworker");
      Workshop_Index: Natural :=
        Natural'Value(Current(ComboBox => Modules_Box)) + 1;
   begin
      if Element(Source => Recipe_Index, Index => 1) = '{' then
         Recipe_Index :=
           Bounded_Slice
             (Source => Recipe_Index, Low => 2,
              High => Length(Source => Recipe_Index) - 1);
      end if;
      Set_Module_Loop :
      for I in
        Player_Ship.Modules.First_Index .. Player_Ship.Modules.Last_Index loop
         if Player_Ship.Modules(I).Name =
           To_Bounded_String(Source => Get(Widgt => Modules_Box)) then
            Workshop_Index := Workshop_Index - 1;
         end if;
         if Workshop_Index = 0 then
            Set_Recipe
              (Workshop => I,
               Amount => Positive'Value(Get(Widgt => Amount_Box)),
               Recipe_Index => Recipe_Index);
            if Assign_Worker = "fromlist" then
               Give_Orders
                 (Ship => Player_Ship,
                  Member_Index =>
                    Positive'Value(Current(ComboBox => Members_Box)) + 1,
                  Given_Order => CRAFT, Module_Index => I);
            elsif Assign_Worker = "best" then
               Assing_Best_Worker_Block :
               declare
                  Recipe: constant Craft_Data :=
                    Set_Recipe_Data(Recipe_Index => Recipe_Index);
                  Worker_Assigned: Boolean := False;
               begin
                  Set_Best_Worker_Loop :
                  for J in Player_Ship.Crew.Iterate loop
                     if Get_Skill_Marks
                         (Skill_Index => Recipe.Skill,
                          Member_Index =>
                            Crew_Container.To_Index(Position => J)) =
                       " ++" then
                        Give_Orders
                          (Ship => Player_Ship,
                           Member_Index =>
                             Crew_Container.To_Index(Position => J),
                           Given_Order => CRAFT, Module_Index => I);
                        Worker_Assigned := True;
                        exit Set_Best_Worker_Loop;
                     end if;
                  end loop Set_Best_Worker_Loop;
                  if not Worker_Assigned then
                     Give_Orders
                       (Ship => Player_Ship, Member_Index => 1,
                        Given_Order => CRAFT, Module_Index => I);
                  end if;
               end Assing_Best_Worker_Block;
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
     (NAMEASC, NAMEDESC, WORKPLACEASC, WORKPLACEDESC, TOOLSASC, TOOLSDESC,
      MATERIALSASC, MATERIALSDESC, NONE) with
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

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* CUI4/CUI4.Recipes_Sort_Order
   -- FUNCTION
   -- The current sorting order for crafting recipes list
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Recipes_Sort_Order: Recipes_Sort_Orders := Default_Recipes_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

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
      Convention => C;
      -- ****

   function Sort_Crafting_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Column: constant Positive :=
        Get_Column_Number
          (Table => Recipes_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 1)));
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      --## rule off TYPE_INITIAL_VALUES
      type Local_Module_Data is record
         Name: Unbounded_String;
         Workplace: Boolean;
         Tool: Boolean;
         Materials: Boolean;
         Id: Bounded_String;
      end record;
      type Recipes_Array is array(Positive range <>) of Local_Module_Data;
      --## rule on TYPE_INITIAL_VALUES
      Can_Craft, Has_Tool, Has_Materials, Has_Workplace: Boolean := False;
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Local_Module_Data) return Boolean is
      begin
         if Recipes_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Recipes_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
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
            if Recipes_Sort_Order = WORKPLACEASC then
               Recipes_Sort_Order := WORKPLACEDESC;
            else
               Recipes_Sort_Order := WORKPLACEASC;
            end if;
         when 3 =>
            if Recipes_Sort_Order = TOOLSASC then
               Recipes_Sort_Order := TOOLSDESC;
            else
               Recipes_Sort_Order := TOOLSASC;
            end if;
         when 4 =>
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
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Sort_Known_Recipes_Block :
      declare
         --## rule off IMPROPER_INITIALIZATION
         Local_Recipes: Recipes_Array(0 .. Get_Known_Recipes_Amount - 1);
         --## rule on IMPROPER_INITIALIZATION
         procedure Sort_Recipes is new Ada.Containers.Generic_Array_Sort
           (Index_Type => Positive, Element_Type => Local_Module_Data,
            Array_Type => Recipes_Array);
      begin
         Set_Local_Recipes_Loop :
         for I in Local_Recipes'Range loop
            Is_Craftable
              (Recipe =>
                 Get_Recipe
                   (Recipe_Index =>
                      To_Bounded_String
                        (Source =>
                           To_String(Source => Get_Known_Recipe(Index => I)))),
               Can_Craft => Can_Craft, Has_Workplace => Has_Workplace,
               Has_Tool => Has_Tool, Has_Materials => Has_Materials);
            Local_Recipes(I) :=
              (Name =>
                 To_Unbounded_String
                   (Source =>
                      To_String
                        (Source =>
                           Get_Proto_Item
                             (Index =>
                                Get_Recipe
                                  (Recipe_Index =>
                                     To_Bounded_String
                                       (Source =>
                                          To_String
                                            (Source =>
                                               Get_Known_Recipe(Index => I))))
                                  .Result_Index)
                             .Name)),
               Workplace => Has_Workplace, Tool => Has_Tool,
               Materials => Has_Materials, Id => Get_Known_Recipe(Index => I));
         end loop Set_Local_Recipes_Loop;
         Sort_Recipes(Container => Local_Recipes);
         --## rule off DIRECTLY_ACCESSED_GLOBALS
         Recipes_Indexes.Clear;
         Set_Recipes_Indexes_Loop :
         for Recipe of Local_Recipes loop
            Recipes_Indexes.Append(New_Item => Recipe.Id);
         end loop Set_Recipes_Indexes_Loop;
         --## rule off DIRECTLY_ACCESSED_GLOBALS
      end Sort_Known_Recipes_Block;
      Check_Study_Prerequisites
        (Can_Craft => Can_Craft, Has_Tool => Has_Tool,
         Has_Workplace => Has_Workplace);
      Sort_Studying_Recipes_Block :
      declare
         --## rule off DIRECTLY_ACCESSED_GLOBALS
         --## rule off IMPROPER_INITIALIZATION
         Local_Recipes: Recipes_Array(1 .. Positive(Studies.Length));
         --## rule on IMPROPER_INITIALIZATION
         procedure Sort_Recipes is new Ada.Containers.Generic_Array_Sort
           (Index_Type => Positive, Element_Type => Local_Module_Data,
            Array_Type => Recipes_Array);
      begin
         Set_Local_Studies_Loop :
         for I in Studies.Iterate loop
            Local_Recipes(Positive_Container.To_Index(Position => I)) :=
              (Name =>
                 To_Unbounded_String
                   (Source =>
                      To_String
                        (Source => Get_Proto_Item(Index => Studies(I)).Name)),
               Tool => Has_Tool, Workplace => Has_Workplace, Materials => True,
               Id =>
                 To_Bounded_String
                   (Source =>
                      Trim
                        (Source => Positive'Image(Studies(I)), Side => Left)));
         end loop Set_Local_Studies_Loop;
         Sort_Recipes(Container => Local_Recipes);
         Set_Studies_Indexes_Loop :
         for Recipe of Local_Recipes loop
            Recipes_Indexes.Append(New_Item => Recipe.Id);
         end loop Set_Studies_Indexes_Loop;
         --## rule on DIRECTLY_ACCESSED_GLOBALS
      end Sort_Studying_Recipes_Block;
      Sort_Deconstruct_Recipes_Block :
      declare
         --## rule off DIRECTLY_ACCESSED_GLOBALS
         --## rule off IMPROPER_INITIALIZATION
         Local_Recipes: Recipes_Array(1 .. Positive(Deconstructs.Length));
         --## rule on IMPROPER_INITIALIZATION
         procedure Sort_Recipes is new Ada.Containers.Generic_Array_Sort
           (Index_Type => Positive, Element_Type => Local_Module_Data,
            Array_Type => Recipes_Array);
      begin
         Set_Local_Deconstruct_Loop :
         for I in Deconstructs.Iterate loop
            Local_Recipes(Positive_Container.To_Index(Position => I)) :=
              (Name =>
                 To_Unbounded_String
                   (Source =>
                      To_String
                        (Source =>
                           Get_Proto_Item(Index => Deconstructs(I)).Name)),
               Workplace => Has_Workplace, Tool => Has_Tool, Materials => True,
               Id =>
                 To_Bounded_String
                   (Source =>
                      Trim
                        (Source => Positive'Image(Deconstructs(I)),
                         Side => Left)));
         end loop Set_Local_Deconstruct_Loop;
         Sort_Recipes(Container => Local_Recipes);
         Set_Deconstruct_Indexes_Loop :
         for Recipe of Local_Recipes loop
            Recipes_Indexes.Append(New_Item => Recipe.Id);
         end loop Set_Deconstruct_Indexes_Loop;
         --## rule on DIRECTLY_ACCESSED_GLOBALS
      end Sort_Deconstruct_Recipes_Block;
      return
        Show_Crafting_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv => CArgv.Empty & "ShowCrafting" & "1");
   end Sort_Crafting_Command;

   procedure Add_Commands is
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
