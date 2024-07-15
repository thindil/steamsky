-- Copyright (c) 2020-2023 Bartek thindil Jasicki
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

-- with Ada.Characters.Handling;
with Ada.Containers.Generic_Array_Sort;
with Ada.Strings; use Ada.Strings;
with Interfaces.C; use Interfaces.C;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- with GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
-- with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
-- with Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkButton;
-- with Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
-- with Tcl.Tk.Ada.Widgets.TtkLabel;
-- with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
-- with Tcl.Tk.Ada.Winfo;
with Bases.Ship; use Bases.Ship;
with Bases.Trade; use Bases.Trade;
with BasesTypes; use BasesTypes;
with Config; use Config;
with CoreUI;
with Crafts; use Crafts;
with Dialogs;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships.Crew; use Ships.Crew;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Bases.UI is

   -- ****iv* BUI/BUI.Base_Table
   -- FUNCTION
   -- Table with info about available base actions
   -- SOURCE
   Base_Table: Table_Widget (Amount => 3);
   -- ****

   -- ****iv* BUI/BUI.Items_Indexes
   -- FUNCTION
   -- Indexes of the crafting recipes/wounded crew members/damaged ship modules
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Items_Indexes: UnboundedString_Container.Vector;
   -- ****

   -- ****o* BUI/BUI.Show_Base_Ui_Command
   -- FUNCTION
   -- Show the selected base action
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBaseUI UIType search page
   -- UIType can be heal, repair, recipes. Search is a string which will be
   -- looked for in names of recipes (only). Page is the number of current
   -- page on the list to show
   -- SOURCE
   function Show_Base_Ui_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Ui_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      use Ada.Characters.Handling;
--      use GNAT.Directory_Operations;
--      use Tcl.Ada;
--      use Tcl.Tk.Ada.Widgets.Canvas;
--      use Tcl.Tk.Ada.Widgets.TtkEntry;
--      use Tcl.Tk.Ada.Widgets.TtkLabel;
--      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
--      use Tcl.Tk.Ada.Winfo;
      use CoreUI;
--      use Tiny_String;

      Base_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".baseframe", Interp => Interp);
--      Base_Canvas: constant Tk_Canvas :=
--        Get_Widget(pathName => Base_Frame & ".canvas", Interp => Interp);
--      Search_Frame: constant Ttk_Frame :=
--        Get_Widget
--          (pathName => Base_Canvas & ".base.searchframe", Interp => Interp);
--      Search_Entry: constant Ttk_Entry :=
--        Get_Widget(pathName => Search_Frame & ".search", Interp => Interp);
--      First_Index, Formatted_Time: Unbounded_String := Null_Unbounded_String;
      Base_Index: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
--      Base_Type: constant Bounded_String := Sky_Bases(Base_Index).Base_Type;
--      Cost, Time: Natural := 0;
--      Money_Index_2: constant Natural :=
--        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
--      Money_Label: constant Ttk_Label :=
--        Get_Widget
--          (pathName => Base_Canvas & ".base.lblmoney", Interp => Interp);
--      Page: constant Positive :=
--        (if Argc = 4 then Positive'Value(CArgv.Arg(Argv => Argv, N => 3))
--         else 1);
--      --## rule off SIMPLIFIABLE_EXPRESSIONS
--      Start_Row: constant Positive :=
--        ((Page - 1) * Get_Integer_Setting(Name => "listsLimit")) + 1;
--      --## rule on SIMPLIFIABLE_EXPRESSIONS
--      Arguments: constant String :=
--        (if Argc > 2 then
--           "{" & CArgv.Arg(Argv => Argv, N => 1) & "} {" &
--           CArgv.Arg(Argv => Argv, N => 2) & "}"
--         else CArgv.Arg(Argv => Argv, N => 1) & " {}");
--      Current_Row: Positive := 1;
--      procedure Format_Time is
--      begin
--         if Time < 60 then
--            Formatted_Time :=
--              To_Unbounded_String(Source => Natural'Image(Time) & " minute");
--            if Time > 1 then
--               Append(Source => Formatted_Time, New_Item => "s");
--            end if;
--         else
--            Formatted_Time :=
--              To_Unbounded_String
--                (Source => Positive'Image(Time / 60) & " hour");
--            if Time / 60 > 1 then
--               Append(Source => Formatted_Time, New_Item => "s");
--            end if;
--            if Time mod 60 > 0 then
--               Append
--                 (Source => Formatted_Time,
--                  New_Item =>
--                    " and" & Positive'Image(Time mod 60) & " minute");
--               if Time mod 60 > 1 then
--                  Append(Source => Formatted_Time, New_Item => "s");
--               end if;
--            end if;
--         end if;
--      end Format_Time;
--      function Get_Color(Action_Cost: Positive) return String is
--      begin
--         if Money_Index_2 = 0
--           or else
--             Inventory_Container.Element
--               (Container => Player_Ship.Cargo, Index => Money_Index_2)
--               .Amount <
--             Action_Cost then
--            return "red";
--         end if;
--         return "";
--      end Get_Color;
      function Show_Ada_Base_Ui_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Import => True,
         Convention => C,
         External_Name => "showBaseUiCommand";
   begin
--      if Winfo_Get(Widgt => Base_Canvas, Info => "exists") = "0" then
--         Tcl_EvalFile
--           (interp => Get_Context,
--            fileName =>
--              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
--              "base.tcl");
--         Bind
--           (Widgt => Base_Frame, Sequence => "<Configure>",
--            Script => "{ResizeCanvas %W.canvas %w %h}");
--      elsif Winfo_Get(Widgt => Base_Canvas, Info => "ismapped") = "1" and
--        Argc = 1 then
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
--         Show_Sky_Map(Clear => True);
--         return TCL_OK;
--      end if;
--      Base_Frame.Name := New_String(Str => Base_Canvas & ".base");
--      if Winfo_Get
--          (Widgt => Ttk_Frame'(Get_Widget(pathName => Base_Frame & ".table")),
--           Info => "exists") =
--        "1" then
--         Destroy(Widgt => Base_Table.Canvas);
--      end if;
      if Show_Ada_Base_Ui_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv) =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
      if CArgv.Arg(Argv => Argv, N => 1) = "recipes" then
--         Tcl.Tk.Ada.Grid.Grid(Slave => Search_Frame);
--         if Argc /= 3 then
--            configure(Widgt => Search_Entry, options => "-validatecommand {}");
--            Delete
--              (TextEntry => Search_Entry, FirstIndex => "0",
--               LastIndex => "end");
--            configure
--              (Widgt => Search_Entry,
--               options => "-validatecommand {SearchRecipes %P}");
--         end if;
         Base_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Base_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Cost"),
                 3 => Null_Unbounded_String),
              Scrollbar =>
                Get_Widget(pathName => Main_Paned & ".baseframe.scrolly"),
              Command => "SortBaseItems " & CArgv.Arg(Argv => Argv, N => 1),
              Tooltip_Text => "Press mouse button to sort the recipes.");
         if Natural(Items_Indexes.Length) /= Get_Recipes_Amount then
            Items_Indexes.Clear;
            Fill_Recipes_Indexes_Loop :
            for I in 1 .. Get_Recipes_Amount loop
               Items_Indexes.Append
                 (New_Item =>
                    To_Unbounded_String
                      (Source => Trim(Source => I'Img, Side => Both)));
            end loop Fill_Recipes_Indexes_Loop;
         end if;
      else
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Search_Frame);
         Base_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Base_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Action"),
                 2 => To_Unbounded_String(Source => "Cost"),
                 3 => To_Unbounded_String(Source => "Time")),
              Scrollbar =>
                Get_Widget(pathName => Main_Paned & ".baseframe.scrolly"),
              Command => "SortBaseItems " & CArgv.Arg(Argv => Argv, N => 1),
              Tooltip_Text => "Press mouse button to sort the actions.");
         if CArgv.Arg(Argv => Argv, N => 1) = "heal"
           and then Items_Indexes.Length /= Player_Ship.Crew.Length + 1 then
            Items_Indexes.Clear;
            Fill_Heal_Indexes_Loop :
            for I in Player_Ship.Crew.Iterate loop
               Items_Indexes.Append
                 (New_Item =>
                    To_Unbounded_String
                      (Source =>
                         Positive'Image
                           (Crew_Container.To_Index(Position => I))));
            end loop Fill_Heal_Indexes_Loop;
            Items_Indexes.Append
              (New_Item => To_Unbounded_String(Source => "0"));
         elsif CArgv.Arg(Argv => Argv, N => 1) = "repair"
           and then Items_Indexes.Length /= Player_Ship.Modules.Length + 3 then
            Items_Indexes.Clear;
            Fill_Repair_Indexes_Loop :
            for I in Player_Ship.Modules.Iterate loop
               Items_Indexes.Append
                 (New_Item =>
                    To_Unbounded_String
                      (Source =>
                         Positive'Image
                           (Modules_Container.To_Index(Position => I))));
            end loop Fill_Repair_Indexes_Loop;
            Items_Indexes.Append
              (New_Item => To_Unbounded_String(Source => "0"));
            Items_Indexes.Append
              (New_Item =>
                 To_Unbounded_String
                   (Source =>
                      (if Sky_Bases(Base_Index).Population > 149 then "-1"
                       else "-3")));
            Items_Indexes.Append
              (New_Item =>
                 To_Unbounded_String
                   (Source =>
                      (if Sky_Bases(Base_Index).Population > 299 then "-2"
                       else "-3")));
         end if;
      end if;
      return TCL_OK;
--      if Money_Index_2 > 0 then
--         configure
--           (Widgt => Money_Label,
--            options =>
--              "-text {You have" &
--              Natural'Image
--                (Inventory_Container.Element
--                   (Container => Player_Ship.Cargo, Index => Money_Index_2)
--                   .Amount) &
--              " " & To_String(Source => Money_Name) & ".}");
--      else
--         configure
--           (Widgt => Money_Label,
--            options =>
--              "-text {You don't have any " & To_String(Source => Money_Name) &
--              " to buy anything.}");
--      end if;
--      if CArgv.Arg(Argv => Argv, N => 1) = "heal" then
--         Show_Wounded_Crew_Loop :
--         for I of Items_Indexes loop
--            if Integer'Value(To_String(Source => I)) > 0 then
--               if Player_Ship.Crew(Positive'Value(To_String(Source => I)))
--                   .Health =
--                 100 then
--                  goto End_Of_Wounded_Loop;
--               end if;
--               if First_Index = Null_Unbounded_String then
--                  First_Index := I;
--               end if;
--            end if;
--            if Current_Row < Start_Row then
--               Current_Row := Current_Row + 1;
--               goto End_Of_Wounded_Loop;
--            end if;
--            Cost := 0;
--            Time := 0;
--            Heal_Cost
--              (Cost => Cost, Time => Time,
--               Member_Index => Positive'Value(To_String(Source => I)));
--            Add_Button
--              (Table => Base_Table,
--               Text =>
--                 (if Integer'Value(To_String(Source => I)) > 0 then
--                    To_String
--                      (Source =>
--                         Player_Ship.Crew
--                           (Positive'Value(To_String(Source => I)))
--                           .Name)
--                  else "Heal all wounded crew members"),
--               Tooltip => "Show available options",
--               Command => "ShowBaseMenu heal " & To_String(Source => I),
--               Column => 1);
--            Add_Button
--              (Table => Base_Table,
--               Text =>
--                 Positive'Image(Cost) & " " & To_String(Source => Money_Name),
--               Tooltip => "Show available options",
--               Command => "ShowBaseMenu heal " & To_String(Source => I),
--               Column => 2, Color => Get_Color(Action_Cost => Cost));
--            Format_Time;
--            Add_Button
--              (Table => Base_Table,
--               Text => To_String(Source => Formatted_Time),
--               Tooltip => "Show available options",
--               Command => "ShowBaseMenu heal " & To_String(Source => I),
--               Column => 3, New_Row => True);
--            exit Show_Wounded_Crew_Loop when Base_Table.Row =
--              Get_Integer_Setting(Name => "listsLimit") + 1;
--            <<End_Of_Wounded_Loop>>
--         end loop Show_Wounded_Crew_Loop;
--      elsif CArgv.Arg(Argv => Argv, N => 1) = "repair" then
--         Show_Damaged_Modules_Loop :
--         for I of Items_Indexes loop
--            if Integer'Value(To_String(Source => I)) > 0 then
--               if Player_Ship.Modules(Positive'Value(To_String(Source => I)))
--                   .Durability =
--                 Player_Ship.Modules(Positive'Value(To_String(Source => I)))
--                   .Max_Durability then
--                  goto End_Of_Damaged_Modules_Loop;
--               end if;
--               if First_Index = Null_Unbounded_String then
--                  First_Index := I;
--               end if;
--            end if;
--            if Current_Row < Start_Row then
--               Current_Row := Current_Row + 1;
--               goto End_Of_Damaged_Modules_Loop;
--            end if;
--            if I = To_Unbounded_String(Source => "-3") then
--               goto End_Of_Damaged_Modules_Loop;
--            end if;
--            Cost := 0;
--            Time := 0;
--            Repair_Cost
--              (Cost => Cost, Time => Time,
--               Module_Index => Integer'Value(To_String(Source => I)));
--            Count_Price
--              (Price => Cost, Trader_Index => Find_Member(Order => TALK));
--            Add_Button
--              (Table => Base_Table,
--               Text =>
--                 (case Integer'Value(To_String(Source => I)) is
--                    when 0 => "Slowly repair the whole ship",
--                    when -1 => "Repair the whole ship",
--                    when -2 => "Quickly repair the whole ship",
--                    when others =>
--                      To_String
--                        (Source =>
--                           Player_Ship.Modules
--                             (Positive'Value(To_String(Source => I)))
--                             .Name)),
--               Tooltip => "Show available options",
--               Command => "ShowBaseMenu repair " & To_String(Source => I),
--               Column => 1);
--            Add_Button
--              (Table => Base_Table,
--               Text =>
--                 Positive'Image(Cost) & " " & To_String(Source => Money_Name),
--               Tooltip => "Show available options",
--               Command => "ShowBaseMenu repair " & To_String(Source => I),
--               Column => 2, Color => Get_Color(Action_Cost => Cost));
--            Format_Time;
--            Add_Button
--              (Table => Base_Table,
--               Text => To_String(Source => Formatted_Time),
--               Tooltip => "Show available options",
--               Command => "ShowBaseMenu repair " & To_String(Source => I),
--               Column => 3, New_Row => True);
--            exit Show_Damaged_Modules_Loop when Base_Table.Row =
--              Get_Integer_Setting(Name => "listsLimit") + 1;
--            <<End_Of_Damaged_Modules_Loop>>
--         end loop Show_Damaged_Modules_Loop;
--      elsif CArgv.Arg(Argv => Argv, N => 1) = "recipes" then
--         Show_Available_Recipes_Loop :
--         for I of Items_Indexes loop
--            if not Has_Recipe
--                (Base_Type => Base_Type, Recipe => To_String(Source => I)) or
--              Is_Known_Recipe
--                (Recipe_Index =>
--                   To_Bounded_String(Source => To_String(Source => I))) or
--              Get_Recipe
--                  (Recipe_Index =>
--                     To_Bounded_String(Source => To_String(Source => I)))
--                  .Reputation >
--                Sky_Bases(Base_Index).Reputation.Level then
--               goto End_Of_Recipes_Loop;
--            end if;
--            if Argc > 2 and then CArgv.Arg(Argv => Argv, N => 2)'Length > 0
--              and then
--                Index
--                  (Source =>
--                     To_Lower
--                       (Item =>
--                          To_String
--                            (Source =>
--                               Get_Proto_Item
--                                 (Index =>
--                                    Get_Recipe
--                                      (Recipe_Index =>
--                                         To_Bounded_String
--                                           (Source => To_String(Source => I)))
--                                      .Result_Index)
--                                 .Name)),
--                   Pattern =>
--                     To_Lower(Item => CArgv.Arg(Argv => Argv, N => 2))) =
--                0 then
--               goto End_Of_Recipes_Loop;
--            end if;
--            if First_Index = Null_Unbounded_String then
--               First_Index := I;
--            end if;
--            if Current_Row < Start_Row then
--               Current_Row := Current_Row + 1;
--               goto End_Of_Recipes_Loop;
--            end if;
--            Add_Button
--              (Table => Base_Table,
--               Text =>
--                 To_String
--                   (Source =>
--                      Get_Proto_Item
--                        (Index =>
--                           Get_Recipe
--                             (Recipe_Index =>
--                                To_Bounded_String
--                                  (Source => To_String(Source => I)))
--                             .Result_Index)
--                        .Name),
--               Tooltip => "Show available options",
--               Command =>
--                 "ShowBaseMenu recipes {" & To_String(Source => I) & "}",
--               Column => 1);
--            Cost :=
--              (if
--                 Get_Price
--                   (Base_Type => Sky_Bases(Base_Index).Base_Type,
--                    Item_Index =>
--                      Get_Recipe
--                        (Recipe_Index =>
--                           To_Bounded_String(Source => To_String(Source => I)))
--                        .Result_Index) >
--                 0
--               then
--                 Get_Price
--                   (Base_Type => Sky_Bases(Base_Index).Base_Type,
--                    Item_Index =>
--                      Get_Recipe
--                        (Recipe_Index =>
--                           To_Bounded_String(Source => To_String(Source => I)))
--                        .Result_Index) *
--                 Get_Recipe
--                   (Recipe_Index =>
--                      To_Bounded_String(Source => To_String(Source => I)))
--                   .Difficulty *
--                 10
--               else Get_Recipe
--                   (Recipe_Index =>
--                      To_Bounded_String(Source => To_String(Source => I)))
--                   .Difficulty *
--                 10);
--            --## rule off ASSIGNMENTS
--            Cost :=
--              Natural(Float(Cost) * Get_Float_Setting(Name => "pricesBonus"));
--            --## rule on ASSIGNMENTS
--            if Cost = 0 then
--               Cost := 1;
--            end if;
--            Count_Price
--              (Price => Cost, Trader_Index => Find_Member(Order => TALK));
--            Add_Button
--              (Table => Base_Table,
--               Text =>
--                 Positive'Image(Cost) & " " & To_String(Source => Money_Name),
--               Tooltip => "Show available options",
--               Command =>
--                 "ShowBaseMenu recipes {" & To_String(Source => I) & "}",
--               Column => 2, New_Row => True,
--               Color => Get_Color(Action_Cost => Cost));
--            exit Show_Available_Recipes_Loop when Base_Table.Row =
--              Get_Integer_Setting(Name => "listsLimit") + 1;
--            <<End_Of_Recipes_Loop>>
--         end loop Show_Available_Recipes_Loop;
--      end if;
--      Add_Pagination
--        (Table => Base_Table,
--         Previous_Command =>
--           (if Page > 1 then
--              "ShowBaseUI " & Arguments & Positive'Image(Page - 1)
--            else ""),
--         Next_Command =>
--           (if Base_Table.Row < Get_Integer_Setting(Name => "listsLimit") + 1
--            then ""
--            else "ShowBaseUI " & Arguments & Positive'Image(Page + 1)));
--      Update_Table
--        (Table => Base_Table,
--         Grab_Focus =>
--           (if Focus = Widget_Image(Win => Search_Entry) then False));
--      if First_Index = Null_Unbounded_String and Argc < 3 then
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
--         Show_Sky_Map(Clear => True);
--         return TCL_OK;
--      end if;
--      Tcl.Tk.Ada.Grid.Grid
--        (Slave => Close_Button, Options => "-row 0 -column 1");
--      Base_Frame.Name := New_String(Str => Base_Canvas & ".base");
--      configure
--        (Widgt => Base_Canvas,
--         options =>
--           "-height [expr " & SashPos(Paned => Main_Paned, Index => "0") &
--           " - 20] -width " & cget(Widgt => Main_Paned, option => "-width"));
--      Tcl_Eval(interp => Get_Context, strng => "update");
--      Canvas_Create
--        (Parent => Base_Canvas, Child_Type => "window",
--         Options => "0 0 -anchor nw -window " & Base_Frame);
--      Tcl_Eval(interp => Get_Context, strng => "update");
--      configure
--        (Widgt => Base_Canvas,
--         options =>
--           "-scrollregion [list " &
--           BBox(CanvasWidget => Base_Canvas, TagOrId => "all") & "]");
--      Show_Screen(New_Screen_Name => "baseframe");
--      Tcl_SetResult(interp => Interp, str => "1");
--      return TCL_OK;
   end Show_Base_Ui_Command;

   -- ****o* BUI/BUI.Base_Action_Command
   -- FUNCTION
   -- Execute the selected action
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- BaseAction ActionType
   -- ActionType can be heal, repair, recipes
   -- SOURCE
   function Base_Action_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Base_Action_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      Item_Index: constant String := CArgv.Arg(Argv => Argv, N => 2);
   begin
      if CArgv.Arg(Argv => Argv, N => 1) = "heal" then
         Heal_Wounded(Member_Index => Natural'Value(Item_Index));
      elsif CArgv.Arg(Argv => Argv, N => 1) = "repair" then
         Bases.Ship.Repair_Ship(Module_Index => Integer'Value(Item_Index));
      elsif CArgv.Arg(Argv => Argv, N => 1) = "recipes" then
         Buy_Recipe(Recipe_Index => To_Bounded_String(Source => Item_Index));
      end if;
      Update_Header;
      Update_Messages;
      return
        Show_Base_Ui_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv =>
             CArgv.Empty & "ShowBaseUI" & CArgv.Arg(Argv => Argv, N => 1));
   end Base_Action_Command;

   -- ****o* BUI/BUI.Search_Recipes_Command
   -- FUNCTION
   -- Show only this recipes which contains the selected sequence
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SearchRecipes TextToSearch
   -- SOURCE
   function Search_Recipes_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Recipes_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      Search_Text: constant String := CArgv.Arg(Argv => Argv, N => 1);
   begin
      if Search_Text'Length = 0 then
         return
           Show_Base_Ui_Command
             (Client_Data => Client_Data, Interp => Interp, Argc => 2,
              Argv => CArgv.Empty & "ShowBaseUI" & "recipes");
      end if;
      return
        Show_Base_Ui_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 3,
           Argv => CArgv.Empty & "ShowBaseUI" & "recipes" & Search_Text);
   end Search_Recipes_Command;

   -- ****o* BUI/BUI.Show_Base_Menu_Command
   -- FUNCTION
   -- Show menu with options for the selected item
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBaseMenu action index
   -- Action is name of action (heal,repair or recipe) and index is the index
   -- of the item
   -- SOURCE
   function Show_Base_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Dialogs;
      use Tiny_String;

      Cost, Time: Natural := 0;
      Base_Index: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Money_Index_2: constant Natural :=
        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
      Action: constant String := CArgv.Arg(Argv => Argv, N => 1);
      Item_Index: constant String := CArgv.Arg(Argv => Argv, N => 2);
      Base_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".basemenu", Title => "Actions", Parent_Name => ".");
      procedure Add_Button(Name, Label, Command: String) is
         use Tcl.Tk.Ada.Widgets.TtkButton;

         Button: constant Ttk_Button :=
           Create
             (pathName => Base_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Base_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Base_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Base_Menu & ".action;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      if Action = "heal" then
         Heal_Cost
           (Cost => Cost, Time => Time,
            Member_Index => Integer'Value(Item_Index));
      elsif Action = "repair" then
         Repair_Cost
           (Cost => Cost, Time => Time,
            Module_Index => Integer'Value(Item_Index));
         Count_Price
           (Price => Cost, Trader_Index => Find_Member(Order => TALK));
      else
         Cost :=
           (if
              Get_Price
                (Base_Type => Sky_Bases(Base_Index).Base_Type,
                 Item_Index =>
                   Get_Recipe
                     (Recipe_Index => To_Bounded_String(Source => Item_Index))
                     .Result_Index) >
              0
            then
              Get_Price
                (Base_Type => Sky_Bases(Base_Index).Base_Type,
                 Item_Index =>
                   Get_Recipe
                     (Recipe_Index => To_Bounded_String(Source => Item_Index))
                     .Result_Index) *
              Get_Recipe
                (Recipe_Index => To_Bounded_String(Source => Item_Index))
                .Difficulty *
              10
            else Get_Recipe
                (Recipe_Index => To_Bounded_String(Source => Item_Index))
                .Difficulty *
              10);
         --## rule off ASSIGNMENTS
         Cost :=
           Natural(Float(Cost) * Get_Float_Setting(Name => "pricesBonus"));
         --## rule on ASSIGNMENTS
         if Cost = 0 then
            Cost := 1;
         end if;
         Count_Price
           (Price => Cost, Trader_Index => Find_Member(Order => TALK));
      end if;
      if Money_Index_2 = 0
        or else
          Inventory_Container.Element
            (Container => Player_Ship.Cargo, Index => Money_Index_2)
            .Amount <
          Cost then
         Add_Button
           (Name => ".action", Label => "You don't have money for this",
            Command => "");
      else
         Add_Button
           (Name => ".action",
            Label =>
              (if Action = "heal" then "Buy healing"
               elsif Action = "repair" then "Buy repair" else "Buy recipe"),
            Command => "BaseAction " & Action & " " & Item_Index);
         Add_Button(Name => ".close", Label => "Close", Command => "");
      end if;
      Show_Dialog(Dialog => Base_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Base_Menu_Command;

   -- ****it* BUI/BUI.Base_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the crafting recipes/healing/repair in bases
   -- OPTIONS
   -- NAMEASC  - Sort items by name ascending
   -- NAMEDESC - Sort items by name descending
   -- COSTASC  - Sort items by cost ascending
   -- COSTDESC - Sort items by cost descending
   -- TIMEASC  - Sort items by time ascending
   -- TIMEDESC - Sort items by time descending
   -- NONE     - No sorting items (default)
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   type Base_Sort_Orders is
     (NAMEASC, NAMEDESC, COSTASC, COSTDESC, TIMEASC, TIMEDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* BUI/BUI.Default_Base_Sort_Order
      -- FUNCTION
      -- Default sorting order for the items
      -- HISTORY
      -- 6.5 - Added
      -- SOURCE
   Default_Base_Sort_Order: constant Base_Sort_Orders := NONE;
   -- ****

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* BUI/BUI.Base_Sort_Order
   -- FUNCTION
   -- The current sorting order for items
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Base_Sort_Order: Base_Sort_Orders := Default_Base_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* BUI/BUI.Sort_Modules_Command
   -- FUNCTION
   -- Sort the list with recipes to buy/healing wounded/repair ship
   -- PARAMETERS
   -- Client_Data - Custom data send to the command.
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- HISTORY
   -- 6.5 - Added
   -- COMMANDS
   -- SortBaseItems x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Base_Items_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Base_Items_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Column: constant Positive :=
        Get_Column_Number
          (Table => Base_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 2)));
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      --## rule off TYPE_INITIAL_VALUES
      type Local_Item_Data is record
         Name: Unbounded_String;
         Cost: Positive;
         Time: Positive;
         Id: Unbounded_String;
      end record;
      type Items_Array is array(Positive range <>) of Local_Item_Data;
      --## rule on TYPE_INITIAL_VALUES
      Base_Index: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      --## rule off IMPROPER_INITIALIZATION
      Local_Items: Items_Array
        (1 ..
             (if CArgv.Arg(Argv => Argv, N => 1) = "recipes" then
                Get_Recipes_Amount
              elsif CArgv.Arg(Argv => Argv, N => 1) = "heal" then
                Positive(Player_Ship.Crew.Length) + 1
              else Positive(Player_Ship.Modules.Length) +
                (if Sky_Bases(Base_Index).Population > 299 then 3
                 elsif Sky_Bases(Base_Index).Population > 149 then 2 else 1)));
      --## rule on IMPROPER_INITIALIZATION
      Index: Positive := 1;
      Cost, Time: Natural := 0;
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Local_Item_Data) return Boolean is
      begin
         if Base_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Base_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Base_Sort_Order = COSTASC and then Left.Cost < Right.Cost then
            return True;
         end if;
         if Base_Sort_Order = COSTDESC and then Left.Cost > Right.Cost then
            return True;
         end if;
         if Base_Sort_Order = TIMEASC and then Left.Time < Right.Time then
            return True;
         end if;
         if Base_Sort_Order = TIMEDESC and then Left.Time > Right.Time then
            return True;
         end if;
         return False;
      end "<";
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      procedure Sort_Items is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Item_Data,
         Array_Type => Items_Array);
      procedure Count_Repair_Cost(I: Integer) is
      begin
         Cost := 0;
         Time := 0;
         Repair_Cost(Cost => Cost, Time => Time, Module_Index => I);
         Count_Price
           (Price => Cost, Trader_Index => Find_Member(Order => TALK));
      end Count_Repair_Cost;
   begin
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      case Column is
         when 1 =>
            if Base_Sort_Order = NAMEASC then
               Base_Sort_Order := NAMEDESC;
            else
               Base_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Base_Sort_Order = COSTASC then
               Base_Sort_Order := COSTDESC;
            else
               Base_Sort_Order := COSTASC;
            end if;
         when 3 =>
            if Base_Sort_Order = TIMEASC then
               Base_Sort_Order := TIMEDESC;
            else
               Base_Sort_Order := TIMEASC;
            end if;
         when others =>
            null;
      end case;
      if Base_Sort_Order = NONE then
         return TCL_OK;
      end if;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      if CArgv.Arg(Argv => Argv, N => 1) = "heal" then
         Fill_Heal_Items_Loop :
         for I in Player_Ship.Crew.Iterate loop
            Cost := 0;
            Time := 0;
            Heal_Cost
              (Cost => Cost, Time => Time,
               Member_Index => Crew_Container.To_Index(Position => I));
            Local_Items(Crew_Container.To_Index(Position => I)) :=
              (Name =>
                 To_Unbounded_String
                   (Source => To_String(Source => Player_Ship.Crew(I).Name)),
               Cost => Cost, Time => Time,
               Id =>
                 To_Unbounded_String
                   (Source =>
                      Positive'Image(Crew_Container.To_Index(Position => I))));
         end loop Fill_Heal_Items_Loop;
         Cost := 0;
         Time := 0;
         Heal_Cost(Cost => Cost, Time => Time, Member_Index => 0);
         Local_Items(Local_Items'Last) :=
           (Name =>
              To_Unbounded_String(Source => "Heal all wounded crew members"),
            Cost => Cost, Time => Time,
            Id => To_Unbounded_String(Source => "0"));
      elsif CArgv.Arg(Argv => Argv, N => 1) = "repair" then
         Fill_Repair_Items_Loop :
         for I in Player_Ship.Modules.Iterate loop
            Count_Repair_Cost(I => Modules_Container.To_Index(Position => I));
            Local_Items(Modules_Container.To_Index(Position => I)) :=
              (Name =>
                 To_Unbounded_String
                   (Source =>
                      To_String(Source => Player_Ship.Modules(I).Name)),
               Cost => Cost, Time => Time,
               Id =>
                 To_Unbounded_String
                   (Source =>
                      Positive'Image
                        (Modules_Container.To_Index(Position => I))));
         end loop Fill_Repair_Items_Loop;
         if Sky_Bases(Base_Index).Population > 299 then
            Count_Repair_Cost(I => 0);
            Local_Items(Local_Items'Last - 2) :=
              (Name =>
                 To_Unbounded_String(Source => "Slowly repair the whole ship"),
               Cost => Cost, Time => Time,
               Id => To_Unbounded_String(Source => "0"));
            Count_Repair_Cost(I => -1);
            Local_Items(Local_Items'Last - 1) :=
              (Name => To_Unbounded_String(Source => "Repair the whole ship"),
               Cost => Cost, Time => Time,
               Id => To_Unbounded_String(Source => "-1"));
            Count_Repair_Cost(I => -2);
            Local_Items(Local_Items'Last) :=
              (Name =>
                 To_Unbounded_String
                   (Source => "Quickly repair the whole ship"),
               Cost => Cost, Time => Time,
               Id => To_Unbounded_String(Source => "-2"));
         elsif Sky_Bases(Base_Index).Population > 149 then
            Count_Repair_Cost(I => 0);
            Local_Items(Local_Items'Last - 1) :=
              (Name =>
                 To_Unbounded_String(Source => "Slowly repair the whole ship"),
               Cost => Cost, Time => Time,
               Id => To_Unbounded_String(Source => "0"));
            Count_Repair_Cost(I => -1);
            Local_Items(Local_Items'Last) :=
              (Name => To_Unbounded_String(Source => "Repair the whole ship"),
               Cost => Cost, Time => Time,
               Id => To_Unbounded_String(Source => "-1"));
         else
            Count_Repair_Cost(I => 0);
            Local_Items(Local_Items'Last) :=
              (Name =>
                 To_Unbounded_String(Source => "Slowly repair the whole ship"),
               Cost => Cost, Time => Time,
               Id => To_Unbounded_String(Source => "0"));
         end if;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "recipes" then
         Fill_Recipes_Items_Loop :
         for I in 1 .. Get_Recipes_Amount loop
            Cost :=
              (if
                 Get_Price
                   (Base_Type => Sky_Bases(Base_Index).Base_Type,
                    Item_Index =>
                      Get_Recipe
                        (Recipe_Index =>
                           To_Bounded_String
                             (Source => Trim(Source => I'Img, Side => Both)))
                        .Result_Index) >
                 0
               then
                 Get_Price
                   (Base_Type => Sky_Bases(Base_Index).Base_Type,
                    Item_Index =>
                      Get_Recipe
                        (Recipe_Index =>
                           To_Bounded_String
                             (Source => Trim(Source => I'Img, Side => Both)))
                        .Result_Index) *
                 Get_Recipe
                   (Recipe_Index =>
                      To_Bounded_String
                        (Source => Trim(Source => I'Img, Side => Both)))
                   .Difficulty *
                 10
               else Get_Recipe
                   (Recipe_Index =>
                      To_Bounded_String
                        (Source => Trim(Source => I'Img, Side => Both)))
                   .Difficulty *
                 10);
            --## rule off ASSIGNMENTS
            Cost :=
              Natural(Float(Cost) * Get_Float_Setting(Name => "pricesBonus"));
            --## rule on ASSIGNMENTS
            if Cost = 0 then
               Cost := 1;
            end if;
            Count_Price
              (Price => Cost, Trader_Index => Find_Member(Order => TALK));
            Local_Items(Index) :=
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
                                          Trim(Source => I'Img, Side => Both)))
                                  .Result_Index)
                             .Name)),
               Cost => Cost, Time => 1,
               Id =>
                 To_Unbounded_String
                   (Source => Trim(Source => I'Img, Side => Both)));
            Index := Index + 1;
         end loop Fill_Recipes_Items_Loop;
      end if;
      Sort_Items(Container => Local_Items);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Items_Indexes.Clear;
      Fill_Items_Indexes_Loop :
      for Item of Local_Items loop
         Items_Indexes.Append(New_Item => Item.Id);
      end loop Fill_Items_Indexes_Loop;
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      return
        Show_Base_Ui_Command
          (Client_Data => Client_Data, Interp => Interp, Argc => 2,
           Argv =>
             CArgv.Empty & "ShowBaseUI" & CArgv.Arg(Argv => Argv, N => 1));
   end Sort_Base_Items_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowBaseUI", Ada_Command => Show_Base_Ui_Command'Access);
      Add_Command
        (Name => "BaseAction", Ada_Command => Base_Action_Command'Access);
      Add_Command
        (Name => "SearchRecipes",
         Ada_Command => Search_Recipes_Command'Access);
      Add_Command
        (Name => "ShowBaseMenu", Ada_Command => Show_Base_Menu_Command'Access);
      Add_Command
        (Name => "SortBaseItems",
         Ada_Command => Sort_Base_Items_Command'Access);
   end Add_Commands;

end Bases.UI;
