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
with Ada.Containers.Generic_Array_Sort;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases.Ship; use Bases.Ship;
with Bases.Trade; use Bases.Trade;
with BasesTypes; use BasesTypes;
with Config; use Config;
with CoreUI; use CoreUI;
with Crafts; use Crafts;
with Dialogs; use Dialogs;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships.Crew; use Ships.Crew;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Bases.UI is

   -- ****iv* BUI/BUI.BaseTable
   -- FUNCTION
   -- Table with info about available base actions
   -- SOURCE
   BaseTable: Table_Widget (3);
   -- ****

   -- ****iv* BUI/BUI.Items_Indexes
   -- FUNCTION
   -- Indexes of the crafting recipes/wounded crew members/damaged ship modules
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Items_Indexes: UnboundedString_Container.Vector;
   -- ****

   -- ****o* BUI/BUI.Show_Base_UI_Command
   -- FUNCTION
   -- Show the selected base action
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBaseUI UIType search page
   -- UIType can be heal, repair, recipes. Search is a string which will be
   -- looked for in names of recipes (only). Page is the number of current
   -- page on the list to show
   -- SOURCE
   function Show_Base_UI_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_UI_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      use Tiny_String;

      BaseFrame: Ttk_Frame := Get_Widget(Main_Paned & ".baseframe", Interp);
      BaseCanvas: constant Tk_Canvas :=
        Get_Widget(BaseFrame & ".canvas", Interp);
      SearchFrame: constant Ttk_Frame :=
        Get_Widget(BaseCanvas & ".base.searchframe", Interp);
      SearchEntry: constant Ttk_Entry :=
        Get_Widget(SearchFrame & ".search", Interp);
      FirstIndex, FormattedTime: Unbounded_String;
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      BaseType: constant Unbounded_String := Sky_Bases(BaseIndex).Base_Type;
      Cost, Time: Natural := 0;
      MoneyIndex2: constant Natural :=
        Find_Item(Player_Ship.Cargo, Money_Index);
      MoneyLabel: constant Ttk_Label :=
        Get_Widget(BaseCanvas & ".base.lblmoney", Interp);
      Page: constant Positive :=
        (if Argc = 4 then Positive'Value(CArgv.Arg(Argv, 3)) else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Arguments: constant String :=
        (if Argc > 2 then
           "{" & CArgv.Arg(Argv, 1) & "} {" & CArgv.Arg(Argv, 2) & "}"
         else CArgv.Arg(Argv, 1) & " {}");
      Current_Row: Positive := 1;
      procedure Format_Time is
      begin
         if Time < 60 then
            FormattedTime :=
              To_Unbounded_String(Natural'Image(Time) & " minute");
            if Time > 1 then
               Append(FormattedTime, "s");
            end if;
         else
            FormattedTime :=
              To_Unbounded_String(Positive'Image(Time / 60) & " hour");
            if (Time / 60) > 1 then
               Append(FormattedTime, "s");
            end if;
            if (Time mod 60) > 0 then
               Append
                 (FormattedTime,
                  " and" & Positive'Image(Time mod 60) & " minute");
               if (Time mod 60) > 1 then
                  Append(FormattedTime, "s");
               end if;
            end if;
         end if;
      end Format_Time;
      function Get_Color(Cost: Positive) return String is
      begin
         if MoneyIndex2 = 0
           or else
             Inventory_Container.Element
               (Container => Player_Ship.Cargo, Index => MoneyIndex2)
               .Amount <
             Cost then
            return "red";
         end if;
         return "";
      end Get_Color;
   begin
      if Winfo_Get(BaseCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "base.tcl");
         Bind(BaseFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(BaseCanvas, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         Show_Sky_Map(True);
         return TCL_OK;
      end if;
      BaseFrame.Name := New_String(BaseCanvas & ".base");
      if Winfo_Get(Ttk_Frame'(Get_Widget(BaseFrame & ".table")), "exists") =
        "1" then
         Destroy(BaseTable.Canvas);
      end if;
      if CArgv.Arg(Argv, 1) /= "recipes" then
         Tcl.Tk.Ada.Grid.Grid_Remove(SearchFrame);
         BaseTable :=
           Create_Table
             (Widget_Image(BaseFrame),
              (To_Unbounded_String("Action"), To_Unbounded_String("Cost"),
               To_Unbounded_String("Time")),
              Get_Widget(Main_Paned & ".baseframe.scrolly"),
              "SortBaseItems " & CArgv.Arg(Argv, 1),
              "Press mouse button to sort the actions.");
         if CArgv.Arg(Argv, 1) = "heal"
           and then Items_Indexes.Length /= Player_Ship.Crew.Length + 1 then
            Items_Indexes.Clear;
            for I in Player_Ship.Crew.Iterate loop
               Items_Indexes.Append
                 (To_Unbounded_String
                    (Positive'Image(Crew_Container.To_Index(I))));
            end loop;
            Items_Indexes.Append(To_Unbounded_String("0"));
         elsif CArgv.Arg(Argv, 1) = "repair"
           and then Items_Indexes.Length /= Player_Ship.Modules.Length + 3 then
            Items_Indexes.Clear;
            for I in Player_Ship.Modules.Iterate loop
               Items_Indexes.Append
                 (To_Unbounded_String
                    (Positive'Image(Modules_Container.To_Index(I))));
            end loop;
            Items_Indexes.Append(To_Unbounded_String("0"));
            Items_Indexes.Append
              (To_Unbounded_String
                 (if Sky_Bases(BaseIndex).Population > 149 then "-1"
                  else "-3"));
            Items_Indexes.Append
              (To_Unbounded_String
                 (if Sky_Bases(BaseIndex).Population > 299 then "-2"
                  else "-3"));
         end if;
      else
         Tcl.Tk.Ada.Grid.Grid(SearchFrame);
         if Argc /= 3 then
            configure(SearchEntry, "-validatecommand {}");
            Delete(SearchEntry, "0", "end");
            configure(SearchEntry, "-validatecommand {SearchRecipes %P}");
         end if;
         BaseTable :=
           Create_Table
             (Widget_Image(BaseFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Cost"),
               Null_Unbounded_String),
              Get_Widget(Main_Paned & ".baseframe.scrolly"),
              "SortBaseItems " & CArgv.Arg(Argv, 1),
              "Press mouse button to sort the recipes.");
         if Items_Indexes.Length /= Recipes_List.Length then
            Items_Indexes.Clear;
            for I in Recipes_List.Iterate loop
               Items_Indexes.Append
                 (To_Unbounded_String
                    (Source => To_String(Source => Recipes_Container.Key(I))));
            end loop;
         end if;
      end if;
      if MoneyIndex2 > 0 then
         configure
           (MoneyLabel,
            "-text {You have" &
            Natural'Image
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => MoneyIndex2)
                 .Amount) &
            " " & To_String(Money_Name) & ".}");
      else
         configure
           (MoneyLabel,
            "-text {You don't have any " & To_String(Money_Name) &
            " to buy anything.}");
      end if;
      if CArgv.Arg(Argv, 1) = "heal" then
         Show_Wounded_Crew_Loop :
         for I of Items_Indexes loop
            if Integer'Value(To_String(I)) > 0 then
               if Player_Ship.Crew(Positive'Value(To_String(I))).Health =
                 100 then
                  goto End_Of_Wounded_Loop;
               end if;
               if FirstIndex = Null_Unbounded_String then
                  FirstIndex := I;
               end if;
            end if;
            if Current_Row < Start_Row then
               Current_Row := Current_Row + 1;
               goto End_Of_Wounded_Loop;
            end if;
            Cost := 0;
            Time := 0;
            HealCost(Cost, Time, Positive'Value(To_String(I)));
            Add_Button
              (BaseTable,
               (if Integer'Value(To_String(I)) > 0 then
                  To_String
                    (Player_Ship.Crew(Positive'Value(To_String(I))).Name)
                else "Heal all wounded crew members"),
               "Show available options", "ShowBaseMenu heal " & To_String(I),
               1);
            Add_Button
              (Table => BaseTable,
               Text => Positive'Image(Cost) & " " & To_String(Money_Name),
               Tooltip => "Show available options",
               Command => "ShowBaseMenu heal " & To_String(I), Column => 2,
               Color => Get_Color(Cost));
            Format_Time;
            Add_Button
              (BaseTable, To_String(FormattedTime), "Show available options",
               "ShowBaseMenu heal " & To_String(I), 3, True);
            exit Show_Wounded_Crew_Loop when BaseTable.Row =
              Game_Settings.Lists_Limit + 1;
            <<End_Of_Wounded_Loop>>
         end loop Show_Wounded_Crew_Loop;
      elsif CArgv.Arg(Argv, 1) = "repair" then
         Show_Damaged_Modules_Loop :
         for I of Items_Indexes loop
            if Integer'Value(To_String(I)) > 0 then
               if Player_Ship.Modules(Positive'Value(To_String(I)))
                   .Durability =
                 Player_Ship.Modules(Positive'Value(To_String(I)))
                   .Max_Durability then
                  goto End_Of_Damaged_Modules_Loop;
               end if;
               if FirstIndex = Null_Unbounded_String then
                  FirstIndex := I;
               end if;
            end if;
            if Current_Row < Start_Row then
               Current_Row := Current_Row + 1;
               goto End_Of_Damaged_Modules_Loop;
            end if;
            if I = To_Unbounded_String("-3") then
               goto End_Of_Damaged_Modules_Loop;
            end if;
            Cost := 0;
            Time := 0;
            Repair_Cost(Cost, Time, Integer'Value(To_String(I)));
            Count_Price(Cost, Find_Member(TALK));
            Add_Button
              (BaseTable,
               (case Integer'Value(To_String(I)) is
                  when 0 => "Slowly repair the whole ship",
                  when -1 => "Repair the whole ship",
                  when -2 => "Quickly repair the whole ship",
                  when others =>
                    To_String
                      (Player_Ship.Modules(Positive'Value(To_String(I)))
                         .Name)),
               "Show available options", "ShowBaseMenu repair " & To_String(I),
               1);
            Add_Button
              (Table => BaseTable,
               Text => Positive'Image(Cost) & " " & To_String(Money_Name),
               Tooltip => "Show available options",
               Command => "ShowBaseMenu repair " & To_String(I), Column => 2,
               Color => Get_Color(Cost));
            Format_Time;
            Add_Button
              (BaseTable, To_String(FormattedTime), "Show available options",
               "ShowBaseMenu repair " & To_String(I), 3, True);
            exit Show_Damaged_Modules_Loop when BaseTable.Row =
              Game_Settings.Lists_Limit + 1;
            <<End_Of_Damaged_Modules_Loop>>
         end loop Show_Damaged_Modules_Loop;
      elsif CArgv.Arg(Argv, 1) = "recipes" then
         Show_Available_Recipes_Loop :
         for I of Items_Indexes loop
            if not Bases_Types_List(BaseType).Recipes.Contains(I) or
              Known_Recipes.Find_Index(Item => I) /=
                Positive_Container.No_Index or
              Recipes_List(To_Bounded_String(Source => To_String(Source => I)))
                  .Reputation >
                Sky_Bases(BaseIndex).Reputation.Level then
               goto End_Of_Recipes_Loop;
            end if;
            if Argc > 2 and then CArgv.Arg(Argv, 2)'Length > 0
              and then
                Index
                  (To_Lower
                     (To_String
                        (Items_List
                           (Recipes_List
                              (To_Bounded_String
                                 (Source => To_String(Source => I)))
                              .Result_Index)
                           .Name)),
                   To_Lower(CArgv.Arg(Argv, 2))) =
                0 then
               goto End_Of_Recipes_Loop;
            end if;
            if FirstIndex = Null_Unbounded_String then
               FirstIndex := I;
            end if;
            if Current_Row < Start_Row then
               Current_Row := Current_Row + 1;
               goto End_Of_Recipes_Loop;
            end if;
            Add_Button
              (BaseTable,
               To_String
                 (Items_List
                    (Recipes_List
                       (To_Bounded_String(Source => To_String(Source => I)))
                       .Result_Index)
                    .Name),
               "Show available options",
               "ShowBaseMenu recipes {" & To_String(I) & "}", 1);
            Cost :=
              (if
                 Get_Price
                   (Sky_Bases(BaseIndex).Base_Type,
                    Recipes_List
                      (To_Bounded_String(Source => To_String(Source => I)))
                      .Result_Index) >
                 0
               then
                 Get_Price
                   (Sky_Bases(BaseIndex).Base_Type,
                    Recipes_List
                      (To_Bounded_String(Source => To_String(Source => I)))
                      .Result_Index) *
                 Recipes_List
                   (To_Bounded_String(Source => To_String(Source => I)))
                   .Difficulty *
                 10
               else Recipes_List
                   (To_Bounded_String(Source => To_String(Source => I)))
                   .Difficulty *
                 10);
            Cost :=
              Natural(Float(Cost) * Float(New_Game_Settings.Prices_Bonus));
            if Cost = 0 then
               Cost := 1;
            end if;
            Count_Price(Cost, Find_Member(TALK));
            Add_Button
              (Table => BaseTable,
               Text => Positive'Image(Cost) & " " & To_String(Money_Name),
               Tooltip => "Show available options",
               Command => "ShowBaseMenu recipes {" & To_String(I) & "}",
               Column => 2, New_Row => True, Color => Get_Color(Cost));
            exit Show_Available_Recipes_Loop when BaseTable.Row =
              Game_Settings.Lists_Limit + 1;
            <<End_Of_Recipes_Loop>>
         end loop Show_Available_Recipes_Loop;
      end if;
      Add_Pagination
        (BaseTable,
         (if Page > 1 then "ShowBaseUI " & Arguments & Positive'Image(Page - 1)
          else ""),
         (if BaseTable.Row < Game_Settings.Lists_Limit + 1 then ""
          else "ShowBaseUI " & Arguments & Positive'Image(Page + 1)));
      Update_Table
        (BaseTable, (if Focus = Widget_Image(SearchEntry) then False));
      if FirstIndex = Null_Unbounded_String and Argc < 3 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         Show_Sky_Map(True);
         return TCL_OK;
      end if;
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      BaseFrame.Name := New_String(BaseCanvas & ".base");
      configure
        (BaseCanvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (BaseCanvas, "window", "0 0 -anchor nw -window " & BaseFrame);
      Tcl_Eval(Get_Context, "update");
      configure
        (BaseCanvas, "-scrollregion [list " & BBox(BaseCanvas, "all") & "]");
      Show_Screen("baseframe");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Base_UI_Command;

   -- ****o* BUI/BUI.Base_Action_Command
   -- FUNCTION
   -- Execute the selected action
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- BaseAction ActionType
   -- ActionType can be heal, repair, recipes
   -- SOURCE
   function Base_Action_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Base_Action_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      ItemIndex: constant String := CArgv.Arg(Argv, 2);
   begin
      if CArgv.Arg(Argv, 1) = "heal" then
         HealWounded(Natural'Value(ItemIndex));
      elsif CArgv.Arg(Argv, 1) = "repair" then
         Bases.Ship.Repair_Ship(Integer'Value(ItemIndex));
      elsif CArgv.Arg(Argv, 1) = "recipes" then
         BuyRecipe(To_Bounded_String(ItemIndex));
      end if;
      Update_Header;
      Update_Messages;
      return
        Show_Base_UI_Command
          (ClientData, Interp, 2,
           CArgv.Empty & "ShowBaseUI" & CArgv.Arg(Argv, 1));
   end Base_Action_Command;

   -- ****o* BUI/BUI.Search_Recipes_Command
   -- FUNCTION
   -- Show only this recipes which contains the selected sequence
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SearchRecipes TextToSearch
   -- SOURCE
   function Search_Recipes_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Recipes_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      SearchText: constant String := CArgv.Arg(Argv, 1);
   begin
      if SearchText'Length = 0 then
         return
           Show_Base_UI_Command
             (ClientData, Interp, 2, CArgv.Empty & "ShowBaseUI" & "recipes");
      end if;
      return
        Show_Base_UI_Command
          (ClientData, Interp, 3,
           CArgv.Empty & "ShowBaseUI" & "recipes" & SearchText);
   end Search_Recipes_Command;

   -- ****o* BUI/BUI.Show_Base_Menu_Command
   -- FUNCTION
   -- Show menu with options for the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBaseMenu action index
   -- Action is name of action (heal,repair or recipe) and index is the index
   -- of the item
   -- SOURCE
   function Show_Base_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      Cost, Time: Natural := 0;
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      MoneyIndex2: constant Natural :=
        Find_Item(Player_Ship.Cargo, Money_Index);
      Action: constant String := CArgv.Arg(Argv, 1);
      ItemIndex: constant String := CArgv.Arg(Argv, 2);
      Base_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".basemenu", Title => "Actions", Parent_Name => ".");
      procedure Add_Button(Name, Label, Command: String) is
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
         HealCost(Cost, Time, Integer'Value(ItemIndex));
      elsif Action = "repair" then
         Repair_Cost(Cost, Time, Integer'Value(ItemIndex));
         Count_Price(Cost, Find_Member(TALK));
      else
         Cost :=
           (if
              Get_Price
                (Sky_Bases(BaseIndex).Base_Type,
                 Recipes_List(To_Bounded_String(ItemIndex)).Result_Index) >
              0
            then
              Get_Price
                (Sky_Bases(BaseIndex).Base_Type,
                 Recipes_List(To_Bounded_String(ItemIndex)).Result_Index) *
              Recipes_List(To_Bounded_String(ItemIndex)).Difficulty * 10
            else Recipes_List(To_Bounded_String(ItemIndex)).Difficulty * 10);
         Cost := Natural(Float(Cost) * Float(New_Game_Settings.Prices_Bonus));
         if Cost = 0 then
            Cost := 1;
         end if;
         Count_Price(Cost, Find_Member(TALK));
      end if;
      if MoneyIndex2 = 0
        or else
          Inventory_Container.Element
            (Container => Player_Ship.Cargo, Index => MoneyIndex2)
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
            Command => "BaseAction " & Action & " " & ItemIndex);
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

   -- ****iv* BUI/BUI.Base_Sort_Order
   -- FUNCTION
   -- The current sorting order for items
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Base_Sort_Order: Base_Sort_Orders := Default_Base_Sort_Order;
   -- ****

   -- ****o* BUI/BUI.Sort_Modules_Command
   -- FUNCTION
   -- Sort the list with recipes to buy/healing wounded/repair ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- HISTORY
   -- 6.5 - Added
   -- COMMANDS
   -- SortBaseItems x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Base_Items_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Base_Items_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number(BaseTable, Natural'Value(CArgv.Arg(Argv, 2)));
      type Local_Item_Data is record
         Name: Unbounded_String;
         Cost: Positive;
         Time: Positive;
         Id: Unbounded_String;
      end record;
      type Items_Array is array(Positive range <>) of Local_Item_Data;
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Local_Items: Items_Array
        (1 ..
             (if CArgv.Arg(Argv, 1) = "recipes" then
                Positive(Recipes_List.Length)
              elsif CArgv.Arg(Argv, 1) = "heal" then
                Positive(Player_Ship.Crew.Length) + 1
              else Positive(Player_Ship.Modules.Length) +
                (if Sky_Bases(BaseIndex).Population > 299 then 3
                 elsif Sky_Bases(BaseIndex).Population > 149 then 2 else 1)));
      Index: Positive := 1;
      Cost, Time: Natural := 0;
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
      procedure Sort_Items is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Item_Data,
         Array_Type => Items_Array);
      procedure Count_Repair_Cost(Index: Integer) is
      begin
         Cost := 0;
         Time := 0;
         Repair_Cost(Cost, Time, Index);
         Count_Price(Cost, Find_Member(TALK));
      end Count_Repair_Cost;
   begin
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
      if CArgv.Arg(Argv, 1) = "heal" then
         for I in Player_Ship.Crew.Iterate loop
            Cost := 0;
            Time := 0;
            HealCost(Cost, Time, Crew_Container.To_Index(I));
            Local_Items(Crew_Container.To_Index(I)) :=
              (Name =>
                 To_Unbounded_String
                   (Source => To_String(Source => Player_Ship.Crew(I).Name)),
               Cost => Cost, Time => Time,
               Id =>
                 To_Unbounded_String
                   (Positive'Image(Crew_Container.To_Index(I))));
         end loop;
         Cost := 0;
         Time := 0;
         HealCost(Cost, Time, 0);
         Local_Items(Local_Items'Last) :=
           (Name => To_Unbounded_String("Heal all wounded crew members"),
            Cost => Cost, Time => Time, Id => To_Unbounded_String("0"));
      elsif CArgv.Arg(Argv, 1) = "repair" then
         for I in Player_Ship.Modules.Iterate loop
            Count_Repair_Cost(Modules_Container.To_Index(I));
            Local_Items(Modules_Container.To_Index(I)) :=
              (Name => Player_Ship.Modules(I).Name, Cost => Cost, Time => Time,
               Id =>
                 To_Unbounded_String
                   (Positive'Image(Modules_Container.To_Index(I))));
         end loop;
         if Sky_Bases(BaseIndex).Population > 299 then
            Count_Repair_Cost(0);
            Local_Items(Local_Items'Last - 2) :=
              (Name => To_Unbounded_String("Slowly repair the whole ship"),
               Cost => Cost, Time => Time, Id => To_Unbounded_String("0"));
            Count_Repair_Cost(-1);
            Local_Items(Local_Items'Last - 1) :=
              (Name => To_Unbounded_String("Repair the whole ship"),
               Cost => Cost, Time => Time, Id => To_Unbounded_String("-1"));
            Count_Repair_Cost(-2);
            Local_Items(Local_Items'Last) :=
              (Name => To_Unbounded_String("Quickly repair the whole ship"),
               Cost => Cost, Time => Time, Id => To_Unbounded_String("-2"));
         elsif Sky_Bases(BaseIndex).Population > 149 then
            Count_Repair_Cost(0);
            Local_Items(Local_Items'Last - 1) :=
              (Name => To_Unbounded_String("Slowly repair the whole ship"),
               Cost => Cost, Time => Time, Id => To_Unbounded_String("0"));
            Count_Repair_Cost(-1);
            Local_Items(Local_Items'Last) :=
              (Name => To_Unbounded_String("Repair the whole ship"),
               Cost => Cost, Time => Time, Id => To_Unbounded_String("-1"));
         else
            Count_Repair_Cost(0);
            Local_Items(Local_Items'Last) :=
              (Name => To_Unbounded_String("Slowly repair the whole ship"),
               Cost => Cost, Time => Time, Id => To_Unbounded_String("0"));
         end if;
      elsif CArgv.Arg(Argv, 1) = "recipes" then
         for I in Recipes_List.Iterate loop
            Cost :=
              (if
                 Get_Price
                   (Sky_Bases(BaseIndex).Base_Type,
                    Recipes_List(I).Result_Index) >
                 0
               then
                 Get_Price
                   (Sky_Bases(BaseIndex).Base_Type,
                    Recipes_List(I).Result_Index) *
                 Recipes_List(I).Difficulty * 10
               else Recipes_List(I).Difficulty * 10);
            Cost :=
              Natural(Float(Cost) * Float(New_Game_Settings.Prices_Bonus));
            if Cost = 0 then
               Cost := 1;
            end if;
            Count_Price(Cost, Find_Member(TALK));
            Local_Items(Index) :=
              (Name => Items_List(Recipes_List(I).Result_Index).Name,
               Cost => Cost, Time => 1,
               Id =>
                 To_Unbounded_String
                   (Source => To_String(Source => Recipes_Container.Key(I))));
            Index := Index + 1;
         end loop;
      end if;
      Sort_Items(Local_Items);
      Items_Indexes.Clear;
      for Item of Local_Items loop
         Items_Indexes.Append(Item.Id);
      end loop;
      return
        Show_Base_UI_Command
          (ClientData, Interp, 2,
           CArgv.Empty & "ShowBaseUI" & CArgv.Arg(Argv, 1));
   end Sort_Base_Items_Command;

   procedure AddCommands is
   begin
      Add_Command("ShowBaseUI", Show_Base_UI_Command'Access);
      Add_Command("BaseAction", Base_Action_Command'Access);
      Add_Command("SearchRecipes", Search_Recipes_Command'Access);
      Add_Command("ShowBaseMenu", Show_Base_Menu_Command'Access);
      Add_Command("SortBaseItems", Sort_Base_Items_Command'Access);
   end AddCommands;

end Bases.UI;
