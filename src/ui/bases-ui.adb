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

with Ada.Containers.Generic_Array_Sort;
with Ada.Strings; use Ada.Strings;
with Interfaces.C; use Interfaces.C;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
-- with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
-- with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Bases.Ship; use Bases.Ship;
with Bases.Trade; use Bases.Trade;
with BasesTypes; use BasesTypes;
with Config; use Config;
with CoreUI;
with Crafts; use Crafts;
-- with Dialogs;
with Maps; use Maps;
with Ships.Crew; use Ships.Crew;
with Table; use Table;
with Utils.UI;

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
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use CoreUI;

      Base_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".baseframe", Interp => Interp);
      Base_Index: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      function Show_Ada_Base_Ui_Command
        (C_Data: Integer; I: Tcl.Tcl_Interp; Ac: Interfaces.C.int;
         Av: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
         Import => True,
         Convention => C,
         External_Name => "showBaseUiCommand";
   begin
      if Show_Ada_Base_Ui_Command
          (C_Data => Client_Data, I => Interp, Ac => Argc, Av => Argv) =
        TCL_ERROR then
         return TCL_ERROR;
      end if;
      if CArgv.Arg(Argv => Argv, N => 1) = "recipes" then
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
      Import => True,
      Convention => C,
      External_Name => "baseActionCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "searchRecipesCommand";
      -- ****

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
      Import => True,
      Convention => C,
      External_Name => "showBaseMenuCommand";
      -- ****

--   function Show_Base_Menu_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Client_Data, Interp, Argc);
--      use Dialogs;
--      use Tiny_String;
--
--      Cost, Time: Natural := 0;
--      Base_Index: constant Positive :=
--        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
--      Money_Index_2: constant Natural :=
--        Find_Item(Inventory => Player_Ship.Cargo, Proto_Index => Money_Index);
--      Action: constant String := CArgv.Arg(Argv => Argv, N => 1);
--      Item_Index: constant String := CArgv.Arg(Argv => Argv, N => 2);
--      Base_Menu: constant Ttk_Frame :=
--        Create_Dialog
--          (Name => ".basemenu", Title => "Actions", Parent_Name => ".");
--      procedure Add_Button(Name, Label, Command: String) is
--         use Tcl.Tk.Ada.Widgets.TtkButton;
--
--         Button: constant Ttk_Button :=
--           Create
--             (pathName => Base_Menu & Name,
--              options =>
--                "-text {" & Label & "} -command {CloseDialog " & Base_Menu &
--                " .;" & Command & "}");
--      begin
--         Tcl.Tk.Ada.Grid.Grid
--           (Slave => Button,
--            Options =>
--              "-sticky we -padx 5" &
--              (if Command'Length = 0 then " -pady {0 3}" else ""));
--         Bind
--           (Widgt => Button, Sequence => "<Escape>",
--            Script => "{CloseDialog " & Base_Menu & " .;break}");
--         if Command'Length = 0 then
--            Bind
--              (Widgt => Button, Sequence => "<Tab>",
--               Script => "{focus " & Base_Menu & ".action;break}");
--            Focus(Widgt => Button);
--         end if;
--      end Add_Button;
--   begin
--      if Action = "heal" then
--         Heal_Cost
--           (Cost => Cost, Time => Time,
--            Member_Index => Integer'Value(Item_Index));
--      elsif Action = "repair" then
--         Repair_Cost
--           (Cost => Cost, Time => Time,
--            Module_Index => Integer'Value(Item_Index));
--         Count_Price
--           (Price => Cost, Trader_Index => Find_Member(Order => TALK));
--      else
--         Cost :=
--           (if
--              Get_Price
--                (Base_Type => Sky_Bases(Base_Index).Base_Type,
--                 Item_Index =>
--                   Get_Recipe
--                     (Recipe_Index => To_Bounded_String(Source => Item_Index))
--                     .Result_Index) >
--              0
--            then
--              Get_Price
--                (Base_Type => Sky_Bases(Base_Index).Base_Type,
--                 Item_Index =>
--                   Get_Recipe
--                     (Recipe_Index => To_Bounded_String(Source => Item_Index))
--                     .Result_Index) *
--              Get_Recipe
--                (Recipe_Index => To_Bounded_String(Source => Item_Index))
--                .Difficulty *
--              10
--            else Get_Recipe
--                (Recipe_Index => To_Bounded_String(Source => Item_Index))
--                .Difficulty *
--              10);
--         --## rule off ASSIGNMENTS
--         Cost :=
--           Natural(Float(Cost) * Get_Float_Setting(Name => "pricesBonus"));
--         --## rule on ASSIGNMENTS
--         if Cost = 0 then
--            Cost := 1;
--         end if;
--         Count_Price
--           (Price => Cost, Trader_Index => Find_Member(Order => TALK));
--      end if;
--      if Money_Index_2 = 0
--        or else
--          Inventory_Container.Element
--            (Container => Player_Ship.Cargo, Index => Money_Index_2)
--            .Amount <
--          Cost then
--         Add_Button
--           (Name => ".action", Label => "You don't have money for this",
--            Command => "");
--      else
--         Add_Button
--           (Name => ".action",
--            Label =>
--              (if Action = "heal" then "Buy healing"
--               elsif Action = "repair" then "Buy repair" else "Buy recipe"),
--            Command => "BaseAction " & Action & " " & Item_Index);
--         Add_Button(Name => ".close", Label => "Close", Command => "");
--      end if;
--      Show_Dialog(Dialog => Base_Menu, Parent_Frame => ".");
--      return TCL_OK;
--   end Show_Base_Menu_Command;

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
      use Utils.UI;
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
