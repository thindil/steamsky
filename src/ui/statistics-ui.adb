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
-- with Ada.Float_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C;
-- with GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
-- with Tcl.Ada;
-- with Tcl.Tk.Ada;
-- with Tcl.Tk.Ada.Grid;
-- with Tcl.Tk.Ada.Widgets;
-- with Tcl.Tk.Ada.Widgets.Canvas;
-- with Tcl.Tk.Ada.Widgets.TtkFrame;
-- with Tcl.Tk.Ada.Widgets.TtkLabel;
-- with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
-- with Tcl.Tk.Ada.Widgets.TtkTreeView;
-- with Tcl.Tk.Ada.Winfo;
-- with Tcl.Tklib.Ada.Tooltip;
with Crafts; use Crafts;
-- with CoreUI;
with Game; use Game;
use Game.Tiny_String;
with Goals; use Goals;
with Items; use Items;
with Missions; use Missions;
with Ships; use Ships;
with Utils.UI; use Utils.UI;

package body Statistics.UI is

   -- ****iv* SUI/SUI.Crafting_Indexes
   -- FUNCTION
   -- Indexes of the finished crafting orders
   -- SOURCE
   Crafting_Indexes: Positive_Container.Vector;
   -- ****

   -- ****iv* SUI/SUI.Missions_Indexes
   -- FUNCTION
   -- Indexes of the finished missions
   -- SOURCE
   Missions_Indexes: Positive_Container.Vector;
   -- ****

   -- ****iv* SUI/SUI.Goals_Indexes
   -- FUNCTION
   -- Indexes of the finished goals
   -- SOURCE
   Goals_Indexes: Positive_Container.Vector;
   -- ****

   -- ****iv* SUI/SUI.Destroyed_Indexes
   -- FUNCTION
   -- Indexes of the destroyed ships
   -- SOURCE
   Destroyed_Indexes: Positive_Container.Vector;
   -- ****

   -- ****iv* SUI/SUI.Killed_Indexes
   -- FUNCTION
   -- Indexes of the killed mobs
   -- SOURCE
   Killed_Indexes: Positive_Container.Vector;
   -- ****

   procedure Show_Statistics(Refresh: Boolean := False) is
--      use Interfaces.C.Strings;
--      use GNAT.Directory_Operations;
--      use Tcl.Ada;
--      use Tcl.Tk.Ada;
--      use Tcl.Tk.Ada.Widgets;
--      use Tcl.Tk.Ada.Widgets.Canvas;
--      use Tcl.Tk.Ada.Widgets.TtkFrame;
--      use Tcl.Tk.Ada.Widgets.TtkLabel;
--      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
--      use Tcl.Tk.Ada.Widgets.TtkTreeView;
--      use Tcl.Tk.Ada.Winfo;
--      use Tcl.Tklib.Ada.Tooltip;
--      use CoreUI;
--
--      Total_Finished, Total_Destroyed: Natural := 0;
--      Stats_Text: Unbounded_String;
--      Proto_Index: Positive := 1;
--      Stats_Frame: Ttk_Frame :=
--        Get_Widget(pathName => Main_Paned & ".statsframe");
--      Stats_Canvas: constant Tk_Canvas :=
--        Get_Widget(pathName => Stats_Frame & ".canvas");
--      Label: Ttk_Label :=
--        Get_Widget(pathName => Stats_Canvas & ".stats.left.points");
--      --## rule off IMPROPER_INITIALIZATION
--      Tree_View: Ttk_Tree_View;
--      Stats_List: Statistics_Container.Vector;
--      --## rule on IMPROPER_INITIALIZATION
      procedure Show_Ada_Statistics(Refr: Integer) with
         Import => True,
         Convention => C,
         External_Name => "showAdaStatistics";
   begin
      Show_Ada_Statistics(Refr => (if Refresh then 1 else 0));
--      if Winfo_Get(Widgt => Label, Info => "exists") = "0" then
--         Tcl_EvalFile
--           (interp => Get_Context,
--            fileName =>
--              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
--              "stats.tcl");
--         Bind
--           (Widgt => Stats_Frame, Sequence => "<Configure>",
--            Script => "{ResizeCanvas %W.canvas %w %h}");
--      elsif Winfo_Get(Widgt => Label, Info => "ismapped") = "1" and
--        not Refresh then
--         Tcl_Eval
--           (interp => Get_Context, strng => "InvokeButton " & Close_Button);
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
--         return;
--      end if;
--      configure
--        (Widgt => Label,
--         options => "-text {Points:" & Natural'Image(Get_Game_Points) & "}");
--      Add
--        (Widget => Label,
--         Message => "The amount of points gained in this game");
--      Stats_Text := To_Unbounded_String(Source => "Time passed:");
--      Add_Time_Info_Block :
--      declare
--         --## rule off SIMPLIFIABLE_EXPRESSIONS
--         Minutes_Diff: constant Natural :=
--           (Game_Date.Minutes + (Game_Date.Hour * 60) +
--            (Game_Date.Day * 1_440) + (Game_Date.Month * 43_200) +
--            (Game_Date.Year * 518_400)) -
--           829_571_520;
--         --## rule on SIMPLIFIABLE_EXPRESSIONS
--      begin
--         Minutes_To_Date(Minutes => Minutes_Diff, Info_Text => Stats_Text);
--      end Add_Time_Info_Block;
--      Label := Get_Widget(pathName => Stats_Canvas & ".stats.left.time");
--      configure
--        (Widgt => Label,
--         options => "-text {" & To_String(Source => Stats_Text) & "}");
--      Add
--        (Widget => Label,
--         Message => "In game time which was passed since it started");
--      Add_Visited_Map_Block :
--      declare
--         use Ada.Float_Text_IO;
--
--         --## rule off TYPE_INITIAL_VALUES
--         type Visited_Factor is digits 4 range 0.0 .. 100.0;
--         --## rule on TYPE_INITIAL_VALUES
--         Visited_Percent: Visited_Factor;
--         Visited_String: String(1 .. 5);
--      begin
--         Visited_Percent :=
--           Visited_Factor
--             ((Float(Get_Game_Stats_Number(Name => "basesVisited")) /
--               1_024.0) *
--              100.0);
--         Put
--           (To => Visited_String, Item => Float(Visited_Percent), Aft => 3,
--            Exp => 0);
--         Stats_Text :=
--           To_Unbounded_String
--             (Source =>
--                "Bases visited:" &
--                Positive'Image(Get_Game_Stats_Number(Name => "basesVisited")) &
--                " (" & Visited_String & "%)");
--         Label := Get_Widget(pathName => Stats_Canvas & ".stats.left.bases");
--         configure
--           (Widgt => Label,
--            options => "-text {" & To_String(Source => Stats_Text) & "}");
--         Add
--           (Widget => Label,
--            Message =>
--              "The amount of sky bases visited and total percentage of all bases");
--         Visited_Percent :=
--           Visited_Factor
--             (Float(Get_Game_Stats_Number(Name => "mapVisited")) /
--              (1_024.0 * 1_024.0)) *
--           100.0;
--         if Visited_Percent < 0.001 then
--            Visited_Percent := 0.001;
--         end if;
--         Put
--           (To => Visited_String, Item => Float(Visited_Percent), Aft => 3,
--            Exp => 0);
--         Stats_Text :=
--           To_Unbounded_String
--             (Source => "Map discovered: " & Visited_String & "%");
--         Label := Get_Widget(pathName => Stats_Canvas & ".stats.left.map");
--         configure
--           (Widgt => Label,
--            options => "-text {" & To_String(Source => Stats_Text) & "}");
--         Add
--           (Widget => Label,
--            Message => "The amount of unique map's fields visited");
--      end Add_Visited_Map_Block;
--      Stats_Text :=
--        To_Unbounded_String
--          (Source =>
--             "Distance traveled:" &
--             Natural'Image(Get_Game_Stats_Number(Name => "distanceTraveled")));
--      Label := Get_Widget(pathName => Stats_Canvas & ".stats.left.distance");
--      configure
--        (Widgt => Label,
--         options => "-text {" & To_String(Source => Stats_Text) & "}");
--      Add
--        (Widget => Label,
--         Message => "The total amount of map's fields visited");
--      Stats_Frame.Name := New_String(Str => Stats_Canvas & ".stats");
--      Total_Finished := 0;
--      Stats_List := Get_Game_Stats_List(Name => "craftingOrders");
--      Count_Finished_Crafting_Loop :
--      for CraftingOrder of Stats_List loop
--         Total_Finished := Total_Finished + CraftingOrder.Amount;
--      end loop Count_Finished_Crafting_Loop;
--      Label.Name := New_String(Str => Stats_Frame & ".left.crafts");
--      configure
--        (Widgt => Label,
--         options =>
--           "-text {Crafting orders finished:" & Natural'Image(Total_Finished) &
--           "}");
--      Add
--        (Widget => Label,
--         Message =>
--           "The total amount of crafting orders finished in this game");
--      Stats_Frame :=
--        Get_Widget(pathName => Stats_Canvas & ".stats.left.craftsframe");
--      Tree_View := Get_Widget(pathName => Stats_Frame & ".craftsview");
--      if Children(TreeViewWidget => Tree_View, Item => "{}") /= "{}" then
--         Delete
--           (TreeViewWidget => Tree_View,
--            ItemsList =>
--              "[list " & Children(TreeViewWidget => Tree_View, Item => "{}") &
--              "]");
--      end if;
--      if Total_Finished > 0 then
--         if Crafting_Indexes.Length /= Stats_List.Length then
--            Crafting_Indexes.Clear;
--            Fill_Crafting_Indexes_Loop :
--            for I in Stats_List.Iterate loop
--               Crafting_Indexes.Append
--                 (New_Item => Statistics_Container.To_Index(Position => I));
--            end loop Fill_Crafting_Indexes_Loop;
--         end if;
--         Show_Finished_Crafting_Loop :
--         for I of Crafting_Indexes loop
--            Insert
--              (TreeViewWidget => Tree_View,
--               Options =>
--                 "{} end -values [list {" &
--                 To_String
--                   (Source =>
--                      Get_Proto_Item
--                        (Index =>
--                           Get_Recipe
--                             (Recipe_Index =>
--                                To_Bounded_String
--                                  (Source =>
--                                     To_String(Source => Stats_List(I).Index)))
--                             .Result_Index)
--                        .Name) &
--                 "} {" & Positive'Image(Stats_List(I).Amount) & "}]");
--         end loop Show_Finished_Crafting_Loop;
--         configure
--           (Widgt => Tree_View,
--            options =>
--              "-height" &
--              (if Stats_List.Length < 10 then
--                 Positive'Image(Positive(Stats_List.Length))
--               else " 10"));
--         Tcl.Tk.Ada.Grid.Grid(Slave => Stats_Frame);
--      else
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Stats_Frame);
--      end if;
--      Total_Finished := 0;
--      Stats_List := Get_Game_Stats_List(Name => "finishedMissions");
--      Count_Finished_Missions_Loop :
--      for FinishedMission of Stats_List loop
--         Total_Finished := Total_Finished + FinishedMission.Amount;
--      end loop Count_Finished_Missions_Loop;
--      Label.Name := New_String(Str => Stats_Canvas & ".stats.left.missions");
--      Add_Finished_Missions_Block :
--      declare
--         Missions_Percent: Natural := 0;
--      begin
--         if Get_Game_Stats_Number(Name => "acceptedMissions") > 0 then
--            Missions_Percent :=
--              Natural
--                ((Float(Total_Finished) /
--                  Float(Get_Game_Stats_Number(Name => "acceptedMissions"))) *
--                 100.0);
--         end if;
--         configure
--           (Widgt => Label,
--            options =>
--              "-text {Missions completed:" & Natural'Image(Total_Finished) &
--              " (" &
--              To_String
--                (Source =>
--                   Trim
--                     (Source =>
--                        To_Unbounded_String
--                          (Source => Natural'Image(Missions_Percent)),
--                      Side => Ada.Strings.Left)) &
--              "%)" & "}");
--         Add
--           (Widget => Label,
--            Message => "The total amount of missions finished in this game");
--      end Add_Finished_Missions_Block;
--      Stats_Frame :=
--        Get_Widget(pathName => Stats_Canvas & ".stats.left.missionsframe");
--      Tree_View.Name := New_String(Str => Stats_Frame & ".missionsview");
--      if Children(TreeViewWidget => Tree_View, Item => "{}") /= "{}" then
--         Delete
--           (TreeViewWidget => Tree_View,
--            ItemsList =>
--              "[list " & Children(TreeViewWidget => Tree_View, Item => "{}") &
--              "]");
--      end if;
--      if Total_Finished > 0 then
--         if Missions_Indexes.Length /= Stats_List.Length then
--            Missions_Indexes.Clear;
--            Fill_Missions_Indexes_Loop :
--            for I in Stats_List.Iterate loop
--               Missions_Indexes.Append
--                 (New_Item => Statistics_Container.To_Index(Position => I));
--            end loop Fill_Missions_Indexes_Loop;
--         end if;
--         Show_Finished_Missions_Loop :
--         for I of Missions_Indexes loop
--            case Missions_Types'Val
--              (Integer'Value(To_String(Source => Stats_List(I).Index))) is
--               when DELIVER =>
--                  Insert
--                    (TreeViewWidget => Tree_View,
--                     Options =>
--                       "{} end -values [list {Delivered items} {" &
--                       Positive'Image(Stats_List(I).Amount) & "}]");
--               when PATROL =>
--                  Insert
--                    (TreeViewWidget => Tree_View,
--                     Options =>
--                       "{} end -values [list {Patroled areas} {" &
--                       Positive'Image(Stats_List(I).Amount) & "}]");
--               when DESTROY =>
--                  Insert
--                    (TreeViewWidget => Tree_View,
--                     Options =>
--                       "{} end -values [list {Destroyed ships} {" &
--                       Positive'Image(Stats_List(I).Amount) & "}]");
--               when EXPLORE =>
--                  Insert
--                    (TreeViewWidget => Tree_View,
--                     Options =>
--                       "{} end -values [list {Explored areas} {" &
--                       Positive'Image(Stats_List(I).Amount) & "}]");
--               when PASSENGER =>
--                  Insert
--                    (TreeViewWidget => Tree_View,
--                     Options =>
--                       "{} end -values [list {Passengers transported} {" &
--                       Positive'Image(Stats_List(I).Amount) & "}]");
--            end case;
--         end loop Show_Finished_Missions_Loop;
--         configure
--           (Widgt => Tree_View,
--            options =>
--              "-height" &
--              (if Stats_List.Length < 10 then
--                 Positive'Image(Positive(Stats_List.Length))
--               else " 10"));
--         Tcl.Tk.Ada.Grid.Grid(Slave => Stats_Frame);
--      else
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Stats_Frame);
--      end if;
--      Label.Name := New_String(Str => Stats_Canvas & ".stats.left.goal");
--      configure
--        (Widgt => Label,
--         options =>
--           "-text {" &
--           (if Goal_Text(Index => 0)'Length < 22 then Goal_Text(Index => 0)
--            else Goal_Text(Index => 0)(1 .. 22)) &
--           "...}");
--      Add
--        (Widget => Label,
--         Message => "The current goal: " & Goal_Text(Index => 0));
--      Total_Finished := 0;
--      Stats_List := Get_Game_Stats_List(Name => "finishedGoals");
--      Count_Finished_Goals_Loop :
--      for FinishedGoal of Stats_List loop
--         Total_Finished := Total_Finished + FinishedGoal.Amount;
--      end loop Count_Finished_Goals_Loop;
--      Label.Name := New_String(Str => Stats_Canvas & ".stats.left.goals");
--      configure
--        (Widgt => Label,
--         options =>
--           "-text {Finished goals:" & Natural'Image(Total_Finished) & "}");
--      Add
--        (Widget => Label,
--         Message => "The total amount of goals finished in this game");
--      Stats_Frame :=
--        Get_Widget(pathName => Stats_Canvas & ".stats.left.goalsframe");
--      Tree_View.Name := New_String(Str => Stats_Frame & ".goalsview");
--      if Children(TreeViewWidget => Tree_View, Item => "{}") /= "{}" then
--         Delete
--           (TreeViewWidget => Tree_View,
--            ItemsList =>
--              "[list " & Children(TreeViewWidget => Tree_View, Item => "{}") &
--              "]");
--      end if;
--      if Total_Finished > 0 then
--         if Goals_Indexes.Length /= Stats_List.Length then
--            Goals_Indexes.Clear;
--            Fill_Goals_Indexes_Loop :
--            for I in Stats_List.Iterate loop
--               Goals_Indexes.Append
--                 (New_Item => Statistics_Container.To_Index(Position => I));
--            end loop Fill_Goals_Indexes_Loop;
--         end if;
--         Show_Finished_Goals_Loop :
--         for I of Goals_Indexes loop
--            Get_Proto_Goal_Loop :
--            for J in 1 .. 256 loop
--               if Get_Goal(Index => J).Index = Stats_List(I).Index then
--                  Proto_Index := J;
--                  exit Get_Proto_Goal_Loop;
--               end if;
--            end loop Get_Proto_Goal_Loop;
--            Insert
--              (TreeViewWidget => Tree_View,
--               Options =>
--                 "{} end -values [list {" & Goal_Text(Index => Proto_Index) &
--                 "} {" & Positive'Image(Stats_List(I).Amount) & "}]");
--         end loop Show_Finished_Goals_Loop;
--         configure
--           (Widgt => Tree_View,
--            options =>
--              "-height" &
--              (if Stats_List.Length < 10 then
--                 Positive'Image(Positive(Stats_List.Length))
--               else " 10"));
--         Tcl.Tk.Ada.Grid.Grid(Slave => Stats_Frame);
--      else
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Stats_Frame);
--      end if;
--      Stats_Frame :=
--        Get_Widget(pathName => Stats_Canvas & ".stats.right.destroyedframe");
--      Tree_View.Name := New_String(Str => Stats_Frame & ".destroyedview");
--      Stats_List := Get_Game_Stats_List(Name => "destroyedShips");
--      if Stats_List.Length > 0 then
--         if Children(TreeViewWidget => Tree_View, Item => "{}") /= "{}" then
--            Delete
--              (TreeViewWidget => Tree_View,
--               ItemsList =>
--                 "[list " &
--                 Children(TreeViewWidget => Tree_View, Item => "{}") & "]");
--         end if;
--         if Destroyed_Indexes.Length /= Stats_List.Length then
--            Destroyed_Indexes.Clear;
--            Fill_Destroyed_Ships_Indexes_Loop :
--            for I in Stats_List.Iterate loop
--               Destroyed_Indexes.Append
--                 (New_Item => Statistics_Container.To_Index(Position => I));
--            end loop Fill_Destroyed_Ships_Indexes_Loop;
--         end if;
--         Count_Destroyed_Ships_Loop :
--         for I of Destroyed_Indexes loop
--            Get_Proto_Ship_Loop :
--            for J in 1 .. Get_Proto_Ships_Amount loop
--               if To_Unbounded_String
--                   (Source => Trim(Source => J'Img, Side => Left)) =
--                 Stats_List(I).Index then
--                  Insert
--                    (TreeViewWidget => Tree_View,
--                     Options =>
--                       "{} end -values [list {" &
--                       To_String
--                         (Source => Get_Proto_Ship(Proto_Index => J).Name) &
--                       "} {" & Positive'Image(Stats_List(I).Amount) & "}]");
--                  exit Get_Proto_Ship_Loop;
--               end if;
--            end loop Get_Proto_Ship_Loop;
--            Total_Destroyed := Total_Destroyed + Stats_List(I).Amount;
--         end loop Count_Destroyed_Ships_Loop;
--         configure
--           (Widgt => Tree_View,
--            options =>
--              "-height" &
--              (if Stats_List.Length < 10 then
--                 Positive'Image(Positive(Stats_List.Length))
--               else " 10"));
--         Tcl.Tk.Ada.Grid.Grid(Slave => Stats_Frame);
--      else
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Stats_Frame);
--      end if;
--      Label.Name := New_String(Str => Stats_Canvas & ".stats.right.destroyed");
--      configure
--        (Widgt => Label,
--         options =>
--           "-text {Destroyed ships (Total:" & Natural'Image(Total_Destroyed) &
--           ")}");
--      Add
--        (Widget => Label,
--         Message => "The total amount of destroyed ships in this game");
--      Stats_Frame :=
--        Get_Widget(pathName => Stats_Canvas & ".stats.right.killedframe");
--      Tree_View.Name := New_String(Str => Stats_Frame & ".killedview");
--      Total_Destroyed := 0;
--      Stats_List := Get_Game_Stats_List(Name => "killedMobs");
--      if Stats_List.Length > 0 then
--         if Children(TreeViewWidget => Tree_View, Item => "{}") /= "{}" then
--            Delete
--              (TreeViewWidget => Tree_View,
--               ItemsList =>
--                 "[list " &
--                 Children(TreeViewWidget => Tree_View, Item => "{}") & "]");
--         end if;
--         if Killed_Indexes.Length /= Stats_List.Length then
--            Killed_Indexes.Clear;
--            Fill_Killed_Indexes_Loop :
--            for I in Stats_List.Iterate loop
--               Killed_Indexes.Append
--                 (New_Item => Statistics_Container.To_Index(Position => I));
--            end loop Fill_Killed_Indexes_Loop;
--         end if;
--         Show_Killed_Mobs_Loop :
--         for KilledMob of Stats_List loop
--            Insert
--              (TreeViewWidget => Tree_View,
--               Options =>
--                 "{} end -values [list {" &
--                 To_String(Source => KilledMob.Index) & "} {" &
--                 Positive'Image(KilledMob.Amount) & "}]");
--            Total_Destroyed := Total_Destroyed + KilledMob.Amount;
--         end loop Show_Killed_Mobs_Loop;
--         configure
--           (Widgt => Tree_View,
--            options =>
--              "-height" &
--              (if Stats_List.Length < 10 then
--                 Positive'Image(Positive(Stats_List.Length))
--               else " 10"));
--         Tcl.Tk.Ada.Grid.Grid(Slave => Stats_Frame);
--      else
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Stats_Frame);
--      end if;
--      Label.Name := New_String(Str => Stats_Canvas & ".stats.right.killed");
--      configure
--        (Widgt => Label,
--         options =>
--           "-text {Killed enemies (Total:" & Natural'Image(Total_Destroyed) &
--           ")}");
--      Add
--        (Widget => Label,
--         Message =>
--           "The total amount of enemies killed in melee combat in this game");
--      configure
--        (Widgt => Stats_Canvas,
--         options =>
--           "-height [expr " & SashPos(Paned => Main_Paned, Index => "0") &
--           " - 20] -width " & cget(Widgt => Main_Paned, option => "-width"));
--      Tcl_Eval(interp => Get_Context, strng => "update");
--      Stats_Frame := Get_Widget(pathName => Stats_Canvas & ".stats");
--      Canvas_Create
--        (Parent => Stats_Canvas, Child_Type => "window",
--         Options => "0 0 -anchor nw -window " & Stats_Frame);
--      Tcl_Eval(interp => Get_Context, strng => "update");
--      configure
--        (Widgt => Stats_Canvas,
--         options =>
--           "-scrollregion [list " &
--           BBox(CanvasWidget => Stats_Canvas, TagOrId => "all") & "]");
--      Show_Screen(New_Screen_Name => "statsframe");
   end Show_Statistics;

   -- ****it* SUI/SUI.Lists_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the various lists
   -- OPTIONS
   -- NAMEASC    - Sort list by name ascending
   -- NAMEDESC   - Sort list by name descending
   -- AMOUNTASC  - Sort list by amount ascending
   -- AMOUNTDESC - Sort list by amount descending
   -- NONE       - No sorting list (default)
   -- HISTORY
   -- 6.5 - Added
   -- 6.6 - Changed to List_Sort_Orders
   -- SOURCE
   type List_Sort_Orders is
     (NAMEASC, NAMEDESC, AMOUNTASC, AMOUNTDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* SUI/SUI.Default_List_Sort_Order
      -- FUNCTION
      -- Default sorting order for the various lists
      -- HISTORY
      -- 6.5 - Added
      -- 6.6 - Changed to Default_List_Sort_Order
      -- SOURCE
   Default_List_Sort_Order: constant List_Sort_Orders := NONE;
   -- ****

   --## rule off TYPE_INITIAL_VALUES
   -- ****is* SUI/SUI.Sorting_Data
   -- FUNCTION
   -- Data structure used to sort various lists
   -- PARAMETERS
   -- Name   - The name of the item (mission, goal, crafting order, etc)
   -- Amount - The amount of the item (mission, goal, crafting order, etc)
   -- Id     - The index of the item on the list
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   type Sorting_Data is record
      Name: Unbounded_String;
      Amount: Positive;
      Id: Positive;
   end record;
   -- ****

   -- ****it* SUI/SUI.Sorting_Array
   -- FUNCTION
   -- Array used to sort various lists
   -- SOURCE
   type Sorting_Array is array(Positive range <>) of Sorting_Data;
   -- ****
   --## rule on TYPE_INITIAL_VALUES

   -- ****if* SUI/SUI.Set_Sorting_Order
   -- FUNCTION
   -- Set sorting order for the selected list
   -- PARAMETERS
   -- Sorting_Order - The sorting order to set
   -- Column        - The column in ttk_tree_view whith was clicked
   -- OUTPUT
   -- Parameter Sorting_Order
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   procedure Set_Sorting_Order
     (Sorting_Order: in out List_Sort_Orders; Column: Positive) is
     -- ****
   begin
      Sorting_Order :=
        (case Column is
           when 1 => (if Sorting_Order = NAMEASC then NAMEDESC else NAMEASC),
           when 2 =>
             (if Sorting_Order = AMOUNTASC then AMOUNTDESC else AMOUNTASC),
           when others => NONE);
   end Set_Sorting_Order;

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* SUI/SUI.Crafting_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of finished crafting orders
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Crafting_Sort_Order: List_Sort_Orders := Default_List_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* SUI/SUI.Sort_Crafting_Command
   -- FUNCTION
   -- Sort the list of finished crafting orders
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortFinishedCrafting x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Crafting_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Crafting_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Column: constant Positive :=
        Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
      Crafting_Orders: constant Statistics_Container.Vector :=
        Get_Game_Stats_List(Name => "craftingOrders");
      --## rule off IMPROPER_INITIALIZATION
      Local_Crafting: Sorting_Array(1 .. Positive(Crafting_Orders.Length));
      --## rule on IMPROPER_INITIALIZATION
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      function Get_Crafting_Sort_Order return List_Sort_Orders is
      begin
         return Crafting_Sort_Order;
      end Get_Crafting_Sort_Order;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Sorting_Data) return Boolean is
      begin
         if Get_Crafting_Sort_Order = NAMEASC
           and then Left.Name < Right.Name then
            return True;
         end if;
         if Get_Crafting_Sort_Order = NAMEDESC
           and then Left.Name > Right.Name then
            return True;
         end if;
         if Get_Crafting_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Get_Crafting_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Crafting is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Sorting_Data,
         Array_Type => Sorting_Array);
   begin
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Set_Sorting_Order
        (Sorting_Order => Crafting_Sort_Order, Column => Column);
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      if Get_Crafting_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Crafting_Loop :
      for I in Crafting_Orders.Iterate loop
         Local_Crafting(Statistics_Container.To_Index(Position => I)) :=
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
                                         (Source => Crafting_Orders(I).Index)))
                               .Result_Index)
                          .Name)),
            Amount => Crafting_Orders(I).Amount,
            Id => Statistics_Container.To_Index(Position => I));
      end loop Fill_Local_Crafting_Loop;
      Sort_Crafting(Container => Local_Crafting);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Crafting_Indexes.Clear;
      Fill_Crafting_Indexes_Loop :
      for Order of Local_Crafting loop
         Crafting_Indexes.Append(New_Item => Order.Id);
      end loop Fill_Crafting_Indexes_Loop;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Show_Statistics(Refresh => True);
      return TCL_OK;
   end Sort_Crafting_Command;

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* SUI/SUI.Missions_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of finished missions
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Missions_Sort_Order: List_Sort_Orders := Default_List_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* SUI/SUI.Sort_Missions_Command
   -- FUNCTION
   -- Sort the list of finished missions
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortFinishedMissions x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Missions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Missions_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Column: constant Positive :=
        Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
      Finished_Missions: constant Statistics_Container.Vector :=
        Get_Game_Stats_List(Name => "finishedMissions");
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      --## rule off IMPROPER_INITIALIZATION
      Local_Missions: Sorting_Array(1 .. Positive(Finished_Missions.Length));
      --## rule on IMPROPER_INITIALIZATION
      function Get_Missions_Sort_Order return List_Sort_Orders is
      begin
         return Missions_Sort_Order;
      end Get_Missions_Sort_Order;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Sorting_Data) return Boolean is
      begin
         if Get_Missions_Sort_Order = NAMEASC
           and then Left.Name < Right.Name then
            return True;
         end if;
         if Get_Missions_Sort_Order = NAMEDESC
           and then Left.Name > Right.Name then
            return True;
         end if;
         if Get_Missions_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Get_Missions_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Missions is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Sorting_Data,
         Array_Type => Sorting_Array);
   begin
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Set_Sorting_Order
        (Sorting_Order => Missions_Sort_Order, Column => Column);
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      if Get_Missions_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Missions_Loop :
      for I in Finished_Missions.Iterate loop
         Local_Missions(Statistics_Container.To_Index(Position => I)) :=
           (Name =>
              (case Missions_Types'Val
                 (Integer'Value
                    (To_String(Source => Finished_Missions(I).Index))) is
                 when DELIVER =>
                   To_Unbounded_String(Source => "Delivered items"),
                 when PATROL =>
                   To_Unbounded_String(Source => "Patroled areas"),
                 when DESTROY =>
                   To_Unbounded_String(Source => "Destroyed ships"),
                 when EXPLORE =>
                   To_Unbounded_String(Source => "Explored areas"),
                 when PASSENGER =>
                   To_Unbounded_String(Source => "Passengers transported")),
            Amount => Finished_Missions(I).Amount,
            Id => Statistics_Container.To_Index(Position => I));
      end loop Fill_Local_Missions_Loop;
      Sort_Missions(Container => Local_Missions);
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      Missions_Indexes.Clear;
      Fill_Missions_Indexes_Loop :
      for Mission of Local_Missions loop
         Missions_Indexes.Append(New_Item => Mission.Id);
      end loop Fill_Missions_Indexes_Loop;
      --## rule on DIRECTLY_ACCESSED_GLOBALS
      Show_Statistics(Refresh => True);
      return TCL_OK;
   end Sort_Missions_Command;

   --## rule off DIRECTLY_ACCESSED_GLOBALS
   -- ****iv* SUI/SUI.Goals_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of finished goals
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   Goals_Sort_Order: List_Sort_Orders := Default_List_Sort_Order;
   -- ****
   --## rule on DIRECTLY_ACCESSED_GLOBALS

   -- ****o* SUI/SUI.Sort_Goals_Command
   -- FUNCTION
   -- Sort the list of finished goals
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortFinishedGoals x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Goals_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Goals_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Column: constant Positive :=
        Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
      Proto_Index: Positive := 1;
      Finished_Goals: constant Statistics_Container.Vector :=
        Get_Game_Stats_List(Name => "finishedGoals");
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      --## rule off IMPROPER_INITIALIZATION
      Local_Goals: Sorting_Array(1 .. Positive(Finished_Goals.Length));
      --## rule on IMPROPER_INITIALIZATION
      function Get_Goals_Sort_Order return List_Sort_Orders is
      begin
         return Goals_Sort_Order;
      end Get_Goals_Sort_Order;
      --## rule off DIRECTLY_ACCESSED_GLOBALS
      function "<"(Left, Right: Sorting_Data) return Boolean is
      begin
         if Get_Goals_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Get_Goals_Sort_Order = NAMEDESC
           and then Left.Name > Right.Name then
            return True;
         end if;
         if Get_Goals_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Get_Goals_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Goals is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Sorting_Data,
         Array_Type => Sorting_Array);
   begin
      Set_Sorting_Order(Sorting_Order => Goals_Sort_Order, Column => Column);
      if Get_Goals_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Goals_Loop :
      for I in Finished_Goals.Iterate loop
         Get_Proto_Goal_Loop :
         for J in 1 .. 256 loop
            if Get_Goal(Index => J).Index = Finished_Goals(I).Index then
               Proto_Index := J;
               exit Get_Proto_Goal_Loop;
            end if;
         end loop Get_Proto_Goal_Loop;
         Local_Goals(Statistics_Container.To_Index(Position => I)) :=
           (Name =>
              To_Unbounded_String(Source => Goal_Text(Index => Proto_Index)),
            Amount => Finished_Goals(I).Amount,
            Id => Statistics_Container.To_Index(Position => I));
      end loop Fill_Local_Goals_Loop;
      Sort_Goals(Container => Local_Goals);
      Goals_Indexes.Clear;
      Fill_Goals_Indexes_Loop :
      for Goal of Local_Goals loop
         Goals_Indexes.Append(New_Item => Goal.Id);
      end loop Fill_Goals_Indexes_Loop;
      Show_Statistics(Refresh => True);
      return TCL_OK;
   end Sort_Goals_Command;

   -- ****iv* SUI/SUI.Destroyed_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of destroyed enemy ships
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   Destroyed_Sort_Order: List_Sort_Orders := Default_List_Sort_Order;
   -- ****

   -- ****o* SUI/SUI.Sort_Destroyed_Command
   -- FUNCTION
   -- Sort the list of destroyed enemy ships
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortDestroyedShips x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Destroyed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Destroyed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Column: constant Positive :=
        Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
      Destroyed_Ships: constant Statistics_Container.Vector :=
        Get_Game_Stats_List(Name => "destroyedShips");
      --## rule off IMPROPER_INITIALIZATION
      Local_Destroyed: Sorting_Array(1 .. Positive(Destroyed_Ships.Length));
      --## rule on IMPROPER_INITIALIZATION
      function "<"(Left, Right: Sorting_Data) return Boolean is
      begin
         if Destroyed_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Destroyed_Sort_Order = NAMEDESC
           and then Left.Name > Right.Name then
            return True;
         end if;
         if Destroyed_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Destroyed_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Destroyed is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Sorting_Data,
         Array_Type => Sorting_Array);
   begin
      Set_Sorting_Order
        (Sorting_Order => Destroyed_Sort_Order, Column => Column);
      if Destroyed_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Destroyed_Loop :
      for I in Destroyed_Ships.Iterate loop
         Get_Proto_Ship_Loop :
         for J in 1 .. Get_Proto_Ships_Amount loop
            if To_Unbounded_String
                (Source => Trim(Source => Positive'Image(J), Side => Left)) =
              Destroyed_Ships(I).Index then
               Local_Destroyed(Statistics_Container.To_Index(Position => I)) :=
                 (Name =>
                    To_Unbounded_String
                      (Source =>
                         Tiny_String.To_String
                           (Source => Get_Proto_Ship(Proto_Index => J).Name)),
                  Amount => Destroyed_Ships(I).Amount,
                  Id => Statistics_Container.To_Index(Position => I));
               exit Get_Proto_Ship_Loop;
            end if;
         end loop Get_Proto_Ship_Loop;
      end loop Fill_Local_Destroyed_Loop;
      Sort_Destroyed(Container => Local_Destroyed);
      Destroyed_Indexes.Clear;
      Fill_Destroyed_Indexes_Loop :
      for Ship of Local_Destroyed loop
         Destroyed_Indexes.Append(New_Item => Ship.Id);
      end loop Fill_Destroyed_Indexes_Loop;
      Show_Statistics(Refresh => True);
      return TCL_OK;
   end Sort_Destroyed_Command;

   -- ****iv* SUI/SUI.Killed_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of killed enemies
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   Killed_Sort_Order: List_Sort_Orders := Default_List_Sort_Order;
   -- ****

   -- ****o* SUI/SUI.Sort_Killed_Command
   -- FUNCTION
   -- Sort the list of killed enemies
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortKilledEnemies x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Killed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Killed_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Column: constant Positive :=
        Natural'Value(CArgv.Arg(Argv => Argv, N => 1));
      Killed_Mobs: constant Statistics_Container.Vector :=
        Get_Game_Stats_List(Name => "killedMobs");
      --## rule off IMPROPER_INITIALIZATION
      Local_Killed: Sorting_Array(1 .. Positive(Killed_Mobs.Length));
      --## rule on IMPROPER_INITIALIZATION
      function "<"(Left, Right: Sorting_Data) return Boolean is
      begin
         if Killed_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Killed_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Killed_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Killed_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Killed is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Sorting_Data,
         Array_Type => Sorting_Array);
   begin
      Set_Sorting_Order(Sorting_Order => Killed_Sort_Order, Column => Column);
      if Killed_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Killed_Loop :
      for I in Killed_Mobs.Iterate loop
         Local_Killed(Statistics_Container.To_Index(Position => I)) :=
           (Name => Killed_Mobs(I).Index, Amount => Killed_Mobs(I).Amount,
            Id => Statistics_Container.To_Index(Position => I));
      end loop Fill_Local_Killed_Loop;
      Sort_Killed(Container => Local_Killed);
      Killed_Indexes.Clear;
      Fill_Killed_Indexes_Loop :
      for Mob of Local_Killed loop
         Killed_Indexes.Append(New_Item => Mob.Id);
      end loop Fill_Killed_Indexes_Loop;
      Show_Statistics(Refresh => True);
      return TCL_OK;
   end Sort_Killed_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "SortFinishedCrafting",
         Ada_Command => Sort_Crafting_Command'Access);
      Add_Command
        (Name => "SortFinishedMissions",
         Ada_Command => Sort_Missions_Command'Access);
      Add_Command
        (Name => "SortFinishedGoals",
         Ada_Command => Sort_Goals_Command'Access);
      Add_Command
        (Name => "SortDestroyedShips",
         Ada_Command => Sort_Destroyed_Command'Access);
      Add_Command
        (Name => "SortKilledMobs", Ada_Command => Sort_Killed_Command'Access);
   end Add_Commands;

end Statistics.UI;
