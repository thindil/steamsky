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

with Ada.Containers.Generic_Array_Sort;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Crafts; use Crafts;
with CoreUI; use CoreUI;
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
      Total_Finished, Total_Destroyed: Natural := 0;
      Stats_Text: Unbounded_String;
      Proto_Index: Positive;
      Stats_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".statsframe");
      Stats_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Stats_Frame & ".canvas");
      Label: Ttk_Label :=
        Get_Widget(pathName => Stats_Canvas & ".stats.left.points");
      Tree_View: Ttk_Tree_View;
   begin
      if Winfo_Get(Widgt => Label, Info => "exists") = "0" then
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "stats.tcl");
         Bind
           (Widgt => Stats_Frame, Sequence => "<Configure>",
            Script => "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Widgt => Label, Info => "ismapped") = "1" and
        not Refresh then
         Tcl_Eval
           (interp => Get_Context, strng => "InvokeButton " & Close_Button);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         return;
      end if;
      configure
        (Widgt => Label,
         options => "-text {Points:" & Natural'Image(Get_Game_Points) & "}");
      Add
        (Widget => Label,
         Message => "The amount of points gained in this game");
      Stats_Text := To_Unbounded_String(Source => "Time passed:");
      Add_Time_Info_Block :
      declare
         Minutes_Diff: constant Natural :=
           (Game_Date.Minutes + (Game_Date.Hour * 60) +
            (Game_Date.Day * 1_440) + (Game_Date.Month * 43_200) +
            (Game_Date.Year * 518_400)) -
           829_571_520;
      begin
         Minutes_To_Date(Minutes => Minutes_Diff, Info_Text => Stats_Text);
      end Add_Time_Info_Block;
      Label := Get_Widget(pathName => Stats_Canvas & ".stats.left.time");
      configure(Widgt => Label, options => "-text {" & To_String(Source => Stats_Text) & "}");
      Add(Widget => Label, Message => "In game time which was passed since it started");
      Add_Visited_Map_Block:
      declare
         type Visited_Factor is digits 4 range 0.0 .. 100.0;
         Visited_Percent: Visited_Factor;
         Visited_String: String(1 .. 5);
      begin
         Visited_Percent :=
           Visited_Factor((Float(Game_Stats.Bases_Visited) / 1_024.0) * 100.0);
         Put
           (To => Visited_String, Item => Float(Visited_Percent), Aft => 3,
            Exp => 0);
         Stats_Text :=
           To_Unbounded_String
             (Source => "Bases visited:" & Positive'Image(Game_Stats.Bases_Visited) &
              " (" & Visited_String & "%)");
         Label := Get_Widget(pathName => Stats_Canvas & ".stats.left.bases");
         configure(Widgt => Label, options => "-text {" & To_String(Source => Stats_Text) & "}");
         Add
           (Widget => Label,
            Message => "The amount of sky bases visited and total percentage of all bases");
         Visited_Percent :=
           Visited_Factor(Float(Game_Stats.Map_Visited) / (1_024.0 * 1_024.0)) *
           100.0;
         if Visited_Percent < 0.001 then
            Visited_Percent := 0.001;
         end if;
         Put
           (To => Visited_String, Item => Float(Visited_Percent), Aft => 3,
            Exp => 0);
         Stats_Text :=
           To_Unbounded_String(Source => "Map discovered: " & Visited_String & "%");
         Label := Get_Widget(pathName => Stats_Canvas & ".stats.left.map");
         configure(Widgt => Label, options => "-text {" & To_String(Source => Stats_Text) & "}");
         Add(Widget => Label, Message => "The amount of unique map's fields visited");
      end Add_Visited_Map_Block;
      Stats_Text :=
        To_Unbounded_String
          (Source => "Distance traveled:" & Natural'Image(Game_Stats.Distance_Traveled));
      Label := Get_Widget(pathName => Stats_Canvas & ".stats.left.distance");
      configure(Widgt => Label, options => "-text {" & To_String(Source => Stats_Text) & "}");
      Add(Widget => Label, Message => "The total amount of map's fields visited");
      Stats_Frame.Name := New_String(Str => Stats_Canvas & ".stats");
      Total_Finished := 0;
      Count_Finished_Crafting_Loop :
      for CraftingOrder of Game_Stats.Crafting_Orders loop
         Total_Finished := Total_Finished + CraftingOrder.Amount;
      end loop Count_Finished_Crafting_Loop;
      Label.Name := New_String(Str => Stats_Frame & ".left.crafts");
      configure
        (Widgt => Label,
         options => "-text {Crafting orders finished:" & Natural'Image(Total_Finished) &
         "}");
      Add(Widget => Label, Message => "The total amount of crafting orders finished in this game");
      Stats_Frame := Get_Widget(pathName => Stats_Canvas & ".stats.left.craftsframe");
      Tree_View := Get_Widget(pathName => Stats_Frame & ".craftsview");
      if Children(TreeViewWidget => Tree_View, Item => "{}") /= "{}" then
         Delete(TreeViewWidget => Tree_View, ItemsList => "[list " & Children(TreeViewWidget => Tree_View, Item => "{}") & "]");
      end if;
      if Total_Finished > 0 then
         if Crafting_Indexes.Length /= Game_Stats.Crafting_Orders.Length then
            Crafting_Indexes.Clear;
            Fill_Crafting_Indexes_Loop:
            for I in Game_Stats.Crafting_Orders.Iterate loop
               Crafting_Indexes.Append(New_Item => Statistics_Container.To_Index(Position => I));
            end loop Fill_Crafting_Indexes_Loop;
         end if;
         Show_Finished_Crafting_Loop :
         for I of Crafting_Indexes loop
            Insert
              (TreeViewWidget => Tree_View,
               Options => "{} end -values [list {" &
               To_String
                 (Source => Objects_Container.Element
                    (Container => Items_List,
                     Index =>
                       Recipes_List
                         (To_Bounded_String
                            (Source =>
                               To_String
                                 (Source =>
                                    Game_Stats.Crafting_Orders(I).Index)))
                         .Result_Index)
                    .Name) &
               "} {" & Positive'Image(Game_Stats.Crafting_Orders(I).Amount) &
               "}]");
         end loop Show_Finished_Crafting_Loop;
         configure
           (Widgt => Tree_View,
            options => "-height" &
            (if Game_Stats.Crafting_Orders.Length < 10 then
               Positive'Image(Positive(Game_Stats.Crafting_Orders.Length))
             else " 10"));
         Tcl.Tk.Ada.Grid.Grid(Slave => Stats_Frame);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Stats_Frame);
      end if;
      Total_Finished := 0;
      Count_Finished_Missions_Loop :
      for FinishedMission of Game_Stats.Finished_Missions loop
         Total_Finished := Total_Finished + FinishedMission.Amount;
      end loop Count_Finished_Missions_Loop;
      Label.Name := New_String(Str => Stats_Canvas & ".stats.left.missions");
      Add_Finished_Missions_Block:
      declare
         Missions_Percent: Natural := 0;
      begin
         if Game_Stats.Accepted_Missions > 0 then
            Missions_Percent :=
              Natural
                ((Float(Total_Finished) /
                  Float(Game_Stats.Accepted_Missions)) *
                 100.0);
         end if;
         configure
           (Widgt => Label,
            options => "-text {Missions completed:" & Natural'Image(Total_Finished) &
            " (" &
            To_String
              (Source => Trim
                 (Source => To_Unbounded_String(Natural'Image(Missions_Percent)),
                  Side => Ada.Strings.Left)) &
            "%)" & "}");
         Add(Widget => Label, Message => "The total amount of missions finished in this game");
      end Add_Finished_Missions_Block;
      Stats_Frame := Get_Widget(pathName => Stats_Canvas & ".stats.left.missionsframe");
      Tree_View.Name := New_String(Str => Stats_Frame & ".missionsview");
      if Children(TreeViewWidget => Tree_View, Item => "{}") /= "{}" then
         Delete(TreeViewWidget => Tree_View, ItemsList => "[list " & Children(TreeViewWidget => Tree_View, Item => "{}") & "]");
      end if;
      if Total_Finished > 0 then
         if Missions_Indexes.Length /= Game_Stats.Finished_Missions.Length then
            Missions_Indexes.Clear;
            Fill_Missions_Indexes_Loop:
            for I in Game_Stats.Finished_Missions.Iterate loop
               Missions_Indexes.Append(New_Item => Statistics_Container.To_Index(Position => I));
            end loop Fill_Missions_Indexes_Loop;
         end if;
         Show_Finished_Missions_Loop :
         for I of Missions_Indexes loop
            case Missions_Types'Val
              (Integer'Value
                 (To_String(Source => Game_Stats.Finished_Missions(I).Index))) is
               when DELIVER =>
                  Insert
                    (TreeViewWidget => Tree_View,
                     Options => "{} end -values [list {Delivered items} {" &
                     Positive'Image(Game_Stats.Finished_Missions(I).Amount) &
                     "}]");
               when PATROL =>
                  Insert
                    (TreeViewWidget => Tree_View,
                     Options => "{} end -values [list {Patroled areas} {" &
                     Positive'Image(Game_Stats.Finished_Missions(I).Amount) &
                     "}]");
               when DESTROY =>
                  Insert
                    (TreeViewWidget => Tree_View,
                     Options => "{} end -values [list {Destroyed ships} {" &
                     Positive'Image(Game_Stats.Finished_Missions(I).Amount) &
                     "}]");
               when EXPLORE =>
                  Insert
                    (TreeViewWidget => Tree_View,
                     Options => "{} end -values [list {Explored areas} {" &
                     Positive'Image(Game_Stats.Finished_Missions(I).Amount) &
                     "}]");
               when PASSENGER =>
                  Insert
                    (TreeViewWidget => Tree_View,
                     Options => "{} end -values [list {Passengers transported} {" &
                     Positive'Image(Game_Stats.Finished_Missions(I).Amount) &
                     "}]");
            end case;
         end loop Show_Finished_Missions_Loop;
         configure
           (Widgt => Tree_View,
            options => "-height" &
            (if Game_Stats.Finished_Missions.Length < 10 then
               Positive'Image(Positive(Game_Stats.Finished_Missions.Length))
             else " 10"));
         Tcl.Tk.Ada.Grid.Grid(Slave => Stats_Frame);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Stats_Frame);
      end if;
      Label.Name := New_String(Str => Stats_Canvas & ".stats.left.goal");
      configure
        (Widgt => Label,
         options => "-text {" &
         (if Goal_Text(Index => 0)'Length < 22 then Goal_Text(Index => 0)
          else Goal_Text(Index => 0)(1 .. 22)) &
         "...}");
      Add(Label, "The current goal: " & Goal_Text(0));
      Total_Finished := 0;
      Count_Finished_Goals_Loop :
      for FinishedGoal of Game_Stats.Finished_Goals loop
         Total_Finished := Total_Finished + FinishedGoal.Amount;
      end loop Count_Finished_Goals_Loop;
      Label.Name := New_String(Stats_Canvas & ".stats.left.goals");
      configure
        (Label,
         "-text {Finished goals:" & Natural'Image(Total_Finished) & "}");
      Add(Label, "The total amount of goals finished in this game");
      Stats_Frame := Get_Widget(Stats_Canvas & ".stats.left.goalsframe");
      Tree_View.Name := New_String(Stats_Frame & ".goalsview");
      if Children(Tree_View, "{}") /= "{}" then
         Delete(Tree_View, "[list " & Children(Tree_View, "{}") & "]");
      end if;
      if Total_Finished > 0 then
         if Goals_Indexes.Length /= Game_Stats.Finished_Goals.Length then
            Goals_Indexes.Clear;
            for I in Game_Stats.Finished_Goals.Iterate loop
               Goals_Indexes.Append(Statistics_Container.To_Index(I));
            end loop;
         end if;
         Show_Finished_Goals_Loop :
         for I of Goals_Indexes loop
            Get_Proto_Goal_Loop :
            for J in Goals_List.Iterate loop
               if Goals_List(J).Index = Game_Stats.Finished_Goals(I).Index then
                  Proto_Index := Goals_Container.To_Index(J);
                  exit Get_Proto_Goal_Loop;
               end if;
            end loop Get_Proto_Goal_Loop;
            Insert
              (Tree_View,
               "{} end -values [list {" & Goal_Text(Proto_Index) & "} {" &
               Positive'Image(Game_Stats.Finished_Goals(I).Amount) & "}]");
         end loop Show_Finished_Goals_Loop;
         configure
           (Tree_View,
            "-height" &
            (if Game_Stats.Finished_Goals.Length < 10 then
               Positive'Image(Positive(Game_Stats.Finished_Goals.Length))
             else " 10"));
         Tcl.Tk.Ada.Grid.Grid(Stats_Frame);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Stats_Frame);
      end if;
      Stats_Frame := Get_Widget(Stats_Canvas & ".stats.right.destroyedframe");
      Tree_View.Name := New_String(Stats_Frame & ".destroyedview");
      if Game_Stats.Destroyed_Ships.Length > 0 then
         if Children(Tree_View, "{}") /= "{}" then
            Delete(Tree_View, "[list " & Children(Tree_View, "{}") & "]");
         end if;
         if Destroyed_Indexes.Length /= Game_Stats.Destroyed_Ships.Length then
            Destroyed_Indexes.Clear;
            for I in Game_Stats.Destroyed_Ships.Iterate loop
               Destroyed_Indexes.Append(Statistics_Container.To_Index(I));
            end loop;
         end if;
         Count_Destroyed_Ships_Loop :
         for I of Destroyed_Indexes loop
            Get_Proto_Ship_Loop :
            for J in Proto_Ships_List.Iterate loop
               if To_Unbounded_String
                   (Source => Proto_Ships_Container.To_Index(J)'Img) =
                 Game_Stats.Destroyed_Ships(I).Index then
                  Insert
                    (Tree_View,
                     "{} end -values [list {" &
                     To_String(Proto_Ships_List(J).Name) & "} {" &
                     Positive'Image(Game_Stats.Destroyed_Ships(I).Amount) &
                     "}]");
                  exit Get_Proto_Ship_Loop;
               end if;
            end loop Get_Proto_Ship_Loop;
            Total_Destroyed :=
              Total_Destroyed + Game_Stats.Destroyed_Ships(I).Amount;
         end loop Count_Destroyed_Ships_Loop;
         configure
           (Tree_View,
            "-height" &
            (if Game_Stats.Destroyed_Ships.Length < 10 then
               Positive'Image(Positive(Game_Stats.Destroyed_Ships.Length))
             else " 10"));
         Tcl.Tk.Ada.Grid.Grid(Stats_Frame);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Stats_Frame);
      end if;
      Label.Name := New_String(Stats_Canvas & ".stats.right.destroyed");
      configure
        (Label,
         "-text {Destroyed ships (Total:" & Natural'Image(Total_Destroyed) &
         ")}");
      Add(Label, "The total amount of destroyed ships in this game");
      Stats_Frame := Get_Widget(Stats_Canvas & ".stats.right.killedframe");
      Tree_View.Name := New_String(Stats_Frame & ".killedview");
      Total_Destroyed := 0;
      if Game_Stats.Killed_Mobs.Length > 0 then
         if Children(Tree_View, "{}") /= "{}" then
            Delete(Tree_View, "[list " & Children(Tree_View, "{}") & "]");
         end if;
         if Killed_Indexes.Length /= Game_Stats.Killed_Mobs.Length then
            Killed_Indexes.Clear;
            for I in Game_Stats.Killed_Mobs.Iterate loop
               Killed_Indexes.Append(Statistics_Container.To_Index(I));
            end loop;
         end if;
         Show_Killed_Mobs_Loop :
         for KilledMob of Game_Stats.Killed_Mobs loop
            Insert
              (Tree_View,
               "{} end -values [list {" & To_String(KilledMob.Index) & "} {" &
               Positive'Image(KilledMob.Amount) & "}]");
            Total_Destroyed := Total_Destroyed + KilledMob.Amount;
         end loop Show_Killed_Mobs_Loop;
         configure
           (Tree_View,
            "-height" &
            (if Game_Stats.Killed_Mobs.Length < 10 then
               Positive'Image(Positive(Game_Stats.Killed_Mobs.Length))
             else " 10"));
         Tcl.Tk.Ada.Grid.Grid(Stats_Frame);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Stats_Frame);
      end if;
      Label.Name := New_String(Stats_Canvas & ".stats.right.killed");
      configure
        (Label,
         "-text {Killed enemies (Total:" & Natural'Image(Total_Destroyed) &
         ")}");
      Add
        (Label,
         "The total amount of enemies killed in melee combat in this game");
      configure
        (Stats_Canvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Stats_Frame := Get_Widget(Stats_Canvas & ".stats");
      Canvas_Create
        (Stats_Canvas, "window", "0 0 -anchor nw -window " & Stats_Frame);
      Tcl_Eval(Get_Context, "update");
      configure
        (Stats_Canvas,
         "-scrollregion [list " & BBox(Stats_Canvas, "all") & "]");
      Show_Screen("statsframe");
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

   -- ****iv* SUI/SUI.Crafting_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of finished crafting orders
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Crafting_Sort_Order: List_Sort_Orders := Default_List_Sort_Order;
   -- ****

   -- ****o* SUI/SUI.Sort_Crafting_Command
   -- FUNCTION
   -- Sort the list of finished crafting orders
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortFinishedCrafting x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Crafting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Crafting_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Column: constant Positive := Natural'Value(CArgv.Arg(Argv, 1));
      Local_Crafting: Sorting_Array
        (1 .. Positive(Game_Stats.Crafting_Orders.Length));
      function "<"(Left, Right: Sorting_Data) return Boolean is
      begin
         if Crafting_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Crafting_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Crafting_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Crafting_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Crafting is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Sorting_Data,
         Array_Type => Sorting_Array);
   begin
      Set_Sorting_Order(Crafting_Sort_Order, Column);
      if Crafting_Sort_Order = NONE then
         return TCL_OK;
      end if;
      for I in Game_Stats.Crafting_Orders.Iterate loop
         Local_Crafting(Statistics_Container.To_Index(I)) :=
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
                                     To_String
                                       (Source =>
                                          Game_Stats.Crafting_Orders(I)
                                            .Index)))
                               .Result_Index)
                          .Name)),
            Amount => Game_Stats.Crafting_Orders(I).Amount,
            Id => Statistics_Container.To_Index(I));
      end loop;
      Sort_Crafting(Local_Crafting);
      Crafting_Indexes.Clear;
      for Order of Local_Crafting loop
         Crafting_Indexes.Append(Order.Id);
      end loop;
      Show_Statistics(True);
      return TCL_OK;
   end Sort_Crafting_Command;

   -- ****iv* SUI/SUI.Missions_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of finished missions
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Missions_Sort_Order: List_Sort_Orders := Default_List_Sort_Order;
   -- ****

   -- ****o* SUI/SUI.Sort_Missions_Command
   -- FUNCTION
   -- Sort the list of finished missions
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortFinishedMissions x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Missions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Missions_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Column: constant Positive := Natural'Value(CArgv.Arg(Argv, 1));
      Local_Missions: Sorting_Array
        (1 .. Positive(Game_Stats.Finished_Missions.Length));
      function "<"(Left, Right: Sorting_Data) return Boolean is
      begin
         if Missions_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Missions_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Missions_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Missions_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Missions is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Sorting_Data,
         Array_Type => Sorting_Array);
   begin
      Set_Sorting_Order(Missions_Sort_Order, Column);
      if Missions_Sort_Order = NONE then
         return TCL_OK;
      end if;
      for I in Game_Stats.Finished_Missions.Iterate loop
         Local_Missions(Statistics_Container.To_Index(I)) :=
           (Name =>
              (case Missions_Types'Val
                 (Integer'Value
                    (To_String(Game_Stats.Finished_Missions(I).Index))) is
                 when DELIVER => To_Unbounded_String("Delivered items"),
                 when PATROL => To_Unbounded_String("Patroled areas"),
                 when DESTROY => To_Unbounded_String("Destroyed ships"),
                 when EXPLORE => To_Unbounded_String("Explored areas"),
                 when PASSENGER =>
                   To_Unbounded_String("Passengers transported")),
            Amount => Game_Stats.Finished_Missions(I).Amount,
            Id => Statistics_Container.To_Index(I));
      end loop;
      Sort_Missions(Local_Missions);
      Missions_Indexes.Clear;
      for Mission of Local_Missions loop
         Missions_Indexes.Append(Mission.Id);
      end loop;
      Show_Statistics(True);
      return TCL_OK;
   end Sort_Missions_Command;

   -- ****iv* SUI/SUI.Goals_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of finished goals
   -- HISTORY
   -- 6.6 - Added
   -- SOURCE
   Goals_Sort_Order: List_Sort_Orders := Default_List_Sort_Order;
   -- ****

   -- ****o* SUI/SUI.Sort_Goals_Command
   -- FUNCTION
   -- Sort the list of finished goals
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortFinishedGoals x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Goals_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Goals_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Column: constant Positive := Natural'Value(CArgv.Arg(Argv, 1));
      ProtoIndex: Positive := 1;
      Local_Goals: Sorting_Array
        (1 .. Positive(Game_Stats.Finished_Goals.Length));
      function "<"(Left, Right: Sorting_Data) return Boolean is
      begin
         if Goals_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Goals_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Goals_Sort_Order = AMOUNTASC
           and then Left.Amount < Right.Amount then
            return True;
         end if;
         if Goals_Sort_Order = AMOUNTDESC
           and then Left.Amount > Right.Amount then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Goals is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Sorting_Data,
         Array_Type => Sorting_Array);
   begin
      Set_Sorting_Order(Goals_Sort_Order, Column);
      if Goals_Sort_Order = NONE then
         return TCL_OK;
      end if;
      for I in Game_Stats.Finished_Goals.Iterate loop
         Get_Proto_Goal_Loop :
         for J in Goals_List.Iterate loop
            if Goals_List(J).Index = Game_Stats.Finished_Goals(I).Index then
               ProtoIndex := Goals_Container.To_Index(J);
               exit Get_Proto_Goal_Loop;
            end if;
         end loop Get_Proto_Goal_Loop;
         Local_Goals(Statistics_Container.To_Index(I)) :=
           (Name => To_Unbounded_String(Goal_Text(ProtoIndex)),
            Amount => Game_Stats.Finished_Goals(I).Amount,
            Id => Statistics_Container.To_Index(I));
      end loop;
      Sort_Goals(Local_Goals);
      Goals_Indexes.Clear;
      for Goal of Local_Goals loop
         Goals_Indexes.Append(Goal.Id);
      end loop;
      Show_Statistics(True);
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
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortDestroyedShips x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Destroyed_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Destroyed_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Column: constant Positive := Natural'Value(CArgv.Arg(Argv, 1));
      Local_Destroyed: Sorting_Array
        (1 .. Positive(Game_Stats.Destroyed_Ships.Length));
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
      Set_Sorting_Order(Destroyed_Sort_Order, Column);
      if Destroyed_Sort_Order = NONE then
         return TCL_OK;
      end if;
      for I in Game_Stats.Destroyed_Ships.Iterate loop
         Get_Proto_Ship_Loop :
         for J in Proto_Ships_List.Iterate loop
            if To_Unbounded_String
                (Source =>
                   Trim
                     (Source =>
                        Positive'Image
                          (Proto_Ships_Container.To_Index(Position => J)),
                      Side => Left)) =
              Game_Stats.Destroyed_Ships(I).Index then
               Local_Destroyed(Statistics_Container.To_Index(I)) :=
                 (Name =>
                    To_Unbounded_String
                      (Source =>
                         Tiny_String.To_String
                           (Source => Proto_Ships_List(J).Name)),
                  Amount => Game_Stats.Destroyed_Ships(I).Amount,
                  Id => Statistics_Container.To_Index(I));
               exit Get_Proto_Ship_Loop;
            end if;
         end loop Get_Proto_Ship_Loop;
      end loop;
      Sort_Destroyed(Local_Destroyed);
      Destroyed_Indexes.Clear;
      for Ship of Local_Destroyed loop
         Destroyed_Indexes.Append(Ship.Id);
      end loop;
      Show_Statistics(True);
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
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortKilledEnemies x
   -- X is the number of column where the player clicked the mouse button
   -- SOURCE
   function Sort_Killed_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Killed_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      Column: constant Positive := Natural'Value(CArgv.Arg(Argv, 1));
      Local_Killed: Sorting_Array
        (1 .. Positive(Game_Stats.Killed_Mobs.Length));
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
      Set_Sorting_Order(Killed_Sort_Order, Column);
      if Killed_Sort_Order = NONE then
         return TCL_OK;
      end if;
      for I in Game_Stats.Killed_Mobs.Iterate loop
         Local_Killed(Statistics_Container.To_Index(I)) :=
           (Name => Game_Stats.Killed_Mobs(I).Index,
            Amount => Game_Stats.Killed_Mobs(I).Amount,
            Id => Statistics_Container.To_Index(I));
      end loop;
      Sort_Killed(Local_Killed);
      Killed_Indexes.Clear;
      for Mob of Local_Killed loop
         Killed_Indexes.Append(Mob.Id);
      end loop;
      Show_Statistics(True);
      return TCL_OK;
   end Sort_Killed_Command;

   procedure Add_Commands is
   begin
      Add_Command("SortFinishedCrafting", Sort_Crafting_Command'Access);
      Add_Command("SortFinishedMissions", Sort_Missions_Command'Access);
      Add_Command("SortFinishedGoals", Sort_Goals_Command'Access);
      Add_Command("SortDestroyedShips", Sort_Destroyed_Command'Access);
      Add_Command("SortKilledMobs", Sort_Killed_Command'Access);
   end Add_Commands;

end Statistics.UI;
