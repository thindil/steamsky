-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Maps.UI; use Maps.UI;
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

   procedure ShowStatistics(Refresh: Boolean := False) is
      TotalFinished, TotalDestroyed: Natural := 0;
      StatsText: Unbounded_String;
      ProtoIndex: Positive;
      StatsFrame: Ttk_Frame := Get_Widget(Main_Paned & ".statsframe");
      StatsCanvas: constant Tk_Canvas := Get_Widget(StatsFrame & ".canvas");
      Label: Ttk_Label := Get_Widget(StatsCanvas & ".stats.left.points");
      TreeView: Ttk_Tree_View;
   begin
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "stats.tcl");
         Bind(StatsFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" and not Refresh then
         Tcl_Eval(Get_Context, "InvokeButton " & Close_Button);
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         return;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      configure(Label, "-text {Points:" & Natural'Image(GetGamePoints) & "}");
      Add(Label, "The amount of points gained in this game");
      StatsText := To_Unbounded_String("Time passed:");
      declare
         MinutesDiff: constant Natural :=
           (Game_Date.Minutes + (Game_Date.Hour * 60) +
            (Game_Date.Day * 1_440) + (Game_Date.Month * 43_200) +
            (Game_Date.Year * 518_400)) -
           829_571_520;
      begin
         MinutesToDate(MinutesDiff, StatsText);
      end;
      Label := Get_Widget(StatsCanvas & ".stats.left.time");
      configure(Label, "-text {" & To_String(StatsText) & "}");
      Add(Label, "In game time which was passed since it started");
      declare
         type VisitedFactor is digits 4 range 0.0 .. 100.0;
         VisitedPercent: VisitedFactor;
         VisitedString: String(1 .. 5);
      begin
         VisitedPercent :=
           VisitedFactor((Float(GameStats.BasesVisited) / 1_024.0) * 100.0);
         Put
           (To => VisitedString, Item => Float(VisitedPercent), Aft => 3,
            Exp => 0);
         StatsText :=
           To_Unbounded_String
             ("Bases visited:" & Positive'Image(GameStats.BasesVisited) &
              " (" & VisitedString & "%)");
         Label := Get_Widget(StatsCanvas & ".stats.left.bases");
         configure(Label, "-text {" & To_String(StatsText) & "}");
         Add
           (Label,
            "The amount of sky bases visited and total percentage of all bases");
         VisitedPercent :=
           VisitedFactor(Float(GameStats.MapVisited) / (1_024.0 * 1_024.0)) *
           100.0;
         if VisitedPercent < 0.001 then
            VisitedPercent := 0.001;
         end if;
         Put
           (To => VisitedString, Item => Float(VisitedPercent), Aft => 3,
            Exp => 0);
         StatsText :=
           To_Unbounded_String("Map discovered: " & VisitedString & "%");
         Label := Get_Widget(StatsCanvas & ".stats.left.map");
         configure(Label, "-text {" & To_String(StatsText) & "}");
         Add(Label, "The amount of unique map's fields visited");
      end;
      StatsText :=
        To_Unbounded_String
          ("Distance traveled:" & Natural'Image(GameStats.DistanceTraveled));
      Label := Get_Widget(StatsCanvas & ".stats.left.distance");
      configure(Label, "-text {" & To_String(StatsText) & "}");
      Add(Label, "The total amount of map's fields visited");
      StatsFrame.Name := New_String(StatsCanvas & ".stats");
      TotalFinished := 0;
      Count_Finished_Crafting_Loop :
      for CraftingOrder of GameStats.CraftingOrders loop
         TotalFinished := TotalFinished + CraftingOrder.Amount;
      end loop Count_Finished_Crafting_Loop;
      Label.Name := New_String(StatsFrame & ".left.crafts");
      configure
        (Label,
         "-text {Crafting orders finished:" & Natural'Image(TotalFinished) &
         "}");
      Add(Label, "The total amount of crafting orders finished in this game");
      StatsFrame := Get_Widget(StatsCanvas & ".stats.left.craftsframe");
      TreeView := Get_Widget(StatsFrame & ".craftsview");
      if Children(TreeView, "{}") /= "{}" then
         Delete(TreeView, "[list " & Children(TreeView, "{}") & "]");
      end if;
      if TotalFinished > 0 then
         if Crafting_Indexes.Length /= GameStats.CraftingOrders.Length then
            Crafting_Indexes.Clear;
            for I in GameStats.CraftingOrders.Iterate loop
               Crafting_Indexes.Append(Statistics_Container.To_Index(I));
            end loop;
         end if;
         Show_Finished_Crafting_Loop :
         for I of Crafting_Indexes loop
            Insert
              (TreeView,
               "{} end -values [list {" &
               To_String
                 (Items_List
                    (Recipes_List(GameStats.CraftingOrders(I).Index)
                       .ResultIndex)
                    .Name) &
               "} {" & Positive'Image(GameStats.CraftingOrders(I).Amount) &
               "}]");
         end loop Show_Finished_Crafting_Loop;
         configure
           (TreeView,
            "-height" &
            (if GameStats.CraftingOrders.Length < 10 then
               Positive'Image(Positive(GameStats.CraftingOrders.Length))
             else " 10"));
         Tcl.Tk.Ada.Grid.Grid(StatsFrame);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(StatsFrame);
      end if;
      TotalFinished := 0;
      Count_Finished_Missions_Loop :
      for FinishedMission of GameStats.FinishedMissions loop
         TotalFinished := TotalFinished + FinishedMission.Amount;
      end loop Count_Finished_Missions_Loop;
      Label.Name := New_String(StatsCanvas & ".stats.left.missions");
      declare
         MissionsPercent: Natural := 0;
      begin
         if GameStats.AcceptedMissions > 0 then
            MissionsPercent :=
              Natural
                ((Float(TotalFinished) / Float(GameStats.AcceptedMissions)) *
                 100.0);
         end if;
         configure
           (Label,
            "-text {Missions completed:" & Natural'Image(TotalFinished) &
            " (" &
            To_String
              (Trim
                 (To_Unbounded_String(Natural'Image(MissionsPercent)),
                  Ada.Strings.Left)) &
            "%)" & "}");
         Add(Label, "The total amount of missions finished in this game");
      end;
      StatsFrame := Get_Widget(StatsCanvas & ".stats.left.missionsframe");
      TreeView.Name := New_String(StatsFrame & ".missionsview");
      if Children(TreeView, "{}") /= "{}" then
         Delete(TreeView, "[list " & Children(TreeView, "{}") & "]");
      end if;
      if TotalFinished > 0 then
         Show_Finished_Missions_Loop :
         for FinishedMission of GameStats.FinishedMissions loop
            case Missions_Types'Val
              (Integer'Value(To_String(FinishedMission.Index))) is
               when Deliver =>
                  Insert
                    (TreeView,
                     "{} end -values [list {Delivered items} {" &
                     Positive'Image(FinishedMission.Amount) & "}]");
               when Patrol =>
                  Insert
                    (TreeView,
                     "{} end -values [list {Patroled areas} {" &
                     Positive'Image(FinishedMission.Amount) & "}]");
               when Destroy =>
                  Insert
                    (TreeView,
                     "{} end -values [list {Destroyed ships} {" &
                     Positive'Image(FinishedMission.Amount) & "}]");
               when Explore =>
                  Insert
                    (TreeView,
                     "{} end -values [list {Explored areas} {" &
                     Positive'Image(FinishedMission.Amount) & "}]");
               when Passenger =>
                  Insert
                    (TreeView,
                     "{} end -values [list {Passengers transported} {" &
                     Positive'Image(FinishedMission.Amount) & "}]");
            end case;
         end loop Show_Finished_Missions_Loop;
         configure
           (TreeView,
            "-height" &
            (if GameStats.FinishedMissions.Length < 10 then
               Positive'Image(Positive(GameStats.FinishedMissions.Length))
             else " 10"));
         Tcl.Tk.Ada.Grid.Grid(StatsFrame);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(StatsFrame);
      end if;
      Label.Name := New_String(StatsCanvas & ".stats.left.goal");
      configure
        (Label,
         "-text {" &
         (if GoalText(0)'Length < 22 then GoalText(0)
          else GoalText(0)(1 .. 22)) &
         "...}");
      Add(Label, "The current goal: " & GoalText(0));
      TotalFinished := 0;
      Count_Finished_Goals_Loop :
      for FinishedGoal of GameStats.FinishedGoals loop
         TotalFinished := TotalFinished + FinishedGoal.Amount;
      end loop Count_Finished_Goals_Loop;
      Label.Name := New_String(StatsCanvas & ".stats.left.goals");
      configure
        (Label, "-text {Finished goals:" & Natural'Image(TotalFinished) & "}");
      Add(Label, "The total amount of goals finished in this game");
      StatsFrame := Get_Widget(StatsCanvas & ".stats.left.goalsframe");
      TreeView.Name := New_String(StatsFrame & ".goalsview");
      if Children(TreeView, "{}") /= "{}" then
         Delete(TreeView, "[list " & Children(TreeView, "{}") & "]");
      end if;
      if TotalFinished > 0 then
         Show_Finished_Goals_Loop :
         for Goal of GameStats.FinishedGoals loop
            Get_Proto_Goal_Loop :
            for J in Goals_List.Iterate loop
               if Goal.Index = Goals_List(J).Index then
                  ProtoIndex := Goals_Container.To_Index(J);
                  exit Get_Proto_Goal_Loop;
               end if;
            end loop Get_Proto_Goal_Loop;
            Insert
              (TreeView,
               "{} end -values [list {" & GoalText(ProtoIndex) & "} {" &
               Positive'Image(Goal.Amount) & "}]");
         end loop Show_Finished_Goals_Loop;
         configure
           (TreeView,
            "-height" &
            (if GameStats.FinishedGoals.Length < 10 then
               Positive'Image(Positive(GameStats.FinishedGoals.Length))
             else " 10"));
         Tcl.Tk.Ada.Grid.Grid(StatsFrame);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(StatsFrame);
      end if;
      StatsFrame := Get_Widget(StatsCanvas & ".stats.right.destroyedframe");
      TreeView.Name := New_String(StatsFrame & ".destroyedview");
      if GameStats.DestroyedShips.Length > 0 then
         if Children(TreeView, "{}") /= "{}" then
            Delete(TreeView, "[list " & Children(TreeView, "{}") & "]");
         end if;
         Count_Destroyed_Ships_Loop :
         for DestroyedShip of GameStats.DestroyedShips loop
            Get_Proto_Ship_Loop :
            for J in Proto_Ships_List.Iterate loop
               if Proto_Ships_Container.Key(J) = DestroyedShip.Index then
                  Insert
                    (TreeView,
                     "{} end -values [list {" &
                     To_String(Proto_Ships_List(J).Name) & "} {" &
                     Positive'Image(DestroyedShip.Amount) & "}]");
                  exit Get_Proto_Ship_Loop;
               end if;
            end loop Get_Proto_Ship_Loop;
            TotalDestroyed := TotalDestroyed + DestroyedShip.Amount;
         end loop Count_Destroyed_Ships_Loop;
         configure
           (TreeView,
            "-height" &
            (if GameStats.DestroyedShips.Length < 10 then
               Positive'Image(Positive(GameStats.DestroyedShips.Length))
             else " 10"));
         Tcl.Tk.Ada.Grid.Grid(StatsFrame);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(StatsFrame);
      end if;
      Label.Name := New_String(StatsCanvas & ".stats.right.destroyed");
      configure
        (Label,
         "-text {Destroyed ships (Total:" & Natural'Image(TotalDestroyed) &
         ")}");
      Add(Label, "The total amount of destroyed ships in this game");
      StatsFrame := Get_Widget(StatsCanvas & ".stats.right.killedframe");
      TreeView.Name := New_String(StatsFrame & ".killedview");
      TotalDestroyed := 0;
      if GameStats.KilledMobs.Length > 0 then
         if Children(TreeView, "{}") /= "{}" then
            Delete(TreeView, "[list " & Children(TreeView, "{}") & "]");
         end if;
         Show_Killed_Mobs_Loop :
         for KilledMob of GameStats.KilledMobs loop
            Insert
              (TreeView,
               "{} end -values [list {" & To_String(KilledMob.Index) & "} {" &
               Positive'Image(KilledMob.Amount) & "}]");
            TotalDestroyed := TotalDestroyed + KilledMob.Amount;
         end loop Show_Killed_Mobs_Loop;
         configure
           (TreeView,
            "-height" &
            (if GameStats.KilledMobs.Length < 10 then
               Positive'Image(Positive(GameStats.KilledMobs.Length))
             else " 10"));
         Tcl.Tk.Ada.Grid.Grid(StatsFrame);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(StatsFrame);
      end if;
      Label.Name := New_String(StatsCanvas & ".stats.right.killed");
      configure
        (Label,
         "-text {Killed enemies (Total:" & Natural'Image(TotalDestroyed) &
         ")}");
      Add
        (Label,
         "The total amount of enemies killed in melee combat in this game");
      configure
        (StatsCanvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      StatsFrame := Get_Widget(StatsCanvas & ".stats");
      Canvas_Create
        (StatsCanvas, "window", "0 0 -anchor nw -window " & StatsFrame);
      Tcl_Eval(Get_Context, "update");
      configure
        (StatsCanvas, "-scrollregion [list " & BBox(StatsCanvas, "all") & "]");
      ShowScreen("statsframe");
   end ShowStatistics;

   -- ****it* SUI/SUI.Crafting_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the finished crafting orders list
   -- OPTIONS
   -- NAMEASC    - Sort orders by name ascending
   -- NAMEDESC   - Sort orders by name descending
   -- AMOUNTASC  - Sort orders by amount ascending
   -- AMOUNTDESC - Sort orders by amount descending
   -- NONE       - No sorting orders (default)
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   type Crafting_Sort_Orders is
     (NAMEASC, NAMEDESC, AMOUNTASC, AMOUNTDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* SUI/SUI.Default_Crafting_Sort_Order
      -- FUNCTION
      -- Default sorting order for the finished crafting orders list
      -- HISTORY
      -- 6.5 - Added
      -- SOURCE
   Default_Crafting_Sort_Order: constant Crafting_Sort_Orders := NONE;
   -- ****

   -- ****iv* SUI/SUI.Crafting_Sort_Order
   -- FUNCTION
   -- The current sorting order for the list of finished crafting orders
   -- HISTORY
   -- 6.5 - Added
   -- SOURCE
   Crafting_Sort_Order: Crafting_Sort_Orders := Default_Crafting_Sort_Order;
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
      type Local_Crafting_Data is record
         Name: Unbounded_String;
         Amount: Positive;
         Id: Positive;
      end record;
      type Crafting_Array is array(Positive range <>) of Local_Crafting_Data;
      Local_Crafting: Crafting_Array
        (1 .. Positive(GameStats.CraftingOrders.Length));
      function "<"(Left, Right: Local_Crafting_Data) return Boolean is
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
        (Index_Type => Positive, Element_Type => Local_Crafting_Data,
         Array_Type => Crafting_Array);
   begin
      case Column is
         when 1 =>
            if Crafting_Sort_Order = NAMEASC then
               Crafting_Sort_Order := NAMEDESC;
            else
               Crafting_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Crafting_Sort_Order = AMOUNTASC then
               Crafting_Sort_Order := AMOUNTDESC;
            else
               Crafting_Sort_Order := AMOUNTASC;
            end if;
         when others =>
            null;
      end case;
      if Crafting_Sort_Order = NONE then
         return TCL_OK;
      end if;
      for I in GameStats.CraftingOrders.Iterate loop
         Local_Crafting(Statistics_Container.To_Index(I)) :=
           (Name =>
              Items_List
                (Recipes_List(GameStats.CraftingOrders(I).Index).ResultIndex)
                .Name,
            Amount => GameStats.CraftingOrders(I).Amount,
            Id => Statistics_Container.To_Index(I));
      end loop;
      Sort_Crafting(Local_Crafting);
      Crafting_Indexes.Clear;
      for Order of Local_Crafting loop
         Crafting_Indexes.Append(Order.Id);
      end loop;
      ShowStatistics(True);
      return TCL_OK;
   end Sort_Crafting_Command;

   procedure AddCommands is
   begin
      AddCommand("SortFinishedCrafting", Sort_Crafting_Command'Access);
   end AddCommands;

end Statistics.UI;
