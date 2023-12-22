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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations;
with GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with CoreUI; use CoreUI;
with Crew; use Crew;
with Dialogs; use Dialogs;
with Events;
with Factions;
with Items;
with Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with ShipModules;
with Ships.Crew; use Ships.Crew;
with Utils.UI; use Utils.UI;

package body Combat.UI is

   -- ****if* CUI/CUI.Update_Messages
   -- FUNCTION
   -- Update in-game messages in combat
   -- SOURCE
   procedure Update_Messages with
      Import => True,
      Convention => C,
      External_Name => "updateCombatAdaMessages";
      -- ****

   -- ****if* CUI/CUI.Update_Combat_Ui
   -- FUNCTION
   -- Update information about combat: remove old UI and create new elements
   -- SOURCE
   procedure Update_Combat_Ui with
      Import => True,
      Convention => C,
      External_Name => "updateAdaCombatUi";
      -- ****

   -- ****if* CUI/CUI.ShowCombatFrame
   -- FUNCTION
   -- Show ship to ship combat UI or boarding UI
   -- PARAMETERS
   -- Frame_Name - The parent frame, ship combat or boarding
   -- SOURCE
   procedure Show_Combat_Frame(Frame_Name: String) is
      -- ****
--      Combat_Frame: constant Ttk_Frame :=
--        Get_Widget(pathName => ".gameframe.paned.combatframe");
--      Child_Frame: Ttk_Frame :=
--        Get_Widget
--          (pathName =>
--             Tcl.Tk.Ada.Grid.Grid_Slaves
--               (Master => Combat_Frame, Option => "-row 0 -column 0"));
--      Combat_Children: constant array(1 .. 5) of Unbounded_String :=
--        (1 => To_Unbounded_String(Source => ".crew"),
--         2 => To_Unbounded_String(Source => ".damage"),
--         3 => To_Unbounded_String(Source => ".enemy"),
--         4 => To_Unbounded_String(Source => ".status"),
--         5 => To_Unbounded_String(Source => ".next"));
--      Boarding_Children: constant array(1 .. 3) of Unbounded_String :=
--        (1 => To_Unbounded_String(Source => ".left"),
--         2 => To_Unbounded_String(Source => ".right"),
--         3 => To_Unbounded_String(Source => ".next"));
      procedure Show_Ada_Combat_Frame(F_Name: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "showAdaCombatFrame";
   begin
      Show_Ada_Combat_Frame(F_Name => New_String(Str => Frame_Name));
--      if Frame_Name = ".combat" then
--         if Widget_Image(Win => Child_Frame) =
--           Combat_Frame & To_String(Source => Combat_Children(1)) then
--            return;
--         end if;
--         Hide_Boarding_UI_Loop :
--         for BoardingChild of Boarding_Children loop
--            Child_Frame :=
--              Get_Widget
--                (pathName =>
--                   Combat_Frame & To_String(Source => BoardingChild));
--            Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Child_Frame);
--         end loop Hide_Boarding_UI_Loop;
--         Show_Combat_UI_Loop :
--         for CombatChild of Combat_Children loop
--            Child_Frame :=
--              Get_Widget
--                (pathName => Combat_Frame & To_String(Source => CombatChild));
--            Tcl.Tk.Ada.Grid.Grid(Slave => Child_Frame);
--         end loop Show_Combat_UI_Loop;
--      else
--         if Widget_Image(Win => Child_Frame) =
--           Combat_Frame & To_String(Source => Boarding_Children(1)) then
--            return;
--         end if;
--         Hide_Combat_UI_Loop :
--         for CombatChild of Combat_Children loop
--            Child_Frame :=
--              Get_Widget
--                (pathName => Combat_Frame & To_String(Source => CombatChild));
--            Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Child_Frame);
--         end loop Hide_Combat_UI_Loop;
--         Show_Boarding_UI_Loop :
--         for BoardingChild of Boarding_Children loop
--            Child_Frame :=
--              Get_Widget
--                (pathName =>
--                   Combat_Frame & To_String(Source => BoardingChild));
--            Tcl.Tk.Ada.Grid.Grid(Slave => Child_Frame);
--         end loop Show_Boarding_UI_Loop;
--      end if;
   end Show_Combat_Frame;

   -- ****if* CUI/CUI.UpdateBoardingUI
   -- FUNCTION
   -- Update information about boarding party: remove old UI and create new elements
   -- SOURCE
   procedure Update_Boarding_Ui is
      -- ****
      use GNAT.String_Split;
      use Tcl.Tk.Ada.Widgets.TtkLabel;
      use Tcl.Tk.Ada.Widgets.TtkProgressBar;
      use Ada.Characters.Handling;
      use Tiny_String;

      Orders_List, Order_Name: Unbounded_String := Null_Unbounded_String;
      Frame_Name: constant String := Main_Paned & ".combatframe";
      Frame: Ttk_Frame :=
        Get_Widget(pathName => Frame_Name & ".right.canvas.frame");
      Tokens: Slice_Set;
      Rows: Natural;
      Order_Index: Positive := 1;
      --## rule off IMPROPER_INITIALIZATION
      Progress_Bar: Ttk_ProgressBar;
      Label: Ttk_Label;
      Combo_Box: Ttk_ComboBox;
      Combat_Canvas: Tk_Canvas;
      Button: Ttk_Button;
      --## rule on IMPROPER_INITIALIZATION
   begin
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & Get_General_Accelerator(Index => 1) & ">",
         Script => "{InvokeButton " & Frame & ".maxmin}");
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence => "<" & Get_General_Accelerator(Index => 2) & ">",
         Script =>
           "{InvokeButton " & Frame_Name & ".left.canvas.frame.maxmin}");
      Create
        (S => Tokens, From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets(Start_Index => 1, End_Index => Rows - 1, Frame => Frame);
      Show_Enemy_Crew_Loop :
      for I in Enemy.Ship.Crew.Iterate loop
         Append
           (Source => Orders_List,
            New_Item =>
              "{Attack " & To_String(Source => Enemy.Ship.Crew(I).Name) &
              "} ");
         Button :=
           Create
             (pathName =>
                Frame & ".name" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-text {" & To_String(Source => Enemy.Ship.Crew(I).Name) &
                "} -command {ShowCombatInfo enemy" &
                Positive'Image(Crew_Container.To_Index(Position => I)) & "}");
         Add
           (Widget => Button,
            Message => "Show more information about the enemy's crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-row" & Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx {5 0}");
         Progress_Bar :=
           Create
             (pathName =>
                Frame & ".health" &
                Trim
                  (Source =>
                     Natural'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-orient horizontal -value " &
                Natural'Image(Enemy.Ship.Crew(I).Health) & " -length 150" &
                (if Enemy.Ship.Crew(I).Health > 74 then
                   " -style green.Horizontal.TProgressbar"
                 elsif Enemy.Ship.Crew(I).Health > 24 then
                   " -style yellow.Horizontal.TProgressbar"
                 else " -style Horizontal.TProgressbar"));
         Add(Widget => Progress_Bar, Message => "Enemy's health");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Progress_Bar,
            Options =>
              "-column 1 -row" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx 5");
         Tcl_Eval
           (interp => Get_Context,
            strng =>
              "SetScrollbarBindings " & Progress_Bar &
              " $combatframe.right.scrolly");
         Order_Name :=
           To_Unbounded_String
             (Source => Crew_Orders'Image(Enemy.Ship.Crew(I).Order));
         Replace_Slice
           (Source => Order_Name, Low => 2,
            High => Length(Source => Order_Name),
            By =>
              To_Lower
                (Item =>
                   Slice
                     (Source => Order_Name, Low => 2,
                      High => Length(Source => Order_Name))));
         Label :=
           Create
             (pathName =>
                Frame & ".order" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options => "-text {" & To_String(Source => Order_Name) & "}");
         Add(Widget => Label, Message => "Enemy's current order.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Label,
            Options =>
              "-column 2 -row" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx {0 5}");
         Tcl_Eval
           (interp => Get_Context,
            strng =>
              "SetScrollbarBindings " & Label & " $combatframe.right.scrolly");
      end loop Show_Enemy_Crew_Loop;
      Tcl_Eval(interp => Get_Context, strng => "update");
      Combat_Canvas := Get_Widget(pathName => Frame_Name & ".right.canvas");
      configure
        (Widgt => Combat_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Combat_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Append(Source => Orders_List, New_Item => " {Back to the ship}");
      Frame.Name := New_String(Str => Frame_Name & ".left.canvas.frame");
      Create
        (S => Tokens, From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets(Start_Index => 1, End_Index => Rows - 1, Frame => Frame);
      Show_Boarding_Party_Loop :
      for I in Player_Ship.Crew.Iterate loop
         if Player_Ship.Crew(I).Order /= BOARDING then
            goto End_Of_Loop;
         end if;
         Button :=
           Create
             (pathName =>
                Frame & ".name" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-text {" & To_String(Source => Player_Ship.Crew(I).Name) &
                "} -command {ShowCombatInfo player" &
                Positive'Image(Crew_Container.To_Index(Position => I)) & "}");
         Add
           (Widget => Button,
            Message => "Show more information about the crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-row" & Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx {5 0}");
         Progress_Bar :=
           Create
             (pathName =>
                Frame & ".health" &
                Trim
                  (Source =>
                     Natural'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-orient horizontal -value " &
                Natural'Image(Player_Ship.Crew(I).Health) & " -length 150" &
                (if Player_Ship.Crew(I).Health > 74 then
                   " -style green.Horizontal.TProgressbar"
                 elsif Player_Ship.Crew(I).Health > 24 then
                   " -style yellow.Horizontal.TProgressbar"
                 else " -style Horizontal.TProgressbar"));
         Add(Widget => Progress_Bar, Message => "The crew member health.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Progress_Bar,
            Options =>
              "-column 1 -row" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx 5");
         Tcl_Eval
           (interp => Get_Context,
            strng =>
              "SetScrollbarBindings " & Progress_Bar &
              " $combatframe.left.scrolly");
         Combo_Box :=
           Create
             (pathName =>
                Frame & ".order" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-values [list " & To_String(Source => Orders_List) &
                "] -state readonly -width 15");
         Current
           (ComboBox => Combo_Box,
            NewIndex => Natural'Image(Boarding_Orders(Order_Index)));
         Bind
           (Widgt => Combo_Box, Sequence => "<<ComboboxSelected>>",
            Script =>
              "{SetBoardingOrder" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              Positive'Image(Order_Index) & "}");
         Add(Widget => Combo_Box, Message => "The crew member current order.");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Combo_Box,
            Options =>
              "-column 2 -row" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              " -padx {0 5}");
         Order_Index := Order_Index + 1;
         <<End_Of_Loop>>
      end loop Show_Boarding_Party_Loop;
      Tcl_Eval(interp => Get_Context, strng => "update");
      Combat_Canvas := Get_Widget(pathName => Frame_Name & ".left.canvas");
      configure
        (Widgt => Combat_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Combat_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Combat_Canvas, Fraction => "0.0");
      Update_Messages;
   end Update_Boarding_Ui;

   -- ****if* CUI/CUI.Next_Turn_Command
   -- FUNCTION
   -- Execute combat orders and go to next turn
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- NextTurn
   -- SOURCE
   function Next_Turn_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Next_Turn_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      Combat_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".combatframe", Interp => Interp);
      Frame: Ttk_Frame :=
        Get_Widget(pathName => Combat_Frame & ".crew", Interp => Interp);
      Next_Button: constant Ttk_Button :=
        Get_Widget(pathName => Combat_Frame & ".next", Interp => Interp);
   begin
      Combat_Turn;
      Update_Header;
      if Get_End_Combat then
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence => "<" & Get_General_Accelerator(Index => 1) & ">");
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence => "<" & Get_General_Accelerator(Index => 2) & ">");
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence => "<" & Get_General_Accelerator(Index => 3) & ">");
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence => "<" & Get_General_Accelerator(Index => 4) & ">");
         Update_Combat_Ui;
         configure(Widgt => Close_Button, options => "-command {ShowSkyMap}");
         Tcl_SetVar
           (interp => Interp, varName => "gamestate", newValue => "general");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Close_Button, Options => "-row 0 -column 1");
         Frame.Name :=
           New_String(Str => Widget_Image(Win => Combat_Frame) & ".left");
         if Winfo_Get(Widgt => Frame, Info => "ismapped") = "1" then
            Show_Combat_Frame(Frame_Name => ".combat");
         end if;
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Next_Button);
         return TCL_OK;
      end if;
      if Player_Ship.Crew(1).Order = BOARDING and
        Winfo_Get(Widgt => Frame, Info => "ismapped") = "1" then
         Update_Boarding_Ui;
         Show_Combat_Frame(Frame_Name => ".boarding");
         return TCL_OK;
      end if;
      if Player_Ship.Crew(1).Order /= BOARDING and
        Winfo_Get(Widgt => Frame, Info => "ismapped") = "0" then
         Update_Combat_Ui;
         Show_Combat_Frame(Frame_Name => ".combat");
         return TCL_OK;
      end if;
      if Winfo_Get(Widgt => Frame, Info => "ismapped") = "1" then
         Update_Combat_Ui;
      else
         Update_Boarding_Ui;
      end if;
      return TCL_OK;
   end Next_Turn_Command;

   -- ****if* CUI/CUI.Show_Combat_UI_Command
   -- FUNCTION
   -- Show combat UI
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowCombatUI
   -- SOURCE
   function Show_Combat_Ui_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Combat_Ui_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc, Argv);
   begin
      Show_Combat_Ui(New_Combat => False);
      return TCL_OK;
   end Show_Combat_Ui_Command;

   -- ****if* CUI/CUI.Set_Combat_Order_Command
   -- FUNCTION
   -- Set combat order for the selected crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCombatOrder Position
   -- Position argument can be pilot, engineer or number of the gun which
   -- gunner will take a new combat order
   -- SOURCE
   function Set_Combat_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Combat_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Factions;
      use ShipModules;
      use Tiny_String;

      --## rule off IMPROPER_INITIALIZATION
      Combo_Box: Ttk_ComboBox;
      --## rule on IMPROPER_INITIALIZATION
      Gun_Index: Positive := 1;
      Frame_Name: constant String :=
        Main_Paned & ".combatframe.crew.canvas.frame";
      Faction: constant Faction_Record :=
        Get_Faction(Index => Player_Ship.Crew(1).Faction);
   begin
      Combo_Box.Interp := Interp;
      if CArgv.Arg(Argv => Argv, N => 1) = "pilot" then
         Combo_Box.Name := New_String(Str => Frame_Name & ".pilotorder");
         --## rule off IMPROPER_INITIALIZATION
         Set_Pilot_Order
           (New_Order => Positive'Value(Current(ComboBox => Combo_Box)) + 1);
         --## rule on IMPROPER_INITIALIZATION
         if Faction.Flags.Contains
             (Item => To_Unbounded_String(Source => "sentientships")) then
            Add_Message
              (Message =>
                 "Order for ship was set on: " & Get(Widgt => Combo_Box),
               M_Type => COMBATMESSAGE);
         else
            Add_Message
              (Message =>
                 "Order for " &
                 To_String
                   (Source =>
                      Player_Ship.Crew(Find_Member(Order => PILOT)).Name) &
                 " was set on: " & Get(Widgt => Combo_Box),
               M_Type => COMBATMESSAGE);
         end if;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "engineer" then
         Combo_Box.Name := New_String(Str => Frame_Name & ".engineerorder");
         --## rule off IMPROPER_INITIALIZATION
         Set_Engineer_Order
           (New_Order => Positive'Value(Current(ComboBox => Combo_Box)) + 1);
         --## rule on IMPROPER_INITIALIZATION
         if Faction.Flags.Contains
             (Item => To_Unbounded_String(Source => "sentientships")) then
            Add_Message
              (Message =>
                 "Order for ship was set on: " & Get(Widgt => Combo_Box),
               M_Type => COMBATMESSAGE);
         else
            Add_Message
              (Message =>
                 "Order for " &
                 To_String
                   (Source =>
                      Player_Ship.Crew(Find_Member(Order => ENGINEER)).Name) &
                 " was set on: " & Get(Widgt => Combo_Box),
               M_Type => COMBATMESSAGE);
         end if;
      else
         Combo_Box.Name :=
           New_String
             (Str =>
                Frame_Name & ".gunorder" & CArgv.Arg(Argv => Argv, N => 1));
         Gun_Index := Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
         --## rule off IMPROPER_INITIALIZATION
         Guns(Gun_Index)(2) :=
           Positive'Value(Current(ComboBox => Combo_Box)) + 1;
         Guns(Gun_Index)(3) :=
           (if Current(ComboBox => Combo_Box) = "0" then 0
            else Get_Module
                (Index => Player_Ship.Modules(Guns(Gun_Index)(1)).Proto_Index)
                .Speed);
         Add_Message
           (Message =>
              "Order for " &
              To_String
                (Source =>
                   Player_Ship.Crew
                     (Player_Ship.Modules(Guns(Gun_Index)(1)).Owner(1))
                     .Name) &
              " was set on: " & Get(Widgt => Combo_Box),
            M_Type => COMBATMESSAGE);
         --## rule on IMPROPER_INITIALIZATION
      end if;
      Update_Messages;
      return TCL_OK;
   end Set_Combat_Order_Command;

   -- ****o* CUI/CUI.Set_Boarding_Order_Command
   -- FUNCTION
   -- Set boarding order for the selected player's ship crew member
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetBoardingOrder EnemyIndex
   -- EnemyIndex parameter is the index of the enemy in the enemy ship crew
   -- which will be set as target for the selected player ship crew member.
   -- SOURCE
   function Set_Boarding_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Boarding_Order_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Combo_Box: constant Ttk_ComboBox :=
        Get_Widget
          (pathName =>
             Main_Paned & ".combatframe.left.canvas.frame.order" &
             CArgv.Arg(Argv => Argv, N => 1),
           Interp => Interp);
   begin
      Boarding_Orders(Positive'Value(CArgv.Arg(Argv => Argv, N => 2))) :=
        (if
           Natural'Value(Current(ComboBox => Combo_Box)) + 1 >
           Natural(Enemy.Ship.Crew.Length)
         then -1
         else Natural'Value(Current(ComboBox => Combo_Box)) + 1);
      return TCL_OK;
   end Set_Boarding_Order_Command;

   -- ****o* CUI/CUI.Set_Combat_Party_Command
   -- FUNCTION
   -- Set combat party (boarding or defenders)
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCombatParty partytype
   -- Partytype is a type of party to set. Possible options are boarding or
   -- defenders
   -- SOURCE
   function Set_Combat_Party_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Combat_Party_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      use Tcl.Tk.Ada.Widgets.TtkButton.TtkCheckButton;
      use Tcl.Tk.Ada.Widgets.TtkScrollbar;
      use Tcl.Tklib.Ada.Autoscroll;
      use Tiny_String;

      Crew_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".boardingdialog",
           Title =>
             "Assign a crew members to " &
             (if CArgv.Arg(Argv => Argv, N => 1) = "boarding" then
                "boarding party"
              else "defenders"),
           Title_Width => 245);
      Y_Scroll: constant Ttk_Scrollbar :=
        Create
          (pathName => Crew_Dialog & ".yscroll",
           options =>
             "-orient vertical -command [list " & Crew_Dialog &
             ".canvas yview]");
      Crew_Canvas: constant Tk_Canvas :=
        Create
          (pathName => Crew_Dialog & ".canvas",
           options => "-yscrollcommand [list " & Y_Scroll & " set]");
      Crew_Frame: constant Ttk_Frame :=
        Create(pathName => Crew_Canvas & ".frame");
      Buttons_Box2: constant Ttk_Frame :=
        Create(pathName => Crew_Dialog & ".buttons");
      Close_Dialog_Button: constant Ttk_Button :=
        Create
          (pathName => Crew_Dialog & ".buttons.button",
           options =>
             "-text Cancel -command {CloseDialog " & Crew_Dialog &
             "} -image cancelicon -style Dialog.TButton");
      Accept_Button: constant Ttk_Button :=
        Create
          (pathName => Crew_Dialog & ".buttons.button2",
           options =>
             "-text Assign -command {SetParty " &
             CArgv.Arg(Argv => Argv, N => 1) & "; CloseDialog " & Crew_Dialog &
             "} -image giveordericon -style Dialog.TButton");
      Buttons_Frame: constant Ttk_Frame :=
        Create(pathName => Crew_Dialog & ".selectframe");
      Height: Positive := 10;
      Width: Positive := 250;
      --## rule off IMPROPER_INITIALIZATION
      Crew_Button: Ttk_CheckButton;
      Button: Ttk_Button;
      --## rule on IMPROPER_INITIALIZATION
      Order: constant Crew_Orders :=
        (if CArgv.Arg(Argv => Argv, N => 1) = "boarding" then BOARDING
         else DEFEND);
   begin
      Button :=
        Create
          (pathName => Buttons_Frame & ".selectallbutton",
           options =>
             "-image selectallicon -command {ToggleAllCombat select " &
             CArgv.Arg(Argv => Argv, N => 1) & "} -style Small.TButton");
      Add(Widget => Button, Message => "Select all crew members.");
      Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-padx {5 2}");
      Button :=
        Create
          (pathName => Buttons_Frame & ".unselectallbutton",
           options =>
             "-image unselectallicon -command {ToggleAllCombat unselect " &
             CArgv.Arg(Argv => Argv, N => 1) & "} -style Small.TButton");
      Add(Widget => Button, Message => "Unselect all crew members.");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Button, Options => "-sticky w -row 0 -column 1");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Buttons_Frame, Options => "-columnspan 2 -sticky w");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Crew_Canvas, Options => "-sticky nwes -padx 5 -pady 5");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Y_Scroll,
         Options => "-sticky ns -padx {0 5} -pady {5 0} -row 2 -column 1");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Buttons_Box2, Options => "-pady {0 5} -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid(Slave => Accept_Button, Options => "-padx {5 2}");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Dialog_Button,
         Options => "-sticky w -row 0 -column 1");
      Focus(Widgt => Close_Dialog_Button);
      Autoscroll(Scroll => Y_Scroll);
      Show_Player_Ship_Crew_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Crew_Button :=
           Create
             (pathName =>
                Crew_Frame & ".crewbutton" &
                Trim
                  (Source =>
                     Positive'Image(Crew_Container.To_Index(Position => I)),
                   Side => Left),
              options =>
                "-text {" & To_String(Source => Player_Ship.Crew(I).Name) &
                "}");
         if Player_Ship.Crew(I).Order = Order then
            Tcl_SetVar
              (interp => Interp, varName => Widget_Image(Win => Crew_Button),
               newValue => "1");
         else
            Tcl_SetVar
              (interp => Interp, varName => Widget_Image(Win => Crew_Button),
               newValue => "0");
         end if;
         Tcl.Tk.Ada.Pack.Pack(Slave => Crew_Button, Options => "-anchor w");
         Height :=
           Height +
           Positive'Value
             (Winfo_Get(Widgt => Crew_Button, Info => "reqheight"));
         if Positive'Value
             (Winfo_Get(Widgt => Crew_Button, Info => "reqwidth")) +
           10 >
           Width then
            Width :=
              Positive'Value
                (Winfo_Get(Widgt => Crew_Button, Info => "reqwidth")) +
              10;
         end if;
         Bind
           (Widgt => Crew_Button, Sequence => "<Escape>",
            Script => "{" & Close_Dialog_Button & " invoke;break}");
         Bind
           (Widgt => Crew_Button, Sequence => "<Tab>",
            Script =>
              "{focus [GetActiveButton" &
              Positive'Image(Crew_Container.To_Index(Position => I)) &
              "];break}");
      end loop Show_Player_Ship_Crew_Loop;
      if Height > 500 then
         Height := 500;
      end if;
      Canvas_Create
        (Parent => Crew_Canvas, Child_Type => "window",
         Options =>
           "0 0 -anchor nw -window " & Widget_Image(Win => Crew_Frame));
      Tcl_Eval(interp => Interp, strng => "update");
      configure
        (Widgt => Crew_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Crew_Canvas, TagOrId => "all") & "] -height" &
           Positive'Image(Height) & " -width" & Positive'Image(Width));
      Bind
        (Widgt => Close_Dialog_Button, Sequence => "<Escape>",
         Script => "{" & Close_Dialog_Button & " invoke;break}");
      Bind
        (Widgt => Close_Dialog_Button, Sequence => "<Tab>",
         Script => "{focus [GetActiveButton 0];break}");
      Show_Dialog(Dialog => Crew_Dialog, Relative_Y => 0.2);
      return TCL_OK;
   end Set_Combat_Party_Command;

   -- ****if* CUI/CUI.Set_Combat_Position_Command
   -- FUNCTION
   -- Set crew member position (pilot, engineer, gunner) in combat
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCombatPosition position
   -- Position is the combat crew member position which will be set
   -- SOURCE
   function Set_Combat_Position_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Combat_Position_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      --## rule off IMPROPER_INITIALIZATION
      Combo_Box: Ttk_ComboBox;
      --## rule on IMPROPER_INITIALIZATION
      Gun_Index: Positive := 1;
      Crew_Index: Natural;
      Frame_Name: constant String :=
        ".gameframe.paned.combatframe.crew.canvas.frame";
   begin
      Combo_Box.Interp := Interp;
      if CArgv.Arg(Argv => Argv, N => 1) = "pilot" then
         Combo_Box.Name := New_String(Str => Frame_Name & ".pilotcrew");
         --## rule off IMPROPER_INITIALIZATION
         Crew_Index := Natural'Value(Current(ComboBox => Combo_Box));
         --## rule on IMPROPER_INITIALIZATION
         if Crew_Index > 0 then
            Give_Orders
              (Ship => Player_Ship, Member_Index => Crew_Index,
               Given_Order => PILOT);
         else
            Crew_Index := Find_Member(Order => PILOT);
            if Crew_Index > 0 then
               Give_Orders
                 (Ship => Player_Ship, Member_Index => Crew_Index,
                  Given_Order => REST);
            end if;
         end if;
      elsif CArgv.Arg(Argv => Argv, N => 1) = "engineer" then
         Combo_Box.Name := New_String(Str => Frame_Name & ".engineercrew");
         --## rule off IMPROPER_INITIALIZATION
         Crew_Index := Natural'Value(Current(ComboBox => Combo_Box));
         --## rule on IMPROPER_INITIALIZATION
         if Crew_Index > 0 then
            Give_Orders
              (Ship => Player_Ship, Member_Index => Crew_Index,
               Given_Order => ENGINEER);
         else
            Crew_Index := Find_Member(Order => ENGINEER);
            if Crew_Index > 0 then
               Give_Orders
                 (Ship => Player_Ship, Member_Index => Crew_Index,
                  Given_Order => REST);
            end if;
         end if;
      else
         Combo_Box.Name :=
           New_String
             (Str =>
                Frame_Name & ".guncrew" & CArgv.Arg(Argv => Argv, N => 2));
         Gun_Index := Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
         --## rule off IMPROPER_INITIALIZATION
         Crew_Index := Natural'Value(Current(ComboBox => Combo_Box));
         --## rule on IMPROPER_INITIALIZATION
         if Crew_Index > 0 then
            Give_Orders
              (Ship => Player_Ship, Member_Index => Crew_Index,
               Given_Order => GUNNER, Module_Index => Guns(Gun_Index)(1));
         else
            Crew_Index := Player_Ship.Modules(Guns(Gun_Index)(1)).Owner(1);
            if Crew_Index > 0 then
               Give_Orders
                 (Ship => Player_Ship, Member_Index => Crew_Index,
                  Given_Order => REST);
            end if;
         end if;
      end if;
      Update_Combat_Ui;
      return TCL_OK;
   end Set_Combat_Position_Command;

   -- ****if* CUI/CUI.Show_Combat_Info_Command
   -- FUNCTION
   -- Show information about the selected mob in combat
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetCombatPosition player|enemy index
   -- Position is the combat crew member position which will be set
   -- SOURCE
   function Show_Combat_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Combat_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Ada.Characters.Latin_1;
      use Items;

      Crew_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 2));
      Info: Unbounded_String;
   begin
      Info := To_Unbounded_String(Source => "Uses: ");
      if CArgv.Arg(Argv => Argv, N => 1) = "player" then
         Show_Player_Crew_Equipment_Loop :
         for Item of Player_Ship.Crew(Crew_Index).Equipment loop
            if Item /= 0 then
               Append
                 (Source => Info,
                  New_Item =>
                    LF &
                    Get_Item_Name
                      (Item =>
                         Inventory_Container.Element
                           (Container =>
                              Player_Ship.Crew(Crew_Index).Inventory,
                            Index => Item)));
            end if;
         end loop Show_Player_Crew_Equipment_Loop;
      else
         Show_Enemy_Crew_Equipment_Loop :
         for Item of Enemy.Ship.Crew(Crew_Index).Equipment loop
            if Item /= 0 then
               Append
                 (Source => Info,
                  New_Item =>
                    LF &
                    Get_Item_Name
                      (Item =>
                         Inventory_Container.Element
                           (Container => Enemy.Ship.Crew(Crew_Index).Inventory,
                            Index => Item)));
            end if;
         end loop Show_Enemy_Crew_Equipment_Loop;
      end if;
      Show_Info(Text => To_String(Source => Info), Title => "More info");
      return TCL_OK;
   end Show_Combat_Info_Command;

   -- ****o* CUI/CUI.Combat_Max_Min_Command
   -- FUNCTION
   -- Maximize or minimize the selected section of the combat UI
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CombatMaxMin framename
   -- Framename is name of the frame to maximize or minimize
   -- SOURCE
   function Combat_Max_Min_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Combat_Max_Min_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      --## rule off TYPE_INITIAL_VALUES
      type Frame_Info is record
         Name: Unbounded_String;
         Column: Natural range 0 .. 1;
         Row: Natural range 0 .. 1;
      end record;
      --## rule on TYPE_INITIAL_VALUES
      type Frames_Array is array(Positive range <>) of Frame_Info;
      Combat_Frames: constant Frames_Array(1 .. 4) :=
        (1 =>
           (Name => To_Unbounded_String(Source => "crew"), Column => 0,
            Row => 0),
         2 =>
           (Name => To_Unbounded_String(Source => "damage"), Column => 0,
            Row => 1),
         3 =>
           (Name => To_Unbounded_String(Source => "enemy"), Column => 1,
            Row => 0),
         4 =>
           (Name => To_Unbounded_String(Source => "status"), Column => 1,
            Row => 1));
      Boarding_Frames: constant Frames_Array(1 .. 2) :=
        (1 =>
           (Name => To_Unbounded_String(Source => "left"), Column => 0,
            Row => 0),
         2 =>
           (Name => To_Unbounded_String(Source => "right"), Column => 1,
            Row => 0));
      Frame: Ttk_Frame :=
        Get_Widget
          (pathName => Main_Paned & ".combatframe.crew", Interp => Interp);
      Button: constant Ttk_Button :=
        Get_Widget
          (pathName =>
             Main_Paned & ".combatframe." & CArgv.Arg(Argv => Argv, N => 1) &
             ".canvas.frame.maxmin",
           Interp => Interp);
      Frames: constant Frames_Array :=
        (if CArgv.Arg(Argv => Argv, N => 3) = "combat" then Combat_Frames
         else Boarding_Frames);
   begin
      if CArgv.Arg(Argv => Argv, N => 2) = "show" then
         Hide_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Str =>
                   Main_Paned & ".combatframe." &
                   To_String(Source => FrameInfo.Name));
            if To_String(Source => FrameInfo.Name) =
              CArgv.Arg(Argv => Argv, N => 1) then
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Slave => Frame,
                  Options => "-columnspan 2 -rowspan 2 -row 0 -column 0");
            else
               Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Frame);
            end if;
         end loop Hide_Frames_Loop;
         configure
           (Widgt => Button,
            options =>
              "-image movemapdownicon -command {CombatMaxMin " &
              CArgv.Arg(Argv => Argv, N => 1) & " hide " &
              CArgv.Arg(Argv => Argv, N => 3) & "}");
      else
         Show_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Str =>
                   Main_Paned & ".combatframe." &
                   To_String(Source => FrameInfo.Name));
            if To_String(Source => FrameInfo.Name) =
              CArgv.Arg(Argv => Argv, N => 1) then
               Tcl.Tk.Ada.Grid.Grid(Slave => Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Slave => Frame,
                  Options =>
                    "-columnspan 1 -rowspan 1 -column" &
                    Natural'Image(FrameInfo.Column) & " -row" &
                    Natural'Image(FrameInfo.Row));
            end if;
         end loop Show_Frames_Loop;
         configure
           (Widgt => Button,
            options =>
              "-image movemapupicon -command {CombatMaxMin " &
              CArgv.Arg(Argv => Argv, N => 1) & " show " &
              CArgv.Arg(Argv => Argv, N => 3) & "}");
      end if;
      return TCL_OK;
   end Combat_Max_Min_Command;

   -- ****o* CUI/CUI.Toggle_All_Command
   -- FUNCTION
   -- Select or deselect all crew members in boarding and defending party
   -- setting
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ToggleAllCombat action order
   -- Action is the action which will be performed. Possible values are
   -- select or deselect. Order is the order to give to the player's ship
   -- crew. Possible values are boarding and defending.
   -- SOURCE
   function Toggle_All_Combat_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Toggle_All_Combat_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
   begin
      Boarding_Orders.Clear;
      Set_Crew_Selection_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Tcl_SetVar
           (interp => Interp,
            varName =>
              ".boardingdialog.canvas.frame.crewbutton" &
              Trim
                (Source => Crew_Container.To_Index(Position => I)'Image,
                 Side => Left),
            newValue =>
              (if CArgv.Arg(Argv => Argv, N => 1) = "select" then "1"
               else "0"));
      end loop Set_Crew_Selection_Loop;
      return TCL_OK;
   end Toggle_All_Combat_Command;

   -- ****o* CUI/CUI.Set_Party_Command
   -- FUNCTION
   -- Set crew members in or out of boarding and defending party
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetParty order
   -- Order is the order to give to the player's ship crew. Possible
   -- values are boarding and defending.
   -- SOURCE
   function Set_Party_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Party_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc);
      Order: constant Crew_Orders :=
        (if CArgv.Arg(Argv => Argv, N => 1) = "boarding" then BOARDING
         else DEFEND);
      Selected: Boolean := False;
   begin
      Boarding_Orders.Clear;
      Set_Crew_Selection_Loop :
      for I in Player_Ship.Crew.Iterate loop
         Selected :=
           Tcl_GetVar
             (interp => Interp,
              varName =>
                ".boardingdialog.canvas.frame.crewbutton" &
                Trim
                  (Source => Crew_Container.To_Index(Position => I)'Image,
                   Side => Left)) =
           "1";
         if Player_Ship.Crew(I).Order = Order and then not Selected then
            Give_Orders
              (Ship => Player_Ship,
               Member_Index => Crew_Container.To_Index(Position => I),
               Given_Order => REST);
         elsif Selected and Player_Ship.Crew(I).Order /= Order then
            Give_Orders
              (Ship => Player_Ship,
               Member_Index => Crew_Container.To_Index(Position => I),
               Given_Order => Order, Module_Index => 0);
            if Order = BOARDING then
               Boarding_Orders.Append(New_Item => 0);
            end if;
         end if;
      end loop Set_Crew_Selection_Loop;
      Update_Combat_Ui;
      return TCL_OK;
   end Set_Party_Command;

   procedure Show_Combat_Ui(New_Combat: Boolean := True) is
      use GNAT.Directory_Operations;
      use Events;
      use Maps;
      use Tiny_String;

      Combat_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".combatframe");
      Combat_Started: Boolean := False;
      Button: constant Ttk_Button :=
        Get_Widget(pathName => Combat_Frame & ".next");
      Enemy_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Combat_Frame & ".status");
   begin
      Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
      if New_Combat then
         if Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index > 0
           and then Get_Enemy_Name /=
             Get_Proto_Ship
               (Proto_Index =>
                  Get_Event
                    (Index =>
                       Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                         .Event_Index)
                    .Ship_Index)
               .Name then
            Combat_Started :=
              Start_Combat
                (Enemy_Index =>
                   Get_Event
                     (Index =>
                        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y)
                          .Event_Index)
                     .Ship_Index,
                 New_Combat => False);
            if not Combat_Started then
               return;
            end if;
         end if;
         if Winfo_Get(Widgt => Combat_Frame, Info => "exists") = "0" then
            Tcl_EvalFile
              (interp => Get_Context,
               fileName =>
                 To_String(Source => Data_Directory) & "ui" & Dir_Separator &
                 "combat.tcl");
            Set_Pilot_Order(New_Order => 2);
            Set_Engineer_Order(New_Order => 3);
            Add_Command
              (Name => "NextTurn", Ada_Command => Next_Turn_Command'Access);
            Add_Command
              (Name => "ShowCombatUI",
               Ada_Command => Show_Combat_Ui_Command'Access);
            Add_Command
              (Name => "SetCombatOrder",
               Ada_Command => Set_Combat_Order_Command'Access);
            Add_Command
              (Name => "SetBoardingOrder",
               Ada_Command => Set_Boarding_Order_Command'Access);
            Add_Command
              (Name => "SetCombatParty",
               Ada_Command => Set_Combat_Party_Command'Access);
            Add_Command
              (Name => "SetCombatPosition",
               Ada_Command => Set_Combat_Position_Command'Access);
            Add_Command
              (Name => "ShowCombatInfo",
               Ada_Command => Show_Combat_Info_Command'Access);
            Add_Command
              (Name => "CombatMaxMin",
               Ada_Command => Combat_Max_Min_Command'Access);
            Add_Command
              (Name => "ToggleAllCombat",
               Ada_Command => Toggle_All_Combat_Command'Access);
            Add_Command
              (Name => "SetParty", Ada_Command => Set_Party_Command'Access);
         else
            Tcl.Tk.Ada.Grid.Grid(Slave => Button);
            Tcl.Tk.Ada.Grid.Grid(Slave => Enemy_Frame);
         end if;
         configure(Widgt => Close_Button, options => "-command ShowCombatUI");
         Tcl_SetVar
           (interp => Get_Context, varName => "gamestate",
            newValue => "combat");
         Back_To_Work_Loop :
         for Member of Player_Ship.Crew loop
            if Member.Order = REST
              and then Member.Previous_Order in PILOT | ENGINEER | GUNNER then
               Member.Order := Member.Previous_Order;
               Member.Order_Time := 15;
               Add_Message
                 (Message =>
                    To_String(Source => Member.Name) &
                    " back to work for combat.",
                  M_Type => ORDERMESSAGE);
            end if;
         end loop Back_To_Work_Loop;
      end if;
      if Player_Ship.Crew(1).Order = BOARDING then
         Update_Boarding_Ui;
         Show_Combat_Frame(Frame_Name => ".boarding");
      else
         Update_Combat_Ui;
         Show_Combat_Frame(Frame_Name => ".combat");
      end if;
      Show_Screen(New_Screen_Name => "combatframe");
   end Show_Combat_Ui;

end Combat.UI;
