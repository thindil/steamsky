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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Config; use Config;
with CoreUI; use CoreUI;
with Dialogs; use Dialogs;
with Factions; use Factions;
with Game; use Game;
with Maps; use Maps;
with Messages; use Messages;
with Ships; use Ships;
with Table; use Table;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Knowledge.Bases is

   -- ****iv* KBases/KBases.Bases_Table
   -- FUNCTION
   -- Table with info about the know bases
   -- SOURCE
   Bases_Table: Table_Widget (Amount => 7);
   -- ****

   -- ****iv* KBases/KBases.Modules_Indexes
   -- FUNCTION
   -- Indexes of the player ship modules
   -- SOURCE
   Bases_Indexes: Positive_Container.Vector;
   -- ****

   -- ****if* KBases/KBases.Get_Reputation_Text
   -- FUNCTION
   -- Get the name of the reputation level in the selected base
   -- PARAMETERS
   -- Reputation_Level - The numerical level of reputation in a base
   -- RESULT
   -- The name of the reputation level in the selected base
   -- SOURCE
   function Get_Reputation_Text(Reputation_Level: Integer) return String is
      -- ****
   begin
      case Reputation_Level is
         when -100 .. -75 =>
            return "Hated";
         when -74 .. -50 =>
            return "Outlaw";
         when -49 .. -25 =>
            return "Hostile";
         when -24 .. -1 =>
            return "Unfriendly";
         when 0 =>
            return "Unknown";
         when 1 .. 25 =>
            return "Visitor";
         when 26 .. 50 =>
            return "Trader";
         when 51 .. 75 =>
            return "Friend";
         when 76 .. 100 =>
            return "Well known";
         when others =>
            return "";
      end case;
   end Get_Reputation_Text;

   procedure Update_Bases_List(Base_Name: String := ""; Page: Positive := 1) is
      use Tiny_String;

      Bases_Canvas: constant Tk_Canvas :=
        Get_Widget(pathName => Main_Paned & ".knowledgeframe.bases.canvas");
      Bases_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Bases_Canvas & ".frame");
      Search_Entry: constant Ttk_Entry :=
        Get_Widget(pathName => Bases_Frame & ".options.search");
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Bases_Frame & ".options.types");
      Bases_Type, Bases_Owner, Bases_Status, Color: Unbounded_String;
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
   begin
      Create
        (S => Tokens, From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Bases_Frame),
         Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      if Bases_Table.Row > 1 then
         Clear_Table(Table => Bases_Table);
      end if;
      Delete_Widgets
        (Start_Index => 2, End_Index => Rows - 1, Frame => Bases_Frame);
      Bases_Table :=
        Create_Table
          (Parent => Widget_Image(Win => Bases_Frame),
           Headers =>
             (1 => To_Unbounded_String(Source => "Name"),
              2 => To_Unbounded_String(Source => "Distance"),
              3 => To_Unbounded_String(Source => "Population"),
              4 => To_Unbounded_String(Source => "Size"),
              5 => To_Unbounded_String(Source => "Owner"),
              6 => To_Unbounded_String(Source => "Type"),
              7 => To_Unbounded_String(Source => "Reputation")),
           Scrollbar =>
             Get_Widget
               (pathName => ".gameframe.paned.knowledgeframe.bases.scrolly"),
           Command => "SortKnownBases {" & Base_Name & "}",
           Tooltip => "Press mouse button to sort the bases.");
      if Bases_Indexes.Is_Empty then
         Fill_Bases_Indexes_Loop :
         for I in Sky_Bases'Range loop
            Bases_Indexes.Append(New_Item => I);
         end loop Fill_Bases_Indexes_Loop;
      end if;
      if Base_Name'Length = 0 then
         configure(Widgt => Search_Entry, options => "-validatecommand {}");
         Delete
           (TextEntry => Search_Entry, FirstIndex => "0", LastIndex => "end");
         configure
           (Widgt => Search_Entry,
            options => "-validatecommand {ShowBases %P}");
      end if;
      Bases_Type := To_Unbounded_String(Source => Get(Widgt => Combo_Box));
      Combo_Box.Name := New_String(Str => Bases_Frame & ".options.status");
      Bases_Status := To_Unbounded_String(Source => Get(Widgt => Combo_Box));
      Combo_Box.Name := New_String(Str => Bases_Frame & ".options.owner");
      Bases_Owner := To_Unbounded_String(Source => Get(Widgt => Combo_Box));
      Rows := 0;
      Load_Bases_Loop :
      for I of Bases_Indexes loop
         if not Sky_Bases(I).Known then
            goto End_Of_Loop;
         end if;
         if Base_Name'Length > 0
           and then
             Index
               (Source =>
                  To_Lower(Item => To_String(Source => Sky_Bases(I).Name)),
                Pattern => To_Lower(Item => Base_Name), From => 1) =
             0 then
            goto End_Of_Loop;
         end if;
         if Bases_Status =
           To_Unbounded_String(Source => "Only not visited") and
           Sky_Bases(I).Visited.Year /= 0 then
            goto End_Of_Loop;
         end if;
         if Bases_Status = To_Unbounded_String(Source => "Only visited") and
           Sky_Bases(I).Visited.Year = 0 then
            goto End_Of_Loop;
         end if;
         if Sky_Bases(I).Visited.Year = 0
           and then
           (Bases_Type /= To_Unbounded_String(Source => "Any") or
            Bases_Owner /= To_Unbounded_String(Source => "Any")) then
            goto End_Of_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         Color :=
           (if Sky_Bases(I).Visited.Year > 0 then
              To_Unbounded_String(Source => "green3")
            else To_Unbounded_String(Source => ""));
         if Sky_Bases(I).Sky_X = Player_Ship.Destination_X and
           Sky_Bases(I).Sky_Y = Player_Ship.Destination_Y then
            Color := To_Unbounded_String(Source => "yellow");
         end if;
         Add_Button
           (Table => Bases_Table,
            Text => To_String(Source => Sky_Bases(I).Name),
            Tooltip => "Show available base's options",
            Command => "ShowBaseInfo" & Positive'Image(I), Column => 1,
            Color => To_String(Source => Color));
         Add_Button
           (Table => Bases_Table,
            Text =>
              Natural'Image
                (Count_Distance
                   (Destination_X => Sky_Bases(I).Sky_X,
                    Destination_Y => Sky_Bases(I).Sky_Y)),
            Tooltip => "The distance to the base",
            Command => "ShowBaseInfo" & Positive'Image(I), Column => 2,
            Color => To_String(Source => Color));
         if Sky_Bases(I).Visited.Year > 0 then
            Add_Button
              (Table => Bases_Table,
               Text =>
                 (case Sky_Bases(I).Population is when 0 => "empty",
                    when 1 .. 150 => "small", when 151 .. 299 => "medium",
                    when others => "large"),
               Tooltip => "The population size of the base",
               Command => "ShowBaseInfo" & Positive'Image(I), Column => 3,
               Color => To_String(Source => Color));
            Add_Button
              (Table => Bases_Table,
               Text => To_Lower(Item => Bases_Size'Image(Sky_Bases(I).Size)),
               Tooltip => "The size of the base",
               Command => "ShowBaseInfo" & Positive'Image(I), Column => 4,
               Color => To_String(Source => Color));
            Add_Button
              (Table => Bases_Table,
               Text =>
                 To_String(Source => Factions_List(Sky_Bases(I).Owner).Name),
               Tooltip => "The faction which own the base",
               Command => "ShowBaseInfo" & Positive'Image(I), Column => 5,
               Color => To_String(Source => Color));
            Add_Button
              (Table => Bases_Table,
               Text =>
                 To_String
                   (Source => Bases_Types_List(Sky_Bases(I).Base_Type).Name),
               Tooltip => "The type of the base",
               Command => "ShowBaseInfo" & Positive'Image(I), Column => 6,
               Color => To_String(Source => Color));
            Add_Button
              (Table => Bases_Table,
               Text =>
                 Get_Reputation_Text
                   (Reputation_Level => Sky_Bases(I).Reputation.Level),
               Tooltip => "Your reputation in the base",
               Command => "ShowBaseInfo" & Positive'Image(I), Column => 7,
               New_Row => True, Color => To_String(Source => Color));
         else
            Add_Button
              (Table => Bases_Table, Text => "not",
               Tooltip => "Show available base's options",
               Command => "ShowBaseInfo" & Positive'Image(I), Column => 3,
               Color => To_String(Source => Color));
            Add_Button
              (Table => Bases_Table, Text => "",
               Tooltip => "Show available base's options",
               Command => "ShowBaseInfo" & Positive'Image(I), Column => 4,
               Color => To_String(Source => Color));
            Add_Button
              (Table => Bases_Table, Text => "visited",
               Tooltip => "Show available base's options",
               Command => "ShowBaseInfo" & Positive'Image(I), Column => 5,
               Color => To_String(Source => Color));
            Add_Button
              (Table => Bases_Table, Text => "",
               Tooltip => "Show available base's options",
               Command => "ShowBaseInfo" & Positive'Image(I), Column => 6,
               Color => To_String(Source => Color));
            Add_Button
              (Table => Bases_Table, Text => "yet",
               Tooltip => "Show available base's options",
               Command => "ShowBaseInfo" & Positive'Image(I), Column => 7,
               Color => To_String(Source => Color), New_Row => True);
         end if;
         Rows := Rows + 1;
         exit Load_Bases_Loop when Rows = Game_Settings.Lists_Limit + 1 and
           I < Sky_Bases'Last;
         <<End_Of_Loop>>
      end loop Load_Bases_Loop;
      if Page > 1 then
         Add_Pagination
           (Table => Bases_Table,
            Previous_Command =>
              "ShowBases {" & Base_Name & "}" & Positive'Image(Page - 1),
            Next_Command =>
              (if Bases_Table.Row < Game_Settings.Lists_Limit + 1 then ""
               else "ShowBases {" & Base_Name & "}" &
                 Positive'Image(Page + 1)));
      elsif Bases_Table.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (Table => Bases_Table, Previous_Command => "",
            Next_Command =>
              "ShowBases {" & Base_Name & "}" & Positive'Image(Page + 1));
      end if;
      Update_Table
        (Table => Bases_Table,
         Grab_Focus =>
           (if Focus = Widget_Image(Win => Search_Entry) then False));
      Xview_Move_To(CanvasWidget => Bases_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Bases_Canvas, Fraction => "0.0");
      Tcl_Eval(interp => Get_Context, strng => "update");
      configure
        (Widgt => Bases_Canvas,
         options =>
           "-scrollregion [list " &
           BBox(CanvasWidget => Bases_Canvas, TagOrId => "all") & "]");
   end Update_Bases_List;

   -- ****o* KBases/KBases.Show_Bases_Command
   -- FUNCTION
   -- Show the list of known bases to a player
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBases ?basename? ?page?
   -- Basename parameter is a string which will be looking for in the bases
   -- names, page parameter is a index of page from which starts showing
   -- bases.
   -- SOURCE
   function Show_Bases_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Bases_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
   begin
      case Argc is
         when 3 =>
            Update_Bases_List
              (Base_Name => CArgv.Arg(Argv => Argv, N => 1),
               Page => Positive'Value(CArgv.Arg(Argv => Argv, N => 2)));
         when 2 =>
            Update_Bases_List(Base_Name => CArgv.Arg(Argv => Argv, N => 1));
         when others =>
            Update_Bases_List;
      end case;
      Tcl_SetResult(interp => Interp, str => "1");
      return TCL_OK;
   end Show_Bases_Command;

   -- ****o* KBases/KBases.Show_Base_Info_Command
   -- FUNCTION
   -- Show information about the selected base
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBaseInfo baseindex
   -- BaseIndex is the index of the base to show
   -- SOURCE
   function Show_Base_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Base_Index: constant Positive :=
        Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Base_Dialog: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".basedialog",
           Title => To_String(Source => Sky_Bases(Base_Index).Name),
           Columns => 3);
      Base_Label: Ttk_Label;
      Base_Info: Unbounded_String;
      Base_Button: Ttk_Button :=
        Create
          (pathName => Base_Dialog & ".destination",
           options =>
             "-text Target -image destinationicon -command {CloseDialog " &
             Base_Dialog & ";SetDestination2" &
             Map_X_Range'Image(Sky_Bases(Base_Index).Sky_X) &
             Map_Y_Range'Image(Sky_Bases(Base_Index).Sky_Y) &
             "} -style Dialog.TButton");
      procedure Set_Reputation_Text(Reputation_Text: String) is
         Reputation_Bar: constant Ttk_Frame :=
           Create
             (pathName => Base_Dialog & ".reputation",
              options => "-width 204 -height 24 -style ProgressBar.TFrame");
         Reputation_Label: constant Ttk_Label :=
           Create(pathName => Base_Dialog & ".reputationlabel");
         Reputation_Progress: constant Ttk_Frame :=
           Create
             (pathName => Reputation_Bar & ".reputation",
              options => "-height 18");
      begin
         if Sky_Bases(Base_Index).Reputation.Level = 0 then
            configure
              (Widgt => Reputation_Label,
               options => "-text {Reputation: Unknown}");
         else
            configure
              (Widgt => Reputation_Label, options => "-text {Reputation:}");
            Tcl.Tk.Ada.Grid.Grid
              (Slave => Reputation_Bar,
               Options => "-row 2 -column 1 -padx 5 -columnspan 2");
            Tcl.Tk.Ada.Grid.Grid_Propagate
              (Master => Reputation_Bar, Value => "off");
            configure
              (Widgt => Reputation_Progress,
               options =>
                 "-width" &
                 Positive'Image(abs (Sky_Bases(Base_Index).Reputation.Level)));
            if Sky_Bases(Base_Index).Reputation.Level > 0 then
               configure
                 (Widgt => Reputation_Progress,
                  options => "-style GreenProgressBar.TFrame");
               Tcl.Tk.Ada.Grid.Grid
                 (Slave => Reputation_Progress,
                  Options => "-padx {100 0} -pady 3");
            else
               configure
                 (Widgt => Reputation_Progress,
                  options => "-style RedProgressBar.TFrame");
               Tcl.Tk.Ada.Grid.Grid
                 (Slave => Reputation_Progress,
                  Options =>
                    "-padx {" &
                    Trim
                      (Source =>
                         Positive'Image
                           (100 + Sky_Bases(Base_Index).Reputation.Level),
                       Side => Left) &
                    " 0} -pady 3");
            end if;
            Add(Widget => Reputation_Bar, Message => Reputation_Text);
         end if;
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Reputation_Label,
            Options => "-row 2 -sticky w -padx {5 0}");
      end Set_Reputation_Text;
   begin
      Base_Info :=
        To_Unbounded_String
          (Source =>
             "Coordinates X:" & Positive'Image(Sky_Bases(Base_Index).Sky_X) &
             " Y:" & Positive'Image(Sky_Bases(Base_Index).Sky_Y));
      if Sky_Bases(Base_Index).Visited.Year > 0 then
         Append
           (Source => Base_Info,
            New_Item =>
              LF & "Last visited: " &
              Formated_Time(Time => Sky_Bases(Base_Index).Visited));
         Show_Mission_And_Recruits_Info_Block :
         declare
            Time_Diff: Integer;
         begin
            if Sky_Bases(Base_Index).Population > 0 and
              Sky_Bases(Base_Index).Reputation.Level > -25 then
               Time_Diff :=
                 30 -
                 Days_Difference
                   (Date_To_Compare => Sky_Bases(Base_Index).Recruit_Date);
               if Time_Diff > 0 then
                  Append
                    (Source => Base_Info,
                     New_Item =>
                       LF & "New recruits available in" &
                       Natural'Image(Time_Diff) & " days.");
               else
                  Append
                    (Source => Base_Info,
                     New_Item => LF & "New recruits available now.");
               end if;
            else
               Append
                 (Source => Base_Info,
                  New_Item =>
                    LF & "You can't recruit crew members at this base.");
            end if;
            if Sky_Bases(Base_Index).Population > 0 and
              Sky_Bases(Base_Index).Reputation.Level > -25 then
               Time_Diff :=
                 Days_Difference
                   (Date_To_Compare => Sky_Bases(Base_Index).Asked_For_Events);
               if Time_Diff < 7 then
                  Append
                    (Source => Base_Info,
                     New_Item =>
                       LF & "You asked for events" & Natural'Image(Time_Diff) &
                       " days ago.");
               else
                  Append
                    (Source => Base_Info,
                     New_Item => LF & "You can ask for events again.");
               end if;
            else
               Append
                 (Source => Base_Info,
                  New_Item => LF & "You can't ask for events at this base.");
            end if;
            if Sky_Bases(Base_Index).Population > 0 and
              Sky_Bases(Base_Index).Reputation.Level > -1 then
               Time_Diff :=
                 7 -
                 Days_Difference
                   (Date_To_Compare => Sky_Bases(Base_Index).Missions_Date);
               if Time_Diff > 0 then
                  Append
                    (Source => Base_Info,
                     New_Item =>
                       LF & "New missions available in" &
                       Natural'Image(Time_Diff) & " days.");
               else
                  Append
                    (Source => Base_Info,
                     New_Item => LF & "New missions available now.");
               end if;
            else
               Append
                 (Source => Base_Info,
                  New_Item => LF & "You can't take missions at this base.");
            end if;
         end Show_Mission_And_Recruits_Info_Block;
         Set_Reputation_Text
           (Reputation_Text =>
              Get_Reputation_Text
                (Reputation_Level => Sky_Bases(Base_Index).Reputation.Level));
         if Base_Index = Player_Ship.Home_Base then
            Append
              (Source => Base_Info, New_Item => LF & "It is your home base.");
         end if;
      else
         Append(Source => Base_Info, New_Item => LF & "Not visited yet.");
      end if;
      Base_Label :=
        Create
          (pathName => Base_Dialog & ".info",
           options =>
             "-text {" & To_String(Source => Base_Info) & "} -wraplength 400");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Base_Label,
         Options => "-row 1 -columnspan 3 -padx 5 -pady {5 0} -sticky w");
      Add
        (Widget => Base_Button,
         Message => "Set the base as the ship destination");
      Tcl.Tk.Ada.Grid.Grid(Slave => Base_Button, Options => "-row 3 -padx 5");
      Bind
        (Widgt => Base_Button, Sequence => "<Tab>",
         Script => "{focus " & Base_Dialog & ".button;break}");
      Bind
        (Widgt => Base_Button, Sequence => "<Escape>",
         Script => "{" & Base_Dialog & ".button invoke;break}");
      Add_Close_Button
        (Name => Base_Dialog & ".button", Text => "Close",
         Command => "CloseDialog " & Base_Dialog, Row => 3, Column => 1);
      Base_Button := Get_Widget(pathName => Base_Dialog & ".button");
      Bind
        (Widgt => Base_Button, Sequence => "<Tab>",
         Script => "{focus " & Base_Dialog & ".show;break}");
      Base_Button :=
        Create
          (pathName => Base_Dialog & ".show",
           options =>
             "-text Show -image showicon -command {CloseDialog " &
             Base_Dialog & ";ShowOnMap" &
             Map_X_Range'Image(Sky_Bases(Base_Index).Sky_X) &
             Map_Y_Range'Image(Sky_Bases(Base_Index).Sky_Y) &
             "} -style Dialog.TButton");
      Add(Widget => Base_Button, Message => "Show the base on the map");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Base_Button, Options => "-row 3 -column 2 -padx 5");
      Bind
        (Widgt => Base_Button, Sequence => "<Tab>",
         Script => "{focus " & Base_Dialog & ".destination;break}");
      Bind
        (Widgt => Base_Button, Sequence => "<Escape>",
         Script => "{" & Base_Dialog & ".button invoke;break}");
      Show_Dialog(Dialog => Base_Dialog);
      return TCL_OK;
   end Show_Base_Info_Command;

   -- ****it* KBases/KBases.Bases_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the known bases list
   -- OPTIONS
   -- NAMEASC        - Sort bases by name ascending
   -- NAMEDESC       - Sort bases by name descending
   -- DISTANCEASC    - Sort bases by distance ascending
   -- DISTANCEDESC   - Sort bases by distance descending
   -- POPULATIONASC  - Sort bases by population ascending
   -- POPULATIONDESC - Sort bases by population descending
   -- SIZEASC        - Sort bases by size ascending
   -- SIZEDESC       - Sort bases by size descending
   -- OWNERASC       - Sort bases by owner ascending
   -- OWNERDESC      - Sort bases by owner descending
   -- TYPEASC        - Sort bases by type ascending
   -- TYPEDESC       - Sort bases by type descending
   -- REPUTATIONASC  - Sort bases by reputation ascending
   -- REPUTATIONDESC - Sort bases by reputation descending
   -- NONE           - No sorting bases (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Bases_Sort_Orders is
     (NAMEASC, NAMEDESC, DISTANCEASC, DISTANCEDESC, POPULATIONASC,
      POPULATIONDESC, SIZEASC, SIZEDESC, OWNERASC, OWNERDESC, TYPEASC,
      TYPEDESC, REPUTATIONASC, REPUTATIONDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* KBases/KBases.Default_Bases_Sort_Order
      -- FUNCTION
      -- Default sorting order for the list of known bases
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Bases_Sort_Order: constant Bases_Sort_Orders := NONE;
   -- ****

   -- ****iv* KBases/KBases.Bases_Sort_Order
   -- FUNCTION
   -- The current sorting order for known bases list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Bases_Sort_Order: Bases_Sort_Orders := Default_Bases_Sort_Order;
   -- ****

   -- ****o* KBases/KBases.Sort_Bases_Command
   -- FUNCTION
   -- Sort the list of known bases
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed. Unused
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortKnownBases x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Bases_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Bases_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number
          (Table => Bases_Table,
           X_Position => Natural'Value(CArgv.Arg(Argv => Argv, N => 2)));
      type Local_Base_Data is record
         Name: Bounded_String;
         Distance: Natural;
         Population: Integer;
         Size: Bases_Size;
         Owner: Bounded_String;
         Base_Type: Bounded_String;
         Reputation: Integer;
         Id: Positive;
      end record;
      type Bases_Array is array(Positive range <>) of Local_Base_Data;
      Local_Bases: Bases_Array (Bases_Range);
      function "<"(Left, Right: Local_Base_Data) return Boolean is
      begin
         if Bases_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Bases_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Bases_Sort_Order = DISTANCEASC
           and then Left.Distance < Right.Distance then
            return True;
         end if;
         if Bases_Sort_Order = DISTANCEDESC
           and then Left.Distance > Right.Distance then
            return True;
         end if;
         if Bases_Sort_Order = POPULATIONASC
           and then Left.Population < Right.Population then
            return True;
         end if;
         if Bases_Sort_Order = POPULATIONDESC
           and then Left.Population > Right.Population then
            return True;
         end if;
         if Bases_Sort_Order = SIZEASC and then Left.Size < Right.Size then
            return True;
         end if;
         if Bases_Sort_Order = SIZEDESC and then Left.Size > Right.Size then
            return True;
         end if;
         if Bases_Sort_Order = OWNERASC and then Left.Owner < Right.Owner then
            return True;
         end if;
         if Bases_Sort_Order = OWNERDESC and then Left.Owner > Right.Owner then
            return True;
         end if;
         if Bases_Sort_Order = TYPEASC
           and then Left.Base_Type < Right.Base_Type then
            return True;
         end if;
         if Bases_Sort_Order = TYPEDESC
           and then Left.Base_Type > Right.Base_Type then
            return True;
         end if;
         if Bases_Sort_Order = REPUTATIONASC
           and then Left.Reputation < Right.Reputation then
            return True;
         end if;
         if Bases_Sort_Order = REPUTATIONDESC
           and then Left.Reputation > Right.Reputation then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Bases is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Base_Data,
         Array_Type => Bases_Array);
   begin
      case Column is
         when 1 =>
            if Bases_Sort_Order = NAMEASC then
               Bases_Sort_Order := NAMEDESC;
            else
               Bases_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Bases_Sort_Order = DISTANCEASC then
               Bases_Sort_Order := DISTANCEDESC;
            else
               Bases_Sort_Order := DISTANCEASC;
            end if;
         when 3 =>
            if Bases_Sort_Order = POPULATIONASC then
               Bases_Sort_Order := POPULATIONDESC;
            else
               Bases_Sort_Order := POPULATIONASC;
            end if;
         when 4 =>
            if Bases_Sort_Order = SIZEASC then
               Bases_Sort_Order := SIZEDESC;
            else
               Bases_Sort_Order := SIZEASC;
            end if;
         when 5 =>
            if Bases_Sort_Order = OWNERASC then
               Bases_Sort_Order := OWNERDESC;
            else
               Bases_Sort_Order := OWNERASC;
            end if;
         when 6 =>
            if Bases_Sort_Order = TYPEASC then
               Bases_Sort_Order := TYPEDESC;
            else
               Bases_Sort_Order := TYPEASC;
            end if;
         when 7 =>
            if Bases_Sort_Order = REPUTATIONASC then
               Bases_Sort_Order := REPUTATIONDESC;
            else
               Bases_Sort_Order := REPUTATIONASC;
            end if;
         when others =>
            null;
      end case;
      if Bases_Sort_Order = NONE then
         return TCL_OK;
      end if;
      Fill_Local_Bases_Loop :
      for I in Sky_Bases'Range loop
         Local_Bases(I) :=
           (Name => Sky_Bases(I).Name,
            Distance =>
              Count_Distance
                (Destination_X => Sky_Bases(I).Sky_X,
                 Destination_Y => Sky_Bases(I).Sky_Y),
            Population =>
              (if Sky_Bases(I).Visited = (others => 0) then -1
               else Sky_Bases(I).Population),
            Size =>
              (if Sky_Bases(I).Visited = (others => 0) then UNKNOWN
               else Sky_Bases(I).Size),
            Owner =>
              (if Sky_Bases(I).Visited = (others => 0) then Null_Bounded_String
               else Sky_Bases(I).Owner),
            Base_Type =>
              (if Sky_Bases(I).Visited = (others => 0) then Null_Bounded_String
               else Sky_Bases(I).Base_Type),
            Reputation =>
              (if Sky_Bases(I).Visited = (others => 0) then 200
               else Sky_Bases(I).Reputation.Level),
            Id => I);
      end loop Fill_Local_Bases_Loop;
      Sort_Bases(Container => Local_Bases);
      Bases_Indexes.Clear;
      Fill_Bases_Indexes_Loop :
      for Base of Local_Bases loop
         Bases_Indexes.Append(New_Item => Base.Id);
      end loop Fill_Bases_Indexes_Loop;
      Update_Bases_List(Base_Name => CArgv.Arg(Argv => Argv, N => 1));
      return TCL_OK;
   end Sort_Bases_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowBases", Ada_Command => Show_Bases_Command'Access);
      Add_Command
        (Name => "ShowBaseInfo", Ada_Command => Show_Base_Info_Command'Access);
      Add_Command
        (Name => "SortKnownBases", Ada_Command => Sort_Bases_Command'Access);
   end Add_Commands;

end Knowledge.Bases;
