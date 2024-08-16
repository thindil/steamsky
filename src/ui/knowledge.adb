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

with Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
-- with GNAT.Directory_Operations;
-- with GNAT.String_Split;
with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl; use Tcl;
-- with Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
-- with Tcl.Tk.Ada.Event;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
-- with Tcl.Tk.Ada.Widgets.Canvas;
-- with Tcl.Tk.Ada.Widgets.Text;
-- with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
-- with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
-- with Tcl.Tk.Ada.Widgets.TtkLabel;
-- with Tcl.Tk.Ada.Winfo;
-- with BasesTypes;
with CoreUI; use CoreUI;
-- with Factions;
-- with Game;
with Knowledge.Bases;
with Knowledge.Events;
with Knowledge.Missions;
-- with Maps.UI;
--## rule off REDUCEABLE_SCOPE
-- with Stories; use Stories;
--## rule on REDUCEABLE_SCOPE
with Knowledge.Stories;
with Utils.UI; use Utils.UI;
-- with Ships;

package body Knowledge is

   -- ****o* Knowledge/Knowledge.Show_Knowledge_Command
   -- FUNCTION
   -- Show information about known by player things
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowKnowledge
   -- SOURCE
   function Show_Knowledge_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Import => True,
      Convention => C,
      External_Name => "showKnowledgeCommand";
      -- ****

--   function Show_Knowledge_Command
--     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
--      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
--      pragma Unreferenced(Client_Data, Argv);
--      use GNAT.Directory_Operations;
--      use GNAT.String_Split;
--      use Tcl.Ada;
--      use Tcl.Tk.Ada.Widgets.Canvas;
--      use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
--      use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
--      use Tcl.Tk.Ada.Widgets.TtkLabel;
--      use Tcl.Tk.Ada.Winfo;
--      use BasesTypes;
--      use Factions;
--      use Game;
--      use Maps.UI;
--      use Ships;
--      use Tiny_String;
--
--      Knowledge_Frame: Ttk_Frame :=
--        Get_Widget(pathName => Main_Paned & ".knowledgeframe");
--      --## rule off IMPROPER_INITIALIZATION
--      Tokens: Slice_Set;
--      Label: Ttk_Label;
--      --## rule on IMPROPER_INITIALIZATION
--      Rows: Natural := 0;
--      Knowledge_Canvas: Tk_Canvas :=
--        Get_Widget
--          (pathName => Knowledge_Frame & ".bases.canvas", Interp => Interp);
--      Combo_Box: Ttk_ComboBox :=
--        Get_Widget(pathName => Knowledge_Canvas & ".frame.options.types");
--      Combo_Values: Unbounded_String := Null_Unbounded_String;
--   begin
--      Get_Ship_From_Nim(Ship => Player_Ship);
--      if Winfo_Get(Widgt => Knowledge_Frame, Info => "exists") = "0" then
--         Tcl_EvalFile
--           (interp => Get_Context,
--            fileName =>
--              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
--              "knowledge.tcl");
--         Append(Source => Combo_Values, New_Item => " {Any}");
--         Load_Bases_Types_Loop :
--         for BaseType of Bases_Types loop
--            exit Load_Bases_Types_Loop when Length(Source => BaseType) = 0;
--            Append
--              (Source => Combo_Values,
--               New_Item =>
--                 " {" & Get_Base_Type_Name(Base_Type => BaseType) & "}");
--         end loop Load_Bases_Types_Loop;
--         configure
--           (Widgt => Combo_Box,
--            options =>
--              "-values [list" & To_String(Source => Combo_Values) & "]");
--         Current(ComboBox => Combo_Box, NewIndex => "0");
--         Combo_Values := To_Unbounded_String(Source => " {Any}");
--         Combo_Box.Name :=
--           New_String(Str => Knowledge_Canvas & ".frame.options.owner");
--         Load_Bases_Owners_Loop :
--         for I in 1 .. Get_Factions_Amount loop
--            Append
--              (Source => Combo_Values,
--               New_Item =>
--                 " {" & To_String(Source => Get_Faction(Number => I).Name) &
--                 "}");
--         end loop Load_Bases_Owners_Loop;
--         configure
--           (Widgt => Combo_Box,
--            options =>
--              "-values [list" & To_String(Source => Combo_Values) & "]");
--         Current(ComboBox => Combo_Box, NewIndex => "0");
--      elsif Winfo_Get(Widgt => Knowledge_Frame, Info => "ismapped") = "1" and
--        Argc = 1 then
--         Tcl_Eval(interp => Interp, strng => "InvokeButton " & Close_Button);
--         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
--         Unbind_From_Main_Window
--           (Interp => Interp,
--            Sequence => "<" & Get_General_Accelerator(Index => 1) & ">");
--         Unbind_From_Main_Window
--           (Interp => Interp,
--            Sequence => "<" & Get_General_Accelerator(Index => 2) & ">");
--         Unbind_From_Main_Window
--           (Interp => Interp,
--            Sequence => "<" & Get_General_Accelerator(Index => 3) & ">");
--         Unbind_From_Main_Window
--           (Interp => Interp,
--            Sequence => "<" & Get_General_Accelerator(Index => 4) & ">");
--         return TCL_OK;
--      end if;
--      Tcl_SetVar
--        (interp => Interp, varName => "gamestate", newValue => "knowledge");
--      Bind_To_Main_Window
--        (Interp => Interp,
--         Sequence => "<" & Get_General_Accelerator(Index => 1) & ">",
--         Script => "{InvokeButton " & Knowledge_Canvas & ".frame.maxmin}");
--      Bind_To_Main_Window
--        (Interp => Interp,
--         Sequence => "<" & Get_General_Accelerator(Index => 3) & ">",
--         Script =>
--           "{InvokeButton " & Knowledge_Frame &
--           ".missions.canvas.frame.maxmin}");
--      Bind_To_Main_Window
--        (Interp => Interp,
--         Sequence => "<" & Get_General_Accelerator(Index => 2) & ">",
--         Script =>
--           "{InvokeButton " & Knowledge_Frame &
--           ".events.canvas.frame.maxmin}");
--      Bind_To_Main_Window
--        (Interp => Interp,
--         Sequence => "<" & Get_General_Accelerator(Index => 4) & ">",
--         Script =>
--           "{InvokeButton " & Knowledge_Frame &
--           ".stories.canvas.frame.maxmin}");
--      Tcl.Tk.Ada.Grid.Grid
--        (Slave => Close_Button, Options => "-row 0 -column 1");
--      -- Setting bases list
--      Knowledge.Bases.Update_Bases_List;
--      -- Setting accepted missions info
--      Knowledge.Missions.Update_Missions_List;
--      -- Setting the known events list
--      Knowledge.Events.Update_Events_List;
--      -- Setting the known stories list
--      Knowledge_Frame.Name :=
--        New_String(Str => Main_Paned & ".knowledgeframe.stories.canvas.frame");
--      Create
--        (S => Tokens,
--         From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Knowledge_Frame),
--         Separators => " ");
--      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
--      Delete_Widgets
--        (Start_Index => 1, End_Index => Rows - 1, Frame => Knowledge_Frame);
--      if Length(Source => Get_Finished_Story(Index => 1).Index) = 0 then
--         Label :=
--           Create
--             (pathName => Knowledge_Frame & ".nostories",
--              options =>
--                "-text {You didn't discover any story yet.} -wraplength 400");
--         Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-padx 10");
--      else
--         Load_Finished_Stories_Block :
--         declare
--            use Tcl.Tk.Ada.Event;
--            use Tcl.Tk.Ada.Widgets.Text;
--
--            Options_Frame: constant Ttk_Frame :=
--              Create(pathName => Knowledge_Frame & ".options");
--            Stories_Box: constant Ttk_ComboBox :=
--              Create
--                (pathName => Options_Frame & ".titles",
--                 options => "-state readonly");
--            Finished_Stories_List: Unbounded_String := Null_Unbounded_String;
--            Stories_View: constant Tk_Text :=
--              Create
--                (pathName => Knowledge_Frame & ".view",
--                 options => "-wrap word");
--            --## rule off IMPROPER_INITIALIZATION
--            Finished_Story: Finished_Story_Data;
--            Button: Ttk_Button;
--            --## rule on IMPROPER_INITIALIZATION
--            Amount: Natural := 0;
--         begin
--            Load_Finished_Stories_Loop :
--            for I in 1 .. 100 loop
--               Finished_Story := Get_Finished_Story(Index => I);
--               exit Load_Finished_Stories_Loop when Length
--                   (Source => Finished_Story.Index) =
--                 0;
--               Append
--                 (Source => Finished_Stories_List,
--                  New_Item =>
--                    " {" & Get_Story(Index => Finished_Story.Index).Name &
--                    "}");
--               Amount := Amount + 1;
--            end loop Load_Finished_Stories_Loop;
--            configure
--              (Widgt => Stories_Box,
--               options =>
--                 "-values [list " &
--                 To_String(Source => Finished_Stories_List) & "]");
--            Bind
--              (Widgt => Stories_Box, Sequence => "<<ComboboxSelected>>",
--               Script => "ShowStory");
--            Current
--              (ComboBox => Stories_Box, NewIndex => Natural'Image(Amount - 1));
--            Tcl.Tk.Ada.Grid.Grid(Slave => Stories_Box);
--            Button :=
--              Create
--                (pathName => Options_Frame & ".show",
--                 options => "-text {Show on map} -command ShowStoryLocation");
--            Tcl.Tk.Ada.Grid.Grid
--              (Slave => Button, Options => "-column 1 -row 0");
--            Button :=
--              Create
--                (pathName => Options_Frame & ".set",
--                 options =>
--                   "-text {Set as destintion for ship} -command SetStory");
--            Tcl.Tk.Ada.Grid.Grid
--              (Slave => Button, Options => "-column 2 -row 0");
--            Tcl.Tk.Ada.Grid.Grid
--              (Slave => Options_Frame, Options => "-sticky w");
--            Tcl.Tk.Ada.Grid.Grid
--              (Slave => Stories_View, Options => "-sticky w");
--            Generate
--              (Window => Stories_Box, EventName => "<<ComboboxSelected>>");
--         end Load_Finished_Stories_Block;
--      end if;
--      Tcl_Eval(interp => Get_Context, strng => "update");
--      Knowledge_Canvas.Name :=
--        New_String(Str => Main_Paned & ".knowledgeframe.stories.canvas");
--      configure
--        (Widgt => Knowledge_Canvas,
--         options =>
--           "-scrollregion [list " &
--           BBox(CanvasWidget => Knowledge_Canvas, TagOrId => "all") & "]");
--      Xview_Move_To(CanvasWidget => Knowledge_Canvas, Fraction => "0.0");
--      Yview_Move_To(CanvasWidget => Knowledge_Canvas, Fraction => "0.0");
--      -- Show knowledge
--      Show_Screen(New_Screen_Name => "knowledgeframe");
--      return TCL_OK;
--   end Show_Knowledge_Command;

   -- ****o* Knowledge/Knowledge.Knowledge_Max_Min_Command
   -- FUNCTION
   -- Maximize or minimize the selected section of knowledge info
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- KnowledgeMaxMin framename
   -- Framename is name of the frame to maximize or minimize
   -- SOURCE
   function Knowledge_Max_Min_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Knowledge_Max_Min_Command
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
      Frames: constant array(1 .. 4) of Frame_Info :=
        (1 =>
           (Name => To_Unbounded_String(Source => "bases"), Column => 0,
            Row => 0),
         2 =>
           (Name => To_Unbounded_String(Source => "missions"), Column => 0,
            Row => 1),
         3 =>
           (Name => To_Unbounded_String(Source => "events"), Column => 1,
            Row => 0),
         4 =>
           (Name => To_Unbounded_String(Source => "stories"), Column => 1,
            Row => 1));
      Frame_Name: constant String := Main_Paned & ".knowledgeframe";
      Frame: Ttk_Frame := Get_Widget(pathName => Frame_Name, Interp => Interp);
      Button: constant Ttk_Button :=
        Get_Widget
          (pathName =>
             Frame_Name & "." & CArgv.Arg(Argv => Argv, N => 1) &
             ".canvas.frame.maxmin",
           Interp => Interp);
   begin
      if CArgv.Arg(Argv => Argv, N => 2) = "show" then
         Show_Manipulate_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Str =>
                   Frame_Name & "." & To_String(Source => FrameInfo.Name));
            if To_String(Source => FrameInfo.Name) =
              CArgv.Arg(Argv => Argv, N => 1) then
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Slave => Frame,
                  Options => "-columnspan 2 -rowspan 2 -row 0 -column 0");
            else
               Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Frame);
            end if;
         end loop Show_Manipulate_Frames_Loop;
         configure
           (Widgt => Button,
            options =>
              "-image movemapdownicon -command {KnowledgeMaxMin " &
              CArgv.Arg(Argv => Argv, N => 1) & " hide}");
      else
         Hide_Manipulate_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (Str =>
                   Frame_Name & "." & To_String(Source => FrameInfo.Name));
            if To_String(Source => FrameInfo.Name) =
              CArgv.Arg(Argv => Argv, N => 1) then
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Slave => Frame,
                  Options =>
                    "-columnspan 1 -rowspan 1 -column" &
                    Natural'Image(FrameInfo.Column) & " -row" &
                    Natural'Image(FrameInfo.Row));
            else
               Tcl.Tk.Ada.Grid.Grid(Slave => Frame);
            end if;
         end loop Hide_Manipulate_Frames_Loop;
         configure
           (Widgt => Button,
            options =>
              "-image movemapupicon -command {KnowledgeMaxMin " &
              CArgv.Arg(Argv => Argv, N => 1) & " show}");
      end if;
      return TCL_OK;
   end Knowledge_Max_Min_Command;

   procedure Add_Commands is
   begin
      Add_Command
        (Name => "ShowKnowledge",
         Ada_Command => Show_Knowledge_Command'Access);
      Add_Command
        (Name => "KnowledgeMaxMin",
         Ada_Command => Knowledge_Max_Min_Command'Access);
      Knowledge.Bases.Add_Knowledge_Bases_Commands;
      Knowledge.Events.Add_Knowledge_Events_Commands;
      Knowledge.Missions.Add_Knowledge_Missions_Commands;
      Knowledge.Stories.Add_Knowledge_Stories_Commands;
   end Add_Commands;

end Knowledge;
