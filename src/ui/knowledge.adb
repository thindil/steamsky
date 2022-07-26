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

with Ada.Containers; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Event; use Tcl.Tk.Ada.Event;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with BasesTypes; use BasesTypes;
with CoreUI; use CoreUI;
with Factions; use Factions;
with Game; use Game;
with Knowledge.Bases;
with Knowledge.Events;
with Knowledge.Missions;
with Maps.UI; use Maps.UI;
with Stories; use Stories;
with Knowledge.Stories;
with Utils.UI; use Utils.UI;

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
      Convention => C;
      -- ****

   function Show_Knowledge_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argv);
      use Tiny_String;

      Knowledge_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".knowledgeframe");
      Tokens: Slice_Set;
      Rows: Natural := 0;
      Knowledge_Canvas: Tk_Canvas :=
        Get_Widget
          (pathName => Knowledge_Frame & ".bases.canvas", Interp => Interp);
      Combo_Box: Ttk_ComboBox :=
        Get_Widget(pathName => Knowledge_Canvas & ".frame.options.types");
      Combo_Values: Unbounded_String;
      Label: Ttk_Label;
      Button: Ttk_Button;
   begin
      if Winfo_Get(Widgt => Knowledge_Frame, Info => "exists") = "0" then
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "knowledge.tcl");
         Append(Source => Combo_Values, New_Item => " {Any}");
         Load_Bases_Types_Loop :
         for BaseType of Bases_Types_List loop
            Append
              (Source => Combo_Values, New_Item => " {" & BaseType.Name & "}");
         end loop Load_Bases_Types_Loop;
         configure
           (Widgt => Combo_Box,
            options =>
              "-values [list" & To_String(Source => Combo_Values) & "]");
         Current(ComboBox => Combo_Box, NewIndex => "0");
         Combo_Values := To_Unbounded_String(Source => " {Any}");
         Combo_Box.Name :=
           New_String(Str => Knowledge_Canvas & ".frame.options.owner");
         Load_Bases_Owners_Loop :
         for I in Factions_List.Iterate loop
            Append
              (Source => Combo_Values,
               New_Item =>
                 " {" & To_String(Source => Factions_List(I).Name) & "}");
         end loop Load_Bases_Owners_Loop;
         configure
           (Widgt => Combo_Box,
            options =>
              "-values [list" & To_String(Source => Combo_Values) & "]");
         Current(ComboBox => Combo_Box, NewIndex => "0");
      elsif Winfo_Get(Widgt => Knowledge_Frame, Info => "ismapped") = "1" and
        Argc = 1 then
         Tcl_Eval(interp => Interp, strng => "InvokeButton " & Close_Button);
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence =>
              "<" & To_String(Source => General_Accelerators(1)) & ">");
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence =>
              "<" & To_String(Source => General_Accelerators(2)) & ">");
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence =>
              "<" & To_String(Source => General_Accelerators(3)) & ">");
         Unbind_From_Main_Window
           (Interp => Interp,
            Sequence =>
              "<" & To_String(Source => General_Accelerators(4)) & ">");
         return TCL_OK;
      end if;
      Bind_To_Main_Window
        (Interp => Interp,
         Sequence => "<" & To_String(Source => General_Accelerators(1)) & ">",
         Script => "{InvokeButton " & Knowledge_Canvas & ".frame.maxmin}");
      Bind_To_Main_Window
        (Interp => Interp,
         Sequence => "<" & To_String(Source => General_Accelerators(3)) & ">",
         Script =>
           "{InvokeButton " & Knowledge_Frame &
           ".missions.canvas.frame.maxmin}");
      Bind_To_Main_Window
        (Interp => Interp, Sequence => "<" & To_String(Source => General_Accelerators(2)) & ">",
         Script => "{InvokeButton " & Knowledge_Frame & ".events.canvas.frame.maxmin}");
      Bind_To_Main_Window
        (Interp => Interp, Sequence => "<" & To_String(Source => General_Accelerators(4)) & ">",
         Script => "{InvokeButton " & Knowledge_Frame & ".stories.canvas.frame.maxmin}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Close_Button, Options => "-row 0 -column 1");
      -- Setting bases list
      Knowledge.Bases.UpdateBasesList;
      -- Setting accepted missions info
      Knowledge.Missions.UpdateMissionsList;
      -- Setting the known events list
      Knowledge.Events.UpdateEventsList;
      -- Setting the known stories list
      Knowledge_Frame.Name :=
        New_String(Str => Main_Paned & ".knowledgeframe.stories.canvas.frame");
      Create(S => Tokens, From => Tcl.Tk.Ada.Grid.Grid_Size(Master => Knowledge_Frame), Separators => " ");
      Rows := Natural'Value(Slice(S => Tokens, Index => 2));
      Delete_Widgets(Start_Index => 1, End_Index => Rows - 1, Frame => Knowledge_Frame);
      if Finished_Stories.Length = 0 then
         Label :=
           Create
             (pathName => Knowledge_Frame & ".nostories",
              options => "-text {You didn't discover any story yet.} -wraplength 400");
         Tcl.Tk.Ada.Grid.Grid(Slave => Label, Options => "-padx 10");
      else
         Load_Finished_Stories_Block:
         declare
            Options_Frame: constant Ttk_Frame :=
              Create(pathName => Knowledge_Frame & ".options");
            Stories_Box: constant Ttk_ComboBox :=
              Create(pathName => Options_Frame & ".titles", options => "-state readonly");
            Finished_Stories_List: Unbounded_String;
            Stories_View: constant Tk_Text :=
              Create(pathName => Knowledge_Frame & ".view", options => "-wrap word");
         begin
            Load_Finished_Stories_Loop :
            for FinishedStory of Finished_Stories loop
               Append
                 (Source => Finished_Stories_List,
                  New_Item => " {" & Stories_List(FinishedStory.Index).Name & "}");
            end loop Load_Finished_Stories_Loop;
            configure
              (Widgt => Stories_Box, options => "-values [list " & To_String(Source => Finished_Stories_List) & "]");
            Bind(Widgt => Stories_Box, Sequence => "<<ComboboxSelected>>", Script => "ShowStory");
            Current
              (ComboBox => Stories_Box,
               NewIndex => Natural'Image(Natural(Finished_Stories.Length) - 1));
            Tcl.Tk.Ada.Grid.Grid(Slave => Stories_Box);
            Button :=
              Create
                (pathName => Options_Frame & ".show",
                 options => "-text {Show on map} -command ShowStoryLocation");
            Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-column 1 -row 0");
            Button :=
              Create
                (pathName => Options_Frame & ".set",
                 options => "-text {Set as destintion for ship} -command SetStory");
            Tcl.Tk.Ada.Grid.Grid(Slave => Button, Options => "-column 2 -row 0");
            Tcl.Tk.Ada.Grid.Grid(Slave => Options_Frame, Options => "-sticky w");
            Tcl.Tk.Ada.Grid.Grid(Slave => Stories_View, Options => "-sticky w");
            Generate(Window => Stories_Box, EventName => "<<ComboboxSelected>>");
         end Load_Finished_Stories_Block;
      end if;
      Tcl_Eval(interp => Get_Context, strng => "update");
      Knowledge_Canvas.Name :=
        New_String(Str => Main_Paned & ".knowledgeframe.stories.canvas");
      configure
        (Widgt => Knowledge_Canvas,
         options => "-scrollregion [list " & BBox(CanvasWidget => Knowledge_Canvas, TagOrId => "all") & "]");
      Xview_Move_To(CanvasWidget => Knowledge_Canvas, Fraction => "0.0");
      Yview_Move_To(CanvasWidget => Knowledge_Canvas, Fraction => "0.0");
      -- Show knowledge
      Show_Screen(New_Screen_Name => "knowledgeframe");
      return TCL_OK;
   end Show_Knowledge_Command;

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
      type Frame_Info is record
         Name: Unbounded_String;
         Column: Natural range 0 .. 1;
         Row: Natural range 0 .. 1;
      end record;
      Frames: constant array(1 .. 4) of Frame_Info :=
        (1 => (Name => To_Unbounded_String(Source => "bases"), Column => 0, Row => 0),
         2 => (Name => To_Unbounded_String(Source => "missions"), Column => 0, Row => 1),
         3 => (Name => To_Unbounded_String(Source => "events"), Column => 1, Row => 0),
         4 => (Name => To_Unbounded_String(Source => "stories"), Column => 1, Row => 1));
      Frame_Name: constant String := Main_Paned & ".knowledgeframe";
      Frame: Ttk_Frame := Get_Widget(pathName => Frame_Name, Interp => Interp);
      Button: constant Ttk_Button :=
        Get_Widget
          (pathName => Frame_Name & "." & CArgv.Arg(Argv => Argv, N => 1) & ".canvas.frame.maxmin",
           Interp => Interp);
   begin
      if CArgv.Arg(Argv => Argv, N => 2) /= "show" then
         Hide_Manipulate_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String(Str => Frame_Name & "." & To_String(Source => FrameInfo.Name));
            if To_String(Source => FrameInfo.Name) /= CArgv.Arg(Argv => Argv, N => 1) then
               Tcl.Tk.Ada.Grid.Grid(Slave => Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Slave => Frame,
                  Options => "-columnspan 1 -rowspan 1 -column" &
                  Natural'Image(FrameInfo.Column) & " -row" &
                  Natural'Image(FrameInfo.Row));
            end if;
         end loop Hide_Manipulate_Frames_Loop;
         configure
           (Button,
            "-image movemapupicon -command {KnowledgeMaxMin " &
            CArgv.Arg(Argv, 1) & " show}");
      else
         Show_Manipulate_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String(Frame_Name & "." & To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame, "-columnspan 2 -rowspan 2 -row 0 -column 0");
            end if;
         end loop Show_Manipulate_Frames_Loop;
         configure
           (Button,
            "-image movemapdownicon -command {KnowledgeMaxMin " &
            CArgv.Arg(Argv, 1) & " hide}");
      end if;
      return TCL_OK;
   end Knowledge_Max_Min_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowKnowledge", Show_Knowledge_Command'Access);
      Add_Command("KnowledgeMaxMin", Knowledge_Max_Min_Command'Access);
      Knowledge.Bases.AddCommands;
      Knowledge.Events.AddCommands;
      Knowledge.Missions.AddCommands;
      Knowledge.Stories.AddCommands;
   end Add_Commands;

end Knowledge;
