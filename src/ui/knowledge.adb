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
with Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with Interfaces.C; use Interfaces.C;
with CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with CoreUI;
with Knowledge.Bases;
with Knowledge.Events;
with Knowledge.Missions;
with Knowledge.Stories;
with Utils.UI;

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
      use Ada.Strings.Unbounded;
      use Interfaces.C.Strings;
      use Tcl.Tk.Ada.Widgets;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use CoreUI;

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
      use Utils.UI;
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
