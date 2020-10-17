-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Game; use Game;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Utils.UI; use Utils.UI;

package body Knowledge is

   function Show_Knowledge_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Paned: Ttk_PanedWindow;
      KnowledgeFrame, Item: Ttk_Frame;
      CloseButton: Ttk_Button;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      KnowledgeCanvas: Tk_Canvas;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      KnowledgeFrame.Interp := Interp;
      KnowledgeFrame.Name := New_String(Widget_Image(Paned) & ".knowledgeframe");
      if Winfo_Get(KnowledgeFrame, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "knowledge.tcl");
      elsif Winfo_Get(KnowledgeFrame, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      -- Setting bases list
      KnowledgeFrame.Name :=
        New_String
          (Widget_Image(Paned) & ".knowledgeframe.bases.canvas.frame");
      Tcl_Eval(Get_Context, "update");
      KnowledgeCanvas.Interp := Interp;
      KnowledgeCanvas.Name :=
        New_String(Widget_Image(Paned) & ".knowledgeframe.bases.canvas");
      configure
        (KnowledgeCanvas, "-scrollregion [list " & BBox(KnowledgeCanvas, "all") & "]");
      Xview_Move_To(KnowledgeCanvas, "0.0");
      Yview_Move_To(KnowledgeCanvas, "0.0");
      -- Setting accepted missions info
      KnowledgeFrame.Name := New_String(Widget_Image(Paned) & ".knowledgeframe");
      KnowledgeFrame.Name :=
        New_String(Widget_Image(KnowledgeFrame) & ".missions.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(KnowledgeFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 2 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (KnowledgeFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Interp := Interp;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      Tcl_Eval(Get_Context, "update");
      KnowledgeCanvas.Interp := Interp;
      KnowledgeCanvas.Name :=
        New_String(Widget_Image(Paned) & ".knowledgeframe.missions.canvas");
      configure
        (KnowledgeCanvas, "-scrollregion [list " & BBox(KnowledgeCanvas, "all") & "]");
      Xview_Move_To(KnowledgeCanvas, "0.0");
      Yview_Move_To(KnowledgeCanvas, "0.0");
      -- Setting the known events list
      -- Setting the known stories list
      -- Show knowledge
      ShowScreen("knowledgeframe");
      return TCL_OK;
   end Show_Knowledge_Command;

   -- ****o* Knowledge/Knowledge_Max_Min_Command
   -- FUNCTION
   -- Maximize or minimize the selected section of knowledge info
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- KnowledgeMaxMin framename
   -- Framename is name of the frame to maximize or minimize
   -- SOURCE
   function Knowledge_Max_Min_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Knowledge_Max_Min_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      type Frame_Info is record
         Name: Unbounded_String;
         Column: Natural range 0 .. 1;
         Row: Natural range 0 .. 1;
      end record;
      Frames: constant array(1 .. 4) of Frame_Info :=
        ((To_Unbounded_String("bases"), 0, 0),
         (To_Unbounded_String("missions"), 0, 1),
         (To_Unbounded_String("events"), 1, 0),
         (To_Unbounded_String("stories"), 1, 1));
      Frame: Ttk_Frame;
      Button: Ttk_Button;
   begin
      Frame.Interp := Interp;
      Frame.Name := New_String(".paned.knowledgeframe");
      Button.Interp := Interp;
      Button.Name :=
        New_String
          (Widget_Image(Frame) & "." & CArgv.Arg(Argv, 1) &
           ".canvas.frame.maxmin");
      if CArgv.Arg(Argv, 2) /= "show" then
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String(".paned.knowledgeframe." & To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame,
                  "-columnspan 1 -rowspan 1 -column" &
                  Natural'Image(FrameInfo.Column) & " -row" &
                  Natural'Image(FrameInfo.Row));
            end if;
         end loop;
         configure
           (Button,
            "-text ""[format %c 0xf106]"" -command {KnowledgeMaxMin " &
            CArgv.Arg(Argv, 1) & " show}");
      else
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String(".paned.knowledgeframe." & To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame, "-columnspan 2 -rowspan 2 -row 0 -column 0");
            end if;
         end loop;
         configure
           (Button,
            "-text ""[format %c 0xf107]"" -command {KnowledgeMaxMin " &
            CArgv.Arg(Argv, 1) & " hide}");
      end if;
      return TCL_OK;
   end Knowledge_Max_Min_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowKnowledge", Show_Knowledge_Command'Access);
      AddCommand("KnowledgeMaxMin", Knowledge_Max_Min_Command'Access);
   end AddCommands;

end Knowledge;
