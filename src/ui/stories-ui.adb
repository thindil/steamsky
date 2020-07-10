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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships; use Ships;
with Utils.UI; use Utils.UI;

package body Stories.UI is

   -- ****if* SUI3/Show_Story_Command
   -- FUNCTION
   -- Show the current story event on map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Story_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Story_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      StoriesView: Ttk_Tree_View;
      EventIndex: Positive;
   begin
      StoriesView.Interp := Interp;
      StoriesView.Name :=
        New_String(".paned.storiesframe.canvas.stories.storiesview");
      StoryIndex := Positive'Value(Selection(StoriesView));
      CenterX := Stories_List(StoryIndex).SkyX;
      CenterY := Stories_List(StoryIndex).SkyY;
      ShowSkyMap(True);
      return TCL_OK;
   end Show_Story_Command;

   -- ****if* SUI3/Set_Story_Command
   -- FUNCTION
   -- Set the current story event as the player's ship destination
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Set_Story_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Story_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      StoriesView: Ttk_Tree_View;
      StoryIndex: Positive;
   begin
      StoriesView.Interp := Interp;
      StoriesView.Name :=
        New_String(".paned.storiesframe.canvas.stories.storiesview");
      StoryIndex := Positive'Value(Selection(StoriesView));
      if Stories_List(StoryIndex).SkyX = PlayerShip.SkyX and
        Stories_List(StoryIndex).SkyY = PlayerShip.SkyY then
         ShowMessage("You are at this story event now.");
         return TCL_OK;
      end if;
      PlayerShip.DestinationX := Stories_List(StoryIndex).SkyX;
      PlayerShip.DestinationY := Stories_List(StoryIndex).SkyY;
      AddMessage
        ("You set the travel destination for your ship.", OrderMessage);
      ShowSkyMap(True);
      return TCL_OK;
   end Set_Story_Command;

   procedure ShowStories is
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      StoriesCanvas: Tk_Canvas;
      StoriesFrame: Ttk_Frame;
      StoriesView: Ttk_Tree_View;
   begin
      Paned.Interp := Get_Context;
      Paned.Name := New_String(".paned");
      StoriesFrame.Interp := Get_Context;
      StoriesFrame.Name := New_String(Widget_Image(Paned) & ".storiesframe");
      StoriesCanvas.Interp := Get_Context;
      StoriesCanvas.Name := New_String(Widget_Image(StoriesFrame) & ".canvas");
      Label.Interp := Get_Context;
      Label.Name :=
        New_String(Widget_Image(StoriesCanvas) & ".stories.info.info.label");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "stories.tcl");
         Bind(StoriesFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         AddCommand("ShowEvent", Show_Story_Command'Access);
         AddCommand("SetEvent", Set_Story_Command'Access);
      elsif Winfo_Get(Label, "ismapped") = "1" then
         ShowSkyMap(True);
         return;
      end if;
      StoriesView.Interp := Get_Context;
      StoriesView.Name :=
        New_String(Widget_Image(StoriesCanvas) & ".stories.storiesview");
      Delete(StoriesView, "[list " & Children(StoriesView, "{}") & "]");
      Selection_Set(StoriesView, "[list 1]");
      configure
        (StoriesCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      StoriesFrame.Name := New_String(Widget_Image(StoriesCanvas) & ".stories");
      Canvas_Create
        (StoriesCanvas, "window",
         "[expr " & Winfo_Get(StoriesFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(StoriesFrame, "reqheight") & " / 2] -window " &
         Widget_Image(StoriesFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (StoriesCanvas,
         "-scrollregion [list " & BBox(StoriesCanvas, "all") & "]");
      ShowScreen("storiesframe");
   end ShowStories;

end Stories.UI;
