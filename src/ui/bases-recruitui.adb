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

with Interfaces.C; use Interfaces.C;
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
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Utils.UI; use Utils.UI;

package body Bases.RecruitUI is

   -- ****f* RecruitUI/Show_Recruit_Command
   -- FUNCTION
   -- Show the selected base available recruits
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Recruit_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      RecruitCanvas: Tk_Canvas;
      RecruitFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      RecruitsView: Ttk_Tree_View;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      RecruitFrame.Interp := Interp;
      RecruitFrame.Name := New_String(Widget_Image(Paned) & ".recruitframe");
      RecruitCanvas.Interp := Interp;
      RecruitCanvas.Name := New_String(Widget_Image(RecruitFrame) & ".canvas");
      Label.Interp := Interp;
      Label.Name :=
        New_String
          (Widget_Image(RecruitCanvas) & ".recruit.recruit.info.label");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "recruit.tcl");
         Bind(RecruitFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp crew}");
      RecruitsView.Interp := Interp;
      RecruitsView.Name :=
        New_String(Widget_Image(RecruitCanvas) & ".recruit.recruits.view");
      Delete(RecruitsView, "[list " & Children(RecruitsView, "{}") & "]");
      for I in SkyBases(BaseIndex).Recruits.Iterate loop
         Insert
           (RecruitsView,
            "{} end -id" & Positive'Image(Recruit_Container.To_Index(I)) &
            " -text {" & To_String(SkyBases(BaseIndex).Recruits(I).Name) &
            "}");
      end loop;
      Selection_Set(RecruitsView, "[list 1]");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      RecruitFrame.Name :=
        New_String(Widget_Image(RecruitCanvas) & ".recruit");
      configure
        (RecruitCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (RecruitCanvas, "window",
         "[expr " & Winfo_Get(RecruitFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(RecruitFrame, "reqheight") & " / 2] -window " &
         Widget_Image(RecruitFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (RecruitCanvas,
         "-scrollregion [list " & BBox(RecruitCanvas, "all") & "]");
      ShowScreen("recruitframe");
      return TCL_OK;
   end Show_Recruit_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowRecruit", Show_Recruit_Command'Access);
   end AddCommands;

end Bases.RecruitUI;
