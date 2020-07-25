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
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases.Trade; use Bases.Trade;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Utils.UI; use Utils.UI;

package body Bases.UI is

   -- ****f* BUI/Show_Base_UI_Command
   -- FUNCTION
   -- Show the selected base action
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Show_Base_UI_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_UI_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      Paned: Ttk_PanedWindow;
      BaseCanvas: Tk_Canvas;
      BaseFrame: Ttk_Frame;
      CloseButton, ActionButton: Ttk_Button;
      SearchEntry: Ttk_Entry;
      ItemsView: Ttk_Tree_View;
      FirstIndex: Natural := 0;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      BaseFrame.Interp := Interp;
      BaseFrame.Name := New_String(Widget_Image(Paned) & ".baseframe");
      BaseCanvas.Interp := Interp;
      BaseCanvas.Name := New_String(Widget_Image(BaseFrame) & ".canvas");
      if Winfo_Get(BaseCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "base.tcl");
         Bind(BaseFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(BaseCanvas, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      BaseFrame.Name := New_String(Widget_Image(BaseCanvas) & ".base");
      SearchEntry.Interp := Interp;
      SearchEntry.Name := New_String(Widget_Image(BaseFrame) & ".search");
      if CArgv.Arg(Argv, 1) /= "recipes" then
         Tcl.Tk.Ada.Grid.Grid_Remove(SearchEntry);
      else
         Tcl.Tk.Ada.Grid.Grid(SearchEntry);
      end if;
      ItemsView.Interp := Interp;
      ItemsView.Name := New_String(Widget_Image(BaseFrame) & ".items.view");
      Delete(ItemsView, "[list " & Children(ItemsView, "{}") & "]");
      ActionButton.Interp := Interp;
      ActionButton.Name :=
        New_String(Widget_Image(BaseFrame) & ".info.accept");
      if CArgv.Arg(Argv, 1) = "heal" then
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp crew}");
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Health < 100 then
               if FirstIndex = 0 then
                  FirstIndex := Crew_Container.To_Index(I);
               end if;
               Insert
                 (ItemsView,
                  "{} end -id" & Positive'Image(Crew_Container.To_Index(I)) &
                  " -text {" & To_String(PlayerShip.Crew(I).Name) & "}");
            end if;
         end loop;
         Insert
           (ItemsView, "{} end -id 0 -text {Heal all wounded crew members}");
         Unbind(ItemsView, "<<TreeviewSelect>>");
         Bind(ItemsView, "<<TreeviewSelect>>", "ShowWoundedInfo");
         configure(ActionButton, "-text {Buy healing} -command HealWounded");
      end if;
      if FirstIndex = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Selection_Set(ItemsView, "[list" & Natural'Image(FirstIndex) & "]");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      BaseFrame.Name := New_String(Widget_Image(BaseCanvas) & ".base");
      configure
        (BaseCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (BaseCanvas, "window",
         "[expr " & Winfo_Get(BaseFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(BaseFrame, "reqheight") & " / 2] -window " &
         Widget_Image(BaseFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (BaseCanvas, "-scrollregion [list " & BBox(BaseCanvas, "all") & "]");
      ShowScreen("baseframe");
      return TCL_OK;
   end Show_Base_UI_Command;

   -- ****f* BUI/Show_Wounded_Info_Command
   -- FUNCTION
   -- Show the information about healing action
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Show_Wounded_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Wounded_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      WoundedView: Ttk_Tree_View;
      MemberIndex: Natural;
      FormattedTime: Unbounded_String;
      Cost, Time: Natural := 0;
      InfoLabel: Ttk_Label;
   begin
      WoundedView.Interp := Interp;
      WoundedView.Name :=
        New_String(".paned.baseframe.canvas.base.items.view");
      if Selection(WoundedView) = "" then
         return TCL_OK;
      end if;
      MemberIndex := Natural'Value(Selection(WoundedView));
      HealCost(Cost, Time, MemberIndex);
      if Time < 60 then
         FormattedTime := To_Unbounded_String(Natural'Image(Time) & " minute");
         if Time > 1 then
            Append(FormattedTime, "s");
         end if;
      else
         FormattedTime :=
           To_Unbounded_String(Positive'Image(Time / 60) & " hour");
         if (Time / 60) > 1 then
            Append(FormattedTime, "s");
         end if;
         if (Time mod 60) > 0 then
            Append
              (FormattedTime,
               " and" & Positive'Image(Time mod 60) & " minute");
            if (Time mod 60) > 1 then
               Append(FormattedTime, "s");
            end if;
         end if;
      end if;
      InfoLabel.Interp := Interp;
      InfoLabel.Name := New_String(".paned.baseframe.canvas.base.info.info");
      configure
        (InfoLabel,
         "-text {Heal cost:" & Natural'Image(Cost) & " " &
         To_String(MoneyName) & LF & "Heal time:" & To_String(FormattedTime) &
         "}");
      return TCL_OK;
   end Show_Wounded_Info_Command;

   -- ****f* BUI/Heal_Wounded_Command
   -- FUNCTION
   -- Heal selected crew member or the whole crew
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Heal_Wounded_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Heal_Wounded_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc, Argv);
      WoundedView: Ttk_Tree_View;
      MemberIndex: Natural;
   begin
      WoundedView.Interp := Interp;
      WoundedView.Name :=
        New_String(".paned.baseframe.canvas.base.items.view");
      MemberIndex := Natural'Value(Selection(WoundedView));
      HealWounded(MemberIndex);
      return Show_Base_UI_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowBaseUI" & "heal");
   end Heal_Wounded_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowBaseUI", Show_Base_UI_Command'Access);
      AddCommand("ShowWoundedInfo", Show_Wounded_Info_Command'Access);
      AddCommand("HealWounded", Heal_Wounded_Command'Access);
   end AddCommands;

end Bases.UI;
