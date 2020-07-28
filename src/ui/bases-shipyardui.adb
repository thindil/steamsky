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

with Ada.Characters.Handling; use Ada.Characters.Handling;
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
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with ShipModules; use ShipModules;
with Utils.UI; use Utils.UI;
with ada.Text_IO;

package body Bases.ShipyardUI is

   -- ****if* ShipyardUI/GetModuleType
   -- FUNCTION
   -- Get type of selected module
   -- PARAMETERS
   -- ModuleIndex - Index of module in prototypes list
   -- RETURNS
   -- Formatted type of module
   -- SOURCE
   function GetModuleType(ModuleIndex: Unbounded_String) return String is
      -- ****
      ModuleTypeName: Unbounded_String :=
        To_Unbounded_String
          (To_Lower(ModuleType'Image(Modules_List(ModuleIndex).MType)));
   begin
      Replace_Element(ModuleTypeName, 1, To_Upper(Element(ModuleTypeName, 1)));
      while Index(ModuleTypeName, "_", 1) > 0 loop
         Replace_Element(ModuleTypeName, Index(ModuleTypeName, "_", 1), ' ');
      end loop;
      return To_String(ModuleTypeName);
   end GetModuleType;

   -- ****f* ShipyardUI/Show_Shipyard_Command
   -- FUNCTION
   -- Show the selected base shipyard
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Shipyard_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Shipyard_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Paned: Ttk_PanedWindow;
      ShipyardCanvas: Tk_Canvas;
      ShipyardFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      ModulesView: Ttk_Tree_View;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      ModuleSize: Integer;
      FirstIndex: Unbounded_String;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      ShipyardFrame.Interp := Interp;
      ShipyardFrame.Name := New_String(Widget_Image(Paned) & ".shipyardframe");
      ShipyardCanvas.Interp := Interp;
      ShipyardCanvas.Name :=
        New_String(Widget_Image(ShipyardFrame) & ".canvas");
      if Winfo_Get(ShipyardCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "shipyard.tcl");
         Bind(ShipyardFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(ShipyardCanvas, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp ship}");
      ShipyardFrame.Name :=
        New_String(Widget_Image(ShipyardCanvas) & ".shipyard.notebook");
      ModulesView.Interp := Interp;
      ModulesView.Name :=
        New_String(Widget_Image(ShipyardFrame) & ".install.modules.view");
      Delete(ModulesView, "[list " & Children(ModulesView, "{}") & "]");
      for I in Modules_List.Iterate loop
         if Modules_List(I).Price > 0 and
           SkyBases(BaseIndex).Reputation(1) >= Modules_List(I).Reputation then
            case Modules_List(I).MType is
               when HULL =>
                  ModuleSize := Modules_List(I).MaxValue;
               when others =>
                  ModuleSize := Modules_List(I).Size;
            end case;
            if FirstIndex = Null_Unbounded_String then
               FirstIndex := BaseModules_Container.Key(I);
            end if;
            Insert
              (ModulesView,
               "{} end -id {" & To_String(BaseModules_Container.Key(I)) &
               "} -values [list {" & To_String(Modules_List(I).Name) & "} {" &
               GetModuleType(BaseModules_Container.Key(I)) & "} {" &
               Integer'Image(ModuleSize) & "} {" &
               To_String(Modules_List(I).RepairMaterial) & "}]");
         end if;
      end loop;
      Selection_Set(ModulesView, "[list {" & To_String(FirstIndex) & "}]");
      ModulesView.Name :=
        New_String(Widget_Image(ShipyardFrame) & ".remove.modules.view");
      Delete(ModulesView, "[list " & Children(ModulesView, "{}") & "]");
      for I in PlayerShip.Modules.Iterate loop
         if Modules_List(PlayerShip.Modules(I).ProtoIndex).MType /= HULL then
            Ada.Text_IO.Put_Line(Positive'Image(Modules_Container.To_Index(I)));
            Insert
              (ModulesView,
               "{} end -id" & Positive'Image(Modules_Container.To_Index(I)) &
               "} -values [list {" & To_String(PlayerShip.Modules(I).Name) &
               "} {" & GetModuleType(PlayerShip.Modules(I).ProtoIndex) &
               "} {" &
               Integer'Image
                 (Modules_List(PlayerShip.Modules(I).ProtoIndex).Size) &
               "} {" &
               To_String
                 (Modules_List(PlayerShip.Modules(I).ProtoIndex)
                    .RepairMaterial) &
               "}]");
         end if;
      end loop;
      Selection_Set(ModulesView, "[list 2]");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      ShipyardFrame.Name :=
        New_String(Widget_Image(ShipyardCanvas) & ".shipyard");
      configure
        (ShipyardCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (ShipyardCanvas, "window",
         "[expr " & Winfo_Get(ShipyardFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(ShipyardFrame, "reqheight") & " / 2] -window " &
         Widget_Image(ShipyardFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (ShipyardCanvas,
         "-scrollregion [list " & BBox(ShipyardCanvas, "all") & "]");
      ShowScreen("shipyardframe");
      return TCL_OK;
   end Show_Shipyard_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowShipyard", Show_Shipyard_Command'Access);
   end AddCommands;

end Bases.ShipyardUI;
