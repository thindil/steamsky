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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.MenuButton; use Tcl.Tk.Ada.Widgets.MenuButton;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Missions; use Missions;
with ShipModules; use ShipModules;
with Ships; use Ships;
with Ships.Crew; use Ships.Crew;
with Utils.UI; use Utils.UI;

package body Crew.UI is

   -- ****f* CUI2/Show_Crew_Info_Command
   -- FUNCTION
   -- Show information about the player's ship crew
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Crew_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Crew_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      CrewCanvas: Tk_Canvas;
      CrewFrame, Item: Ttk_Frame;
      CloseButton: Ttk_Button;
      Tokens: Slice_Set;
      Rows, Row: Natural := 0;
      NeedClean, NeedRepair: Boolean;
      OrdersButton: Tk_MenuButton;
      OrdersMenu: Tk_Menu;
      OrdersNames: constant array(Positive range <>) of Unbounded_String :=
        (To_Unbounded_String("Piloting"), To_Unbounded_String("Engineering"),
         To_Unbounded_String("Gunner"), To_Unbounded_String("Repair ship"),
         To_Unbounded_String("Manufacturing"),
         To_Unbounded_String("Upgrading module"),
         To_Unbounded_String("Talking in bases"),
         To_Unbounded_String("Healing wounded"),
         To_Unbounded_String("Cleans ship"), To_Unbounded_String("On break"),
         To_Unbounded_String("Defends ship"), To_Unbounded_String("Boarding"),
         To_Unbounded_String("Trains"));
      function IsWorking
        (Owners: Natural_Container.Vector; MemberIndex: Positive)
         return Boolean is
      begin
         for Owner of Owners loop
            if Owner = MemberIndex then
               return True;
            end if;
         end loop;
         return False;
      end IsWorking;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      CrewFrame.Interp := Interp;
      CrewFrame.Name := New_String(Widget_Image(Paned) & ".crewframe");
      CrewCanvas.Interp := Interp;
      CrewCanvas.Name := New_String(Widget_Image(CrewFrame) & ".canvas");
      Label.Interp := Interp;
      Label.Name := New_String(Widget_Image(CrewCanvas) & ".crew.crew.name");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "crew.tcl");
         Bind(CrewFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp crew}");
      CrewFrame.Name := New_String(Widget_Image(CrewCanvas) & ".crew.crew");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(CrewFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves(CrewFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Interp := Get_Context;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      Row := 1;
      for I in PlayerShip.Crew.Iterate loop
         NeedClean := False;
         NeedRepair := False;
         Label :=
           Create
             (Widget_Image(CrewFrame) & ".name" &
              Trim(Natural'Image(Row), Left),
              "-text {" & To_String(PlayerShip.Crew(I).Name) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row));
         OrdersButton :=
           Create
             (Widget_Image(CrewFrame) & ".orders" &
              Trim(Natural'Image(Row), Left),
              "-text {" &
              To_String
                (OrdersNames(Crew_Orders'Pos(PlayerShip.Crew(I).Order) + 1)) &
              "} -menu " & Widget_Image(CrewFrame) & ".orders" &
              Trim(Natural'Image(Row), Left) & ".menu");
         Tcl.Tk.Ada.Grid.Grid
           (OrdersButton, "-row" & Natural'Image(Row) & " -column 1");
         OrdersMenu.Create
           (Widget_Image(OrdersButton) & ".menu", "-tearoff false");
         if
           ((PlayerShip.Crew(I).Tired = 100 or
             PlayerShip.Crew(I).Hunger = 100 or
             PlayerShip.Crew(I).Thirst = 100) and
            PlayerShip.Crew(I).Order /= Rest) or
           (PlayerShip.Crew(I).Skills.Length = 0 or
            PlayerShip.Crew(I).ContractLength = 0) then
            Add
              (OrdersMenu, "command",
               "-label {Go on break} -command {SetCrewOrder Rest" &
               Positive'Image(Positive(Crew_Container.To_Index(I))) & "}");
         else
            if PlayerShip.Crew(I).Order /= Pilot then
               Add(OrdersMenu, "command", "-label {Piloting}");
            end if;
            if PlayerShip.Crew(I).Order /= Engineer then
               Add(OrdersMenu, "command", "-label {Engineering}");
            end if;
            for Module of PlayerShip.Modules loop
               if Module.Durability > 0 then
                  case Modules_List(Module.ProtoIndex).MType is
                     when GUN | HARPOON_GUN =>
                        if Module.Owner(1) /= Crew_Container.To_Index(I) then
                           Add
                             (OrdersMenu, "command",
                              "-label {Operate " & To_String(Module.Name) &
                              "}");
                        end if;
                     when ALCHEMY_LAB .. GREENHOUSE =>
                        if not IsWorking
                            (Module.Owner, Crew_Container.To_Index(I)) then
                           Add
                             (OrdersMenu, "command",
                              "-label {Work in " & To_String(Module.Name) &
                              "}");
                        end if;
                     when CABIN =>
                        if Module.Cleanliness < Module.Quality and
                          PlayerShip.Crew(I).Order /= Clean and NeedClean then
                           Add(OrdersMenu, "command", "-label {Clean ship}");
                           NeedClean := False;
                        end if;
                     when TRAINING_ROOM =>
                        if not IsWorking
                            (Module.Owner, Crew_Container.To_Index(I)) then
                           Add
                             (OrdersMenu, "command",
                              "-label {Go on training in " &
                              To_String(Module.Name) & "}");
                        end if;
                     when others =>
                        null;
                  end case;
                  if Module.Durability < Module.MaxDurability and
                    NeedRepair then
                     Add(OrdersMenu, "command", "-label {Repair ship}");
                     NeedRepair := False;
                  end if;
               end if;
            end loop;
            for J in PlayerShip.Crew.Iterate loop
               if PlayerShip.Crew(J).Health < 100 and
                 Crew_Container.To_Index(J) /= Crew_Container.To_Index(I) and
                 PlayerShip.Crew(J).Order /= Heal then
                  Add
                    (OrdersMenu, "command",
                     "-label {Heal wounded crew members}");
                  exit;
               end if;
            end loop;
            if PlayerShip.UpgradeModule > 0 and
              PlayerShip.Crew(I).Order /= Upgrading then
               Add(OrdersMenu, "command", "-label {Upgrade module}");
            end if;
            if PlayerShip.Crew(I).Order /= Talk then
               Add(OrdersMenu, "command", "-label {Talking in bases}");
            end if;
            if PlayerShip.Crew(I).Order /= Rest then
               Add
                 (OrdersMenu, "command",
                  "-label {Go on break} -command {SetCrewOrder Rest" &
                  Positive'Image(Positive(Crew_Container.To_Index(I))) & "}");
            end if;
         end if;
         Row := Row + 1;
      end loop;
      -- End of fill
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      CrewFrame.Name := New_String(Widget_Image(CrewCanvas) & ".crew");
      configure
        (CrewCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (CrewCanvas, "window",
         "[expr " & Winfo_Get(CrewFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(CrewFrame, "reqheight") & " / 2] -window " &
         Widget_Image(CrewFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (CrewCanvas, "-scrollregion [list " & BBox(CrewCanvas, "all") & "]");
      ShowScreen("crewframe");
      return TCL_OK;
   end Show_Crew_Info_Command;

   -- ****f* CUI2/Set_Crew_Order_Command
   -- FUNCTION
   -- Set order for the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Set_Crew_Order_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Crew_Order_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Order: Crew_Orders := Crew_Orders'Value(CArgv.Arg(Argv, 1));
      MemberIndex: Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      UpdateHeader;
      UpdateMessages;
      return TCL_OK;
   end Set_Crew_Order_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowCrewInfo", Show_Crew_Info_Command'Access);
      AddCommand("SetCrewOrder", Set_Crew_Order_Command'Access);
   end AddCommands;

end Crew.UI;
