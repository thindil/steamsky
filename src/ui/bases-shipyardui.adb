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
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
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
with Ships.Crew; use Ships.Crew;
with Utils.UI; use Utils.UI;

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
            Insert
              (ModulesView,
               "{} end -id" & Positive'Image(Modules_Container.To_Index(I)) &
               " -values [list {" & To_String(PlayerShip.Modules(I).Name) &
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

   -- ****iv* ShipyardUI/ModuleIndex
   -- SOURCE
   ModuleIndex: Unbounded_String;
   -- ****

   -- ****if* ShipyardUI/GetModuleInfo
   -- FUNCTION
   -- Show information about selected module
   -- PARAMETERS
   -- ModuleInfo - String which contains whole info about the module
   -- Installing - If true, player looking at installing modules list
   -- RESULT
   -- Parameter ModuleInfo
   -- SOURCE
   procedure GetModuleInfo
     (ModuleInfo: in out Unbounded_String; Installing: Boolean) is
      -- ****
      MType: ModuleType;
      MAmount, Weight, MaxValue, Value, MaxOwners: Natural;
      ShipModuleIndex, Size: Positive;
      Speed: Integer;
   begin
      if Installing then
         MType := Modules_List(ModuleIndex).MType;
         MaxValue := Modules_List(ModuleIndex).MaxValue;
         Value := Modules_List(ModuleIndex).Value;
         Size := Modules_List(ModuleIndex).Size;
         Weight := Modules_List(ModuleIndex).Weight;
         MaxOwners := Modules_List(ModuleIndex).MaxOwners;
         Speed := Modules_List(ModuleIndex).Speed;
      else
         ShipModuleIndex := Integer'Value(To_String(ModuleIndex));
         MType :=
           Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex).MType;
         case MType is
            when HARPOON_GUN =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).Duration;
               Value :=
                 Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                   .Value;
            when ENGINE =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).Power;
               Value := PlayerShip.Modules(ShipModuleIndex).FuelUsage;
            when CABIN =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).Quality;
               Value := PlayerShip.Modules(ShipModuleIndex).Cleanliness;
            when GUN =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).Damage;
               Value :=
                 Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                   .Value;
            when ShipModules.CARGO =>
               MaxValue :=
                 Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                   .MaxValue;
               Value :=
                 Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                   .Value;
            when HULL =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).MaxModules;
               Value :=
                 Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                   .Value;
            when BATTERING_RAM =>
               MaxValue := PlayerShip.Modules(ShipModuleIndex).Damage2;
               Value := 0;
            when others =>
               MaxValue := 0;
               Value := 0;
         end case;
         Size :=
           Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex).Size;
         Weight :=
           Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex).Weight;
         MaxOwners :=
           Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
             .MaxOwners;
         Speed :=
           Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex).Speed;
      end if;
      case MType is
         when HULL =>
            if Installing then
               Append(ModuleInfo, LF & "Ship hull can be only replaced.");
               Append
                 (ModuleInfo,
                  LF & "Modules space:" & Positive'Image(MaxValue));
            end if;
            Append(ModuleInfo, LF & "Max module size:" & Integer'Image(Value));
         when ENGINE =>
            Append(ModuleInfo, LF & "Max power:" & Positive'Image(MaxValue));
            if Installing then
               Append(ModuleInfo, LF & "Fuel usage:" & Positive'Image(Value));
            end if;
         when ShipModules.CARGO =>
            Append
              (ModuleInfo,
               LF & "Max cargo:" & Positive'Image(MaxValue) & " kg");
         when CABIN =>
            Append(ModuleInfo, LF & "Quality: ");
            if MaxValue < 30 then
               Append(ModuleInfo, "minimal");
            elsif MaxValue < 60 then
               Append(ModuleInfo, "basic");
            elsif MaxValue < 80 then
               Append(ModuleInfo, "extended");
            else
               Append(ModuleInfo, "luxury");
            end if;
            Append(ModuleInfo, LF & "Max owners:" & Natural'Image(MaxOwners));
         when ALCHEMY_LAB .. GREENHOUSE =>
            Append(ModuleInfo, LF & "Max workers:" & Natural'Image(MaxOwners));
         when GUN | HARPOON_GUN =>
            Append(ModuleInfo, LF & "Strength:" & Natural'Image(MaxValue));
            Append(ModuleInfo, LF & "Ammunition: ");
            MAmount := 0;
            for Item of Items_List loop
               if Item.IType = Items_Types(Value) then
                  if MAmount > 0 then
                     Append(ModuleInfo, " or ");
                  end if;
                  Append(ModuleInfo, Item.Name);
                  MAmount := MAmount + 1;
               end if;
            end loop;
            if MType = GUN then
               Append(ModuleInfo, LF);
               if Speed > 0 then
                  Append
                    (ModuleInfo,
                     "Max fire rate:" & Positive'Image(Speed) & "/round");
               else
                  Append
                    (ModuleInfo,
                     "Max fire rate: 1/" &
                     Trim(Integer'Image(abs (Speed)), Both) & " rounds");
               end if;
            end if;
         when BATTERING_RAM =>
            Append(ModuleInfo, LF & "Strength:" & Natural'Image(MaxValue));
         when others =>
            null;
      end case;
      if MType not in HULL | ARMOR then
         Append(ModuleInfo, LF & "Size:" & Natural'Image(Size));
         if Installing then
            for Module of PlayerShip.Modules loop
               if Module.MType = HULL
                 and then Size > Modules_List(Module.ProtoIndex).Value then
                  Append
                    (ModuleInfo, " <span foreground=""red"">(too big)</span>");
                  exit;
               end if;
            end loop;
         end if;
      end if;
      if Weight > 0 then
         Append(ModuleInfo, LF & "Weight:" & Natural'Image(Weight) & " kg");
      end if;
      if Installing then
         Append(ModuleInfo, LF & "Repair/Upgrade material: ");
         MAmount := 0;
         for Item of Items_List loop
            if Item.IType = Modules_List(ModuleIndex).RepairMaterial then
               if MAmount > 0 then
                  Append(ModuleInfo, " or ");
               end if;
               Append(ModuleInfo, Item.Name);
               MAmount := MAmount + 1;
            end if;
         end loop;
         Append
           (ModuleInfo,
            LF & "Repair/Upgrade skill: " &
            To_String
              (Skills_List(Modules_List(ModuleIndex).RepairSkill).Name) &
            "/" &
            To_String
              (Attributes_List
                 (Skills_List(Modules_List(ModuleIndex).RepairSkill).Attribute)
                 .Name));
         if Modules_List(ModuleIndex).Description /= Null_Unbounded_String then
            Append
              (ModuleInfo, LF & LF & Modules_List(ModuleIndex).Description);
         end if;
      end if;
   end GetModuleInfo;

   -- ****f* ShipyardUI/Show_Install_Info_Command
   -- FUNCTION
   -- Show information about the selected module to install
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Install_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Install_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      ModulesView: Ttk_Tree_View;
      ModuleInfo, InstallInfo: Unbounded_String;
      Cost: Positive;
      MoneyIndex2, UsedSpace, AllSpace, MaxSize: Natural;
   begin
      ModulesView.Interp := Interp;
      ModulesView.Name :=
        New_String(".paned.shipyardframe.canvas.notebook.install.modules.view");
      ModuleIndex := To_Unbounded_String(Selection(ModulesView));
      Cost := Modules_List(ModuleIndex).Price;
      CountPrice(Cost, FindMember(Talk));
      MoneyIndex2 := FindItem(PlayerShip.Cargo, MoneyIndex);
      ModuleInfo := To_Unbounded_String("Install cost:");
      if MoneyIndex2 = 0
        or else PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
         Append
           (ModuleInfo,
            "<span foreground=""red"">" & Positive'Image(Cost) & " " &
            To_String(MoneyName) & "</span> ");
      else
         Append(ModuleInfo, Positive'Image(Cost) & " " & To_String(MoneyName));
      end if;
      Append
        (ModuleInfo,
         LF & "Installation time:" &
         Positive'Image(Modules_List(ModuleIndex).InstallTime) & " minutes");
      GetModuleInfo(ModuleInfo, True);
--      Set_Label
--        (Gtk_Label(Get_Object(Object, "lblinstallinfo")),
--         To_String(ModuleInfo));
      if MoneyIndex2 > 0 then
         InstallInfo :=
           To_Unbounded_String
             (LF & "You have" &
              Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) & " " &
              To_String(MoneyName) & ".");
      else
         InstallInfo :=
           To_Unbounded_String
             (LF & "You don't have any " & To_String(MoneyName) &
              " to install anything.");
      end if;
      for Module of PlayerShip.Modules loop
         if Module.MType = HULL then
            UsedSpace := Module.InstalledModules;
            AllSpace := Module.MaxModules;
            MaxSize := Modules_List(Module.ProtoIndex).Value;
            Append
              (InstallInfo,
               LF & "You have used" & Natural'Image(UsedSpace) &
               " modules space from max" & Natural'Image(AllSpace) &
               " allowed.");
            exit;
         end if;
      end loop;
--      Set_Label
--        (Gtk_Label(Get_Object(Object, "lblmoneyinstall")),
--         To_String(InstallInfo));
--      if MoneyIndex2 = 0 then
--         Set_Sensitive(Gtk_Widget(Get_Object(Object, "btninstall")), False);
--      else
--         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost or
--           ((Modules_List(ModuleIndex).MType not in GUN | HARPOON_GUN |
--                 HULL) and
--            ((AllSpace - UsedSpace) < Modules_List(ModuleIndex).Size or
--             Modules_List(ModuleIndex).Size > MaxSize)) or
--           (Modules_List(ModuleIndex).MType = HULL and
--            Modules_List(ModuleIndex).MaxValue < UsedSpace) then
--            Set_Sensitive(Gtk_Widget(Get_Object(Object, "btninstall")), False);
--         else
--            Set_Sensitive(Gtk_Widget(Get_Object(Object, "btninstall")), True);
--         end if;
--      end if;
      return TCL_OK;
   end Show_Install_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowShipyard", Show_Shipyard_Command'Access);
      AddCommand("ShowInstallInfo", Show_Install_Info_Command'Access);
   end AddCommands;

end Bases.ShipyardUI;
