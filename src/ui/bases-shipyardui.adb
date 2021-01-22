-- Copyright (c) 2020-2021 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
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
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases.Ship; use Bases.Ship;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with ShipModules; use ShipModules;
with Ships.Crew; use Ships.Crew;
with Table; use Table;
with Trades; use Trades;
with Utils.UI; use Utils.UI;

package body Bases.ShipyardUI is

   -- ****if* ShipyardUI/ShipyardUI.GetModuleType
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

   -- ****iv* ShipyardUI/ShipyardUI.InstallTable
   -- FUNCTION
   -- Table with info about the available modules
   -- SOURCE
   InstallTable: Table_Widget (5);
   -- ****

   -- ****f* ShipyardUI/ShipyardUI.Show_Shipyard_Command
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
      pragma Unreferenced(ClientData);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      ShipyardFrame: Ttk_Frame := Get_Widget(Paned & ".shipyardframe", Interp);
      ShipyardCanvas: constant Tk_Canvas :=
        Get_Widget(ShipyardFrame & ".canvas", Interp);
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      ModulesView: Ttk_Tree_View;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      ModuleSize: Integer;
      ModuleTypeBox: constant Ttk_ComboBox :=
        Get_Widget
          (ShipyardCanvas & ".shipyard.notebook.install.options.modules",
           Interp);
   begin
      if Winfo_Get(ShipyardCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "shipyard.tcl");
         Bind(ShipyardFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         ShipyardFrame :=
           Get_Widget(ShipyardCanvas & ".shipyard.notebook.install", Interp);
         InstallTable :=
           CreateTable
             (Widget_Image(ShipyardFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Type"),
               To_Unbounded_String("Size"), To_Unbounded_String("Materials"),
               To_Unbounded_String("Base cost")),
              False);
      elsif Winfo_Get(ShipyardCanvas, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      elsif Winfo_Get(ShipyardCanvas, "ismapped") = "0" and Argc = 1 then
         Current(ModuleTypeBox, "0");
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp ship}");
      ShipyardFrame.Name :=
        New_String(Widget_Image(ShipyardCanvas) & ".shipyard.notebook");
      ClearTable(InstallTable);
      for I in Modules_List.Iterate loop
         if Modules_List(I).Price > 0 and
           SkyBases(BaseIndex).Reputation(1) >= Modules_List(I).Reputation then
            if Argc > 1 and then Natural'Value(CArgv.Arg(Argv, 1)) > 0
              and then Natural'Value(CArgv.Arg(Argv, 1)) /=
                ModuleType'Pos(Modules_List(I).MType) then
               goto End_Of_Loop;
            end if;
            if Argc = 3
              and then Index(Modules_List(I).Name, CArgv.Arg(Argv, 2)) = 0 then
               goto End_Of_Loop;
            end if;
            case Modules_List(I).MType is
               when HULL =>
                  ModuleSize := Modules_List(I).MaxValue;
               when others =>
                  ModuleSize := Modules_List(I).Size;
            end case;
            AddButton
              (InstallTable, To_String(Modules_List(I).Name),
               "Show available options for module", "", 1);
            AddText
              (InstallTable, GetModuleType(BaseModules_Container.Key(I)), "",
               2);
            AddText(InstallTable, Integer'Image(ModuleSize), "", 3);
            AddText
              (InstallTable, To_String(Modules_List(I).RepairMaterial), "", 4);
            AddText
              (InstallTable, Positive'Image(Modules_List(I).Price), "", 5,
               True);
         end if;
         <<End_Of_Loop>>
      end loop;
      UpdateTable(InstallTable);
      ModulesView := Get_Widget(ShipyardFrame & ".remove.modules.view");
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
         "0 0 -anchor nw -window " & Widget_Image(ShipyardFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (ShipyardCanvas,
         "-scrollregion [list " & BBox(ShipyardCanvas, "all") & "]");
      ShowScreen("shipyardframe");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Shipyard_Command;

   -- ****iv* ShipyardUI/ShipyardUI.ModuleIndex
   -- SOURCE
   ModuleIndex: Unbounded_String;
   -- ****

   -- ****if* ShipyardUI/ShipyardUI.SetModuleInfo
   -- FUNCTION
   -- Show information about selected module
   -- PARAMETERS
   -- Installing - If true, player looking at installing modules list
   -- SOURCE
   procedure SetModuleInfo(Installing: Boolean) is
      -- ****
      MType: ModuleType;
      MAmount, Weight, MaxValue, Value, MaxOwners: Natural;
      ShipModuleIndex, Size: Positive;
      Speed: Integer;
      ModuleText: Tk_Text;
   begin
      if Installing then
         MType := Modules_List(ModuleIndex).MType;
         MaxValue := Modules_List(ModuleIndex).MaxValue;
         Value := Modules_List(ModuleIndex).Value;
         Size := Modules_List(ModuleIndex).Size;
         Weight := Modules_List(ModuleIndex).Weight;
         MaxOwners := Modules_List(ModuleIndex).MaxOwners;
         Speed := Modules_List(ModuleIndex).Speed;
         ModuleText :=
           Get_Widget
             (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.install.info.info.info");
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
         ModuleText :=
           Get_Widget
             (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.remove.info.info.info");
      end if;
      case MType is
         when HULL =>
            if Installing then
               Insert
                 (ModuleText, "end",
                  "{" & LF & "Ship hull can be only replaced.}");
               Insert
                 (ModuleText, "end",
                  "{" & LF & "Modules space:" & Positive'Image(MaxValue) &
                  "}");
            end if;
            Insert
              (ModuleText, "end",
               "{" & LF & "Max module size:" & Integer'Image(Value) & "}");
         when ENGINE =>
            Insert
              (ModuleText, "end",
               "{" & LF & "Max power:" & Positive'Image(MaxValue) & "}");
            if Installing then
               Insert
                 (ModuleText, "end",
                  "{" & LF & "Fuel usage:" & Positive'Image(Value) & "}");
            end if;
         when ShipModules.CARGO =>
            Insert
              (ModuleText, "end",
               "{" & LF & "Max cargo:" & Positive'Image(MaxValue) & " kg}");
         when CABIN =>
            Insert(ModuleText, "end", "{" & LF & "Quality: }");
            if MaxValue < 30 then
               Insert(ModuleText, "end", "{minimal}");
            elsif MaxValue < 60 then
               Insert(ModuleText, "end", "{basic}");
            elsif MaxValue < 80 then
               Insert(ModuleText, "end", "{extended}");
            else
               Insert(ModuleText, "end", "{luxury}");
            end if;
            Insert
              (ModuleText, "end",
               "{" & LF & "Max owners:" & Natural'Image(MaxOwners) & "}");
         when ALCHEMY_LAB .. GREENHOUSE =>
            Insert
              (ModuleText, "end",
               "{" & LF & "Max workers:" & Natural'Image(MaxOwners) & "}");
         when GUN | HARPOON_GUN =>
            Insert
              (ModuleText, "end",
               "{" & LF & "Strength:" & Natural'Image(MaxValue) & "}");
            Insert(ModuleText, "end", "{" & LF & "Ammunition: }");
            MAmount := 0;
            for Item of Items_List loop
               if Item.IType = Items_Types(Value) then
                  if MAmount > 0 then
                     Insert(ModuleText, "end", "{ or }");
                  end if;
                  Insert(ModuleText, "end", "{" & To_String(Item.Name) & "}");
                  MAmount := MAmount + 1;
               end if;
            end loop;
            if MType = GUN then
               Insert(ModuleText, "end", "{" & LF & "}");
               if Speed > 0 then
                  Insert
                    (ModuleText, "end",
                     "{Max fire rate:" & Positive'Image(Speed) & "/round}");
               else
                  Insert
                    (ModuleText, "end",
                     "{Max fire rate: 1/" &
                     Trim(Integer'Image(abs (Speed)), Both) & " rounds}");
               end if;
            end if;
         when BATTERING_RAM =>
            Insert
              (ModuleText, "end",
               "{" & LF & "Strength:" & Natural'Image(MaxValue) & "}");
         when others =>
            null;
      end case;
      if MType not in HULL | ARMOR then
         Insert
           (ModuleText, "end", "{" & LF & "Size:" & Natural'Image(Size) & "}");
         if Installing then
            for Module of PlayerShip.Modules loop
               if Module.MType = HULL
                 and then Size > Modules_List(Module.ProtoIndex).Value then
                  Insert(ModuleText, "end", "{ (too big)} [list red]");
                  exit;
               end if;
            end loop;
         end if;
      end if;
      if Weight > 0 then
         Insert
           (ModuleText, "end",
            "{" & LF & "Weight:" & Natural'Image(Weight) & " kg}");
      end if;
      if Installing then
         Insert(ModuleText, "end", "{" & LF & "Repair/Upgrade material: }");
         MAmount := 0;
         for Item of Items_List loop
            if Item.IType = Modules_List(ModuleIndex).RepairMaterial then
               if MAmount > 0 then
                  Insert(ModuleText, "end", "{ or }");
               end if;
               Insert(ModuleText, "end", "{" & To_String(Item.Name) & "}");
               MAmount := MAmount + 1;
            end if;
         end loop;
         Insert
           (ModuleText, "end",
            "{" & LF & "Repair/Upgrade skill: " &
            To_String
              (Skills_List(Modules_List(ModuleIndex).RepairSkill).Name) &
            "/" &
            To_String
              (Attributes_List
                 (Skills_List(Modules_List(ModuleIndex).RepairSkill).Attribute)
                 .Name) &
            "}");
         if Modules_List(ModuleIndex).Description /= Null_Unbounded_String then
            Insert
              (ModuleText, "end",
               "{" & LF & LF &
               To_String(Modules_List(ModuleIndex).Description) & "}");
         end if;
      end if;
   end SetModuleInfo;

   -- ****f* ShipyardUI/ShipyardUI.Show_Install_Info_Command
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
      ModulesView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.install.modules.view",
           Interp);
      InstallInfo: Unbounded_String;
      Cost: Positive;
      MoneyIndex2, UsedSpace, AllSpace, MaxSize: Natural;
      ModuleText: constant Tk_Text :=
        Get_Widget
          (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.install.info.info.info",
           Interp);
      MoneyLabel: constant Ttk_Label :=
        Get_Widget
          (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.install.info.money",
           Interp);
      InstallButton: constant Ttk_Button :=
        Get_Widget
          (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.install.info.install",
           Interp);
   begin
      if Selection(ModulesView) = "" then
         return TCL_OK;
      end if;
      ModuleIndex := To_Unbounded_String(Selection(ModulesView));
      Cost := Modules_List(ModuleIndex).Price;
      CountPrice(Cost, FindMember(Talk));
      MoneyIndex2 := FindItem(PlayerShip.Cargo, MoneyIndex);
      configure(ModuleText, "-state normal");
      Delete(ModuleText, "1.0", "end");
      Insert(ModuleText, "end", "{Install cost:}");
      if MoneyIndex2 = 0
        or else PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
         Insert
           (ModuleText, "end",
            "{" & Positive'Image(Cost) & " " & To_String(MoneyName) &
            "} [list red]");
      else
         Insert
           (ModuleText, "end",
            "{" & Positive'Image(Cost) & " " & To_String(MoneyName) & "}");
      end if;
      Insert
        (ModuleText, "end",
         "{" & LF & "Installation time:" &
         Positive'Image(Modules_List(ModuleIndex).InstallTime) & " minutes}");
      SetModuleInfo(True);
      InstallInfo :=
        (if MoneyIndex2 > 0 then
           To_Unbounded_String
             (LF & "You have" &
              Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) & " " &
              To_String(MoneyName) & ".")
         else To_Unbounded_String
             (LF & "You don't have any " & To_String(MoneyName) &
              " to install anything."));
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
      configure(MoneyLabel, "-text {" & To_String(InstallInfo) & "}");
      if MoneyIndex2 = 0 then
         configure(InstallButton, "-state disabled");
      else
         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost or
           ((Modules_List(ModuleIndex).MType not in GUN | HARPOON_GUN |
                 HULL) and
            ((AllSpace - UsedSpace) < Modules_List(ModuleIndex).Size or
             Modules_List(ModuleIndex).Size > MaxSize)) or
           (Modules_List(ModuleIndex).MType = HULL and
            Modules_List(ModuleIndex).MaxValue < UsedSpace) then
            configure(InstallButton, "-state disabled");
         else
            configure(InstallButton, "-state !disabled");
         end if;
      end if;
      configure(ModuleText, "-state disabled");
      return TCL_OK;
   end Show_Install_Info_Command;

   -- ****f* ShipyardUI/ShipyardUI.Manipulate_Module_Command
   -- FUNCTION
   -- Install or remove the selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Manipulate_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Manipulate_Module_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
   begin
      if CArgv.Arg(Argv, 1) = "install" then
         Bases.Ship.UpgradeShip(True, ModuleIndex);
      else
         Bases.Ship.UpgradeShip(False, ModuleIndex);
      end if;
      UpdateMessages;
      return Show_Shipyard_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowShipyard" & "0");
   exception
      when Trade_No_Money =>
         ShowMessage
           ("You don't have " & To_String(MoneyName) & " to pay for modules.");
         return TCL_OK;
      when An_Exception : Trade_Not_Enough_Money =>
         ShowMessage
           ("You don't have enough " & To_String(MoneyName) & " to pay for " &
            Exception_Message(An_Exception) & ".");
         return TCL_OK;
      when An_Exception : BasesShip_Unique_Module =>
         ShowMessage
           ("You can't install another " & Exception_Message(An_Exception) &
            " because you have installed one module that type. Remove old first.");
         return TCL_OK;
      when An_Exception : BasesShip_Installation_Error |
        BasesShip_Removing_Error =>
         ShowMessage(Exception_Message(An_Exception));
         return TCL_OK;
      when Trade_No_Free_Cargo =>
         ShowMessage
           ("You don't have enough free space for " & To_String(MoneyName) &
            " in ship cargo.");
         return TCL_OK;
      when Trade_No_Money_In_Base =>
         ShowMessage
           ("Base don't have enough " & To_String(MoneyName) &
            " for buy this module.");
         return TCL_OK;
   end Manipulate_Module_Command;

   -- ****f* ShipyardUI/ShipyardUI.Show_Remove_Info_Command
   -- FUNCTION
   -- Show information about the selected module to remove
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Remove_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Remove_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      RemoveInfo: Unbounded_String;
      Cost: Natural;
      Damage: Float;
      ShipModuleIndex: Natural;
      DamageBar: constant Ttk_ProgressBar :=
        Get_Widget
          (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.remove.info.info.damage",
           Interp);
      ModulesView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.remove.modules.view",
           Interp);
      ModuleText: constant Tk_Text :=
        Get_Widget
          (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.remove.info.info.info",
           Interp);
      Label: Ttk_Label :=
        Get_Widget
          (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.remove.info.info.damagelbl",
           Interp);
   begin
      ShipModuleIndex := Natural'Value(Selection(ModulesView));
      ModuleIndex := To_Unbounded_String(Selection(ModulesView));
      Damage :=
        1.0 -
        Float(PlayerShip.Modules(ShipModuleIndex).Durability) /
          Float(PlayerShip.Modules(ShipModuleIndex).MaxDurability);
      Cost :=
        Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex).Price -
        Integer
          (Float
             (Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                .Price) *
           Damage);
      if Cost = 0 then
         Cost := 1;
      end if;
      CountPrice(Cost, FindMember(Talk), False);
      configure(ModuleText, "-state normal");
      Delete(ModuleText, "1.0", "end");
      Insert(ModuleText, "end", "{Remove gain:" & Positive'Image(Cost) & "}");
      Insert
        (ModuleText, "end",
         "{" & LF & "Removing time:" &
         Positive'Image
           (Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
              .InstallTime) &
         " minutes}");
      SetModuleInfo(False);
      if Damage = 0.0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(DamageBar);
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      else
         configure(DamageBar, "-value" & Float'Image(Damage));
         if Damage < 0.2 then
            configure(Label, "-text {Damage: Slightly damaged}");
         elsif Damage < 0.5 then
            configure(Label, "-text {Damage: Damaged}");
         elsif Damage < 0.8 then
            configure(Label, "-text {Damage: Heavily damaged}");
         elsif Damage < 1.0 then
            configure(Label, "-text {Damage: Almost destroyed}");
         else
            configure(Label, "-text {Damage: Destroyed}");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Label);
         Tcl.Tk.Ada.Grid.Grid(DamageBar);
      end if;
      if Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
          .Description /=
        Null_Unbounded_String then
         Label.Name :=
           New_String
             (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.remove.info.info.description");
         configure
           (Label,
            "-text {" & LF &
            To_String
              (Modules_List(PlayerShip.Modules(ShipModuleIndex).ProtoIndex)
                 .Description) &
            "}");
      end if;
      declare
         MoneyIndex2: constant Natural :=
           FindItem(PlayerShip.Cargo, MoneyIndex);
      begin
         RemoveInfo :=
           (if MoneyIndex2 > 0 then
              To_Unbounded_String
                (LF & "You have" &
                 Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) & " " &
                 To_String(MoneyName) & ".")
            else To_Unbounded_String
                (LF & "You don't have any " & To_String(MoneyName) &
                 " to install anything."));
      end;
      for Module of PlayerShip.Modules loop
         if Module.MType = HULL then
            Append
              (RemoveInfo,
               LF & "You have used" & Natural'Image(Module.InstalledModules) &
               " modules space from max" & Natural'Image(Module.MaxModules) &
               " allowed.");
            exit;
         end if;
      end loop;
      Label.Name :=
        New_String
          (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.remove.info.money");
      configure(Label, "-text {" & To_String(RemoveInfo) & "}");
      configure(ModuleText, "-state disabled");
      return TCL_OK;
   end Show_Remove_Info_Command;

   -- ****if* ShipyardUI/ShipyardUI.Search_Shipyard_Command
   -- FUNCTION
   -- Show only this items which contains the selected sequence
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- SOURCE
   function Search_Shipyard_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Shipyard_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget
          (".gameframe.paned.shipyardframe.canvas.shipyard.notebook.install.options.modules",
           Interp);
      SearchText: constant String := CArgv.Arg(Argv, 1);
   begin
      if SearchText'Length = 0 then
         return Show_Shipyard_Command
             (ClientData, Interp, 2,
              CArgv.Empty & "ShowShipyard" & Current(TypeBox));
      end if;
      return Show_Shipyard_Command
          (ClientData, Interp, 3,
           CArgv.Empty & "ShowShipyard" & Current(TypeBox) & SearchText);
   end Search_Shipyard_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowShipyard", Show_Shipyard_Command'Access);
      AddCommand("ShowInstallInfo", Show_Install_Info_Command'Access);
      AddCommand("ManipulateModule", Manipulate_Module_Command'Access);
      AddCommand("ShowRemoveInfo", Show_Remove_Info_Command'Access);
      AddCommand("SearchShipyard", Search_Shipyard_Command'Access);
   end AddCommands;

end Bases.ShipyardUI;
