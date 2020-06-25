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
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Config; use Config;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with ShipModules; use ShipModules;
with Utils.UI; use Utils.UI;

package body Ships.UI is

   -- ****f* SUI2/Show_Ship_Info_Command
   -- FUNCTION
   -- Show information about the player's ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Ship_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Ship_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Label, UpgradeLabel: Ttk_Label;
      Paned: Ttk_PanedWindow;
      ShipInfoCanvas: Tk_Canvas;
      ShipInfoFrame: Ttk_Frame;
      NameEntry: Ttk_Entry;
      ShipInfo, UpgradeInfo: Unbounded_String;
      MaxUpgrade: Integer;
      UpgradePercent: Float;
      UpgradeProgress: Ttk_ProgressBar;
      ModulesView: Ttk_Tree_View;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      ShipInfoFrame.Interp := Interp;
      ShipInfoFrame.Name := New_String(Widget_Image(Paned) & ".shipinfoframe");
      ShipInfoCanvas.Interp := Interp;
      ShipInfoCanvas.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".canvas");
      Label.Interp := Interp;
      Label.Name :=
        New_String(Widget_Image(ShipInfoCanvas) & ".shipinfo.left.info");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "shipinfo.tcl");
         Bind(ShipInfoFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" then
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      ShipInfoFrame.Name :=
        New_String(Widget_Image(ShipInfoCanvas) & ".shipinfo");
      NameEntry.Interp := Interp;
      NameEntry.Name := New_String(Widget_Image(ShipInfoFrame) & ".left.name");
      Delete(NameEntry, "0", "end");
      Insert(NameEntry, "0", To_String(PlayerShip.Name));
      ShipInfo :=
        To_Unbounded_String
          ("Home: " & To_String(SkyBases(PlayerShip.HomeBase).Name));
      UpgradeLabel.Interp := Interp;
      UpgradeLabel.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".left.upgradelabel");
      UpgradeProgress.Interp := Interp;
      UpgradeProgress.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".left.upgrade");
      if PlayerShip.UpgradeModule = 0 then
         Tcl.Tk.Ada.Grid.Grid_Remove(UpgradeLabel);
         Tcl.Tk.Ada.Grid.Grid_Remove(UpgradeProgress);
      else
         UpgradeInfo := To_Unbounded_String("Upgrading: ");
         Append
           (UpgradeInfo,
            To_String(PlayerShip.Modules(PlayerShip.UpgradeModule).Name) &
            " ");
         case PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeAction is
            when DURABILITY =>
               Append(UpgradeInfo, "(durability)");
               MaxUpgrade :=
                 Modules_List
                   (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                   .Durability;
            when MAX_VALUE =>
               case Modules_List
                 (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                 .MType is
                  when ENGINE =>
                     Append(UpgradeInfo, "(power)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .MaxValue /
                       20;
                  when CABIN =>
                     Append(UpgradeInfo, "(quality)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .MaxValue;
                  when GUN | BATTERING_RAM =>
                     Append(UpgradeInfo, "(damage)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .MaxValue *
                       2;
                  when HULL =>
                     Append(UpgradeInfo, "(enlarge)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .MaxValue *
                       40;
                  when HARPOON_GUN =>
                     Append(UpgradeInfo, "(strength)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .MaxValue *
                       10;
                  when others =>
                     null;
               end case;
            when VALUE =>
               case Modules_List
                 (PlayerShip.Modules(PlayerShip.UpgradeModule).ProtoIndex)
                 .MType is
                  when ENGINE =>
                     Append(UpgradeInfo, "(fuel usage)");
                     MaxUpgrade :=
                       Modules_List
                         (PlayerShip.Modules(PlayerShip.UpgradeModule)
                            .ProtoIndex)
                         .Value *
                       20;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
         MaxUpgrade :=
           Integer(Float(MaxUpgrade) * NewGameSettings.UpgradeCostBonus);
         if MaxUpgrade = 0 then
            MaxUpgrade := 1;
         end if;
         UpgradePercent :=
           1.0 -
           (Float
              (PlayerShip.Modules(PlayerShip.UpgradeModule).UpgradeProgress) /
            Float(MaxUpgrade));
         configure(UpgradeProgress, "-value" & Float'Image(UpgradePercent));
         if UpgradePercent < 0.11 then
            Append(UpgradeInfo, " (started)");
         elsif UpgradePercent < 0.31 then
            Append(UpgradeInfo, " (designing)");
         elsif UpgradePercent < 0.51 then
            Append(UpgradeInfo, " (base upgrades)");
         elsif UpgradePercent < 0.80 then
            Append(UpgradeInfo, " (advanced upgrades)");
         else
            Append(UpgradeInfo, " (final upgrades)");
         end if;
         configure(UpgradeLabel, "-text {" & To_String(UpgradeInfo) & "}");
         Tcl.Tk.Ada.Grid.Grid(UpgradeLabel, "-column 0 -row 1");
         Tcl.Tk.Ada.Grid.Grid(UpgradeProgress, "-column 1 -row 1");
      end if;
      Append(ShipInfo, LF & "Repair first: ");
      if PlayerShip.RepairModule = 0 then
         Append(ShipInfo, "Any module");
      else
         Append
           (ShipInfo,
            To_String(PlayerShip.Modules(PlayerShip.RepairModule).Name));
      end if;
      Append(ShipInfo, LF & "Destination: ");
      if PlayerShip.DestinationX = 0 and PlayerShip.DestinationY = 0 then
         Append(ShipInfo, "None");
      else
         if SkyMap(PlayerShip.DestinationX, PlayerShip.DestinationY)
             .BaseIndex >
           0 then
            Append
              (ShipInfo,
               To_String
                 (SkyBases
                    (SkyMap(PlayerShip.DestinationX, PlayerShip.DestinationY)
                       .BaseIndex)
                    .Name));
         else
            Append
              (ShipInfo,
               "X:" & Positive'Image(PlayerShip.DestinationX) & " Y:" &
               Positive'Image(PlayerShip.DestinationY));
         end if;
      end if;
      Append
        (ShipInfo,
         LF & "Weight:" & Integer'Image(CountShipWeight(PlayerShip)) & "kg");
      configure(Label, "-text {" & To_String(ShipInfo) & "}");
      ModulesView.Interp := Interp;
      ModulesView.Name :=
        New_String(Widget_Image(ShipInfoFrame) & ".left.modules");
      Delete(ModulesView, "[list " & Children(ModulesView, "{}") & "]");
      for I in PlayerShip.Modules.Iterate loop
         Insert
           (ModulesView,
            "{} end -id" & Positive'Image(Modules_Container.To_Index(I)) &
            " -text {" & To_String(PlayerShip.Modules(I).Name) & "}");
      end loop;
      Selection_Set(ModulesView, "[list 1]");
      configure
        (ShipInfoCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (ShipInfoCanvas, "window",
         "[expr " & Winfo_Get(ShipInfoFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(ShipInfoFrame, "reqheight") & " / 2] -window " &
         Widget_Image(ShipInfoFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (ShipInfoCanvas,
         "-scrollregion [list " & BBox(ShipInfoCanvas, "all") & "]");
      ShowScreen("shipinfoframe");
      return TCL_OK;
   end Show_Ship_Info_Command;

   -- ****f* SUI2/Set_Ship_Name_Command
   -- FUNCTION
   -- Change name of the player's ship
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Set_Ship_Name_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Ship_Name_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      NameEntry: Ttk_Entry;
   begin
      NameEntry.Interp := Interp;
      NameEntry.Name :=
        New_String(".paned.shipinfoframe.canvas.shipinfo.left.name");
      PlayerShip.Name := To_Unbounded_String(Get(NameEntry));
      return TCL_OK;
   end Set_Ship_Name_Command;

   -- ****f* SUI2/Show_Module_Info_Command
   -- FUNCTION
   -- Show information about the selected module and set option for it
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Module_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Module_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      Module: ModuleData;
      MaxValue, ModuleIndex: Positive;
      HaveAmmo: Boolean;
      Mamount, MaxUpgrade: Natural := 0;
      DamagePercent, UpgradePercent: Float;
      TreeView: Ttk_Tree_View;
      ProgressBar: Ttk_ProgressBar;
      Label: Ttk_Label;
      ModuleInfo, Tag: Unbounded_String;
      procedure AddOwnersInfo(OwnersName: String) is
         HaveOwner: Boolean := False;
         OwnerLabel, OwnerInfo: Unbounded_String;
      begin
         Append(OwnerLabel, OwnersName);
         if Module.Owner.Length > 1 then
            Append(OwnerLabel, "s");
         end if;
         Append
           (OwnerLabel,
            " (max" & Count_Type'Image(Module.Owner.Length) & "): ");
         for I in Module.Owner.First_Index .. Module.Owner.Last_Index loop
            if Module.Owner(I) > 0 then
               if HaveOwner then
                  Append(OwnerInfo, ", ");
               end if;
               HaveOwner := True;
               Append
                 (OwnerInfo, To_String(PlayerShip.Crew(Module.Owner(I)).Name));
            end if;
         end loop;
         if not HaveOwner then
            Append(OwnerInfo, "none");
         end if;
         Insert
           (TreeView,
            "{} end -values [list {" & To_String(OwnerLabel) & "} {" &
            To_String(OwnerInfo) & "}]");
      end AddOwnersInfo;
   begin
      TreeView.Interp := Interp;
      TreeView.Name :=
        New_String(".paned.shipinfoframe.canvas.shipinfo.left.modules");
      ModuleIndex := Positive'Value(Selection(TreeView));
      Module := PlayerShip.Modules(ModuleIndex);
      Label.Interp := Interp;
      Label.Name :=
        New_String
          (".paned.shipinfoframe.canvas.shipinfo.right.module.damagelbl");
      ProgressBar.Interp := Interp;
      ProgressBar.Name :=
        New_String(".paned.shipinfoframe.canvas.shipinfo.right.module.damage");
      if Module.Durability < Module.MaxDurability then
         DamagePercent :=
           (Float(Module.Durability) / Float(Module.MaxDurability));
         if DamagePercent < 1.0 and DamagePercent > 0.79 then
            configure(Label, "-text {Slightly damaged}");
         elsif DamagePercent < 0.8 and DamagePercent > 0.49 then
            configure(Label, "-text {Damaged}");
         elsif DamagePercent < 0.5 and DamagePercent > 0.19 then
            configure(Label, "-text {Heavily damaged}");
         elsif DamagePercent < 0.2 and DamagePercent > 0.0 then
            configure(Label, "-text {Almost destroyed}");
         elsif DamagePercent = 0.0 then
            configure(Label, "-text {Destroyed}");
         end if;
         configure(ProgressBar, "-value {" & Float'Image(DamagePercent) & "}");
         MaxValue :=
           Positive(Float(Modules_List(Module.ProtoIndex).Durability) * 1.5);
         if Module.MaxDurability = MaxValue then
            configure
              (Label, "-text {" & cget(Label, "-text") & " (max upgrade)}");
         end if;
      end if;
      TreeView.Name :=
        New_String(".paned.shipinfoframe.canvas.shipinfo.right.module.info");
      Delete(TreeView, "[list " & Children(TreeView, "{}") & "]");
      Insert
        (TreeView,
         "{} end -values [list {Weight:} {" & Integer'Image(Module.Weight) &
         " kg}]");
      for Item of Items_List loop
         if Item.IType = Modules_List(Module.ProtoIndex).RepairMaterial then
            if Mamount > 0 then
               Append(ModuleInfo, " or ");
            end if;
            Append(ModuleInfo, To_String(Item.Name));
            if FindItem
                (Inventory => PlayerShip.Cargo, ItemType => Item.IType) =
              0 and
              Tag = Null_Unbounded_String then
               Tag := To_Unbounded_String(" -tags [list Red]");
            end if;
            Mamount := Mamount + 1;
         end if;
      end loop;
      Insert
        (TreeView,
         "{} end -values [list {Repair/Upgrade material:} {" &
         To_String(ModuleInfo) & "}]" & To_String(Tag));
--      Append
--        (ModuleInfo,
--         LF & "Repair/Upgrade skill: " &
--         To_String
--           (Skills_List(Modules_List(Module.ProtoIndex).RepairSkill).Name) &
--         "/" &
--         To_String
--           (Attributes_List
--              (Skills_List(Modules_List(Module.ProtoIndex).RepairSkill)
--                 .Attribute)
--              .Name));
--      Set_Markup
--        (Gtk_Label(Get_Object(Object, "lblmoduleinfo")),
--         To_String(ModuleInfo));
--      ModuleInfo := Null_Unbounded_String;
--      Hide(Gtk_Widget(CleanBar));
--      Hide(Gtk_Widget(QualityBar));
--      case Module.MType is
--         when ENGINE =>
--            Append(ModuleInfo, "Max power:" & Integer'Image(Module.Power));
--            MaxValue :=
--              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
--            if Module.Power = MaxValue then
--               Append(ModuleInfo, " (max upgrade)");
--            end if;
--            if Module.Disabled then
--               Append(ModuleInfo, " (disabled)");
--            end if;
--            Append
--              (ModuleInfo,
--               LF & "Fuel usage:" & Integer'Image(Module.FuelUsage));
--            MaxValue :=
--              Positive(Float(Modules_List(Module.ProtoIndex).Value) / 2.0);
--            if Module.FuelUsage = MaxValue then
--               Append(ModuleInfo, " (max upgrade)");
--            end if;
--         when CARGO_ROOM =>
--            Append
--              (ModuleInfo,
--               "Max cargo:" &
--               Integer'Image(Modules_List(Module.ProtoIndex).MaxValue) &
--               " kg");
--         when HULL =>
--            Show_All(Gtk_Widget(CleanBar));
--            DamagePercent :=
--              Gdouble(Module.InstalledModules) / Gdouble(Module.MaxModules);
--            Set_Fraction(Gtk_Progress_Bar(CleanBar), DamagePercent);
--            Set_Text
--              (Gtk_Progress_Bar(CleanBar),
--               "Modules installed:" & Integer'Image(Module.InstalledModules) &
--               " /" & Integer'Image(Module.MaxModules));
--            MaxValue :=
--              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
--            if Module.MaxModules = MaxValue then
--               Set_Text
--                 (Gtk_Progress_Bar(CleanBar),
--                  Get_Text(Gtk_Progress_Bar(CleanBar)) & " (max upgrade)");
--            end if;
--         when CABIN =>
--            AddOwnersInfo("Owner");
--            Show_All(Gtk_Widget(QualityBar));
--            Set_Fraction
--              (Gtk_Progress_Bar(QualityBar), Gdouble(Module.Quality) / 100.0);
--            Set_Text
--              (Gtk_Progress_Bar(QualityBar), GetCabinQuality(Module.Quality));
--            MaxValue :=
--              Positive(Float(Modules_List(Module.ProtoIndex).MaxValue) * 1.5);
--            if Module.Quality = MaxValue then
--               Set_Text
--                 (Gtk_Progress_Bar(QualityBar),
--                  Get_Text(Gtk_Progress_Bar(QualityBar)) & " (max upgrade)");
--            end if;
--            if Module.Cleanliness /= Module.Quality then
--               Show_All(Gtk_Widget(CleanBar));
--               DamagePercent :=
--                 1.0 - (Gdouble(Module.Cleanliness) / Gdouble(Module.Quality));
--               if DamagePercent > 0.0 and DamagePercent < 0.2 then
--                  Set_Text(Gtk_Progress_Bar(CleanBar), "Bit dusty");
--               elsif DamagePercent > 0.19 and DamagePercent < 0.5 then
--                  Set_Text(Gtk_Progress_Bar(CleanBar), "Dusty");
--               elsif DamagePercent > 0.49 and DamagePercent < 0.8 then
--                  Set_Text(Gtk_Progress_Bar(CleanBar), "Dirty");
--               elsif DamagePercent > 0.79 and DamagePercent < 1.0 then
--                  Set_Text(Gtk_Progress_Bar(CleanBar), "Very dirty");
--               else
--                  Set_Text(Gtk_Progress_Bar(CleanBar), "Ruined");
--               end if;
--               Set_Fraction(Gtk_Progress_Bar(CleanBar), DamagePercent);
--            end if;
--         when GUN | HARPOON_GUN =>
--            Append(ModuleInfo, "Strength:");
--            if Modules_List(Module.ProtoIndex).MType = GUN then
--               Append(ModuleInfo, Positive'Image(Module.Damage));
--            else
--               Append(ModuleInfo, Positive'Image(Module.Duration));
--            end if;
--            Append(ModuleInfo, LF & "Ammunition: ");
--            HaveAmmo := False;
--            declare
--               AmmoIndex: Natural;
--            begin
--               if Module.MType = GUN then
--                  AmmoIndex := Module.AmmoIndex;
--               else
--                  AmmoIndex := Module.HarpoonIndex;
--               end if;
--               if
--                 (AmmoIndex >= PlayerShip.Cargo.First_Index and
--                  AmmoIndex <= PlayerShip.Cargo.Last_Index)
--                 and then
--                   Items_List(PlayerShip.Cargo(AmmoIndex).ProtoIndex).IType =
--                   Items_Types(Modules_List(Module.ProtoIndex).Value) then
--                  Append
--                    (ModuleInfo,
--                     To_String
--                       (Items_List(PlayerShip.Cargo(AmmoIndex).ProtoIndex)
--                          .Name) &
--                     " (assigned)");
--                  HaveAmmo := True;
--               end if;
--            end;
--            if not HaveAmmo then
--               Mamount := 0;
--               for I in Items_List.Iterate loop
--                  if Items_List(I).IType =
--                    Items_Types(Modules_List(Module.ProtoIndex).Value) then
--                     if Mamount > 0 then
--                        Append(ModuleInfo, " or ");
--                     end if;
--                     if FindItem(PlayerShip.Cargo, Objects_Container.Key(I)) >
--                       0 then
--                        Append(ModuleInfo, To_String(Items_List(I).Name));
--                     else
--                        Append
--                          (ModuleInfo,
--                           "<span foreground=""red"">" &
--                           To_String(Items_List(I).Name) & "</span>");
--                     end if;
--                     Mamount := Mamount + 1;
--                  end if;
--               end loop;
--            end if;
--            Append(ModuleInfo, LF);
--            if Module.Owner(1) > 0 then
--               Append
--                 (ModuleInfo,
--                  "Gunner: " &
--                  To_String(PlayerShip.Crew(Module.Owner(1)).Name));
--            else
--               Append(ModuleInfo, "Gunner: none");
--            end if;
--            if Module.MType = GUN then
--               Append(ModuleInfo, LF);
--               if Modules_List(Module.ProtoIndex).Speed > 0 then
--                  Append
--                    (ModuleInfo,
--                     "Max fire rate:" &
--                     Positive'Image(Modules_List(Module.ProtoIndex).Speed) &
--                     "/round");
--               else
--                  Append
--                    (ModuleInfo,
--                     "Max fire rate: 1/" &
--                     Trim
--                       (Integer'Image
--                          (abs (Modules_List(Module.ProtoIndex).Speed)),
--                        Both) &
--                     " rounds");
--               end if;
--            end if;
--         when TURRET =>
--            if Module.GunIndex > 0 then
--               Append
--                 (ModuleInfo,
--                  "Weapon: " &
--                  To_String(PlayerShip.Modules(Module.GunIndex).Name));
--            else
--               Append(ModuleInfo, "Weapon: none");
--            end if;
--         when WORKSHOP =>
--            AddOwnersInfo("Worker");
--            Append(ModuleInfo, LF);
--            if Module.CraftingIndex /= Null_Unbounded_String then
--               if Length(Module.CraftingIndex) > 6
--                 and then Slice(Module.CraftingIndex, 1, 5) = "Study" then
--                  Append
--                    (ModuleInfo,
--                     "Studying " &
--                     To_String
--                       (Items_List
--                          (Unbounded_Slice
--                             (Module.CraftingIndex, 7,
--                              Length(Module.CraftingIndex)))
--                          .Name));
--               elsif Length(Module.CraftingIndex) > 12
--                 and then Slice(Module.CraftingIndex, 1, 11) =
--                   "Deconstruct" then
--                  Append
--                    (ModuleInfo,
--                     "Deconstructing " &
--                     To_String
--                       (Items_List
--                          (Unbounded_Slice
--                             (Module.CraftingIndex, 13,
--                              Length(Module.CraftingIndex)))
--                          .Name));
--               else
--                  Append
--                    (ModuleInfo,
--                     "Manufacturing:" & Positive'Image(Module.CraftingAmount) &
--                     "x " &
--                     To_String
--                       (Items_List
--                          (Recipes_List(Module.CraftingIndex).ResultIndex)
--                          .Name));
--               end if;
--               Append
--                 (ModuleInfo,
--                  LF & "Time to complete current:" &
--                  Positive'Image(Module.CraftingTime) & " mins");
--            else
--               Append(ModuleInfo, "Manufacturing: nothing");
--            end if;
--         when MEDICAL_ROOM =>
--            AddOwnersInfo("Medic");
--         when TRAINING_ROOM =>
--            if Module.TrainedSkill > 0 then
--               Append
--                 (ModuleInfo,
--                  "Set for training " &
--                  To_String(Skills_List(Module.TrainedSkill).Name) & ".");
--            else
--               Append(ModuleInfo, "Must be set for training.");
--            end if;
--            Append(ModuleInfo, LF);
--            AddOwnersInfo("Trainee");
--         when BATTERING_RAM =>
--            Append(ModuleInfo, "Strength:");
--            Append(ModuleInfo, Positive'Image(Module.Damage2));
--         when others =>
--            null;
--      end case;
--      if Modules_List(Module.ProtoIndex).Size > 0 then
--         if ModuleInfo /= Null_Unbounded_String then
--            Append(ModuleInfo, LF);
--         end if;
--         Append
--           (ModuleInfo,
--            "Size:" & Natural'Image(Modules_List(Module.ProtoIndex).Size));
--      end if;
--      if Modules_List(Module.ProtoIndex).Description /=
--        Null_Unbounded_String then
--         if ModuleInfo /= Null_Unbounded_String then
--            Append(ModuleInfo, LF);
--         end if;
--         Append
--           (ModuleInfo,
--            LF & To_String(Modules_List(Module.ProtoIndex).Description));
--      end if;
--      Set_Markup
--        (Gtk_Label(Get_Object(Object, "lblmoduleinfo2")),
--         To_String(ModuleInfo));
--      if Module.UpgradeAction /= NONE then
--         ModuleInfo := To_Unbounded_String("Upgrading: ");
--         case Module.UpgradeAction is
--            when DURABILITY =>
--               Append(ModuleInfo, "durability");
--               MaxUpgrade := Modules_List(Module.ProtoIndex).Durability;
--            when MAX_VALUE =>
--               case Modules_List(Module.ProtoIndex).MType is
--                  when ENGINE =>
--                     Append(ModuleInfo, "power");
--                     MaxUpgrade :=
--                       Modules_List(Module.ProtoIndex).MaxValue / 20;
--                  when CABIN =>
--                     Append(ModuleInfo, "quality");
--                     MaxUpgrade := Modules_List(Module.ProtoIndex).MaxValue;
--                  when GUN | BATTERING_RAM =>
--                     Append(ModuleInfo, "damage");
--                     MaxUpgrade :=
--                       Modules_List(Module.ProtoIndex).MaxValue * 2;
--                  when HULL =>
--                     Append(ModuleInfo, "enlarge");
--                     MaxUpgrade :=
--                       Modules_List(Module.ProtoIndex).MaxValue * 40;
--                  when HARPOON_GUN =>
--                     Append(ModuleInfo, "strength");
--                     MaxUpgrade :=
--                       Modules_List(Module.ProtoIndex).MaxValue * 10;
--                  when others =>
--                     null;
--               end case;
--            when VALUE =>
--               case Modules_List(Module.ProtoIndex).MType is
--                  when ENGINE =>
--                     Append(ModuleInfo, "fuel usage");
--                     MaxUpgrade := Modules_List(Module.ProtoIndex).Value * 20;
--                  when others =>
--                     null;
--               end case;
--            when others =>
--               null;
--         end case;
--         MaxUpgrade :=
--           Integer(Float(MaxUpgrade) * NewGameSettings.UpgradeCostBonus);
--         if MaxUpgrade = 0 then
--            MaxUpgrade := 1;
--         end if;
--         UpgradePercent :=
--           1.0 - (Gdouble(Module.UpgradeProgress) / Gdouble(MaxUpgrade));
--         Set_Fraction(Gtk_Progress_Bar(UpgradeBar), UpgradePercent);
--         if UpgradePercent < 0.11 then
--            Append(ModuleInfo, " (started)");
--         elsif UpgradePercent < 0.31 then
--            Append(ModuleInfo, " (designing)");
--         elsif UpgradePercent < 0.51 then
--            Append(ModuleInfo, " (base upgrades)");
--         elsif UpgradePercent < 0.80 then
--            Append(ModuleInfo, " (advanced upgrades)");
--         else
--            Append(ModuleInfo, " (final upgrades)");
--         end if;
--         Set_Text(Gtk_Progress_Bar(UpgradeBar), To_String(ModuleInfo));
--         Show_All(Gtk_Widget(UpgradeBar));
--      else
--         Hide(Gtk_Widget(UpgradeBar));
--      end if;
--      ShowModuleOptions;
      return TCL_OK;
   end Show_Module_Info_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowShipInfo", Show_Ship_Info_Command'Access);
      AddCommand("SetShipName", Set_Ship_Name_Command'Access);
      AddCommand("ShowModuleInfo", Show_Module_Info_Command'Access);
   end AddCommands;

end Ships.UI;
