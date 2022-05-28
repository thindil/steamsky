-- Copyright (c) 2020-2022 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Containers.Generic_Array_Sort;
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
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Font; use Tcl.Tk.Ada.Font;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases.Ship; use Bases.Ship;
with Config; use Config;
with CoreUI; use CoreUI;
with Dialogs; use Dialogs;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with ShipModules; use ShipModules;
with Ships.Crew; use Ships.Crew;
with Table; use Table;
with Trades; use Trades;
with Utils.UI; use Utils.UI;

package body Bases.ShipyardUI is

   -- ****iv* ShipyardUI/ShipyardUI.InstallTable
   -- FUNCTION
   -- Table with info about the available modules
   -- SOURCE
   InstallTable: Table_Widget (5);
   -- ****

   -- ****iv* ShipyardUI/ShipyardUI.RemoveTable
   -- FUNCTION
   -- Table with info about the installed modules
   -- SOURCE
   RemoveTable: Table_Widget (5);
   -- ****

   -- ****iv* ShipyardUI/ShipyardUI.Install_Indexes
   -- FUNCTION
   -- Indexes of the available modules to install
   -- SOURCE
   Install_Indexes: Positive_Container.Vector;
   -- ****

   -- ****iv* ShipyardUI/ShipyardUI.Remove_Indexes
   -- FUNCTION
   -- Indexes of the modules in the player's ship (to remove)
   -- SOURCE
   Remove_Indexes: Positive_Container.Vector;
   -- ****

   -- ****f* ShipyardUI/ShipyardUI.Show_Shipyard_Command
   -- FUNCTION
   -- Show the selected base shipyard
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- COMMAND
   -- ShowShipyard ?moduletype? ?modulename?
   -- Show the base shipyard and load all available and installed modules
   -- lists. Moduletype is the type of modules to show in available modules,
   -- modulename is the name of the module to search in available modules.
   -- SOURCE
   function Show_Shipyard_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Shipyard_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      use Tiny_String;

      ShipyardFrame: Ttk_Frame :=
        Get_Widget(Main_Paned & ".shipyardframe", Interp);
      ShipyardCanvas: constant Tk_Canvas :=
        Get_Widget(ShipyardFrame & ".canvas", Interp);
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      ModuleSize: Integer;
      ModuleTypeBox: constant Ttk_ComboBox :=
        Get_Widget
          (ShipyardCanvas & ".shipyard.install.options.modules", Interp);
      Cost, UsedSpace: Natural;
      Damage: Float;
      MoneyIndex2: constant Natural :=
        Find_Item(Player_Ship.Cargo, Money_Index);
      MaxSize, AllSpace: Positive;
      InstallInfo: Unbounded_String;
      MoneyLabel: constant Ttk_Label :=
        Get_Widget(ShipyardCanvas & ".shipyard.moneyinfo", Interp);
      Page: constant Positive :=
        (if Argc = 4 then Positive'Value(CArgv.Arg(Argv, 3)) else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
      Arguments: constant String :=
        (if Argc > 2 then
           "{" & CArgv.Arg(Argv, 1) & "} {" & CArgv.Arg(Argv, 2) & "}"
         elsif Argc = 2 then CArgv.Arg(Argv, 1) & " {}" else "0 {}");
      SearchEntry: constant Ttk_Entry :=
        Get_Widget(ShipyardCanvas & ".shipyard.install.options.search");
   begin
      if Winfo_Get(ShipyardCanvas, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "shipyard.tcl");
         Bind(ShipyardFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         ShipyardFrame :=
           Get_Widget(ShipyardCanvas & ".shipyard.install", Interp);
         InstallTable :=
           Create_Table
             (Widget_Image(ShipyardFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Type"),
               To_Unbounded_String("Size"), To_Unbounded_String("Materials"),
               To_Unbounded_String("Cost")),
              Get_Widget(".gameframe.paned.shipyardframe.scrolly"), "",
              "Press mouse button to sort the modules.");
         ShipyardFrame :=
           Get_Widget(ShipyardCanvas & ".shipyard.remove", Interp);
         RemoveTable :=
           Create_Table
             (Widget_Image(ShipyardFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Type"),
               To_Unbounded_String("Size"), To_Unbounded_String("Materials"),
               To_Unbounded_String("Price")),
              Get_Widget(".gameframe.paned.shipyardframe.scrolly"),
              "SortShipyardModules remove 0 {}",
              "Press mouse button to sort the modules.");
      elsif Winfo_Get(ShipyardCanvas, "ismapped") = "1" then
         if Argc = 1 then
            Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
            Show_Sky_Map(True);
            return TCL_OK;
         else
            Current(ModuleTypeBox, CArgv.Arg(Argv, 1));
         end if;
      elsif Winfo_Get(ShipyardCanvas, "ismapped") = "0" and Argc = 1 then
         Current(ModuleTypeBox, "0");
      end if;
      Tcl_SetVar(Interp, "gamestate", "repair");
      Find_Max_Module_Size_Loop :
      for Module of Player_Ship.Modules loop
         if Module.M_Type = HULL then
            MaxSize :=
              BaseModules_Container.Element
                (Container => Modules_List, Index => Module.Proto_Index)
                .Value;
            UsedSpace := Module.Installed_Modules;
            AllSpace := Module.Max_Modules;
            exit Find_Max_Module_Size_Loop;
         end if;
      end loop Find_Max_Module_Size_Loop;
      ShipyardFrame.Name := New_String(ShipyardCanvas & ".shipyard");
      InstallInfo :=
        (if MoneyIndex2 > 0 then
           To_Unbounded_String
             ("You have" &
              Natural'Image
                (Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => MoneyIndex2)
                   .Amount) &
              " " & To_String(Money_Name) & ".")
         else To_Unbounded_String
             (LF & "You don't have any " & To_String(Money_Name) &
              " to install anything."));
      Append
        (InstallInfo,
         LF & "You have used" & Natural'Image(UsedSpace) &
         " modules space from max" & Natural'Image(AllSpace) & " allowed.");
      configure(MoneyLabel, "-text {" & To_String(InstallInfo) & "}");
      Tcl_Eval
        (Interp,
         "SetScrollbarBindings " & MoneyLabel &
         " .gameframe.paned.shipyardframe.scrolly");
      if Argc < 3 then
         configure(SearchEntry, "-validatecommand {}");
         Delete(SearchEntry, "0", "end");
         configure
           (SearchEntry,
            "-validatecommand {ShowShipyard [" & ShipyardFrame &
            ".install.options.modules current] %P}");
      end if;
      if Install_Indexes.Length = 0 then
         for I in
           BaseModules_Container.First_Index(Container => Modules_List) ..
             BaseModules_Container.Last_Index(Container => Modules_List) loop
            Install_Indexes.Append(I);
         end loop;
      end if;
      Update_Headers_Command
        (InstallTable, "SortShipyardModules install " & Arguments);
      Clear_Table(InstallTable);
      Load_Install_Modules_Loop :
      for I of Install_Indexes loop
         if BaseModules_Container.Element
             (Container => Modules_List, Index => I)
             .Price =
           0 or
           Sky_Bases(BaseIndex).Reputation.Level <
             BaseModules_Container.Element
               (Container => Modules_List, Index => I)
               .Reputation then
            goto End_Of_Loop;
         end if;
         if Argc > 1 and then Natural'Value(CArgv.Arg(Argv, 1)) > 0
           and then Natural'Value(CArgv.Arg(Argv, 1)) /=
             Module_Type'Pos
               (BaseModules_Container.Element
                  (Container => Modules_List, Index => I)
                  .M_Type) then
            goto End_Of_Loop;
         end if;
         if Argc > 2 and then CArgv.Arg(Argv, 2)'Length > 0
           and then
             Index
               (To_Lower
                  (To_String
                     (BaseModules_Container.Element
                        (Container => Modules_List, Index => I)
                        .Name)),
                To_Lower(CArgv.Arg(Argv, 2))) =
             0 then
            goto End_Of_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         ModuleSize :=
           (if
              BaseModules_Container.Element
                (Container => Modules_List, Index => I)
                .M_Type =
              HULL
            then
              BaseModules_Container.Element
                (Container => Modules_List, Index => I)
                .Max_Value
            else BaseModules_Container.Element
                (Container => Modules_List, Index => I)
                .Size);
         Add_Button
           (InstallTable,
            To_String
              (BaseModules_Container.Element
                 (Container => Modules_List, Index => I)
                 .Name),
            "Show available options for module",
            "ShowShipyardModuleMenu {" & Trim(I'Img, Left) & "} install", 1);
         Add_Button
           (InstallTable, Get_Module_Type(I),
            "Show available options for module",
            "ShowShipyardModuleMenu {" & Trim(I'Img, Left) & "} install", 2);
         Add_Button
           (InstallTable, Integer'Image(ModuleSize),
            "Show available options for module",
            "ShowShipyardModuleMenu {" & Trim(I'Img, Left) & "} install", 3,
            False,
            (if
               BaseModules_Container.Element
                 (Container => Modules_List, Index => I)
                 .M_Type =
               HULL
             then
               (if ModuleSize < AllSpace then "red"
                elsif ModuleSize > AllSpace then "green" else "")
             else (if ModuleSize > MaxSize then "red" else "")));
         Add_Button
           (InstallTable,
            To_String
              (BaseModules_Container.Element
                 (Container => Modules_List, Index => I)
                 .Repair_Material),
            "Show available options for module",
            "ShowShipyardModuleMenu {" & Trim(I'Img, Left) & "} install", 4);
         Cost :=
           BaseModules_Container.Element(Container => Modules_List, Index => I)
             .Price;
         Count_Price(Cost, Find_Member(TALK));
         Add_Button
           (InstallTable, Natural'Image(Cost),
            "Show available options for module",
            "ShowShipyardModuleMenu {" & Trim(I'Img, Left) & "} install", 5,
            True,
            (if
               MoneyIndex2 > 0
               and then Cost <=
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => MoneyIndex2)
                   .Amount
             then ""
             else "red"));
         exit Load_Install_Modules_Loop when InstallTable.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Loop>>
      end loop Load_Install_Modules_Loop;
      Add_Pagination
        (InstallTable,
         (if Page > 1 then
            "ShowShipyard " & Arguments & Positive'Image(Page - 1)
          else ""),
         (if InstallTable.Row < Game_Settings.Lists_Limit + 1 then ""
          else "ShowShipyard " & Arguments & Positive'Image(Page + 1)));
      Update_Table
        (InstallTable, (if Focus = Widget_Image(SearchEntry) then False));
      if Remove_Indexes.Length /= Player_Ship.Modules.Length then
         for I in Player_Ship.Modules.Iterate loop
            Remove_Indexes.Append(Modules_Container.To_Index(I));
         end loop;
      end if;
      Clear_Table(RemoveTable);
      Current_Row := 1;
      Load_Remove_Modules_Loop :
      for I of Remove_Indexes loop
         if BaseModules_Container.Element
             (Container => Modules_List,
              Index => Player_Ship.Modules(I).Proto_Index)
             .M_Type =
           HULL then
            goto End_Of_Remove_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Remove_Loop;
         end if;
         Add_Button
           (RemoveTable, To_String(Player_Ship.Modules(I).Name),
            "Show available options for module",
            "ShowShipyardModuleMenu {" & Positive'Image(I) & "} remove", 1);
         Add_Button
           (RemoveTable, Get_Module_Type(Player_Ship.Modules(I).Proto_Index),
            "Show available options for module",
            "ShowShipyardModuleMenu {" & Positive'Image(I) & "} remove", 2);
         Add_Button
           (RemoveTable,
            Integer'Image
              (BaseModules_Container.Element
                 (Container => Modules_List,
                  Index => Player_Ship.Modules(I).Proto_Index)
                 .Size),
            "Show available options for module",
            "ShowShipyardModuleMenu {" & Positive'Image(I) & "} remove", 3);
         Add_Button
           (RemoveTable,
            To_String
              (BaseModules_Container.Element
                 (Container => Modules_List,
                  Index => Player_Ship.Modules(I).Proto_Index)
                 .Repair_Material),
            "Show available options for module",
            "ShowShipyardModuleMenu {" & Positive'Image(I) & "} remove", 4);
         Damage :=
           1.0 -
           Float(Player_Ship.Modules(I).Durability) /
             Float(Player_Ship.Modules(I).Max_Durability);
         Cost :=
           BaseModules_Container.Element
             (Container => Modules_List,
              Index => Player_Ship.Modules(I).Proto_Index)
             .Price -
           Integer
             (Float
                (BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(I).Proto_Index)
                   .Price) *
              Damage);
         if Cost = 0 then
            Cost := 1;
         end if;
         Count_Price(Cost, Find_Member(TALK), False);
         Add_Button
           (RemoveTable, Natural'Image(Cost),
            "Show available options for module",
            "ShowShipyardModuleMenu {" & Positive'Image(I) & "} remove", 5,
            True);
         exit Load_Remove_Modules_Loop when RemoveTable.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Remove_Loop>>
      end loop Load_Remove_Modules_Loop;
      Add_Pagination
        (RemoveTable,
         (if Page > 1 then
            "ShowShipyard " & Arguments & Positive'Image(Page - 1)
          else ""),
         (if RemoveTable.Row < Game_Settings.Lists_Limit + 1 then ""
          else "ShowShipyard " & Arguments & Positive'Image(Page + 1)));
      Update_Table(RemoveTable);
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      configure
        (ShipyardCanvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
      Xview_Move_To(ShipyardCanvas, "0.0");
      Yview_Move_To(ShipyardCanvas, "0.0");
      Show_Screen("shipyardframe");
      Tcl_SetResult(Interp, "1");
      Tcl_Eval(Get_Context, "ShowShipyardTab show");
      return TCL_OK;
   end Show_Shipyard_Command;

   -- ****iv* ShipyardUI/ShipyardUI.ModuleIndex
   -- SOURCE
   ModuleIndex: BaseModules_Container.Extended_Index;
   -- ****

   -- ****if* ShipyardUI/ShipyardUI.SetModuleInfo
   -- FUNCTION
   -- Show information about selected module
   -- PARAMETERS
   -- Installing - If true, player looking at installing modules list
   -- SOURCE
   procedure SetModuleInfo(Installing: Boolean) is
      -- ****
      use Short_String;
      use Tiny_String;

      MType: Module_Type;
      MAmount, Weight, MaxValue, Value, MaxOwners: Natural;
      ShipModuleIndex: Natural := 0;
      Size: Positive;
      Speed: Integer;
      ModuleText: Tk_Text;
      Added: Boolean := False;
      Cost: Positive;
      MoneyIndex2: Natural;
   begin
      if Installing then
         MType :=
           BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .M_Type;
         MaxValue :=
           BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .Max_Value;
         Value :=
           BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .Value;
         Size :=
           BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .Size;
         Weight :=
           BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .Weight;
         MaxOwners :=
           BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .Max_Owners;
         Speed :=
           BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .Speed;
         ModuleText := Get_Widget(".moduledialog.info");
         Get_Module_Index_Block :
         declare
            Compare_Box: constant Ttk_ComboBox :=
              Get_Widget(pathName => ".moduledialog.compare.combo");
            Module_Iterator: Natural := 1;
         begin
            if Winfo_Get(Compare_Box, "ismapped") = "1" then
               Module_Iterator := Natural'Value(Current(Compare_Box)) + 1;
            end if;
            for I in Player_Ship.Modules.Iterate loop
               if BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(I).Proto_Index)
                   .M_Type =
                 MType then
                  Module_Iterator := Module_Iterator - 1;
                  if Module_Iterator = 0 then
                     ShipModuleIndex := Modules_Container.To_Index(I);
                     exit;
                  end if;
               end if;
            end loop;
         end Get_Module_Index_Block;
         Cost :=
           BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .Price;
         Count_Price(Cost, Find_Member(TALK));
         MoneyIndex2 := Find_Item(Player_Ship.Cargo, Money_Index);
         configure(ModuleText, "-state normal");
         Delete(ModuleText, "1.0", "end");
         Insert(ModuleText, "end", "{Install cost:}");
         Insert
           (ModuleText, "end",
            "{" & Positive'Image(Cost) & " " & To_String(Money_Name) & "}" &
            (if
               MoneyIndex2 = 0
               or else
                 Inventory_Container.Element
                   (Container => Player_Ship.Cargo, Index => MoneyIndex2)
                   .Amount <
                 Cost
             then " [list red]"
             else ""));
         Insert
           (ModuleText, "end",
            "{" & LF & "Installation time:" &
            Positive'Image
              (BaseModules_Container.Element
                 (Container => Modules_List, Index => ModuleIndex)
                 .Install_Time) &
            " minutes}");
      else
         ShipModuleIndex := ModuleIndex;
         MType :=
           BaseModules_Container.Element
             (Container => Modules_List,
              Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
             .M_Type;
         case MType is
            when HARPOON_GUN =>
               MaxValue := Player_Ship.Modules(ShipModuleIndex).Duration;
               Value :=
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Value;
            when ENGINE =>
               MaxValue := Player_Ship.Modules(ShipModuleIndex).Power;
               Value := Player_Ship.Modules(ShipModuleIndex).Fuel_Usage;
            when CABIN =>
               MaxValue := Player_Ship.Modules(ShipModuleIndex).Quality;
               Value := Player_Ship.Modules(ShipModuleIndex).Cleanliness;
            when GUN =>
               MaxValue := Player_Ship.Modules(ShipModuleIndex).Damage;
               Value :=
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Value;
            when ShipModules.CARGO =>
               MaxValue :=
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Max_Value;
               Value :=
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Value;
            when HULL =>
               MaxValue := Player_Ship.Modules(ShipModuleIndex).Max_Modules;
               Value :=
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Value;
            when BATTERING_RAM =>
               MaxValue := Player_Ship.Modules(ShipModuleIndex).Damage2;
               Value := 0;
            when others =>
               MaxValue := 0;
               Value := 0;
         end case;
         Size :=
           BaseModules_Container.Element
             (Container => Modules_List,
              Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
             .Size;
         Weight :=
           BaseModules_Container.Element
             (Container => Modules_List,
              Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
             .Weight;
         MaxOwners :=
           BaseModules_Container.Element
             (Container => Modules_List,
              Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
             .Max_Owners;
         Speed :=
           BaseModules_Container.Element
             (Container => Modules_List,
              Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
             .Speed;
         ModuleText := Get_Widget(".moduledialog.info");
      end if;
      case MType is
         when HULL =>
            if Installing then
               Insert
                 (ModuleText, "end",
                  "{" & LF & "Ship hull can be only replaced." & LF &
                  "Modules space:}");
               if MaxValue <
                 Player_Ship.Modules(ShipModuleIndex).Max_Modules then
                  Insert
                    (ModuleText, "end",
                     "{" & Positive'Image(MaxValue) &
                     " (smaller)} [list red]");
               elsif MaxValue >
                 Player_Ship.Modules(ShipModuleIndex).Max_Modules then
                  Insert
                    (ModuleText, "end",
                     "{" & Positive'Image(MaxValue) &
                     " (bigger)} [list green]");
               else
                  Insert
                    (ModuleText, "end", "{" & Positive'Image(MaxValue) & "}");
               end if;
            end if;
            Insert(ModuleText, "end", "{" & LF & "Max module size:}");
            if Installing then
               if Value <
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Value then
                  Insert
                    (ModuleText, "end",
                     "{" & Positive'Image(Value) & " (smaller)} [list red]");
               elsif Value >
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Value then
                  Insert
                    (ModuleText, "end",
                     "{" & Positive'Image(Value) & " (bigger)} [list green]");
               else
                  Insert(ModuleText, "end", "{" & Positive'Image(Value) & "}");
               end if;
            else
               Insert(ModuleText, "end", "{" & Positive'Image(Value) & "}");
            end if;
         when ENGINE =>
            Insert(ModuleText, "end", "{" & LF & "Max power:}");
            if Installing and then ShipModuleIndex > 0 then
               if MaxValue < Player_Ship.Modules(ShipModuleIndex).Power then
                  Insert
                    (ModuleText, "end",
                     "{" & Positive'Image(MaxValue) & " (weaker)} [list red]");
               elsif MaxValue > Player_Ship.Modules(ShipModuleIndex).Power then
                  Insert
                    (ModuleText, "end",
                     "{" & Positive'Image(MaxValue) &
                     " (stronger)} [list green]");
               else
                  Insert
                    (ModuleText, "end", "{" & Positive'Image(MaxValue) & "}");
               end if;
               Insert(ModuleText, "end", "{" & LF & "Fuel usage:}");
               if Value < Player_Ship.Modules(ShipModuleIndex).Fuel_Usage then
                  Insert
                    (ModuleText, "end",
                     "{" & Positive'Image(Value) & " (less)} [list green]");
               elsif Value >
                 Player_Ship.Modules(ShipModuleIndex).Fuel_Usage then
                  Insert
                    (ModuleText, "end",
                     "{" & Positive'Image(Value) & " (more)} [list red]");
               else
                  Insert(ModuleText, "end", "{" & Positive'Image(Value) & "}");
               end if;
            else
               Insert(ModuleText, "end", "{" & Positive'Image(MaxValue) & "}");
               Insert
                 (ModuleText, "end",
                  "{" & LF & "Fuel usage:" & Positive'Image(Value) & "}");
            end if;
         when ShipModules.CARGO =>
            Insert(ModuleText, "end", "{" & LF & "Max cargo:}");
            if Installing and then ShipModuleIndex > 0 then
               if MaxValue >
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Max_Value then
                  Insert
                    (ModuleText, "end",
                     "{" & Positive'Image(MaxValue) &
                     " kg (bigger)} [list green]");
               elsif MaxValue <
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Max_Value then
                  Insert
                    (ModuleText, "end",
                     "{" & Positive'Image(MaxValue) &
                     " kg (smaller)} [list red]");
               else
                  Insert
                    (ModuleText, "end",
                     "{" & Positive'Image(MaxValue) & " kg}");
               end if;
            else
               Insert
                 (ModuleText, "end", "{" & Positive'Image(MaxValue) & " kg}");
            end if;
         when CABIN =>
            Insert(ModuleText, "end", "{" & LF & "Quality: }");
            if Installing and then ShipModuleIndex > 0 then
               if MaxValue < 30 then
                  if Player_Ship.Modules(ShipModuleIndex).Quality >
                    MaxValue then
                     Insert(ModuleText, "end", "{minimal (worse)} [list red]");
                  elsif Player_Ship.Modules(ShipModuleIndex).Quality <
                    MaxValue then
                     Insert
                       (ModuleText, "end", "{minimal (better)} [list green]");
                  else
                     Insert(ModuleText, "end", "{minimal}");
                  end if;
               elsif MaxValue < 60 then
                  if Player_Ship.Modules(ShipModuleIndex).Quality >
                    MaxValue then
                     Insert(ModuleText, "end", "{basic (worse)} [list red]");
                  elsif Player_Ship.Modules(ShipModuleIndex).Quality <
                    MaxValue then
                     Insert
                       (ModuleText, "end", "{basic (better)} [list green]");
                  else
                     Insert(ModuleText, "end", "{basic}");
                  end if;
               elsif MaxValue < 80 then
                  if Player_Ship.Modules(ShipModuleIndex).Quality >
                    MaxValue then
                     Insert
                       (ModuleText, "end", "{extended (worse)} [list red]");
                  elsif Player_Ship.Modules(ShipModuleIndex).Quality <
                    MaxValue then
                     Insert
                       (ModuleText, "end", "{extended (better)} [list green]");
                  else
                     Insert(ModuleText, "end", "{extended}");
                  end if;
               else
                  if Player_Ship.Modules(ShipModuleIndex).Quality >
                    MaxValue then
                     Insert(ModuleText, "end", "{luxury (worse) [list red]}");
                  elsif Player_Ship.Modules(ShipModuleIndex).Quality <
                    MaxValue then
                     Insert
                       (ModuleText, "end", "{luxury (better) [list green]}");
                  else
                     Insert(ModuleText, "end", "{luxury}");
                  end if;
               end if;
            else
               if MaxValue < 30 then
                  Insert(ModuleText, "end", "{minimal}");
               elsif MaxValue < 60 then
                  Insert(ModuleText, "end", "{basic}");
               elsif MaxValue < 80 then
                  Insert(ModuleText, "end", "{extended}");
               else
                  Insert(ModuleText, "end", "{luxury}");
               end if;
            end if;
            Insert(ModuleText, "end", "{" & LF & "Max owners:}");
            if Installing and then ShipModuleIndex > 0 then
               if BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Max_Owners >
                 MaxOwners then
                  Insert
                    (ModuleText, "end",
                     "{" & Natural'Image(MaxOwners) & " (less)} [list red]");
               elsif BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Max_Owners <
                 MaxOwners then
                  Insert
                    (ModuleText, "end",
                     "{" & Natural'Image(MaxOwners) & " (more)} [list green]");
               else
                  Insert
                    (ModuleText, "end", "{" & Natural'Image(MaxOwners) & "}");
               end if;
            else
               Insert(ModuleText, "end", "{" & Natural'Image(MaxOwners) & "}");
            end if;
         when ALCHEMY_LAB .. GREENHOUSE =>
            Insert(ModuleText, "end", "{" & LF & "Max workers:}");
            if Installing and then ShipModuleIndex > 0 then
               if BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Max_Owners >
                 MaxOwners then
                  Insert
                    (ModuleText, "end",
                     "{" & Natural'Image(MaxOwners) & " (less)} [list red]");
               elsif BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Max_Owners <
                 MaxOwners then
                  Insert
                    (ModuleText, "end",
                     "{" & Natural'Image(MaxOwners) & " (more)} [list green]");
               else
                  Insert
                    (ModuleText, "end", "{" & Natural'Image(MaxOwners) & "}");
               end if;
            else
               Insert(ModuleText, "end", "{" & Natural'Image(MaxOwners) & "}");
            end if;
         when GUN | HARPOON_GUN =>
            Insert(ModuleText, "end", "{" & LF & "Strength:}");
            if Installing and then ShipModuleIndex > 0 then
               if MType = GUN then
                  if Player_Ship.Modules(ShipModuleIndex).Damage >
                    MaxValue then
                     Insert
                       (ModuleText, "end",
                        "{" & Natural'Image(MaxValue) &
                        " (weaker)} [list red]");
                  elsif Player_Ship.Modules(ShipModuleIndex).Damage <
                    MaxValue then
                     Insert
                       (ModuleText, "end",
                        "{" & Natural'Image(MaxValue) &
                        " (stronger)} [list green]");
                  else
                     Insert
                       (ModuleText, "end",
                        "{" & Natural'Image(MaxValue) & "}");
                  end if;
               else
                  if Player_Ship.Modules(ShipModuleIndex).Duration >
                    MaxValue then
                     Insert
                       (ModuleText, "end",
                        "{" & Natural'Image(MaxValue) &
                        " (weaker)} [list red]");
                  elsif Player_Ship.Modules(ShipModuleIndex).Damage <
                    MaxValue then
                     Insert
                       (ModuleText, "end",
                        "{" & Natural'Image(MaxValue) &
                        " (stronger)} [list green]");
                  else
                     Insert
                       (ModuleText, "end",
                        "{" & Natural'Image(MaxValue) & "}");
                  end if;
               end if;
            else
               Insert(ModuleText, "end", "{" & Natural'Image(MaxValue) & "}");
            end if;
            Insert(ModuleText, "end", "{" & LF & "Ammunition: }");
            Ammunition_Info_Loop :
            for I in
              Objects_Container.First_Index(Container => Items_List) ..
                Objects_Container.Last_Index(Container => Items_List) loop
               if Objects_Container.Element
                   (Container => Items_List, Index => I)
                   .I_Type =
                 TinyString_Formal_Container.Element
                   (Container => Items_Types, Index => Value) then
                  Insert
                    (ModuleText, "end",
                     "{Any" &
                     Slice
                       (Objects_Container.Element
                          (Container => Items_List, Index => I)
                          .Name,
                        Index
                          (Objects_Container.Element
                             (Container => Items_List, Index => I)
                             .Name,
                           " "),
                        Length
                          (Objects_Container.Element
                             (Container => Items_List, Index => I)
                             .Name)) &
                     "}");
                  exit Ammunition_Info_Loop;
               end if;
            end loop Ammunition_Info_Loop;
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
         Insert(ModuleText, "end", "{" & LF & "Size:}");
         if Installing then
            Check_Module_Size_Loop :
            for Module of Player_Ship.Modules loop
               if Module.M_Type = HULL
                 and then Size >
                   BaseModules_Container.Element
                     (Container => Modules_List, Index => Module.Proto_Index)
                     .Value then
                  Insert
                    (ModuleText, "end",
                     "{" & Natural'Image(Size) &
                     " (needs a bigger hull)} [list red]");
                  Added := True;
                  exit Check_Module_Size_Loop;
               end if;
            end loop Check_Module_Size_Loop;
         end if;
         if not Added then
            Insert(ModuleText, "end", "{" & Natural'Image(Size) & "}");
         end if;
      end if;
      if Weight > 0 then
         Insert
           (ModuleText, "end",
            "{" & LF & "Weight:" & Natural'Image(Weight) & " kg}");
         if ShipModuleIndex > 0 then
            if Weight > Player_Ship.Modules(ShipModuleIndex).Weight then
               Insert(ModuleText, "end", "{ (heavier)}");
            elsif Weight < Player_Ship.Modules(ShipModuleIndex).Weight then
               Insert(ModuleText, "end", "{ (lighter)}");
            end if;
         end if;
      end if;
      if Installing then
         Insert(ModuleText, "end", "{" & LF & "Repair/Upgrade material: }");
         MAmount := 0;
         Repair_Materials_Loop :
         for I in
           Objects_Container.First_Index(Container => Items_List) ..
             Objects_Container.Last_Index(Container => Items_List) loop
            if To_String
                (Source =>
                   Objects_Container.Element
                     (Container => Items_List, Index => I)
                     .I_Type) =
              To_String
                (Source =>
                   BaseModules_Container.Element
                     (Container => Modules_List, Index => ModuleIndex)
                     .Repair_Material) then
               if MAmount > 0 then
                  Insert(ModuleText, "end", "{ or }");
               end if;
               Insert
                 (ModuleText, "end",
                  "{" &
                  To_String
                    (Objects_Container.Element
                       (Container => Items_List, Index => I)
                       .Name) &
                  "}");
               MAmount := MAmount + 1;
            end if;
         end loop Repair_Materials_Loop;
         Insert
           (ModuleText, "end",
            "{" & LF & "Repair/Upgrade skill: " &
            To_String
              (SkillsData_Container.Element
                 (Skills_List,
                  BaseModules_Container.Element
                    (Container => Modules_List, Index => ModuleIndex)
                    .Repair_Skill)
                 .Name) &
            "/" &
            To_String
              (AttributesData_Container.Element
                 (Attributes_List,
                  SkillsData_Container.Element
                    (Skills_List,
                     BaseModules_Container.Element
                       (Container => Modules_List, Index => ModuleIndex)
                       .Repair_Skill)
                    .Attribute)
                 .Name) &
            "}");
         if BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .Unique then
            Insert
              (ModuleText, "end",
               "{" & LF &
               "The module is uniquie. Only one module of that type can be installed on the ship.}");
         end if;
         if BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .Description /=
           Short_String.Null_Bounded_String then
            Insert
              (ModuleText, "end",
               "{" & LF & LF &
               To_String
                 (BaseModules_Container.Element
                    (Container => Modules_List, Index => ModuleIndex)
                    .Description) &
               "}");
         end if;
      end if;
   end SetModuleInfo;

   -- ****if* ShipyardUI/ShipyardUI.Set_Install_Button
   -- FUNCTION
   -- Set enabled/disabled state for the install button
   -- PARAMETERS
   -- InstallButton - The button which state will be set
   -- MoneyIndex2   - The index of money in the player's ship's cargo
   -- Cost          - The cost of the module to install
   -- SOURCE
   procedure Set_Install_Button
     (InstallButton: Ttk_Button; MoneyIndex2, Cost: Natural) is
      -- ****
      UsedSpace, AllSpace, MaxSize: Natural;
      Has_Unique: Boolean := False;
      Free_Turret_Index: Natural := 0;
   begin
      Find_Hull_And_Free_Turret_Loop :
      for I in Player_Ship.Modules.Iterate loop
         case Player_Ship.Modules(I).M_Type is
            when HULL =>
               MaxSize :=
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(I).Proto_Index)
                   .Value;
               UsedSpace := Player_Ship.Modules(I).Installed_Modules;
               AllSpace := Player_Ship.Modules(I).Max_Modules;
            when TURRET =>
               if Player_Ship.Modules(I).Gun_Index = 0
                 and then
                   BaseModules_Container.Element
                     (Container => Modules_List,
                      Index => Player_Ship.Modules(I).Proto_Index)
                     .Size >=
                   BaseModules_Container.Element
                     (Container => Modules_List, Index => ModuleIndex)
                     .Size then
                  Free_Turret_Index :=
                    Modules_Container.To_Index(Position => I);
               end if;
            when others =>
               null;
         end case;
      end loop Find_Hull_And_Free_Turret_Loop;
      Check_Unique_Module_Loop :
      for Module of Player_Ship.Modules loop
         if BaseModules_Container.Element
             (Container => Modules_List, Index => Module.Proto_Index)
             .M_Type =
           BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .M_Type and
           BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .Unique then
            Has_Unique := True;
            exit Check_Unique_Module_Loop;
         end if;
      end loop Check_Unique_Module_Loop;
      if MoneyIndex2 = 0 then
         configure(InstallButton, "-state disabled -text {No money}");
         Add(InstallButton, "You don't have any money to buy the module.");
      else
         if Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => MoneyIndex2)
             .Amount <
           Cost then
            configure(InstallButton, "-state disabled -text {No money}");
            Add
              (InstallButton,
               "You don't have enough money to buy the module.");
         elsif Has_Unique then
            configure(InstallButton, "-state disabled -text {Unique}");
            Add
              (InstallButton,
               "Only one module of that type can be installed on the ship.");
         elsif BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .M_Type not in
             GUN | HARPOON_GUN | HULL then
            if BaseModules_Container.Element
                (Container => Modules_List, Index => ModuleIndex)
                .Size >
              MaxSize then
               configure(InstallButton, "-state disabled -text {Too big}");
               Add
                 (InstallButton,
                  "The selected module is too big for your's ship's hull.");
            elsif (AllSpace - UsedSpace) <
              BaseModules_Container.Element
                (Container => Modules_List, Index => ModuleIndex)
                .Size and
              BaseModules_Container.Element
                  (Container => Modules_List, Index => ModuleIndex)
                  .M_Type /=
                ARMOR then
               configure(InstallButton, "-state disabled -text {No space}");
               Add
                 (InstallButton,
                  "You don't have enough space in your ship's hull to install the module.");
            end if;
         elsif BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .M_Type =
           HULL and
           BaseModules_Container.Element
               (Container => Modules_List, Index => ModuleIndex)
               .Max_Value <
             UsedSpace then
            configure(InstallButton, "-state disabled -text {Too small}");
            Add
              (InstallButton,
               "The selected hull is too small to replace your current hull.");
         elsif BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .M_Type in
             GUN | HARPOON_GUN
           and then Free_Turret_Index = 0 then
            configure(InstallButton, "-state disabled -text {No turret}");
            Add
              (InstallButton,
               "You don't have a free turret to install the selected gun.");
         else
            configure(InstallButton, "-state !disabled -text Install");
         end if;
      end if;
   end Set_Install_Button;

   -- ****f* ShipyardUI/ShipyardUI.Show_Install_Info_Command
   -- FUNCTION
   -- Show information about the selected module to install
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Install_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Install_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      use Tiny_String;

      Cost: Positive;
      MoneyIndex2: Natural;
      ModuleDialog: constant Ttk_Frame :=
        Create_Dialog
          (".moduledialog",
           To_String
             (BaseModules_Container.Element
                (Container => Modules_List, Index => ModuleIndex)
                .Name));
      ModuleText: constant Tk_Text :=
        Create(ModuleDialog & ".info", "-height 10 -width 40");
      Frame: constant Ttk_Frame := Create(ModuleDialog & ".buttonbox");
      CloseButton: constant Ttk_Button :=
        Create
          (ModuleDialog & ".buttonbox.button",
           "-text Close -command {CloseDialog " & ModuleDialog & "}");
      InstallButton: constant Ttk_Button :=
        Create
          (ModuleDialog & ".buttonbox.install",
           "-text Install -command {CloseDialog " & ModuleDialog &
           ";ManipulateModule install}");
      Compare_Frame: constant Ttk_Frame := Create(ModuleDialog & ".compare");
      Compare_Box: constant Ttk_ComboBox :=
        Create(Compare_Frame & ".combo", "-state readonly");
      Compare_Label: constant Ttk_Label :=
        Create(Compare_Frame & ".label", "-text {Compare with:}");
      Module_Iterator: Natural := 0;
      Compare_Modules: Unbounded_String := Null_Unbounded_String;
   begin
      for I in Player_Ship.Modules.Iterate loop
         if BaseModules_Container.Element
             (Container => Modules_List,
              Index => Player_Ship.Modules(I).Proto_Index)
             .M_Type =
           BaseModules_Container.Element
             (Container => Modules_List, Index => ModuleIndex)
             .M_Type then
            Module_Iterator := Module_Iterator + 1;
            Append
              (Source => Compare_Modules,
               New_Item =>
                 "{" & To_String(Source => Player_Ship.Modules(I).Name) &
                 "} ");
         end if;
      end loop;
      if Module_Iterator > 1 then
         configure
           (Widgt => Compare_Box,
            options =>
              "-values {" & To_String(Source => Compare_Modules) & "}");
         Current(ComboBox => Compare_Box, NewIndex => "0");
         Tcl.Tk.Ada.Grid.Grid(Compare_Label, "-padx {0 5}");
         Tcl.Tk.Ada.Grid.Grid(Compare_Box, "-row 0 -column 1 -padx {5 0}");
         Tcl.Tk.Ada.Grid.Grid(Compare_Frame, "-pady {0 5}");
         Bind(Compare_Box, "<<ComboboxSelected>>", "{CompareModules}");
      end if;
      Cost :=
        BaseModules_Container.Element
          (Container => Modules_List, Index => ModuleIndex)
          .Price;
      Count_Price(Cost, Find_Member(TALK));
      MoneyIndex2 := Find_Item(Player_Ship.Cargo, Money_Index);
      Tag_Configure(ModuleText, "red", "-foreground red");
      Tag_Configure(ModuleText, "green", "-foreground green");
      SetModuleInfo(True);
      configure
        (ModuleText,
         "-state disabled -height" &
         Positive'Image
           (Positive'Value(Count(ModuleText, "-displaylines", "0.0", "end")) /
            Positive'Value(Metrics("InterfaceFont", "-linespace")) +
            1));
      Tcl.Tk.Ada.Grid.Grid(ModuleText, "-padx 5 -pady {5 0}");
      Tcl.Tk.Ada.Grid.Grid(InstallButton, "-padx {0 5}");
      Set_Install_Button(InstallButton, MoneyIndex2, Cost);
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1 -padx {5 0}");
      Tcl.Tk.Ada.Grid.Grid(Frame, "-pady {0 5}");
      Focus(CloseButton);
      Bind(CloseButton, "<Tab>", "{focus " & InstallButton & ";break}");
      Bind(ModuleDialog, "<Escape>", "{" & CloseButton & " invoke;break}");
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      Show_Dialog
        (Dialog => ModuleDialog, Relative_X => 0.25, Relative_Y => 0.15);
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Manipulate_Module_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
   begin
      if CArgv.Arg(Argv, 1) = "install" then
         Bases.Ship.Upgrade_Ship(True, ModuleIndex);
      else
         Bases.Ship.Upgrade_Ship(False, ModuleIndex);
         Tcl_Eval(Interp, "SortShipyardModules remove 0 {} 10");
      end if;
      Update_Messages;
      return
        Show_Shipyard_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowShipyard" & "0");
   exception
      when Trade_No_Money =>
         Show_Message
           (Text =>
              "You don't have " & To_String(Money_Name) &
              " to pay for modules.",
            Title => "Can't install module");
         return TCL_OK;
      when An_Exception : Trade_Not_Enough_Money =>
         Show_Message
           (Text =>
              "You don't have enough " & To_String(Money_Name) &
              " to pay for " & Exception_Message(An_Exception) & ".",
            Title => "Can't install module");
         return TCL_OK;
      when An_Exception : Bases_Ship_Unique_Module =>
         Show_Message
           (Text =>
              "You can't install another " & Exception_Message(An_Exception) &
              " because you have installed one module that type. Remove old first.",
            Title => "Can't install module");
         return TCL_OK;
      when An_Exception : Bases_Ship_Installation_Error |
        Bases_Ship_Removing_Error =>
         Show_Message
           (Text => Exception_Message(An_Exception),
            Title =>
              "Can't" &
              (if CArgv.Arg(Argv, 1) = "install" then "install"
               else "remove") &
              " module");
         return TCL_OK;
      when Trade_No_Free_Cargo =>
         Show_Message
           (Text =>
              "You don't have enough free space for " & To_String(Money_Name) &
              " in ship cargo.",
            Title => "Can't remove module");
         return TCL_OK;
      when Trade_No_Money_In_Base =>
         Show_Message
           (Text =>
              "Base don't have enough " & To_String(Money_Name) &
              " for buy this module.",
            Title => "Can't remove module");
         return TCL_OK;
   end Manipulate_Module_Command;

   -- ****f* ShipyardUI/ShipyardUI.Show_Remove_Info_Command
   -- FUNCTION
   -- Show information about the selected module to remove
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- SOURCE
   function Show_Remove_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Remove_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      use Short_String;
      use Tiny_String;

      Cost: Natural;
      Damage: Float;
      ShipModuleIndex: constant Natural := ModuleIndex;
      ModuleDialog: constant Ttk_Frame :=
        Create_Dialog
          (".moduledialog",
           To_String(Player_Ship.Modules(ShipModuleIndex).Name));
      DamageBar: constant Ttk_ProgressBar := Create(ModuleDialog & ".damage");
      ModuleText: constant Tk_Text :=
        Create(ModuleDialog & ".info", "-height 10 -width 40");
      Label: Ttk_Label := Create(ModuleDialog & ".damagelbl");
      RemoveButton, CloseButton: Ttk_Button;
      Frame: constant Ttk_Frame := Create(ModuleDialog & ".buttonbox");
   begin
      Tcl.Tk.Ada.Busy.Busy(Game_Header);
      Tcl.Tk.Ada.Busy.Busy(Main_Paned);
      Damage :=
        1.0 -
        Float(Player_Ship.Modules(ShipModuleIndex).Durability) /
          Float(Player_Ship.Modules(ShipModuleIndex).Max_Durability);
      Cost :=
        BaseModules_Container.Element
          (Container => Modules_List,
           Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
          .Price -
        Integer
          (Float
             (BaseModules_Container.Element
                (Container => Modules_List,
                 Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                .Price) *
           Damage);
      if Cost = 0 then
         Cost := 1;
      end if;
      Count_Price(Cost, Find_Member(TALK), False);
      Tcl.Tk.Ada.Grid.Grid(ModuleText, "-padx 5 -pady {5 0}");
      configure(ModuleText, "-state normal");
      Delete(ModuleText, "1.0", "end");
      Insert
        (ModuleText, "end",
         "{Remove gain:" & Positive'Image(Cost) & LF & "Removing time:" &
         Positive'Image
           (BaseModules_Container.Element
              (Container => Modules_List,
               Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
              .Install_Time) &
         " minutes}");
      SetModuleInfo(False);
      if Damage > 0.0 then
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
      if BaseModules_Container.Element
          (Container => Modules_List,
           Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
          .Description /=
        Short_String.Null_Bounded_String then
         Label :=
           Create
             (ModuleDialog & ".description",
              "-text {" & LF &
              To_String
                (BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(ShipModuleIndex).Proto_Index)
                   .Description) &
              "} -wraplength 450");
         Tcl.Tk.Ada.Grid.Grid(Label, "-sticky w -padx 5");
      end if;
      configure
        (ModuleText,
         "-state disabled -height" &
         Positive'Image
           (Positive'Value(Count(ModuleText, "-displaylines", "0.0", "end")) /
            Positive'Value(Metrics("InterfaceFont", "-linespace")) +
            1));
      RemoveButton :=
        Create
          (ModuleDialog & ".buttonbox.install",
           "-text Remove -command {CloseDialog " & ModuleDialog &
           ";ManipulateModule remove}");
      Tcl.Tk.Ada.Grid.Grid(RemoveButton, "-padx {0 5}");
      CloseButton :=
        Create
          (ModuleDialog & ".buttonbox.button",
           "-text Close -command {CloseDialog " & ModuleDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1 -padx {5 0}");
      Tcl.Tk.Ada.Grid.Grid(Frame, "-pady {0 5}");
      Focus(CloseButton);
      Bind(CloseButton, "<Tab>", "{focus " & RemoveButton & ";break}");
      Bind(ModuleDialog, "<Escape>", "{" & CloseButton & " invoke;break}");
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      Show_Dialog(Dialog => ModuleDialog, Relative_Y => 0.2);
      return TCL_OK;
   end Show_Remove_Info_Command;

   -- ****o* ShipyardUI/ShipyardUI.Show_Module_Menu_Command
   -- FUNCTION
   -- Show menu with actions for the selected module
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowModuleMenu moduleindex actiontype
   -- ModuleIndex is a index of the module which menu will be shown,
   -- actiontype is action related to the module. Can be install or remove.
   -- SOURCE
   function Show_Module_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Module_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      use Tiny_String;

      Module_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".modulemenu", Title => "Module actions",
           Parent_Name => ".");
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Module_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Module_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Module_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Module_Menu & ".info;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      ModuleIndex := Natural'Value(CArgv.Arg(Argv, 1));
      if CArgv.Arg(Argv, 2) = "install" then
         Change_Title
           (Module_Menu,
            To_String
              (BaseModules_Container.Element
                 (Container => Modules_List, Index => ModuleIndex)
                 .Name) &
            " actions");
         Add_Button
           (Name => ".info", Label => "Show module details",
            Command => "ShowInstallInfo");
         Add_Button
           (Name => ".install", Label => "Install module",
            Command => "ManipulateModule install");
         Set_Install_Button_Block :
         declare
            Cost: Positive;
            MoneyIndex2: Natural := 0;
            Button: constant Ttk_Button :=
              Get_Widget(Module_Menu & ".install");
         begin
            Cost :=
              BaseModules_Container.Element
                (Container => Modules_List, Index => ModuleIndex)
                .Price;
            Count_Price(Cost, Find_Member(TALK));
            MoneyIndex2 := Find_Item(Player_Ship.Cargo, Money_Index);
            Set_Install_Button(Button, MoneyIndex2, Cost);
         end Set_Install_Button_Block;
      else
         Change_Title
           (Module_Menu,
            To_String(Player_Ship.Modules(ModuleIndex).Name) & " actions");
         Add_Button
           (Name => ".info", Label => "Show module details",
            Command => "ShowRemoveInfo");
         Add_Button
           (Name => ".install", Label => "Remove module",
            Command => "ManipulateModule remove");
      end if;
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Module_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Module_Menu_Command;

   -- ****o* ShipyardUI/ShipyardUI.Show_Shipyard_Tab_Command
   -- FUNCTION
   -- Show the install or remove modules options in shipyard
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowShipyardTab
   -- SOURCE
   function Show_Shipyard_Tab_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Shipyard_Tab_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argv);
      ShipyardCanvas: constant Tk_Canvas :=
        Get_Widget(Main_Paned & ".shipyardframe.canvas", Interp);
      ShipyardFrame: constant Ttk_Frame :=
        Get_Widget(ShipyardCanvas & ".shipyard");
      Frame: Ttk_Frame;
   begin
      if Tcl_GetVar(Interp, "newtab") = "install" then
         Frame := Get_Widget(ShipyardFrame & ".remove");
         Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
         Frame := Get_Widget(ShipyardFrame & ".install");
         Tcl.Tk.Ada.Grid.Grid(Frame);
      else
         Frame := Get_Widget(ShipyardFrame & ".install");
         Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
         Frame := Get_Widget(ShipyardFrame & ".remove");
         Tcl.Tk.Ada.Grid.Grid(Frame);
      end if;
      Delete(ShipyardCanvas, "all");
      Canvas_Create
        (ShipyardCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(ShipyardFrame));
      Tcl_Eval(Interp, "update");
      configure
        (ShipyardCanvas,
         "-scrollregion [list " & BBox(ShipyardCanvas, "all") & "]");
      Tcl_SetResult(Interp, "1");
      if Argc = 1 then
         return
           Show_Shipyard_Command
             (ClientData, Interp, 2, CArgv.Empty & "ShowShipyard" & "0");
      else
         return TCL_OK;
      end if;
   end Show_Shipyard_Tab_Command;

   -- ****it* ShipyardUI/ShipyardUI.Modules_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the ship modules list
   -- OPTIONS
   -- NAMEASC      - Sort modules by name ascending
   -- NAMEDESC     - Sort modules by name descending
   -- TYPEASC      - Sort modules by type ascending
   -- TYPEDESC     - Sort modules by type descending
   -- SIZEASC      - Sort modules by size ascending
   -- SIZEDESC     - Sort modules by size descending
   -- MATERIALASC  - Sort modules by material ascending
   -- MATERIALDESC - Sort modules by material descending
   -- PRICEASC     - Sort modules by price ascending
   -- PRICEDESC    - Sort modules by price descending
   -- NONE       - No sorting modules (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Modules_Sort_Orders is
     (NAMEASC, NAMEDESC, TYPEASC, TYPEDESC, SIZEASC, SIZEDESC, MATERIALASC,
      MATERIALDESC, PRICEASC, PRICEDESC, NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* ShipyardUI/ShipyardUI.Default_Modules_Sort_Order
      -- FUNCTION
      -- Default sorting order for the player's ship's modules
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Modules_Sort_Order: constant Modules_Sort_Orders := NONE;
   -- ****

   -- ****iv* ShipyardUI/ShipyardUI.Modules_Sort_Order
   -- FUNCTION
   -- The current sorting order for modules list
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Modules_Sort_Order: Modules_Sort_Orders := Default_Modules_Sort_Order;
   -- ****

   -- ****o* ShipyardUI/ShipyardUI.Sort_Modules_Command
   -- FUNCTION
   -- Sort the ship modules lists
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortShipModules action moduletype page x
   -- Action is a type of action, can be install or remove, moduletype is a
   -- type of modules to show, page is the number of currently showed page
   -- of list and x is X axis coordinate where the player clicked the mouse
   -- button
   -- SOURCE
   function Sort_Modules_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Modules_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number
          ((if CArgv.Arg(Argv, 1) = "install" then InstallTable
            else RemoveTable),
           Natural'Value(CArgv.Arg(Argv, 4)));
      type Local_Module_Data is record
         Name: Bounded_String;
         MType: Unbounded_String;
         Size: Natural;
         Material: Bounded_String;
         Price: Positive;
         Id: Positive;
      end record;
      type Modules_Array is array(Positive range <>) of Local_Module_Data;
      Local_Modules: Modules_Array
        (1 ..
             Positive
               ((if CArgv.Arg(Argv, 1) = "install" then
                   BaseModules_Container.Length(Container => Modules_List)
                 else Player_Ship.Modules.Length)));
      Index: Positive := 1;
      Cost: Natural;
      Damage: Float;
      function "<"(Left, Right: Local_Module_Data) return Boolean is
      begin
         if Modules_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Modules_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Modules_Sort_Order = TYPEASC and then Left.MType < Right.MType then
            return True;
         end if;
         if Modules_Sort_Order = TYPEDESC
           and then Left.MType > Right.MType then
            return True;
         end if;
         if Modules_Sort_Order = SIZEASC and then Left.Size < Right.Size then
            return True;
         end if;
         if Modules_Sort_Order = SIZEDESC and then Left.Size > Right.Size then
            return True;
         end if;
         if Modules_Sort_Order = MATERIALASC
           and then Left.Material < Right.Material then
            return True;
         end if;
         if Modules_Sort_Order = MATERIALDESC
           and then Left.Material > Right.Material then
            return True;
         end if;
         if Modules_Sort_Order = PRICEASC
           and then Left.Price < Right.Price then
            return True;
         end if;
         if Modules_Sort_Order = PRICEDESC
           and then Left.Price > Right.Price then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Modules is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Module_Data,
         Array_Type => Modules_Array);
   begin
      case Column is
         when 1 =>
            if Modules_Sort_Order = NAMEASC then
               Modules_Sort_Order := NAMEDESC;
            else
               Modules_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Modules_Sort_Order = TYPEASC then
               Modules_Sort_Order := TYPEDESC;
            else
               Modules_Sort_Order := TYPEASC;
            end if;
         when 3 =>
            if Modules_Sort_Order = SIZEASC then
               Modules_Sort_Order := SIZEDESC;
            else
               Modules_Sort_Order := SIZEASC;
            end if;
         when 4 =>
            if Modules_Sort_Order = MATERIALASC then
               Modules_Sort_Order := MATERIALDESC;
            else
               Modules_Sort_Order := MATERIALASC;
            end if;
         when 5 =>
            if Modules_Sort_Order = PRICEASC then
               Modules_Sort_Order := PRICEDESC;
            else
               Modules_Sort_Order := PRICEASC;
            end if;
         when others =>
            null;
      end case;
      if Modules_Sort_Order = NONE then
         return TCL_OK;
      end if;
      if CArgv.Arg(Argv, 1) = "install" then
         for I in
           BaseModules_Container.First_Index(Container => Modules_List) ..
             BaseModules_Container.Last_Index(Container => Modules_List) loop
            Cost :=
              BaseModules_Container.Element
                (Container => Modules_List, Index => I)
                .Price;
            Count_Price(Cost, Find_Member(TALK));
            if Cost = 0 then
               Cost := 1;
            end if;
            Local_Modules(Index) :=
              (Name =>
                 BaseModules_Container.Element
                   (Container => Modules_List, Index => I)
                   .Name,
               MType => To_Unbounded_String(Get_Module_Type(I)),
               Size =>
                 (if
                    BaseModules_Container.Element
                      (Container => Modules_List, Index => I)
                      .M_Type =
                    HULL
                  then
                    BaseModules_Container.Element
                      (Container => Modules_List, Index => I)
                      .Max_Value
                  else BaseModules_Container.Element
                      (Container => Modules_List, Index => I)
                      .Size),
               Material =>
                 BaseModules_Container.Element
                   (Container => Modules_List, Index => I)
                   .Repair_Material,
               Price => Cost, Id => I);
            Index := Index + 1;
         end loop;
      else
         for I in Player_Ship.Modules.Iterate loop
            Damage :=
              1.0 -
              Float(Player_Ship.Modules(I).Durability) /
                Float(Player_Ship.Modules(I).Max_Durability);
            Cost :=
              BaseModules_Container.Element
                (Container => Modules_List,
                 Index => Player_Ship.Modules(I).Proto_Index)
                .Price -
              Integer
                (Float
                   (BaseModules_Container.Element
                      (Container => Modules_List,
                       Index => Player_Ship.Modules(I).Proto_Index)
                      .Price) *
                 Damage);
            if Cost = 0 then
               Cost := 1;
            end if;
            Count_Price(Cost, Find_Member(TALK), False);
            Local_Modules(Index) :=
              (Name =>
                 To_Bounded_String
                   (Source =>
                      To_String(Source => Player_Ship.Modules(I).Name)),
               MType =>
                 To_Unbounded_String
                   (Get_Module_Type(Player_Ship.Modules(I).Proto_Index)),
               Size =>
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(I).Proto_Index)
                   .Size,
               Material =>
                 BaseModules_Container.Element
                   (Container => Modules_List,
                    Index => Player_Ship.Modules(I).Proto_Index)
                   .Repair_Material,
               Price => Cost, Id => Modules_Container.To_Index(I));
            Index := Index + 1;
         end loop;
      end if;
      Sort_Modules(Local_Modules);
      if CArgv.Arg(Argv, 1) = "install" then
         Install_Indexes.Clear;
         for Module of Local_Modules loop
            Install_Indexes.Append(Module.Id);
         end loop;
      else
         Remove_Indexes.Clear;
         for Module of Local_Modules loop
            Remove_Indexes.Append(Module.Id);
         end loop;
      end if;
      return
        Show_Shipyard_Command
          (ClientData, Interp, 3,
           CArgv.Empty & "ShowShipyard" & CArgv.Arg(Argv, 2) &
           CArgv.Arg(Argv, 3));
   end Sort_Modules_Command;

   -- ****o* ShipyardUI/ShipyardUI.Compare_Modules_Command
   -- FUNCTION
   -- Show the comparison between the selected modules in install info
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CompareModules
   -- SOURCE
   function Compare_Modules_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Compare_Modules_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
   begin
      SetModuleInfo(True);
      return TCL_OK;
   end Compare_Modules_Command;

   procedure AddCommands is
   begin
      Add_Command("ShowShipyard", Show_Shipyard_Command'Access);
      Add_Command("ShowInstallInfo", Show_Install_Info_Command'Access);
      Add_Command("ManipulateModule", Manipulate_Module_Command'Access);
      Add_Command("ShowRemoveInfo", Show_Remove_Info_Command'Access);
      Add_Command("ShowShipyardModuleMenu", Show_Module_Menu_Command'Access);
      Add_Command("ShowShipyardTab", Show_Shipyard_Tab_Command'Access);
      Add_Command("SortShipyardModules", Sort_Modules_Command'Access);
      Add_Command("CompareModules", Compare_Modules_Command'Access);
   end AddCommands;

end Bases.ShipyardUI;
