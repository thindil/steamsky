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
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases.Cargo; use Bases.Cargo;
with BasesTypes; use BasesTypes;
with CoreUI; use CoreUI;
with Events; use Events;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Bases.LootUI is

   -- ****iv* LUI/LUI.LootTable
   -- FUNCTION
   -- Table with info about the available items to loot
   -- SOURCE
   LootTable: Table_Widget (5);
   -- ****

   -- ****o* LUI/LUI.Show_Loot_Command
   -- FUNCTION
   -- Show information about looting
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLoot
   -- SOURCE
   function Show_Loot_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Loot_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      LootFrame: Ttk_Frame := Get_Widget(Main_Paned & ".lootframe", Interp);
      LootCanvas: constant Tk_Canvas :=
        Get_Widget(LootFrame & ".canvas", Interp);
      Label: constant Ttk_Label :=
        Get_Widget(LootCanvas & ".loot.options.typelabel", Interp);
      ItemDurability, ItemType, ProtoIndex, ItemName: Unbounded_String;
      ItemsTypes: Unbounded_String := To_Unbounded_String("All");
      ComboBox: Ttk_ComboBox;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargo: BaseCargo_Container.Vector;
      BaseCargoIndex, BaseAmount: Natural;
      IndexesList: Positive_Container.Vector;
      Page: constant Positive :=
        (if Argc = 3 then Positive'Value(CArgv.Arg(Argv, 2)) else 1);
      Start_Row: constant Positive := ((Page - 1) * 25) + 1;
      Current_Row: Positive := 1;
      Arguments: constant String :=
        (if Argc > 1 then "{" & CArgv.Arg(Argv, 1) & "}" else "All");
   begin
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "loot.tcl");
         Bind(LootFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
         LootFrame := Get_Widget(LootCanvas & ".loot");
         LootTable :=
           CreateTable
             (Widget_Image(LootFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Type"),
               To_Unbounded_String("Durability"), To_Unbounded_String("Owned"),
               To_Unbounded_String("Available")),
              Get_Widget(".gameframe.paned.lootframe.scrolly", Interp));
      elsif Winfo_Get(Label, "ismapped") = "1" and Argc = 1 then
         Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp trade}");
      LootFrame.Name := New_String(LootCanvas & ".loot");
      ComboBox := Get_Widget(LootFrame & ".options.type", Interp);
      BaseCargo := SkyBases(BaseIndex).Cargo;
      ClearTable(LootTable);
      Add_Player_Cargo_Loop :
      for I in PlayerShip.Cargo.Iterate loop
         ProtoIndex := PlayerShip.Cargo(I).ProtoIndex;
         BaseCargoIndex :=
           FindBaseCargo(ProtoIndex, PlayerShip.Cargo(I).Durability);
         if BaseCargoIndex > 0 then
            IndexesList.Append(New_Item => BaseCargoIndex);
         end if;
         ItemType :=
           (if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
              Items_List(ProtoIndex).IType
            else Items_List(ProtoIndex).ShowType);
         if Index(ItemsTypes, To_String("{" & ItemType & "}")) = 0 then
            Append(ItemsTypes, " {" & ItemType & "}");
         end if;
         if Argc > 1 and then CArgv.Arg(Argv, 1) /= "All"
           and then To_String(ItemType) /= CArgv.Arg(Argv, 1) then
            goto End_Of_Cargo_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Cargo_Loop;
         end if;
         ItemName :=
           To_Unbounded_String(GetItemName(PlayerShip.Cargo(I), False, False));
         AddButton
           (LootTable, To_String(ItemName), "Show available options for item",
            "ShowLootItemMenu" &
            Positive'Image(Inventory_Container.To_Index(I)),
            1);
         AddButton
           (LootTable, To_String(ItemType), "Show available options for item",
            "ShowLootItemMenu" &
            Positive'Image(Inventory_Container.To_Index(I)),
            2);
         ItemDurability :=
           (if PlayerShip.Cargo(I).Durability < 100 then
              To_Unbounded_String
                (GetItemDamage(PlayerShip.Cargo(I).Durability))
            else To_Unbounded_String("Unused"));
         AddProgressBar
           (LootTable, PlayerShip.Cargo(I).Durability, Default_Item_Durability,
            To_String(ItemDurability),
            "ShowLootItemMenu" &
            Positive'Image(Inventory_Container.To_Index(I)),
            3);
         AddButton
           (LootTable, Natural'Image(PlayerShip.Cargo(I).Amount),
            "Show available options for item",
            "ShowLootItemMenu" &
            Positive'Image(Inventory_Container.To_Index(I)),
            4);
         BaseAmount :=
           (if BaseCargoIndex > 0 then
              SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount
            else 0);
         AddButton
           (LootTable, Natural'Image(BaseAmount),
            "Show available options for item",
            "ShowLootItemMenu" &
            Positive'Image(Inventory_Container.To_Index(I)),
            5, True);
         exit Add_Player_Cargo_Loop when LootTable.Row = 26;
         <<End_Of_Cargo_Loop>>
      end loop Add_Player_Cargo_Loop;
      Add_Base_Cargo_Loop :
      for I in BaseCargo.First_Index .. BaseCargo.Last_Index loop
         exit Add_Base_Cargo_Loop when LootTable.Row = 26;
         if IndexesList.Find_Index(Item => I) > 0 then
            goto End_Of_Base_Cargo_Loop;
         end if;
         ProtoIndex := BaseCargo(I).ProtoIndex;
         ItemType :=
           (if Items_List(ProtoIndex).ShowType = Null_Unbounded_String then
              Items_List(ProtoIndex).IType
            else Items_List(ProtoIndex).ShowType);
         if Index(ItemsTypes, To_String("{" & ItemType & "}")) = 0 then
            Append(ItemsTypes, " {" & ItemType & "}");
         end if;
         if Argc = 2 and then CArgv.Arg(Argv, 1) /= "All"
           and then To_String(ItemType) /= CArgv.Arg(Argv, 1) then
            goto End_Of_Base_Cargo_Loop;
         end if;
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Base_Cargo_Loop;
         end if;
         ItemName := Items_List(ProtoIndex).Name;
         AddButton
           (LootTable, To_String(ItemName), "Show available options for item",
            "ShowLootItemMenu " & Integer'Image(-(I)), 1);
         AddButton
           (LootTable, To_String(ItemType), "Show available options for item",
            "ShowLootItemMenu" & Integer'Image(-(I)), 2);
         ItemDurability :=
           (if BaseCargo(I).Durability < 100 then
              To_Unbounded_String(GetItemDamage(BaseCargo(I).Durability))
            else To_Unbounded_String("Unused"));
         AddProgressBar
           (LootTable, BaseCargo(I).Durability, Default_Item_Durability,
            To_String(ItemDurability),
            "ShowLootItemMenu" & Integer'Image(-(I)), 3);
         AddButton
           (LootTable, "0", "Show available options for item",
            "ShowLootItemMenu" & Integer'Image(-(I)), 4);
         BaseAmount := SkyBases(BaseIndex).Cargo(I).Amount;
         AddButton
           (LootTable, Natural'Image(BaseAmount),
            "Show available options for item",
            "ShowLootItemMenu" & Integer'Image(-(I)), 5, True);
         <<End_Of_Base_Cargo_Loop>>
      end loop Add_Base_Cargo_Loop;
      if Page > 1 then
         if LootTable.Row < 26 then
            AddPagination
              (LootTable, "ShowLoot " & Arguments & Positive'Image(Page - 1),
               "");
         else
            AddPagination
              (LootTable, "ShowLoot " & Arguments & Positive'Image(Page - 1),
               "ShowLoot " & Arguments & Positive'Image(Page + 1));
         end if;
      elsif LootTable.Row = 26 then
         AddPagination
           (LootTable, "", "ShowLoot " & Arguments & Positive'Image(Page + 1));
      end if;
      UpdateTable(LootTable);
      Tcl_Eval(Get_Context, "update");
      configure
        (LootTable.Canvas,
         "-scrollregion [list " & BBox(LootTable.Canvas, "all") & "]");
      configure(ComboBox, "-values [list " & To_String(ItemsTypes) & "]");
      if Argc = 1 then
         Current(ComboBox, "0");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      configure
        (LootCanvas,
         "-height [expr " & SashPos(Main_Paned, "0") & " - 20] -width " &
         cget(Main_Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (LootCanvas, "window", "0 0 -anchor nw -window " & LootFrame);
      Tcl_Eval(Get_Context, "update");
      configure
        (LootCanvas, "-scrollregion [list " & BBox(LootCanvas, "all") & "]");
      Xview_Move_To(LootCanvas, "0.0");
      Yview_Move_To(LootCanvas, "0.0");
      ShowScreen("lootframe");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Loot_Command;

   -- ****if* LUI/LUI.ItemIndex
   -- FUNCTION
   -- Index of the currently selected item
   -- SOURCE
   ItemIndex: Integer;
   -- ****

   -- ****o* TUI/Show_Trade_Loot_Info_Command
   -- FUNCTION
   -- Show information about the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLootItemInfo
   -- SOURCE
   function Show_Loot_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Loot_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      ItemInfo, ProtoIndex: Unbounded_String;
      CargoIndex, BaseCargoIndex: Natural := 0;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      ItemTypes: constant array(1 .. 6) of Unbounded_String :=
        (Weapon_Type, Chest_Armor, Head_Armor, Arms_Armor, Legs_Armor,
         Shield_Type);
   begin
      if ItemIndex < 0 then
         BaseCargoIndex := abs (ItemIndex);
      else
         CargoIndex := ItemIndex;
      end if;
      if CargoIndex > Natural(PlayerShip.Cargo.Length) or
        BaseCargoIndex > Natural(SkyBases(BaseIndex).Cargo.Length) then
         return TCL_OK;
      end if;
      ProtoIndex :=
        (if CargoIndex > 0 then PlayerShip.Cargo(CargoIndex).ProtoIndex
         else SkyBases(BaseIndex).Cargo(BaseCargoIndex).ProtoIndex);
      Append
        (ItemInfo,
         "Weight:" & Integer'Image(Items_List(ProtoIndex).Weight) & " kg");
      if Items_List(ProtoIndex).IType = Weapon_Type then
         Append
           (ItemInfo,
            LF & "Skill: " &
            To_String(Skills_List(Items_List(ProtoIndex).Value(3)).Name) &
            "/" &
            To_String
              (Attributes_List
                 (Skills_List(Items_List(ProtoIndex).Value(3)).Attribute)
                 .Name));
         if Items_List(ProtoIndex).Value(4) = 1 then
            Append(ItemInfo, LF & "Can be used with shield.");
         else
            Append
              (ItemInfo,
               LF & "Can't be used with shield (two-handed weapon).");
         end if;
         Append(ItemInfo, LF & "Damage type: ");
         case Items_List(ProtoIndex).Value(5) is
            when 1 =>
               Append(ItemInfo, "cutting");
            when 2 =>
               Append(ItemInfo, "impaling");
            when 3 =>
               Append(ItemInfo, "blunt");
            when others =>
               null;
         end case;
      end if;
      Show_Weapon_Info_Loop :
      for ItemType of ItemTypes loop
         if Items_List(ProtoIndex).IType = ItemType then
            Append
              (ItemInfo,
               LF & "Damage chance: " &
               GetItemChanceToDamage(Items_List(ProtoIndex).Value(1)));
            Append
              (ItemInfo,
               LF & "Strength:" &
               Integer'Image(Items_List(ProtoIndex).Value(2)));
            exit Show_Weapon_Info_Loop;
         end if;
      end loop Show_Weapon_Info_Loop;
      if Tools_List.Contains(Items_List(ProtoIndex).IType) then
         Append
           (ItemInfo,
            LF & "Damage chance: " &
            GetItemChanceToDamage(Items_List(ProtoIndex).Value(1)));
      end if;
      if Length(Items_List(ProtoIndex).IType) > 4
        and then
        (Slice(Items_List(ProtoIndex).IType, 1, 4) = "Ammo" or
         Items_List(ProtoIndex).IType = To_Unbounded_String("Harpoon")) then
         Append
           (ItemInfo,
            LF & "Strength:" & Integer'Image(Items_List(ProtoIndex).Value(1)));
      end if;
      if Items_List(ProtoIndex).Description /= Null_Unbounded_String then
         Append
           (ItemInfo, LF & LF & To_String(Items_List(ProtoIndex).Description));
      end if;
      ShowInfo(To_String(ItemInfo));
      return TCL_OK;
   end Show_Loot_Item_Info_Command;

   -- ****o* LUI/LUI.Loot_Item_Command
   -- FUNCTION
   -- Take or drop the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- LootItem actiontype
   -- actiontype can be: drop, dropall, take, takeall
   -- SOURCE
   function Loot_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Loot_Item_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseCargoIndex, CargoIndex: Natural := 0;
      Amount: Natural;
      ProtoIndex: Unbounded_String;
      AmountBox: constant Ttk_SpinBox :=
        Get_Widget(".itemdialog.amount", Interp);
      TypeBox: constant Ttk_ComboBox :=
        Get_Widget(Main_Paned & ".lootframe.canvas.loot.options.type", Interp);
   begin
      if ItemIndex < 0 then
         BaseCargoIndex := abs (ItemIndex);
      else
         CargoIndex := ItemIndex;
      end if;
      if CargoIndex > 0 then
         ProtoIndex := PlayerShip.Cargo(CargoIndex).ProtoIndex;
         if BaseCargoIndex = 0 then
            BaseCargoIndex := FindBaseCargo(ProtoIndex);
         end if;
      else
         ProtoIndex := SkyBases(BaseIndex).Cargo(BaseCargoIndex).ProtoIndex;
      end if;
      if CArgv.Arg(Argv, 1) in "drop" | "dropall" then
         Amount :=
           (if CArgv.Arg(Argv, 1) = "drop" then Positive'Value(Get(AmountBox))
            else PlayerShip.Cargo(CargoIndex).Amount);
         if BaseCargoIndex > 0 then
            UpdateBaseCargo
              (CargoIndex => BaseCargoIndex, Amount => Amount,
               Durability => PlayerShip.Cargo.Element(CargoIndex).Durability);
         else
            UpdateBaseCargo
              (ProtoIndex, Amount,
               PlayerShip.Cargo.Element(CargoIndex).Durability);
         end if;
         UpdateCargo
           (Ship => PlayerShip, CargoIndex => CargoIndex,
            Amount => (0 - Amount),
            Durability => PlayerShip.Cargo.Element(CargoIndex).Durability);
         AddMessage
           ("You drop" & Positive'Image(Amount) & " " &
            To_String(Items_List(ProtoIndex).Name) & ".",
            OrderMessage);
      else
         Amount :=
           (if CArgv.Arg(Argv, 1) = "take" then Positive'Value(Get(AmountBox))
            else SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount);
         if FreeCargo(0 - (Amount * Items_List(ProtoIndex).Weight)) < 0 then
            ShowMessage
              ("You can't take that much " &
               To_String(Items_List(ProtoIndex).Name) & ".");
            return TCL_OK;
         end if;
         if CargoIndex > 0 then
            UpdateCargo
              (Ship => PlayerShip, CargoIndex => CargoIndex, Amount => Amount,
               Durability =>
                 SkyBases(BaseIndex).Cargo(BaseCargoIndex).Durability);
         else
            UpdateCargo
              (PlayerShip, ProtoIndex, Amount,
               SkyBases(BaseIndex).Cargo(BaseCargoIndex).Durability);
         end if;
         UpdateBaseCargo
           (CargoIndex => BaseCargoIndex, Amount => (0 - Amount),
            Durability =>
              SkyBases(BaseIndex).Cargo.Element(BaseCargoIndex).Durability);
         AddMessage
           ("You took" & Positive'Image(Amount) & " " &
            To_String(Items_List(ProtoIndex).Name) & ".",
            OrderMessage);
      end if;
      if CArgv.Arg(Argv, 1) in "take" | "drop" then
         if Close_Dialog_Command
             (ClientData, Interp, 2,
              CArgv.Empty & "CloseDialog" & ".itemdialog") =
           TCL_ERROR then
            return TCL_ERROR;
         end if;
      end if;
      UpdateHeader;
      UpdateMessages;
      return Show_Loot_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowLoot" & Get(TypeBox));
   end Loot_Item_Command;

   -- ****o* LUI/LUI.Show_Module_Menu_Command
   -- FUNCTION
   -- Show menu with actions for the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowLootItemMenu itemindex
   -- ItemIndex is a index of the item which menu will be shown.
   -- SOURCE
   function Show_Item_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Item_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ItemMenu: Tk_Menu := Get_Widget(".itemmenu", Interp);
      BaseCargoIndex, CargoIndex: Natural := 0;
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      ItemIndex := Integer'Value(CArgv.Arg(Argv, 1));
      if ItemIndex < 0 then
         BaseCargoIndex := abs (ItemIndex);
      else
         CargoIndex := ItemIndex;
      end if;
      if CargoIndex > 0 and then BaseCargoIndex = 0 then
         BaseCargoIndex :=
           FindBaseCargo(PlayerShip.Cargo(CargoIndex).ProtoIndex);
      end if;
      if Winfo_Get(ItemMenu, "exists") = "0" then
         ItemMenu := Create(".itemmenu", "-tearoff false");
      end if;
      Delete(ItemMenu, "0", "end");
      if BaseCargoIndex > 0 then
         Menu.Add
           (ItemMenu, "command",
            "-label {Take selected amount} -command {LootAmount take" &
            Natural'Image(SkyBases(BaseIndex).Cargo(BaseCargoIndex).Amount) &
            "}");
         Menu.Add
           (ItemMenu, "command",
            "-label {Take all available} -command {LootItem takeall}");
      end if;
      if CargoIndex > 0 then
         Menu.Add
           (ItemMenu, "command",
            "-label {Drop selected amount} -command {LootAmount drop" &
            Natural'Image(PlayerShip.Cargo(CargoIndex).Amount) & "}");
         Menu.Add
           (ItemMenu, "command",
            "-label {Drop all owned} -command {LootItem dropall}");
      end if;
      Menu.Add
        (ItemMenu, "command",
         "-label {Show item details} -command {ShowLootItemInfo}");
      Tk_Popup
        (ItemMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
         Winfo_Get(Get_Main_Window(Interp), "pointery"));
      return TCL_OK;
   end Show_Item_Menu_Command;

   -- ****o* LUI/Loot_Amount_Command
   -- FUNCTION
   -- Show dialog to enter amount of items to drop or take
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- LootAmount action baseindex
   -- Action which will be taken. Can be take or drop. BaseIndex is the index
   -- of the base from which item will be take.
   -- SOURCE
   function Loot_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Loot_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      BaseIndex: constant Natural :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      if CArgv.Arg(Argv, 1) = "drop" then
         ShowManipulateItem
           ("Drop " & GetItemName(PlayerShip.Cargo(ItemIndex)),
            "LootItem drop", "drop", ItemIndex);
      else
         if ItemIndex > 0 then
            ShowManipulateItem
              ("Take " & GetItemName(PlayerShip.Cargo(ItemIndex)),
               "LootItem take", "take", ItemIndex,
               Natural'Value(CArgv.Arg(Argv, 2)));
         else
            ShowManipulateItem
              ("Take " &
               To_String
                 (Items_List
                    (SkyBases(BaseIndex).Cargo(abs (ItemIndex)).ProtoIndex)
                    .Name),
               "LootItem take", "take", abs (ItemIndex),
               Natural'Value(CArgv.Arg(Argv, 2)));
         end if;
      end if;
      return TCL_OK;
   end Loot_Amount_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowLoot", Show_Loot_Command'Access);
      AddCommand("ShowLootItemInfo", Show_Loot_Item_Info_Command'Access);
      AddCommand("LootItem", Loot_Item_Command'Access);
      AddCommand("ShowLootItemMenu", Show_Item_Menu_Command'Access);
      AddCommand("LootAmount", Loot_Amount_Command'Access);
   end AddCommands;

end Bases.LootUI;
