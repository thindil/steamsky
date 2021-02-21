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
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
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
with Bases.Ship; use Bases.Ship;
with Bases.Trade; use Bases.Trade;
with BasesTypes; use BasesTypes;
with Config; use Config;
with Crafts; use Crafts;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships.Crew; use Ships.Crew;
with Utils.UI; use Utils.UI;

package body Bases.UI is

   -- ****o* BUI/BUI.Show_Base_UI_Command
   -- FUNCTION
   -- Show the selected base action
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowBaseUI UIType
   -- UIType can be heal, repair, recipes
   -- SOURCE
   function Show_Base_UI_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Base_UI_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      BaseFrame: Ttk_Frame := Get_Widget(Paned & ".baseframe", Interp);
      BaseCanvas: constant Tk_Canvas :=
        Get_Widget(BaseFrame & ".canvas", Interp);
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      ActionButton: Ttk_Button;
      SearchEntry: Ttk_Entry;
      ItemsView: Ttk_Tree_View;
      FirstIndex, ButtonText: Unbounded_String;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      BaseType: constant Unbounded_String := SkyBases(BaseIndex).BaseType;
   begin
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
      SearchEntry := Get_Widget(BaseFrame & ".search", Interp);
      if CArgv.Arg(Argv, 1) /= "recipes" then
         Tcl.Tk.Ada.Grid.Grid_Remove(SearchEntry);
      else
         Tcl.Tk.Ada.Grid.Grid(SearchEntry);
         if Argc < 3 then
            Delete(SearchEntry, "0", "end");
         end if;
      end if;
      ItemsView := Get_Widget(BaseFrame & ".items.view", Interp);
      Delete(ItemsView, "[list " & Children(ItemsView, "{}") & "]");
      ActionButton := Get_Widget(BaseFrame & ".info.accept", Interp);
      if CArgv.Arg(Argv, 1) = "heal" then
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp crew}");
         for I in PlayerShip.Crew.Iterate loop
            if PlayerShip.Crew(I).Health < 100 then
               if FirstIndex = Null_Unbounded_String then
                  FirstIndex :=
                    To_Unbounded_String
                      (Natural'Image(Crew_Container.To_Index(I)));
               end if;
               Insert
                 (ItemsView,
                  "{} end -id" & Positive'Image(Crew_Container.To_Index(I)) &
                  " -text {" & To_String(PlayerShip.Crew(I).Name) & "}");
            end if;
         end loop;
         Insert
           (ItemsView, "{} end -id 0 -text {Heal all wounded crew members}");
         ButtonText := To_Unbounded_String("Buy healing");
         Heading(ItemsView, "#0", "-text Wounded");
      elsif CArgv.Arg(Argv, 1) = "repair" then
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp ship}");
         for I in PlayerShip.Modules.Iterate loop
            if PlayerShip.Modules(I).Durability <
              PlayerShip.Modules(I).MaxDurability then
               if FirstIndex = Null_Unbounded_String then
                  FirstIndex :=
                    To_Unbounded_String
                      (Natural'Image(Modules_Container.To_Index(I)));
               end if;
               Insert
                 (ItemsView,
                  "{} end -id" &
                  Positive'Image(Modules_Container.To_Index(I)) & " -text {" &
                  To_String(PlayerShip.Modules(I).Name) & "}");
            end if;
         end loop;
         Insert
           (ItemsView, "{} end -id 0 -text {Slowly repair the whole ship}");
         if SkyBases(BaseIndex).Population > 149 then
            Insert(ItemsView, "{} end -id {-1} -text {Repair the whole ship}");
         end if;
         if SkyBases(BaseIndex).Population > 299 then
            Insert
              (ItemsView,
               "{} end -id {-2} -text {Quickly repair the whole ship}");
         end if;
         ButtonText := To_Unbounded_String("Buy repairs");
         Heading(ItemsView, "#0", "-text Damaged");
      elsif CArgv.Arg(Argv, 1) = "recipes" then
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp craft}");
         for I in Recipes_List.Iterate loop
            if BasesTypes_List(BaseType).Recipes.Contains
                (Recipes_Container.Key(I)) and
              Known_Recipes.Find_Index(Item => Recipes_Container.Key(I)) =
                Positive_Container.No_Index and
              Recipes_List(I).Reputation <=
                SkyBases(BaseIndex).Reputation(1) then
               if Argc = 3
                 and then
                   Index
                     (To_Lower
                        (To_String
                           (Items_List(Recipes_List(I).ResultIndex).Name)),
                      To_Lower(CArgv.Arg(Argv, 2))) =
                   0 then
                  goto End_Of_Recipes_Loop;
               end if;
               if FirstIndex = Null_Unbounded_String then
                  FirstIndex := Recipes_Container.Key(I);
               end if;
               Insert
                 (ItemsView,
                  "{} end -id " & To_String(Recipes_Container.Key(I)) &
                  " -text {" &
                  To_String
                    (Items_List(Recipes_List(I).ResultIndex).Name & "}"));
               <<End_Of_Recipes_Loop>>
            end if;
         end loop;
         ButtonText := To_Unbounded_String("Buy recipe");
         Heading(ItemsView, "#0", "-text Recipes");
      end if;
      Unbind(ItemsView, "<<TreeviewSelect>>");
      Bind
        (ItemsView, "<<TreeviewSelect>>",
         "{ShowItemInfo " & CArgv.Arg(Argv, 1) & "}");
      configure
        (ActionButton,
         "-text {" & To_String(ButtonText) & "} -command {BaseAction " &
         CArgv.Arg(Argv, 1) & "}");
      if FirstIndex = Null_Unbounded_String and Argc < 3 then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Selection_Set(ItemsView, "[list " & To_String(FirstIndex) & "]");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      BaseFrame.Name := New_String(Widget_Image(BaseCanvas) & ".base");
      configure
        (BaseCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (BaseCanvas, "window",
         "0 0 -anchor nw -window " & Widget_Image(BaseFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (BaseCanvas, "-scrollregion [list " & BBox(BaseCanvas, "all") & "]");
      ShowScreen("baseframe");
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   end Show_Base_UI_Command;

   -- ****o* BUI/BUI.Show_Item_Info_Command
   -- FUNCTION
   -- Show the information about the selected item
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowItemInfo ItemType
   -- ItemType can be heal, repair, recipes
   -- SOURCE
   function Show_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Item_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      ItemsView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.baseframe.canvas.base.items.view", Interp);
      FormattedTime, ItemIndex: Unbounded_String;
      Cost, Time: Natural := 0;
      InfoLabel: Ttk_Label :=
        Get_Widget(".gameframe.paned.baseframe.canvas.base.info.info", Interp);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MoneyIndex2: Natural;
      ActionButton: constant Ttk_Button :=
        Get_Widget
          (".gameframe.paned.baseframe.canvas.base.info.accept", Interp);
   begin
      if Selection(ItemsView) = "" then
         return TCL_OK;
      end if;
      ItemIndex := To_Unbounded_String(Selection(ItemsView));
      if CArgv.Arg(Argv, 1) = "heal" then
         HealCost(Cost, Time, Natural'Value(To_String(ItemIndex)));
         if Time < 60 then
            FormattedTime :=
              To_Unbounded_String(Natural'Image(Time) & " minute");
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
         configure
           (InfoLabel,
            "-text {Heal cost:" & Natural'Image(Cost) & " " &
            To_String(MoneyName) & LF & "Heal time:" &
            To_String(FormattedTime) & "}");
      elsif CArgv.Arg(Argv, 1) = "repair" then
         RepairCost(Cost, Time, Integer'Value(To_String(ItemIndex)));
         CountPrice(Cost, FindMember(Talk));
         if Time < 60 then
            FormattedTime :=
              To_Unbounded_String(Natural'Image(Time) & " minute");
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
         configure
           (InfoLabel,
            "-text {Repair cost:" & Natural'Image(Cost) & " " &
            To_String(MoneyName) & LF & "Repair time:" &
            To_String(FormattedTime) & "}");
      elsif CArgv.Arg(Argv, 1) = "recipes" then
         Cost :=
           (if
              Get_Price
                (SkyBases(BaseIndex).BaseType,
                 Recipes_List(ItemIndex).ResultIndex) >
              0
            then
              Get_Price
                (SkyBases(BaseIndex).BaseType,
                 Recipes_List(ItemIndex).ResultIndex) *
              Recipes_List(ItemIndex).Difficulty * 10
            else Recipes_List(ItemIndex).Difficulty * 10);
         Cost := Natural(Float(Cost) * Float(NewGameSettings.PricesBonus));
         if Cost = 0 then
            Cost := 1;
         end if;
         CountPrice(Cost, FindMember(Talk));
         configure
           (InfoLabel,
            "-text {Base price:" & Positive'Image(Cost) & " " &
            To_String(MoneyName) & "}");
      end if;
      MoneyIndex2 := FindItem(PlayerShip.Cargo, MoneyIndex);
      InfoLabel.Name :=
        New_String(".gameframe.paned.baseframe.canvas.base.info.money");
      if MoneyIndex2 > 0 then
         configure
           (InfoLabel,
            "-text {You have" &
            Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) & " " &
            To_String(MoneyName) & ".}");
         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
            configure(ActionButton, "-state disabled");
         else
            configure(ActionButton, "-state !disabled");
         end if;
      else
         configure(ActionButton, "-state disabled");
         configure(InfoLabel, "-text {You don't have any money.}");
      end if;
      return TCL_OK;
   end Show_Item_Info_Command;

   -- ****o* BUI/BUI.Base_Action_Command
   -- FUNCTION
   -- Execute the selected action
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- BaseAction ActionType
   -- ActionType can be heal, repair, recipes
   -- SOURCE
   function Base_Action_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Base_Action_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
      ItemsView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.baseframe.canvas.base.items.view", Interp);
      ItemIndex: Unbounded_String;
   begin
      ItemIndex := To_Unbounded_String(Selection(ItemsView));
      if CArgv.Arg(Argv, 1) = "heal" then
         HealWounded(Natural'Value(To_String(ItemIndex)));
      elsif CArgv.Arg(Argv, 1) = "repair" then
         Bases.Ship.RepairShip(Integer'Value(To_String(ItemIndex)));
      elsif CArgv.Arg(Argv, 1) = "recipes" then
         BuyRecipe(ItemIndex);
      end if;
      UpdateHeader;
      UpdateMessages;
      return Show_Base_UI_Command
          (ClientData, Interp, 2,
           CArgv.Empty & "ShowBaseUI" & CArgv.Arg(Argv, 1));
   end Base_Action_Command;

   -- ****o* BUI/BUI.Search_Recipes_Command
   -- FUNCTION
   -- Show only this recipes which contains the selected sequence
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SearchRecipes TextToSearch
   -- SOURCE
   function Search_Recipes_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Search_Recipes_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
      SearchText: constant String := CArgv.Arg(Argv, 1);
   begin
      if SearchText'Length = 0 then
         return Show_Base_UI_Command
             (ClientData, Interp, 2, CArgv.Empty & "ShowBaseUI" & "recipes");
      end if;
      return Show_Base_UI_Command
          (ClientData, Interp, 3,
           CArgv.Empty & "ShowBaseUI" & "recipes" & SearchText);
   end Search_Recipes_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowBaseUI", Show_Base_UI_Command'Access);
      AddCommand("ShowItemInfo", Show_Item_Info_Command'Access);
      AddCommand("BaseAction", Base_Action_Command'Access);
      AddCommand("SearchRecipes", Search_Recipes_Command'Access);
   end AddCommands;

end Bases.UI;
