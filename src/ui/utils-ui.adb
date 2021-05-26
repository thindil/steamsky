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
with Ada.Directories; use Ada.Directories;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry; use Tcl.Tk.Ada.Widgets.TtkEntry;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with Combat.UI; use Combat.UI;
with Config; use Config;
with CoreUI; use CoreUI;
with Crew; use Crew;
with Dialogs; use Dialogs;
with Events; use Events;
with Factions; use Factions;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with MainMenu; use MainMenu;
with Messages; use Messages;
with Missions; use Missions;
with Ships.Cargo; use Ships.Cargo;
with Ships.Crew; use Ships.Crew;
with Ships.Movement; use Ships.Movement;
with Ships.UI.Crew; use Ships.UI.Crew;
with Statistics.UI; use Statistics.UI;

package body Utils.UI is

   procedure ShowMessage
     (Text: String; ParentFrame: String := ".gameframe"; Title: String) is
      MessageDialog: constant Ttk_Frame :=
        Create_Dialog(ParentFrame & ".message", Title);
      MessageLabel: constant Ttk_Label :=
        Create
          (MessageDialog & ".text", "-text {" & Text & "} -wraplength 300");
   begin
      Tcl.Tk.Ada.Grid.Grid(MessageLabel, "-sticky we -padx 5 -pady 5");
      Add_Close_Button
        (MessageDialog & ".button",
         "Close" & Positive'Image(Game_Settings.Auto_Close_Messages_Time),
         "CloseDialog " & MessageDialog);
      Show_Dialog(MessageDialog, ParentFrame);
   end ShowMessage;

   procedure AddCommand
     (Name: String; AdaCommand: not null CreateCommands.Tcl_CmdProc) is
      Command: Tcl.Tcl_Command;
      SteamSky_Add_Command_Error: exception;
   begin
      Tcl_Eval(Get_Context, "info commands " & Name);
      if Tcl_GetResult(Get_Context) /= "" then
         raise SteamSky_Add_Command_Error
           with "Command with name " & Name & " exists";
      end if;
      Command :=
        CreateCommands.Tcl_CreateCommand
          (Get_Context, Name, AdaCommand, 0, null);
      if Command = null then
         raise SteamSky_Add_Command_Error with "Can't add command " & Name;
      end if;
   end AddCommand;

   -- ****o* UUI/UUI.Resize_Canvas_Command
   -- PARAMETERS
   -- Resize the selected canvas
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ResizeCanvas name width height
   -- Name is the name of the canvas to resize, width it a new width, height
   -- is a new height
   -- SOURCE
   function Resize_Canvas_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Resize_Canvas_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Canvas: constant Ttk_Frame := Get_Widget(CArgv.Arg(Argv, 1), Interp);
   begin
      Widgets.configure
        (Canvas,
         "-width " & CArgv.Arg(Argv, 2) & " -height [expr " &
         CArgv.Arg(Argv, 3) & " - 20]");
      return TCL_OK;
   end Resize_Canvas_Command;

   -- ****o* UUI/UUI.Check_Amount_Command
   -- PARAMETERS
   -- Check amount of the item, if it is not below low level warning or if
   -- entered amount is a proper number
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CheckAmount name cargoindex value
   -- Name is the name of spinbox which value will be checked, cargoindex is
   -- the index of the item in the cargo
   -- SOURCE
   function Check_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Check_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      CargoIndex: constant Natural := Natural'Value(CArgv.Arg(Argv, 2));
      LabelName, WarningText: Unbounded_String;
      Amount: Integer;
      Label: Ttk_Label;
      Value: Integer :=
        (if CArgv.Arg(Argv, 3)'Length > 0 then
           Integer'Value(CArgv.Arg(Argv, 3))
         else 0);
      SpinBox: constant Ttk_SpinBox := Get_Widget(CArgv.Arg(Argv, 1), Interp);
      MaxValue: constant Positive :=
        Positive'Value(Widgets.cget(SpinBox, "-to"));
   begin
      if CArgv.Arg(Argv, 1) = ".itemdialog.giveamount" then
         LabelName := To_Unbounded_String(".itemdialog.errorlbl");
         WarningText :=
           To_Unbounded_String("You will give amount below low level of ");
      else
         LabelName := To_Unbounded_String(".itemdialog.errorlbl");
         WarningText :=
           To_Unbounded_String
             ("You will " & CArgv.Arg(Argv, 4) &
              " amount below low level of ");
      end if;
      if Value < 1 then
         Set(SpinBox, "1");
         Value := 1;
      elsif Value > MaxValue then
         Set(SpinBox, Positive'Image(MaxValue));
         Value := MaxValue;
      end if;
      if Argc > 4 and then CArgv.Arg(Argv, 4) in "buy" | "take" then
         Tcl_SetResult(Interp, "1");
         return TCL_OK;
      end if;
      Label := Get_Widget(To_String(LabelName), Interp);
      if Items_List(PlayerShip.Cargo(CargoIndex).ProtoIndex).IType =
        Fuel_Type then
         Amount := GetItemAmount(Fuel_Type) - Value;
         if Amount <= Game_Settings.Low_Fuel then
            Widgets.configure
              (Label, "-text {" & To_String(WarningText) & "fuel.}");
            Tcl.Tk.Ada.Grid.Grid(Label);
            Tcl_SetResult(Interp, "1");
            return TCL_OK;
         end if;
      end if;
      Check_Food_And_Drinks_Loop :
      for Member of PlayerShip.Crew loop
         if Factions_List(Member.Faction).DrinksTypes.Contains
             (Items_List(PlayerShip.Cargo(CargoIndex).ProtoIndex).IType) then
            Amount := GetItemsAmount("Drinks") - Value;
            if Amount <= Game_Settings.Low_Drinks then
               Widgets.configure
                 (Label, "-text {" & To_String(WarningText) & "drinks.}");
               Tcl.Tk.Ada.Grid.Grid(Label);
               Tcl_SetResult(Interp, "1");
               return TCL_OK;
            end if;
            exit Check_Food_And_Drinks_Loop;
         elsif Factions_List(Member.Faction).FoodTypes.Contains
             (Items_List(PlayerShip.Cargo(CargoIndex).ProtoIndex).IType) then
            Amount := GetItemsAmount("Food") - Value;
            if Amount <= Game_Settings.Low_Food then
               Widgets.configure
                 (Label, "-text {" & To_String(WarningText) & "food.}");
               Tcl.Tk.Ada.Grid.Grid(Label);
               Tcl_SetResult(Interp, "1");
               return TCL_OK;
            end if;
            exit Check_Food_And_Drinks_Loop;
         end if;
      end loop Check_Food_And_Drinks_Loop;
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   exception
      when Constraint_Error =>
         Tcl_SetResult(Interp, "0");
         return TCL_OK;
   end Check_Amount_Command;

   -- ****o* UUI/UUI.Validate_Amount_Command
   -- PARAMETERS
   -- Validate amount of the item when button to increase or decrease the
   -- amount was pressed
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ValidateAmount name
   -- Name is the name of spinbox which value will be validated
   -- SOURCE
   function Validate_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Validate_Amount_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      SpinBox: constant Ttk_SpinBox := Get_Widget(CArgv.Arg(Argv, 1), Interp);
      NewArgv: CArgv.Chars_Ptr_Ptr := Argv;
   begin
      if Argc < 4 then
         NewArgv := NewArgv & Get(SpinBox);
      else
         NewArgv :=
           CArgv.Empty & CArgv.Arg(Argv, 0) & CArgv.Arg(Argv, 1) &
           CArgv.Arg(Argv, 2) & Get(SpinBox) & CArgv.Arg(Argv, 3);
      end if;
      return
        Check_Amount_Command(ClientData, Interp, CArgv.Argc(NewArgv), NewArgv);
   end Validate_Amount_Command;

   -- ****o* UUI/UUI.Get_String_Command
   -- FUNCTION
   -- Get string value from the player, like new ship or module name
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- GetString caption closeaction title
   -- Caption is the text showed above entry field in the dialog, variable
   -- is the variable which will be set and title is the title of the dialog
   -- SOURCE
   function Get_String_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Get_String_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      StringDialog: constant Ttk_Frame :=
        Create_Dialog(".getstring", CArgv.Arg(Argv, 3), 275, 2);
      StringLabel: constant Ttk_Label :=
        Create
          (StringDialog & ".text",
           "-text {" & CArgv.Arg(Argv, 1) & "} -wraplength 300");
      StringEntry: constant Ttk_Entry :=
        Create
          (StringDialog & ".entry",
           "-validate key -validatecommand {set value %P;if {$value == {}} {.getstring.okbutton state disabled; return 1} else {.getstring.okbutton state !disabled; return 1}}");
      OkButton: constant Ttk_Button :=
        Create
          (StringDialog & ".okbutton",
           "-text {Ok} -command {SetTextVariable " & CArgv.Arg(Argv, 2) &
           "; CloseDialog " & StringDialog & "}");
      CancelButton: constant Ttk_Button :=
        Create
          (StringDialog & ".closebutton",
           "-text {Cancel} -command {CloseDialog " & StringDialog & "}");
   begin
      Tcl.Tk.Ada.Grid.Grid(StringLabel, "-padx 5 -pady {5 0} -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid(StringEntry, "-sticky we -padx 5 -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid(OkButton, "-row 3 -pady 5 -padx 5");
      State(OkButton, "disabled");
      Tcl.Tk.Ada.Grid.Grid(CancelButton, "-row 3 -column 1 -pady 5 -padx 5");
      Bind(CancelButton, "<Tab>", "{focus .getstring.entry;break}");
      Bind(CancelButton, "<Escape>", "{" & CancelButton & " invoke;break}");
      Bind(OkButton, "<Escape>", "{" & CancelButton & " invoke;break}");
      Bind(StringEntry, "<Escape>", "{" & CancelButton & " invoke;break}");
      Bind(StringEntry, "<Return>", "{" & OkButton & " invoke;break}");
      Focus(StringEntry);
      Show_Dialog(StringDialog);
      return TCL_OK;
   end Get_String_Command;

   -- ****o* UUI/UUI.Set_Text_Variable_Command
   -- FUNCTION
   -- Set the selected Tcl text variable and the proper the Ada its equivalent
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetTextVariable variablename
   -- Variablename is the name of variable to set
   -- SOURCE
   function Set_Text_Variable_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Text_Variable_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      TEntry: constant Ttk_Entry := Get_Widget(".getstring.entry", Interp);
      Value: constant String := Get(TEntry);
      VarName: constant String := CArgv.Arg(Argv, 1);
   begin
      Tcl_SetVar(Interp, VarName, Value);
      if VarName = "shipname" then
         PlayerShip.Name := To_Unbounded_String(Value);
      elsif VarName'Length > 10 and then VarName(1 .. 10) = "modulename" then
         declare
            ModuleIndex: constant Positive :=
              Positive'Value(VarName(11 .. VarName'Last));
            Button: constant Ttk_Button :=
              Get_Widget
                (".gameframe.paned.shipinfoframe.modules.canvas.frame.name" &
                 Trim(Positive'Image(ModuleIndex + 1), Left));
         begin
            PlayerShip.Modules(ModuleIndex).Name := To_Unbounded_String(Value);
            Widgets.configure(Button, "-text $" & VarName);
            Tcl_UnsetVar(Interp, VarName);
         end;
      elsif VarName'Length > 8 and then VarName(1 .. 8) = "crewname" then
         declare
            CrewIndex: constant Positive :=
              Positive'Value(VarName(9 .. VarName'Last));
            Button: constant Ttk_Button :=
              Get_Widget
                (".gameframe.paned.shipinfoframe.crew.canvas.frame.name" &
                 Trim(Positive'Image(CrewIndex), Left));
         begin
            PlayerShip.Crew(CrewIndex).Name := To_Unbounded_String(Value);
            Widgets.configure(Button, "-text $" & VarName);
            Tcl_UnsetVar(Interp, VarName);
         end;
      end if;
      return TCL_OK;
   end Set_Text_Variable_Command;

   -- ****o* UUI/UUI.Process_Question_Command
   -- FUNCTION
   -- Process question from dialog when the player answer Yes there
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ProcessQuestion answer
   -- Answer is the answer set for the selected question
   -- SOURCE
   function Process_Question_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Process_Question_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Result: constant String := CArgv.Arg(Argv, 1);
   begin
      if Result = "deletesave" then
         declare
            Frame: constant Ttk_Frame := Get_Widget(".loadmenu", Interp);
            LoadView: constant Ttk_Tree_View := Get_Widget(Frame & ".view");
            ItemIndex, Items: Unbounded_String;
         begin
            ItemIndex := To_Unbounded_String(Selection(LoadView));
            Delete_File(To_String(Save_Directory & ItemIndex));
            Delete(LoadView, To_String(ItemIndex));
            Items := To_Unbounded_String(Children(LoadView, "{}"));
            if Items = Null_Unbounded_String then
               Unbind_From_Main_Window(Interp, "<Alt-b>");
               Unbind_From_Main_Window(Interp, "<Alt-l>");
               Unbind_From_Main_Window(Interp, "<Alt-d>");
               Unbind_From_Main_Window(Interp, "<Escape>");
               Tcl.Tk.Ada.Pack.Pack_Forget(Frame);
               Show_Main_Menu;
            else
               ItemIndex := Unbounded_Slice(Items, 1, Index(Items, " "));
               if ItemIndex = Null_Unbounded_String then
                  ItemIndex := Items;
               end if;
               Selection_Set(LoadView, To_String(ItemIndex));
            end if;
         end;
      elsif Result = "sethomebase" then
         declare
            TraderIndex: constant Natural := FindMember(Talk);
            Price: Positive := 1_000;
            MoneyIndex2: constant Natural :=
              FindItem(PlayerShip.Cargo, Money_Index);
         begin
            if MoneyIndex2 = 0 then
               ShowMessage
                 (Text =>
                    "You don't have any " & To_String(Money_Name) &
                    " for change ship home base.",
                  Title => "No money");
               return TCL_OK;
            end if;
            CountPrice(Price, TraderIndex);
            if PlayerShip.Cargo(MoneyIndex2).Amount < Price then
               ShowMessage
                 (Text =>
                    "You don't have enough " & To_String(Money_Name) &
                    " for change ship home base.",
                  Title => "No money");
               return TCL_OK;
            end if;
            PlayerShip.HomeBase :=
              SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
            UpdateCargo
              (Ship => PlayerShip, CargoIndex => MoneyIndex2,
               Amount => -(Price));
            AddMessage
              ("You changed your ship home base to: " &
               To_String(SkyBases(PlayerShip.HomeBase).Name),
               OtherMessage);
            GainExp(1, Talking_Skill, TraderIndex);
            Update_Game(10);
            ShowSkyMap;
         end;
      elsif Result = "nopilot" then
         WaitForRest;
         declare
            StartsCombat: constant Boolean := CheckForEvent;
            Message: Unbounded_String := Null_Unbounded_String;
         begin
            if not StartsCombat and Game_Settings.Auto_Finish then
               Message := To_Unbounded_String(AutoFinishMissions);
            end if;
            if Message /= Null_Unbounded_String then
               ShowMessage(Text => To_String(Message), Title => "Error");
            end if;
            CenterX := PlayerShip.SkyX;
            CenterY := PlayerShip.SkyY;
            if StartsCombat then
               ShowCombatUI;
            else
               ShowSkyMap;
            end if;
         end;
      elsif Result = "quit" then
         declare
            Paned: constant Ttk_PanedWindow :=
              Get_Widget(".gameframe.paned", Interp);
         begin
            Game_Settings.Messages_Position :=
              Game_Settings.Window_Height - Natural'Value(SashPos(Paned, "0"));
            End_Game(True);
            Show_Main_Menu;
         end;
      elsif Result = "resign" then
         Death(1, To_Unbounded_String("resignation"), PlayerShip);
         ShowQuestion
           ("You are dead. Would you like to see your game statistics?",
            "showstats");
      elsif Result = "showstats" then
         declare
            Button: Ttk_Button := Get_Widget(".gameframe.header.menubutton");
         begin
            Tcl.Tk.Ada.Grid.Grid(Button);
            Button.Name := New_String(".gameframe.header.closebutton");
            Widgets.configure(Button, "-command ShowMainMenu");
            Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 1");
            Delete(GameMenu, "3", "4");
            Delete(GameMenu, "6", "14");
            ShowStatistics;
         end;
      elsif Result = "mainmenu" then
         declare
            Paned: constant Ttk_PanedWindow := Get_Widget(".gameframe.paned");
         begin
            Game_Settings.Messages_Position :=
              Game_Settings.Window_Height - Natural'Value(SashPos(Paned, "0"));
            End_Game(False);
            Show_Main_Menu;
         end;
      elsif Result = "messages" then
         declare
            TypeBox: constant Ttk_ComboBox :=
              Get_Widget
                (".gameframe.paned.messagesframe.canvas.messages.options.types",
                 Get_Context);
         begin
            ClearMessages;
            Current(TypeBox, "0");
            Tcl_Eval(Get_Context, "ShowLastMessages");
         end;
      elsif Result = "retire" then
         Death
           (1, To_Unbounded_String("retired after finished the game"),
            PlayerShip);
         ShowQuestion
           ("You are dead. Would you like to see your game statistics?",
            "showstats");
      else
         declare
            BaseIndex: constant Positive :=
              SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
            MemberIndex: constant Positive :=
              Positive'Value(CArgv.Arg(Argv, 1));
         begin
            AddMessage
              ("You dismissed " &
               To_String(PlayerShip.Crew(MemberIndex).Name) & ".",
               OrderMessage);
            DeleteMember(MemberIndex, PlayerShip);
            SkyBases(BaseIndex).Population :=
              SkyBases(BaseIndex).Population + 1;
            Update_Morale_Loop :
            for I in PlayerShip.Crew.Iterate loop
               UpdateMorale
                 (PlayerShip, Crew_Container.To_Index(I), GetRandom(-5, -1));
            end loop Update_Morale_Loop;
            UpdateCrewInfo;
            UpdateHeader;
            UpdateMessages;
         end;
      end if;
      return TCL_OK;
   end Process_Question_Command;

   -- ****o* UUI/UUI.Set_Scrollbar_Bindings_Command
   -- FUNCTION
   -- Assign scrolling events with mouse wheel to the selected vertical
   -- scrollbar from the selected widget
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetScrollbarBindings widget scrollbar
   -- Widget is the widget from which events will be fired, scrollbar is
   -- Ttk::scrollbar which to which bindings will be added
   -- SOURCE
   function Set_Scrollbar_Bindings_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Scrollbar_Bindings_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Widget: constant Ttk_Frame := Get_Widget(CArgv.Arg(Argv, 1), Interp);
      Scrollbar: constant Ttk_Scrollbar :=
        Get_Widget(CArgv.Arg(Argv, 2), Interp);
   begin
      Bind
        (Widget, "<Button-4>",
         "{if {[winfo ismapped " & Scrollbar & "]} {event generate " &
         Scrollbar & " <Button-4>}}");
      Bind
        (Widget, "<Button-5>",
         "{if {[winfo ismapped " & Scrollbar & "]} {event generate " &
         Scrollbar & " <Button-5>}}");
      Bind
        (Widget, "<MouseWheel>",
         "{if {[winfo ismapped " & Scrollbar & "]} {event generate " &
         Scrollbar & " <MouseWheel>}}");
      return TCL_OK;
   end Set_Scrollbar_Bindings_Command;

   function Show_On_Map_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
   begin
      CenterX := Positive'Value(CArgv.Arg(Argv, 1));
      CenterY := Positive'Value(CArgv.Arg(Argv, 2));
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      Tcl_Eval(Interp, "InvokeButton " & Close_Button);
      Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
      return TCL_OK;
   end Show_On_Map_Command;

   function Set_Destination_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
   begin
      if Positive'Value(CArgv.Arg(Argv, 1)) = PlayerShip.SkyX and
        Positive'Value(CArgv.Arg(Argv, 2)) = PlayerShip.SkyY then
         ShowMessage
           (Text => "You are at this location now.",
            Title => "Can't set destination");
         return TCL_OK;
      end if;
      PlayerShip.DestinationX := Positive'Value(CArgv.Arg(Argv, 1));
      PlayerShip.DestinationY := Positive'Value(CArgv.Arg(Argv, 2));
      AddMessage
        ("You set the travel destination for your ship.", OrderMessage);
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      Tcl_Eval(Interp, "InvokeButton " & Close_Button);
      Tcl.Tk.Ada.Grid.Grid_Remove(Close_Button);
      return TCL_OK;
   end Set_Destination_Command;

   procedure AddCommands is
   begin
      AddCommand("ResizeCanvas", Resize_Canvas_Command'Access);
      AddCommand("CheckAmount", Check_Amount_Command'Access);
      AddCommand("ValidateAmount", Validate_Amount_Command'Access);
      AddCommand("GetString", Get_String_Command'Access);
      AddCommand("SetTextVariable", Set_Text_Variable_Command'Access);
      AddCommand("ProcessQuestion", Process_Question_Command'Access);
      AddCommand
        ("SetScrollbarBindings", Set_Scrollbar_Bindings_Command'Access);
      AddCommand("ShowOnMap", Show_On_Map_Command'Access);
      AddCommand("SetDestination2", Set_Destination_Command'Access);
   end AddCommands;

   procedure MinutesToDate
     (Minutes: Natural; InfoText: in out Unbounded_String) is
      TravelTime: Date_Record := (others => 0);
      MinutesDiff: Integer := Minutes;
   begin
      Count_Time_Loop :
      while MinutesDiff > 0 loop
         if MinutesDiff >= 518_400 then
            TravelTime.Year := TravelTime.Year + 1;
            MinutesDiff := MinutesDiff - 518_400;
         elsif MinutesDiff >= 43_200 then
            TravelTime.Month := TravelTime.Month + 1;
            MinutesDiff := MinutesDiff - 43_200;
         elsif MinutesDiff >= 1_440 then
            TravelTime.Day := TravelTime.Day + 1;
            MinutesDiff := MinutesDiff - 1_440;
         elsif MinutesDiff >= 60 then
            TravelTime.Hour := TravelTime.Hour + 1;
            MinutesDiff := MinutesDiff - 60;
         else
            TravelTime.Minutes := MinutesDiff;
            MinutesDiff := 0;
         end if;
      end loop Count_Time_Loop;
      if TravelTime.Year > 0 then
         Append(InfoText, Positive'Image(TravelTime.Year) & "y");
      end if;
      if TravelTime.Month > 0 then
         Append(InfoText, Positive'Image(TravelTime.Month) & "m");
      end if;
      if TravelTime.Day > 0 then
         Append(InfoText, Positive'Image(TravelTime.Day) & "d");
      end if;
      if TravelTime.Hour > 0 then
         Append(InfoText, Positive'Image(TravelTime.Hour) & "h");
      end if;
      if TravelTime.Minutes > 0 then
         Append(InfoText, Positive'Image(TravelTime.Minutes) & "mins");
      end if;
   end MinutesToDate;

   procedure TravelInfo
     (InfoText: in out Unbounded_String; Distance: Positive;
      ShowFuelName: Boolean := False) is
      type SpeedType is digits 2;
      Speed: constant SpeedType :=
        (SpeedType(RealSpeed(PlayerShip, True)) / 1_000.0);
      MinutesDiff: Integer;
      Rests, CabinIndex, RestTime: Natural := 0;
      Damage: Damage_Factor := 0.0;
      Tired, CabinBonus, TempTime: Natural;
   begin
      if Speed = 0.0 then
         Append(InfoText, LF & "ETA: Never");
         return;
      end if;
      MinutesDiff := Integer(100.0 / Speed);
      case PlayerShip.Speed is
         when QUARTER_SPEED =>
            if MinutesDiff < 60 then
               MinutesDiff := 60;
            end if;
         when HALF_SPEED =>
            if MinutesDiff < 30 then
               MinutesDiff := 30;
            end if;
         when FULL_SPEED =>
            if MinutesDiff < 15 then
               MinutesDiff := 15;
            end if;
         when others =>
            null;
      end case;
      Append(InfoText, LF & "ETA:");
      MinutesDiff := MinutesDiff * Distance;
      Count_Rest_Time_Loop :
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Order = Pilot or
           PlayerShip.Crew(I).Order = Engineer then
            Tired := (MinutesDiff / 15) + PlayerShip.Crew(I).Tired;
            if
              (Tired /
               (80 + PlayerShip.Crew(I).Attributes(Condition_Index)(1))) >
              Rests then
               Rests :=
                 (Tired /
                  (80 + PlayerShip.Crew(I).Attributes(Condition_Index)(1)));
            end if;
            if Rests > 0 then
               CabinIndex := FindCabin(Crew_Container.To_Index(I));
               if CabinIndex > 0 then
                  Damage :=
                    1.0 -
                    Damage_Factor
                      (Float(PlayerShip.Modules(CabinIndex).Durability) /
                       Float(PlayerShip.Modules(CabinIndex).MaxDurability));
                  CabinBonus :=
                    PlayerShip.Modules(CabinIndex).Cleanliness -
                    Natural
                      (Float(PlayerShip.Modules(CabinIndex).Cleanliness) *
                       Float(Damage));
                  if CabinBonus = 0 then
                     CabinBonus := 1;
                  end if;
                  TempTime :=
                    ((80 + PlayerShip.Crew(I).Attributes(Condition_Index)(1)) /
                     CabinBonus) *
                    15;
                  if TempTime = 0 then
                     TempTime := 15;
                  end if;
               else
                  TempTime :=
                    (80 + PlayerShip.Crew(I).Attributes(Condition_Index)(1)) *
                    15;
               end if;
               TempTime := TempTime + 15;
               if TempTime > RestTime then
                  RestTime := TempTime;
               end if;
            end if;
         end if;
      end loop Count_Rest_Time_Loop;
      MinutesDiff := MinutesDiff + (Rests * RestTime);
      MinutesToDate(MinutesDiff, InfoText);
      Append
        (InfoText,
         LF & "Approx fuel usage:" &
         Natural'Image
           (abs (Distance * CountFuelNeeded) + (Rests * (RestTime / 10))) &
         " ");
      if ShowFuelName then
         Append
           (InfoText, Items_List(FindProtoItem(ItemType => Fuel_Type)).Name);
      end if;
   end TravelInfo;

   procedure UpdateMessages is
      LoopStart: Integer := 0 - MessagesAmount;
      Message: Message_Data;
      TagNames: constant array(1 .. 5) of Unbounded_String :=
        (To_Unbounded_String("yellow"), To_Unbounded_String("green"),
         To_Unbounded_String("red"), To_Unbounded_String("blue"),
         To_Unbounded_String("cyan"));
      MessagesView: constant Tk_Text :=
        Get_Widget(".gameframe.paned.controls.messages.view");
      procedure ShowMessage is
      begin
         if Message.Color = WHITE then
            Insert
              (MessagesView, "end", "{" & To_String(Message.Message) & "}");
         else
            Insert
              (MessagesView, "end",
               "{" & To_String(Message.Message) & "} [list " &
               To_String(TagNames(Message_Color'Pos(Message.Color))) & "]");
         end if;
      end ShowMessage;
   begin
      Tcl.Tk.Ada.Widgets.configure(MessagesView, "-state normal");
      Delete(MessagesView, "1.0", "end");
      if LoopStart = 0 then
         return;
      end if;
      if LoopStart < -10 then
         LoopStart := -10;
      end if;
      if Game_Settings.Messages_Order = OLDER_FIRST then
         Show_Older_First_Loop :
         for I in LoopStart .. -1 loop
            Message := GetMessage(I + 1);
            ShowMessage;
            if I < -1 then
               Insert(MessagesView, "end", "{" & LF & "}");
            end if;
         end loop Show_Older_First_Loop;
         See(MessagesView, "end");
      else
         Show_Newer_First_Loop :
         for I in reverse LoopStart .. -1 loop
            Message := GetMessage(I + 1);
            ShowMessage;
            if I > LoopStart then
               Insert(MessagesView, "end", "{" & LF & "}");
            end if;
         end loop Show_Newer_First_Loop;
      end if;
      Tcl.Tk.Ada.Widgets.configure(MessagesView, "-state disable");
   end UpdateMessages;

   procedure ShowScreen(NewScreenName: String) is
      Paned: Ttk_PanedWindow := Get_Widget(".gameframe.paned");
      SubWindow, OldSubWindow: Ttk_Frame;
      SubWindows: Unbounded_String;
      MessagesFrame: constant Ttk_Frame :=
        Get_Widget(Paned & ".controls.messages");
   begin
      SubWindows := To_Unbounded_String(Panes(Paned));
      OldSubWindow :=
        (if Index(SubWindows, " ") = 0 then Get_Widget(To_String(SubWindows))
         else Get_Widget(Slice(SubWindows, 1, Index(SubWindows, " "))));
      Forget(Paned, OldSubWindow);
      SubWindow.Name := New_String(".gameframe.paned." & NewScreenName);
      Insert(Paned, "0", SubWindow, "-weight 1");
      if NewScreenName in "optionsframe" | "messagesframe" or
        not Game_Settings.Show_Last_Messages then
         Tcl.Tk.Ada.Grid.Grid_Remove(MessagesFrame);
         if NewScreenName /= "mapframe" then
            SashPos(Paned, "0", Winfo_Get(Paned, "height"));
         end if;
      else
         if Trim(Widget_Image(OldSubWindow), Both) in
             Paned & ".messagesframe" | Paned & ".optionsframe" then
            SashPos
              (Paned, "0",
               Natural'Image
                 (Game_Settings.Window_Height -
                  Game_Settings.Messages_Position));
         end if;
         Tcl.Tk.Ada.Grid.Grid(MessagesFrame);
      end if;
      Paned.Name := New_String(".gameframe.paned.controls.buttons");
      if NewScreenName = "mapframe" then
         Tcl.Tk.Ada.Grid.Grid(Paned);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Paned);
      end if;
   end ShowScreen;

   procedure ShowInventoryItemInfo
     (Parent: String; ItemIndex: Positive; MemberIndex: Natural) is
      ProtoIndex, ItemInfo: Unbounded_String;
      ItemTypes: constant array(1 .. 6) of Unbounded_String :=
        (Weapon_Type, Chest_Armor, Head_Armor, Arms_Armor, Legs_Armor,
         Shield_Type);
   begin
      if MemberIndex > 0 then
         ProtoIndex :=
           PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).ProtoIndex;
         if PlayerShip.Crew(MemberIndex).Inventory(ItemIndex).Durability <
           Default_Item_Durability then
            Append
              (ItemInfo,
               GetItemDamage
                 (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex)
                    .Durability) &
               LF);
         end if;
      else
         ProtoIndex := PlayerShip.Cargo(ItemIndex).ProtoIndex;
         if PlayerShip.Cargo(ItemIndex).Durability <
           Default_Item_Durability then
            Append
              (ItemInfo,
               GetItemDamage(PlayerShip.Cargo(ItemIndex).Durability) & LF);
         end if;
      end if;
      Append
        (ItemInfo,
         "Weight:" & Positive'Image(Items_List(ProtoIndex).Weight) & " kg");
      if Items_List(ProtoIndex).IType = Weapon_Type then
         Append
           (ItemInfo,
            LF & "Skill: " &
            Skills_List(Items_List(ProtoIndex).Value(3)).Name & "/" &
            Attributes_List
              (Skills_List(Items_List(ProtoIndex).Value(3)).Attribute)
              .Name);
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
      Show_More_Item_Info_Loop :
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
            exit Show_More_Item_Info_Loop;
         end if;
      end loop Show_More_Item_Info_Loop;
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
      if Parent = "." then
         ShowInfo
           (Text => To_String(ItemInfo),
            Title =>
              (if MemberIndex > 0 then
                 GetItemName
                   (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex), False,
                    False)
               else GetItemName(PlayerShip.Cargo(ItemIndex), False, False)));
      else
         ShowInfo
           (To_String(ItemInfo), Parent,
            (if MemberIndex > 0 then
               GetItemName
                 (PlayerShip.Crew(MemberIndex).Inventory(ItemIndex), False,
                  False)
             else GetItemName(PlayerShip.Cargo(ItemIndex), False, False)));
         Tcl_Eval(Get_Context, "raise .info");
      end if;
   end ShowInventoryItemInfo;

   procedure ShowInfo
     (Text: String; ParentName: String := ".gameframe"; Title: String) is
      InfoDialog: constant Ttk_Frame :=
        Create_Dialog(".info", Title, 275, 1, ParentName);
      InfoLabel: constant Ttk_Label :=
        Create(InfoDialog & ".text", "-text {" & Text & "} -wraplength 300");
   begin
      Tcl.Tk.Ada.Grid.Grid(InfoLabel, "-sticky we -padx 5 -pady {5 0}");
      if ParentName = ".gameframe" then
         Add_Close_Button
           (InfoDialog & ".button", "Close", "CloseDialog " & InfoDialog);
      else
         Add_Close_Button
           (InfoDialog & ".button", "Close",
            "CloseDialog " & InfoDialog & " " & ParentName);
      end if;
      Show_Dialog(InfoDialog);
   end ShowInfo;

   procedure ShowManipulateItem
     (Title, Command, Action: String;
      ItemIndex: Inventory_Container.Extended_Index;
      MaxAmount: Natural := 0) is
      ItemDialog: constant Ttk_Frame :=
        Create_Dialog(".itemdialog", Title, 275, 2);
      Button: Ttk_Button :=
        Create
          (ItemDialog & ".dropbutton", "-text Ok -command {" & Command & "}");
      Label: Ttk_Label;
      AmountBox: Ttk_SpinBox;
   begin
      if MaxAmount = 0 then
         AmountBox :=
           Create
             (ItemDialog & ".amount",
              "-width 10 -from 1 -to" &
              Positive'Image(PlayerShip.Cargo(ItemIndex).Amount) &
              " -validate key -validatecommand {CheckAmount " & ItemDialog &
              ".amount" & Positive'Image(ItemIndex) & " %P " & Action &
              "} -command {ValidateAmount " & ItemDialog & ".amount" &
              Positive'Image(ItemIndex) & " " & Action & "}");
      else
         AmountBox :=
           Create
             (ItemDialog & ".amount",
              "-width 10 -from 1 -to" & Positive'Image(MaxAmount) &
              " -validate key -validatecommand {CheckAmount " & ItemDialog &
              ".amount" & Positive'Image(ItemIndex) & " %P " & Action &
              "} -command {ValidateAmount " & ItemDialog & ".amount" &
              Positive'Image(ItemIndex) & " " & Action & "}");
      end if;
      if MaxAmount = 0 then
         Label :=
           Create
             (ItemDialog & ".amountlbl",
              "-text {Amount (max:" &
              Positive'Image(PlayerShip.Cargo(ItemIndex).Amount) &
              "):} -takefocus 0");
      else
         Label :=
           Create
             (ItemDialog & ".amountlbl",
              "-text {Amount (max:" & Positive'Image(MaxAmount) &
              "):} -takefocus 0");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Label, "-padx {5 0}");
      Set(AmountBox, "1");
      Tcl.Tk.Ada.Grid.Grid(AmountBox, "-column 1 -row 1");
      Bind
        (AmountBox, "<Escape>",
         "{" & ItemDialog & ".cancelbutton invoke;break}");
      Label :=
        Create
          (ItemDialog & ".errorlbl",
           "-style Headerred.TLabel -wraplength 370 -takefocus 0");
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2 -padx 5");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 0 -row 3 -pady {0 5}");
      Bind
        (Button, "<Escape>", "{" & ItemDialog & ".cancelbutton invoke;break}");
      Button :=
        Create
          (ItemDialog & ".cancelbutton",
           "-text Cancel -command {CloseDialog " & ItemDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 3 -pady {0 5}");
      Focus(Button);
      Bind(Button, "<Tab>", "{focus .itemdialog.dropbutton;break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      Show_Dialog(ItemDialog);
   end ShowManipulateItem;

   procedure ShowQuestion
     (Question, Result: String; In_Game: Boolean := True) is
      QuestionDialog: constant Ttk_Frame :=
        Create_Dialog
          (".questiondialog",
           (if Result = "showstats" then "Question" else "Confirmation"), 275,
           2, (if In_Game then ".gameframe" else "."));
      Label: constant Ttk_Label :=
        Create
          (QuestionDialog & ".question",
           "-text {" & Question & "} -wraplength 370 -takefocus 0");
      Button: Ttk_Button :=
        Create
          (QuestionDialog & ".yesbutton",
           "-text Yes -command {.questiondialog.nobutton invoke; ProcessQuestion " &
           Result & "}");
   begin
      Tcl.Tk.Ada.Grid.Grid(Label, "-columnspan 2 -padx 5 -pady {5 0}");
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 0 -row 2 -pady {0 5} -padx 5");
      Bind
        (Button, "<Escape>", "{" & QuestionDialog & ".nobutton invoke;break}");
      if not In_Game then
         Button :=
           Create
             (QuestionDialog & ".nobutton",
              "-text No -command {CloseDialog " & QuestionDialog & " .}");
      else
         Button :=
           Create
             (QuestionDialog & ".nobutton",
              "-text No -command {CloseDialog " & QuestionDialog & "}");
      end if;
      Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 2 -pady {0 5} -padx 5");
      Focus(Button);
      if In_Game then
         Show_Dialog(QuestionDialog);
      else
         Show_Dialog(QuestionDialog, ".");
      end if;
      Bind(Button, "<Tab>", "{focus .questiondialog.yesbutton;break}");
      Bind(Button, "<Escape>", "{" & Button & " invoke;break}");
      if Result = "showstats" then
         Widgets.configure
           (Button,
            "-command {CloseDialog " & QuestionDialog &
            "; ProcessQuestion mainmenu}");
         Button := Get_Widget(QuestionDialog & ".yesbutton");
         Widgets.configure
           (Button,
            "-command {CloseDialog " & QuestionDialog &
            "; ProcessQuestion showstats}");
      end if;
   end ShowQuestion;

   procedure Delete_Widgets
     (Start_Index, End_Index: Integer; Frame: Tk_Widget'Class) is
      Tokens: Slice_Set;
      Item: Ttk_Frame;
   begin
      if End_Index < Start_Index then
         return;
      end if;
      Delete_Widgets_Loop :
      for I in Start_Index .. End_Index loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves(Frame, "-row" & Positive'Image(I)),
            " ");
         Delete_Row_Loop :
         for J in 1 .. Slice_Count(Tokens) loop
            Item := Get_Widget(Slice(Tokens, J));
            Destroy(Item);
         end loop Delete_Row_Loop;
      end loop Delete_Widgets_Loop;
   end Delete_Widgets;

end Utils.UI;
