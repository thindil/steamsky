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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy; use Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel; use Tcl.Tk.Ada.Widgets.Toplevel;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkSpinBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Config; use Config;
with Crew; use Crew;
with Factions; use Factions;
with Items; use Items;
with Messages; use Messages;
with Ships.Cargo; use Ships.Cargo;
with Ships.Movement; use Ships.Movement;

package body Utils.UI is

   -- ****iv* UUI/TimerId
   -- FUNCTION
   -- Id of timer for auto close command
   -- SOURCE
   TimerId: Unbounded_String := Null_Unbounded_String;
   -- ****

   -- ****o* UUI/Close_Dialog_Command
   -- FUNCTION
   -- Close the selected dialog
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- CloseDialog dialogname ?parentname?
   -- Dialogname is name of the dialog to close, optional parameter parentname
   -- is the name of the parent window
   -- SOURCE
   function Close_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Close_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData);
      Dialog: Tk_Toplevel := Get_Widget(CArgv.Arg(Argv, 1), Interp);
      TopWindow: Tk_Toplevel := Get_Main_Window(Interp);
   begin
      if TimerId /= Null_Unbounded_String then
         Cancel(To_String(TimerId));
         TimerId := Null_Unbounded_String;
      end if;
      Destroy(Dialog);
      if (Argc > 2) then
         TopWindow := Get_Widget(CArgv.Arg(Argv, 2));
      end if;
      if Winfo_Get(TopWindow, "exists") = "1"
        and then Status(TopWindow) = "1" then
         Forget(TopWindow);
      end if;
      return TCL_OK;
   end Close_Dialog_Command;

   -- ****if* Utils.UI/Update_Dialog_Command
   -- FUNCTION
   -- Update countdown timer on the selected dialog. If timer reach 0, close
   -- dialog
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- UpdateDialog dialogname
   -- Dialogname is name of the dialog to update
   -- SOURCE
   function Update_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Update_Dialog_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      MessageButton: constant Ttk_Button :=
        Get_Widget(".message.button", Interp);
      Text: constant String := Widgets.cget(MessageButton, "-text");
      Seconds: constant Natural := Natural'Value(Text(6 .. Text'Last)) - 1;
   begin
      if Seconds = 0 then
         return Close_Dialog_Command(ClientData, Interp, Argc, Argv);
      end if;
      Widgets.configure
        (MessageButton, "-text {Close" & Positive'Image(Seconds) & "}");
      TimerId := To_Unbounded_String(After(1_000, "UpdateDialog .message"));
      return TCL_OK;
   end Update_Dialog_Command;

   procedure ShowMessage(Text: String) is
      MessageDialog: constant Tk_Toplevel :=
        Create
          (".message",
           "-class Dialog -background [ttk::style lookup . -background] -relief solid -borderwidth 2");
      MainWindow: constant Tk_Toplevel := Get_Main_Window(Get_Context);
      X, Y: Integer;
      MessageLabel: constant Ttk_Label :=
        Create
          (MessageDialog & ".text", "-text {" & Text & "} -wraplength 300");
      MessageButton: constant Ttk_Button :=
        Create
          (MessageDialog & ".button",
           "-text {Close" &
           Positive'Image(GameSettings.AutoCloseMessagesTime) &
           "} -command {CloseDialog " & MessageDialog & "}");
      DialogHeight: constant Positive :=
        Positive'Value(Winfo_Get(MessageLabel, "reqheight")) +
        Positive'Value(Winfo_Get(MessageButton, "reqheight")) + 10;
   begin
      Tcl.Tk.Ada.Busy.Busy(MainWindow);
      if TimerId /= Null_Unbounded_String then
         Cancel(To_String(TimerId));
         TimerId := Null_Unbounded_String;
      end if;
      Wm_Set(MessageDialog, "transient", ".");
      if Tcl_GetVar(Get_Context, "tcl_platform(os)") = "Linux" then
         Wm_Set(MessageDialog, "attributes", "-type dialog");
      end if;
      Tcl.Tk.Ada.Grid.Grid(MessageLabel, "-sticky we");
      Tcl.Tk.Ada.Grid.Grid(MessageButton);
      X := (Positive'Value(Winfo_Get(MessageDialog, "vrootwidth")) - 310) / 2;
      if X < 0 then
         X := 0;
      end if;
      Y :=
        (Positive'Value(Winfo_Get(MessageDialog, "vrootheight")) -
         DialogHeight) /
        2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (MessageDialog, "geometry",
         "310x" & Trim(Positive'Image(DialogHeight), Left) & "+" &
         Trim(Positive'Image(X), Left) & "+" & Trim(Positive'Image(Y), Left));
      Wm_Set(MessageDialog, "overrideredirect", "1");
      Focus(MessageButton);
      Bind(MessageDialog, "<Destroy>", "{CloseDialog " & MessageDialog & "}");
      TimerId := To_Unbounded_String(After(1_000, "UpdateDialog .message"));
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

   -- ****o* UUI/Resize_Canvas_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Resize_Canvas_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Canvas: constant Ttk_Frame := Get_Widget(CArgv.Arg(Argv, 1), Interp);
   begin
      Widgets.configure
        (Canvas,
         "-width " & CArgv.Arg(Argv, 2) & " -height [expr " &
         CArgv.Arg(Argv, 3) & " - 20]");
      return TCL_OK;
   end Resize_Canvas_Command;

   -- ****o* UUI/Check_Amount_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Check_Amount_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      CargoIndex: Natural;
      LabelName, WarningText, Value: Unbounded_String;
      Amount: Integer;
      Label: Ttk_Label;
   begin
      if CArgv.Arg(Argv, 1) =
        ".paned.tradeframe.canvas.trade.item.sellframe.amount" then
         LabelName :=
           To_Unbounded_String
             (".paned.tradeframe.canvas.trade.item.sellframe.error");
         WarningText :=
           To_Unbounded_String("You will sell amount below low level of ");
      elsif CArgv.Arg(Argv, 1) =
        ".paned.lootframe.canvas.loot.item.dropframe.amount" then
         LabelName :=
           To_Unbounded_String
             (".paned.lootframe.canvas.loot.item.dropframe.error");
         WarningText :=
           To_Unbounded_String("You will drop amount below low level of ");
      elsif CArgv.Arg(Argv, 1) = ".itemdialog.canvas.frame.amount" then
         LabelName := To_Unbounded_String(".itemdialog.canvas.frame.errorlbl");
         WarningText :=
           To_Unbounded_String("You will drop amount below low level of ");
      else
         LabelName := To_Unbounded_String(".itemdialog.canvas.frame.errorlbl");
         WarningText :=
           To_Unbounded_String("You will give amount below low level of ");
      end if;
      CargoIndex := Natural'Value(CArgv.Arg(Argv, 2));
      Value := To_Unbounded_String(CArgv.Arg(Argv, 3));
      if Integer'Value(To_String(Value)) < 1 or
        Integer'Value(To_String(Value)) >
          PlayerShip.Cargo(CargoIndex).Amount then
         Tcl_SetResult(Interp, "0");
         return TCL_OK;
      end if;
      Label := Get_Widget(To_String(LabelName), Interp);
      if CArgv.Arg(Argv, 1) /=
        ".paned.tradeframe.canvas.trade.item.sellframe.amount"
        and then Items_List(PlayerShip.Cargo(CargoIndex).ProtoIndex).IType =
          FuelType then
         Amount := GetItemAmount(FuelType) - Natural'Value(To_String(Value));
         if Amount <= GameSettings.LowFuel then
            Widgets.configure
              (Label, "-text {" & To_String(WarningText) & "fuel.}");
            Tcl.Tk.Ada.Grid.Grid(Label);
            Tcl_SetResult(Interp, "1");
            return TCL_OK;
         end if;
      end if;
      for Member of PlayerShip.Crew loop
         if Factions_List(Member.Faction).DrinksTypes.Contains
             (Items_List(PlayerShip.Cargo(CargoIndex).ProtoIndex).IType) then
            Amount :=
              GetItemsAmount("Drinks") - Natural'Value(To_String(Value));
            if Amount <= GameSettings.LowDrinks then
               Widgets.configure
                 (Label, "-text {" & To_String(WarningText) & "drinks.}");
               Tcl.Tk.Ada.Grid.Grid(Label);
               Tcl_SetResult(Interp, "1");
               return TCL_OK;
            end if;
            exit;
         elsif Factions_List(Member.Faction).FoodTypes.Contains
             (Items_List(PlayerShip.Cargo(CargoIndex).ProtoIndex).IType) then
            Amount := GetItemsAmount("Food") - Natural'Value(To_String(Value));
            if Amount <= GameSettings.LowFood then
               Widgets.configure
                 (Label, "-text {" & To_String(WarningText) & "food.}");
               Tcl.Tk.Ada.Grid.Grid(Label);
               Tcl_SetResult(Interp, "1");
               return TCL_OK;
            end if;
            exit;
         end if;
      end loop;
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Tcl_SetResult(Interp, "1");
      return TCL_OK;
   exception
      when Constraint_Error =>
         Tcl_SetResult(Interp, "0");
         return TCL_OK;
   end Check_Amount_Command;

   -- ****o* UUI/Validate_Amount_Command
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Validate_Amount_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
      SpinBox: constant Ttk_SpinBox := Get_Widget(CArgv.Arg(Argv, 1), Interp);
      NewArgv: CArgv.Chars_Ptr_Ptr := Argv;
   begin
      NewArgv := NewArgv & Get(SpinBox);
      return Check_Amount_Command
          (ClientData, Interp, CArgv.Argc(NewArgv), NewArgv);
   end Validate_Amount_Command;

   procedure AddCommands is
   begin
      AddCommand("CloseDialog", Close_Dialog_Command'Access);
      AddCommand("UpdateDialog", Update_Dialog_Command'Access);
      AddCommand("ResizeCanvas", Resize_Canvas_Command'Access);
      AddCommand("CheckAmount", Check_Amount_Command'Access);
      AddCommand("ValidateAmount", Validate_Amount_Command'Access);
   end AddCommands;

   procedure MinutesToDate
     (Minutes: Natural; InfoText: in out Unbounded_String) is
      TravelTime: Date_Record := (others => 0);
      MinutesDiff: Integer := Minutes;
   begin
      while MinutesDiff > 0 loop
         if MinutesDiff >= 518400 then
            TravelTime.Year := TravelTime.Year + 1;
            MinutesDiff := MinutesDiff - 518400;
         elsif MinutesDiff >= 43200 then
            TravelTime.Month := TravelTime.Month + 1;
            MinutesDiff := MinutesDiff - 43200;
         elsif MinutesDiff >= 1440 then
            TravelTime.Day := TravelTime.Day + 1;
            MinutesDiff := MinutesDiff - 1440;
         elsif MinutesDiff >= 60 then
            TravelTime.Hour := TravelTime.Hour + 1;
            MinutesDiff := MinutesDiff - 60;
         else
            TravelTime.Minutes := MinutesDiff;
            MinutesDiff := 0;
         end if;
      end loop;
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
        (SpeedType(RealSpeed(PlayerShip, True)) / 1000.0);
      MinutesDiff: Integer;
      Rests, CabinIndex, RestTime: Natural := 0;
      Damage: DamageFactor := 0.0;
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
      for I in PlayerShip.Crew.Iterate loop
         if PlayerShip.Crew(I).Order = Pilot or
           PlayerShip.Crew(I).Order = Engineer then
            Tired := (MinutesDiff / 15) + PlayerShip.Crew(I).Tired;
            if
              (Tired /
               (80 + PlayerShip.Crew(I).Attributes(ConditionIndex)(1))) >
              Rests then
               Rests :=
                 (Tired /
                  (80 + PlayerShip.Crew(I).Attributes(ConditionIndex)(1)));
            end if;
            if Rests > 0 then
               CabinIndex := FindCabin(Crew_Container.To_Index(I));
               if CabinIndex > 0 then
                  Damage :=
                    1.0 -
                    DamageFactor
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
                    ((80 + PlayerShip.Crew(I).Attributes(ConditionIndex)(1)) /
                     CabinBonus) *
                    15;
                  if TempTime = 0 then
                     TempTime := 15;
                  end if;
               else
                  TempTime :=
                    (80 + PlayerShip.Crew(I).Attributes(ConditionIndex)(1)) *
                    15;
               end if;
               TempTime := TempTime + 15;
               if TempTime > RestTime then
                  RestTime := TempTime;
               end if;
            end if;
         end if;
      end loop;
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
           (InfoText, Items_List(FindProtoItem(ItemType => FuelType)).Name);
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
        Get_Widget(".paned.controls.messages.view");
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
      if GameSettings.MessagesOrder = OLDER_FIRST then
         for I in LoopStart .. -1 loop
            Message := GetMessage(I + 1);
            ShowMessage;
            if I < -1 then
               Insert(MessagesView, "end", "{" & LF & "}");
            end if;
         end loop;
      else
         for I in reverse LoopStart .. -1 loop
            Message := GetMessage(I + 1);
            ShowMessage;
            if I > LoopStart then
               Insert(MessagesView, "end", "{" & LF & "}");
            end if;
         end loop;
      end if;
      Tcl.Tk.Ada.Widgets.configure(MessagesView, "-state disable");
   end UpdateMessages;

   procedure ShowScreen(NewScreenName: String) is
      Paned: Ttk_PanedWindow := Get_Widget(".paned");
      SubWindow: Ttk_Frame;
      SubWindows: Unbounded_String;
      MessagesFrame: constant Ttk_Frame :=
        Get_Widget(Paned & ".controls.messages");
   begin
      SubWindows := To_Unbounded_String(Panes(Paned));
      if Index(SubWindows, " ") = 0 then
         SubWindow := Get_Widget(To_String(SubWindows));
      else
         SubWindow := Get_Widget(Slice(SubWindows, 1, Index(SubWindows, " ")));
      end if;
      Forget(Paned, SubWindow);
      SubWindow.Name := New_String(".paned." & NewScreenName);
      Insert(Paned, "0", SubWindow, "-weight 1");
      SashPos(Paned, "0", Natural'Image(GameSettings.MessagesPosition));
      if NewScreenName in "optionsframe" | "messagesframe" or
        not GameSettings.ShowLastMessages then
         Tcl.Tk.Ada.Grid.Grid_Remove(MessagesFrame);
         if NewScreenName /= "mapframe" then
            SashPos(Paned, "0", Winfo_Get(Paned, "height"));
         end if;
      else
         Tcl.Tk.Ada.Grid.Grid(MessagesFrame);
      end if;
      Paned.Name := New_String(".paned.controls.buttons");
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
        (WeaponType, ChestArmor, HeadArmor, ArmsArmor, LegsArmor, ShieldType);
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
      if Items_List(ProtoIndex).IType = WeaponType then
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
            exit;
         end if;
      end loop;
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
      ShowInfo(To_String(ItemInfo), Parent);
   end ShowInventoryItemInfo;

   procedure ShowInfo(Text, ParentName: String) is
      InfoDialog: constant Tk_Toplevel :=
        Create
          (ParentName & "info",
           "-class Dialog -background [ttk::style lookup . -background] -relief solid -borderwidth 2");
      X, Y: Integer;
      InfoLabel: constant Ttk_Label :=
        Create(InfoDialog & ".text", "-text {" & Text & "} -wraplength 300");
      InfoButton: constant Ttk_Button :=
        Create
          (InfoDialog & ".button",
           "-text Close -command {CloseDialog " & InfoDialog & " " &
           ParentName & "}");
      DialogHeight: constant Positive :=
        Positive'Value(Winfo_Get(InfoLabel, "reqheight")) +
        Positive'Value(Winfo_Get(InfoButton, "reqheight")) + 10;
      Parent: constant Ttk_Frame := Get_Widget(ParentName);
   begin
      Tcl.Tk.Ada.Busy.Busy(Parent);
      if TimerId /= Null_Unbounded_String then
         Cancel(To_String(TimerId));
         TimerId := Null_Unbounded_String;
      end if;
      Wm_Set(InfoDialog, "transient", ParentName);
      if Tcl_GetVar(Get_Context, "tcl_platform(os)") = "Linux" then
         Wm_Set(InfoDialog, "attributes", "-type dialog");
      end if;
      Tcl.Tk.Ada.Grid.Grid(InfoLabel, "-sticky we");
      Tcl.Tk.Ada.Grid.Grid(InfoButton);
      X := (Positive'Value(Winfo_Get(InfoDialog, "vrootwidth")) - 310) / 2;
      if X < 0 then
         X := 0;
      end if;
      Y :=
        (Positive'Value(Winfo_Get(InfoDialog, "vrootheight")) - DialogHeight) /
        2;
      if Y < 0 then
         Y := 0;
      end if;
      Wm_Set
        (InfoDialog, "geometry",
         "310x" & Trim(Positive'Image(DialogHeight), Left) & "+" &
         Trim(Positive'Image(X), Left) & "+" & Trim(Positive'Image(Y), Left));
      Wm_Set(InfoDialog, "overrideredirect", "1");
      Focus(InfoButton);
   end ShowInfo;

end Utils.UI;
