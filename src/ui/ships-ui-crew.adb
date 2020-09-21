-- /home/thindil/Projekty/steamsky/steamsky/src/ui/ships-ui-crew.adb
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
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Strings;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkMenuButton; use Tcl.Tk.Ada.Widgets.TtkMenuButton;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.GetString; use Tcl.Tklib.Ada.GetString;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with Config; use Config;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships.Crew; use Ships.Crew;
with Themes; use Themes;
with Utils; use Utils;
with Utils.UI; use Utils.UI;

package body Ships.UI.Crew is

   procedure UpdateCrewInfo is
      Label: Ttk_Label;
      CrewInfoFrame, Item, ButtonsFrame: Ttk_Frame;
      UpgradeProgress: Ttk_ProgressBar;
      Tokens: Slice_Set;
      Rows: Natural := 0;
      ShipCanvas: Tk_Canvas;
      ProgressBarStyle: Unbounded_String;
      CrewButton: Ttk_MenuButton;
      Row: Positive := 1;
      NeedRepair, NeedClean: Boolean := False;
      Button: Ttk_Button;
      CrewMenu: Tk_Menu;
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
      CrewInfoFrame.Interp := Get_Context;
      CrewInfoFrame.Name :=
        New_String(CrewInfoFrame & ".paned.shipinfoframe.crew.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(CrewInfoFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (CrewInfoFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item.Interp := Get_Context;
            Item.Name := New_String(Slice(Tokens, J));
            Destroy(Item);
         end loop;
      end loop;
      for Module of PlayerShip.Modules loop
         if Module.Durability < Module.MaxDurability then
            NeedRepair := True;
         end if;
         if (Module.Durability > 0 and Module.MType = CABIN)
           and then Module.Cleanliness < Module.Quality then
            NeedClean := True;
         end if;
         exit when NeedClean and NeedRepair;
      end loop;
      ButtonsFrame := Create(CrewInfoFrame & ".ordersbuttons");
      if NeedClean then
         Button :=
           Create
             (ButtonsFrame & ".clean",
              "-text {" &
              Encode
                ("" &
                 Themes_List(To_String(GameSettings.InterfaceTheme))
                   .CleanIcon) &
              "} -style Header.Toolbutton -command {OrderForAll Clean}");
         Add(Button, "Clean ship everyone");
         Tcl.Tk.Ada.Grid.Grid(Button);
         Row := 2;
      end if;
      if NeedRepair then
         Button :=
           Create
             (ButtonsFrame & ".repair",
              "-text {" &
              Encode
                ("" &
                 Themes_List(To_String(GameSettings.InterfaceTheme))
                   .RepairIcon) &
              "} -style Header.Toolbutton -command {OrderForAll Repair}");
         Add(Button, "Repair ship everyone");
         Tcl.Tk.Ada.Grid.Grid(Button, "-row 0 -column 1");
         Row := 2;
      end if;
      Tcl.Tk.Ada.Grid.Grid(ButtonsFrame, "-sticky w");
      Label := Create(CrewInfoFrame & ".name", "-text {Name}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row));
      Label := Create(CrewInfoFrame & ".order", "-text {Order}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row) & " -column 1");
      Label := Create(CrewInfoFrame & ".health", "-text {Health}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row) & " -column 2");
      Label := Create(CrewInfoFrame & ".tired", "-text {Fatigue}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row) & " -column 3");
      Label := Create(CrewInfoFrame & ".thirst", "-text {Thirst}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row) & " -column 4");
      Label := Create(CrewInfoFrame & ".hunger", "-text {Hunger}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row) & " -column 5");
      Label := Create(CrewInfoFrame & ".morale", "-text {Morale}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-row" & Natural'Image(Row) & " -column 6");
      Row := Row + 1;
      CrewMenu.Interp := Get_Context;
      for I in PlayerShip.Crew.Iterate loop
         CrewMenu.Name :=
           New_String(".membermenu" & Trim(Positive'Image(Row), Left));
         if (Winfo_Get(CrewMenu, "exists")) = "0" then
            CrewMenu :=
              Create
                (".membermenu" & Trim(Positive'Image(Row), Left),
                 "-tearoff false");
         end if;
         Delete(CrewMenu, "0", "end");
         Menu.Add
           (CrewMenu, "command",
            "-label {Rename crew member} -command {RenameMember" &
            Positive'Image(Crew_Container.To_Index(I)) & "}");
         Menu.Add
           (CrewMenu, "command",
            "-label {Show more info about the crew member}");
         Menu.Add
           (CrewMenu, "command", "-label {Show inventory of the crew member}");
         Menu.Add
           (CrewMenu, "command",
            "-label {Set order priorities of the crew member}");
         if
           ((PlayerShip.Crew(I).Tired = 100 or
             PlayerShip.Crew(I).Hunger = 100 or
             PlayerShip.Crew(I).Thirst = 100) and
            PlayerShip.Crew(I).Order /= Rest) or
           (PlayerShip.Crew(I).Skills.Length = 0 or
            PlayerShip.Crew(I).ContractLength = 0) then
            Menu.Add
              (CrewMenu, "command",
               "-label {Go on break} -command {SetCrewOrder Rest" &
               Positive'Image(Crew_Container.To_Index(I)) & "}");
         else
            if PlayerShip.Crew(I).Order /= Pilot then
               Menu.Add
                 (CrewMenu, "command",
                  "-label {Go piloting the ship} -command {SetCrewOrder Pilot" &
                  Positive'Image(Crew_Container.To_Index(I)) & "}");
            end if;
            if PlayerShip.Crew(I).Order /= Engineer then
               Menu.Add
                 (CrewMenu, "command",
                  "-label {Go engineering the ship} -command {SetCrewOrder Engineer" &
                  Positive'Image(Crew_Container.To_Index(I)) & "}");
            end if;
            for J in PlayerShip.Modules.Iterate loop
               if PlayerShip.Modules(J).Durability <
                 PlayerShip.Modules(J).MaxDurability then
                  NeedRepair := True;
               end if;
               if PlayerShip.Modules(J).Durability > 0 then
                  case PlayerShip.Modules(J).MType is
                     when GUN | HARPOON_GUN =>
                        if PlayerShip.Modules(J).Owner(1) /=
                          Crew_Container.To_Index(I) then
                           Menu.Add
                             (CrewMenu, "command",
                              "-label {Operate " &
                              To_String(PlayerShip.Modules(J).Name) &
                              "} -command {SetCrewOrder Gunner" &
                              Positive'Image(Crew_Container.To_Index(I)) &
                              Positive'Image
                                (Positive(Modules_Container.To_Index(J))) &
                              "}");
                        end if;
                     when WORKSHOP =>
                        if not IsWorking
                            (PlayerShip.Modules(J).Owner, Row - 1) and
                          PlayerShip.Modules(J).CraftingIndex /=
                            Null_Unbounded_String then
                           Menu.Add
                             (CrewMenu, "command",
                              "-label {Work in " &
                              To_String(PlayerShip.Modules(J).Name) &
                              "} -command {SetCrewOrder Craft" &
                              Positive'Image(Crew_Container.To_Index(I)) &
                              Positive'Image
                                (Positive(Modules_Container.To_Index(J))) &
                              "}");
                        end if;
                     when CABIN =>
                        if PlayerShip.Modules(J).Cleanliness <
                          PlayerShip.Modules(J).Quality and
                          PlayerShip.Crew(I).Order /= Clean and NeedClean then
                           Menu.Add
                             (CrewMenu, "command",
                              "-label {Clean ship} -command {SetCrewOrder Clean" &
                              Positive'Image(Crew_Container.To_Index(I)) &
                              "}");
                           NeedClean := False;
                        end if;
                     when TRAINING_ROOM =>
                        if not IsWorking
                            (PlayerShip.Modules(J).Owner, Row - 1) then
                           Menu.Add
                             (CrewMenu, "command",
                              "-label {Go on training in " &
                              To_String(PlayerShip.Modules(J).Name) &
                              "} -command {SetCrewOrder Train" &
                              Positive'Image(Crew_Container.To_Index(I)) &
                              Positive'Image
                                (Positive(Modules_Container.To_Index(J))) &
                              "}");
                        end if;
                     when others =>
                        null;
                  end case;
                  if PlayerShip.Modules(J).Durability <
                    PlayerShip.Modules(J).MaxDurability and
                    NeedRepair then
                     Menu.Add
                       (CrewMenu, "command",
                        "-label {Repair ship} -command {SetCrewOrder Repair" &
                        Positive'Image(Crew_Container.To_Index(I)) & "}");
                     NeedRepair := False;
                  end if;
               end if;
            end loop;
            for J in PlayerShip.Crew.Iterate loop
               if PlayerShip.Crew(J).Health < 100 and
                 Crew_Container.To_Index(J) /= (Row - 1) and
                 PlayerShip.Crew(J).Order /= Heal then
                  Menu.Add
                    (CrewMenu, "command",
                     "-label {Heal wounded crew members} -command {SetCrewOrder Heal" &
                     Positive'Image(Crew_Container.To_Index(I)) & "}");
                  exit;
               end if;
            end loop;
            if PlayerShip.UpgradeModule > 0 and
              PlayerShip.Crew(I).Order /= Upgrading then
               Menu.Add
                 (CrewMenu, "command",
                  "-label {Upgrade module} -command {SetCrewOrder Upgrading" &
                  Positive'Image(Crew_Container.To_Index(I)) & "}");
            end if;
            if PlayerShip.Crew(I).Order /= Talk then
               Menu.Add
                 (CrewMenu, "command",
                  "-label {Talking in bases} -command {SetCrewOrder Talk" &
                  Positive'Image(Crew_Container.To_Index(I)) & "}");
            end if;
            if PlayerShip.Crew(I).Order /= Rest then
               Menu.Add
                 (CrewMenu, "command",
                  "-label {Go on break} -command {SetCrewOrder Rest" &
                  Positive'Image(Crew_Container.To_Index(I)) & "}");
            end if;
         end if;
         if Crew_Container.To_Index(I) /= 1 and PlayerShip.Speed = DOCKED then
            Menu.Add
              (CrewMenu, "command",
               "-label {Dismiss} -command {Dismiss" &
               Positive'Image(Crew_Container.To_Index(I)) & "}");
         end if;
         CrewButton :=
           Create
             (CrewInfoFrame & ".name" &
              Trim(Positive'Image(Crew_Container.To_Index(I)), Left),
              "-text {" & To_String(PlayerShip.Crew(I).Name) & "} -menu " &
              CrewMenu);
         Add(CrewButton, "Show available crew member's options");
         Tcl.Tk.Ada.Grid.Grid
           (CrewButton, "-row" & Natural'Image(Row) & " -sticky w");
         Label :=
           Create
             (CrewInfoFrame & ".order" & Trim(Natural'Image(Row), Left),
              "-text {" & Crew_Orders'Image(PlayerShip.Crew(I).Order)(1) &
              To_Lower
                (Crew_Orders'Image(PlayerShip.Crew(I).Order)
                   (2 .. Crew_Orders'Image(PlayerShip.Crew(I).Order)'Last)) &
              "}");
         Add(Label, "The current order for the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (Label, "-row" & Natural'Image(Row) & " -column 1");
         if PlayerShip.Crew(I).Health > 74 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif PlayerShip.Crew(I).Health > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         UpgradeProgress :=
           Create
             (CrewInfoFrame & ".health" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(PlayerShip.Crew(I).Health) & "}" &
              To_String(ProgressBarStyle));
         Add
           (UpgradeProgress,
            "The current health level of the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 2");
         if PlayerShip.Crew(I).Tired -
           PlayerShip.Crew(I).Attributes(ConditionIndex)(1) <
           25 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif PlayerShip.Crew(I).Tired -
           PlayerShip.Crew(I).Attributes(ConditionIndex)(1) >
           24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         UpgradeProgress :=
           Create
             (CrewInfoFrame & ".fatigue" & Trim(Natural'Image(Row), Left),
              "-value {" &
              Integer'Image
                (PlayerShip.Crew(I).Tired -
                 PlayerShip.Crew(I).Attributes(ConditionIndex)(1)) &
              "}" & To_String(ProgressBarStyle));
         Add
           (UpgradeProgress,
            "The current tired level of the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 3");
         if PlayerShip.Crew(I).Thirst < 25 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif PlayerShip.Crew(I).Thirst > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         UpgradeProgress :=
           Create
             (CrewInfoFrame & ".thirst" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(PlayerShip.Crew(I).Thirst) & "}" &
              To_String(ProgressBarStyle));
         Add
           (UpgradeProgress,
            "The current thirst level of the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 4");
         if PlayerShip.Crew(I).Hunger < 25 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif PlayerShip.Crew(I).Hunger > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         UpgradeProgress :=
           Create
             (CrewInfoFrame & ".hunger" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(PlayerShip.Crew(I).Hunger) & "}" &
              To_String(ProgressBarStyle));
         Add
           (UpgradeProgress,
            "The current hunger level of the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 5");
         if PlayerShip.Crew(I).Morale(1) > 49 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style green.Horizontal.TProgressbar");
         elsif PlayerShip.Crew(I).Morale(1) > 24 then
            ProgressBarStyle :=
              To_Unbounded_String(" -style yellow.Horizontal.TProgressbar");
         else
            ProgressBarStyle :=
              To_Unbounded_String(" -style Horizontal.TProgressbar");
         end if;
         UpgradeProgress :=
           Create
             (CrewInfoFrame & ".morale" & Trim(Natural'Image(Row), Left),
              "-value {" & Natural'Image(PlayerShip.Crew(I).Morale(1)) & "}" &
              To_String(ProgressBarStyle));
         Add
           (UpgradeProgress,
            "The current morale level of the selected crew member.");
         Tcl.Tk.Ada.Grid.Grid
           (UpgradeProgress, "-row" & Natural'Image(Row) & " -column 6");
         Row := Row + 1;
      end loop;
      Tcl_Eval(Get_Context, "update");
      ShipCanvas.Interp := Get_Context;
      ShipCanvas.Name := New_String(".paned.shipinfoframe.crew.canvas");
      configure
        (ShipCanvas, "-scrollregion [list " & BBox(ShipCanvas, "all") & "]");
      Xview_Move_To(ShipCanvas, "0.0");
      Yview_Move_To(ShipCanvas, "0.0");
   end UpdateCrewInfo;

   -- ****o* SUCrew/Order_For_All_Command
   -- FUNCTION
   -- Set the selected order for the whole crew
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- OrderForAll order
   -- Order is the name of the order which will be assigned to the whole
   -- player ship crew
   -- SOURCE
   function Order_For_All_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Order_For_All_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
   begin
      for I in PlayerShip.Crew.Iterate loop
         GiveOrders
           (PlayerShip, Crew_Container.To_Index(I),
            Crew_Orders'Value(CArgv.Arg(Argv, 1)));
      end loop;
      UpdateHeader;
      UpdateMessages;
      UpdateCrewInfo;
      return TCL_OK;
   exception
      when An_Exception : Crew_Order_Error =>
         AddMessage(Exception_Message(An_Exception), OrderMessage);
         UpdateHeader;
         UpdateMessages;
         return TCL_OK;
   end Order_For_All_Command;

   -- ****o* SUCrew/Rename_Member_Command
   -- FUNCTION
   -- Change name of the selected player's ship crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- RenameMember memberindex
   -- Memberindex is the index of the crew member which name will be changed
   -- SOURCE
   function Rename_Member_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Rename_Member_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      Button: Ttk_Button;
      CrewIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      Button.Interp := Interp;
      Button.Name :=
        New_String
          (".paned.shipinfoframe.crew.canvas.frame.name" &
           Trim(Positive'Image(CrewIndex), Left));
      if Tk_Get_String
          (Interp, ".gs", "text",
           "{Enter a new name for the " & cget(Button, "-text") & "}") =
        "0" then
         return TCL_OK;
      end if;
      PlayerShip.Crew(CrewIndex).Name :=
        To_Unbounded_String(Tcl_GetVar(Interp, "text"));
      configure(Button, "-text $text");
      return TCL_OK;
   end Rename_Member_Command;

   -- ****o* SUCrew/Dismiss_Command
   -- FUNCTION
   -- Dismiss the selected crew member
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Dismiss memberindex
   -- Memberindex is the index of the player ship crew member which will be
   -- dismissed
   -- SOURCE
   function Dismiss_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Dismiss_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      MemberIndex: constant Positive := Positive'Value(CArgv.Arg(Argv, 1));
   begin
      if MessageBox
          ("-message {Are you sure want to dismiss " &
           To_String(PlayerShip.Crew(MemberIndex).Name) &
           "?} -icon question -type yesno") /=
        "yes" then
         return TCL_OK;
      end if;
      AddMessage
        ("You dismissed " & To_String(PlayerShip.Crew(MemberIndex).Name) & ".",
         OrderMessage);
      DeleteMember(MemberIndex, PlayerShip);
      SkyBases(BaseIndex).Population := SkyBases(BaseIndex).Population + 1;
      for I in PlayerShip.Crew.Iterate loop
         UpdateMorale
           (PlayerShip, Crew_Container.To_Index(I), GetRandom(-5, -1));
      end loop;
      UpdateCrewInfo;
      UpdateHeader;
      UpdateMessages;
      return TCL_OK;
   end Dismiss_Command;

   procedure AddCommands is
   begin
      AddCommand("OrderForAll", Order_For_All_Command'Access);
      AddCommand("RenameMember", Rename_Member_Command'Access);
      AddCommand("Dismiss", Dismiss_Command'Access);
   end AddCommands;

end Ships.UI.Crew;
