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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Busy;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Place;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScale; use Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases.Trade; use Bases.Trade;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships.Crew; use Ships.Crew;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Bases.RecruitUI is

   -- ****iv* RecruitUI/RecruitUI.RecruitTable
   -- FUNCTION
   -- Table with info about the available recruits
   -- SOURCE
   RecruitTable: Table_Widget (6);
   -- ****

   -- ****o* RecruitUI/RecruitUI.Show_Recruit_Command
   -- FUNCTION
   -- Show the selected base available recruits
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command.
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecruit
   -- SOURCE
   function Show_Recruit_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      RecruitFrame: Ttk_Frame := Get_Widget(Paned & ".recruitframe", Interp);
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      HighestLevel, HighestIndex: Positive;
   begin
      if Winfo_Get(RecruitFrame, "exists") = "0" then
         RecruitFrame := Create(Widget_Image(RecruitFrame));
         RecruitTable :=
           CreateTable
             (Widget_Image(RecruitFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Gender"),
               To_Unbounded_String("Faction"),
               To_Unbounded_String("Base cost"),
               To_Unbounded_String("Highest stat"),
               To_Unbounded_String("Highest skill")));
         Bind
           (RecruitFrame, "<Configure>",
            "{ResizeCanvas " & RecruitTable.Canvas & " %w %h}");
      elsif Winfo_Get(RecruitFrame, "ismapped") = "1" and
        (Argc = 1 or SkyBases(BaseIndex).Recruits.Length = 0) then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp crew}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      ClearTable(RecruitTable);
      for I in SkyBases(BaseIndex).Recruits.Iterate loop
         AddButton
           (RecruitTable, To_String(SkyBases(BaseIndex).Recruits(I).Name),
            "Show available options for recruit",
            "ShowRecruitMenu" & Positive'Image(Recruit_Container.To_Index(I)),
            1);
         if SkyBases(BaseIndex).Recruits(I).Gender = 'M' then
            AddButton
              (RecruitTable, "Male", "Show available options for recruit",
               "ShowRecruitMenu" &
               Positive'Image(Recruit_Container.To_Index(I)),
               2);
         else
            AddButton
              (RecruitTable, "Female", "Show available options for recruit",
               "ShowRecruitMenu" &
               Positive'Image(Recruit_Container.To_Index(I)),
               2);
         end if;
         AddButton
           (RecruitTable,
            To_String
              (Factions_List(SkyBases(BaseIndex).Recruits(I).Faction).Name),
            "Show available options for recruit",
            "ShowRecruitMenu" & Positive'Image(Recruit_Container.To_Index(I)),
            3);
         AddButton
           (RecruitTable,
            Positive'Image(SkyBases(BaseIndex).Recruits(I).Price),
            "Show available options for recruit",
            "ShowRecruitMenu" & Positive'Image(Recruit_Container.To_Index(I)),
            4);
         HighestLevel := 1;
         HighestIndex := 1;
         for J in SkyBases(BaseIndex).Recruits(I).Attributes.Iterate loop
            if SkyBases(BaseIndex).Recruits(I).Attributes(J)(1) >
              HighestLevel then
               HighestLevel :=
                 SkyBases(BaseIndex).Recruits(I).Attributes(J)(1);
               HighestIndex := Attributes_Container.To_Index(J);
            end if;
         end loop;
         AddButton
           (RecruitTable, To_String(Attributes_List(HighestIndex).Name),
            "Show available options for recruit",
            "ShowRecruitMenu" & Positive'Image(Recruit_Container.To_Index(I)),
            5);
         HighestLevel := 1;
         HighestIndex := 1;
         for J in SkyBases(BaseIndex).Recruits(I).Skills.Iterate loop
            if SkyBases(BaseIndex).Recruits(I).Skills(J)(1) > HighestLevel then
               HighestLevel := SkyBases(BaseIndex).Recruits(I).Skills(J)(1);
               HighestIndex := Skills_Container.To_Index(J);
            end if;
         end loop;
         AddButton
           (RecruitTable, To_String(Skills_List(HighestIndex).Name),
            "Show available options for recruit",
            "ShowRecruitMenu" & Positive'Image(Recruit_Container.To_Index(I)),
            6, True);
      end loop;
      UpdateTable(RecruitTable);
      configure
        (RecruitTable.Canvas,
         "-scrollregion [list " & BBox(RecruitTable.Canvas, "all") & "]");
      ShowScreen("recruitframe");
      return TCL_OK;
   end Show_Recruit_Command;

   -- ****iv* RecruitUI/RecruitUI.RecruitIndex
   -- FUNCTION
   -- The index of currently selected recruit
   -- SOURCE
   RecruitIndex: Positive;
   -- ****

   -- ****o* RecruitUI/RecruitUI.Show_Recruit_Menu_Command
   -- FUNCTION
   -- Show menu with actions for the selected recruit
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecruitMenu recruitindex
   -- RecruitIndex is a index of the recruit which menu will be shown
   -- SOURCE
   function Show_Recruit_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Menu_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      RecruitMenu: Tk_Menu := Get_Widget(".recruitmenu", Interp);
   begin
      RecruitIndex := Positive'Value(CArgv.Arg(Argv, 1));
      if Winfo_Get(RecruitMenu, "exists") = "0" then
         RecruitMenu := Create(".recruitmenu", "-tearoff false");
      end if;
      Delete(RecruitMenu, "0", "end");
      Menu.Add
        (RecruitMenu, "command",
         "-label {Show recruit details} -command {ShowRecruitInfo}");
      Menu.Add
        (RecruitMenu, "command",
         "-label {Start negotiations} -command {Negotiate}");
      Tk_Popup
        (RecruitMenu, Winfo_Get(Get_Main_Window(Interp), "pointerx"),
         Winfo_Get(Get_Main_Window(Interp), "pointery"));
      return TCL_OK;
   end Show_Recruit_Menu_Command;

   -- ****o* RecruitUI/RecruitUI.Show_Recruit_Info_Command
   -- FUNCTION
   -- Show information about the selected recruit
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecruitInfoCommand
   -- SOURCE
   function Show_Recruit_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Info_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      RecruitInfo: Unbounded_String;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Recruit: constant Recruit_Data :=
        SkyBases(BaseIndex).Recruits(RecruitIndex);
      RecruitDialog: constant Ttk_Frame :=
        Create(".recruitdialog", "-style Dialog.TFrame");
      YScroll: constant Ttk_Scrollbar :=
        Create
          (RecruitDialog & ".yscroll",
           "-orient vertical -command [list " & RecruitDialog &
           ".canvas yview]");
      RecruitCanvas: constant Tk_Canvas :=
        Create
          (RecruitDialog & ".canvas",
           "-yscrollcommand [list " & YScroll & " set]");
      CloseButton, InfoButton, Button: Ttk_Button;
      Height, NewHeight: Positive := 1;
      Width, NewWidth: Positive := 1;
      ProgressBar: Ttk_ProgressBar;
      TabButton: Ttk_RadioButton;
      Frame: Ttk_Frame := Get_Widget(".gameframe.header");
      RecruitLabel: Ttk_Label;
      ProgressFrame: Ttk_Frame;
   begin
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Get_Widget(".gameframe.paned");
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Create(RecruitDialog & ".buttonbox");
      Tcl_SetVar(Interp, "newtab", "general");
      TabButton :=
        Create
          (Frame & ".general",
           " -text General -state selected -style Radio.Toolbutton -value general -variable newtab -command ShowRecruitTab");
      Tcl.Tk.Ada.Grid.Grid(TabButton);
      Bind
        (TabButton, "<Escape>",
         "{" & RecruitDialog & ".buttonbox2.button invoke;break}");
      Height := Positive'Value(Winfo_Get(TabButton, "reqheight"));
      TabButton :=
        Create
          (Frame & ".stats",
           " -text Statistics -style Radio.Toolbutton -value stats -variable newtab -command ShowRecruitTab");
      Tcl.Tk.Ada.Grid.Grid(TabButton, "-column 1 -row 0");
      Bind
        (TabButton, "<Escape>",
         "{" & RecruitDialog & ".buttonbox2.button invoke;break}");
      TabButton :=
        Create
          (Frame & ".skills",
           " -text Skills -style Radio.Toolbutton -value skills -variable newtab -command ShowRecruitTab");
      Tcl.Tk.Ada.Grid.Grid(TabButton, "-column 2 -row 0");
      Bind
        (TabButton, "<Escape>",
         "{" & RecruitDialog & ".buttonbox2.button invoke;break}");
      TabButton :=
        Create
          (Frame & ".inventory",
           " -text Inventory -style Radio.Toolbutton -value inventory -variable newtab -command ShowRecruitTab");
      Tcl.Tk.Ada.Grid.Grid(TabButton, "-column 3 -row 0");
      Bind
        (TabButton, "<Escape>",
         "{" & RecruitDialog & ".buttonbox2.button invoke;break}");
      Bind
        (TabButton, "<Tab>",
         "{focus " & RecruitDialog & ".buttonbox2.hirebutton;break}");
      Tcl.Tk.Ada.Grid.Grid(Frame, "-pady {5 0} -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid(RecruitCanvas, "-sticky nwes -pady 5 -padx 5");
      Tcl.Tk.Ada.Grid.Grid
        (YScroll, " -sticky ns -pady 5 -padx {0 5} -row 1 -column 1");
      Frame := Create(RecruitDialog & ".buttonbox2");
      Button :=
        Create
          (RecruitDialog & ".buttonbox2.hirebutton",
           "-text Negotiate -command {CloseDialog " & RecruitDialog &
           ";Negotiate}");
      Tcl.Tk.Ada.Grid.Grid(Button);
      CloseButton :=
        Create
          (RecruitDialog & ".buttonbox2.button",
           "-text Close -command {CloseDialog " & RecruitDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      Tcl.Tk.Ada.Grid.Grid(Frame, "-pady {0 5}");
      Focus(CloseButton);
      Autoscroll(YScroll);
      -- General info about the selected recruit
      Frame := Create(RecruitCanvas & ".general");
      if not Factions_List(Recruit.Faction).Flags.Contains
          (To_Unbounded_String("nogender")) then
         RecruitInfo :=
           (if Recruit.Gender = 'M' then To_Unbounded_String("Gender: Male")
            else To_Unbounded_String("Gender: Female"));
      end if;
      Append(RecruitInfo, LF & "Faction: ");
      Append(RecruitInfo, Factions_List(Recruit.Faction).Name);
      Append(RecruitInfo, LF & "Home base: ");
      Append(RecruitInfo, SkyBases(Recruit.HomeBase).Name);
      RecruitLabel :=
        Create
          (Frame & ".label",
           "-text {" & To_String(RecruitInfo) & "} -wraplength 400");
      Tcl.Tk.Ada.Grid.Grid(RecruitLabel, "-sticky w");
      Height := Height + Positive'Value(Winfo_Get(RecruitLabel, "reqheight"));
      Width := Positive'Value(Winfo_Get(RecruitLabel, "reqwidth"));
      Tcl.Tk.Ada.Grid.Grid(Frame);
      -- Statistics of the selected recruit
      Frame := Create(RecruitCanvas & ".stats");
      for I in Recruit.Attributes.Iterate loop
         ProgressFrame :=
           Create
             (Frame & ".statinfo" &
              Trim(Positive'Image(Attributes_Container.To_Index(I)), Left));
         RecruitLabel :=
           Create
             (ProgressFrame & ".label",
              "-text {" &
              To_String
                (Attributes_List(Attributes_Container.To_Index(I)).Name) &
              ": " & GetAttributeLevelName(Recruit.Attributes(I)(1)) & "}");
         Tcl.Tk.Ada.Grid.Grid(RecruitLabel);
         InfoButton :=
           Create
             (ProgressFrame & ".button",
              "-text ""[format %c 0xf05a]"" -style Header.Toolbutton -command {ShowCrewStatsInfo" &
              Positive'Image(Attributes_Container.To_Index(I)) &
              " .recruitdialog}");
         Tcl.Tklib.Ada.Tooltip.Add
           (InfoButton,
            "Show detailed information about the selected statistic.");
         Tcl.Tk.Ada.Grid.Grid(InfoButton, "-column 1 -row 0");
         NewHeight :=
           NewHeight + Positive'Value(Winfo_Get(InfoButton, "reqheight"));
         Tcl.Tk.Ada.Grid.Grid(ProgressFrame);
         ProgressBar :=
           Create
             (Frame & ".level" &
              Trim(Positive'Image(Attributes_Container.To_Index(I)), Left),
              "-value" & Positive'Image(Recruit.Attributes(I)(1) * 2) &
              " -length 200");
         Tcl.Tklib.Ada.Tooltip.Add
           (ProgressBar, "The current level of the attribute.");
         Tcl.Tk.Ada.Grid.Grid(ProgressBar);
         NewHeight :=
           NewHeight + Positive'Value(Winfo_Get(ProgressBar, "reqheight"));
      end loop;
      if NewHeight > Height then
         Height := NewHeight;
      end if;
      -- Skills of the selected recruit
      Frame := Create(RecruitCanvas & ".skills");
      NewHeight := 1;
      for I in Recruit.Skills.Iterate loop
         ProgressFrame :=
           Create
             (Frame & ".skillinfo" &
              Trim(Positive'Image(Skills_Container.To_Index(I)), Left));
         RecruitLabel :=
           Create
             (ProgressFrame & ".label" &
              Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
              "-text {" & To_String(Skills_List(Recruit.Skills(I)(1)).Name) &
              ": " & GetSkillLevelName(Recruit.Skills(I)(2)) & "}");
         Tcl.Tk.Ada.Grid.Grid(RecruitLabel);
         declare
            ToolQuality: Positive := 100;
         begin
            Tool_Quality_Loop :
            for Quality of Skills_List(Skills_Container.To_Index(I))
              .ToolsQuality loop
               if Recruit.Skills(I)(2) <= Quality(1) then
                  ToolQuality := Quality(2);
                  exit Tool_Quality_Loop;
               end if;
            end loop Tool_Quality_Loop;
            InfoButton :=
              Create
                (ProgressFrame & ".button",
                 "-text ""[format %c 0xf05a]"" -style Header.Toolbutton -command {ShowCrewSkillInfo" &
                 Positive'Image(Recruit.Skills(I)(1)) &
                 Positive'Image(ToolQuality) & " .recruitdialog}");
         end;
         Tcl.Tklib.Ada.Tooltip.Add
           (InfoButton, "Show detailed information about the selected skill.");
         Tcl.Tk.Ada.Grid.Grid(InfoButton, "-column 1 -row 0");
         NewHeight :=
           NewHeight + Positive'Value(Winfo_Get(InfoButton, "reqheight"));
         Tcl.Tk.Ada.Grid.Grid(ProgressFrame);
         ProgressBar :=
           Create
             (Frame & ".level" &
              Trim(Positive'Image(Skills_Container.To_Index(I)), Left),
              "-value" & Positive'Image(Recruit.Skills(I)(2)) &
              " -length 200");
         Tcl.Tklib.Ada.Tooltip.Add
           (ProgressBar, "The current level of the skill.");
         Tcl.Tk.Ada.Grid.Grid(ProgressBar);
         NewHeight :=
           NewHeight + Positive'Value(Winfo_Get(ProgressBar, "reqheight"));
      end loop;
      if NewHeight > Height then
         Height := NewHeight;
      end if;
      -- Equipment of the selected recruit
      Frame := Create(RecruitCanvas & ".inventory");
      NewHeight := 1;
      RecruitInfo := Null_Unbounded_String;
      for Item of Recruit.Inventory loop
         Append(RecruitInfo, Items_List(Item).Name & LF);
      end loop;
      RecruitLabel :=
        Create
          (Frame & ".label",
           "-text {" & To_String(RecruitInfo) & "} -wraplength 400");
      Tcl.Tk.Ada.Grid.Grid(RecruitLabel, "-sticky w");
      NewHeight := Positive'Value(Winfo_Get(RecruitLabel, "reqheight"));
      if NewHeight > Height then
         Height := NewHeight;
      end if;
      NewWidth := Positive'Value(Winfo_Get(RecruitLabel, "reqwidth"));
      if NewWidth > Width then
         Width := NewWidth;
      end if;
      if Height > 500 then
         Height := 500;
      end if;
      if Width < 350 then
         Width := 350;
      end if;
      Frame := Get_Widget(RecruitCanvas & ".general");
      declare
         XPos: constant Natural :=
           (Positive'Value(Winfo_Get(RecruitCanvas, "reqwidth")) -
            Positive'Value(Winfo_Get(Frame, "reqwidth"))) /
           4;
      begin
         Canvas_Create
           (RecruitCanvas, "window",
            Trim(Natural'Image(XPos), Left) & " 0 -anchor nw -window " &
            Frame & " -tag info");
      end;
      Tcl_Eval(Interp, "update");
      configure
        (RecruitCanvas,
         "-scrollregion [list " & BBox(RecruitCanvas, "all") & "] -width" &
         Positive'Image(Width) & " -height" & Positive'Image(Height));
      Tcl.Tk.Ada.Place.Place
        (RecruitDialog, "-in .gameframe -relx 0.3 -rely 0.2");
      Bind
        (CloseButton, "<Tab>",
         "{focus " & RecruitDialog & ".buttonbox.general;break}");
      Bind(RecruitDialog, "<Escape>", "{" & CloseButton & " invoke;break}");
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      return TCL_OK;
   end Show_Recruit_Info_Command;

   -- ****o* RecruitUI/RecruitUI.Negotiate_Hire_Command
   -- FUNCTION
   -- Show information about the selected recruit
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- NegotiateHire
   -- SOURCE
   function Negotiate_Hire_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Negotiate_Hire_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MoneyIndex2: constant Natural := FindItem(PlayerShip.Cargo, MoneyIndex);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Recruit: constant Recruit_Data :=
        SkyBases(BaseIndex).Recruits(RecruitIndex);
      Cost: Integer;
      Scale: Ttk_Scale := Get_Widget(".negotiatedialog.daily", Interp);
      DailyPayment: constant Natural :=
        Natural(Float'Value(cget(Scale, "-value")));
      ContractBox: constant Ttk_ComboBox :=
        Get_Widget(".negotiatedialog.contract", Interp);
      ContractLength: constant Natural := Natural'Value(Current(ContractBox));
      TradePayment: Natural;
      Label: Ttk_Label := Get_Widget(".negotiatedialog.cost", Interp);
      HireButton: constant Ttk_Button :=
        Get_Widget(".negotiatedialog.buttonbox.hirebutton", Interp);
   begin
      Scale.Name := New_String(".negotiatedialog.percent");
      TradePayment := Natural(Float'Value(cget(Scale, "-value")));
      Cost :=
        Recruit.Price - ((DailyPayment - Recruit.Payment) * 50) -
        (TradePayment * 5000);
      case ContractLength is
         when 1 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.1);
         when 2 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.5);
         when 3 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.75);
         when 4 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.9);
         when others =>
            null;
      end case;
      if Cost < 1 then
         Cost := 1;
      end if;
      CountPrice(Cost, FindMember(Talk));
      configure
        (Label,
         "-text {Hire for" & Natural'Image(Cost) & " " & To_String(MoneyName) &
         "}");
      Label.Name := New_String(".negotiatedialog.dailylbl");
      configure
        (Label, "-text {Daily payment:" & Natural'Image(DailyPayment) & "}");
      Label.Name := New_String(".negotiatedialog.percentlbl");
      configure
        (Label,
         "-text {Percent of profit from trades: " &
         Natural'Image(TradePayment) & "}");
      if MoneyIndex2 > 0 then
         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
            configure(HireButton, "-state disabled");
         else
            configure(HireButton, "-state !disabled");
         end if;
      end if;
      return TCL_OK;
   end Negotiate_Hire_Command;

   -- ****o* RecruitUI/RecruitUI.Hire_Command
   -- FUNCTION
   -- Hire the selected recruit
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Hire
   -- SOURCE
   function Hire_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Hire_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
      Cost, ContractLength2: Integer;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Recruit: constant Recruit_Data :=
        SkyBases(BaseIndex).Recruits(RecruitIndex);
      Scale: Ttk_Scale := Get_Widget(".negotiatedialog.daily", Interp);
      DailyPayment: constant Natural :=
        Natural(Float'Value(cget(Scale, "-value")));
      ContractBox: constant Ttk_ComboBox :=
        Get_Widget(".negotiatedialog.contract", Interp);
      ContractLength: constant Natural := Natural'Value(Current(ContractBox));
      TradePayment: Natural;
   begin
      Scale.Name := New_String(".negotiatedialog.percent");
      TradePayment := Natural(Float'Value(cget(Scale, "-value")));
      Cost :=
        Recruit.Price - ((DailyPayment - Recruit.Payment) * 50) -
        (TradePayment * 5000);
      case ContractLength is
         when 1 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.1);
            ContractLength2 := 100;
         when 2 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.5);
            ContractLength2 := 30;
         when 3 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.75);
            ContractLength2 := 20;
         when 4 =>
            Cost := Cost - Integer(Float(Recruit.Price) * 0.9);
            ContractLength2 := 10;
         when others =>
            ContractLength2 := -1;
      end case;
      if Cost < 1 then
         Cost := 1;
      end if;
      HireRecruit
        (RecruitIndex, Cost, DailyPayment, TradePayment, ContractLength2);
      UpdateMessages;
      return Show_Recruit_Command(ClientData, Interp, 2, Argv);
   end Hire_Command;

   -- ****o* RecruitUI/RecruitUI.Show_Recruit_Tab_Command
   -- FUNCTION
   -- Show the selected information about the selected recruit
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowMemberTab
   -- SOURCE
   function Show_Recruit_Tab_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Tab_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      RecruitCanvas: constant Tk_Canvas :=
        Get_Widget(".recruitdialog.canvas", Interp);
      Frame: constant Ttk_Frame :=
        Get_Widget(RecruitCanvas & "." & Tcl_GetVar(Interp, "newtab"));
      XPos: constant Natural :=
        (Positive'Value(Winfo_Get(RecruitCanvas, "reqwidth")) -
         Positive'Value(Winfo_Get(Frame, "reqwidth"))) /
        2;
   begin
      Delete(RecruitCanvas, "info");
      Canvas_Create
        (RecruitCanvas, "window",
         Trim(Positive'Image(XPos), Left) & " 0 -anchor nw -window " & Frame &
         " -tag info");
      Tcl_Eval(Interp, "update");
      configure
        (RecruitCanvas,
         "-scrollregion [list " & BBox(RecruitCanvas, "all") & "]");
      return TCL_OK;
   end Show_Recruit_Tab_Command;

   -- ****o* RecruitUI/RecruitUI.Negotiate_Command
   -- FUNCTION
   -- Show negotation UI to the player
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Negotiate
   -- SOURCE
   function Negotiate_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Negotiate_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp;
      Argc: Interfaces.C.int; Argv: CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      NegotiateDialog: constant Ttk_Frame :=
        Create(".negotiatedialog", "-style Dialog.TFrame");
      CloseButton, HireButton: Ttk_Button;
      Frame: Ttk_Frame := Get_Widget(".gameframe.header", Interp);
      Label: Ttk_Label;
      Scale: Ttk_Scale;
      ContractBox: constant Ttk_ComboBox :=
        Create
          (NegotiateDialog & ".contract",
           "-state readonly -values [list {Pernament} {100 days} {30 days} {20 days} {10 days}]");
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Recruit: constant Recruit_Data :=
        SkyBases(BaseIndex).Recruits(RecruitIndex);
      MoneyIndex2: constant Natural := FindItem(PlayerShip.Cargo, MoneyIndex);
      Cost: Positive;
   begin
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Frame := Get_Widget(".gameframe.paned", Interp);
      Tcl.Tk.Ada.Busy.Busy(Frame);
      Label :=
        Create
          (NegotiateDialog & ".dailylbl",
           "-text {Daily payment:" & Natural'Image(Recruit.Payment) & "}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-pady {5 0}");
      Scale :=
        Create(NegotiateDialog & ".daily", "-from 0 -command NegotiateHire");
      Tcl.Tk.Ada.Grid.Grid(Scale);
      configure
        (Scale,
         "-to" & Natural'Image(Recruit.Payment * 2) & " -value" &
         Natural'Image(Recruit.Payment));
      Label :=
        Create
          (NegotiateDialog & ".percentlbl",
           "-text {Percent of profit from trades: 0}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-padx 5");
      Scale :=
        Create
          (NegotiateDialog & ".percent",
           "-from 0 -to 10 -command NegotiateHire");
      Tcl.Tk.Ada.Grid.Grid(Scale);
      configure(Scale, "-value 0");
      Label :=
        Create(NegotiateDialog & ".contractlbl", "-text {Contract time:}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Tcl.Tk.Ada.Grid.Grid(ContractBox);
      Bind(ContractBox, "<<ComboboxSelected>>", "{NegotiateHire}");
      Current(ContractBox, "0");
      Frame := Create(NegotiateDialog & ".buttonbox");
      HireButton :=
        Create
          (NegotiateDialog & ".buttonbox.hirebutton",
           "-text Hire -command {Hire}");
      Label := Create(NegotiateDialog & ".money");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Cost := Recruit.Price;
      CountPrice(Cost, FindMember(Talk));
      if MoneyIndex2 > 0 then
         configure
           (Label,
            "-text {You have" &
            Natural'Image(PlayerShip.Cargo(MoneyIndex2).Amount) & " " &
            To_String(MoneyName) & ".}");
         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
            configure(HireButton, "-state disabled");
         else
            configure(HireButton, "-state !disabled");
         end if;
      else
         configure
           (Label, "-text {You don't have enough money to recruit anyone}");
         configure(HireButton, "-state disabled");
      end if;
      Label := Create(NegotiateDialog & ".cost");
      Tcl.Tk.Ada.Grid.Grid(Label);
      configure
        (Label,
         "-text {Hire for" & Positive'Image(Cost) & " " &
         To_String(MoneyName) & "}");
      Tcl.Tk.Ada.Grid.Grid(HireButton);
      CloseButton :=
        Create
          (NegotiateDialog & ".buttonbox.button",
           "-text Close -command {CloseDialog " & NegotiateDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      Tcl.Tk.Ada.Grid.Grid(Frame, "-pady {0 5}");
      Focus(CloseButton);
      Tcl.Tk.Ada.Place.Place
        (NegotiateDialog, "-in .gameframe -relx 0.3 -rely 0.2");
      Bind(CloseButton, "<Tab>", "{focus " & HireButton & ";break}");
      Bind(HireButton, "<Tab>", "{focus " & CloseButton & ";break}");
      Bind(NegotiateDialog, "<Escape>", "{" & CloseButton & " invoke;break}");
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      return TCL_OK;
   end Negotiate_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowRecruit", Show_Recruit_Command'Access);
      AddCommand("ShowRecruitMenu", Show_Recruit_Menu_Command'Access);
      AddCommand("ShowRecruitInfo", Show_Recruit_Info_Command'Access);
      AddCommand("NegotiateHire", Negotiate_Hire_Command'Access);
      AddCommand("Hire", Hire_Command'Access);
      AddCommand("ShowRecruitTab", Show_Recruit_Tab_Command'Access);
      AddCommand("Negotiate", Negotiate_Command'Access);
   end AddCommands;

end Bases.RecruitUI;
