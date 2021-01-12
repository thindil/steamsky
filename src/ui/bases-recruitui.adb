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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkLabelFrame; use Tcl.Tk.Ada.Widgets.TtkLabelFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScale; use Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkTreeView; use Tcl.Tk.Ada.Widgets.TtkTreeView;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
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
   RecruitTable: Table_Widget (5);
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      RecruitFrame: Ttk_Frame := Get_Widget(Paned & ".recruitframe", Interp);
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
   begin
      if Winfo_Get(RecruitFrame, "exists") = "0" then
         RecruitFrame := Create(Widget_Image(RecruitFrame));
         RecruitTable :=
           CreateTable
             (Widget_Image(RecruitFrame),
              (To_Unbounded_String("#"), To_Unbounded_String("Name"),
               To_Unbounded_String("Gender"), To_Unbounded_String("Faction"),
               To_Unbounded_String("Base cost")));
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Info_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      RecruitsView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.recruitframe.canvas.recruit.recruits.view",
           Interp);
      Recruit: Recruit_Data;
      RecruitInfo: Unbounded_String;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Label: Ttk_Label :=
        Get_Widget
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.info.info",
           Interp);
      LabelFrame: Ttk_LabelFrame :=
        Get_Widget
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.info.stats",
           Interp);
      Tokens: Slice_Set;
      Item: Ttk_Frame;
      ProgressBar: Ttk_ProgressBar;
      Row: Natural := 0;
      EquipmentView: constant Ttk_Tree_View :=
        Get_Widget
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.info.equipment.view",
           Interp);
      MoneyIndex2: constant Natural := FindItem(PlayerShip.Cargo, MoneyIndex);
      Cost: Positive;
      Scale: Ttk_Scale;
      HireButton: constant Ttk_Button :=
        Get_Widget
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.hire",
           Interp);
   begin
      RecruitIndex := Positive'Value(Selection(RecruitsView));
      Recruit := SkyBases(BaseIndex).Recruits(RecruitIndex);
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
      configure(Label, "-text {" & To_String(RecruitInfo) & "}");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Slaves(LabelFrame), " ");
      Item.Interp := Interp;
      for I in 1 .. Slice_Count(Tokens) loop
         Item.Name := New_String(Slice(Tokens, I));
         Destroy(Item);
      end loop;
      for I in Recruit.Attributes.Iterate loop
         Label :=
           Create
             (".gameframe.paned.recruitframe.canvas.recruit.recruit.info.stats.label" &
              Trim(Positive'Image(Attributes_Container.To_Index(I)), Left),
              "-text {" &
              To_String
                (Attributes_List(Attributes_Container.To_Index(I)).Name) &
              ": " & GetAttributeLevelName(Recruit.Attributes(I)(1)) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         Add
           (Label,
            To_String
              (Attributes_List(Attributes_Container.To_Index(I)).Description));
         ProgressBar :=
           Create
             (".gameframe.paned.recruitframe.canvas.recruit.recruit.info.stats.levelbar" &
              Trim(Positive'Image(Attributes_Container.To_Index(I)), Left),
              "-value" & Positive'Image(Recruit.Attributes(I)(1) * 2));
         Tcl.Tk.Ada.Grid.Grid
           (ProgressBar,
            "-column 1 -row" &
            Natural'Image(Attributes_Container.To_Index(I) - 1));
         Add
           (ProgressBar,
            To_String
              (Attributes_List(Attributes_Container.To_Index(I)).Description));
      end loop;
      LabelFrame.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.info.skills");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Slaves(LabelFrame), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         Item.Name := New_String(Slice(Tokens, I));
         Destroy(Item);
      end loop;
      for Skill of Recruit.Skills loop
         Label :=
           Create
             (".gameframe.paned.recruitframe.canvas.recruit.recruit.info.skills.label" &
              Trim(Positive'Image(Skill(1)), Left),
              "-text {" & To_String(Skills_List(Skill(1)).Name) & ": " &
              GetSkillLevelName(Skill(2)) & "}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         Add
           (Label,
            "Related statistic: " &
            To_String(Attributes_List(Skills_List(Skill(1)).Attribute).Name) &
            ". " & To_String(Skills_List(Skill(1)).Description));
         ProgressBar :=
           Create
             (".gameframe.paned.recruitframe.canvas.recruit.recruit.info.skills.levelbar" &
              Trim(Positive'Image(Skill(1)), Left),
              "-value" & Positive'Image(Skill(2)));
         Tcl.Tk.Ada.Grid.Grid
           (ProgressBar, "-column 1 -row" & Natural'Image(Row));
         Row := Row + 1;
         Add
           (ProgressBar,
            "Related statistic: " &
            To_String(Attributes_List(Skills_List(Skill(1)).Attribute).Name) &
            ". " & To_String(Skills_List(Skill(1)).Description));
      end loop;
      Delete(EquipmentView, "[list " & Children(EquipmentView, "{}") & "]");
      for Item of Recruit.Inventory loop
         Insert
           (EquipmentView,
            "{} end -text {" & To_String(Items_List(Item).Name) & "}");
      end loop;
      Label.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.info.initialcost");
      RecruitInfo := To_Unbounded_String("Starting offer:");
      Append
        (RecruitInfo,
         LF & "Payment:" & Natural'Image(Recruit.Payment) & " " &
         To_String(MoneyName) & " each day.");
      Cost := Recruit.Price;
      CountPrice(Cost, FindMember(Talk));
      Append
        (RecruitInfo,
         LF & "One time fee:" & Positive'Image(Cost) & " " &
         To_String(MoneyName) & ".");
      configure(Label, "-text {" & To_String(RecruitInfo) & "}");
      Label.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.dailylbl");
      configure
        (Label,
         "-text {Daily payment:" & Natural'Image(Recruit.Payment) & "}");
      Scale.Interp := Interp;
      Scale.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.daily");
      configure
        (Scale,
         "-to" & Natural'Image(Recruit.Payment * 2) & " -value" &
         Natural'Image(Recruit.Payment));
      Label.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.percentlbl");
      configure(Label, "-text {Percent of profit from trades: 0}");
      Scale.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.percent");
      configure(Scale, "-value 0");
      Cost := Recruit.Price;
      CountPrice(Cost, FindMember(Talk));
      Label.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.cost");
      configure
        (Label,
         "-text {Hire for" & Positive'Image(Cost) & " " &
         To_String(MoneyName) & "}");
      Label.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.money");
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Negotiate_Hire_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      MoneyIndex2: constant Natural := FindItem(PlayerShip.Cargo, MoneyIndex);
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Recruit: constant Recruit_Data :=
        SkyBases(BaseIndex).Recruits(RecruitIndex);
      Cost: Integer;
      Scale: Ttk_Scale :=
        Get_Widget
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.daily",
           Interp);
      DailyPayment: constant Natural :=
        Natural(Float'Value(cget(Scale, "-value")));
      ContractBox: constant Ttk_ComboBox :=
        Get_Widget
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.contract",
           Interp);
      ContractLength: constant Natural := Natural'Value(Current(ContractBox));
      TradePayment: Natural;
      Label: Ttk_Label :=
        Get_Widget
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.cost",
           Interp);
      HireButton: constant Ttk_Button :=
        Get_Widget
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.hire",
           Interp);
   begin
      Scale.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.percent");
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
      Label.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.dailylbl");
      configure
        (Label, "-text {Daily payment:" & Natural'Image(DailyPayment) & "}");
      Label.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.percentlbl");
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Hire_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(Argc);
      Cost, ContractLength2: Integer;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Recruit: constant Recruit_Data :=
        SkyBases(BaseIndex).Recruits(RecruitIndex);
      Scale: Ttk_Scale :=
        Get_Widget
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.daily",
           Interp);
      DailyPayment: constant Natural :=
        Natural(Float'Value(cget(Scale, "-value")));
      ContractBox: constant Ttk_ComboBox :=
        Get_Widget
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.contract",
           Interp);
      ContractLength: constant Natural := Natural'Value(Current(ContractBox));
      TradePayment: Natural;
   begin
      Scale.Name :=
        New_String
          (".gameframe.paned.recruitframe.canvas.recruit.recruit.percent");
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

   procedure AddCommands is
   begin
      AddCommand("ShowRecruit", Show_Recruit_Command'Access);
      AddCommand("ShowRecruitInfo", Show_Recruit_Info_Command'Access);
      AddCommand("NegotiateHire", Negotiate_Hire_Command'Access);
      AddCommand("Hire", Hire_Command'Access);
   end AddCommands;

end Bases.RecruitUI;
