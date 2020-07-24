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

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
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
with Utils.UI; use Utils.UI;

package body Bases.RecruitUI is

   -- ****f* RecruitUI/Show_Recruit_Command
   -- FUNCTION
   -- Show the selected base available recruits
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      Label: Ttk_Label;
      Paned: Ttk_PanedWindow;
      RecruitCanvas: Tk_Canvas;
      RecruitFrame: Ttk_Frame;
      CloseButton: Ttk_Button;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      RecruitsView: Ttk_Tree_View;
   begin
      Paned.Interp := Interp;
      Paned.Name := New_String(".paned");
      CloseButton.Interp := Interp;
      CloseButton.Name := New_String(".header.closebutton");
      RecruitFrame.Interp := Interp;
      RecruitFrame.Name := New_String(Widget_Image(Paned) & ".recruitframe");
      RecruitCanvas.Interp := Interp;
      RecruitCanvas.Name := New_String(Widget_Image(RecruitFrame) & ".canvas");
      Label.Interp := Interp;
      Label.Name :=
        New_String(Widget_Image(RecruitCanvas) & ".recruit.recruit.info.info");
      if Winfo_Get(Label, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "recruit.tcl");
         Bind(RecruitFrame, "<Configure>", "{ResizeCanvas %W.canvas %w %h}");
      elsif Winfo_Get(Label, "ismapped") = "1" and
        (Argc = 1 or SkyBases(BaseIndex).Recruits.Length = 0) then
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         ShowSkyMap(True);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp crew}");
      RecruitsView.Interp := Interp;
      RecruitsView.Name :=
        New_String(Widget_Image(RecruitCanvas) & ".recruit.recruits.view");
      Delete(RecruitsView, "[list " & Children(RecruitsView, "{}") & "]");
      for I in SkyBases(BaseIndex).Recruits.Iterate loop
         Insert
           (RecruitsView,
            "{} end -id" & Positive'Image(Recruit_Container.To_Index(I)) &
            " -text {" & To_String(SkyBases(BaseIndex).Recruits(I).Name) &
            "}");
      end loop;
      Selection_Set(RecruitsView, "[list 1]");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      RecruitFrame.Name :=
        New_String(Widget_Image(RecruitCanvas) & ".recruit");
      configure
        (RecruitCanvas,
         "-height [expr " & SashPos(Paned, "0") & " - 20] -width " &
         cget(Paned, "-width"));
      Tcl_Eval(Get_Context, "update");
      Canvas_Create
        (RecruitCanvas, "window",
         "[expr " & Winfo_Get(RecruitFrame, "reqwidth") & " / 2] [expr " &
         Winfo_Get(RecruitFrame, "reqheight") & " / 2] -window " &
         Widget_Image(RecruitFrame));
      Tcl_Eval(Get_Context, "update");
      configure
        (RecruitCanvas,
         "-scrollregion [list " & BBox(RecruitCanvas, "all") & "]");
      ShowScreen("recruitframe");
      return TCL_OK;
   end Show_Recruit_Command;

   -- ****iv* RecruitUI/RecruitIndex
   -- FUNCTION
   -- The index of currently selected recruit
   -- SOURCE
   RecruitIndex: Positive;
   -- ****

   -- ****f* RecruitUI/Show_Recruit_Info_Command
   -- FUNCTION
   -- Show information about the selected recruit
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      RecruitsView: Ttk_Tree_View;
      Recruit: Recruit_Data;
      RecruitInfo: Unbounded_String;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Label: Ttk_Label;
      LabelFrame: Ttk_LabelFrame;
      Tokens: Slice_Set;
      Item: Ttk_Frame;
      ProgressBar: Ttk_ProgressBar;
      Row: Natural := 0;
      EquipmentView: Ttk_Tree_View;
      MoneyIndex2: constant Natural := FindItem(PlayerShip.Cargo, MoneyIndex);
      Cost: Positive;
      Scale: Ttk_Scale;
      HireButton: Ttk_Button;
   begin
      RecruitsView.Interp := Interp;
      RecruitsView.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruits.view");
      RecruitIndex := Positive'Value(Selection(RecruitsView));
      Recruit := SkyBases(BaseIndex).Recruits(RecruitIndex);
      if not Factions_List(Recruit.Faction).Flags.Contains
          (To_Unbounded_String("nogender")) then
         if Recruit.Gender = 'M' then
            RecruitInfo := To_Unbounded_String("Gender: Male");
         else
            RecruitInfo := To_Unbounded_String("Gender: Female");
         end if;
      end if;
      Append(RecruitInfo, LF & "Faction: ");
      Append(RecruitInfo, Factions_List(Recruit.Faction).Name);
      Append(RecruitInfo, LF & "Home base: ");
      Append(RecruitInfo, SkyBases(Recruit.HomeBase).Name);
      Label.Interp := Interp;
      Label.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.info.info");
      configure(Label, "-text {" & To_String(RecruitInfo) & "}");
      LabelFrame.Interp := Interp;
      LabelFrame.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.info.stats");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Slaves(LabelFrame), " ");
      Item.Interp := Interp;
      for I in 1 .. Slice_Count(Tokens) loop
         Item.Name := New_String(Slice(Tokens, I));
         Destroy(Item);
      end loop;
      for I in Recruit.Attributes.Iterate loop
         Label :=
           Create
             (".paned.recruitframe.canvas.recruit.recruit.info.stats.label" &
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
             (".paned.recruitframe.canvas.recruit.recruit.info.stats.levelbar" &
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
        New_String(".paned.recruitframe.canvas.recruit.recruit.info.skills");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Slaves(LabelFrame), " ");
      for I in 1 .. Slice_Count(Tokens) loop
         Item.Name := New_String(Slice(Tokens, I));
         Destroy(Item);
      end loop;
      for Skill of Recruit.Skills loop
         Label :=
           Create
             (".paned.recruitframe.canvas.recruit.recruit.info.skills.label" &
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
             (".paned.recruitframe.canvas.recruit.recruit.info.skills.levelbar" &
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
      EquipmentView.Interp := Interp;
      EquipmentView.Name :=
        New_String
          (".paned.recruitframe.canvas.recruit.recruit.info.equipment.view");
      Delete(EquipmentView, "[list " & Children(EquipmentView, "{}") & "]");
      for Item of Recruit.Inventory loop
         Insert
           (EquipmentView,
            "{} end -text {" & To_String(Items_List(Item).Name) & "}");
      end loop;
      Label.Name :=
        New_String
          (".paned.recruitframe.canvas.recruit.recruit.info.initialcost");
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
        New_String(".paned.recruitframe.canvas.recruit.recruit.dailylbl");
      configure
        (Label,
         "-text {Daily payment:" & Natural'Image(Recruit.Payment) & "}");
      Scale.Interp := Interp;
      Scale.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.daily");
      configure
        (Scale,
         "-to" & Natural'Image(Recruit.Payment * 2) & " -value" &
         Natural'Image(Recruit.Payment));
      Label.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.percentlbl");
      configure(Label, "-text {Percent of profit from trades: 0}");
      Scale.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.percent");
      configure(Scale, "-value 0");
      Cost := Recruit.Price;
      CountPrice(Cost, FindMember(Talk));
      Label.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.cost");
      configure
        (Label,
         "-text {Hire for" & Positive'Image(Cost) & " " &
         To_String(MoneyName) & "}");
      Label.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.money");
      HireButton.Interp := Interp;
      HireButton.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.hire");
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

   -- ****f* RecruitUI/Negotiate_Hire_Command
   -- FUNCTION
   -- Show information about the selected recruit
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      Recruit: Recruit_Data;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Cost: Integer;
      Scale: Ttk_Scale;
      ContractBox: Ttk_ComboBox;
      DailyPayment, TradePayment, ContractLength: Natural;
      Label: Ttk_Label;
      HireButton: Ttk_Button;
   begin
      Recruit := SkyBases(BaseIndex).Recruits(RecruitIndex);
      Scale.Interp := Interp;
      Scale.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.daily");
      DailyPayment := Natural(Float'Value(cget(Scale, "-value")));
      Scale.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.percent");
      TradePayment := Natural(Float'Value(cget(Scale, "-value")));
      Cost :=
        Recruit.Price - ((DailyPayment - Recruit.Payment) * 50) -
        (TradePayment * 5000);
      ContractBox.Interp := Interp;
      ContractBox.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.contract");
      ContractLength := Natural'Value(Current(ContractBox));
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
      Label.Interp := Interp;
      Label.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.cost");
      configure
        (Label,
         "-text {Hire for" & Natural'Image(Cost) & " " & To_String(MoneyName) &
         "}");
      Label.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.dailylbl");
      configure
        (Label, "-text {Daily payment:" & Natural'Image(DailyPayment) & "}");
      Label.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.percentlbl");
      configure
        (Label,
         "-text {Percent of profit from trades: " &
         Natural'Image(TradePayment) & "}");
      HireButton.Interp := Interp;
      HireButton.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.hire");
      if MoneyIndex2 > 0 then
         if PlayerShip.Cargo(MoneyIndex2).Amount < Cost then
            configure(HireButton, "-state disabled");
         else
            configure(HireButton, "-state !disabled");
         end if;
      end if;
      return TCL_OK;
   end Negotiate_Hire_Command;

   -- ****f* RecruitUI/Hire_Command
   -- FUNCTION
   -- Hire the selected recruit
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
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
      Recruit: Recruit_Data;
      BaseIndex: constant Positive :=
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex;
      Scale: Ttk_Scale;
      ContractBox: Ttk_ComboBox;
      DailyPayment, TradePayment, ContractLength: Natural;
   begin
      Recruit := SkyBases(BaseIndex).Recruits(RecruitIndex);
      Scale.Interp := Interp;
      Scale.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.daily");
      DailyPayment := Natural(Float'Value(cget(Scale, "-value")));
      Scale.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.percent");
      TradePayment := Natural(Float'Value(cget(Scale, "-value")));
      Cost :=
        Recruit.Price - ((DailyPayment - Recruit.Payment) * 50) -
        (TradePayment * 5000);
      ContractBox.Interp := Interp;
      ContractBox.Name :=
        New_String(".paned.recruitframe.canvas.recruit.recruit.contract");
      ContractLength := Natural'Value(Current(ContractBox));
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
