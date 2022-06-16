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
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with CArgv; use CArgv;
with Tcl; use Tcl;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
use Tcl.Tk.Ada.Widgets.TtkButton.TtkRadioButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkProgressBar; use Tcl.Tk.Ada.Widgets.TtkProgressBar;
with Tcl.Tk.Ada.Widgets.TtkScale; use Tcl.Tk.Ada.Widgets.TtkScale;
with Tcl.Tk.Ada.Widgets.TtkScrollbar; use Tcl.Tk.Ada.Widgets.TtkScrollbar;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Autoscroll; use Tcl.Tklib.Ada.Autoscroll;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases.Trade; use Bases.Trade;
with Config; use Config;
with CoreUI; use CoreUI;
with Dialogs; use Dialogs;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships.Crew; use Ships.Crew;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Bases.RecruitUI is

   -- ****iv* RecruitUI/RecruitUI.Recruit_Table
   -- FUNCTION
   -- Table with info about the available recruits
   -- SOURCE
   Recruit_Table: Table_Widget (Amount => 6);
   -- ****

   -- ****iv* RecruitUI/RecruitUI.Modules_Indexes
   -- FUNCTION
   -- Indexes of the available recruits in base
   -- SOURCE
   Recruits_Indexes: Positive_Container.Vector;
   -- ****

   -- ****if* RecruitUI/RecruitUI.Get_Highest_Attribute
   -- FUNCTION
   -- Get the highest attribute's name of the selected recruit
   -- PARAMETERS
   -- Base_Index   - The index of the base in which the recruit's attributes
   --                will be check
   -- Member_Index - The index of the recruit which attributes will be check
   -- RESULT
   -- The name of the attribute with the highest level of the selected recruit
   -- HISTORY
   -- 6.5 - Added
   -- 7.6 - Renamed parameters to Base_Index and Member_Index
   -- SOURCE
   function Get_Highest_Attribute
     (Base_Index, Member_Index: Positive) return Unbounded_String is
     -- ****
      use Tiny_String;

      Highest_Level, Highest_Index: Positive := 1;
   begin
      Get_Highest_Attribute_Level_Loop :
      for I in Recruit_Container.Element
        (Container => Sky_Bases(Base_Index).Recruits, Index => Member_Index)
        .Attributes'
        Range loop
         if Recruit_Container.Element
             (Container => Sky_Bases(Base_Index).Recruits,
              Index => Member_Index)
             .Attributes
             (I)
             .Level >
           Highest_Level then
            Highest_Level :=
              Recruit_Container.Element
                (Container => Sky_Bases(Base_Index).Recruits,
                 Index => Member_Index)
                .Attributes
                (I)
                .Level;
            Highest_Index := I;
         end if;
      end loop Get_Highest_Attribute_Level_Loop;
      return
        To_Unbounded_String
          (Source =>
             To_String
               (Source =>
                  AttributesData_Container.Element
                    (Container => Attributes_List, Index => Highest_Index)
                    .Name));
   end Get_Highest_Attribute;

   -- ****if* RecruitUI/RecruitUI.Get_Highest_Skill
   -- FUNCTION
   -- Get the highest skill's name of the selected recruit
   -- PARAMETERS
   -- Base_Index   - The index of the base in which the recruit's skills will
   --                be check
   -- Member_Index - The index of the recruit which skills will be check
   -- RESULT
   -- The name of the skill with the highest level of the selected recruit
   -- HISTORY
   -- 6.5 - Added
   -- 7.6 - Renamed parameters to Base_Index and Member_Index
   -- SOURCE
   function Get_Highest_Skill
     (Base_Index, Member_Index: Positive) return Unbounded_String is
     -- ****
      use Tiny_String;

      Highest_Level: Positive := 1;
      Highest_Index: Skills_Amount_Range := 1;
   begin
      Get_Highest_Skill_Level_Loop :
      for Skill of Recruit_Container.Element
        (Container => Sky_Bases(Base_Index).Recruits, Index => Member_Index)
        .Skills loop
         if Skill.Level > Highest_Level then
            Highest_Level := Skill.Level;
            Highest_Index := Skill.Index;
         end if;
      end loop Get_Highest_Skill_Level_Loop;
      return
        To_Unbounded_String
          (Source =>
             To_String
               (Source =>
                  SkillsData_Container.Element
                    (Container => Skills_List, Index => Highest_Index)
                    .Name));
   end Get_Highest_Skill;

   -- ****o* RecruitUI/RecruitUI.Show_Recruit_Command
   -- FUNCTION
   -- Show the selected base available recruits
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command.
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecruit
   -- SOURCE
   function Show_Recruit_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data);
      use Tiny_String;

      Recruit_Frame: Ttk_Frame :=
        Get_Widget(pathName => Main_Paned & ".recruitframe", Interp => Interp);
      Base_Index: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Page: constant Positive :=
        (if Argc = 2 then Positive'Value(CArgv.Arg(Argv => Argv, N => 1))
         else 1);
      Start_Row: constant Positive :=
        ((Page - 1) * Game_Settings.Lists_Limit) + 1;
      Current_Row: Positive := 1;
   begin
      if Winfo_Get(Widgt => Recruit_Frame, Info => "exists") = "0" then
         Recruit_Frame :=
           Create(pathName => Widget_Image(Win => Recruit_Frame));
         Recruit_Table :=
           Create_Table
             (Parent => Widget_Image(Win => Recruit_Frame),
              Headers =>
                (1 => To_Unbounded_String(Source => "Name"),
                 2 => To_Unbounded_String(Source => "Gender"),
                 3 => To_Unbounded_String(Source => "Faction"),
                 4 => To_Unbounded_String(Source => "Base cost"),
                 5 => To_Unbounded_String(Source => "Highest stat"),
                 6 => To_Unbounded_String(Source => "Highest skill")),
              Command => "SortRecruits",
              Tooltip => "Press mouse button to sort the recruits.");
         Bind
           (Widgt => Recruit_Frame, Sequence => "<Configure>",
            Script => "{ResizeCanvas " & Recruit_Table.Canvas & " %w %h}");
      elsif Winfo_Get(Widgt => Recruit_Frame, Info => "ismapped") = "1" and
        (Argc = 1 or
         Recruit_Container.Length
             (Container => Sky_Bases(Base_Index).Recruits) =
           0) then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Close_Button);
         Show_Sky_Map(Clear => True);
         return TCL_OK;
      end if;
      Tcl_SetVar
        (interp => Interp, varName => "gamestate", newValue => "recruit");
      Tcl.Tk.Ada.Grid.Grid
        (Slave => Close_Button, Options => "-row 0 -column 1");
      if Recruits_Indexes.Length /=
        Recruit_Container.Length
          (Container => Sky_Bases(Base_Index).Recruits) then
         Recruits_Indexes.Clear;
         Fill_Recruit_Indexes_Loop :
         for I in
           Recruit_Container.First_Index
             (Container => Sky_Bases(Base_Index).Recruits) ..
             Recruit_Container.Last_Index
               (Container => Sky_Bases(Base_Index).Recruits) loop
            Recruits_Indexes.Append(New_Item => I);
         end loop Fill_Recruit_Indexes_Loop;
      end if;
      Clear_Table(Table => Recruit_Table);
      Load_Recruits_Loop :
      for I of Recruits_Indexes loop
         if Current_Row < Start_Row then
            Current_Row := Current_Row + 1;
            goto End_Of_Loop;
         end if;
         Add_Button
           (Table => Recruit_Table,
            Text =>
              Tiny_String.To_String
                (Source =>
                   Recruit_Container.Element
                     (Container => Sky_Bases(Base_Index).Recruits, Index => I)
                     .Name),
            Tooltip => "Show available options for recruit",
            Command => "ShowRecruitMenu" & Positive'Image(I), Column => 1);
         Add_Button
           (Table => Recruit_Table,
            Text => (if
               Recruit_Container.Element
                 (Container => Sky_Bases(Base_Index).Recruits, Index => I)
                 .Gender =
               'F'
             then "Female"
             else "Male"),
            Tooltip => "Show available options for recruit",
            Command => "ShowRecruitMenu" & Positive'Image(I), Column => 2);
         Add_Button
           (Table => Recruit_Table,
            Text => To_String
              (Source => Factions_List
                 (Recruit_Container.Element
                    (Container => Sky_Bases(Base_Index).Recruits, Index => I)
                    .Faction)
                 .Name),
            Tooltip => "Show available options for recruit",
            Command => "ShowRecruitMenu" & Positive'Image(I), Column => 3);
         Add_Button
           (Table => Recruit_Table,
            Text => Positive'Image
              (Recruit_Container.Element
                 (Container => Sky_Bases(Base_Index).Recruits, Index => I)
                 .Price),
            Tooltip => "Show available options for recruit",
            Command => "ShowRecruitMenu" & Positive'Image(I), Column => 4);
         Add_Button
           (Table => Recruit_Table, Text => To_String(Source => Get_Highest_Attribute(Base_Index => Base_Index, Member_Index => I)),
            Tooltip => "Show available options for recruit",
            Command => "ShowRecruitMenu" & Positive'Image(I), Column => 5);
         Add_Button
           (Table => Recruit_Table, Text => To_String(Source => Get_Highest_Skill(Base_Index => Base_Index, Member_Index => I)),
            Tooltip => "Show available options for recruit",
            Command => "ShowRecruitMenu" & Positive'Image(I), Column => 6, New_Row => True);
         exit Load_Recruits_Loop when Recruit_Table.Row =
           Game_Settings.Lists_Limit + 1;
         <<End_Of_Loop>>
      end loop Load_Recruits_Loop;
      if Page > 1 then
         if Recruit_Table.Row < Game_Settings.Lists_Limit + 1 then
            Add_Pagination
              (Table => Recruit_Table, Previous_Command => "ShowRecruit" & Positive'Image(Page - 1), Next_Command => "");
         else
            Add_Pagination
              (Table => Recruit_Table, Previous_Command => "ShowRecruit" & Positive'Image(Page - 1),
               Next_Command => "ShowRecruit" & Positive'Image(Page + 1));
         end if;
      elsif Recruit_Table.Row = Game_Settings.Lists_Limit + 1 then
         Add_Pagination
           (Table => Recruit_Table, Previous_Command => "", Next_Command => "ShowRecruit" & Positive'Image(Page + 1));
      end if;
      Update_Table(Table => Recruit_Table);
      configure
        (Widgt => Recruit_Table.Canvas,
         options => "-scrollregion [list " & BBox(CanvasWidget => Recruit_Table.Canvas, TagOrId => "all") & "]");
      Show_Screen(New_Screen_Name => "recruitframe");
      return TCL_OK;
   end Show_Recruit_Command;

   -- ****iv* RecruitUI/RecruitUI.Recruit_Index
   -- FUNCTION
   -- The index of currently selected recruit
   -- SOURCE
   Recruit_Index: Positive;
   -- ****

   -- ****o* RecruitUI/RecruitUI.Show_Recruit_Menu_Command
   -- FUNCTION
   -- Show menu with actions for the selected recruit
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecruitMenu recruitindex
   -- RecruitIndex is a index of the recruit which menu will be shown
   -- SOURCE
   function Show_Recruit_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Menu_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Interp, Argc);
      Base_Index: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Recruit_Menu: constant Ttk_Frame :=
        Create_Dialog
          (Name => ".recruitmenu",
           Title =>
             Tiny_String.To_String
               (Source => Recruit_Container.Element
                  (Container => Sky_Bases(Base_Index).Recruits,
                   Index => Positive'Value(CArgv.Arg(Argv => Argv, N => 1)))
                  .Name) &
             " actions",
           Parent_Name => ".");
      procedure Add_Button(Name, Label, Command: String) is
         Button: constant Ttk_Button :=
           Create
             (pathName => Recruit_Menu & Name,
              options =>
                "-text {" & Label & "} -command {CloseDialog " & Recruit_Menu &
                " .;" & Command & "}");
      begin
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Button,
            Options =>
              "-sticky we -padx 5" &
              (if Command'Length = 0 then " -pady {0 3}" else ""));
         Bind
           (Widgt => Button, Sequence => "<Escape>",
            Script => "{CloseDialog " & Recruit_Menu & " .;break}");
         if Command'Length = 0 then
            Bind
              (Widgt => Button, Sequence => "<Tab>",
               Script => "{focus " & Recruit_Menu & ".show;break}");
            Focus(Widgt => Button);
         end if;
      end Add_Button;
   begin
      Recruit_Index := Positive'Value(CArgv.Arg(Argv => Argv, N => 1));
      Add_Button
        (Name => ".show", Label => "Show recruit details",
         Command => "ShowRecruitInfo");
      Add_Button
        (Name => ".negotiate", Label => "Start negotiations",
         Command => "Negotiate");
      Add_Button(Name => ".close", Label => "Close", Command => "");
      Show_Dialog(Dialog => Recruit_Menu, Parent_Frame => ".");
      return TCL_OK;
   end Show_Recruit_Menu_Command;

   -- ****o* RecruitUI/RecruitUI.Show_Recruit_Info_Command
   -- FUNCTION
   -- Show information about the selected recruit
   -- PARAMETERS
   -- Client_Data - Custom data send to the command. Unused
   -- Interp      - Tcl interpreter in which command was executed.
   -- Argc        - Number of arguments passed to the command. Unused
   -- Argv        - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowRecruitInfoCommand
   -- SOURCE
   function Show_Recruit_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Info_Command
     (Client_Data: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Client_Data, Argc, Argv);
      use Tiny_String;

      Recruit_Info: Unbounded_String;
      Base_Index: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Recruit: constant Recruit_Data :=
        Recruit_Container.Element
          (Container => Sky_Bases(Base_Index).Recruits, Index => Recruit_Index);
      Recruit_Dialog: constant Ttk_Frame :=
        Create_Dialog(Name => ".recruitdialog", Title => To_String(Source => Recruit.Name));
      Y_Scroll: constant Ttk_Scrollbar :=
        Create
          (pathName => Recruit_Dialog & ".yscroll",
           options => "-orient vertical -command [list " & Recruit_Dialog &
           ".canvas yview]");
      Recruit_Canvas: constant Tk_Canvas :=
        Create
          (pathName => Recruit_Dialog & ".canvas",
           options => "-yscrollcommand [list " & Y_Scroll & " set]");
      Close_Button, Info_Button, Button: Ttk_Button;
      Height, New_Height: Positive := 1;
      Width, New_Width: Positive := 1;
      Progress_Bar: Ttk_ProgressBar;
      Tab_Button: Ttk_RadioButton;
      Frame: Ttk_Frame := Create(pathName => Recruit_Dialog & ".buttonbox");
      Recruit_Label: Ttk_Label;
      Progress_Frame: Ttk_Frame;
      Tab_Names: constant array(1 .. 4) of Unbounded_String :=
        (1 => To_Unbounded_String(Source => "General"), 2 => To_Unbounded_String(Source => "Attributes"),
         3 => To_Unbounded_String(Source => "Skills"), 4 => To_Unbounded_String(Source => "Inventory"));
   begin
      Tcl_SetVar(interp => Interp, varName => "newtab", newValue => To_Lower(Item => To_String(Source => Tab_Names(1))));
      Set_Tab_Buttons_Loop:
      for I in Tab_Names'Range loop
         Tab_Button :=
           Create
             (pathName => Frame & "." & To_Lower(Item => To_String(Source => Tab_Names(I))),
              options => " -text " & To_String(Source => Tab_Names(I)) &
              " -style Radio.Toolbutton -value " &
              To_Lower(Item => To_String(Source => Tab_Names(I))) &
              " -variable newtab -command ShowRecruitTab");
         Tcl.Tk.Ada.Grid.Grid
           (Slave => Tab_Button, Options => "-column" & Natural'Image(I - 1) & " -row 0");
         Bind
           (Widgt => Tab_Button, Sequence => "<Escape>",
            Script => "{" & Recruit_Dialog & ".buttonbox2.button invoke;break}");
      end loop Set_Tab_Buttons_Loop;
      Height := Positive'Value(Winfo_Get(Widgt => Tab_Button, Info => "reqheight"));
      Bind
        (Widgt => Tab_Button, Sequence => "<Tab>",
         Script => "{focus " & Recruit_Dialog & ".buttonbox2.hirebutton;break}");
      Tcl.Tk.Ada.Grid.Grid(Slave => Frame, Options => "-pady {5 0} -columnspan 2");
      Tcl.Tk.Ada.Grid.Grid(Slave => Recruit_Canvas, Options => "-sticky nwes -pady 5 -padx 5");
      Tcl.Tk.Ada.Grid.Grid
        (Y_Scroll, " -sticky ns -pady 5 -padx {0 5} -row 1 -column 1");
      Frame := Create(Recruit_Dialog & ".buttonbox2");
      Button :=
        Create
          (Recruit_Dialog & ".buttonbox2.hirebutton",
           "-text Negotiate -command {CloseDialog " & Recruit_Dialog &
           ";Negotiate}");
      Tcl.Tk.Ada.Grid.Grid(Button);
      Close_Button :=
        Create
          (Recruit_Dialog & ".buttonbox2.button",
           "-text Close -command {CloseDialog " & Recruit_Dialog & "}");
      Tcl.Tk.Ada.Grid.Grid(Close_Button, "-row 0 -column 1");
      Tcl.Tk.Ada.Grid.Grid(Frame, "-pady {0 5}");
      Focus(Close_Button);
      Autoscroll(Y_Scroll);
      -- General info about the selected recruit
      Frame := Create(Recruit_Canvas & ".general");
      if not Factions_List(Recruit.Faction).Flags.Contains
          (To_Unbounded_String("nogender")) then
         Recruit_Info :=
           (if Recruit.Gender = 'M' then To_Unbounded_String("Gender: Male")
            else To_Unbounded_String("Gender: Female"));
      end if;
      Append
        (Recruit_Info,
         LF & "Faction: " &
         To_String(Source => Factions_List(Recruit.Faction).Name) & LF &
         "Home base: " &
         To_String(Source => Sky_Bases(Recruit.Home_Base).Name));
      Recruit_Label :=
        Create
          (Frame & ".label",
           "-text {" & To_String(Recruit_Info) & "} -wraplength 400");
      Tcl.Tk.Ada.Grid.Grid(Recruit_Label, "-sticky w");
      Height := Height + Positive'Value(Winfo_Get(Recruit_Label, "reqheight"));
      Width := Positive'Value(Winfo_Get(Recruit_Label, "reqwidth"));
      Tcl.Tk.Ada.Grid.Grid(Frame);
      -- Statistics of the selected recruit
      Frame := Create(Recruit_Canvas & ".attributes");
      Show_Recruit_Stats_Loop :
      for I in Recruit.Attributes'Range loop
         Progress_Frame :=
           Create(Frame & ".statinfo" & Trim(Positive'Image(I), Left));
         Recruit_Label :=
           Create
             (Progress_Frame & ".label",
              "-text {" &
              To_String
                (AttributesData_Container.Element(Attributes_List, I).Name) &
              ": " & Get_Attribute_Level_Name(Recruit.Attributes(I).Level) &
              "}");
         Tcl.Tk.Ada.Grid.Grid(Recruit_Label);
         Info_Button :=
           Create
             (Progress_Frame & ".button",
              "-image helpicon -style Header.Toolbutton -command {ShowCrewStatsInfo" &
              Positive'Image(I) & " .recruitdialog}");
         Tcl.Tklib.Ada.Tooltip.Add
           (Info_Button,
            "Show detailed information about the selected attribute.");
         Tcl.Tk.Ada.Grid.Grid(Info_Button, "-column 1 -row 0");
         New_Height :=
           New_Height + Positive'Value(Winfo_Get(Info_Button, "reqheight"));
         Tcl.Tk.Ada.Grid.Grid(Progress_Frame);
         Progress_Bar :=
           Create
             (Frame & ".level" & Trim(Positive'Image(I), Left),
              "-value" & Positive'Image(Recruit.Attributes(I).Level * 2) &
              " -length 200");
         Tcl.Tklib.Ada.Tooltip.Add
           (Progress_Bar, "The current level of the attribute.");
         Tcl.Tk.Ada.Grid.Grid(Progress_Bar);
         New_Height :=
           New_Height + Positive'Value(Winfo_Get(Progress_Bar, "reqheight"));
      end loop Show_Recruit_Stats_Loop;
      if New_Height > Height then
         Height := New_Height;
      end if;
      -- Skills of the selected recruit
      Frame := Create(Recruit_Canvas & ".skills");
      New_Height := 1;
      Show_Recruit_Skills_Loop :
      for I in
        Skills_Container.First_Index(Container => Recruit.Skills) ..
          Skills_Container.Last_Index(Container => Recruit.Skills) loop
         Progress_Frame :=
           Create
             (Frame & ".skillinfo" & Trim(Skills_Amount_Range'Image(I), Left));
         Recruit_Label :=
           Create
             (Progress_Frame & ".label" &
              Trim(Skills_Amount_Range'Image(I), Left),
              "-text {" &
              To_String
                (SkillsData_Container.Element
                   (Skills_List,
                    Skills_Container.Element
                      (Container => Recruit.Skills, Index => I)
                      .Index)
                   .Name) &
              ": " &
              Get_Skill_Level_Name
                (Skills_Container.Element
                   (Container => Recruit.Skills, Index => I)
                   .Level) &
              "}");
         Tcl.Tk.Ada.Grid.Grid(Recruit_Label);
         declare
            ToolQuality: Positive := 100;
         begin
            Tool_Quality_Loop :
            for Quality of SkillsData_Container.Element(Skills_List, I)
              .Tools_Quality loop
               if Skills_Container.Element
                   (Container => Recruit.Skills, Index => I)
                   .Level <=
                 Quality.Level then
                  ToolQuality := Quality.Quality;
                  exit Tool_Quality_Loop;
               end if;
            end loop Tool_Quality_Loop;
            Info_Button :=
              Create
                (Progress_Frame & ".button",
                 "-image helpicon -style Header.Toolbutton -command {ShowCrewSkillInfo" &
                 Skills_Amount_Range'Image
                   (Skills_Container.Element
                      (Container => Recruit.Skills, Index => I)
                      .Index) &
                 Positive'Image(ToolQuality) & " .recruitdialog}");
         end;
         Tcl.Tklib.Ada.Tooltip.Add
           (Info_Button, "Show detailed information about the selected skill.");
         Tcl.Tk.Ada.Grid.Grid(Info_Button, "-column 1 -row 0");
         New_Height :=
           New_Height + Positive'Value(Winfo_Get(Info_Button, "reqheight"));
         Tcl.Tk.Ada.Grid.Grid(Progress_Frame);
         Progress_Bar :=
           Create
             (Frame & ".level" & Trim(Skills_Amount_Range'Image(I), Left),
              "-value" &
              Positive'Image
                (Skills_Container.Element
                   (Container => Recruit.Skills, Index => I)
                   .Level) &
              " -length 200");
         Tcl.Tklib.Ada.Tooltip.Add
           (Progress_Bar, "The current level of the skill.");
         Tcl.Tk.Ada.Grid.Grid(Progress_Bar);
         New_Height :=
           New_Height + Positive'Value(Winfo_Get(Progress_Bar, "reqheight"));
      end loop Show_Recruit_Skills_Loop;
      if New_Height > Height then
         Height := New_Height;
      end if;
      -- Equipment of the selected recruit
      Frame := Create(Recruit_Canvas & ".inventory");
      New_Height := 1;
      Recruit_Info := Null_Unbounded_String;
      Show_Recruit_Equipment_Loop :
      for Item of Recruit.Inventory loop
         Append
           (Recruit_Info,
            To_String
              (Source =>
                 Objects_Container.Element
                   (Container => Items_List, Index => Item)
                   .Name) &
            LF);
      end loop Show_Recruit_Equipment_Loop;
      Recruit_Label :=
        Create
          (Frame & ".label",
           "-text {" & To_String(Recruit_Info) & "} -wraplength 400");
      Tcl.Tk.Ada.Grid.Grid(Recruit_Label, "-sticky w");
      New_Height := Positive'Value(Winfo_Get(Recruit_Label, "reqheight"));
      if New_Height > Height then
         Height := New_Height;
      end if;
      New_Width := Positive'Value(Winfo_Get(Recruit_Label, "reqwidth"));
      if New_Width > Width then
         Width := New_Width;
      end if;
      if Height > 500 then
         Height := 500;
      end if;
      if Width < 350 then
         Width := 350;
      end if;
      Frame := Get_Widget(Recruit_Canvas & ".general");
      declare
         XPos: constant Natural :=
           (Positive'Value(Winfo_Get(Recruit_Canvas, "reqwidth")) -
            Positive'Value(Winfo_Get(Frame, "reqwidth"))) /
           4;
      begin
         Canvas_Create
           (Recruit_Canvas, "window",
            Trim(Natural'Image(XPos), Left) & " 0 -anchor nw -window " &
            Frame & " -tag info");
      end;
      Tcl_Eval(Interp, "update");
      configure
        (Recruit_Canvas,
         "-scrollregion [list " & BBox(Recruit_Canvas, "all") & "] -width" &
         Positive'Image(Width) & " -height" & Positive'Image(Height));
      Bind
        (Close_Button, "<Tab>",
         "{focus " & Recruit_Dialog & ".buttonbox.general;break}");
      Bind(Recruit_Dialog, "<Escape>", "{" & Close_Button & " invoke;break}");
      Bind(Close_Button, "<Escape>", "{" & Close_Button & " invoke;break}");
      Show_Dialog(Dialog => Recruit_Dialog, Relative_Y => 0.2);
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Negotiate_Hire_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      DialogName: constant String := ".negotiatedialog";
      MoneyIndex2: constant Natural :=
        Find_Item(Player_Ship.Cargo, Money_Index);
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Recruit: constant Recruit_Data :=
        Recruit_Container.Element
          (Container => Sky_Bases(BaseIndex).Recruits, Index => Recruit_Index);
      Cost: Integer;
      Scale: Ttk_Scale := Get_Widget(DialogName & ".daily", Interp);
      DailyPayment: constant Natural :=
        Natural(Float'Value(cget(Scale, "-value")));
      ContractBox: constant Ttk_ComboBox :=
        Get_Widget(DialogName & ".contract", Interp);
      ContractLength: constant Natural := Natural'Value(Current(ContractBox));
      TradePayment: Natural;
      Label: Ttk_Label := Get_Widget(DialogName & ".cost", Interp);
      HireButton: constant Ttk_Button :=
        Get_Widget(DialogName & ".buttonbox.hirebutton", Interp);
   begin
      Scale.Name := New_String(DialogName & ".percent");
      TradePayment := Natural(Float'Value(cget(Scale, "-value")));
      Cost :=
        Recruit.Price - ((DailyPayment - Recruit.Payment) * 50) -
        (TradePayment * 5_000);
      Cost :=
        (case ContractLength is
           when 1 => Cost - Integer(Float(Recruit.Price) * 0.1),
           when 2 => Cost - Integer(Float(Recruit.Price) * 0.5),
           when 3 => Cost - Integer(Float(Recruit.Price) * 0.75),
           when 4 => Cost - Integer(Float(Recruit.Price) * 0.9),
           when others => Cost);
      if Cost < 1 then
         Cost := 1;
      end if;
      Count_Price(Cost, Find_Member(TALK));
      configure
        (Label,
         "-text {Hire for" & Natural'Image(Cost) & " " &
         To_String(Money_Name) & "}");
      Label.Name := New_String(DialogName & ".dailylbl");
      configure
        (Label, "-text {Daily payment:" & Natural'Image(DailyPayment) & "}");
      Label.Name := New_String(DialogName & ".percentlbl");
      configure
        (Label,
         "-text {Percent of profit from trades: " &
         Natural'Image(TradePayment) & "}");
      if MoneyIndex2 > 0
        and then
          Inventory_Container.Element
            (Container => Player_Ship.Cargo, Index => MoneyIndex2)
            .Amount <
          Cost then
         configure(HireButton, "-state disabled");
      else
         configure(HireButton, "-state !disabled");
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
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Hire
   -- SOURCE
   function Hire_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Hire_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc, Argv);
      DialogName: constant String := ".negotiatedialog";
      Cost, ContractLength2: Integer;
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Recruit: constant Recruit_Data :=
        Recruit_Container.Element
          (Container => Sky_Bases(BaseIndex).Recruits, Index => Recruit_Index);
      Scale: Ttk_Scale := Get_Widget(DialogName & ".daily", Interp);
      DailyPayment: constant Natural :=
        Natural(Float'Value(cget(Scale, "-value")));
      ContractBox: constant Ttk_ComboBox :=
        Get_Widget(DialogName & ".contract", Interp);
      ContractLength: constant Natural := Natural'Value(Current(ContractBox));
      TradePayment: Natural;
   begin
      Scale.Name := New_String(DialogName & ".percent");
      TradePayment := Natural(Float'Value(cget(Scale, "-value")));
      Cost :=
        Recruit.Price - ((DailyPayment - Recruit.Payment) * 50) -
        (TradePayment * 5_000);
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
        (Recruit_Index, Cost, DailyPayment, TradePayment, ContractLength2);
      Update_Messages;
      Tcl_Eval(Interp, "CloseDialog " & DialogName);
      return
        Show_Recruit_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowRecruit" & "1");
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
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Recruit_Tab_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
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
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- Negotiate
   -- SOURCE
   function Negotiate_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Negotiate_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Recruit: constant Recruit_Data :=
        Recruit_Container.Element
          (Container => Sky_Bases(BaseIndex).Recruits, Index => Recruit_Index);
      NegotiateDialog: constant Ttk_Frame :=
        Create_Dialog
          (".negotiatedialog",
           "Negotiate with " & Tiny_String.To_String(Recruit.Name));
      CloseButton, HireButton: Ttk_Button;
      Frame: constant Ttk_Frame := Create(NegotiateDialog & ".buttonbox");
      Label: Ttk_Label;
      Scale: Ttk_Scale;
      ContractBox: constant Ttk_ComboBox :=
        Create
          (NegotiateDialog & ".contract",
           "-state readonly -values [list {Pernament} {100 days} {30 days} {20 days} {10 days}]");
      MoneyIndex2: constant Natural :=
        Find_Item(Player_Ship.Cargo, Money_Index);
      Cost: Positive;
   begin
      Label :=
        Create
          (NegotiateDialog & ".dailylbl",
           "-text {Daily payment:" & Natural'Image(Recruit.Payment) & "}");
      Tcl.Tk.Ada.Grid.Grid(Label, "-pady {5 0}");
      Scale :=
        Create
          (NegotiateDialog & ".daily",
           "-from 0 -command NegotiateHire -length 250");
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
           "-from 0 -to 10 -command NegotiateHire -length 250");
      Tcl.Tk.Ada.Grid.Grid(Scale);
      configure(Scale, "-value 0");
      Label :=
        Create(NegotiateDialog & ".contractlbl", "-text {Contract time:}");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Tcl.Tk.Ada.Grid.Grid(ContractBox);
      Bind(ContractBox, "<<ComboboxSelected>>", "{NegotiateHire}");
      Current(ContractBox, "0");
      HireButton :=
        Create
          (NegotiateDialog & ".buttonbox.hirebutton",
           "-text Hire -command {Hire}");
      Label := Create(NegotiateDialog & ".money");
      Tcl.Tk.Ada.Grid.Grid(Label);
      Cost := Recruit.Price;
      Count_Price(Cost, Find_Member(TALK));
      if MoneyIndex2 > 0 then
         configure
           (Label,
            "-text {You have" &
            Natural'Image
              (Inventory_Container.Element
                 (Container => Player_Ship.Cargo, Index => MoneyIndex2)
                 .Amount) &
            " " & To_String(Money_Name) & ".}");
         if Inventory_Container.Element
             (Container => Player_Ship.Cargo, Index => MoneyIndex2)
             .Amount <
           Cost then
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
         To_String(Money_Name) & "}");
      Tcl.Tk.Ada.Grid.Grid(HireButton);
      CloseButton :=
        Create
          (NegotiateDialog & ".buttonbox.button",
           "-text Close -command {CloseDialog " & NegotiateDialog & "}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      Tcl.Tk.Ada.Grid.Grid(Frame, "-pady {0 5}");
      Focus(CloseButton);
      Bind(CloseButton, "<Tab>", "{focus " & HireButton & ";break}");
      Bind(HireButton, "<Tab>", "{focus " & CloseButton & ";break}");
      Bind(NegotiateDialog, "<Escape>", "{" & CloseButton & " invoke;break}");
      Bind(CloseButton, "<Escape>", "{" & CloseButton & " invoke;break}");
      Show_Dialog(Dialog => NegotiateDialog, Relative_Y => 0.2);
      return TCL_OK;
   end Negotiate_Command;

   -- ****it* RecruitUI/RecruitUI.Recruits_Sort_Orders
   -- FUNCTION
   -- Sorting orders for the list of available recruits in base
   -- OPTIONS
   -- NAMEASC       - Sort recruits by name ascending
   -- NAMEDESC      - Sort recruits by name descending
   -- GENDERASC     - Sort recruits by gender ascending
   -- GENDERDESC    - Sort recruits by gender descending
   -- FACTIONASC    - Sort recruits by faction ascending
   -- FACTIONDESC   - Sort recruits by faction descending
   -- PRICEASC      - Sort recruits by price ascending
   -- PRICEDESC     - Sort recruits by price descending
   -- ATTRIBUTEASC  - Sort recruits by attribute ascending
   -- ATTRIBUTEDESC - Sort recruits by attribute descending
   -- SKILLASC      - Sort recruits by skill ascending
   -- SKILLDESC     - Sort recruits by skill descending
   -- NONE       - No sorting recruits (default)
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   type Recruits_Sort_Orders is
     (NAMEASC, NAMEDESC, GENDERASC, GENDERDESC, FACTIONDESC, FACTIONASC,
      PRICEASC, PRICEDESC, ATTRIBUTEASC, ATTRIBUTEDESC, SKILLASC, SKILLDESC,
      NONE) with
      Default_Value => NONE;
      -- ****

      -- ****id* RecruitUI/RecruitUI.Default_Recruits_Sort_Order
      -- FUNCTION
      -- Default sorting order for the available recruits in base
      -- HISTORY
      -- 6.4 - Added
      -- SOURCE
   Default_Recruits_Sort_Order: constant Recruits_Sort_Orders := NONE;
   -- ****

   -- ****iv* RecruitUI/RecruitUI.Recruits_Sort_Order
   -- FUNCTION
   -- The current sorting order for the available recruits in base
   -- HISTORY
   -- 6.4 - Added
   -- SOURCE
   Recruits_Sort_Order: Recruits_Sort_Orders := Default_Recruits_Sort_Order;
   -- ****

   -- ****o* RecruitUI/RecruitUI.Sort_Recruits_Command
   -- FUNCTION
   -- Sort the list of available recruits in base
   -- PARAMETERS
   -- ClientData - Custom data send to the command.
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SortRecruits x
   -- X is X axis coordinate where the player clicked the mouse button
   -- SOURCE
   function Sort_Recruits_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Sort_Recruits_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(Argc);
      use Tiny_String;

      Column: constant Positive :=
        Get_Column_Number(Recruit_Table, Natural'Value(CArgv.Arg(Argv, 1)));
      type Local_Module_Data is record
         Name: Bounded_String;
         Gender: Character;
         Faction: Bounded_String;
         Price: Positive;
         Attribute: Unbounded_String;
         Skill: Unbounded_String;
         Id: Positive;
      end record;
      type Recruits_Array is array(Positive range <>) of Local_Module_Data;
      BaseIndex: constant Positive :=
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index;
      Local_Recruits: Recruits_Array
        (1 ..
             Positive
               (Recruit_Container.Length
                  (Container => Sky_Bases(BaseIndex).Recruits)));
      function "<"(Left, Right: Local_Module_Data) return Boolean is
      begin
         if Recruits_Sort_Order = NAMEASC and then Left.Name < Right.Name then
            return True;
         end if;
         if Recruits_Sort_Order = NAMEDESC and then Left.Name > Right.Name then
            return True;
         end if;
         if Recruits_Sort_Order = GENDERASC
           and then Left.Gender < Right.Gender then
            return True;
         end if;
         if Recruits_Sort_Order = GENDERDESC
           and then Left.Gender > Right.Gender then
            return True;
         end if;
         if Recruits_Sort_Order = FACTIONASC
           and then Left.Faction < Right.Faction then
            return True;
         end if;
         if Recruits_Sort_Order = FACTIONDESC
           and then Left.Faction > Right.Faction then
            return True;
         end if;
         if Recruits_Sort_Order = PRICEASC
           and then Left.Price < Right.Price then
            return True;
         end if;
         if Recruits_Sort_Order = PRICEDESC
           and then Left.Price > Right.Price then
            return True;
         end if;
         if Recruits_Sort_Order = ATTRIBUTEASC
           and then Left.Attribute < Right.Attribute then
            return True;
         end if;
         if Recruits_Sort_Order = ATTRIBUTEDESC
           and then Left.Attribute > Right.Attribute then
            return True;
         end if;
         if Recruits_Sort_Order = SKILLASC
           and then Left.Skill < Right.Skill then
            return True;
         end if;
         if Recruits_Sort_Order = SKILLDESC
           and then Left.Skill > Right.Skill then
            return True;
         end if;
         return False;
      end "<";
      procedure Sort_Recruits is new Ada.Containers.Generic_Array_Sort
        (Index_Type => Positive, Element_Type => Local_Module_Data,
         Array_Type => Recruits_Array);
   begin
      case Column is
         when 1 =>
            if Recruits_Sort_Order = NAMEASC then
               Recruits_Sort_Order := NAMEDESC;
            else
               Recruits_Sort_Order := NAMEASC;
            end if;
         when 2 =>
            if Recruits_Sort_Order = GENDERASC then
               Recruits_Sort_Order := GENDERDESC;
            else
               Recruits_Sort_Order := GENDERASC;
            end if;
         when 3 =>
            if Recruits_Sort_Order = FACTIONASC then
               Recruits_Sort_Order := FACTIONDESC;
            else
               Recruits_Sort_Order := FACTIONASC;
            end if;
         when 4 =>
            if Recruits_Sort_Order = PRICEASC then
               Recruits_Sort_Order := PRICEDESC;
            else
               Recruits_Sort_Order := PRICEASC;
            end if;
         when 5 =>
            if Recruits_Sort_Order = ATTRIBUTEASC then
               Recruits_Sort_Order := ATTRIBUTEDESC;
            else
               Recruits_Sort_Order := ATTRIBUTEASC;
            end if;
         when 6 =>
            if Recruits_Sort_Order = SKILLASC then
               Recruits_Sort_Order := SKILLDESC;
            else
               Recruits_Sort_Order := SKILLASC;
            end if;
         when others =>
            null;
      end case;
      if Recruits_Sort_Order = NONE then
         return TCL_OK;
      end if;
      for I in
        Recruit_Container.First_Index
          (Container => Sky_Bases(BaseIndex).Recruits) ..
          Recruit_Container.Last_Index
            (Container => Sky_Bases(BaseIndex).Recruits) loop
         Local_Recruits(I) :=
           (Name =>
              Recruit_Container.Element
                (Container => Sky_Bases(BaseIndex).Recruits, Index => I)
                .Name,
            Gender =>
              Recruit_Container.Element
                (Container => Sky_Bases(BaseIndex).Recruits, Index => I)
                .Gender,
            Faction =>
              Recruit_Container.Element
                (Container => Sky_Bases(BaseIndex).Recruits, Index => I)
                .Faction,
            Price =>
              Recruit_Container.Element
                (Container => Sky_Bases(BaseIndex).Recruits, Index => I)
                .Price,
            Attribute => Get_Highest_Attribute(BaseIndex, I),
            Skill => Get_Highest_Skill(BaseIndex, I), Id => I);
      end loop;
      Sort_Recruits(Local_Recruits);
      Recruits_Indexes.Clear;
      for Recruit of Local_Recruits loop
         Recruits_Indexes.Append(Recruit.Id);
      end loop;
      return
        Show_Recruit_Command
          (ClientData, Interp, 2, CArgv.Empty & "ShowRecruits" & "1");
   end Sort_Recruits_Command;

   procedure Add_Commands is
   begin
      Add_Command("ShowRecruit", Show_Recruit_Command'Access);
      Add_Command("ShowRecruitMenu", Show_Recruit_Menu_Command'Access);
      Add_Command("ShowRecruitInfo", Show_Recruit_Info_Command'Access);
      Add_Command("NegotiateHire", Negotiate_Hire_Command'Access);
      Add_Command("Hire", Hire_Command'Access);
      Add_Command("ShowRecruitTab", Show_Recruit_Tab_Command'Access);
      Add_Command("Negotiate", Negotiate_Command'Access);
      Add_Command("SortRecruits", Sort_Recruits_Command'Access);
   end Add_Commands;

end Bases.RecruitUI;
