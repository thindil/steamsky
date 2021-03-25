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

with Ada.Containers; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.String_Split; use GNAT.String_Split;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Event; use Tcl.Tk.Ada.Event;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Canvas; use Tcl.Tk.Ada.Widgets.Canvas;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Bases; use Bases;
with BasesTypes; use BasesTypes;
with Factions; use Factions;
with Game; use Game;
with Items; use Items;
with Knowledge.Bases;
with Knowledge.Events;
with Missions; use Missions;
with Knowledge.Missions;
with Ships; use Ships;
with Stories; use Stories;
with Knowledge.Stories;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Table; use Table;
with Utils.UI; use Utils.UI;

package body Knowledge is

   -- ****iv* Knowledge/Knowledge.MissionsTable
   -- FUNCTION
   -- Table with info about the known events
   -- SOURCE
   MissionsTable: Table_Widget (5);
   -- ****

   function Show_Knowledge_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      KnowledgeFrame: Ttk_Frame := Get_Widget(Paned & ".knowledgeframe");
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      Tokens: Slice_Set;
      Rows: Natural := 0;
      KnowledgeCanvas: Tk_Canvas :=
        Get_Widget(KnowledgeFrame & ".bases.canvas", Interp);
      ComboBox: Ttk_ComboBox :=
        Get_Widget(KnowledgeCanvas & ".frame.options.types");
      ComboValues, Mission_Time: Unbounded_String;
      Row: Positive;
      Label: Ttk_Label;
      Button: Ttk_Button;
   begin
      if Winfo_Get(KnowledgeFrame, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator &
            "knowledge.tcl");
         Append(ComboValues, " {Any}");
         Load_Bases_Types_Loop :
         for BaseType of BasesTypes_List loop
            Append(ComboValues, " {" & BaseType.Name & "}");
         end loop Load_Bases_Types_Loop;
         configure(ComboBox, "-values [list" & To_String(ComboValues) & "]");
         Current(ComboBox, "0");
         ComboValues := To_Unbounded_String(" {Any}");
         ComboBox.Name := New_String(KnowledgeCanvas & ".frame.options.owner");
         Load_Bases_Owners_Loop :
         for I in Factions_List.Iterate loop
            Append(ComboValues, " {" & Factions_List(I).Name & "}");
         end loop Load_Bases_Owners_Loop;
         configure(ComboBox, "-values [list" & To_String(ComboValues) & "]");
         Current(ComboBox, "0");
      elsif Winfo_Get(KnowledgeFrame, "ismapped") = "1" and Argc = 1 then
         Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
         Tcl_Eval(Interp, "InvokeButton " & CloseButton);
         Tcl.Tk.Ada.Grid.Grid_Remove(CloseButton);
         return TCL_OK;
      end if;
      Entry_Configure(GameMenu, "Help", "-command {ShowHelp general}");
      Tcl.Tk.Ada.Grid.Grid(CloseButton, "-row 0 -column 1");
      -- Setting bases list
      Knowledge.Bases.UpdateBasesList;
      -- Setting accepted missions info
      KnowledgeFrame.Name :=
        New_String(Paned & ".knowledgeframe.missions.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(KnowledgeFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      if MissionsTable.Row > 1 then
         ClearTable(MissionsTable);
      end if;
      Delete_Widgets(1, Rows - 1, KnowledgeFrame);
      if AcceptedMissions.Length = 0 then
         Label :=
           Create
             (KnowledgeFrame & ".nomissions",
              "-text {You didn't accept any mission yet. You may ask for missions in bases. When your ship is docked to base, check Missions from ship orders menu.} -wraplength 400");
         Tcl.Tk.Ada.Grid.Grid(Label);
      else
         Row := 2;
         MissionsTable :=
           CreateTable
             (Widget_Image(KnowledgeFrame),
              (To_Unbounded_String("Name"), To_Unbounded_String("Distance"),
               To_Unbounded_String("Details"),
               To_Unbounded_String("Time limit"),
               To_Unbounded_String("Base reward")),
              False);
         Load_Accepted_Missions_Loop :
         for I in
           AcceptedMissions.First_Index .. AcceptedMissions.Last_Index loop
            case AcceptedMissions(I).MType is
               when Deliver =>
                  AddButton
                    (MissionsTable, "Deliver item to base",
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 1);
                  AddButton
                    (MissionsTable,
                     To_String
                       (Items_List(AcceptedMissions(I).ItemIndex).Name) &
                     " to " &
                     To_String
                       (SkyBases
                          (SkyMap
                             (AcceptedMissions(I).TargetX,
                              AcceptedMissions(I).TargetY)
                             .BaseIndex)
                          .Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Patrol =>
                  AddButton
                    (MissionsTable, "Patrol area",
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 1);
                  AddButton
                    (MissionsTable,
                     "X:" & Natural'Image(AcceptedMissions(I).TargetX) &
                     " Y:" & Natural'Image(AcceptedMissions(I).TargetY),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Destroy =>
                  AddButton
                    (MissionsTable, "Destroy ship",
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 1);
                  AddButton
                    (MissionsTable,
                     To_String
                       (ProtoShips_List(AcceptedMissions(I).ShipIndex).Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Explore =>
                  AddButton
                    (MissionsTable, "Explore area",
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 1);
                  AddButton
                    (MissionsTable,
                     "X:" & Natural'Image(AcceptedMissions(I).TargetX) &
                     " Y:" & Natural'Image(AcceptedMissions(I).TargetY),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
               when Passenger =>
                  AddButton
                    (MissionsTable, "Transport passenger to base",
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 1);
                  AddButton
                    (MissionsTable,
                     "To " &
                     To_String
                       (SkyBases
                          (SkyMap
                             (AcceptedMissions(I).TargetX,
                              AcceptedMissions(I).TargetY)
                             .BaseIndex)
                          .Name),
                     "Show available mission's options",
                     "ShowMissionMenu" & Positive'Image(Row - 1), 3);
            end case;
            AddButton
              (MissionsTable,
               Natural'Image
                 (CountDistance
                    (AcceptedMissions(I).TargetX,
                     AcceptedMissions(I).TargetY)),
               "The distance to the mission",
               "ShowMissionMenu" & Positive'Image(Row - 1), 2);
            Mission_Time := Null_Unbounded_String;
            MinutesToDate(AcceptedMissions(I).Time, Mission_Time);
            AddButton
              (MissionsTable, To_String(Mission_Time),
               "The time limit for finish and return the mission",
               "ShowMissionMenu" & Positive'Image(Row - 1), 4);
            AddButton
              (MissionsTable,
               Natural'Image
                 (Natural
                    (Float(AcceptedMissions(I).Reward) *
                     Float(AcceptedMissions(I).Multiplier))) &
               " " & To_String(Money_Name),
               "The base money reward for the mission",
               "ShowMissionMenu" & Positive'Image(Row - 1), 5, True);
            Row := Row + 1;
         end loop Load_Accepted_Missions_Loop;
         UpdateTable(MissionsTable);
      end if;
      Tcl_Eval(Get_Context, "update");
      KnowledgeCanvas.Name :=
        New_String(Paned & ".knowledgeframe.missions.canvas");
      configure
        (KnowledgeCanvas,
         "-scrollregion [list " & BBox(KnowledgeCanvas, "all") & "]");
      Xview_Move_To(KnowledgeCanvas, "0.0");
      Yview_Move_To(KnowledgeCanvas, "0.0");
      -- Setting the known events list
      Knowledge.Events.UpdateEventsList;
      -- Setting the known stories list
      KnowledgeFrame.Name :=
        New_String(Paned & ".knowledgeframe.stories.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(KnowledgeFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      Delete_Widgets(1, Rows - 1, KnowledgeFrame);
      if FinishedStories.Length = 0 then
         Label :=
           Create
             (KnowledgeFrame & ".nostories",
              "-text {You didn't discover any story yet.} -wraplength 400");
         Tcl.Tk.Ada.Grid.Grid(Label);
      else
         declare
            OptionsFrame: constant Ttk_Frame :=
              Create(KnowledgeFrame & ".options");
            StoriesBox: constant Ttk_ComboBox :=
              Create(OptionsFrame & ".titles", "-state readonly");
            StoriesList: Unbounded_String;
            StoriesView: constant Tk_Text :=
              Create(KnowledgeFrame & ".view", "-wrap word");
         begin
            Load_Finished_Stories_Loop :
            for FinishedStory of FinishedStories loop
               Append
                 (StoriesList,
                  " {" & Stories_List(FinishedStory.Index).Name & "}");
            end loop Load_Finished_Stories_Loop;
            configure
              (StoriesBox, "-values [list " & To_String(StoriesList) & "]");
            Bind(StoriesBox, "<<ComboboxSelected>>", "ShowStory");
            Current
              (StoriesBox, Natural'Image(Natural(FinishedStories.Length) - 1));
            Tcl.Tk.Ada.Grid.Grid(StoriesBox);
            Button :=
              Create
                (OptionsFrame & ".show",
                 "-text {Show on map} -command ShowStoryLocation");
            Tcl.Tk.Ada.Grid.Grid(Button, "-column 1 -row 0");
            Button :=
              Create
                (OptionsFrame & ".set",
                 "-text {Set as destintion for ship} -command SetStory");
            Tcl.Tk.Ada.Grid.Grid(Button, "-column 2 -row 0");
            Tcl.Tk.Ada.Grid.Grid(OptionsFrame, "-sticky w");
            Tcl.Tk.Ada.Grid.Grid(StoriesView, "-sticky w");
            Generate(StoriesBox, "<<ComboboxSelected>>");
         end;
      end if;
      Tcl_Eval(Get_Context, "update");
      KnowledgeCanvas.Name :=
        New_String(Paned & ".knowledgeframe.stories.canvas");
      configure
        (KnowledgeCanvas,
         "-scrollregion [list " & BBox(KnowledgeCanvas, "all") & "]");
      Xview_Move_To(KnowledgeCanvas, "0.0");
      Yview_Move_To(KnowledgeCanvas, "0.0");
      -- Show knowledge
      ShowScreen("knowledgeframe");
      return TCL_OK;
   end Show_Knowledge_Command;

   -- ****o* Knowledge/Knowledge.Knowledge_Max_Min_Command
   -- FUNCTION
   -- Maximize or minimize the selected section of knowledge info
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command.
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- KnowledgeMaxMin framename
   -- Framename is name of the frame to maximize or minimize
   -- SOURCE
   function Knowledge_Max_Min_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int with
      Convention => C;
      -- ****

   function Knowledge_Max_Min_Command
     (ClientData: Integer; Interp: Tcl.Tcl_Interp; Argc: Interfaces.C.int;
      Argv: CArgv.Chars_Ptr_Ptr) return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc);
      type Frame_Info is record
         Name: Unbounded_String;
         Column: Natural range 0 .. 1;
         Row: Natural range 0 .. 1;
      end record;
      Frames: constant array(1 .. 4) of Frame_Info :=
        ((To_Unbounded_String("bases"), 0, 0),
         (To_Unbounded_String("missions"), 0, 1),
         (To_Unbounded_String("events"), 1, 0),
         (To_Unbounded_String("stories"), 1, 1));
      Frame: Ttk_Frame;
      Button: Ttk_Button;
   begin
      Frame.Interp := Interp;
      Frame.Name := New_String(".gameframe.paned.knowledgeframe");
      Button.Interp := Interp;
      Button.Name :=
        New_String
          (Widget_Image(Frame) & "." & CArgv.Arg(Argv, 1) &
           ".canvas.frame.maxmin");
      if CArgv.Arg(Argv, 2) /= "show" then
         Hide_Manipulate_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (".gameframe.paned.knowledgeframe." &
                 To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame,
                  "-columnspan 1 -rowspan 1 -column" &
                  Natural'Image(FrameInfo.Column) & " -row" &
                  Natural'Image(FrameInfo.Row));
            end if;
         end loop Hide_Manipulate_Frames_Loop;
         configure
           (Button,
            "-text ""[format %c 0xf106]"" -command {KnowledgeMaxMin " &
            CArgv.Arg(Argv, 1) & " show}");
      else
         Show_Manipulate_Frames_Loop :
         for FrameInfo of Frames loop
            Frame.Name :=
              New_String
                (".gameframe.paned.knowledgeframe." &
                 To_String(FrameInfo.Name));
            if To_String(FrameInfo.Name) /= CArgv.Arg(Argv, 1) then
               Tcl.Tk.Ada.Grid.Grid_Remove(Frame);
            else
               Tcl.Tk.Ada.Grid.Grid_Configure
                 (Frame, "-columnspan 2 -rowspan 2 -row 0 -column 0");
            end if;
         end loop Show_Manipulate_Frames_Loop;
         configure
           (Button,
            "-text ""[format %c 0xf107]"" -command {KnowledgeMaxMin " &
            CArgv.Arg(Argv, 1) & " hide}");
      end if;
      return TCL_OK;
   end Knowledge_Max_Min_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowKnowledge", Show_Knowledge_Command'Access);
      AddCommand("KnowledgeMaxMin", Knowledge_Max_Min_Command'Access);
      Knowledge.Bases.AddCommands;
      Knowledge.Events.AddCommands;
      Knowledge.Missions.AddCommands;
      Knowledge.Stories.AddCommands;
   end AddCommands;

end Knowledge;
