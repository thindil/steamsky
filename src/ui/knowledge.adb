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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
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
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with BasesTypes; use BasesTypes;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Knowledge.Bases;
with Knowledge.Events;
with Missions; use Missions;
with Knowledge.Missions;
with Stories; use Stories;
with Knowledge.Stories;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Utils.UI; use Utils.UI;

package body Knowledge is

   function Show_Knowledge_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argv);
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(".gameframe.paned", Interp);
      KnowledgeFrame: Ttk_Frame := Get_Widget(Paned & ".knowledgeframe");
      Item: Ttk_Frame;
      CloseButton: constant Ttk_Button :=
        Get_Widget(".gameframe.header.closebutton", Interp);
      Tokens: Slice_Set;
      Rows: Natural := 0;
      KnowledgeCanvas: Tk_Canvas :=
        Get_Widget(KnowledgeFrame & ".bases.canvas", Interp);
      ComboBox: Ttk_ComboBox :=
        Get_Widget(KnowledgeCanvas & ".frame.options.types");
      ComboValues: Unbounded_String;
      Row: Positive;
      Label: Ttk_Label;
      Button: Ttk_Button;
   begin
      if Winfo_Get(KnowledgeFrame, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "knowledge.tcl");
         Append(ComboValues, " {Any}");
         for BaseType of BasesTypes_List loop
            Append(ComboValues, " {" & BaseType.Name & "}");
         end loop;
         configure(ComboBox, "-values [list" & To_String(ComboValues) & "]");
         Current(ComboBox, "0");
         ComboValues := To_Unbounded_String(" {Any}");
         ComboBox.Name := New_String(KnowledgeCanvas & ".frame.options.owner");
         for I in Factions_List.Iterate loop
            Append(ComboValues, " {" & Factions_List(I).Name & "}");
         end loop;
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
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (KnowledgeFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item := Get_Widget(Slice(Tokens, J), Interp);
            Destroy(Item);
         end loop;
      end loop;
      if AcceptedMissions.Length = 0 then
         Label :=
           Create
             (KnowledgeFrame & ".nomissions",
              "-text {You didn't accept any mission yet. You may ask for missions in bases. When your ship is docked to base, check Missions from ship orders menu.} -wraplength 400");
         Tcl.Tk.Ada.Grid.Grid(Label);
      else
         Label := Create(KnowledgeFrame & ".name", "-text {Name}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         Label := Create(KnowledgeFrame & ".distance", "-text {Distance}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-row 1 -column 1");
         Row := 2;
         for I in
           AcceptedMissions.First_Index .. AcceptedMissions.Last_Index loop
            case AcceptedMissions(I).MType is
               when Deliver =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Deliver item to base} -command {ShowMissionMenu" &
                       Positive'Image(Row - 1) & "}");
               when Patrol =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Patrol area} -command {ShowMissionMenu" &
                       Positive'Image(Row - 1) & "}");
               when Destroy =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Destroy ship} -command {ShowMissionMenu" &
                       Positive'Image(Row - 1) & "}");
               when Explore =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Explore area} -command {ShowMissionMenu" &
                       Positive'Image(Row - 1) & "}");
               when Passenger =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Transport passenger to base} -command {ShowMissionMenu" &
                       Positive'Image(Row - 1) & "}");
            end case;
            Add(Button, "Show available mission's options");
            Tcl.Tk.Ada.Grid.Grid
              (Button, "-row" & Positive'Image(Row) & " -sticky w");
            Label :=
              Create
                (KnowledgeFrame & ".distance" &
                 Trim(Positive'Image(Row), Left),
                 "-text {" &
                 Natural'Image
                   (CountDistance
                      (AcceptedMissions(I).TargetX,
                       AcceptedMissions(I).TargetY)) &
                 "}");
            Tcl.Tk.Ada.Grid.Grid
              (Label, "-row" & Positive'Image(Row) & " -column 1");
            Row := Row + 1;
         end loop;
         null;
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
      KnowledgeFrame.Name :=
        New_String(Paned & ".knowledgeframe.events.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(KnowledgeFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (KnowledgeFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item := Get_Widget(Slice(Tokens, J), Interp);
            Destroy(Item);
         end loop;
      end loop;
      if Events_List.Length = 0 then
         Label :=
           Create
             (KnowledgeFrame & ".noevents",
              "-text {You don't know any event yet. You may ask for events in bases. When your ship is docked to base, select Ask for Events from ship orders menu.} -wraplength 400");
         Tcl.Tk.Ada.Grid.Grid(Label);
      else
         Label := Create(KnowledgeFrame & ".name", "-text {Name}");
         Tcl.Tk.Ada.Grid.Grid(Label);
         Label := Create(KnowledgeFrame & ".distance", "-text {Distance}");
         Tcl.Tk.Ada.Grid.Grid(Label, "-row 1 -column 1");
         Row := 2;
         for Event of Events_List loop
            case Event.EType is
               when EnemyShip =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Enemy ship spotted} -command {ShowEventMenu" &
                       Positive'Image(Row - 1) & "}");
               when FullDocks =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Full docks in base} -command {ShowEventMenu" &
                       Positive'Image(Row - 1) & "}");
               when AttackOnBase =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Base is under attack} -command {ShowEventMenu" &
                       Positive'Image(Row - 1) & "}");
               when Disease =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Disease in base} -command {ShowEventMenu" &
                       Positive'Image(Row - 1) & "}");
               when EnemyPatrol =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Enemy patrol} -command {ShowEventMenu" &
                       Positive'Image(Row - 1) & "}");
               when DoublePrice =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Double price in base} -command {ShowEventMenu" &
                       Positive'Image(Row - 1) & "}");
               when Trader =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Friendly trader spotted} -command {ShowEventMenu" &
                       Positive'Image(Row - 1) & "}");
               when FriendlyShip =>
                  Button :=
                    Create
                      (KnowledgeFrame & ".name" &
                       Trim(Positive'Image(Row), Left),
                       "-text {Friendly ship spotted} -command {ShowEventMenu" &
                       Positive'Image(Row - 1) & "}");
               when None | BaseRecovery =>
                  null;
            end case;
            Add(Button, "Show available event's options");
            Tcl.Tk.Ada.Grid.Grid
              (Button, "-row" & Positive'Image(Row) & " -sticky w");
            Label :=
              Create
                (KnowledgeFrame & ".distance" &
                 Trim(Positive'Image(Row), Left),
                 "-text {" &
                 Natural'Image(CountDistance(Event.SkyX, Event.SkyY)) & "}");
            Tcl.Tk.Ada.Grid.Grid
              (Label, "-row" & Positive'Image(Row) & " -column 1");
            Row := Row + 1;
         end loop;
      end if;
      Tcl_Eval(Get_Context, "update");
      KnowledgeCanvas.Name :=
        New_String(Paned & ".knowledgeframe.events.canvas");
      configure
        (KnowledgeCanvas,
         "-scrollregion [list " & BBox(KnowledgeCanvas, "all") & "]");
      Xview_Move_To(KnowledgeCanvas, "0.0");
      Yview_Move_To(KnowledgeCanvas, "0.0");
      -- Setting the known stories list
      KnowledgeFrame.Name :=
        New_String(Paned & ".knowledgeframe.stories.canvas.frame");
      Create(Tokens, Tcl.Tk.Ada.Grid.Grid_Size(KnowledgeFrame), " ");
      Rows := Natural'Value(Slice(Tokens, 2));
      for I in 1 .. (Rows - 1) loop
         Create
           (Tokens,
            Tcl.Tk.Ada.Grid.Grid_Slaves
              (KnowledgeFrame, "-row" & Positive'Image(I)),
            " ");
         for J in 1 .. Slice_Count(Tokens) loop
            Item := Get_Widget(Slice(Tokens, J), Interp);
            Destroy(Item);
         end loop;
      end loop;
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
            for FinishedStory of FinishedStories loop
               Append
                 (StoriesList,
                  " {" & Stories_List(FinishedStory.Index).Name & "}");
            end loop;
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
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Knowledge_Max_Min_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
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
         end loop;
         configure
           (Button,
            "-text ""[format %c 0xf106]"" -command {KnowledgeMaxMin " &
            CArgv.Arg(Argv, 1) & " show}");
      else
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
         end loop;
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
