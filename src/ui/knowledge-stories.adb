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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.String_Split; use GNAT.String_Split;
with CArgv;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Bases; use Bases;
with Factions; use Factions;
with Items; use Items;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with Ships; use Ships;
with Stories; use Stories;
with Utils.UI; use Utils.UI;
with ada.text_io;

package body Knowledge.Stories is

   -- ****o* KStories/Show_Story_Command
   -- FUNCTION
   -- Show the current story information
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed.
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowStory
   -- SOURCE
   function Show_Story_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Story_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Argc, Argv);
      StoryView: constant Tk_Text :=
        Get_Widget
          (".paned.knowledgeframe.stories.canvas.frame.list.view", Interp);
      TargetText: Unbounded_String;
      Tokens: Slice_Set;
      Step: Step_Data;
      StoryIndex: Positive;
      StoriesBox: constant Ttk_ComboBox :=
        Get_Widget
          (".paned.knowledgeframe.stories.canvas.frame.options.titles",
           Interp);
      Button: Ttk_Button :=
        Get_Widget
          (".paned.knowledgeframe.stories.canvas.frame.options.show", Interp);
   begin
      StoryIndex := Natural'Value(Current(StoriesBox)) + 1;
      configure(StoryView, "-state normal");
      Delete(StoryView, "1.0", "end");
      for StepText of FinishedStories(StoryIndex).StepsTexts loop
         Ada.Text_IO.Put_Line(To_String(StepText));
         Insert(StoryView, "end", To_String(StepText) & LF & LF);
      end loop;
      if Natural(FinishedStories(StoryIndex).StepsTexts.Length) <
        FinishedStories(StoryIndex).StepsAmount then
         Ada.Text_IO.Put_Line(To_String(GetCurrentStoryText));
         Insert(StoryView, "end", To_String(GetCurrentStoryText) & LF);
         if CurrentStory.Data /= Null_Unbounded_String then
            if CurrentStory.CurrentStep = 0 then
               Step := Stories_List(CurrentStory.Index).StartingStep;
            elsif CurrentStory.CurrentStep > 0 then
               Step :=
                 Stories_List(CurrentStory.Index).Steps
                   (CurrentStory.CurrentStep);
            else
               Step := Stories_List(CurrentStory.Index).FinalStep;
            end if;
            Create(Tokens, To_String(CurrentStory.Data), ";");
            case Step.FinishCondition is
               when ASKINBASE =>
                  if Slice_Count(Tokens) < 2 then
                     TargetText :=
                       To_Unbounded_String(" You must travel to base ") &
                       CurrentStory.Data & To_Unbounded_String(" at X:");
                     for I in SkyBases'Range loop
                        if SkyBases(I).Name = CurrentStory.Data then
                           Append
                             (TargetText, Positive'Image(SkyBases(I).SkyX));
                           Append(TargetText, " Y:");
                           Append
                             (TargetText, Positive'Image(SkyBases(I).SkyY));
                           exit;
                        end if;
                     end loop;
                  else
                     TargetText :=
                       To_Unbounded_String(" You can ask in any base. ");
                  end if;
               when DESTROYSHIP =>
                  TargetText :=
                    To_Unbounded_String(" You must find ") &
                    ProtoShips_List(To_Unbounded_String(Slice(Tokens, 3)))
                      .Name &
                    To_Unbounded_String(" at X:") &
                    To_Unbounded_String(Slice(Tokens, 1)) &
                    To_Unbounded_String(" Y:") &
                    To_Unbounded_String(Slice(Tokens, 2));
               when EXPLORE =>
                  TargetText :=
                    To_Unbounded_String(" You must travel to X:") &
                    To_Unbounded_String(Slice(Tokens, 1)) &
                    To_Unbounded_String(" Y:") &
                    To_Unbounded_String(Slice(Tokens, 2));
               when LOOT =>
                  TargetText :=
                    To_Unbounded_String(" You must loot: ") &
                    Items_List(To_Unbounded_String((Slice(Tokens, 1)))).Name &
                    To_Unbounded_String(" from ");
                  if Slice(Tokens, 2) = "any" then
                     Append(TargetText, "any ");
                     if Factions_Container.Contains
                         (Factions_List,
                          GetStepData(Step.FinishData, "faction")) then
                        Append
                          (TargetText,
                           Factions_List
                             (GetStepData(Step.FinishData, "faction"))
                             .Name);
                        Append(TargetText, " ship.");
                     end if;
                  else
                     for I in ProtoShips_List.Iterate loop
                        if ProtoShips_Container.Key(I) =
                          To_Unbounded_String(Slice(Tokens, 2)) then
                           Append(TargetText, ProtoShips_List(I).Name);
                           Append(TargetText, ".");
                           exit;
                        end if;
                     end loop;
                  end if;
               when ANY =>
                  null;
            end case;
         end if;
         Ada.Text_IO.Put_Line(To_String(TargetText));
         Insert(StoryView, "end", To_String(TargetText) & LF);
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name :=
           New_String(".paned.knowledgeframe.stories.canvas.frame.options.set");
         Tcl.Tk.Ada.Grid.Grid(Button);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         Button.Name :=
           New_String(".paned.knowledgeframe.stories.canvas.frame.options.set");
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
      end if;
      configure(StoryView, "-state disabled");
      return TCL_OK;
   end Show_Story_Command;

   -- ****o* KStories/Show_Story_Location_Command
   -- FUNCTION
   -- Show the current story event on map
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- ShowStoryLocation
   -- SOURCE
   function Show_Story_Location_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Show_Story_Location_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      NewX, NewY: Positive := 1;
   begin
      GetStoryLocation(NewX, NewY);
      CenterX := NewX;
      CenterY := NewY;
      ShowSkyMap(True);
      return TCL_OK;
   end Show_Story_Location_Command;

   -- ****o* KStories/Set_Story_Command
   -- FUNCTION
   -- Set the current story event as the player's ship destination
   -- PARAMETERS
   -- ClientData - Custom data send to the command. Unused
   -- Interp     - Tcl interpreter in which command was executed. Unused
   -- Argc       - Number of arguments passed to the command. Unused
   -- Argv       - Values of arguments passed to the command. Unused
   -- RESULT
   -- This function always return TCL_OK
   -- COMMANDS
   -- SetStory
   -- SOURCE
   function Set_Story_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int with
      Convention => C;
      -- ****

   function Set_Story_Command
     (ClientData: in Integer; Interp: in Tcl.Tcl_Interp;
      Argc: in Interfaces.C.int; Argv: in CArgv.Chars_Ptr_Ptr)
      return Interfaces.C.int is
      pragma Unreferenced(ClientData, Interp, Argc, Argv);
      NewX, NewY: Positive := 1;
   begin
      GetStoryLocation(NewX, NewY);
      if NewX = PlayerShip.SkyX and NewY = PlayerShip.SkyY then
         ShowMessage("You are at this story location now.");
         return TCL_OK;
      end if;
      PlayerShip.DestinationX := NewX;
      PlayerShip.DestinationY := NewY;
      AddMessage
        ("You set the travel destination for your ship.", OrderMessage);
      ShowSkyMap(True);
      return TCL_OK;
   end Set_Story_Command;

   procedure AddCommands is
   begin
      AddCommand("ShowStory", Show_Story_Command'Access);
      AddCommand("ShowStoryLocation", Show_Story_Location_Command'Access);
      AddCommand("SetStory", Set_Story_Command'Access);
   end AddCommands;

end Knowledge.Stories;
