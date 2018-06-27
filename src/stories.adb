--    Copyright 2018 Bartek thindil Jasicki
--
--    This file is part of Steam Sky.
--
--    Steam Sky is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    Steam Sky is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Directories; use Ada.Directories;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Log; use Log;

package body Stories is

   procedure LoadStories is
      TempRecord: Story_Data;
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
      StoriesFile: File_Input;
      Reader: Tree_Reader;
      NodesList, ChildNodes, StepDataNodes: Node_List;
      StoriesData: Document;
      TempValue: UnboundedString_Container.Vector;
      TempStep: Step_Data;
      TempSteps: Steps_Container.Vector;
      StartStep: Unbounded_String;
   begin
      if Stories_List.Length > 0 then
         return;
      end if;
      if not Exists(To_String(DataDirectory) & "stories" & Dir_Separator) then
         raise Stories_Directory_Not_Found;
      end if;
      Start_Search
        (Files,
         To_String(DataDirectory) & "stories" & Dir_Separator,
         "*.dat");
      if not More_Entries(Files) then
         raise Stories_Files_Not_Found;
      end if;
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         TempRecord :=
           (Index => Null_Unbounded_String,
            StartCondition => Null_Unbounded_String,
            StartData => TempValue,
            MinSteps => 1,
            MaxSteps => 2,
            StartingStep => TempStep,
            Steps => TempSteps);
         StartStep := Null_Unbounded_String;
         LogMessage
           ("Loading stories file: " & Full_Name(FoundFile),
            Everything);
         Open(Full_Name(FoundFile), StoriesFile);
         Parse(Reader, StoriesFile);
         Close(StoriesFile);
         StoriesData := Get_Tree(Reader);
         NodesList :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name(StoriesData, "story");
         for I in 0 .. Length(NodesList) - 1 loop
            StartStep :=
              To_Unbounded_String
                (Get_Attribute(Item(NodesList, I), "startstep"));
            TempRecord.Index :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "index"));
            TempRecord.StartCondition :=
              To_Unbounded_String
                (Get_Attribute(Item(NodesList, I), "startcondition"));
            TempRecord.MinSteps :=
              Positive'Value(Get_Attribute(Item(NodesList, I), "minsteps"));
            TempRecord.MaxSteps :=
              Positive'Value(Get_Attribute(Item(NodesList, I), "maxsteps"));
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Item(NodesList, I),
                 "startdata");
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempRecord.StartData.Append
               (New_Item =>
                  To_Unbounded_String
                    (Get_Attribute(Item(ChildNodes, J), "value")));
            end loop;
            ChildNodes :=
              DOM.Core.Elements.Get_Elements_By_Tag_Name
                (Item(NodesList, I),
                 "step");
            for J in 0 .. Length(ChildNodes) - 1 loop
               TempStep :=
                 (Index => Null_Unbounded_String,
                  FinishCondition => Null_Unbounded_String,
                  FinishData => TempValue,
                  Text => Null_Unbounded_String);
               TempStep.Index :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "index"));
               TempStep.FinishCondition :=
                 To_Unbounded_String
                   (Get_Attribute(Item(ChildNodes, J), "finishcondition"));
               StepDataNodes :=
                 DOM.Core.Elements.Get_Elements_By_Tag_Name
                   (Item(ChildNodes, J),
                    "finishdata");
               for K in 0 .. Length(StepDataNodes) - 1 loop
                  TempStep.FinishData.Append
                  (New_Item =>
                     To_Unbounded_String
                       (Get_Attribute(Item(StepDataNodes, K), "value")));
               end loop;
               StepDataNodes :=
                 DOM.Core.Elements.Get_Elements_By_Tag_Name
                   (Item(ChildNodes, J),
                    "text");
               TempStep.Text :=
                 To_Unbounded_String
                   (Node_Value(First_Child(Item(StepDataNodes, 0))));
               if TempStep.Index = StartStep then
                  TempRecord.StartingStep := TempStep;
               else
                  TempRecord.Steps.Append(New_Item => TempStep);
               end if;
            end loop;
            Stories_List.Append(New_Item => TempRecord);
            LogMessage
              ("Story added: " & To_String(TempRecord.Index),
               Everything);
            TempRecord :=
              (Index => Null_Unbounded_String,
               StartCondition => Null_Unbounded_String,
               StartData => TempValue,
               MinSteps => 1,
               MaxSteps => 2,
               StartingStep => TempStep,
               Steps => TempSteps);
         end loop;
         Free(Reader);
      end loop;
      End_Search(Files);
   end LoadStories;

   procedure ClearCurrentStory is
   begin
      CurrentStory :=
        (Index => Null_Unbounded_String,
         Step => 1,
         CurrentStep => Null_Unbounded_String,
         MaxSteps => 1);
   end ClearCurrentStory;

end Stories;
