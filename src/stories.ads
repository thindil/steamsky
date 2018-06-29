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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with Game; use Game;

package Stories is

   type StartConditionType is
     (DROPITEM); -- Types of conditions to start stories
   type StepConditionType is
     (ASKINBASE); -- Types of conditions to finish story step
   type Step_Data is -- Data structure for stories steps
   record
      Index: Unbounded_String; -- Index of step
      FinishCondition: StepConditionType; -- Condition which must be met to finish selected step and process to next
      FinishData: UnboundedString_Container
        .Vector; -- Data for finish condition
      Text: Unbounded_String; -- Text which will be show to player when step starts.
      FailText: Unbounded_String; -- Text which will be show to player when step fails to progress.
   end record;
   package Steps_Container is new Vectors(Positive, Step_Data);
   type Story_Data is -- Data structure for stories
   record
      Index: Unbounded_String; -- Index of story
      StartCondition: StartConditionType; -- Condition which must be met to start story
      StartData: UnboundedString_Container
        .Vector; -- Data for starting condition
      MinSteps: Positive; -- Minimal amount of steps in story
      MaxSteps: Positive; -- Maxium amount of steps in story
      StartingStep: Step_Data; -- Starting step of story
      Steps: Steps_Container.Vector; -- Contains all steps in story
   end record;
   package Stories_Container is new Vectors(Positive, Story_Data);
   type CurrentStory_Data is -- Data structure for stories
   record
      Index: Natural; -- Index of story or 0 if no story currently active
      Step: Positive; -- Number of current step in story
      CurrentStep: Integer; -- Index of current step, 0 for starting step, -1 for finish step
      MaxSteps: Positive; -- Number of maxium  amounts of steps in story
      ShowText: Boolean; -- If true, show text of current step to player
      Data: Unbounded_String; -- Various data for current step, depends on step
   end record;
   CurrentStory: CurrentStory_Data; -- Contains data about current story on which player is
   Stories_List: Stories_Container.Vector; -- List of available stories in game
   Stories_Directory_Not_Found: exception; -- Raised when no directory with stories files
   Stories_Files_Not_Found: exception; -- Raised when no files with stories

   procedure LoadStories; -- Load stories data from files
   procedure StartStory
     (FactionName: Unbounded_String;
      Condition: StartConditionType); -- Check if any story can starts
   procedure ClearCurrentStory; -- Resets current story

end Stories;
