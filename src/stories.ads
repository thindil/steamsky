--    Copyright 2018-2019 Bartek thindil Jasicki
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
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with DOM.Readers; use DOM.Readers;
with Game; use Game;

package Stories is

   -- Types of conditions to start stories
   type StartConditionType is (DROPITEM);
   -- Types of conditions to finish story step
   type StepConditionType is (ASKINBASE, DESTROYSHIP, EXPLORE, ANY, LOOT);
   -- Data structure for stories steps texts
   type StepText_Data is record
      Condition: StepConditionType; -- Finish condition of previous step
      Text: Unbounded_String; -- Text which will be show to player when step starts.
   end record;
   package StepTexts_Container is new Vectors(Positive, StepText_Data);
   -- Structure for finish condition data
   type StepFinish_Data is record
      Name: Unbounded_String; -- Name of data
      Value: Unbounded_String; -- Value of data
   end record;
   package StepData_Container is new Vectors(Positive, StepFinish_Data);
   -- Data structure for stories steps
   type Step_Data is record
      Index: Unbounded_String; -- Index of step
      FinishCondition: StepConditionType; -- Condition which must be met to finish selected step and process to next
      FinishData: StepData_Container.Vector; -- Data for finish condition
      Texts: StepTexts_Container
        .Vector; -- Texts which will be show to player when step starts, depends on finish condition of previous step.
      FailText: Unbounded_String; -- Text which will be show to player when step fails to progress.
   end record;
   package Steps_Container is new Vectors(Positive, Step_Data);
   -- Data structure for stories
   type Story_Data is record
      StartCondition: StartConditionType; -- Condition which must be met to start story
      StartData: UnboundedString_Container
        .Vector; -- Data for starting condition
      MinSteps: Positive; -- Minimal amount of steps in story
      MaxSteps: Positive; -- Maxium amount of steps in story
      StartingStep: Step_Data; -- Starting step of story
      Steps: Steps_Container.Vector; -- Contains all steps in story
      FinalStep: Step_Data; -- Final step of story
      EndText: Unbounded_String; -- Text which will be show to player when story ends.
      Name: Unbounded_String; -- Name of story, show in game
      ForbiddenFactions: UnboundedString_Container
        .Vector; -- If player is in one of this factions, he/she can't start this story.
   end record;
   package Stories_Container is new Hashed_Maps(Unbounded_String, Story_Data,
      Ada.Strings.Unbounded.Hash, "=");
   -- Data structure for stories
   type CurrentStory_Data is record
      Index: Unbounded_String; -- Index of story or empty string if no story currently active
      Step: Positive; -- Number of current step in story
      CurrentStep: Integer; -- Index of current step, 0 for starting step, -1 for finish step
      MaxSteps: Positive; -- Number of maxium  amounts of steps in story
      ShowText: Boolean; -- If true, show text of current step to player
      Data: Unbounded_String; -- Various data for current step, depends on step
      FinishedStep: StepConditionType; -- Finish condition for previous step
   end record;
   -- Data structure for finished story/steps
   type FinishedStory_Data is record
      Index: Unbounded_String; -- Index of story
      StepsAmount: Positive; -- Amount of steps in this story
      StepsTexts: UnboundedString_Container
        .Vector; -- Texts of steps done in this story. If less than StepsAmount then it is current story.
   end record;
   package FinishedStories_Container is new Vectors(Positive,
      FinishedStory_Data);
   -- Contains data about current story on which player is
   CurrentStory: CurrentStory_Data;
   -- List of available stories in game
   Stories_List: Stories_Container.Map;
   -- List of finished stories (or past data of current story)
   FinishedStories: FinishedStories_Container.Vector;

   -- Load stories data from files
   procedure LoadStories(Reader: Tree_Reader);
   -- Check if any story can starts
   procedure StartStory
     (FactionName: Unbounded_String; Condition: StartConditionType) with
      Pre => FactionName /= Null_Unbounded_String;
      -- Resets current story
   procedure ClearCurrentStory;
   -- Returns true if story goes to next step, otherwise false
   function ProgressStory(NextStep: Boolean := False) return Boolean;
   -- Get text of current step in story
   function GetCurrentStoryText return Unbounded_String;
   -- Get step finish data with selected name
   function GetStepData
     (FinishData: StepData_Container.Vector; Name: String)
      return Unbounded_String with
      Pre => Name /= "";
      -- Get target location of current story
   procedure GetStoryLocation(StoryX, StoryY: in out Positive);

end Stories;
