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

-- ****h* Steamsky/Stories
-- FUNCTION
-- Provide code for manipulate the game stories
-- SOURCE
package Stories is
-- ****

   -- ****t* Stories/StartConditionType
   -- FUNCTION
   -- Types of conditions to start stories
   -- SOURCE
   type StartConditionType is (DROPITEM);
   -- ****
   -- ****t* Stories/StepConditionType
   -- FUNCTION
   -- Types of conditions to finish story step
   -- SOURCE
   type StepConditionType is (ASKINBASE, DESTROYSHIP, EXPLORE, ANY, LOOT);
   -- ****
   -- ****t* Stories/StepText_Data
   -- FUNCTION
   -- Data structure for stories steps texts
   -- PARAMETERS
   -- Condition - Finish condition of previous step
   -- Text      - Text which will be show to player when step starts.
   -- SOURCE
   type StepText_Data is record
      Condition: StepConditionType;
      Text: Unbounded_String;
   end record;
   -- ****
   -- ****t* Stories/StepTexts_Container
   -- FUNCTION
   -- Used to store stories steps text data
   -- SOURCE
   package StepTexts_Container is new Vectors(Positive, StepText_Data);
   -- ****
   -- ****t* Stories/StepFinish_Data
   -- FUNCTION
   -- Structure for finish condition data
   -- PARAMETERS
   -- Name  - Name of data
   -- Value - Value of data
   -- SOURCE
   type StepFinish_Data is record
      Name: Unbounded_String;
      Value: Unbounded_String;
   end record;
   -- ****
   -- ****t* Stories/StepData_Container
   -- FUNCTION
   -- Used to store stories steps data
   -- SOURCE
   package StepData_Container is new Vectors(Positive, StepFinish_Data);
   -- ****
   -- ****t* Stories/Step_Data
   -- FUNCTION
   -- Data structure for stories steps
   -- PARAMETERS
   -- Index           - Index of step
   -- FinishCondition - Condition which must be met to finish selected step
   --                   and process to next
   -- FinishData      - Data for finish condition
   -- Texts           - Texts which will be show to player when step starts,
   --                   depends on finish condition of previous step.
   -- FailText        - Text which will be show to player when step fails to
   --                   progress.
   -- SOURCE
   type Step_Data is record
      Index: Unbounded_String;
      FinishCondition: StepConditionType;
      FinishData: StepData_Container.Vector;
      Texts: StepTexts_Container.Vector;
      FailText: Unbounded_String;
   end record;
   -- ****
   -- ****t* Stories/Steps_Container
   -- FUNCTION
   -- Used to store stories steps
   -- SOURCE
   package Steps_Container is new Vectors(Positive, Step_Data);
   -- ****
   -- ****t* Stories/Story_Data
   -- FUNCTION
   -- Data structure for stories
   -- PARAMETERS
   -- StartCondition    - Condition which must be met to start story
   -- StartData         - Data for starting condition
   -- MinSteps          - Minimal amount of steps in story
   -- MaxSteps          - Maxium amount of steps in story
   -- StartingStep      - Starting step of story
   -- Steps             - Contains all steps in story
   -- FinalStep         - Final step of story
   -- EndText           - Text which will be show to player when story ends.
   -- Name              - Name of story, show in game
   -- ForbiddenFactions - If player is in one of this factions, he/she can't
   --                     start this story.
   -- SOURCE
   type Story_Data is record
      StartCondition: StartConditionType;
      StartData: UnboundedString_Container.Vector;
      MinSteps: Positive;
      MaxSteps: Positive;
      StartingStep: Step_Data;
      Steps: Steps_Container.Vector;
      FinalStep: Step_Data;
      EndText: Unbounded_String;
      Name: Unbounded_String;
      ForbiddenFactions: UnboundedString_Container.Vector;
   end record;
   -- ****
   -- ****t* Stories/Stories_Container
   -- FUNCTION
   -- Used to store stories
   -- SOURCE
   package Stories_Container is new Hashed_Maps(Unbounded_String, Story_Data,
      Ada.Strings.Unbounded.Hash, "=");
   -- ****
   -- ****t* Stories/CurrentStory_Data
   -- FUNCTION
   -- Data structure for current active story
   -- PARAMETERS
   -- Index        - Index of story or empty string if no story currently
   --                active
   -- Step         - Number of current step in story
   -- CurrentStep  - Index of current step, 0 for starting step, -1 for finish
   --                step
   -- MaxSteps     - Number of maxium  amounts of steps in story
   -- ShowText     - If true, show text of current step to player
   -- Data         - Various data for current step, depends on step
   -- FinishedStep - Finish condition for previous step
   -- SOURCE
   type CurrentStory_Data is record
      Index: Unbounded_String;
      Step: Positive;
      CurrentStep: Integer;
      MaxSteps: Positive;
      ShowText: Boolean;
      Data: Unbounded_String;
      FinishedStep: StepConditionType;
   end record;
   -- ****
   -- ****t* Stories/FinishedStory_Data
   -- FUNCTION
   -- Data structure for finished story/steps
   -- PARAMETERS
   -- Index       - Index of story
   -- StepsAmount - Amount of steps in this story
   -- StepsTexts  - Texts of steps done in this story. If less than
   --               StepsAmount then it is current story.
   -- SOURCE
   type FinishedStory_Data is record
      Index: Unbounded_String;
      StepsAmount: Positive;
      StepsTexts: UnboundedString_Container.Vector;
   end record;
   -- ****
   -- ****t* Stories/FinishedStories_Container
   -- FUNCTIONS
   -- Used to store finished stories
   -- SOURCE
   package FinishedStories_Container is new Vectors(Positive,
      FinishedStory_Data);
   -- ****
   -- ****v* Stories/CurrentStory
   -- FUNCTION
   -- Contains data about current story on which player is
   -- SOURCE
   CurrentStory: CurrentStory_Data;
   -- ****
   -- ****v* Stories/Stories_List
   -- FUNCTION
   -- List of available stories in game
   -- SOURCE
   Stories_List: Stories_Container.Map;
   -- ****
   -- ****v* Stories/FinishedStories
   -- FUNCTION
   -- List of finished stories (or past data of current story)
   -- SOURCE
   FinishedStories: FinishedStories_Container.Vector;
   -- ****

   -- ****f* Stories/LoadStories
   -- FUNCTION
   -- Load stories data from files
   -- PARAMETERS
   -- Reader - XML Reader from which data will be read
   -- SOURCE
   procedure LoadStories(Reader: Tree_Reader);
   -- ****
   -- ****f* Stories/StartStory
   -- FUNCTION
   -- Check if any story can starts
   -- PARAMETERS
   -- FactionName - Name of faction to which players belongs
   -- Condition   - Starting condition which was triggered
   -- SOURCE
   procedure StartStory
     (FactionName: Unbounded_String; Condition: StartConditionType) with
      Pre => FactionName /= Null_Unbounded_String;
      -- ****
      -- ****f* Stories/ClearCurrentStory
      -- FUNCTION
      -- Resets current story
      -- SOURCE
   procedure ClearCurrentStory;
   -- ****
   -- ****f* Stories/ProgressStory
   -- FUNCTION
   -- Progress current story one step
   -- PARAMETERS
   -- NextStep - Used with DESTROYSHIP condition. If false, progress to the
   --            next step in story. Default is false.
   -- RESULT
   -- True if story goes to next step, otherwise false
   -- SOURCE
   function ProgressStory(NextStep: Boolean := False) return Boolean;
   -- ****
   -- ****f* Stories/GetCurrentStoryText
   -- FUNCTION
   -- Get text of current step in story
   -- RESULT
   -- Text of current step in current story
   -- SOURCE
   function GetCurrentStoryText return Unbounded_String;
   -- ****
   -- ****f* Stories/GetStepData
   -- FUNCTION
   -- Get step finish data with selected name
   -- PARAMETERS
   -- FinishData - List of step data
   -- Name       - Name of data to get
   -- RESULT
   -- Selected data from FinishData parameter
   -- SOURCE
   function GetStepData
     (FinishData: StepData_Container.Vector; Name: String)
      return Unbounded_String with
      Pre => Name /= "";
      -- ****
      -- ****f* Stories/GetStoryLocation
      -- FUNCTION
      -- Get target location of current story
      -- PARAMETERS
      -- StoryX - X coordination of current story target
      -- StoryY - Y coordination of current story target
      -- RESULT
      -- Parameters X and Y
      -- SOURCE
   procedure GetStoryLocation(StoryX, StoryY: in out Positive);
   -- ****

end Stories;
