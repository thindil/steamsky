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

   type Step_Data is -- Data structure for stories steps
   record
      Index: Unbounded_String; -- Index of step
      FinishCondition: Unbounded_String; -- Condition which must be met to finish selected step and process to next
      FinishData: UnboundedString_Container
        .Vector; -- Data for finish condition
      Text: Unbounded_String; -- Text which will be show to player when step starts.
   end record;
   package Steps_Container is new Vectors(Positive, Step_Data);
   type Story_Data is -- Data structure for stories
   record
      Index: Unbounded_String; -- Index of story
      StartCondition: Unbounded_String; -- Condition which must be met to start story
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
      Index: Unbounded_String; -- Index of story
      Step: Positive; -- Number of current step in story
      CurrentStep: Unbounded_String; -- Index of current step
      MaxSteps: Positive; -- Number of maxium  amounts of steps in story
   end record;
   CurrentStory: CurrentStory_Data; -- Contains data about current story on which player is
   Stories_List: Stories_Container.Vector; -- List of available stories in game
   Stories_Directory_Not_Found: exception; -- Raised when no directory with stories files
   Stories_Files_Not_Found: exception; -- Raised when no files with stories

   procedure LoadStories; -- Load stories data from files
   procedure ClearCurrentStory; -- Reset current story

end Stories;
