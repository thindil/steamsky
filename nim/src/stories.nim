# Copyright 2023 Bartek thindil Jasicki
#
# This file is part of Steam Sky.
#
# Steam Sky is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Steam Sky is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Steam Sky.  If not, see <http://www.gnu.org/licenses/>.

import std/tables

type
  StartConditionType = enum
    ## Types of requirements to start a story
    dropItem
  StepConditionType = enum
    ## Types of requirements to finish a story step
    askinbase, destroyship, explore, any, loot

  StepTextData = object
    ## Used to store stories' steps' texts
    ## * condition - the requirement for the previous step of a story
    ## * text      - uhe text which will be shown to the player when the step starts
    condition: StepConditionType
    text*: string

  StepFinishData = object
    ## Used to store information about requirements to finish a story's step
    ## * name  - the name of the data
    ## * value - the data's value
    name, value: string

  StepData = object
    ## Used to store information about stories' steps
    ## * index           - the index of the step
    ## * finishCondition - the condition which must be meet to finish the step and
    ##                     progress to the next step
    ## * finishData      - the data for the finish condition
    ## * texts           - the text which will be shown to the player when the step
    ##                     starts, depends on the finish condition of the previous
    ##                     step
    ## * failText        - the text which will be show to the player when the step
    ##                     fails
    index: string
    finishCondition: StepConditionType
    finishData: seq[StepFinishData]
    texts: seq[StepTextData]
    failText: string

  StoryData = object
    ## Used to store information about a story
    ## * startCondition    - the condition which must be meet to start the story
    ## * startData         - the data of the starting condition
    ## * minSteps          - the minimal amount of steps in the story
    ## * maxSteps          - the maximum amount of steps in the story
    ## * startingStep      - the starting step of the story
    ## * steps             - contains all steps of the story
    ## * finalStep         - the final step of the story
    ## * endText           - the text which will be show to the player when the
    ##                       story ends
    ## * name              - the name of the story, show in the game
    ## * forbiddenFactions - if the player belongs to one of these factions, it
    ##                       can't start the story
    startCondition: StartConditionType
    startData: seq[string]
    minSteps: Positive
    maxSteps: Positive
    startingStep: StepData
    steps: seq[StepData]
    finalStep: StepData
    endText: string
    name: string
    forbiddenFactions: seq[string]

  CurrentStoryData = object
    ## Used to store information about the current story
    ## * index        - the index of the story or empty string if no story active
    ## * step         - the number of the step of the current story
    ## * currentStep  - the index of the current step of the story, 0 for starting
    ##                  step, -1 for finishing step
    ## * maxSteps     - the number of maximum amount of steps in the story
    ## * showText     - if true, show the text of the current step to the player
    ## * data         - various data for the current step of the story, depends on
    ##                  the step
    ## * finishedStep - the finish condition for the previous step in the story
    index*: string
    step*: Positive
    currentStep*: int
    maxSteps*: Positive
    showText*: bool
    data*: string
    finishedStep*: StepConditionType

var
  storiesList* = initTable[string, StoryData]() ## The list of available stories in the game
  currentStory*: CurrentStoryData = CurrentStoryData(step: 1,
      maxSteps: 1) ## Contains data about the current story on which the player is
