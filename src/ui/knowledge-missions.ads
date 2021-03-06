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

-- ****h* Knowledge/KMissions
-- FUNCTION
-- Provide code to show the list of known events to the player
-- SOURCE
package Knowledge.Missions is
-- ****

   -- ****f* KMissions/KMissions.AddCommands
   -- FUNCTION
   -- Add Tcl commands related to the list of known bases
   -- SOURCE
   procedure AddCommands;
   -- ****

   -- ****f* KMissions/KMissions.UpdateMissionsList
   -- FUNCTION
   -- Update and show list of accepted missions
   -- PARAMETERS
   -- Page     - The current page of missions list to show
   -- SOURCE
   procedure UpdateMissionsList(Page: Positive := 1);
   -- ****

end Knowledge.Missions;
