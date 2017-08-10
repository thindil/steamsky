--    Copyright 2016-2017 Bartek thindil Jasicki
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

with Ada.Containers.Vectors; use Ada.Containers;
with Game; use Game;

package Events is

   type Events_Types is
     (None,
      EnemyShip,
      AttackOnBase,
      Disease,
      DoublePrice,
      FullDocks,
      EnemyPatrol,
      Trader); -- Types of events
   type EventData is -- Data structure for random events
   record
      EType: Events_Types; -- Type of event
      SkyX: Integer; -- X coordinate on sky map
      SkyY: Integer; -- Y coordinate on sky map
      Time: Integer; -- Time to end of event
      Data: Positive; -- Various data for event (for example index of enemy ship)
   end record;
   package Events_Container is new Vectors(Positive, EventData);
   Events_List: Events_Container.Vector;

   function CheckForEvent
     (OldState: GameStates) return GameStates; -- Check if event happen
   procedure UpdateEvents(Minutes: Positive); -- Update all events timers
   procedure DeleteEvent(EventIndex: Positive); -- Delete selected event
   procedure GenerateTraders; -- Create list of traders needed for trader event

end Events;
