--    Copyright 2016-2019 Bartek thindil Jasicki
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Game; use Game;
with Bases; use Bases;

package Events is

   type Events_Types is
     (None, EnemyShip, AttackOnBase, Disease, DoublePrice, BaseRecovery,
      FullDocks, EnemyPatrol, Trader, FriendlyShip); -- Types of events
   type EventData(EType: Events_Types := None)
   is -- Data structure for random events
   record
      SkyX: Integer; -- X coordinate on sky map
      SkyY: Integer; -- Y coordinate on sky map
      Time: Integer; -- Time to end of event
      case EType is
         when DoublePrice =>
            ItemIndex: Unbounded_String; -- Index of proto item which have bonus to price
         when AttackOnBase | EnemyShip | EnemyPatrol | Trader | FriendlyShip =>
            ShipIndex: Unbounded_String; -- Index of proto ship which player meet
         when others =>
            Data: Natural; -- Various data for event (for example index of enemy ship)
      end case;
   end record;
   package Events_Container is new Vectors(Positive, EventData);
   Events_List: Events_Container.Vector; -- List of all events in the game
   Traders: UnboundedString_Container
     .Vector; -- List of indexes of all friendly traders in the game
   FriendlyShips: UnboundedString_Container
     .Vector; -- List of indexes of all friendly ships in the game

   function CheckForEvent
     return Boolean; -- Check if event happen, returns True, if combat starts
   procedure UpdateEvents(Minutes: Positive); -- Update all events timers
   procedure DeleteEvent(EventIndex: Positive) with
      Pre => EventIndex <= Events_List.Last_Index; -- Delete selected event
   procedure GenerateTraders; -- Create list of traders needed for trader event
   procedure RecoverBase(BaseIndex: BasesRange); -- Recover abandoned base
   procedure GenerateEnemies(Enemies: in out UnboundedString_Container.Vector;
      Owner: Unbounded_String := To_Unbounded_String("Any");
      WithTraders: Boolean := True) with
      Pre => Owner /= Null_Unbounded_String; -- Create list of enemies ships

end Events;
