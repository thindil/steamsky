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

-- ****h* Steamsky/Events
-- FUNCTION
-- Provide code to generate and update random events
-- SOURCE
package Events is
-- ****

   -- ****t* Events/Events_Types
   -- FUNCTION
   -- Types of events
   -- SOURCE
   type Events_Types is
     (None, EnemyShip, AttackOnBase, Disease, DoublePrice, BaseRecovery,
      FullDocks, EnemyPatrol, Trader, FriendlyShip);
   -- ****

   -- ****t* Events/EventData
   -- FUNCTION
   -- Data structure for random events
   -- PARAMETERS
   -- SkyX      - X coordinate on sky map
   -- SkyY      - Y coordinate on sky map
   -- Time      - Time to end of event
   -- ItemIndex - Index of proto item which have bonus to price
   -- ShipIndex - Index of proto ship which player meet
   -- Data      - Various data for event (for example index of enemy ship)
   -- SOURCE
   type EventData(EType: Events_Types := None) is record
      SkyX: Integer;
      SkyY: Integer;
      Time: Integer;
      case EType is
         when DoublePrice =>
            ItemIndex: Unbounded_String;
         when AttackOnBase | EnemyShip | EnemyPatrol | Trader | FriendlyShip =>
            ShipIndex: Unbounded_String;
         when others =>
            Data: Natural;
      end case;
   end record;
   -- ****

   -- ****t* Events/Events_Container
   -- FUNCTION
   -- Used to store events data
   -- SOURCE
   package Events_Container is new Vectors(Positive, EventData);
   -- ****

   -- ****v* Events/Events_List
   -- FUNCTION
   -- List of all events in the game
   -- SOURCE
   Events_List: Events_Container.Vector;
   -- ****

   -- ****v* Events/Traders
   -- FUNCTION
   -- List of indexes of all friendly traders in the game
   -- SOURCE
   Traders: UnboundedString_Container.Vector;
   -- ****

   -- ****v* Events/FriendlyShips
   -- FUNCTION
   -- List of indexes of all friendly ships in the game
   -- SOURCE
   FriendlyShips: UnboundedString_Container.Vector;
   -- ****

   -- ****f* Events/CheckForEvent
   -- FUNCTION
   -- Check if event happen
   -- RESULT
   -- Return true if combat starts, otherwise false
   -- SOURCE
   function CheckForEvent return Boolean with
      Test_Case => ("Test_CheckForEvent", Robustness);
      -- ****

      -- ****f* Events/UpdateEvents
      -- FUNCTION
      -- Update all events timers
      -- PARAMETERS
      -- Minutes - Amount of in-game minutes which passed
      -- SOURCE
   procedure UpdateEvents(Minutes: Positive) with
      Test_Case => ("Test_UpdateEvents", Robustness);
      -- ****

      -- ****f* Events/DeleteEvent
      -- FUNCTION
      -- Delete selected event
      -- PARAMETERS
      -- EventIndex - Index of the event to delete
      -- SOURCE
   procedure DeleteEvent(EventIndex: Positive) with
      Pre => EventIndex <= Events_List.Last_Index,
      Test_Case => ("Test_DeleteEvent", Nominal);
      -- ****

      -- ****f* Events/GenerateTraders
      -- FUNCTION
      -- Create list of traders needed for trader event
      -- SOURCE
   procedure GenerateTraders with
      Test_Case => ("Test_GenerateTraders", Robustness);
      -- ****

      -- ****f* Events/RecoverBase
      -- FUNCTION
      -- Recover abandoned base
      -- PARAMETERS
      -- BaseIndex - Index of the base where recovery happened
      -- SOURCE
   procedure RecoverBase(BaseIndex: BasesRange) with
      Test_Case => ("Test_RecoverBase", Robustness);
      -- ****

      -- ****f* Events/GenerateEnemies
      -- FUNCTION
      -- Create list of enemies ships
      -- PARAMETERS
      -- Enemies     - List of enemies to generate
      -- Owner       - Index of faction which enemies list should contains.
      --               Default all factions
      -- WithTraders - Did list should contains enemy traders too. Default true
      -- SOURCE
   procedure GenerateEnemies
     (Enemies: in out UnboundedString_Container.Vector;
      Owner: Unbounded_String := To_Unbounded_String("Any");
      WithTraders: Boolean := True) with
      Pre => Owner /= Null_Unbounded_String;
      -- ****

end Events;
