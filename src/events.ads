--    Copyright 2016-2023 Bartek thindil Jasicki
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
with Ships; use Ships;

-- ****h* Events/Events
-- FUNCTION
-- Provide code to generate and update random events
-- SOURCE
package Events is
-- ****

   -- ****t* Events/Events.Events_Types
   -- FUNCTION
   -- Types of events
   -- SOURCE
   type Events_Types is
     (NONE, ENEMYSHIP, ATTACKONBASE, DISEASE, DOUBLEPRICE, BASERECOVERY,
      FULLDOCKS, ENEMYPATROL, TRADER, FRIENDLYSHIP) with
      Default_Value => NONE;
      -- ****

      -- ****d* Events/Events.No_Event
      -- FUNCTION
      -- Default value for Events_Types
      -- SOURCE
   No_Event: constant Events_Types := NONE;
   -- ****

   -- ****s* Events/Events.Event_Data
   -- FUNCTION
   -- Data structure for random events
   -- PARAMETERS
   -- E_Type     - The type of the event
   -- Sky_X      - X coordinate on sky map
   -- Sky_Y      - Y coordinate on sky map
   -- Time       - Time to end of event
   -- Item_Index - Index of proto item which have bonus to price
   -- Ship_Index - Index of proto ship which player meet
   -- Data      - Various data for event (for example index of enemy ship)
   -- SOURCE
   type Event_Data(E_Type: Events_Types := NONE) is record
      Sky_X: Map_X_Range;
      Sky_Y: Map_Y_Range;
      Time: Positive;
      case E_Type is
         when DOUBLEPRICE =>
            Item_Index: Positive;
         when ATTACKONBASE | ENEMYSHIP | ENEMYPATROL | TRADER | FRIENDLYSHIP =>
            Ship_Index: Positive;
         when others =>
            Data: Natural := 0;
      end case;
   end record;
   -- ****

   -- ****d* Events/Events.Empty_Event
   -- FUNCTION
   -- Default value for Event_Data, an empty event
   -- SOURCE
   Empty_Event: constant Event_Data :=
     (E_Type => No_Event, Sky_X => 1, Sky_Y => 1, Time => 1, Data => 0);
   -- ****

   -- ****t* Events/Events.Events_Container
   -- FUNCTION
   -- Used to store events data
   -- SOURCE
   package Events_Container is new Vectors
     (Index_Type => Positive, Element_Type => Event_Data);
   -- ****

   -- ****v* Events/Events.Events_List
   -- FUNCTION
   -- List of all events in the game
   -- SOURCE
   Events_List: Events_Container.Vector;
   -- ****

   -- ****f* Events/Events.Check_For_Event
   -- FUNCTION
   -- Check if event happen
   -- RESULT
   -- Return true if combat starts, otherwise false
   -- SOURCE
   function Check_For_Event return Boolean;
   -- ****

   -- ****f* Events/Events.Update_Events
   -- FUNCTION
   -- Update all events timers
   -- PARAMETERS
   -- Minutes - Amount of in-game minutes which passed
   -- SOURCE
   procedure Update_Events(Minutes: Positive);
   -- ****

   -- ****f* Events/Events.Delete_Event
   -- FUNCTION
   -- Delete selected event
   -- PARAMETERS
   -- EventIndex - Index of the event to delete
   -- SOURCE
   procedure Delete_Event(Event_Index: Positive) with
      Pre => Event_Index <= Events_List.Last_Index;
      -- ****

      -- ****f* Events/Events.Generate_Traders
      -- FUNCTION
      -- Create list of traders needed for trader event
      -- SOURCE
   procedure Generate_Traders;
   -- ****

   -- ****f* Events/Events.Recover_Base
   -- FUNCTION
   -- Recover abandoned base
   -- PARAMETERS
   -- Base_Index - Index of the base where recovery happened
   -- SOURCE
   procedure Recover_Base(Base_Index: Bases_Range);
   -- ****

   -- ****f* Events/Events.GenerateEnemies
   -- FUNCTION
   -- Create list of enemies ships
   -- PARAMETERS
   -- Enemies      - List of enemies to generate
   -- Owner        - Index of faction which enemies list should contains.
   --                Default all factions
   -- With_Traders - Did list should contains enemy traders too. Default true
   -- SOURCE
   procedure Generate_Enemies
     (Enemies: in out Positive_Container.Vector;
      Owner: Tiny_String.Bounded_String :=
        Tiny_String.To_Bounded_String(Source => "Any");
      With_Traders: Boolean := True);
   -- ****

-- Temporary code to interact with Nim

   procedure Get_Ada_Event(Index, X, Y, Time, E_Type, Data: Integer) with
      Import => True,
      Convention => C,
      External_Name => "getAdaEvent";

   procedure Set_Event(Index: Positive);

   procedure Set_Nim_Events;

   function Get_Trader_Or_Friendly
     (Index, Get_Trader: Natural) return Natural with
      Import => True,
      Convention => C,
      External_Name => "getTraderOrFriendly";

end Events;
