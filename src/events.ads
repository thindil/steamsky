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

-- with Ada.Containers.Vectors; use Ada.Containers;
with Game; use Game;

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

   --## rule off REDUCEABLE_SCOPE
   -- ****d* Events/Events.Empty_Event
   -- FUNCTION
   -- Default value for Event_Data, an empty event
   -- SOURCE
   Empty_Event: constant Event_Data :=
     (E_Type => No_Event, Sky_X => 1, Sky_Y => 1, Time => 1, Data => 0);
   -- ****
   --## rule on REDUCEABLE_SCOPE

   -- ****f* Events/Events.Check_For_Event
   -- FUNCTION
   -- Check if event happen
   -- RESULT
   -- Return true if combat starts, otherwise false
   -- SOURCE
   function Check_For_Event return Boolean;
   -- ****

   -- ****f* Events/Events.Delete_Event
   -- FUNCTION
   -- Delete selected event
   -- PARAMETERS
   -- EventIndex - Index of the event to delete
   -- SOURCE
   procedure Delete_Event(Event_Index: Positive);
   -- ****

   -- ****f* Events/Events.Generate_Traders
   -- FUNCTION
   -- Create list of traders needed for trader event
   -- SOURCE
   procedure Generate_Traders;
   -- ****

-- Temporary code to interact with Nim

   procedure Get_Ada_Event(Index, X, Y, Time, E_Type, Data: Integer) with
      Import => True,
      Convention => C,
      External_Name => "getAdaEvent";

   procedure Set_Event(Index: Positive);

   function Get_Trader_Or_Friendly
     (Index, Get_Trader: Natural) return Natural with
      Import => True,
      Convention => C,
      External_Name => "getTraderOrFriendly";

   function Get_Event(Index: Positive) return Event_Data;

   function Get_Events_Amount return Natural with
      Import => True,
      Convention => C,
      External_Name => "getAdaEventsAmount";

end Events;
