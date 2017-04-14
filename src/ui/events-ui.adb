--    Copyright 2017 Bartek thindil Jasicki
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
with Ada.Numerics.Generic_Elementary_Functions;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Ships; use Ships;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Messages; use Messages;
with UserInterface; use UserInterface;
with Bases; use Bases;
with Items; use Items;

package body Events.UI is

   EventsMenu: Menu;
   MenuWindow: Window;

   procedure ShowEventInfo is
      InfoWindow: Window;
      EventIndex: constant Positive := Get_Index(Current(EventsMenu));
      type Value_Type is digits 2 range 0.0 .. 9999999.0;
      package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions
        (Value_Type);
      DiffX, DiffY: Natural;
      Distance: Value_Type;
   begin
      InfoWindow := Create(10, (Columns / 2), 4, (Columns / 2));
      Add
        (Win => InfoWindow,
         Str =>
           "X:" &
           Positive'Image(Events_List.Element(EventIndex).SkyX) &
           " Y:" &
           Positive'Image(Events_List.Element(EventIndex).SkyY));
      Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
      case Events_List.Element(EventIndex).EType is
         when EnemyShip | EnemyPatrol =>
            Add
              (Win => InfoWindow,
               Str =>
                 To_String
                   (ProtoShips_List.Element
                    (Events_List.Element(EventIndex).Data)
                      .Name));
         when FullDocks | AttackOnBase | Disease =>
            Add
              (Win => InfoWindow,
               Str =>
                 To_String
                   (SkyBases
                      (SkyMap
                         (Events_List.Element(EventIndex).SkyX,
                          Events_List.Element(EventIndex).SkyY)
                         .BaseIndex)
                      .Name));
         when DoublePrice =>
            Add
              (Win => InfoWindow,
               Str =>
                 To_String
                   (SkyBases
                      (SkyMap
                         (Events_List.Element(EventIndex).SkyX,
                          Events_List.Element(EventIndex).SkyY)
                         .BaseIndex)
                      .Name) &
                 " - " &
                 To_String
                   (Items_List.Element(Events_List.Element(EventIndex).Data)
                      .Name));
         when None =>
            null;
      end case;
      DiffX := abs (PlayerShip.SkyX - Events_List.Element(EventIndex).SkyX);
      DiffY := abs (PlayerShip.SkyY - Events_List.Element(EventIndex).SkyY);
      Distance := Value_Functions.Sqrt(Value_Type((DiffX**2) + (DiffY**2)));
      Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
      Add
        (Win => InfoWindow,
         Str =>
           "Distance:" & Integer'Image(Integer(Value_Type'Floor(Distance))));
      Move_Cursor(Win => InfoWindow, Line => 4, Column => 0);
      Add(Win => InfoWindow, Str => "Press SPACE to show event on map");
      Change_Attributes
        (Win => InfoWindow,
         Line => 4,
         Column => 6,
         Count => 5,
         Color => 1);
      Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
      Add
        (Win => InfoWindow,
         Str => "Press ENTER to set event as a destination for ship");
      Change_Attributes
        (Win => InfoWindow,
         Line => 5,
         Column => 6,
         Count => 5,
         Color => 1);
      Refresh;
      Refresh(InfoWindow);
      Delete(InfoWindow);
   end ShowEventInfo;

   procedure ShowEvents is
      Events_Items: constant Item_Array_Access :=
        new Item_Array
        (Events_List.First_Index .. (Events_List.Last_Index + 1));
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      for I in Events_List.First_Index .. Events_List.Last_Index loop
         case Events_List.Element(I).EType is
            when EnemyShip =>
               Events_Items.all(I) := New_Item("Enemy ship spotted");
            when FullDocks =>
               Events_Items.all(I) := New_Item("Full docks in base");
            when AttackOnBase =>
               Events_Items.all(I) := New_Item("Base is under attack");
            when Disease =>
               Events_Items.all(I) := New_Item("Disease in base");
            when EnemyPatrol =>
               Events_Items.all(I) := New_Item("Enemy patrol");
            when DoublePrice =>
               Events_Items.all(I) := New_Item("Double price in base");
            when None =>
               null;
         end case;
      end loop;
      Events_Items.all(Events_Items'Last) := Null_Item;
      if Events_Items.all(1) /= Null_Item then
         EventsMenu := New_Menu(Events_Items);
         Set_Options(EventsMenu, (Show_Descriptions => False, others => True));
         Set_Format(EventsMenu, Lines - 4, 1);
         Set_Mark(EventsMenu, "");
         Scale(EventsMenu, MenuHeight, MenuLength);
         MenuWindow := Create(MenuHeight, MenuLength, 4, 2);
         Set_Window(EventsMenu, MenuWindow);
         Set_Sub_Window
           (EventsMenu,
            Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
         Post(EventsMenu);
         ShowEventInfo;
         Refresh(MenuWindow);
      else
         Move_Cursor(Line => (Lines / 3), Column => (Columns / 2) - 21);
         Add(Str => "You don't know about any events.");
         Refresh;
      end if;
   end ShowEvents;

   function ShowEventsKeys(Key: Key_Code) return GameStates is
      Result: Driver_Result;
      EventIndex: Positive;
   begin
      if EventsMenu /= Null_Menu then
         EventIndex := Get_Index(Current(EventsMenu));
         case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
               Post(EventsMenu, False);
               Delete(EventsMenu);
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            when KEY_UP => -- Select previous event
               Result := Driver(EventsMenu, M_Up_Item);
               if Result = Request_Denied then
                  Result := Driver(EventsMenu, M_Last_Item);
               end if;
            when KEY_DOWN => -- Select next event
               Result := Driver(EventsMenu, M_Down_Item);
               if Result = Request_Denied then
                  Result := Driver(EventsMenu, M_First_Item);
               end if;
            when 32 => -- Show selected event on map
               MoveMap
                 (Events_List.Element(EventIndex).SkyX,
                  Events_List.Element(EventIndex).SkyY);
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            when 10 => -- Set event as destination point for ship
               if Events_List.Element(EventIndex).SkyX = PlayerShip.SkyX and
                 Events_List.Element(EventIndex).SkyY = PlayerShip.SkyY then
                  ShowDialog("You are at this event now.");
                  DrawGame(Events_View);
                  return Events_View;
               end if;
               PlayerShip.DestinationX := Events_List.Element(EventIndex).SkyX;
               PlayerShip.DestinationY := Events_List.Element(EventIndex).SkyY;
               AddMessage
                 ("You set travel destination for your ship.",
                  OrderMessage);
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            when others =>
               Result := Driver(EventsMenu, Key);
               if Result = Menu_Ok then
                  Refresh(MenuWindow);
               else
                  Result := Driver(EventsMenu, M_Clear_Pattern);
                  Result := Driver(EventsMenu, Key);
                  if Result = Menu_Ok then
                     Refresh(MenuWindow);
                  end if;
               end if;
         end case;
         if Result = Menu_Ok then
            ShowEventInfo;
            Refresh(MenuWindow);
         end if;
      else
         case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            when others =>
               null;
         end case;
      end if;
      return Events_View;
   end ShowEventsKeys;

end Events.UI;
