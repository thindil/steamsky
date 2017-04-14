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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Numerics.Generic_Elementary_Functions;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Bases; use Bases;
with UserInterface; use UserInterface;
with Messages; use Messages;
with Maps; use Maps;
with Maps.UI; use Maps.UI;
with Ships; use Ships;
with Help.UI; use Help.UI;

package body BasesList is

   BasesMenu, OptionsMenu: Menu;
   BasesType: Bases_Types := Any;
   BasesStatus: Positive := 1;
   MenuWindow, MenuWindow2: Window;
   CurrentMenuIndex: Positive := 1;
   BasesOwner: Bases_Owners := Any;

   procedure ShowBaseInfo is
      InfoWindow: Window;
      BaseIndex: constant Positive :=
        Positive'Value(Description(Current(BasesMenu)));
      SearchPattern: String(1 .. 250);
      TrimedSearchPattern: Unbounded_String;
      type Value_Type is digits 2 range 0.0 .. 9999999.0;
      package Value_Functions is new Ada.Numerics.Generic_Elementary_Functions
        (Value_Type);
      DiffX, DiffY: Natural;
      CurrentLine: Line_Position := 1;
      Distance: Value_Type;
      TimeDiff: Integer;
   begin
      InfoWindow := Create(20, (Columns / 2), 4, (Columns / 2));
      if SkyBases(BaseIndex).Visited.Year > 0 then
         Add
           (Win => InfoWindow,
            Str =>
              "X:" &
              Positive'Image(SkyBases(BaseIndex).SkyX) &
              " Y:" &
              Positive'Image(SkyBases(BaseIndex).SkyX));
         Move_Cursor(Win => InfoWindow, Line => 1, Column => 0);
         Add
           (Win => InfoWindow,
            Str =>
              "Type: " &
              To_Lower(Bases_Types'Image(SkyBases(BaseIndex).BaseType)));
         Move_Cursor(Win => InfoWindow, Line => 2, Column => 0);
         Add
           (Win => InfoWindow,
            Str =>
              "Owner: " &
              To_Lower(Bases_Owners'Image(SkyBases(BaseIndex).Owner)));
         Move_Cursor(Win => InfoWindow, Line => 3, Column => 0);
         Add(Win => InfoWindow, Str => "Size: ");
         if SkyBases(BaseIndex).Population < 150 then
            Add(Win => InfoWindow, Str => "small");
         elsif SkyBases(BaseIndex).Population > 149 and
           SkyBases(BaseIndex).Population < 300 then
            Add(Win => InfoWindow, Str => "medium");
         else
            Add(Win => InfoWindow, Str => "large");
         end if;
         Move_Cursor(Win => InfoWindow, Line => 4, Column => 0);
         Add
           (Win => InfoWindow,
            Str =>
              "Last visited: " & FormatedTime(SkyBases(BaseIndex).Visited));
         Move_Cursor(Win => InfoWindow, Line => 5, Column => 0);
         if SkyBases(BaseIndex).Owner /= Abandoned and
           SkyBases(BaseIndex).Reputation(1) > -25 then
            TimeDiff :=
              30 -
              ((GameDate.Day + ((30 * GameDate.Month) * GameDate.Year)) -
               (SkyBases(BaseIndex).RecruitDate.Day +
                ((30 * SkyBases(BaseIndex).RecruitDate.Month) *
                 SkyBases(BaseIndex).RecruitDate.Year)));
            if TimeDiff > 0 then
               Add
                 (Win => InfoWindow,
                  Str =>
                    "New recruits available in" &
                    Natural'Image(TimeDiff) &
                    " days.");
            else
               Add(Win => InfoWindow, Str => "New recruits available now.");
            end if;
         else
            Add
              (Win => InfoWindow,
               Str => "You can't recruit crew members on this base.");
         end if;
         Move_Cursor(Win => InfoWindow, Line => 6, Column => 0);
         if SkyBases(BaseIndex).Owner /= Abandoned and
           SkyBases(BaseIndex).Reputation(1) > -25 then
            TimeDiff :=
              (GameDate.Day + ((30 * GameDate.Month) * GameDate.Year)) -
              (SkyBases(BaseIndex).AskedForEvents.Day +
               ((30 * SkyBases(BaseIndex).AskedForEvents.Month) *
                SkyBases(BaseIndex).AskedForEvents.Year));
            if TimeDiff < 7 then
               Add
                 (Win => InfoWindow,
                  Str =>
                    "You asked for events" &
                    Natural'Image(TimeDiff) &
                    " days ago.");
            else
               Add(Win => InfoWindow, Str => "You can ask for events again.");
            end if;
         else
            Add
              (Win => InfoWindow,
               Str => "You can't ask for events in this base.");
         end if;
         Move_Cursor(Win => InfoWindow, Line => 7, Column => 0);
         if SkyBases(BaseIndex).Owner /= Abandoned and
           SkyBases(BaseIndex).Reputation(1) > -1 then
            TimeDiff :=
              7 -
              ((GameDate.Day + ((30 * GameDate.Month) * GameDate.Year)) -
               (SkyBases(BaseIndex).MissionsDate.Day +
                ((30 * SkyBases(BaseIndex).MissionsDate.Month) *
                 SkyBases(BaseIndex).MissionsDate.Year)));
            if TimeDiff > 0 then
               Add
                 (Win => InfoWindow,
                  Str =>
                    "New missions available in" &
                    Natural'Image(TimeDiff) &
                    " days.");
            else
               Add(Win => InfoWindow, Str => "New missions available now.");
            end if;
         else
            Add
              (Win => InfoWindow,
               Str => "You can't take missions in this base.");
         end if;
         Move_Cursor(Win => InfoWindow, Line => 8, Column => 0);
         Add(Win => InfoWindow, Str => "Reputation: ");
         case SkyBases(BaseIndex).Reputation(1) is
            when -100 .. -75 =>
               Add(Win => InfoWindow, Str => "Hated");
            when -74 .. -50 =>
               Add(Win => InfoWindow, Str => "Outlaw");
            when -49 .. -25 =>
               Add(Win => InfoWindow, Str => "Hostile");
            when -24 .. -1 =>
               Add(Win => InfoWindow, Str => "Unfriendly");
            when 0 =>
               Add(Win => InfoWindow, Str => "Unknown");
            when 1 .. 25 =>
               Add(Win => InfoWindow, Str => "Visitor");
            when 26 .. 50 =>
               Add(Win => InfoWindow, Str => "Trader");
            when 51 .. 75 =>
               Add(Win => InfoWindow, Str => "Friend");
            when 76 .. 100 =>
               Add(Win => InfoWindow, Str => "Well known");
            when others =>
               null;
         end case;
         CurrentLine := 9;
      else
         Add(Win => InfoWindow, Str => "Not visited yet.");
      end if;
      DiffX := abs (PlayerShip.SkyX - SkyBases(BaseIndex).SkyX);
      DiffY := abs (PlayerShip.SkyY - SkyBases(BaseIndex).SkyY);
      Distance := Value_Functions.Sqrt(Value_Type((DiffX**2) + (DiffY**2)));
      Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
      Add
        (Win => InfoWindow,
         Str =>
           "Distance:" & Integer'Image(Integer(Value_Type'Floor(Distance))));
      CurrentLine := CurrentLine + 2;
      Pattern(BasesMenu, SearchPattern);
      TrimedSearchPattern :=
        Trim(To_Unbounded_String(SearchPattern), Ada.Strings.Both);
      if Length(TrimedSearchPattern) > 0 then
         Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
         Add
           (Win => InfoWindow,
            Str => "Search: " & To_String(TrimedSearchPattern));
         CurrentLine := CurrentLine + 2;
      end if;
      Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
      Add(Win => InfoWindow, Str => "Press SPACE to show base on map");
      Change_Attributes
        (Win => InfoWindow,
         Line => CurrentLine,
         Column => 6,
         Count => 5,
         Color => 1);
      CurrentLine := CurrentLine + 1;
      Move_Cursor(Win => InfoWindow, Line => CurrentLine, Column => 0);
      Add
        (Win => InfoWindow,
         Str => "Press ENTER to set base as a destination for ship");
      Change_Attributes
        (Win => InfoWindow,
         Line => CurrentLine,
         Column => 6,
         Count => 5,
         Color => 1);
      Refresh;
      Refresh(InfoWindow);
      Delete(InfoWindow);
   end ShowBaseInfo;

   procedure ShowBasesList is
      Bases_Items: Item_Array_Access;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
      MenuIndex: Positive := 1;
      AddBase: Boolean := False;
      MenuAmount: Natural := 0;
   begin
      for I in SkyBases'Range loop
         if SkyBases(I).Known then
            case BasesStatus is
               when 1 => -- All bases
                  if
                    (BasesType = Any or
                     (BasesType /= Any and
                      SkyBases(I).Visited.Year > 0 and
                      SkyBases(I).BaseType = BasesType)) and
                    (BasesOwner = Any or
                     (BasesOwner /= Any and
                      SkyBases(I).Visited.Year > 0 and
                      SkyBases(I).Owner = BasesOwner)) then
                     AddBase := True;
                  end if;
               when 2 => -- Only visited bases
                  if
                    ((BasesType = Any or
                      (BasesType /= Any and
                       SkyBases(I).BaseType = BasesType)) and
                     (BasesOwner = Any or
                      (BasesOwner /= Any and
                       SkyBases(I).Owner = BasesOwner))) and
                    SkyBases(I).Visited.Year > 0 then
                     AddBase := True;
                  end if;
               when 3 => -- Only not visited bases
                  if SkyBases(I).Visited.Year = 0 then
                     AddBase := True;
                  end if;
               when others =>
                  null;
            end case;
            if AddBase then
               MenuAmount := MenuAmount + 1;
               AddBase := False;
            end if;
         end if;
      end loop;
      Bases_Items := new Item_Array(1 .. (MenuAmount + 1));
      AddBase := False;
      for I in SkyBases'Range loop
         if SkyBases(I).Known then
            case BasesStatus is
               when 1 => -- All bases
                  if
                    (BasesType = Any or
                     (BasesType /= Any and
                      SkyBases(I).Visited.Year > 0 and
                      SkyBases(I).BaseType = BasesType)) and
                    (BasesOwner = Any or
                     (BasesOwner /= Any and
                      SkyBases(I).Visited.Year > 0 and
                      SkyBases(I).Owner = BasesOwner)) then
                     AddBase := True;
                  end if;
               when 2 => -- Only visited bases
                  if
                    ((BasesType = Any or
                      (BasesType /= Any and
                       SkyBases(I).BaseType = BasesType)) and
                     (BasesOwner = Any or
                      (BasesOwner /= Any and
                       SkyBases(I).Owner = BasesOwner))) and
                    SkyBases(I).Visited.Year > 0 then
                     AddBase := True;
                  end if;
               when 3 => -- Only not visited bases
                  if SkyBases(I).Visited.Year = 0 then
                     AddBase := True;
                  end if;
               when others =>
                  null;
            end case;
            if AddBase then
               Bases_Items.all(MenuIndex) :=
                 New_Item(To_String(SkyBases(I).Name), Positive'Image(I));
               MenuIndex := MenuIndex + 1;
               AddBase := False;
            end if;
         end if;
      end loop;
      Bases_Items.all(Bases_Items'Last) := Null_Item;
      Move_Cursor(Line => 2, Column => 5);
      Add(Str => "F2 Type: " & To_Lower(Bases_Types'Image(BasesType)));
      Change_Attributes(Line => 2, Column => 5, Count => 2, Color => 1);
      Move_Cursor(Line => 2, Column => 30);
      Add(Str => "F3 Status: ");
      case BasesStatus is
         when 1 =>
            Add(Str => "any");
         when 2 =>
            Add(Str => "only visited");
         when 3 =>
            Add(Str => "only not visited");
         when others =>
            null;
      end case;
      Change_Attributes(Line => 2, Column => 30, Count => 2, Color => 1);
      Move_Cursor(Line => 2, Column => 60);
      Add(Str => "F4 Owner: " & To_Lower(Bases_Owners'Image(BasesOwner)));
      Change_Attributes(Line => 2, Column => 60, Count => 2, Color => 1);
      if Bases_Items.all(1) /= Null_Item then
         BasesMenu := New_Menu(Bases_Items);
         Set_Options(BasesMenu, (Show_Descriptions => False, others => True));
         Set_Format(BasesMenu, Lines - 4, 1);
         Set_Mark(BasesMenu, "");
         Scale(BasesMenu, MenuHeight, MenuLength);
         MenuWindow := Create(MenuHeight, MenuLength, 4, 2);
         Set_Window(BasesMenu, MenuWindow);
         Set_Sub_Window
           (BasesMenu,
            Derived_Window(MenuWindow, MenuHeight, MenuLength, 0, 0));
         Post(BasesMenu);
         Set_Current(BasesMenu, Bases_Items.all(CurrentMenuIndex));
         ShowBaseInfo;
         Refresh(MenuWindow);
      else
         Move_Cursor(Line => (Lines / 3), Column => (Columns / 2) - 21);
         Add(Str => "You don't visited yet any base that type.");
         Refresh;
      end if;
   end ShowBasesList;

   procedure ShowBasesOptions(CurrentState: GameStates) is
      Options_Items: Item_Array_Access;
      MenuIndex: Positive;
      MenuHeight: Line_Position;
      MenuLength: Column_Position;
   begin
      case CurrentState is
         when BasesList_Types =>
            Options_Items :=
              new Item_Array(1 .. (Bases_Types'Pos(Bases_Types'Last) + 3));
            for I in Bases_Types loop
               Options_Items.all(Bases_Types'Pos(I) + 1) :=
                 New_Item(To_Lower(Bases_Types'Image(I)));
            end loop;
            MenuIndex := Bases_Types'Pos(Bases_Types'Last) + 2;
         when BasesList_Statuses =>
            Options_Items := new Item_Array(1 .. 5);
            Options_Items.all(1) := New_Item("any");
            Options_Items.all(2) := New_Item("only visited");
            Options_Items.all(3) := New_Item("only not visited");
            MenuIndex := 4;
         when BasesList_Owners =>
            Options_Items :=
              new Item_Array(1 .. (Bases_Owners'Pos(Bases_Owners'Last) + 3));
            for I in Bases_Owners loop
               Options_Items.all(Bases_Owners'Pos(I) + 1) :=
                 New_Item(To_Lower(Bases_Owners'Image(I)));
            end loop;
            MenuIndex := Bases_Owners'Pos(Bases_Owners'Last) + 2;
         when others =>
            null;
      end case;
      Options_Items.all(MenuIndex) := New_Item("quit");
      MenuIndex := MenuIndex + 1;
      Options_Items.all(MenuIndex) := Null_Item;
      OptionsMenu := New_Menu(Options_Items);
      Set_Mark(OptionsMenu, "");
      Scale(OptionsMenu, MenuHeight, MenuLength);
      MenuWindow2 :=
        Create
          (MenuHeight + 2,
           MenuLength + 2,
           ((Lines / 3) - (MenuHeight / 2)),
           ((Columns / 2) - (MenuLength / 2)));
      Box(MenuWindow2);
      Set_Window(OptionsMenu, MenuWindow2);
      Set_Sub_Window
        (OptionsMenu,
         Derived_Window(MenuWindow2, MenuHeight, MenuLength, 1, 1));
      Post(OptionsMenu);
      Refresh(MenuWindow2);
      Refresh;
   end ShowBasesOptions;

   function BasesListKeys(Key: Key_Code) return GameStates is
      Result: Driver_Result;
      BaseIndex: Positive;
   begin
      if BasesMenu /= Null_Menu then
         BaseIndex := Positive'Value(Description(Current(BasesMenu)));
         case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
               CurrentMenuIndex := 1;
               BasesStatus := 1;
               BasesType := Any;
               Post(BasesMenu, False);
               Delete(BasesMenu);
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            when KEY_UP => -- Select previous base
               Result := Driver(BasesMenu, M_Up_Item);
               if Result = Request_Denied then
                  Result := Driver(BasesMenu, M_Last_Item);
               end if;
            when KEY_DOWN => -- Select next base
               Result := Driver(BasesMenu, M_Down_Item);
               if Result = Request_Denied then
                  Result := Driver(BasesMenu, M_First_Item);
               end if;
            when KEY_NPAGE => -- Scroll list one screen down
               Result := Driver(BasesMenu, M_ScrollUp_Page);
            when KEY_PPAGE => -- Scroll list one screen up
               Result := Driver(BasesMenu, M_ScrollDown_Page);
            when Key_Home => -- Scroll list to start
               Result := Driver(BasesMenu, M_First_Item);
            when Key_End => -- Scroll list to end
               Result := Driver(BasesMenu, M_Last_Item);
            when 32 => -- Show selected base on map
               MoveMap(SkyBases(BaseIndex).SkyX, SkyBases(BaseIndex).SkyY);
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            when 10 => -- Set base as destination point for ship
               if SkyBases(BaseIndex).SkyX = PlayerShip.SkyX and
                 SkyBases(BaseIndex).SkyY = PlayerShip.SkyY then
                  ShowDialog("You are at this base now.");
                  DrawGame(Bases_List);
                  return Bases_List;
               end if;
               PlayerShip.DestinationX := SkyBases(BaseIndex).SkyX;
               PlayerShip.DestinationY := SkyBases(BaseIndex).SkyY;
               AddMessage
                 ("You set base " &
                  To_String(SkyBases(BaseIndex).Name) &
                  " as a destination for your ship.",
                  OrderMessage);
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            when Key_Backspace => -- Delete last searching character
               Result := Driver(BasesMenu, M_Back_Pattern);
            when KEY_DC => -- Clear whole searching string
               Result := Driver(BasesMenu, M_Clear_Pattern);
            when Key_F2 => -- Show bases types menu
               ShowBasesOptions(BasesList_Types);
               return BasesList_Types;
            when Key_F3 => -- Show bases statuses menu
               ShowBasesOptions(BasesList_Statuses);
               return BasesList_Statuses;
            when Key_F4 => -- Show bases owners menu
               ShowBasesOptions(BasesList_Owners);
               return BasesList_Owners;
            when Key_F1 => -- Show help
               Erase;
               ShowGameHeader(Help_Topic);
               ShowHelp(Bases_List, 8);
               return Help_Topic;
            when others =>
               Result := Driver(BasesMenu, Key);
         end case;
         if Result = Menu_Ok then
            ShowBaseInfo;
            Refresh(MenuWindow);
         end if;
         CurrentMenuIndex := Get_Index(Current(BasesMenu));
      else
         case Key is
            when Character'Pos('q') | Character'Pos('Q') => -- Back to sky map
               CurrentMenuIndex := 1;
               BasesType := Any;
               BasesStatus := 1;
               DrawGame(Sky_Map_View);
               return Sky_Map_View;
            when Key_F2 => -- Show bases types menu
               ShowBasesOptions(BasesList_Types);
               return BasesList_Types;
            when Key_F3 => -- Show bases statuses menu
               ShowBasesOptions(BasesList_Statuses);
               return BasesList_Statuses;
            when Key_F1 => -- Show help
               Erase;
               ShowGameHeader(Help_Topic);
               ShowHelp(Bases_List, 8);
               return Help_Topic;
            when others =>
               null;
         end case;
      end if;
      return Bases_List;
   end BasesListKeys;

   function BasesOptionsKeys
     (Key: Key_Code;
      CurrentState: GameStates) return GameStates is
      Result: Driver_Result;
   begin
      case Key is
         when KEY_UP => -- Select previous option
            Result := Driver(OptionsMenu, M_Up_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_Last_Item);
            end if;
         when KEY_DOWN => -- Select next option
            Result := Driver(OptionsMenu, M_Down_Item);
            if Result = Request_Denied then
               Result := Driver(OptionsMenu, M_First_Item);
            end if;
         when 10 => -- Select option from menu
            if Name(Current(OptionsMenu)) = "quit" then
               DrawGame(Bases_List);
               return Bases_List;
            end if;
            if CurrentState = BasesList_Types then
               BasesType :=
                 Bases_Types'Val(Get_Index(Current(OptionsMenu)) - 1);
            elsif CurrentState = BasesList_Statuses then
               BasesStatus := Get_Index(Current(OptionsMenu));
            else
               BasesOwner :=
                 Bases_Owners'Val(Get_Index(Current(OptionsMenu)) - 1);
            end if;
            CurrentMenuIndex := 1;
            DrawGame(Bases_List);
            return Bases_List;
         when others =>
            Result := Driver(OptionsMenu, Key);
            if Result = Menu_Ok then
               Refresh(MenuWindow2);
            else
               Result := Driver(OptionsMenu, M_Clear_Pattern);
               Result := Driver(OptionsMenu, Key);
            end if;
      end case;
      if Result = Menu_Ok then
         Refresh(MenuWindow2);
      end if;
      return CurrentState;
   end BasesOptionsKeys;
end BasesList;
