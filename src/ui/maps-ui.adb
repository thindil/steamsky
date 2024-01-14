-- Copyright (c) 2020-2024 Bartek thindil Jasicki
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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm;
with Bases; use Bases;
with Bases.LootUI;
with Bases.RecruitUI;
with Bases.SchoolUI;
with Bases.ShipyardUI;
with Bases.UI;
with BasesTypes; use BasesTypes;
with Config;
with Crafts.UI;
with CoreUI; use CoreUI;
with DebugUI;
with Events; use Events;
with Factions; use Factions;
with GameOptions;
with Help.UI;
with Items;
with Knowledge;
with Log;
with Maps.UI.Commands;
with Messages;
with Messages.UI;
with Missions.UI;
with OrdersMenu;
with Ships.UI;
with Statistics;
with Statistics.UI;
with Stories; use Stories;
with Trades.UI;
with Themes; use Themes;
with Utils.UI; use Utils.UI;
with WaitMenu;

package body Maps.UI is

   procedure Update_Header is
      procedure Update_Ada_Header with
         Import => True,
         Convention => C,
         External_Name => "updateAdaHeader";
   begin
      Set_Ship_In_Nim;
      Update_Ada_Header;
   end Update_Header;

   -- ****iv* MUI/MUI.MapView
   -- FUNCTION
   -- Text widget with the sky map
   -- SOURCE
   Map_View: Tk_Text;
   -- ****

   -- ****if* MUI/MUI.Get_Map_View
   -- FUNCTION
   -- Get the text widget with the sky map
   -- RESULT
   -- Returns text widget with the sky map
   -- SOURCE
   function Get_Map_View return Tk_Text is
      -- ****
   begin
      return Map_View;
   end Get_Map_View;

   procedure Draw_Map is
      use Ada.Strings.UTF_Encoding.Wide_Strings;

      Map_Char: Unbounded_String := Null_Unbounded_String;
      End_X, End_Y: Integer;
      Map_Height, Map_Width: Positive;
      Map_Tag: Unbounded_String := Null_Unbounded_String;
      Story_X, Story_Y: Natural := 1;
      Preview: Boolean :=
        (if
           Tcl_GetVar(interp => Get_Context, varName => "mappreview")'Length >
           0
         then True
         else False);
      Center_X, Center_Y: Positive;
   begin
      Get_Center_Point(X => Center_X, Y => Center_Y);
      if Preview and Player_Ship.Speed /= DOCKED then
         Tcl_UnsetVar(interp => Get_Context, varName => "mappreview");
         Preview := False;
      end if;
      configure(Widgt => Get_Map_View, options => "-state normal");
      Delete
        (TextWidget => Get_Map_View, StartIndex => "1.0", Indexes => "end");
      Map_Height :=
        Positive'Value(cget(Widgt => Get_Map_View, option => "-height"));
      Map_Width :=
        Positive'Value(cget(Widgt => Get_Map_View, option => "-width"));
      --## rule off SIMPLIFIABLE_EXPRESSIONS
      Start_Y := Center_Y - (Map_Height / 2);
      Start_X := Center_X - (Map_Width / 2);
      End_Y := Center_Y + (Map_Height / 2);
      End_X := Center_X + (Map_Width / 2);
      --## rule on SIMPLIFIABLE_EXPRESSIONS
      if Start_Y < 1 then
         Start_Y := 1;
         End_Y := Map_Height;
      end if;
      if Start_X < 1 then
         Start_X := 1;
         End_X := Map_Width;
      end if;
      if End_Y > 1_024 then
         End_Y := 1_024;
         Start_Y := 1_025 - Map_Height;
      end if;
      if End_X > 1_024 then
         End_X := 1_024;
         Start_X := 1_025 - Map_Width;
      end if;
      if Get_Current_Story.Index /= Null_Unbounded_String then
         Get_Story_Location(Story_X => Story_X, Story_Y => Story_Y);
         if Story_X = Player_Ship.Sky_X and Story_Y = Player_Ship.Sky_Y then
            Story_X := 0;
            Story_Y := 0;
         end if;
      end if;
      if Player_Ship.Speed = DOCKED and
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index = 0 then
         Player_Ship.Speed := Ships.FULL_STOP;
      end if;
      Draw_Map_Y_Loop :
      for Y in Start_Y .. End_Y loop
         Draw_Map_X_Loop :
         for X in Start_X .. End_X loop
            Map_Tag := Null_Unbounded_String;
            if X = Player_Ship.Sky_X and Y = Player_Ship.Sky_Y then
               Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "playerShipIcon"));
            else
               Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "emptyMapIcon"));
               Map_Tag :=
                 (if Sky_Map(X, Y).Visited then
                    To_Unbounded_String(Source => "black")
                  else To_Unbounded_String(Source => "unvisited gray"));
               if X = Player_Ship.Destination_X and
                 Y = Player_Ship.Destination_Y then
                  Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "targetIcon"));
                  Map_Tag :=
                    (if Sky_Map(X, Y).Visited then Null_Unbounded_String
                     else To_Unbounded_String(Source => "unvisited"));
               elsif Get_Current_Story.Index /= Null_Unbounded_String
                 and then (X = Story_X and Y = Story_Y) then
                  Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "storyIcon"));
                  Map_Tag := To_Unbounded_String(Source => "green");
               elsif Sky_Map(X, Y).Mission_Index > 0 then
                  case Get_Accepted_Mission
                    (Mission_Index => Sky_Map(X, Y).Mission_Index)
                    .M_Type is
                     when DELIVER =>
                        Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "deliverIcon"));
                        Map_Tag := To_Unbounded_String(Source => "yellow");
                     when DESTROY =>
                        Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "destroyIcon"));
                        Map_Tag := To_Unbounded_String(Source => "red");
                     when PATROL =>
                        Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "patrolIcon"));
                        Map_Tag := To_Unbounded_String(Source => "lime");
                     when EXPLORE =>
                        Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "exploreIcon"));
                        Map_Tag := To_Unbounded_String(Source => "green");
                     when PASSENGER =>
                        Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "passengerIcon"));
                        Map_Tag := To_Unbounded_String(Source => "cyan");
                  end case;
                  if not Sky_Map(X, Y).Visited then
                     Append(Source => Map_Tag, New_Item => " unvisited");
                  end if;
               elsif Sky_Map(X, Y).Event_Index > 0 then
                  if Sky_Map(X, Y).Event_Index > Get_Events_Amount then
                     Sky_Map(X, Y).Event_Index := 0;
                  else
                     case Get_Event(Index => Sky_Map(X, Y).Event_Index)
                       .E_Type is
                        when ENEMYSHIP =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "enemyShipIcon"));
                           Map_Tag := To_Unbounded_String(Source => "red");
                        when ATTACKONBASE =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "attackOnBaseIcon"));
                           Map_Tag := To_Unbounded_String(Source => "red2");
                        when ENEMYPATROL =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "enemyPatrolIcon"));
                           Map_Tag := To_Unbounded_String(Source => "red3");
                        when DISEASE =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "diseaseIcon"));
                           Map_Tag := To_Unbounded_String(Source => "yellow");
                        when FULLDOCKS =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "fullDocksIcon"));
                           Map_Tag := To_Unbounded_String(Source => "cyan");
                        when DOUBLEPRICE =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "priceIcon"));
                           Map_Tag := To_Unbounded_String(Source => "lime");
                        when TRADER =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "traderIcon"));
                           Map_Tag := To_Unbounded_String(Source => "green");
                        when FRIENDLYSHIP =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "friendlyShipIcon"));
                           Map_Tag := To_Unbounded_String(Source => "green2");
                        when others =>
                           null;
                     end case;
                  end if;
                  if not Sky_Map(X, Y).Visited then
                     Append(Source => Map_Tag, New_Item => " unvisited");
                  end if;
               elsif Sky_Map(X, Y).Base_Index > 0 then
                  Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "notVisitedBaseIcon"));
                  if Sky_Bases(Sky_Map(X, Y).Base_Index).Known then
                     if Sky_Bases(Sky_Map(X, Y).Base_Index).Visited.Year >
                       0 then
                        Map_Char := To_Unbounded_String(Source => Encode(Item => "" & Get_Faction
                            (Index =>
                               Sky_Bases(Sky_Map(X, Y).Base_Index).Owner)
                            .Base_Icon));
                        Map_Tag :=
                          To_Unbounded_String
                            (Source =>
                               Tiny_String.To_String
                                 (Source =>
                                    Sky_Bases(Sky_Map(X, Y).Base_Index)
                                      .Base_Type));
                     else
                        Map_Tag := To_Unbounded_String(Source => "unvisited");
                     end if;
                  else
                     Map_Tag :=
                       To_Unbounded_String(Source => "unvisited gray");
                  end if;
               end if;
            end if;
            if Preview then
               Preview_Mission_Loop :
               for Mission of Sky_Bases
                 (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index)
                 .Missions loop
                  if Mission.Target_X = X and Mission.Target_Y = Y then
                     case Mission.M_Type is
                        when DELIVER =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "deliverIcon"));
                           Map_Tag := To_Unbounded_String(Source => "yellow");
                        when DESTROY =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "destroyIcon"));
                           Map_Tag := To_Unbounded_String(Source => "red");
                        when PATROL =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "patrolIcon"));
                           Map_Tag := To_Unbounded_String(Source => "lime");
                        when EXPLORE =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "exploreIcon"));
                           Map_Tag := To_Unbounded_String(Source => "green");
                        when PASSENGER =>
                           Map_Char := To_Unbounded_String(Source => Get_Icon(Name => "passengerIcon"));
                           Map_Tag := To_Unbounded_String(Source => "cyan");
                     end case;
                     if not Sky_Map(X, Y).Visited then
                        Append(Source => Map_Tag, New_Item => " unvisited");
                     end if;
                     exit Preview_Mission_Loop;
                  end if;
               end loop Preview_Mission_Loop;
            end if;
            Insert
              (TextWidget => Get_Map_View, Index => "end",
               Text =>
                 To_String(Source => Map_Char) & " [list " &
                 To_String(Source => Map_Tag) & "]");
         end loop Draw_Map_X_Loop;
         if Y < End_Y then
            Insert
              (TextWidget => Get_Map_View, Index => "end",
               Text => "{" & LF & "}");
         end if;
      end loop Draw_Map_Y_Loop;
      configure(Widgt => Get_Map_View, options => "-state disable");
   end Draw_Map;

   procedure Update_Map_Info
     (X: Positive := Player_Ship.Sky_X; Y: Positive := Player_Ship.Sky_Y) is
      use Items;
      use Tiny_String;

      Map_Info_Text, Event_Info_Text, Color: Unbounded_String :=
        Null_Unbounded_String;
      Map_Info: constant Tk_Text :=
        Get_Widget(pathName => Main_Paned & ".mapframe.info");
      Width: Positive := 1;
      procedure Insert_Text
        (New_Text: String;
         Tag_Name: Unbounded_String := Null_Unbounded_String) is
      begin
         if New_Text'Length > Width then
            Width := New_Text'Length;
         end if;
         if Width > 21 then
            Width := 21;
         end if;
         Insert
           (TextWidget => Map_Info, Index => "end",
            Text =>
              "{" & New_Text & "}" &
              (if Length(Source => Tag_Name) = 0 then ""
               else " [list " & To_String(Source => Tag_Name) & "]"));
      end Insert_Text;
   begin
      configure(Widgt => Map_Info, options => "-state normal");
      Delete(TextWidget => Map_Info, StartIndex => "1.0", Indexes => "end");
      Insert_Text(New_Text => "X:");
      Insert_Text
        (New_Text => Positive'Image(X),
         Tag_Name => To_Unbounded_String(Source => "yellow2"));
      Insert_Text(New_Text => " Y:");
      Insert_Text
        (New_Text => Positive'Image(Y),
         Tag_Name => To_Unbounded_String(Source => "yellow2"));
      if Player_Ship.Sky_X /= X or Player_Ship.Sky_Y /= Y then
         Add_Distance_Info_Block :
         declare
            Distance: constant Positive :=
              Count_Distance(Destination_X => X, Destination_Y => Y);
            Distance_Text: Unbounded_String := Null_Unbounded_String;
            Travel_Values: constant Travel_Array :=
              Travel_Info(Distance => Distance);
         begin
            Insert_Text(New_Text => LF & "Distance:");
            Insert_Text
              (New_Text => Positive'Image(Distance),
               Tag_Name => To_Unbounded_String(Source => "yellow2"));
            if Travel_Values(1) > 0 then
               Insert_Text(New_Text => LF & "ETA:");
               Minutes_To_Date
                 (Minutes => Travel_Values(1), Info_Text => Distance_Text);
               Insert_Text
                 (New_Text => To_String(Source => Distance_Text),
                  Tag_Name => To_Unbounded_String(Source => "yellow2"));
               Insert_Text(New_Text => LF & "Approx fuel usage:");
               Insert_Text
                 (New_Text => Positive'Image(Travel_Values(2)),
                  Tag_Name => To_Unbounded_String(Source => "yellow2"));
            end if;
         end Add_Distance_Info_Block;
      end if;
      if Sky_Map(X, Y).Base_Index > 0 then
         Add_Base_Info_Block :
         declare
            use Ada.Characters.Handling;

            Base_Index: constant Bases_Range := Sky_Map(X, Y).Base_Index;
            Base_Info_Text: Unbounded_String := Null_Unbounded_String;
         begin
            if Sky_Bases(Base_Index).Known then
               Insert_Text
                 (New_Text => LF & "Base info:",
                  Tag_Name => To_Unbounded_String(Source => "pink underline"));
               Insert_Text(New_Text => LF & "Name: ");
               Insert_Text
                 (New_Text =>
                    Tiny_String.To_String
                      (Source => Sky_Bases(Base_Index).Name),
                  Tag_Name => To_Unbounded_String(Source => "yellow2"));
            end if;
            if Sky_Bases(Base_Index).Visited.Year > 0 then
               Tag_Configure
                 (TextWidget => Map_Info, TagName => "basetype",
                  Options =>
                    "-foreground #" &
                    Get_Base_Type_Color
                      (Base_Type => Sky_Bases(Base_Index).Base_Type));
               Insert_Text(New_Text => LF & "Type: ");
               Insert_Text
                 (New_Text =>
                    Get_Base_Type_Name
                      (Base_Type => Sky_Bases(Base_Index).Base_Type),
                  Tag_Name => To_Unbounded_String(Source => "basetype"));
               if Sky_Bases(Base_Index).Population > 0 then
                  Insert_Text(New_Text => LF & "Population: ");
               end if;
               if Sky_Bases(Base_Index).Population > 0 and
                 Sky_Bases(Base_Index).Population < 150 then
                  Insert_Text
                    (New_Text => "small",
                     Tag_Name => To_Unbounded_String(Source => "yellow2"));
               elsif Sky_Bases(Base_Index).Population > 149 and
                 Sky_Bases(Base_Index).Population < 300 then
                  Insert_Text
                    (New_Text => "medium",
                     Tag_Name => To_Unbounded_String(Source => "yellow2"));
               elsif Sky_Bases(Base_Index).Population > 299 then
                  Insert_Text
                    (New_Text => "large",
                     Tag_Name => To_Unbounded_String(Source => "yellow2"));
               end if;
               Insert_Text(New_Text => LF & "Size: ");
               Insert_Text
                 (New_Text =>
                    To_Lower
                      (Item => Bases_Size'Image(Sky_Bases(Base_Index).Size)) &
                    LF,
                  Tag_Name => To_Unbounded_String(Source => "yellow2"));
               if Sky_Bases(Base_Index).Population > 0 then
                  Insert_Text(New_Text => "Owner: ");
                  Insert_Text
                    (New_Text =>
                       Tiny_String.To_String
                         (Source =>
                            Get_Faction(Index => Sky_Bases(Base_Index).Owner)
                              .Name),
                     Tag_Name => To_Unbounded_String(Source => "yellow2"));
               else
                  Insert_Text(New_Text => "Base is abandoned");
               end if;
               if Sky_Bases(Base_Index).Population > 0 then
                  Base_Info_Text := To_Unbounded_String(Source => "" & LF);
                  case Sky_Bases(Base_Index).Reputation.Level is
                     when -100 .. -75 =>
                        Append
                          (Source => Base_Info_Text,
                           New_Item => "You are hated here");
                        Color := To_Unbounded_String(Source => "red");
                     when -74 .. -50 =>
                        Append
                          (Source => Base_Info_Text,
                           New_Item => "You are outlawed here");
                        Color := To_Unbounded_String(Source => "red");
                     when -49 .. -25 =>
                        Append
                          (Source => Base_Info_Text,
                           New_Item => "You are disliked here");
                        Color := To_Unbounded_String(Source => "red");
                     when -24 .. -1 =>
                        Append
                          (Source => Base_Info_Text,
                           New_Item => "They are unfriendly to you");
                        Color := To_Unbounded_String(Source => "red");
                     when 0 =>
                        Append
                          (Source => Base_Info_Text,
                           New_Item => "You are unknown here");
                        Color := Null_Unbounded_String;
                     when 1 .. 25 =>
                        Append
                          (Source => Base_Info_Text,
                           New_Item => "You are know here as visitor");
                        Color := To_Unbounded_String(Source => "green");
                     when 26 .. 50 =>
                        Append
                          (Source => Base_Info_Text,
                           New_Item => "You are know here as trader");
                        Color := To_Unbounded_String(Source => "green");
                     when 51 .. 75 =>
                        Append
                          (Source => Base_Info_Text,
                           New_Item => "You are know here as friend");
                        Color := To_Unbounded_String(Source => "green");
                     when 76 .. 100 =>
                        Append
                          (Source => Base_Info_Text,
                           New_Item => "You are well known here");
                        Color := To_Unbounded_String(Source => "green");
                  end case;
                  Insert_Text
                    (New_Text => To_String(Source => Base_Info_Text),
                     Tag_Name => Color);
               end if;
               if Base_Index = Player_Ship.Home_Base then
                  Insert_Text
                    (New_Text => LF & "It is your home base",
                     Tag_Name => To_Unbounded_String(Source => "cyan"));
               end if;
            end if;
         end Add_Base_Info_Block;
      end if;
      if Sky_Map(X, Y).Mission_Index > 0 then
         Add_Mission_Info_Block :
         declare
            Mission_Index: constant Mission_Container.Extended_Index :=
              Sky_Map(X, Y).Mission_Index;
            Mission_Info_Text: Unbounded_String;
         begin
            Mission_Info_Text := To_Unbounded_String(Source => "" & LF);
            if Sky_Map(X, Y).Base_Index > 0 or
              Sky_Map(X, Y).Event_Index > 0 then
               Append(Source => Map_Info_Text, New_Item => LF);
            end if;
            case Get_Accepted_Mission(Mission_Index => Mission_Index).M_Type is
               when DELIVER =>
                  Append
                    (Source => Mission_Info_Text,
                     New_Item =>
                       "Deliver " &
                       To_String
                         (Source =>
                            Get_Proto_Item
                              (Index =>
                                 Get_Accepted_Mission
                                   (Mission_Index => Mission_Index)
                                   .Item_Index)
                              .Name));
               when DESTROY =>
                  Append
                    (Source => Mission_Info_Text,
                     New_Item =>
                       "Destroy " &
                       To_String
                         (Source =>
                            Get_Proto_Ship
                              (Proto_Index =>
                                 Get_Accepted_Mission
                                   (Mission_Index => Mission_Index)
                                   .Ship_Index)
                              .Name));
               when PATROL =>
                  Append
                    (Source => Mission_Info_Text, New_Item => "Patrol area");
               when EXPLORE =>
                  Append
                    (Source => Mission_Info_Text, New_Item => "Explore area");
               when PASSENGER =>
                  Append
                    (Source => Mission_Info_Text,
                     New_Item => "Transport passenger");
            end case;
            Insert_Text(New_Text => To_String(Source => Mission_Info_Text));
         end Add_Mission_Info_Block;
      end if;
      if Get_Current_Story.Index /= Null_Unbounded_String then
         Add_Story_Info_Block :
         declare
            --## rule off IMPROPER_INITIALIZATION
            Story_X, Story_Y: Natural := 1;
            --## rule on IMPROPER_INITIALIZATION
            Finish_Condition: Step_Condition_Type := ANY;
         begin
            Get_Story_Location(Story_X => Story_X, Story_Y => Story_Y);
            if Story_X = Player_Ship.Sky_X and Story_Y = Player_Ship.Sky_Y then
               Story_X := 0;
               Story_Y := 0;
            end if;
            if X = Story_X and Y = Story_Y then
               Finish_Condition :=
                 (if Get_Current_Story.Current_Step = 0 then
                    Get_Story(Index => Get_Current_Story.Index).Starting_Step
                      .Finish_Condition
                  elsif Get_Current_Story.Current_Step > 0 then
                    Get_Story(Index => Get_Current_Story.Index).Steps
                      (Get_Current_Story.Current_Step)
                      .Finish_Condition
                  else Get_Story(Index => Get_Current_Story.Index).Final_Step
                      .Finish_Condition);
               if Finish_Condition in ASKINBASE | DESTROYSHIP | EXPLORE then
                  Insert_Text(New_Text => LF & "Story leads you here");
               end if;
            end if;
         end Add_Story_Info_Block;
      end if;
      if X = Player_Ship.Sky_X and Y = Player_Ship.Sky_Y then
         Insert_Text
           (New_Text => LF & "You are here",
            Tag_Name => To_Unbounded_String(Source => "yellow"));
      end if;
      if Sky_Map(X, Y).Event_Index > 0 then
         Add_Event_Info_Block :
         declare
            Event_Index: constant Natural := Sky_Map(X, Y).Event_Index;
         begin
            if Get_Event(Index => Event_Index).E_Type not in BASERECOVERY |
                  NONE then
               Event_Info_Text := To_Unbounded_String(Source => LF & LF);
            end if;
            case Get_Event(Index => Event_Index).E_Type is
               when TRADER =>
                  Append
                    (Source => Event_Info_Text,
                     New_Item =>
                       To_String
                         (Source =>
                            Get_Proto_Ship
                              (Proto_Index =>
                                 Get_Event(Index => Event_Index).Ship_Index)
                              .Name));
                  Color := To_Unbounded_String(Source => "green");
               when FRIENDLYSHIP =>
                  Append
                    (Source => Event_Info_Text,
                     New_Item =>
                       To_String
                         (Source =>
                            Get_Proto_Ship
                              (Proto_Index =>
                                 Get_Event(Index => Event_Index).Ship_Index)
                              .Name));
                  Color := To_Unbounded_String(Source => "green2");
               when ENEMYSHIP =>
                  Append
                    (Source => Event_Info_Text,
                     New_Item =>
                       To_String
                         (Source =>
                            Get_Proto_Ship
                              (Proto_Index =>
                                 Get_Event(Index => Event_Index).Ship_Index)
                              .Name));
                  Color := To_Unbounded_String(Source => "red");
               when FULLDOCKS =>
                  Append
                    (Source => Event_Info_Text,
                     New_Item => "Full docks in base");
                  Color := To_Unbounded_String(Source => "cyan");
               when ATTACKONBASE =>
                  Append
                    (Source => Event_Info_Text,
                     New_Item => "Base is under attack");
                  Color := To_Unbounded_String(Source => "red");
               when DISEASE =>
                  Append
                    (Source => Event_Info_Text, New_Item => "Disease in base");
                  Color := To_Unbounded_String(Source => "yellow");
               when ENEMYPATROL =>
                  Append
                    (Source => Event_Info_Text, New_Item => "Enemy patrol");
                  Color := To_Unbounded_String(Source => "red3");
               when DOUBLEPRICE =>
                  Append
                    (Source => Event_Info_Text,
                     New_Item =>
                       "Double price for " &
                       To_String
                         (Source =>
                            Get_Proto_Item
                              (Index =>
                                 Get_Event(Index => Event_Index).Item_Index)
                              .Name));
                  Color := To_Unbounded_String(Source => "lime");
               when NONE | BASERECOVERY =>
                  null;
            end case;
            Insert_Text
              (New_Text => To_String(Source => Event_Info_Text),
               Tag_Name => Color);
         end Add_Event_Info_Block;
      end if;
      configure
        (Widgt => Map_Info,
         options =>
           "-state disabled -width" & Positive'Image(Width) & " -height " &
           Text.Count
             (TextWidget => Map_Info, Options => "-displaylines",
              Index1 => "0.0", Index2 => "end"));
   end Update_Map_Info;

   procedure Update_Move_Buttons is
      procedure Update_Ada_Move_Buttons with
         Import => True,
         Convention => C,
         External_Name => "updateAdaMoveButtons";
   begin
      Set_Ship_In_Nim;
      Update_Ada_Move_Buttons;
   end Update_Move_Buttons;

   procedure Create_Game_Ui is
      use Ada.Strings.Fixed;
      use GNAT.Directory_Operations;
      use Tcl.Tk.Ada.Widgets.TtkButton;
      use Tcl.Tk.Ada.Widgets.TtkFrame;
      use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
      use Tcl.Tk.Ada.Winfo;
      use Tcl.Tk.Ada.Wm;
      use Config;
      use DebugUI;
      use Log;
      use Tiny_String;

      Game_Frame: constant Ttk_Frame := Get_Widget(pathName => ".gameframe");
      Paned: constant Ttk_PanedWindow :=
        Get_Widget(pathName => Game_Frame & ".paned");
      Button: constant Ttk_Button :=
        Get_Widget(pathName => Paned & ".mapframe.buttons.hide");
      Steam_Sky_Map_Error: exception;
      Header: constant Ttk_Frame :=
        Get_Widget(pathName => Game_Frame & ".header");
      Messages_Frame: constant Ttk_Frame :=
        Get_Widget(pathName => Paned & ".controls.messages");
      Paned_Position: Natural := 0;
      New_Start: Boolean := False;
   begin
      Map_View := Get_Widget(pathName => Paned & ".mapframe.map");
      if Winfo_Get(Widgt => Get_Map_View, Info => "exists") = "0" then
         New_Start := True;
         Load_Keys_Block :
         declare
            use Ada.Text_IO;

            Keys_File: File_Type;
            Raw_Data, Field_Name, Value: Unbounded_String :=
              Null_Unbounded_String;
            Equal_Index: Natural := 0;
         begin
            Open
              (File => Keys_File, Mode => In_File,
               Name => To_String(Source => Save_Directory) & "keys.cfg");
            Load_Accelerators_Loop :
            while not End_Of_File(File => Keys_File) loop
               Raw_Data :=
                 To_Unbounded_String(Source => Get_Line(File => Keys_File));
               if Length(Source => Raw_Data) = 0 then
                  goto End_Of_Loop;
               end if;
               Equal_Index := Index(Source => Raw_Data, Pattern => "=");
               Field_Name :=
                 Head(Source => Raw_Data, Count => Equal_Index - 2);
               Value :=
                 Tail
                   (Source => Raw_Data,
                    Count => Length(Source => Raw_Data) - Equal_Index - 1);
               if Field_Name = To_Unbounded_String(Source => "ShipInfo") then
                  Set_Menu_Accelerator
                    (Index => 1, Value => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "Orders") then
                  Set_Menu_Accelerator
                    (Index => 2, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "Crafting") then
                  Set_Menu_Accelerator
                    (Index => 3, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "LastMessages") then
                  Set_Menu_Accelerator
                    (Index => 4, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "Knowledge") then
                  Set_Menu_Accelerator
                    (Index => 5, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "WaitOrders") then
                  Set_Menu_Accelerator
                    (Index => 6, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "GameStats") then
                  Set_Menu_Accelerator
                    (Index => 7, Value => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "Help") then
                  Set_Menu_Accelerator
                    (Index => 8, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "GameOptions") then
                  Set_Menu_Accelerator
                    (Index => 9, Value => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "Quit") then
                  Set_Menu_Accelerator
                    (Index => 10, Value => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "Resign") then
                  Set_Menu_Accelerator
                    (Index => 11, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "GameMenu") then
                  Set_Map_Accelerator
                    (Index => 1, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MapOptions") then
                  Set_Map_Accelerator
                    (Index => 2, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ZoomInMap") then
                  Set_Map_Accelerator
                    (Index => 3, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ZoomOutMap") then
                  Set_Map_Accelerator
                    (Index => 4, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveUpLeft") then
                  Set_Map_Accelerator
                    (Index => 5, Value => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "MoveUp") then
                  Set_Map_Accelerator
                    (Index => 6, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveUpRight") then
                  Set_Map_Accelerator
                    (Index => 7, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveLeft") then
                  Set_Map_Accelerator
                    (Index => 8, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "WaitInPlace") then
                  Set_Map_Accelerator
                    (Index => 10, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveRight") then
                  Set_Map_Accelerator
                    (Index => 9, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveDownLeft") then
                  Set_Map_Accelerator
                    (Index => 11, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveDown") then
                  Set_Map_Accelerator
                    (Index => 12, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveDownRight") then
                  Set_Map_Accelerator
                    (Index => 13, Value => To_String(Source => Value));
               elsif Field_Name = To_Unbounded_String(Source => "MoveTo") then
                  Set_Map_Accelerator
                    (Index => 14, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "CenterMap") then
                  Set_Map_Accelerator
                    (Index => 15, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "CenterMapOnHomeBase") then
                  Set_Map_Accelerator
                    (Index => 16, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapUpLeft") then
                  Set_Map_Accelerator
                    (Index => 17, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapUp") then
                  Set_Map_Accelerator
                    (Index => 18, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapUpRight") then
                  Set_Map_Accelerator
                    (Index => 19, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapLeft") then
                  Set_Map_Accelerator
                    (Index => 20, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapRight") then
                  Set_Map_Accelerator
                    (Index => 21, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapDownLeft") then
                  Set_Map_Accelerator
                    (Index => 22, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapDown") then
                  Set_Map_Accelerator
                    (Index => 23, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapDownRight") then
                  Set_Map_Accelerator
                    (Index => 24, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorUpLeft") then
                  Set_Map_Accelerator
                    (Index => 25, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorUp") then
                  Set_Map_Accelerator
                    (Index => 26, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorUpRight") then
                  Set_Map_Accelerator
                    (Index => 27, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorLeft") then
                  Set_Map_Accelerator
                    (Index => 28, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorRight") then
                  Set_Map_Accelerator
                    (Index => 29, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorDownLeft") then
                  Set_Map_Accelerator
                    (Index => 30, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorDown") then
                  Set_Map_Accelerator
                    (Index => 31, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorDownRight") then
                  Set_Map_Accelerator
                    (Index => 32, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "LeftClickMouse") then
                  Set_Map_Accelerator
                    (Index => 33, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "FullStop") then
                  Set_Map_Accelerator
                    (Index => 34, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "QuarterSpeed") then
                  Set_Map_Accelerator
                    (Index => 35, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "HalfSpeed") then
                  Set_Map_Accelerator
                    (Index => 36, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "FullSpeed") then
                  Set_Map_Accelerator
                    (Index => 37, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "FullScreen") then
                  Set_Full_Screen_Accel(Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ResizeFirst") then
                  Set_General_Accelerator
                    (Index => 1, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ResizeSecond") then
                  Set_General_Accelerator
                    (Index => 2, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ResizeThird") then
                  Set_General_Accelerator
                    (Index => 3, Value => To_String(Source => Value));
               elsif Field_Name =
                 To_Unbounded_String(Source => "ResizeForth") then
                  Set_General_Accelerator
                    (Index => 4, Value => To_String(Source => Value));
               end if;
               <<End_Of_Loop>>
            end loop Load_Accelerators_Loop;
            Close(File => Keys_File);
         exception
            when others =>
               if Dir_Separator = '\' then
                  Set_Map_Accelerator(Index => 5, Value => "Home");
                  Set_Map_Accelerator(Index => 6, Value => "Up");
                  Set_Map_Accelerator(Index => 7, Value => "Prior");
                  Set_Map_Accelerator(Index => 8, Value => "Left");
                  Set_Map_Accelerator(Index => 9, Value => "Clear");
                  Set_Map_Accelerator(Index => 10, Value => "Right");
                  Set_Map_Accelerator(Index => 11, Value => "End");
                  Set_Map_Accelerator(Index => 12, Value => "Down");
                  Set_Map_Accelerator(Index => 13, Value => "Next");
                  Set_Map_Accelerator(Index => 14, Value => "slash");
                  Set_Map_Accelerator(Index => 17, Value => "Shift-Home");
                  Set_Map_Accelerator(Index => 18, Value => "Shift-Up");
                  Set_Map_Accelerator(Index => 19, Value => "Shift-Prior");
                  Set_Map_Accelerator(Index => 20, Value => "Shift-Left");
                  Set_Map_Accelerator(Index => 21, Value => "Shift-Right");
                  Set_Map_Accelerator(Index => 22, Value => "Shift-End");
                  Set_Map_Accelerator(Index => 23, Value => "Shift-Down");
                  Set_Map_Accelerator(Index => 24, Value => "Shift-Next");
                  Set_Map_Accelerator(Index => 25, Value => "Control-Home");
                  Set_Map_Accelerator(Index => 26, Value => "Control-Up");
                  Set_Map_Accelerator(Index => 27, Value => "Control-Prior");
                  Set_Map_Accelerator(Index => 28, Value => "Control-Left");
                  Set_Map_Accelerator(Index => 29, Value => "Control-Right");
                  Set_Map_Accelerator(Index => 30, Value => "Control-End");
                  Set_Map_Accelerator(Index => 31, Value => "Control-Down");
                  Set_Map_Accelerator(Index => 32, Value => "Control-Next");
               end if;
         end Load_Keys_Block;
         Maps.UI.Commands.Add_Commands;
         Tcl_EvalFile
           (interp => Get_Context,
            fileName =>
              To_String(Source => Data_Directory) & "ui" & Dir_Separator &
              "game.tcl");
         Main_Paned := Paned;
         Game_Header := Header;
         Close_Button := Get_Widget(pathName => Game_Header & ".closebutton");
         Set_Theme;
         OrdersMenu.Add_Commands;
         WaitMenu.Add_Commands;
         Help.UI.Add_Commands;
         Ships.UI.Add_Commands;
         Crafts.UI.Add_Commands;
         Messages.UI.Add_Commands;
         GameOptions.Add_Commands;
         Trades.UI.Add_Commands;
         SchoolUI.Add_Commands;
         RecruitUI.Add_Commands;
         Bases.UI.Add_Commands;
         ShipyardUI.Add_Commands;
         LootUI.Add_Commands;
         Knowledge.Add_Commands;
         Missions.UI.Add_Commands;
         Statistics.UI.Add_Commands;
         Bind
           (Widgt => Messages_Frame, Sequence => "<Configure>",
            Script => "ResizeLastMessages");
         Bind
           (Widgt => Get_Map_View, Sequence => "<Configure>",
            Script => "DrawMap");
         Bind
           (Widgt => Get_Map_View, Sequence => "<Motion>",
            Script => "{UpdateMapInfo %x %y}");
         Bind
           (Widgt => Get_Map_View,
            Sequence =>
              "<Button-" &
              (if Get_Boolean_Setting(Name => "rightButton") then "3"
               else "1") &
              ">",
            Script => "{ShowDestinationMenu %X %Y}");
         Bind
           (Widgt => Get_Map_View, Sequence => "<MouseWheel>",
            Script => "{if {%D > 0} {ZoomMap raise} else {ZoomMap lower}}");
         Bind
           (Widgt => Get_Map_View, Sequence => "<Button-4>",
            Script => "{ZoomMap raise}");
         Bind
           (Widgt => Get_Map_View, Sequence => "<Button-5>",
            Script => "{ZoomMap lower}");
         Set_Keys;
         if Log.Debug_Mode = Log.MENU then
            Show_Debug_Ui;
         end if;
      else
         Tcl.Tk.Ada.Pack.Pack
           (Slave => Game_Frame, Options => "-fill both -expand true");
      end if;
      Tcl_SetVar
        (interp => Get_Context, varName => "refreshmap", newValue => "1");
      Wm_Set
        (Widgt => Get_Main_Window(Interp => Get_Context), Action => "title",
         Options => "{Steam Sky}");
      if Get_Boolean_Setting(Name => "fullScreen") then
         Wm_Set
           (Widgt => Get_Main_Window(Interp => Get_Context),
            Action => "attributes", Options => "-fullscreen 1");
      end if;
      Set_Accelerators_Loop :
      for Accelerator in 1 .. 11 loop
         Bind_To_Main_Window
           (Interp => Get_Context,
            Sequence =>
              "<" &
              To_String
                (Source =>
                   Insert
                     (Source =>
                        To_Unbounded_String
                          (Source =>
                             Get_Menu_Accelerator(Index => Accelerator)),
                      Before =>
                        Index
                          (Source =>
                             To_Unbounded_String
                               (Source =>
                                  Get_Menu_Accelerator(Index => Accelerator)),
                           Pattern => "-", Going => Backward) +
                        1,
                      New_Item => "KeyPress-")) &
              ">",
            Script =>
              "{InvokeMenu " & Get_Menu_Accelerator(Index => Accelerator) &
              "}");
      end loop Set_Accelerators_Loop;
      if Index
          (Source =>
             Tcl.Tk.Ada.Grid.Grid_Slaves
               (Master => Get_Main_Window(Interp => Get_Context)),
           Pattern => ".gameframe.header") =
        0 then
         Tcl.Tk.Ada.Grid.Grid(Slave => Header);
      end if;
      Update_Header;
      Set_Center_Point(X => Player_Ship.Sky_X, Y => Player_Ship.Sky_Y);
      Set_Tags_Loop :
      for Base_Type of Bases_Types loop
         exit Set_Tags_Loop when Length(Source => Base_Type) = 0;
         Tag_Configure
           (TextWidget => Get_Map_View,
            TagName => To_String(Source => Base_Type),
            Options =>
              "-foreground #" & Get_Base_Type_Color(Base_Type => Base_Type));
      end loop Set_Tags_Loop;
      Paned_Position :=
        (if
           Get_Integer_Setting(Name => "windowHeight") -
           Get_Integer_Setting(Name => "messagesPosition") <
           0
         then Get_Integer_Setting(Name => "windowHeight")
         else Get_Integer_Setting(Name => "windowHeight") -
           Get_Integer_Setting(Name => "messagesPosition"));
      SashPos
        (Paned => Paned, Index => "0",
         NewPos => Natural'Image(Paned_Position));
      if Index
          (Source =>
             Tcl.Tk.Ada.Grid.Grid_Slaves
               (Master => Get_Main_Window(Interp => Get_Context)),
           Pattern => ".gameframe.paned") =
        0 then
         Tcl.Tk.Ada.Grid.Grid(Slave => Paned);
      end if;
      if Invoke(Buttn => Button) /= "" then
         raise Steam_Sky_Map_Error with "Can't hide map buttons";
      end if;
      Bind_To_Main_Window
        (Interp => Get_Context, Sequence => "<Escape>",
         Script => "{InvokeButton " & Close_Button & "}");
      Update_Messages;
      if not New_Start then
         Tcl_Eval(interp => Get_Context, strng => "DrawMap");
      end if;
      Update_Move_Buttons;
      Update_Map_Info;
      if not Get_Boolean_Setting(Name => "showLastMessages") then
         Tcl.Tk.Ada.Grid.Grid_Remove(Slave => Messages_Frame);
      end if;
      Tcl_SetVar
        (interp => Get_Context, varName => "shipname",
         newValue => To_String(Source => Player_Ship.Name));
      Tcl_SetVar
        (interp => Get_Context, varName => "gamestate", newValue => "general");
   end Create_Game_Ui;

   procedure Show_Sky_Map(Clear: Boolean := False) is
      procedure Show_Ada_Sky_Map(Cle: Integer) with
         Import => True,
         Convention => C,
         External_Name => "showAdaSkyMap";
   begin
      Show_Ada_Sky_Map(Cle => (if Clear then 1 else 0));
   end Show_Sky_Map;

   procedure Set_Keys is
      Tcl_Commands_Array: constant array(1 .. 37) of Unbounded_String :=
        (1 =>
           To_Unbounded_String
             (Source =>
                "{if {[winfo class [focus]] != {TEntry} && [tk busy status " &
                Game_Header & "] == 0} {ShowGameMenu}}"),
         2 =>
           To_Unbounded_String
             (Source => "{" & Main_Paned & ".mapframe.buttons.wait invoke}"),
         3 => To_Unbounded_String(Source => "{ZoomMap raise}"),
         4 => To_Unbounded_String(Source => "{ZoomMap lower}"),
         5 => To_Unbounded_String(Source => "{InvokeButton $bframe.nw}"),
         6 => To_Unbounded_String(Source => "{InvokeButton $bframe.n}"),
         7 => To_Unbounded_String(Source => "{InvokeButton $bframe.ne}"),
         8 => To_Unbounded_String(Source => "{InvokeButton $bframe.w}"),
         9 => To_Unbounded_String(Source => "{InvokeButton $bframe.wait}"),
         10 => To_Unbounded_String(Source => "{InvokeButton $bframe.e}"),
         11 => To_Unbounded_String(Source => "{InvokeButton $bframe.sw}"),
         12 => To_Unbounded_String(Source => "{InvokeButton $bframe.s}"),
         13 => To_Unbounded_String(Source => "{InvokeButton $bframe.se}"),
         14 =>
           To_Unbounded_String(Source => "{InvokeButton $bframe.box.moveto}"),
         15 => To_Unbounded_String(Source => "{MoveMap centeronship}"),
         16 => To_Unbounded_String(Source => "{MoveMap centeronhome}"),
         17 => To_Unbounded_String(Source => "{MoveMap nw}"),
         18 => To_Unbounded_String(Source => "{MoveMap n}"),
         19 => To_Unbounded_String(Source => "{MoveMap ne}"),
         20 => To_Unbounded_String(Source => "{MoveMap w}"),
         21 => To_Unbounded_String(Source => "{MoveMap e}"),
         22 => To_Unbounded_String(Source => "{MoveMap sw}"),
         23 => To_Unbounded_String(Source => "{MoveMap s}"),
         24 => To_Unbounded_String(Source => "{MoveMap se}"),
         25 => To_Unbounded_String(Source => "{MoveCursor nw %x %y}"),
         26 => To_Unbounded_String(Source => "{MoveCursor n %x %y}"),
         27 => To_Unbounded_String(Source => "{MoveCursor ne %x %y}"),
         28 => To_Unbounded_String(Source => "{MoveCursor w %x %y}"),
         29 => To_Unbounded_String(Source => "{MoveCursor e %x %y}"),
         30 => To_Unbounded_String(Source => "{MoveCursor sw %x %y}"),
         31 => To_Unbounded_String(Source => "{MoveCursor s %x %y}"),
         32 => To_Unbounded_String(Source => "{MoveCursor se %x %y}"),
         33 => To_Unbounded_String(Source => "{MoveCursor click %x %y}"),
         34 =>
           To_Unbounded_String
             (Source =>
                "{" & Main_Paned & ".controls.buttons.box.speed current 0}"),
         35 =>
           To_Unbounded_String
             (Source =>
                "{" & Main_Paned & ".controls.buttons.box.speed current 1}"),
         36 =>
           To_Unbounded_String
             (Source =>
                "{" & Main_Paned & ".controls.buttons.box.speed current 2}"),
         37 =>
           To_Unbounded_String
             (Source =>
                "{" & Main_Paned & ".controls.buttons.box.speed current 3}"));
   begin
      Bind_Commands_Loop :
      for I in Tcl_Commands_Array'Range loop
         Bind_To_Main_Window
           (Interp => Get_Context,
            Sequence =>
              "<" &
              To_String
                (Source =>
                   Insert
                     (Source =>
                        To_Unbounded_String
                          (Source => Get_Map_Accelerator(Index => I)),
                      Before =>
                        Index
                          (Source =>
                             To_Unbounded_String
                               (Source => Get_Map_Accelerator(Index => I)),
                           Pattern => "-", Going => Backward) +
                        1,
                      New_Item => "KeyPress-")) &
              ">",
            Script => To_String(Source => Tcl_Commands_Array(I)));
      end loop Bind_Commands_Loop;
      Bind_To_Main_Window
        (Interp => Get_Context,
         Sequence =>
           "<" &
           To_String
             (Source =>
                Insert
                  (Source =>
                     To_Unbounded_String(Source => Get_Full_Screen_Accel),
                   Before =>
                     Index
                       (Source =>
                          To_Unbounded_String(Source => Get_Full_Screen_Accel),
                        Pattern => "-", Going => Backward) +
                     1,
                   New_Item => "KeyPress-")) &
           ">",
         Script => "{ToggleFullScreen}");
   end Set_Keys;

   function Get_General_Accelerator(Index: Positive) return String is

      function Get_Ada_General_Accelerator(I: Positive) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaGeneralAccelerator";
   begin
      return Value(Item => Get_Ada_General_Accelerator(I => Index));
   end Get_General_Accelerator;

   procedure Set_General_Accelerator(Index: Positive; Value: String) is
      procedure Set_Ada_General_Accelerator(I: Positive; Val: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaGeneralAccelerator";
   begin
      Set_Ada_General_Accelerator(I => Index, Val => New_String(Str => Value));
   end Set_General_Accelerator;

   function Get_Menu_Accelerator(Index: Positive) return String is

      function Get_Ada_Menu_Accelerator(I: Positive) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaMenuAccelerator";
   begin
      return Value(Item => Get_Ada_Menu_Accelerator(I => Index));
   end Get_Menu_Accelerator;

   procedure Set_Menu_Accelerator(Index: Positive; Value: String) is
      procedure Set_Ada_Menu_Accelerator(I: Positive; Val: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaMenuAccelerator";
   begin
      Set_Ada_Menu_Accelerator(I => Index, Val => New_String(Str => Value));
   end Set_Menu_Accelerator;

   function Get_Map_Accelerator(Index: Positive) return String is

      function Get_Ada_Map_Accelerator(I: Positive) return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaMapAccelerator";
   begin
      return Value(Item => Get_Ada_Map_Accelerator(I => Index));
   end Get_Map_Accelerator;

   procedure Set_Map_Accelerator(Index: Positive; Value: String) is
      procedure Set_Ada_Map_Accelerator(I: Positive; Val: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaMapAccelerator";
   begin
      Set_Ada_Map_Accelerator(I => Index, Val => New_String(Str => Value));
   end Set_Map_Accelerator;

   function Get_Full_Screen_Accel return String is

      function Get_Ada_Full_Screen_Accel return chars_ptr with
         Import => True,
         Convention => C,
         External_Name => "getAdaFullScreenAccel";
   begin
      return Value(Item => Get_Ada_Full_Screen_Accel);
   end Get_Full_Screen_Accel;

   procedure Set_Full_Screen_Accel(Value: String) is
      procedure Set_Ada_Full_Screen_Accel(Val: chars_ptr) with
         Import => True,
         Convention => C,
         External_Name => "setAdaFullScreenAccel";
   begin
      Set_Ada_Full_Screen_Accel(Val => New_String(Str => Value));
   end Set_Full_Screen_Accel;

end Maps.UI;
