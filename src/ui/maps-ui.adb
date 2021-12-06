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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Strings;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Pack;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
use Tcl.Tk.Ada.Widgets.TtkEntry.TtkComboBox;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Widgets.TtkPanedWindow; use Tcl.Tk.Ada.Widgets.TtkPanedWindow;
with Tcl.Tk.Ada.Widgets.TtkWidget; use Tcl.Tk.Ada.Widgets.TtkWidget;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tk.Ada.Wm; use Tcl.Tk.Ada.Wm;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Bases; use Bases;
with Bases.LootUI;
with Bases.RecruitUI;
with Bases.SchoolUI;
with Bases.ShipyardUI;
with Bases.UI;
with BasesTypes; use BasesTypes;
with Config; use Config;
with Crafts.UI;
with CoreUI; use CoreUI;
with Crew; use Crew;
with Dialogs; use Dialogs;
with DebugUI; use DebugUI;
with Factions; use Factions;
with GameOptions;
with Help.UI; use Help.UI;
with Items; use Items;
with Knowledge; use Knowledge;
with Log;
with Maps.UI.Commands;
with Messages; use Messages;
with Messages.UI; use Messages.UI;
with Missions.UI; use Missions.UI;
with OrdersMenu;
with ShipModules; use ShipModules;
with Ships.Cargo; use Ships.Cargo;
with Ships.Movement; use Ships.Movement;
with Ships.UI; use Ships.UI;
with Statistics; use Statistics;
with Statistics.UI;
with Stories; use Stories;
with Trades.UI;
with Themes; use Themes;
with Utils.UI; use Utils.UI;
with WaitMenu;

package body Maps.UI is

   procedure UpdateHeader is
      HaveWorker, HaveGunner: Boolean := True;
      NeedCleaning, NeedRepairs, NeedWorker, HavePilot, HaveEngineer,
      HaveTrader, HaveUpgrader, HaveCleaner, HaveRepairman: Boolean := False;
      ItemAmount: Natural := 0;
      Label: Ttk_Label := Get_Widget(Game_Header & ".time");
      Frame: constant Ttk_Frame := Get_Widget(Main_Paned & ".combat");
   begin
      configure(Label, "-text {" & FormatedTime & "}");
      if Game_Settings.Show_Numbers then
         configure
           (Label,
            "-text {" & FormatedTime & " Speed:" &
            Natural'Image((RealSpeed(Player_Ship) * 60) / 1_000) & " km/h}");
         Add(Label, "Game time and current ship speed.");
      end if;
      Label.Name := New_String(Game_Header & ".nofuel");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ItemAmount := GetItemAmount(Fuel_Type);
      if ItemAmount = 0 then
         configure(Label, "-style Headerred.TLabel");
         Add
           (Label,
            "You can't travel anymore, because you don't have any fuel for ship.");
         Tcl.Tk.Ada.Grid.Grid(Label);
      elsif ItemAmount <= Game_Settings.Low_Fuel then
         configure(Label, "-style TLabel");
         Add
           (Label,
            "Low level of fuel on ship. Only" & Natural'Image(ItemAmount) &
            " left.");
         Tcl.Tk.Ada.Grid.Grid(Label);
      end if;
      Label.Name := New_String(Game_Header & ".nodrink");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ItemAmount := GetItemsAmount("Drinks");
      if ItemAmount = 0 then
         configure(Label, "-style Headerred.TLabel");
         Add
           (Label,
            "You don't have any drinks in ship but your crew needs them to live.");
         Tcl.Tk.Ada.Grid.Grid(Label);
      elsif ItemAmount <= Game_Settings.Low_Drinks then
         configure(Label, "-style TLabel");
         Add
           (Label,
            "Low level of drinks on ship. Only" & Natural'Image(ItemAmount) &
            " left.");
         Tcl.Tk.Ada.Grid.Grid(Label);
      end if;
      Label.Name := New_String(Game_Header & ".nofood");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ItemAmount := GetItemsAmount("Food");
      if ItemAmount = 0 then
         configure(Label, "-style Headerred.TLabel");
         Add
           (Label,
            "You don't have any food in ship but your crew needs it to live.");
         Tcl.Tk.Ada.Grid.Grid(Label);
      elsif ItemAmount <= Game_Settings.Low_Food then
         configure(Label, "-style TLabel");
         Add
           (Label,
            "Low level of food on ship. Only" & Natural'Image(ItemAmount) &
            " left.");
         Tcl.Tk.Ada.Grid.Grid(Label);
      end if;
      for Member of Player_Ship.Crew loop
         case Member.Order is
            when PILOT =>
               HavePilot := True;
            when ENGINEER =>
               HaveEngineer := True;
            when TALK =>
               HaveTrader := True;
            when UPGRADING =>
               HaveUpgrader := True;
            when CLEAN =>
               HaveCleaner := True;
            when REPAIR =>
               HaveRepairman := True;
            when others =>
               null;
         end case;
      end loop;
      Label.Name := New_String(Game_Header & ".overloaded");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      if HavePilot and
        (HaveEngineer or
         Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
           (To_Unbounded_String("sentientships"))) and
        (Winfo_Get(Frame, "exists") = "0"
         or else (Winfo_Get(Frame, "ismapped") = "0")) then
         declare
            type SpeedType is digits 2;
            Speed: constant SpeedType :=
              (if Player_Ship.Speed /= DOCKED then
                 (SpeedType(RealSpeed(Player_Ship)) / 1_000.0)
               else (SpeedType(RealSpeed(Player_Ship, True)) / 1_000.0));
         begin
            if Speed < 0.5 then
               configure(Label, "-style Headerred.TLabel");
               Add
                 (Label,
                  "You can't fly with your ship, because it is overloaded.");
               Tcl.Tk.Ada.Grid.Grid(Label);
            end if;
         end;
      end if;
      Check_Workers_Loop :
      for Module of Player_Ship.Modules loop
         case Modules_List(Module.Proto_Index).MType is
            when GUN | HARPOON_GUN =>
               if Module.Owner(1) = 0 then
                  HaveGunner := False;
               elsif Player_Ship.Crew(Module.Owner(1)).Order /= GUNNER then
                  HaveGunner := False;
               end if;
            when ALCHEMY_LAB .. GREENHOUSE =>
               if Module.Crafting_Index /= Null_Unbounded_String then
                  NeedWorker := True;
                  Check_Owners_Loop :
                  for Owner of Module.Owner loop
                     if Owner = 0 then
                        HaveWorker := False;
                     elsif Player_Ship.Crew(Owner).Order /= CRAFT then
                        HaveWorker := False;
                     end if;
                     exit Check_Owners_Loop when not HaveWorker;
                  end loop Check_Owners_Loop;
               end if;
            when CABIN =>
               if Module.Cleanliness /= Module.Quality then
                  NeedCleaning := True;
               end if;
            when others =>
               null;
         end case;
         if Module.Durability /= Module.Max_Durability then
            NeedRepairs := True;
         end if;
      end loop Check_Workers_Loop;
      Label.Name := New_String(Game_Header & ".pilot");
      if HavePilot then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      else
         if not Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
             (To_Unbounded_String("sentientships")) then
            configure(Label, "-style Headerred.TLabel");
            Add(Label, "No pilot assigned. Ship can't move.");
         else
            configure(Label, "-style TLabel");
            Add(Label, "No pilot assigned. Ship fly on it own.");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Label);
      end if;
      Label.Name := New_String(Game_Header & ".engineer");
      if HaveEngineer then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      else
         if not Factions_List(Player_Ship.Crew(1).Faction).Flags.Contains
             (To_Unbounded_String("sentientships")) then
            configure(Label, "-style Headerred.TLabel");
            Add(Label, "No engineer assigned. Ship can't move.");
         else
            configure(Label, "-style TLabel");
            Add(Label, "No engineer assigned. Ship fly on it own.");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Label);
      end if;
      Label.Name := New_String(Game_Header & ".gunner");
      if HaveGunner then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      else
         configure(Label, "-style Headerred.TLabel");
         Add(Label, "One or more guns don't have a gunner.");
         Tcl.Tk.Ada.Grid.Grid(Label);
      end if;
      Label.Name := New_String(Game_Header & ".repairs");
      if NeedRepairs then
         if HaveRepairman then
            configure(Label, "-style Headergreen.TLabel");
            Add(Label, "The ship is being repaired.");
         else
            configure(Label, "-style Headerred.TLabel");
            Add(Label, "The ship needs repairs but no one is working them.");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Label);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      end if;
      Label.Name := New_String(Game_Header & ".crafting");
      if NeedWorker then
         if HaveWorker then
            configure(Label, "-style Headergreen.TLabel");
            Add(Label, "All crafting orders are being executed.");
         else
            configure(Label, "-style Headerred.TLabel");
            Add
              (Label,
               "You need to assign crew members to begin manufacturing.");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Label);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      end if;
      Label.Name := New_String(Game_Header & ".upgrade");
      if Player_Ship.Upgrade_Module > 0 then
         if HaveUpgrader then
            configure(Label, "-style Headergreen.TLabel");
            Add(Label, "A ship module upgrade in progress.");
         else
            configure(Label, "-style Headerred.TLabel");
            Add
              (Label,
               "A ship module upgrade is in progress but no one is working on it.");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Label);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      end if;
      Label.Name := New_String(Game_Header & ".talk");
      if HaveTrader then
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      elsif Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index > 0 then
         configure(Label, "-style Headerred.TLabel");
         Add(Label, "No trader assigned. You need one to talk/trade.");
         Tcl.Tk.Ada.Grid.Grid(Label);
      elsif Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index > 0 then
         if Events_List
             (Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Event_Index)
             .E_Type =
           FRIENDLYSHIP then
            configure(Label, "-style Headerred.TLabel");
            Add(Label, "No trader assigned. You need one to talk/trade.");
            Tcl.Tk.Ada.Grid.Grid(Label);
         else
            Tcl.Tk.Ada.Grid.Grid_Remove(Label);
         end if;
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      end if;
      Label.Name := New_String(Game_Header & ".clean");
      if NeedCleaning then
         if HaveCleaner then
            configure(Label, "-style Headergreen.TLabel");
            Add(Label, "Ship is cleaned.");
         else
            configure(Label, "-style Headerred.TLabel");
            Add(Label, "Ship is dirty but no one is cleaning it.");
         end if;
         Tcl.Tk.Ada.Grid.Grid(Label);
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      end if;
      if Player_Ship.Crew(1).Health = 0 then
         ShowQuestion
           ("You are dead. Would you like to see your game statistics?",
            "showstats");
      end if;
   end UpdateHeader;

   -- ****iv* MUI/MUI.MapView
   -- FUNCTION
   -- Text widget with the sky map
   -- SOURCE
   MapView: Tk_Text;
   -- ****

   procedure DrawMap is
      MapChar: Wide_Character;
      EndX, EndY: Integer;
      MapHeight, MapWidth: Positive;
      MapTag: Unbounded_String;
      StoryX, StoryY: Natural := 1;
      CurrentTheme: constant Theme_Record :=
        Themes_List(To_String(Game_Settings.Interface_Theme));
   begin
      configure(MapView, "-state normal");
      Delete(MapView, "1.0", "end");
      MapHeight := Positive'Value(cget(MapView, "-height"));
      MapWidth := Positive'Value(cget(MapView, "-width"));
      StartY := CenterY - (MapHeight / 2);
      StartX := CenterX - (MapWidth / 2);
      EndY := CenterY + (MapHeight / 2);
      EndX := CenterX + (MapWidth / 2);
      if StartY < 1 then
         StartY := 1;
         EndY := MapHeight;
      end if;
      if StartX < 1 then
         StartX := 1;
         EndX := MapWidth;
      end if;
      if EndY > 1_024 then
         EndY := 1_024;
         StartY := 1_025 - MapHeight;
      end if;
      if EndX > 1_024 then
         EndX := 1_024;
         StartX := 1_025 - MapWidth;
      end if;
      if CurrentStory.Index /= Null_Unbounded_String then
         GetStoryLocation(StoryX, StoryY);
         if StoryX = Player_Ship.Sky_X and StoryY = Player_Ship.Sky_Y then
            StoryX := 0;
            StoryY := 0;
         end if;
      end if;
      if Player_Ship.Speed = DOCKED and
        Sky_Map(Player_Ship.Sky_X, Player_Ship.Sky_Y).Base_Index = 0 then
         Player_Ship.Speed := Ships.FULL_STOP;
      end if;
      Draw_Map_Loop :
      for Y in StartY .. EndY loop
         for X in StartX .. EndX loop
            MapTag := Null_Unbounded_String;
            if X = Player_Ship.Sky_X and Y = Player_Ship.Sky_Y then
               MapChar := CurrentTheme.Player_Ship_Icon;
            else
               MapChar := CurrentTheme.Empty_Map_Icon;
               MapTag :=
                 (if Sky_Map(X, Y).Visited then To_Unbounded_String("black")
                  else To_Unbounded_String("unvisited gray"));
               if X = Player_Ship.Destination_X and
                 Y = Player_Ship.Destination_Y then
                  MapChar := CurrentTheme.Target_Icon;
                  MapTag :=
                    (if Sky_Map(X, Y).Visited then Null_Unbounded_String
                     else To_Unbounded_String("unvisited"));
               elsif CurrentStory.Index /= Null_Unbounded_String
                 and then (X = StoryX and Y = StoryY) then
                  MapChar := CurrentTheme.Story_Icon;
                  MapTag := To_Unbounded_String("green");
               elsif Sky_Map(X, Y).Mission_Index > 0 then
                  case AcceptedMissions(Sky_Map(X, Y).Mission_Index).MType is
                     when Deliver =>
                        MapChar := CurrentTheme.Deliver_Icon;
                        MapTag := To_Unbounded_String("yellow");
                     when Destroy =>
                        MapChar := CurrentTheme.Destroy_Icon;
                        MapTag := To_Unbounded_String("red");
                     when Patrol =>
                        MapChar := CurrentTheme.Patrol_Icon;
                        MapTag := To_Unbounded_String("lime");
                     when Explore =>
                        MapChar := CurrentTheme.Explore_Icon;
                        MapTag := To_Unbounded_String("green");
                     when Passenger =>
                        MapChar := CurrentTheme.Passenger_Icon;
                        MapTag := To_Unbounded_String("cyan");
                  end case;
                  if not Sky_Map(X, Y).Visited then
                     Append(MapTag, " unvisited");
                  end if;
               elsif Sky_Map(X, Y).Event_Index > 0 then
                  if Sky_Map(X, Y).Event_Index > Events_List.Last_Index then
                     Sky_Map(X, Y).Event_Index := 0;
                  else
                     case Events_List(Sky_Map(X, Y).Event_Index).E_Type is
                        when ENEMYSHIP =>
                           MapChar := CurrentTheme.Enemy_Ship_Icon;
                           MapTag := To_Unbounded_String("red");
                        when ATTACKONBASE =>
                           MapChar := CurrentTheme.Attack_On_Base_Icon;
                           MapTag := To_Unbounded_String("red2");
                        when ENEMYPATROL =>
                           MapChar := CurrentTheme.Enemy_Patrol_Icon;
                           MapTag := To_Unbounded_String("red3");
                        when DISEASE =>
                           MapChar := CurrentTheme.Disease_Icon;
                           MapTag := To_Unbounded_String("yellow");
                        when FULLDOCKS =>
                           MapChar := CurrentTheme.Full_Docks_Icon;
                           MapTag := To_Unbounded_String("cyan");
                        when DOUBLEPRICE =>
                           MapChar := CurrentTheme.Double_Price_Icon;
                           MapTag := To_Unbounded_String("lime");
                        when TRADER =>
                           MapChar := CurrentTheme.Trader_Icon;
                           MapTag := To_Unbounded_String("green");
                        when FRIENDLYSHIP =>
                           MapChar := CurrentTheme.Friendly_Ship_Icon;
                           MapTag := To_Unbounded_String("green2");
                        when others =>
                           null;
                     end case;
                  end if;
                  if not Sky_Map(X, Y).Visited then
                     Append(MapTag, " unvisited");
                  end if;
               elsif Sky_Map(X, Y).Base_Index > 0 then
                  MapChar := CurrentTheme.Not_Visited_Base_Icon;
                  if Sky_Bases(Sky_Map(X, Y).Base_Index).Known then
                     if Sky_Bases(Sky_Map(X, Y).Base_Index).Visited.Year > 0 then
                        MapChar :=
                          Factions_List
                            (Sky_Bases(Sky_Map(X, Y).Base_Index).Owner)
                            .Base_Icon;
                        MapTag := Sky_Bases(Sky_Map(X, Y).Base_Index).Base_Type;
                     else
                        MapTag := To_Unbounded_String("unvisited");
                     end if;
                  else
                     MapTag := To_Unbounded_String("unvisited gray");
                  end if;
               end if;
            end if;
            Insert
              (MapView, "end",
               Encode("" & MapChar) & " [list " & To_String(MapTag) & "]");
         end loop;
         if Y < EndY then
            Insert(MapView, "end", "{" & LF & "}");
         end if;
      end loop Draw_Map_Loop;
      configure(MapView, "-state disable");
   end DrawMap;

   procedure UpdateMapInfo
     (X: Positive := Player_Ship.Sky_X; Y: Positive := Player_Ship.Sky_Y) is
      MapInfoText, EventInfoText: Unbounded_String;
      MapInfo: constant Ttk_Label :=
        Get_Widget(Main_Paned & ".mapframe.info.info");
      EventInfo: constant Ttk_Label :=
        Get_Widget(Main_Paned & ".mapframe.info.eventinfo");
   begin
      Append
        (MapInfoText, "X:" & Positive'Image(X) & " Y:" & Positive'Image(Y));
      if Player_Ship.Sky_X /= X or Player_Ship.Sky_Y /= Y then
         declare
            Distance: constant Positive := Count_Distance(X, Y);
         begin
            Append(MapInfoText, LF & "Distance:" & Positive'Image(Distance));
            Travel_Info(MapInfoText, Distance);
         end;
      end if;
      if Sky_Map(X, Y).Base_Index > 0 then
         declare
            BaseIndex: constant Bases_Range := Sky_Map(X, Y).Base_Index;
         begin
            if Sky_Bases(BaseIndex).Known then
               Append
                 (MapInfoText,
                  LF & "Base info:" & LF & To_Unbounded_String("Name: ") &
                  Sky_Bases(BaseIndex).Name);
            end if;
            if Sky_Bases(BaseIndex).Visited.Year > 0 then
               Append
                 (MapInfoText,
                  LF & "Type: " &
                  To_String
                    (Bases_Types_List(Sky_Bases(BaseIndex).Base_Type).Name));
               if Sky_Bases(BaseIndex).Population > 0 then
                  Append(MapInfoText, LF);
               end if;
               if Sky_Bases(BaseIndex).Population > 0 and
                 Sky_Bases(BaseIndex).Population < 150 then
                  Append(MapInfoText, "Population: small");
               elsif Sky_Bases(BaseIndex).Population > 149 and
                 Sky_Bases(BaseIndex).Population < 300 then
                  Append(MapInfoText, "Population: medium");
               elsif Sky_Bases(BaseIndex).Population > 299 then
                  Append(MapInfoText, "Population: large");
               end if;
               Append
                 (MapInfoText,
                  LF & "Size: " &
                  To_Lower(Bases_Size'Image(Sky_Bases(BaseIndex).Size)) & LF);
               if Sky_Bases(BaseIndex).Population > 0 then
                  Append
                    (MapInfoText,
                     "Owner: " &
                     To_String
                       (Factions_List(Sky_Bases(BaseIndex).Owner).Name));
               else
                  Append(MapInfoText, "Base is abandoned");
               end if;
               if Sky_Bases(BaseIndex).Population > 0 then
                  Append(MapInfoText, LF);
                  case Sky_Bases(BaseIndex).Reputation(1) is
                     when -100 .. -75 =>
                        Append(MapInfoText, "You are hated here");
                     when -74 .. -50 =>
                        Append(MapInfoText, "You are outlawed here");
                     when -49 .. -25 =>
                        Append(MapInfoText, "You are disliked here");
                     when -24 .. -1 =>
                        Append(MapInfoText, "They are unfriendly to you");
                     when 0 =>
                        Append(MapInfoText, "You are unknown here");
                     when 1 .. 25 =>
                        Append(MapInfoText, "You are know here as visitor");
                     when 26 .. 50 =>
                        Append(MapInfoText, "You are know here as trader");
                     when 51 .. 75 =>
                        Append(MapInfoText, "You are know here as friend");
                     when 76 .. 100 =>
                        Append(MapInfoText, "You are well known here");
                     when others =>
                        null;
                  end case;
               end if;
               if BaseIndex = Player_Ship.Home_Base then
                  Append(MapInfoText, LF & "It is your home base");
               end if;
            end if;
         end;
      end if;
      if Sky_Map(X, Y).Event_Index > 0 then
         declare
            EventIndex: constant Events_Container.Extended_Index :=
              Sky_Map(X, Y).Event_Index;
         begin
            if Events_List(EventIndex).E_Type /= BASERECOVERY then
               Append(EventInfoText, LF);
            end if;
            case Events_List(EventIndex).E_Type is
               when ENEMYSHIP | TRADER | FRIENDLYSHIP =>
                  Append
                    (EventInfoText,
                     Proto_Ships_List(Events_List(EventIndex).Ship_Index)
                       .Name);
               when FULLDOCKS =>
                  Append(EventInfoText, "Full docks in base");
               when ATTACKONBASE =>
                  Append(EventInfoText, "Base is under attack");
               when DISEASE =>
                  Append(EventInfoText, "Disease in base");
               when ENEMYPATROL =>
                  Append(EventInfoText, "Enemy patrol");
               when DOUBLEPRICE =>
                  Append
                    (EventInfoText,
                     "Double price for " &
                     To_String
                       (Items_List(Events_List(EventIndex).Item_Index).Name));
               when NONE | BASERECOVERY =>
                  null;
            end case;
            if Events_List(EventIndex).E_Type in DOUBLEPRICE | FRIENDLYSHIP |
                  TRADER then
               configure
                 (EventInfo,
                  "-text {" & To_String(EventInfoText) &
                  "} -style MapInfoGreen.TLabel");
            else
               configure
                 (EventInfo,
                  "-text {" & To_String(EventInfoText) &
                  "} -style MapInfoRed.TLabel");
            end if;
         end;
      end if;
      if Sky_Map(X, Y).Mission_Index > 0 then
         declare
            MissionIndex: constant Mission_Container.Extended_Index :=
              Sky_Map(X, Y).Mission_Index;
         begin
            Append(MapInfoText, LF);
            if Sky_Map(X, Y).Base_Index > 0 or Sky_Map(X, Y).Event_Index > 0 then
               Append(MapInfoText, LF);
            end if;
            case AcceptedMissions(MissionIndex).MType is
               when Deliver =>
                  Append
                    (MapInfoText,
                     "Deliver " &
                     To_String
                       (Items_List(AcceptedMissions(MissionIndex).ItemIndex)
                          .Name));
               when Destroy =>
                  Append
                    (MapInfoText,
                     "Destroy " &
                     To_String
                       (Proto_Ships_List
                          (AcceptedMissions(MissionIndex).ShipIndex)
                          .Name));
               when Patrol =>
                  Append(MapInfoText, "Patrol area");
               when Explore =>
                  Append(MapInfoText, "Explore area");
               when Passenger =>
                  Append(MapInfoText, "Transport passenger");
            end case;
         end;
      end if;
      if CurrentStory.Index /= Null_Unbounded_String then
         declare
            StoryX, StoryY: Natural := 1;
            FinishCondition: StepConditionType;
         begin
            GetStoryLocation(StoryX, StoryY);
            if StoryX = Player_Ship.Sky_X and StoryY = Player_Ship.Sky_Y then
               StoryX := 0;
               StoryY := 0;
            end if;
            if X = StoryX and Y = StoryY then
               FinishCondition :=
                 (if CurrentStory.CurrentStep = 0 then
                    Stories_List(CurrentStory.Index).StartingStep
                      .FinishCondition
                  elsif CurrentStory.CurrentStep > 0 then
                    Stories_List(CurrentStory.Index).Steps
                      (CurrentStory.CurrentStep)
                      .FinishCondition
                  else Stories_List(CurrentStory.Index).FinalStep
                      .FinishCondition);
               if FinishCondition in ASKINBASE | DESTROYSHIP | EXPLORE then
                  Append(MapInfoText, LF & "Story leads you here");
               end if;
            end if;
         end;
      end if;
      if X = Player_Ship.Sky_X and Y = Player_Ship.Sky_Y then
         Append(MapInfoText, LF & "You are here");
      end if;
      configure(MapInfo, "-text {" & To_String(MapInfoText) & "}");
      if EventInfoText /= Null_Unbounded_String then
         Tcl.Tk.Ada.Grid.Grid(EventInfo, "-sticky nwes");
      else
         Tcl.Tk.Ada.Grid.Grid_Remove(EventInfo);
      end if;
   end UpdateMapInfo;

   procedure UpdateMoveButtons is
      MoveButtonsNames: constant array(1 .. 8) of Unbounded_String :=
        (To_Unbounded_String("nw"), To_Unbounded_String("n"),
         To_Unbounded_String("ne"), To_Unbounded_String("w"),
         To_Unbounded_String("e"), To_Unbounded_String("sw"),
         To_Unbounded_String("s"), To_Unbounded_String("se"));
      MoveButtonsTooltips: constant array(1 .. 8) of Unbounded_String :=
        (To_Unbounded_String("Move ship north and west"),
         To_Unbounded_String("Move ship north"),
         To_Unbounded_String("Move ship north and east"),
         To_Unbounded_String("Move ship west"),
         To_Unbounded_String("Move ship east"),
         To_Unbounded_String("Move ship south and west"),
         To_Unbounded_String("Move ship south"),
         To_Unbounded_String("Move ship south and east"));
      Button: Ttk_Button;
      FrameName: constant String := Main_Paned & ".controls.buttons";
      Speedbox: constant Ttk_ComboBox := Get_Widget(FrameName & ".speed");
   begin
      Button.Interp := Get_Context;
      if Player_Ship.Speed = DOCKED then
         Tcl.Tk.Ada.Grid.Grid_Remove(Speedbox);
         Button.Name := New_String(FrameName & ".moveto");
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         Button.Name := New_String(FrameName & ".wait");
         configure(Button, "-text Wait");
         Add(Button, "Wait 1 minute.");
         Disable_Move_Buttons_Loop :
         for ButtonName of MoveButtonsNames loop
            Button.Name := New_String(FrameName & "." & To_String(ButtonName));
            State(Button, "disabled");
            Add
              (Button,
               "You have to give order 'Undock' from\nMenu->Ship orders first to move ship.");
         end loop Disable_Move_Buttons_Loop;
      else
         Current
           (Speedbox, Natural'Image(Ship_Speed'Pos(Player_Ship.Speed) - 1));
         Tcl.Tk.Ada.Grid.Grid(Speedbox);
         if Player_Ship.Destination_X > 0 and
           Player_Ship.Destination_Y > 0 then
            Button.Name := New_String(FrameName & ".moveto");
            Tcl.Tk.Ada.Grid.Grid(Button);
            Tcl.Tk.Ada.Grid.Grid_Configure(Speedbox, "-columnspan 2");
            Button.Name := New_String(FrameName & ".wait");
            configure(Button, "-text Move");
            Add(Button, "Move ship one map field toward destination.");
            Tcl.Tk.Ada.Grid.Grid(Button);
         else
            Button.Name := New_String(FrameName & ".moveto");
            Tcl.Tk.Ada.Grid.Grid_Remove(Button);
            Tcl.Tk.Ada.Grid.Grid_Configure(Speedbox, "-columnspan 3");
            Button.Name := New_String(FrameName & ".wait");
            configure(Button, "-text Wait");
            Add(Button, "Wait 1 minute.");
         end if;
         Enable_Move_Buttons_Loop :
         for I in MoveButtonsNames'Range loop
            Button.Name :=
              New_String(FrameName & "." & To_String(MoveButtonsNames(I)));
            State(Button, "!disabled");
            Add(Button, To_String(MoveButtonsTooltips(I)));
         end loop Enable_Move_Buttons_Loop;
      end if;
   end UpdateMoveButtons;

   procedure CreateGameUI is
      use Log;

      GameFrame: constant Ttk_Frame := Get_Widget(".gameframe");
      Paned: constant Ttk_PanedWindow := Get_Widget(GameFrame & ".paned");
      Button: constant Ttk_Button :=
        Get_Widget(Paned & ".mapframe.buttons.hide");
      SteamSky_Map_Error: exception;
      Header: constant Ttk_Frame := Get_Widget(GameFrame & ".header");
      MessagesFrame: constant Ttk_Frame :=
        Get_Widget(Paned & ".controls.messages");
      PanedPosition: Natural;
   begin
      MapView := Get_Widget(Paned & ".mapframe.map");
      if Winfo_Get(MapView, "exists") = "0" then
         declare
            KeysFile: File_Type;
            Raw_Data, Field_Name, Value: Unbounded_String :=
              Null_Unbounded_String;
            Equal_Index: Natural := 0;
         begin
            Open(KeysFile, In_File, To_String(Save_Directory) & "keys.cfg");
            Load_Accelerators_Loop :
            while not End_Of_File(File => KeysFile) loop
               Raw_Data :=
                 To_Unbounded_String(Source => Get_Line(File => KeysFile));
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
                  MenuAccelerators(1) := Value;
               elsif Field_Name = To_Unbounded_String(Source => "Orders") then
                  MenuAccelerators(2) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "Crafting") then
                  MenuAccelerators(3) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "LastMessages") then
                  MenuAccelerators(4) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "Knowledge") then
                  MenuAccelerators(5) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "WaitOrders") then
                  MenuAccelerators(6) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "GameStats") then
                  MenuAccelerators(7) := Value;
               elsif Field_Name = To_Unbounded_String(Source => "Help") then
                  MenuAccelerators(8) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "GameOptions") then
                  MenuAccelerators(9) := Value;
               elsif Field_Name = To_Unbounded_String(Source => "Quit") then
                  MenuAccelerators(10) := Value;
               elsif Field_Name = To_Unbounded_String(Source => "Resign") then
                  MenuAccelerators(11) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "GameMenu") then
                  MapAccelerators(1) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MapOptions") then
                  MapAccelerators(2) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "ZoomInMap") then
                  MapAccelerators(3) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "ZoomOutMap") then
                  MapAccelerators(4) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveUpLeft") then
                  MapAccelerators(5) := Value;
               elsif Field_Name = To_Unbounded_String(Source => "MoveUp") then
                  MapAccelerators(6) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveUpRight") then
                  MapAccelerators(7) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveLeft") then
                  MapAccelerators(8) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "WaitInPlace") then
                  MapAccelerators(10) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveRight") then
                  MapAccelerators(9) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveDownLeft") then
                  MapAccelerators(11) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveDown") then
                  MapAccelerators(12) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveDownRight") then
                  MapAccelerators(13) := Value;
               elsif Field_Name = To_Unbounded_String(Source => "MoveTo") then
                  MapAccelerators(14) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "CenterMap") then
                  MapAccelerators(15) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "CenterMapOnHomeBase") then
                  MapAccelerators(16) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapUpLeft") then
                  MapAccelerators(17) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapUp") then
                  MapAccelerators(18) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapUpRight") then
                  MapAccelerators(19) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapLeft") then
                  MapAccelerators(20) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapRight") then
                  MapAccelerators(21) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapDownLeft") then
                  MapAccelerators(22) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapDown") then
                  MapAccelerators(23) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveMapDownRight") then
                  MapAccelerators(24) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorUpLeft") then
                  MapAccelerators(25) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorUp") then
                  MapAccelerators(26) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorUpRight") then
                  MapAccelerators(27) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorLeft") then
                  MapAccelerators(28) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorRight") then
                  MapAccelerators(29) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorDownLeft") then
                  MapAccelerators(30) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorDown") then
                  MapAccelerators(31) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "MoveCursorDownRight") then
                  MapAccelerators(32) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "LeftClickMouse") then
                  MapAccelerators(33) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "FullStop") then
                  MapAccelerators(34) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "QuarterSpeed") then
                  MapAccelerators(35) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "HalfSpeed") then
                  MapAccelerators(36) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "FullSpeed") then
                  MapAccelerators(37) := Value;
               elsif Field_Name =
                 To_Unbounded_String(Source => "FullScreen") then
                  FullScreenAccel := Value;
               end if;
               <<End_Of_Loop>>
            end loop Load_Accelerators_Loop;
            Close(KeysFile);
         exception
            when others =>
               if Dir_Separator = '\' then
                  MapAccelerators(5) := To_Unbounded_String(Source => "Home");
                  MapAccelerators(6) := To_Unbounded_String(Source => "Up");
                  MapAccelerators(7) := To_Unbounded_String(Source => "Prior");
                  MapAccelerators(8) := To_Unbounded_String(Source => "Left");
                  MapAccelerators(9) := To_Unbounded_String(Source => "Clear");
                  MapAccelerators(10) :=
                    To_Unbounded_String(Source => "Right");
                  MapAccelerators(11) := To_Unbounded_String(Source => "End");
                  MapAccelerators(12) := To_Unbounded_String(Source => "Down");
                  MapAccelerators(13) := To_Unbounded_String(Source => "Next");
                  MapAccelerators(14) :=
                    To_Unbounded_String(Source => "slash");
                  MapAccelerators(17) :=
                    To_Unbounded_String(Source => "Shift-Home");
                  MapAccelerators(18) :=
                    To_Unbounded_String(Source => "Shift-Up");
                  MapAccelerators(19) :=
                    To_Unbounded_String(Source => "Shift-Prior");
                  MapAccelerators(20) :=
                    To_Unbounded_String(Source => "Shift-Left");
                  MapAccelerators(21) :=
                    To_Unbounded_String(Source => "Shift-Right");
                  MapAccelerators(22) :=
                    To_Unbounded_String(Source => "Shift-End");
                  MapAccelerators(23) :=
                    To_Unbounded_String(Source => "Shift-Down");
                  MapAccelerators(24) :=
                    To_Unbounded_String(Source => "Shift-Next");
                  MapAccelerators(25) :=
                    To_Unbounded_String(Source => "Control-Home");
                  MapAccelerators(26) :=
                    To_Unbounded_String(Source => "Control-Up");
                  MapAccelerators(27) :=
                    To_Unbounded_String(Source => "Control-Prior");
                  MapAccelerators(28) :=
                    To_Unbounded_String(Source => "Control-Left");
                  MapAccelerators(29) :=
                    To_Unbounded_String(Source => "Control-Right");
                  MapAccelerators(30) :=
                    To_Unbounded_String(Source => "Control-End");
                  MapAccelerators(31) :=
                    To_Unbounded_String(Source => "Control-Down");
                  MapAccelerators(32) :=
                    To_Unbounded_String(Source => "Control-Next");
               end if;
         end;
         Tcl_EvalFile
           (Get_Context,
            To_String(Data_Directory) & "ui" & Dir_Separator & "game.tcl");
         Main_Paned := Paned;
         Game_Header := Header;
         Close_Button := Get_Widget(Game_Header & ".closebutton");
         Set_Theme;
         OrdersMenu.AddCommands;
         Maps.UI.Commands.AddCommands;
         WaitMenu.AddCommands;
         Help.UI.AddCommands;
         Ships.UI.AddCommands;
         Crafts.UI.AddCommands;
         Messages.UI.AddCommands;
         GameOptions.AddCommands;
         Trades.UI.AddCommands;
         SchoolUI.AddCommands;
         RecruitUI.AddCommands;
         Bases.UI.AddCommands;
         ShipyardUI.AddCommands;
         LootUI.AddCommands;
         Knowledge.AddCommands;
         Missions.UI.AddCommands;
         Statistics.UI.AddCommands;
         Bind(MessagesFrame, "<Configure>", "ResizeLastMessages");
         Bind(MapView, "<Configure>", "DrawMap");
         Bind(MapView, "<Motion>", "{UpdateMapInfo %x %y}");
         Bind
           (MapView,
            "<Button-" & (if Game_Settings.Right_Button then "3" else "1") &
            ">",
            "{ShowDestinationMenu %X %Y}");
         Bind
           (MapView, "<MouseWheel>",
            "{if {%D > 0} {ZoomMap raise} else {ZoomMap lower}}");
         Bind(MapView, "<Button-4>", "{ZoomMap raise}");
         Bind(MapView, "<Button-5>", "{ZoomMap lower}");
         SetKeys;
         if Log.Debug_Mode = Log.MENU then
            ShowDebugUI;
         end if;
      else
         Tcl.Tk.Ada.Pack.Pack(GameFrame, "-fill both -expand true");
      end if;
      Wm_Set(Get_Main_Window(Get_Context), "title", "{Steam Sky}");
      if Game_Settings.Full_Screen then
         Wm_Set(Get_Main_Window(Get_Context), "attributes", "-fullscreen 1");
      end if;
      Set_Accelerators_Loop :
      for I in MenuAccelerators'Range loop
         Bind_To_Main_Window
           (Get_Context,
            "<" &
            To_String
              (Insert
                 (MenuAccelerators(I),
                  Index(MenuAccelerators(I), "-", Backward) + 1,
                  "KeyPress-")) &
            ">",
            "{InvokeMenu " & To_String(MenuAccelerators(I)) & "}");
      end loop Set_Accelerators_Loop;
      if Index
          (Tcl.Tk.Ada.Grid.Grid_Slaves(Get_Main_Window(Get_Context)),
           ".gameframe.header") =
        0 then
         Tcl.Tk.Ada.Grid.Grid(Header);
      end if;
      UpdateHeader;
      CenterX := Player_Ship.Sky_X;
      CenterY := Player_Ship.Sky_Y;
      Set_Tags_Loop :
      for I in Bases_Types_List.Iterate loop
         Tag_Configure
           (MapView, To_String(BasesTypes_Container.Key(I)),
            "-foreground #" & Bases_Types_List(I).Color);
      end loop Set_Tags_Loop;
      PanedPosition :=
        (if Game_Settings.Window_Height - Game_Settings.Messages_Position < 0
         then Game_Settings.Window_Height
         else Game_Settings.Window_Height - Game_Settings.Messages_Position);
      SashPos(Paned, "0", Natural'Image(PanedPosition));
      if Index
          (Tcl.Tk.Ada.Grid.Grid_Slaves(Get_Main_Window(Get_Context)),
           ".gameframe.paned") =
        0 then
         Tcl.Tk.Ada.Grid.Grid(Paned);
      end if;
      if Invoke(Button) /= "" then
         raise SteamSky_Map_Error with "Can't hide map buttons";
      end if;
      Bind_To_Main_Window
        (Get_Context, "<Escape>", "{InvokeButton " & Close_Button & "}");
      Update_Messages;
      UpdateMoveButtons;
      UpdateMapInfo;
      if not Game_Settings.Show_Last_Messages then
         Tcl.Tk.Ada.Grid.Grid_Remove(MessagesFrame);
      end if;
      Tcl_SetVar(Get_Context, "shipname", To_String(Player_Ship.Name));
      Tcl_SetVar(Get_Context, "gamestate", "general");
   end CreateGameUI;

   procedure ShowSkyMap(Clear: Boolean := False) is
   begin
      if Clear then
         Show_Screen("mapframe");
      end if;
      Tcl_SetVar(Get_Context, "gamestate", "general");
      UpdateHeader;
      Tcl_Eval(Get_Context, "DrawMap");
      UpdateMoveButtons;
      Tcl_Eval(Get_Context, "update");
      Update_Messages;
      if CurrentStory.Index /= Null_Unbounded_String and
        CurrentStory.ShowText then
         if CurrentStory.CurrentStep > -2 then
            ShowInfo(Text => To_String(GetCurrentStoryText), Title => "Story");
         else
            FinishStory;
            if Player_Ship.Crew(1).Health = 0 then
               ShowQuestion
                 ("You are dead. Would you like to see your game statistics?",
                  "showstats");
            end if;
         end if;
         CurrentStory.ShowText := False;
      end if;
   end ShowSkyMap;

   procedure SetKeys is
      Commands: constant array(MapAccelerators'Range) of Unbounded_String :=
        (To_Unbounded_String
           ("{if {[winfo class [focus]] != {TEntry} && [tk busy status " &
            Game_Header & "] == 0} {ShowGameMenu}}"),
         To_Unbounded_String
           ("{" & Main_Paned & ".mapframe.buttons.wait invoke}"),
         To_Unbounded_String("{ZoomMap raise}"),
         To_Unbounded_String("{ZoomMap lower}"),
         To_Unbounded_String("{InvokeButton $bframe.nw}"),
         To_Unbounded_String("{InvokeButton $bframe.n}"),
         To_Unbounded_String("{InvokeButton $bframe.ne}"),
         To_Unbounded_String("{InvokeButton $bframe.w}"),
         To_Unbounded_String("{InvokeButton $bframe.wait}"),
         To_Unbounded_String("{InvokeButton $bframe.e}"),
         To_Unbounded_String("{InvokeButton $bframe.sw}"),
         To_Unbounded_String("{InvokeButton $bframe.s}"),
         To_Unbounded_String("{InvokeButton $bframe.se}"),
         To_Unbounded_String("{InvokeButton $bframe.moveto}"),
         To_Unbounded_String("{MoveMap centeronship}"),
         To_Unbounded_String("{MoveMap centeronhome}"),
         To_Unbounded_String("{MoveMap nw}"),
         To_Unbounded_String("{MoveMap n}"),
         To_Unbounded_String("{MoveMap ne}"),
         To_Unbounded_String("{MoveMap w}"),
         To_Unbounded_String("{MoveMap e}"),
         To_Unbounded_String("{MoveMap sw}"),
         To_Unbounded_String("{MoveMap s}"),
         To_Unbounded_String("{MoveMap se}"),
         To_Unbounded_String("{MoveCursor nw %x %y}"),
         To_Unbounded_String("{MoveCursor n %x %y}"),
         To_Unbounded_String("{MoveCursor ne %x %y}"),
         To_Unbounded_String("{MoveCursor w %x %y}"),
         To_Unbounded_String("{MoveCursor e %x %y}"),
         To_Unbounded_String("{MoveCursor sw %x %y}"),
         To_Unbounded_String("{MoveCursor s %x %y}"),
         To_Unbounded_String("{MoveCursor se %x %y}"),
         To_Unbounded_String("{MoveCursor click %x %y}"),
         To_Unbounded_String
           ("{" & Main_Paned & ".controls.buttons.speed current 0}"),
         To_Unbounded_String
           ("{" & Main_Paned & ".controls.buttons.speed current 1}"),
         To_Unbounded_String
           ("{" & Main_Paned & ".controls.buttons.speed current 2}"),
         To_Unbounded_String
           ("{" & Main_Paned & ".controls.buttons.speed current 3}"));
   begin
      for I in Commands'Range loop
         Bind_To_Main_Window
           (Get_Context,
            "<" &
            To_String
              (Insert
                 (MapAccelerators(I),
                  Index(MapAccelerators(I), "-", Backward) + 1, "KeyPress-")) &
            ">",
            To_String(Commands(I)));
      end loop;
      Bind_To_Main_Window
        (Get_Context,
         "<" &
         To_String
           (Insert
              (FullScreenAccel, Index(FullScreenAccel, "-", Backward) + 1,
               "KeyPress-")) &
         ">",
         "{ToggleFullScreen}");
   end SetKeys;

   procedure FinishStory is
   begin
      GameStats.Points := GameStats.Points + (10_000 * CurrentStory.MaxSteps);
      ClearCurrentStory;
      ShowQuestion
        (To_String(Stories_List(CurrentStory.Index).EndText) &
         " Are you want to finish game?",
         "retire");
   end FinishStory;

end Maps.UI;
