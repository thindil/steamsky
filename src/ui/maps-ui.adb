-- Copyright (c) 2020 Bartek thindil Jasicki <thindil@laeran.pl>
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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.UTF_Encoding.Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Strings;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Dialogs; use Tcl.Tk.Ada.Dialogs;
with Tcl.Tk.Ada.Font; use Tcl.Tk.Ada.Font;
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
with BasesList; use BasesList;
with BasesTypes; use BasesTypes;
with Config; use Config;
with Crafts.UI;
with Crew; use Crew;
with Crew.UI;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with GameOptions;
with Help.UI; use Help.UI;
with Items; use Items;
with MainMenu; use MainMenu;
with Maps.UI.Commands;
with Messages; use Messages;
with Messages.UI; use Messages.UI;
with Missions; use Missions;
with ShipModules; use ShipModules;
with OrdersMenu;
with Ships.Cargo; use Ships.Cargo;
with Ships.Cargo.UI; use Ships.Cargo.UI;
with Ships.Movement; use Ships.Movement;
with Ships.UI; use Ships.UI;
with Statistics.UI; use Statistics.UI;
with Stories; use Stories;
with Utils.UI; use Utils.UI;
with WaitMenu;

package body Maps.UI is

   -- ****if* MUI/CreateGameMenu
   -- FUNCTION
   -- Create the main game menu. Clear old elements and add all default
   -- SOURCE
   procedure CreateGameMenu is
      -- ****
   begin
      Delete(GameMenu, "0", "end");
      Menu.Add
        (GameMenu, "command",
         "-label {Ship information} -command ShowShipInfo");
      Menu.Add
        (GameMenu, "command", "-label {Ship cargo} -command ShowCargoInfo");
      Menu.Add
        (GameMenu, "command",
         "-label {Crew information} -command ShowCrewInfo");
      Menu.Add
        (GameMenu, "command", "-label {Ship orders} -command ShowOrders");
      Menu.Add(GameMenu, "command", "-label {Crafting} -command ShowCrafting");
      Menu.Add
        (GameMenu, "command",
         "-label {Last messages} -command ShowLastMessages");
      Menu.Add
        (GameMenu, "command",
         "-label {List of known bases} -command ShowBases");
      Menu.Add
        (GameMenu, "command",
         "-label {List of known events} -command ShowEvents");
      Menu.Add
        (GameMenu, "command",
         "-label {Accepted missions} -command {ShowMissions accepted}");
      Menu.Add(GameMenu, "command", "-label {Stories} -command ShowStories");
      Menu.Add(GameMenu, "command", "-label {Wait orders} -command ShowWait");
      Menu.Add
        (GameMenu, "command", "-label {Game statistics} -command ShowStats");
      Menu.Add
        (GameMenu, "command", "-label {Help} -command {ShowHelp general}");
      Menu.Add
        (GameMenu, "command", "-label {Game options} -command ShowOptions");
      Menu.Add
        (GameMenu, "command", "-label {Quit from game} -command QuitGame");
      Menu.Add
        (GameMenu, "command", "-label {Resign from game} -command ResignGame");
      for I in MenuAccelerators'Range loop
         Entry_Configure
           (GameMenu, Positive'Image(I),
            "-accelerator {" & To_String(MenuAccelerators(I)) & "}");
      end loop;
   end CreateGameMenu;

   procedure DeathConfirm is
      Button: Ttk_Button;
      Paned: Ttk_PanedWindow;
   begin
      if MessageBox
          ("-message {You are dead. Would you like to see your game statistics?} -icon question -type yesno") =
        "yes" then
         Button.Interp := Get_Context;
         Button.Name := New_String(".header.menubutton");
         Tcl.Tk.Ada.Grid.Grid(Button);
         Button.Name := New_String(".header.closebutton");
         configure(Button, "-command ShowMainMenu");
         Tcl.Tk.Ada.Grid.Grid(Button);
         Delete(GameMenu, "3", "4");
         Delete(GameMenu, "6", "14");
         ShowStatistics;
      else
         Paned.Interp := Get_Context;
         Paned.Name := New_String(".paned");
         GameSettings.MessagesPosition := Natural'Value(SashPos(Paned, "0"));
         EndGame(False);
         ShowMainMenu;
      end if;
   end DeathConfirm;

   procedure UpdateHeader is
      HaveWorker, HaveGunner: Boolean := True;
      NeedCleaning, NeedRepairs, NeedWorker, HavePilot, HaveEngineer,
      HaveTrader, HaveUpgrader, HaveCleaner, HaveRepairman: Boolean := False;
      ItemAmount: Natural := 0;
      Label: Ttk_Label;
      Button: Ttk_Button;
      Frame: Ttk_Frame;
   begin
      Label.Interp := Get_Context;
      Label.Name := New_String(".header.time");
      configure(Label, "-text {" & FormatedTime & "}");
      if GameSettings.ShowNumbers then
         configure
           (Label,
            "-text {" & FormatedTime & " Speed:" &
            Natural'Image((RealSpeed(PlayerShip) * 60) / 1000) & " km/h}");
         Add(Label, "Game time and current ship speed.");
      end if;
      Label.Name := New_String(".header.nofuel");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ItemAmount := GetItemAmount(FuelType);
      if ItemAmount = 0 then
         configure(Label, "-style Headerred.TLabel");
         Add
           (Label,
            "You can't travel anymore, because you don't have any fuel for ship.");
         Tcl.Tk.Ada.Grid.Grid(Label);
      elsif ItemAmount <= GameSettings.LowFuel then
         configure(Label, "-style TLabel");
         Add
           (Label,
            "Low level of fuel on ship. Only" & Natural'Image(ItemAmount) &
            " left.");
         Tcl.Tk.Ada.Grid.Grid(Label);
      end if;
      Label.Name := New_String(".header.nodrink");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ItemAmount := GetItemsAmount("Drinks");
      if ItemAmount = 0 then
         configure(Label, "-style Headerred.TLabel");
         Add
           (Label,
            "You don't have any drinks in ship but your crew needs them to live.");
      elsif ItemAmount <= GameSettings.LowDrinks then
         configure(Label, "-style TLabel");
         Add
           (Label,
            "Low level of drinks on ship. Only" & Natural'Image(ItemAmount) &
            " left.");
      end if;
      Label.Name := New_String(".header.nofood");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      ItemAmount := GetItemsAmount("Food");
      if ItemAmount = 0 then
         configure(Label, "-style Headerred.TLabel");
         Add
           (Label,
            "You don't have any food in ship but your crew needs it to live.");
      elsif ItemAmount <= GameSettings.LowFood then
         configure(Label, "-style TLabel");
         Add
           (Label,
            "Low level of food on ship. Only" & Natural'Image(ItemAmount) &
            " left.");
      end if;
      for Member of PlayerShip.Crew loop
         case Member.Order is
            when Pilot =>
               HavePilot := True;
            when Engineer =>
               HaveEngineer := True;
            when Talk =>
               HaveTrader := True;
            when Upgrading =>
               HaveUpgrader := True;
            when Clean =>
               HaveCleaner := True;
            when Repair =>
               HaveRepairman := True;
            when others =>
               null;
         end case;
      end loop;
      Label.Name := New_String(".header.overloaded");
      Tcl.Tk.Ada.Grid.Grid_Remove(Label);
      Frame.Interp := Get_Context;
      Frame.Name := New_String(".paned.combat");
      if HavePilot and
        (HaveEngineer or
         Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
           (To_Unbounded_String("sentientships"))) and
        (Winfo_Get(Frame, "exists") = "0"
         or else (Winfo_Get(Frame, "ismapped") = "0")) then
         declare
            type SpeedType is digits 2;
            Speed: SpeedType;
         begin
            if PlayerShip.Speed /= DOCKED then
               Speed := (SpeedType(RealSpeed(PlayerShip)) / 1000.0);
            else
               Speed := (SpeedType(RealSpeed(PlayerShip, True)) / 1000.0);
            end if;
            if Speed < 0.5 then
               configure(Label, "-style Headerred.TLabel");
               Add
                 (Label,
                  "You can't fly with your ship, because it is overloaded.");
            end if;
         end;
      end if;
      for Module of PlayerShip.Modules loop
         case Modules_List(Module.ProtoIndex).MType is
            when GUN | HARPOON_GUN =>
               if Module.Owner(1) = 0 then
                  HaveGunner := False;
               elsif PlayerShip.Crew(Module.Owner(1)).Order /= Gunner then
                  HaveGunner := False;
               end if;
            when ALCHEMY_LAB .. GREENHOUSE =>
               if Module.CraftingIndex /= Null_Unbounded_String then
                  NeedWorker := True;
                  for Owner of Module.Owner loop
                     if Owner = 0 then
                        HaveWorker := False;
                     elsif PlayerShip.Crew(Owner).Order /= Craft then
                        HaveWorker := False;
                     end if;
                     exit when not HaveWorker;
                  end loop;
               end if;
            when CABIN =>
               if Module.Cleanliness /= Module.Quality then
                  NeedCleaning := True;
               end if;
            when others =>
               null;
         end case;
         if Module.Durability /= Module.MaxDurability then
            NeedRepairs := True;
         end if;
      end loop;
      Button.Interp := Get_Context;
      Button.Name := New_String(".header.pilot");
      if HavePilot then
         configure(Button, "-style Headergreen.Toolbutton");
         Add(Button, "Pilot is in position.");
      else
         if not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
             (To_Unbounded_String("sentientships")) then
            configure(Button, "-style Headerred.Toolbutton");
            Add(Button, "No pilot assigned. Ship can't move.");
         else
            configure(Button, "-style Header.Toolbutton");
            Add(Button, "No pilot assigned. Ship fly on it own.");
         end if;
      end if;
      Button.Name := New_String(".header.engineer");
      if HaveEngineer then
         configure(Button, "-style Headergreen.Toolbutton");
         Add(Button, "Engineer is in position.");
      else
         if not Factions_List(PlayerShip.Crew(1).Faction).Flags.Contains
             (To_Unbounded_String("sentientships")) then
            configure(Button, "-style Headerred.Toolbutton");
            Add(Button, "No engineer assigned. Ship can't move.");
         else
            configure(Button, "-style Header.Toolbutton");
            Add(Button, "No engineer assigned. Ship fly on it own.");
         end if;
      end if;
      Button.Name := New_String(".header.gunner");
      if HaveGunner then
         configure(Button, "-style Headergreen.Toolbutton");
         Add(Button, "All guns are manned.");
      else
         configure(Button, "-style Headerred.Toolbutton");
         Add(Button, "One or more guns don't have a gunner.");
      end if;
      Button.Name := New_String(".header.repairs");
      if NeedRepairs then
         if HaveRepairman then
            configure(Button, "-style Headergreen.Toolbutton");
            Add(Button, "The ship is being repaired.");
         else
            configure(Button, "-style Headerred.Toolbutton");
            Add(Button, "The ship needs repairs but no one is working them.");
         end if;
      else
         configure(Button, "-style Header.Toolbutton");
         Add(Button, "The ship doesn't require repairs.");
      end if;
      Button.Name := New_String(".header.crafting");
      if NeedWorker then
         if HaveWorker then
            configure(Button, "-style Headergreen.Toolbutton");
            Add(Button, "All crafting orders are being executed.");
         else
            configure(Button, "-style Headerred.Toolbutton");
            Add
              (Button,
               "You need to assign crew members to begin manufacturing.");
         end if;
      else
         configure(Button, "-style Header.Toolbutton");
         Add(Button, "No crafting orders were set.");
      end if;
      Button.Name := New_String(".header.upgrade");
      if PlayerShip.UpgradeModule > 0 then
         if HaveUpgrader then
            configure(Button, "-style Headergreen.Toolbutton");
            Add(Button, "A ship module upgrade in progress.");
         else
            configure(Button, "-style Headerred.Toolbutton");
            Add
              (Button,
               "A ship module upgrade is in progress but no one is working on it.");
         end if;
      else
         configure(Button, "-style Header.Toolbutton");
         Add(Button, "No ship module upgrade was set.");
      end if;
      Button.Name := New_String(".header.talk");
      if HaveTrader then
         configure(Button, "-style Headergreen.Toolbutton");
         Add(Button, "Trader is in position.");
      elsif SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex > 0 then
         configure(Button, "-style Headerred.Toolbutton");
         Add(Button, "No trader assigned. You need one to talk/trade.");
      elsif SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex > 0 then
         if Events_List(SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).EventIndex)
             .EType =
           FriendlyShip then
            configure(Button, "-style Headerred.Toolbutton");
            Add(Button, "No trader assigned. You need one to talk/trade.");
         else
            configure(Button, "-style Header.Toolbutton");
            Add(Button, "No trader needed.");
         end if;
      else
         configure(Button, "-style Header.Toolbutton");
         Add(Button, "No trader needed.");
      end if;
      Button.Name := New_String(".header.clean");
      if NeedCleaning then
         if HaveCleaner then
            configure(Button, "-style Headergreen.Toolbutton");
            Add(Button, "Ship is cleaned.");
         else
            configure(Button, "-style Headerred.Toolbutton");
            Add(Button, "Ship is dirty but no one is cleaning it.");
         end if;
      else
         configure(Button, "-style Header.Toolbutton");
         Add(Button, "Ship needs no cleaning.");
      end if;
      if PlayerShip.Crew(1).Health = 0 then
         DeathConfirm;
      end if;
   end UpdateHeader;

   -- ****iv* Maps.UI/MapView
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
      StoryX, StoryY: Integer := 0;
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
      if EndY > 1024 then
         EndY := 1024;
         StartY := 1024 - MapHeight;
      end if;
      if EndX > 1024 then
         EndX := 1024;
         StartX := 1024 - MapWidth;
      end if;
      if CurrentStory.Index /= Null_Unbounded_String then
         GetStoryLocation(StoryX, StoryY);
         if StoryX = PlayerShip.SkyX and StoryY = PlayerShip.SkyY then
            StoryX := 0;
            StoryY := 0;
         end if;
      end if;
      if PlayerShip.Speed = DOCKED and
        SkyMap(PlayerShip.SkyX, PlayerShip.SkyY).BaseIndex = 0 then
         PlayerShip.Speed := Ships.FULL_STOP;
      end if;
      for Y in StartY .. EndY loop
         for X in StartX .. EndX loop
            MapTag := Null_Unbounded_String;
            if X = PlayerShip.SkyX and Y = PlayerShip.SkyY then
               MapChar := Wide_Character'Val(16#f135#);
            else
               MapChar := Wide_Character'Val(16#f0c8#);
               if SkyMap(X, Y).Visited then
                  MapTag := To_Unbounded_String("black");
               else
                  MapTag := To_Unbounded_String("unvisited gray");
               end if;
               if X = PlayerShip.DestinationX and
                 Y = PlayerShip.DestinationY then
                  MapChar := Wide_Character'Val(16#f05b#);
                  if SkyMap(X, Y).Visited then
                     MapTag := Null_Unbounded_String;
                  else
                     MapTag := To_Unbounded_String("unvisited");
                  end if;
               elsif X = StoryX and Y = StoryY then
                  MapChar := Wide_Character'Val(16#f059#);
                  MapTag := To_Unbounded_String("green");
               elsif SkyMap(X, Y).MissionIndex > 0 then
                  case AcceptedMissions(SkyMap(X, Y).MissionIndex).MType is
                     when Deliver =>
                        MapChar := Wide_Character'Val(16#f53b#);
                        MapTag := To_Unbounded_String("yellow");
                     when Destroy =>
                        MapChar := Wide_Character'Val(16#fc6a#);
                        MapTag := To_Unbounded_String("red");
                     when Patrol =>
                        MapChar := Wide_Character'Val(16#f540#);
                        MapTag := To_Unbounded_String("lime");
                     when Explore =>
                        MapChar := Wide_Character'Val(16#f707#);
                        MapTag := To_Unbounded_String("green");
                     when Passenger =>
                        MapChar := Wide_Character'Val(16#f183#);
                        MapTag := To_Unbounded_String("cyan");
                  end case;
                  if not SkyMap(X, Y).Visited then
                     Append(MapTag, " unvisited");
                  end if;
               elsif SkyMap(X, Y).EventIndex > 0 then
                  if SkyMap(X, Y).EventIndex > Events_List.Last_Index then
                     SkyMap(X, Y).EventIndex := 0;
                  else
                     case Events_List(SkyMap(X, Y).EventIndex).EType is
                        when EnemyShip =>
                           MapChar := Wide_Character'Val(16#f51c#);
                           MapTag := To_Unbounded_String("red");
                        when AttackOnBase =>
                           MapChar := Wide_Character'Val(16#f543#);
                           MapTag := To_Unbounded_String("red2");
                        when EnemyPatrol =>
                           MapChar := Wide_Character'Val(16#f51b#);
                           MapTag := To_Unbounded_String("red3");
                        when Disease =>
                           MapChar := Wide_Character'Val(16#f5a6#);
                           MapTag := To_Unbounded_String("yellow");
                        when FullDocks =>
                           MapChar := Wide_Character'Val(16#f057#);
                           MapTag := To_Unbounded_String("cyan");
                        when DoublePrice =>
                           MapChar := Wide_Character'Val(16#f0d6#);
                           MapTag := To_Unbounded_String("lime");
                        when Trader =>
                           MapChar := Wide_Character'Val(16#f197#);
                           MapTag := To_Unbounded_String("green");
                        when FriendlyShip =>
                           MapChar := Wide_Character'Val(16#f197#);
                           MapTag := To_Unbounded_String("green2");
                        when others =>
                           null;
                     end case;
                  end if;
                  if not SkyMap(X, Y).Visited then
                     Append(MapTag, " unvisited");
                  end if;
               elsif SkyMap(X, Y).BaseIndex > 0 then
                  MapChar := Wide_Character'Val(16#229b#);
                  if SkyBases(SkyMap(X, Y).BaseIndex).Known then
                     if SkyBases(SkyMap(X, Y).BaseIndex).Visited.Year > 0 then
                        MapChar :=
                          Factions_List(SkyBases(SkyMap(X, Y).BaseIndex).Owner)
                            .BaseIcon;
                        MapTag := SkyBases(SkyMap(X, Y).BaseIndex).BaseType;
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
      end loop;
      configure(MapView, "-state disable");
   end DrawMap;

   procedure UpdateMapInfo
     (X: Positive := PlayerShip.SkyX; Y: Positive := PlayerShip.SkyY) is
      MapInfoText, EventInfoText: Unbounded_String;
      MapInfo, EventInfo: Ttk_Label;
   begin
      MapInfo.Interp := Get_Context;
      MapInfo.Name := New_String(".paned.mapframe.info.info");
      EventInfo.Interp := Get_Context;
      EventInfo.Name := New_String(".paned.mapframe.info.eventinfo");
      Append
        (MapInfoText, "X:" & Positive'Image(X) & " Y:" & Positive'Image(Y));
      if PlayerShip.SkyX /= X or PlayerShip.SkyY /= Y then
         declare
            Distance: constant Positive := CountDistance(X, Y);
         begin
            Append(MapInfoText, LF & "Distance:" & Positive'Image(Distance));
            TravelInfo(MapInfoText, Distance);
         end;
      end if;
      if SkyMap(X, Y).BaseIndex > 0 then
         declare
            BaseIndex: constant Positive := SkyMap(X, Y).BaseIndex;
         begin
            if SkyBases(BaseIndex).Known then
               Append(MapInfoText, LF);
               Append(MapInfoText, "Base info:");
               Append(MapInfoText, LF);
               Append
                 (MapInfoText,
                  To_Unbounded_String("Name: ") & SkyBases(BaseIndex).Name);
            end if;
            if SkyBases(BaseIndex).Visited.Year > 0 then
               Append(MapInfoText, LF);
               Append
                 (MapInfoText,
                  "Type: " &
                  To_String
                    (BasesTypes_List(SkyBases(BaseIndex).BaseType).Name));
               if SkyBases(BaseIndex).Population > 0 then
                  Append(MapInfoText, LF);
               end if;
               if SkyBases(BaseIndex).Population > 0 and
                 SkyBases(BaseIndex).Population < 150 then
                  Append(MapInfoText, "Population: small");
               elsif SkyBases(BaseIndex).Population > 149 and
                 SkyBases(BaseIndex).Population < 300 then
                  Append(MapInfoText, "Population: medium");
               elsif SkyBases(BaseIndex).Population > 299 then
                  Append(MapInfoText, "Population: large");
               end if;
               Append
                 (MapInfoText,
                  LF & "Size: " &
                  To_Lower(Bases_Size'Image(SkyBases(BaseIndex).Size)) & LF);
               if SkyBases(BaseIndex).Population > 0 then
                  Append
                    (MapInfoText,
                     "Owner: " &
                     To_String(Factions_List(SkyBases(BaseIndex).Owner).Name));
               else
                  Append(MapInfoText, "Base is abandoned");
               end if;
               if SkyBases(BaseIndex).Population > 0 then
                  Append(MapInfoText, LF);
                  case SkyBases(BaseIndex).Reputation(1) is
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
               if BaseIndex = PlayerShip.HomeBase then
                  Append(MapInfoText, LF);
                  Append(MapInfoText, "It is your home base");
               end if;
            end if;
         end;
      end if;
      if SkyMap(X, Y).EventIndex > 0 then
         declare
            EventIndex: constant Positive := SkyMap(X, Y).EventIndex;
         begin
            if Events_List(EventIndex).EType /= BaseRecovery and
              SkyMap(X, Y).BaseIndex > 0 then
               Append(EventInfoText, LF);
            end if;
            case Events_List(EventIndex).EType is
               when EnemyShip | Trader | FriendlyShip =>
                  Append
                    (EventInfoText,
                     ProtoShips_List(Events_List(EventIndex).ShipIndex).Name);
               when FullDocks =>
                  Append(EventInfoText, "Full docks in base");
               when AttackOnBase =>
                  Append(EventInfoText, "Base is under attack");
               when Disease =>
                  Append(EventInfoText, "Disease in base");
               when EnemyPatrol =>
                  Append(EventInfoText, "Enemy patrol");
               when DoublePrice =>
                  Append
                    (EventInfoText,
                     "Double price for " &
                     To_String
                       (Items_List(Events_List(EventIndex).ItemIndex).Name));
               when None | BaseRecovery =>
                  null;
            end case;
            if
              (Events_List(EventIndex).EType = DoublePrice or
               Events_List(EventIndex).EType = FriendlyShip) then
               configure
                 (EventInfo,
                  "-text {" & To_String(EventInfoText) &
                  "} -style Headergreen.TLabel");
            else
               configure
                 (EventInfo,
                  "-text {" & To_String(EventInfoText) &
                  "} -style Headerred.TLabel");
            end if;
         end;
      end if;
      if SkyMap(X, Y).MissionIndex > 0 then
         declare
            MissionIndex: constant Positive := SkyMap(X, Y).MissionIndex;
         begin
            Append(MapInfoText, LF);
            if SkyMap(X, Y).BaseIndex > 0 or SkyMap(X, Y).EventIndex > 0 then
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
                       (ProtoShips_List
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
            StoryX, StoryY: Integer := 0;
            FinishCondition: StepConditionType;
         begin
            GetStoryLocation(StoryX, StoryY);
            if StoryX = PlayerShip.SkyX and StoryY = PlayerShip.SkyY then
               StoryX := 0;
               StoryY := 0;
            end if;
            if X = StoryX and Y = StoryY then
               if CurrentStory.CurrentStep = 0 then
                  FinishCondition :=
                    Stories_List(CurrentStory.Index).StartingStep
                      .FinishCondition;
               elsif CurrentStory.CurrentStep > 0 then
                  FinishCondition :=
                    Stories_List(CurrentStory.Index).Steps
                      (CurrentStory.CurrentStep)
                      .FinishCondition;
               else
                  FinishCondition :=
                    Stories_List(CurrentStory.Index).FinalStep.FinishCondition;
               end if;
               if FinishCondition = ASKINBASE or
                 FinishCondition = DESTROYSHIP or
                 FinishCondition = EXPLORE then
                  Append(MapInfoText, LF & "Story leads you here");
               end if;
            end if;
         end;
      end if;
      if X = PlayerShip.SkyX and Y = PlayerShip.SkyY then
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
      Speedbox: Ttk_ComboBox;
   begin
      Button.Interp := Get_Context;
      Speedbox.Interp := Get_Context;
      Speedbox.Name := New_String(".paned.controls.buttons.speed");
      if PlayerShip.Speed = DOCKED then
         Tcl.Tk.Ada.Grid.Grid_Remove(Speedbox);
         Button.Name := New_String(".paned.controls.buttons.moveto");
         Tcl.Tk.Ada.Grid.Grid_Remove(Button);
         Button.Name := New_String(".paned.controls.buttons.wait");
         configure(Button, "-text Wait");
         Add(Button, "Wait 1 minute.");
         for ButtonName of MoveButtonsNames loop
            Button.Name :=
              New_String(".paned.controls.buttons." & To_String(ButtonName));
            State(Button, "disabled");
            Add
              (Button,
               "You must give order 'Undock' from Ship Orders first to move ship.");
         end loop;
      else
         Current(Speedbox, Natural'Image(ShipSpeed'Pos(PlayerShip.Speed) - 1));
         Tcl.Tk.Ada.Grid.Grid(Speedbox);
         if PlayerShip.DestinationX > 0 and PlayerShip.DestinationY > 0 then
            Button.Name := New_String(".paned.controls.buttons.moveto");
            Tcl.Tk.Ada.Grid.Grid(Button);
            Button.Name := New_String(".paned.controls.buttons.wait");
            configure(Button, "-text Move");
            Add(Button, "Move ship one map field toward destination.");
            Tcl.Tk.Ada.Grid.Grid(Button);
         else
            Button.Name := New_String(".paned.controls.buttons.moveto");
            Tcl.Tk.Ada.Grid.Grid_Remove(Button);
            Button.Name := New_String(".paned.controls.buttons.wait");
            configure(Button, "-text Wait");
            Add(Button, "Wait 1 minute.");
         end if;
         for I in MoveButtonsNames'Range loop
            Button.Name :=
              New_String
                (".paned.controls.buttons." & To_String(MoveButtonsNames(I)));
            State(Button, "!disabled");
            Add(Button, To_String(MoveButtonsTooltips(I)));
         end loop;
      end if;
   end UpdateMoveButtons;

   procedure CreateGameUI is
      Paned: Ttk_PanedWindow;
      Button: Ttk_Button;
      SteamSky_Map_Error: exception;
      Header, MessagesFrame: Ttk_Frame;
   begin
      GameMenu.Interp := Get_Context;
      GameMenu.Name := New_String(".gamemenu");
      MapView.Interp := Get_Context;
      MapView.Name := New_String(".paned.mapframe.map");
      if Winfo_Get(GameMenu, "exists") = "0" then
         Font.Configure
           ("MapFont", "-size" & Positive'Image(GameSettings.MapFontSize));
         Font.Configure
           ("InterfaceFont",
            "-size" & Positive'Image(GameSettings.InterfaceFontSize));
         Font.Configure
           ("HelpFont", "-size" & Positive'Image(GameSettings.HelpFontSize));
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "game.tcl");
         OrdersMenu.AddCommands;
         Maps.UI.Commands.AddCommands;
         WaitMenu.AddCommands;
         Help.UI.AddCommands;
         Ships.Cargo.UI.AddCommands;
         Ships.UI.AddCommands;
         Crew.UI.AddCommands;
         Crafts.UI.AddCommands;
         Messages.UI.AddCommands;
         BasesList.AddCommands;
         GameOptions.AddCommands;
         Bind(MapView, "<Configure>", "DrawMap");
         Bind(MapView, "<Motion>", "{UpdateMapInfo %x %y}");
         Bind(MapView, "<1>", "{ShowDestinationMenu %x %y}");
      end if;
      Wm_Set(Get_Main_Window(Get_Context), "title", "{Steam Sky}");
      CreateGameMenu;
      for I in MenuAccelerators'Range loop
         Bind_To_Main_Window
           (Get_Context, "<" & To_String(MenuAccelerators(I)) & ">",
            "{.gamemenu invoke" & Positive'Image(I) & "}");
      end loop;
      if Index
          (Tcl.Tk.Ada.Pack.Pack_Slaves(Get_Main_Window(Get_Context)),
           ".header") =
        0 then
         Header.Interp := Get_Context;
         Header.Name := New_String(".header");
         Tcl.Tk.Ada.Pack.Pack(Header);
      end if;
      UpdateHeader;
      CenterX := PlayerShip.SkyX;
      CenterY := PlayerShip.SkyY;
      for I in BasesTypes_List.Iterate loop
         Tag_Configure
           (MapView, To_String(BasesTypes_Container.Key(I)),
            "-foreground #" & BasesTypes_List(I).Color);
      end loop;
      Paned.Interp := Get_Context;
      Paned.Name := New_String(".paned");
      SashPos(Paned, "0", Natural'Image(GameSettings.MessagesPosition));
      if Index
          (Tcl.Tk.Ada.Pack.Pack_Slaves(Get_Main_Window(Get_Context)),
           ".paned") =
        0 then
         Tcl.Tk.Ada.Pack.Pack(Paned);
      end if;
      UpdateMapInfo;
      Button.Interp := Get_Context;
      Button.Name := New_String(".paned.mapframe.buttons.hide");
      if Invoke(Button) /= "" then
         raise SteamSky_Map_Error with "Can't hide map buttons";
      end if;
      Bind_To_Main_Window
        (Get_Context, "<Escape>", "{InvokeButton .header.closebutton}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(1)) & ">",
         "{tk_popup " & Widget_Image(GameMenu) & " %X %Y}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(2)) & ">",
         "{.paned.mapframe.buttons.wait invoke}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(3)) & ">",
         "{ZoomMap raise}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(4)) & ">",
         "{ZoomMap lower}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(5)) & ">",
         "{InvokeButton $bframe.nw}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(6)) & ">",
         "{InvokeButton $bframe.n}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(7)) & ">",
         "{InvokeButton $bframe.ne}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(8)) & ">",
         "{InvokeButton $bframe.w}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(9)) & ">",
         "{InvokeButton $bframe.wait}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(10)) & ">",
         "{InvokeButton $bframe.e}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(11)) & ">",
         "{InvokeButton $bframe.sw}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(12)) & ">",
         "{InvokeButton $bframe.s}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(13)) & ">",
         "{InvokeButton $bframe.se}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(14)) & ">",
         "{InvokeButton $bframe.moveto}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(15)) & ">",
         "{MoveMap centeronship}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(16)) & ">",
         "{MoveMap centeronhome}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(17)) & ">",
         "{MoveMap nw}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(18)) & ">",
         "{MoveMap n}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(19)) & ">",
         "{MoveMap ne}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(20)) & ">",
         "{MoveMap w}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(21)) & ">",
         "{MoveMap e}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(22)) & ">",
         "{MoveMap sw}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(23)) & ">",
         "{MoveMap s}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(24)) & ">",
         "{MoveMap se}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(25)) & ">",
         "{MoveCursor nw %x %y}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(26)) & ">",
         "{MoveCursor n %x %y}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(27)) & ">",
         "{MoveCursor ne %x %y}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(28)) & ">",
         "{MoveCursor w %x %y}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(29)) & ">",
         "{MoveCursor e %x %y}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(30)) & ">",
         "{MoveCursor sw %x %y}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(31)) & ">",
         "{MoveCursor s %x %y}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(32)) & ">",
         "{MoveCursor se %x %y}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(33)) & ">",
         "{MoveCursor click %x %y}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(34)) & ">",
         "{.paned.controls.buttons.speed current 0}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(35)) & ">",
         "{.paned.controls.buttons.speed current 1}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(36)) & ">",
         "{.paned.controls.buttons.speed current 2}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(MapAccelerators(37)) & ">",
         "{.paned.controls.buttons.speed current 3}");
      Bind_To_Main_Window
        (Get_Context, "<" & To_String(FullScreenAccel) & ">",
         "{ToggleFullScreen}");
      UpdateMessages;
      UpdateMoveButtons;
      if not GameSettings.ShowLastMessages then
         MessagesFrame.Interp := Get_Context;
         MessagesFrame.Name := New_String(".paned.controls.messages");
         Tcl.Tk.Ada.Grid.Grid_Remove(MessagesFrame);
      end if;
   end CreateGameUI;

   procedure ShowSkyMap(Clear: Boolean := False) is
   begin
      if Clear then
         ShowScreen("mapframe");
      end if;
      UpdateHeader;
      DrawMap;
      UpdateMessages;
      UpdateMoveButtons;
   end ShowSkyMap;

end Maps.UI;
