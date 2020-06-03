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

with Ada.Strings.UTF_Encoding.Wide_Strings;
use Ada.Strings.UTF_Encoding.Wide_Strings;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Tcl.Ada; use Tcl.Ada;
with Tcl.Tk.Ada; use Tcl.Tk.Ada;
with Tcl.Tk.Ada.Grid;
with Tcl.Tk.Ada.Widgets; use Tcl.Tk.Ada.Widgets;
with Tcl.Tk.Ada.Widgets.Menu; use Tcl.Tk.Ada.Widgets.Menu;
with Tcl.Tk.Ada.Widgets.Text; use Tcl.Tk.Ada.Widgets.Text;
with Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
use Tcl.Tk.Ada.Widgets.Toplevel.MainWindow;
with Tcl.Tk.Ada.Widgets.TtkButton; use Tcl.Tk.Ada.Widgets.TtkButton;
with Tcl.Tk.Ada.Widgets.TtkFrame; use Tcl.Tk.Ada.Widgets.TtkFrame;
with Tcl.Tk.Ada.Widgets.TtkLabel; use Tcl.Tk.Ada.Widgets.TtkLabel;
with Tcl.Tk.Ada.Winfo; use Tcl.Tk.Ada.Winfo;
with Tcl.Tklib.Ada.Tooltip; use Tcl.Tklib.Ada.Tooltip;
with Config; use Config;
with Crew; use Crew;
with Events; use Events;
with Factions; use Factions;
with Game; use Game;
with Messages; use Messages;
with ShipModules; use ShipModules;
with OrdersMenu;
with Ships; use Ships;
with Ships.Cargo; use Ships.Cargo;
with Ships.Movement; use Ships.Movement;
with Stories; use Stories;

package body Maps.UI is

   -- ****iv* MUI/GameMenu
   -- FUNCTION
   -- The main game menu
   -- SOURCE
   GameMenu: Tk_Menu;
   -- ****

   -- ****if* MUI/CreateGameMenu
   -- FUNCTION
   -- Create the main game menu. Clear old elements and add all default
   -- SOURCE
   procedure CreateGameMenu is
      -- ****
   begin
      Delete(GameMenu, "0", "end");
      Menu.Add(GameMenu, "command", "-label {Ship information}");
      Menu.Add(GameMenu, "command", "-label {Ship cargo}");
      Menu.Add(GameMenu, "command", "-label {Crew information}");
      Menu.Add
        (GameMenu, "command", "-label {Ship orders} -command {ShowOrders}");
      Menu.Add(GameMenu, "command", "-label {Crafting}");
      Menu.Add(GameMenu, "command", "-label {Last messages}");
      Menu.Add(GameMenu, "command", "-label {List of known bases}");
      Menu.Add(GameMenu, "command", "-label {List of known events}");
      Menu.Add(GameMenu, "command", "-label {Accepted missions}");
      Menu.Add(GameMenu, "command", "-label {Stories}");
      Menu.Add(GameMenu, "command", "-label {Wait orders}");
      Menu.Add(GameMenu, "command", "-label {Game statistics}");
      Menu.Add(GameMenu, "command", "-label {Help}");
      Menu.Add(GameMenu, "command", "-label {Game options}");
      Menu.Add(GameMenu, "command", "-label {Quit from game}");
      Menu.Add(GameMenu, "command", "-label {Resign from game}");
   end CreateGameMenu;

   -- ****if* MUI/DeathConfirm
   -- FUNCTION
   -- Show stats or go to main menu on player character death
   -- TODO
   -- Implement it
   -- SOURCE
   procedure DeathConfirm is
      -- ****
   begin
      null;
   end DeathConfirm;

   -- ****if* MUI/UpdateHeader
   -- FUNCTION
   -- Update the game information on the UI header (time, crew, etc)
   -- SOURCE
   procedure UpdateHeader is
      -- ****
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

   -- ****iv* MUI/CenterX
   -- FUNCTION
   -- X coordinate of the center point of the map
   -- SOURCE
   CenterX: Positive;
   -- ****

   -- ****iv* MUI/CenterY
   -- FUNCTION
   -- Y coordinate of the center point of the map
   -- SOURCE
   CenterY: Positive;
   -- ****

   -- ****if* MUI/DrawMap
   -- FUNCTION
   -- Draw map on the screen
   -- SOURCE
   procedure DrawMap is
      -- ****
      MapView: Tk_Text;
      MapChar: Wide_Character;
      StartX, StartY, EndX, EndY: Integer;
      MapHeight, MapWidth: Positive;
      MapTag: Unbounded_String;
      StoryX, StoryY: Integer := 0;
   begin
      MapView.Interp := Get_Context;
      MapView.Name := New_String(".paned.mapframe.map");
      MapHeight := Positive'Value(cget(Mapview, "-height"));
      MapWidth := Positive'Value(cget(Mapview, "-width"));
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
            if X = PlayerShip.SkyX and Y = PlayerShip.SkyY then
               MapChar := Wide_Character'Val(16#f135#);
            else
               MapChar := Wide_Character'Val(16#f0c8#);
               if SkyMap(X, Y).Visited then
                  MapTag := To_Unbounded_String("black");
               else
                  MapTag := To_Unbounded_String("unvisited gray");
               end if;
--               if X = PlayerShip.DestinationX and
--                 Y = PlayerShip.DestinationY then
--                  MapChar := CurrentTheme.TargetIcon;
--                  if SkyMap(X, Y).Visited then
--                     MapColor := WhiteColor;
--                  else
--                     MapColor := WhiteGrayColor;
--                  end if;
--               elsif X = StoryX and Y = StoryY then
--                  MapChar := CurrentTheme.StoryIcon;
--                  MapColor := GreenColor;
--               elsif SkyMap(X, Y).MissionIndex > 0 then
--                  case AcceptedMissions(SkyMap(X, Y).MissionIndex).MType is
--                     when Deliver =>
--                        MapChar := CurrentTheme.DeliverIcon;
--                     when Destroy =>
--                        MapChar := CurrentTheme.DestroyIcon;
--                     when Patrol =>
--                        MapChar := CurrentTheme.PatrolIcon;
--                     when Explore =>
--                        MapChar := CurrentTheme.ExploreIcon;
--                     when Passenger =>
--                        MapChar := CurrentTheme.PassengerIcon;
--                  end case;
--                  if SkyMap(X, Y).Visited then
--                     case AcceptedMissions(SkyMap(X, Y).MissionIndex).MType is
--                        when Deliver =>
--                           MapColor := YellowColor;
--                        when Destroy =>
--                           MapColor := RedColor;
--                        when Patrol =>
--                           MapColor := LimeColor;
--                        when Explore =>
--                           MapColor := GreenColor;
--                        when Passenger =>
--                           MapColor := CyanColor;
--                     end case;
--                  else
--                     case AcceptedMissions(SkyMap(X, Y).MissionIndex).MType is
--                        when Deliver =>
--                           MapColor := YellowGrayColor;
--                        when Destroy =>
--                           MapColor := RedGrayColor;
--                        when Patrol =>
--                           MapColor := LimeGrayColor;
--                        when Explore =>
--                           MapColor := GreenGrayColor;
--                        when Passenger =>
--                           MapColor := CyanGrayColor;
--                     end case;
--                  end if;
--               elsif SkyMap(X, Y).EventIndex > 0 then
--                  if SkyMap(X, Y).EventIndex > Events_List.Last_Index then
--                     SkyMap(X, Y).EventIndex := 0;
--                  else
--                     case Events_List(SkyMap(X, Y).EventIndex).EType is
--                        when EnemyShip =>
--                           MapChar := CurrentTheme.EnemyShipIcon;
--                        when AttackOnBase =>
--                           MapChar := CurrentTheme.AttackOnBaseIcon;
--                        when EnemyPatrol =>
--                           MapChar := CurrentTheme.EnemyPatrolIcon;
--                        when Disease =>
--                           MapChar := CurrentTheme.DiseaseIcon;
--                        when FullDocks =>
--                           MapChar := CurrentTheme.FullDocksIcon;
--                        when DoublePrice =>
--                           MapChar := CurrentTheme.DoublePriceIcon;
--                        when Trader =>
--                           MapChar := CurrentTheme.TraderIcon;
--                        when FriendlyShip =>
--                           MapChar := CurrentTheme.FriendlyShipIcon;
--                        when others =>
--                           null;
--                     end case;
--                  end if;
--                  if SkyMap(X, Y).Visited then
--                     case Events_List(SkyMap(X, Y).EventIndex).EType is
--                        when EnemyShip =>
--                           MapColor := RedColor;
--                        when AttackOnBase =>
--                           MapColor := Red2Color;
--                        when EnemyPatrol =>
--                           MapColor := Red3Color;
--                        when Disease =>
--                           MapColor := YellowColor;
--                        when FullDocks =>
--                           MapColor := CyanColor;
--                        when DoublePrice =>
--                           MapColor := LimeColor;
--                        when Trader =>
--                           MapColor := GreenColor;
--                        when FriendlyShip =>
--                           MapColor := Green2Color;
--                        when others =>
--                           null;
--                     end case;
--                  else
--                     case Events_List(SkyMap(X, Y).EventIndex).EType is
--                        when EnemyShip =>
--                           MapColor := RedGrayColor;
--                        when AttackOnBase =>
--                           MapColor := Red2GrayColor;
--                        when EnemyPatrol =>
--                           MapColor := Red3GrayColor;
--                        when Disease =>
--                           MapColor := YellowGrayColor;
--                        when FullDocks =>
--                           MapColor := CyanGrayColor;
--                        when DoublePrice =>
--                           MapColor := LimeGrayColor;
--                        when Trader =>
--                           MapColor := GreenGrayColor;
--                        when FriendlyShip =>
--                           MapColor := Green2GrayColor;
--                        when others =>
--                           null;
--                     end case;
--                  end if;
--               elsif SkyMap(X, Y).BaseIndex > 0 then
--                  if SkyBases(SkyMap(X, Y).BaseIndex).Known then
--                     MapChar := CurrentTheme.NotVisitedBaseIcon;
--                     if SkyBases(SkyMap(X, Y).BaseIndex).Visited.Year > 0 then
--                        MapChar :=
--                          Factions_List(SkyBases(SkyMap(X, Y).BaseIndex).Owner)
--                            .BaseIcon;
--                        MapColor :=
--                          Lookup
--                            (Tags,
--                             To_String
--                               (SkyBases(SkyMap(X, Y).BaseIndex).BaseType));
--                        if MapColor = null then
--                           Gtk_New
--                             (MapColor,
--                              To_String
--                                (SkyBases(SkyMap(X, Y).BaseIndex).BaseType));
--                           Parse
--                             (Color,
--                              "#" &
--                              BasesTypes_List
--                                (SkyBases(SkyMap(X, Y).BaseIndex).BaseType)
--                                .Color,
--                              Success);
--                           if Success then
--                              Set_Property
--                                (GObject(MapColor), Foreground_Rgba_Property,
--                                 Color);
--                              Set_Property
--                                (GObject(MapColor), Background_Rgba_Property,
--                                 Black_RGBA);
--                           end if;
--                           Add(Tags, MapColor);
--                        end if;
--                     else
--                        MapColor := WhiteGrayColor;
--                     end if;
--                  end if;
--               end if;
            end if;
            Insert(MapView, "end", Encode("" & MapChar) & " [list " & To_String(MapTag) & "]");
--            Insert_With_Tags(MapBuffer, Iter, Encode("" & MapChar), MapColor);
         end loop;
--         if Y < EndY then
--            Insert(MapBuffer, Iter, "" & LF);
--         end if;
      end loop;
--      if TextWindow /= null then
--         Set_Cursor(TextWindow, Gdk_Cursor_New(Cross));
--      end if;
   end DrawMap;

   procedure CreateGameUI is
   begin
      GameMenu.Interp := Get_Context;
      GameMenu.Name := New_String(".gamemenu");
      if Winfo_Get(GameMenu, "exists") = "0" then
         Tcl_EvalFile
           (Get_Context,
            To_String(DataDirectory) & "ui" & Dir_Separator & "game.tcl");
         OrdersMenu.AddCommands;
      end if;
      CreateGameMenu;
      for I in MenuAccelerators'Range loop
         Entry_Configure
           (GameMenu, Positive'Image(I),
            "-accelerator {" & To_String(MenuAccelerators(I)) & "}");
         Bind_To_Main_Window
           (Get_Context, "<" & To_String(MenuAccelerators(I)) & ">",
            "{.gamemenu invoke" & Positive'Image(I) & "}");
      end loop;
      UpdateHeader;
      CenterX := PlayerShip.SkyX;
      CenterY := PlayerShip.SkyY;
      DrawMap;
   end CreateGameUI;

end Maps.UI;
