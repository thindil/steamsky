--    Copyright 2018-2019 Bartek thindil Jasicki
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Gtk.Css_Provider; use Gtk.Css_Provider;
with Gtk.Style_Context; use Gtk.Style_Context;
with Gdk.Screen; use Gdk.Screen;
with Gdk.Display; use Gdk.Display;
with Glib; use Glib;
with Glib.Error; use Glib.Error;
with Config; use Config;
with Game; use Game;

package body Themes is

   OldProvider: Gtk_Css_Provider;

   function LoadCssText return Unbounded_String is
      CssText: Unbounded_String;
      ThemeFile: File_Type;
   begin
      Open
        (ThemeFile, In_File,
         To_String
           (Themes_List(To_String(GameSettings.InterfaceTheme)).FileName));
      while not End_Of_File(ThemeFile) loop
         Append(CssText, Get_Line(ThemeFile));
      end loop;
      Close(ThemeFile);
      return CssText;
   end LoadCssText;

   procedure SetFontSize(FontType: FontTypes) is
      CssProvider: Gtk_Css_Provider;
      CssText: Unbounded_String := LoadCssText;
      StartIndex, EndIndex: Positive;
      Error: aliased GError;
   begin
      if FontType = HELPFONT or FontType = ALLFONTS then
         StartIndex := Index(CssText, "*#normalfont", 1);
         StartIndex := Index(CssText, "font-size", StartIndex);
         EndIndex := Index(CssText, ";", StartIndex);
         Replace_Slice
           (CssText, StartIndex, EndIndex,
            "font-size:" & Positive'Image(GameSettings.HelpFontSize) & "px;");
      end if;
      if FontType = MAPFONT or FontType = ALLFONTS then
         StartIndex := Index(CssText, "#mapview", 1);
         StartIndex := Index(CssText, "font-size", StartIndex);
         EndIndex := Index(CssText, ";", StartIndex);
         Replace_Slice
           (CssText, StartIndex, EndIndex,
            "font-size:" & Positive'Image(GameSettings.MapFontSize) & "px;");
      end if;
      if FontType = INTERFACEFONT or FontType = ALLFONTS then
         StartIndex := 1;
         StartIndex := Index(CssText, "font-size", StartIndex);
         EndIndex := Index(CssText, ";", StartIndex);
         Replace_Slice
           (CssText, StartIndex, EndIndex,
            "font-size:" & Positive'Image(GameSettings.InterfaceFontSize) &
            "px;");
      end if;
      Gtk_New(CssProvider);
      if not Load_From_Data(CssProvider, To_String(CssText), Error'Access) then
         Put_Line("Error: " & Get_Message(Error));
         return;
      end if;
      if OldProvider /= null then
         Remove_Provider_For_Screen
           (Get_Default_Screen(Get_Default), +(OldProvider));
      end if;
      Add_Provider_For_Screen
        (Get_Default_Screen(Get_Default), +(CssProvider), Guint'Last);
      OldProvider := CssProvider;
   end SetFontSize;

   procedure LoadTheme is
      CssProvider: Gtk_Css_Provider;
      CssText: Unbounded_String := LoadCssText;
      Error: aliased GError;
   begin
      if not GameSettings.ShowTooltips then
         Append(CssText, ".tooltip {opacity:0;}");
      else
         Append(CssText, ".tooltip {opacity:1;}");
      end if;
      Gtk_New(CssProvider);
      if not Load_From_Data(CssProvider, To_String(CssText), Error'Access) then
         Put_Line("Error: " & Get_Message(Error));
         return;
      end if;
      SetFontSize(ALLFONTS);
   end LoadTheme;

   procedure ResetFontsSizes is
      CssText: Unbounded_String := Null_Unbounded_String;
      CssFile: File_Type;
      function GetFontSize(FontName: String) return Positive is
         StartIndex, EndIndex: Positive;
      begin
         StartIndex := Index(CssText, FontName, 1);
         StartIndex := Index(CssText, "font-size", StartIndex);
         StartIndex := Index(CssText, ":", StartIndex) + 1;
         EndIndex := Index(CssText, "p", StartIndex) - 1;
         return Positive'Value(Slice(CssText, StartIndex, EndIndex));
      end GetFontSize;
   begin
      Open
        (CssFile, In_File,
         To_String
           (Themes_List(To_String(GameSettings.InterfaceTheme)).FileName));
      while not End_Of_File(CssFile) loop
         Append(CssText, Get_Line(CssFile));
      end loop;
      Close(CssFile);
      GameSettings.HelpFontSize := GetFontSize("*#normalfont");
      GameSettings.MapFontSize := GetFontSize("#mapview");
      GameSettings.InterfaceFontSize := GetFontSize("* {");
   end ResetFontsSizes;

   procedure LoadThemes is
      Directories, Files: Search_Type;
      FoundDirectory, FoundFile: Directory_Entry_Type;
      ConfigFile: File_Type;
      RawData, FieldName, Value: Unbounded_String;
      EqualIndex: Natural;
      TempRecord: ThemeRecord;
   begin
      Themes_Container.Include
        (Themes_List, "default",
         (Name => To_Unbounded_String("Default theme"),
          Filename =>
            DataDirectory &
            To_Unbounded_String("ui" & Dir_Separator & "steamsky.css")));
      Start_Search
        (Directories, To_String(ThemesDirectory), "",
         (Directory => True, others => False));
      while More_Entries(Directories) loop
         Get_Next_Entry(Directories, FoundDirectory);
         if Simple_Name(FoundDirectory) /= "." and
           Simple_Name(FoundDirectory) /= ".." then
            Start_Search(Files, Full_Name(FoundDirectory), "*.cfg");
            while More_Entries(Files) loop
               Get_Next_Entry(Files, FoundFile);
               Open(ConfigFile, In_File, Full_Name(FoundFile));
               while not End_Of_File(ConfigFile) loop
                  RawData := To_Unbounded_String(Get_Line(ConfigFile));
                  if Length(RawData) > 0 then
                     EqualIndex := Index(RawData, "=");
                     FieldName := Head(RawData, EqualIndex - 2);
                     Value :=
                       Tail(RawData, (Length(RawData) - EqualIndex - 1));
                     if FieldName = To_Unbounded_String("Name") then
                        TempRecord.Name := Value;
                     elsif FieldName = To_Unbounded_String("FileName") then
                        TempRecord.FileName :=
                          To_Unbounded_String
                            (Full_Name(FoundDirectory) & Dir_Separator) &
                          Value;
                     end if;
                  end if;
               end loop;
               Close(ConfigFile);
               Themes_Container.Include
                 (Themes_List, Simple_Name(FoundDirectory), TempRecord);
            end loop;
            End_Search(Files);
         end if;
      end loop;
      End_Search(Directories);
   end LoadThemes;

end Themes;
