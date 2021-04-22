--    Copyright 2017-2021 Bartek thindil Jasicki
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
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Directories;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Game; use Game;
with Statistics; use Statistics;

package body HallOfFame is

   -- ****iv* HallOfFame/HallOfFame.HoFData
   -- FUNCTION
   -- XML structure for save or load hall of fame data from file
   -- SOURCE
   HoFData: Document;
   -- ****

   procedure Load_Hall_Of_Fame is
      HoFFile: File_Input;
      Reader: Tree_Reader;
      EntriesList: Node_List;
      EntryNode: Node;
   begin
      if Hall_Of_Fame_Array(1).Name /= Null_Unbounded_String then
         return;
      end if;
      Open(To_String(Save_Directory) & "halloffame.dat", HoFFile);
      Parse(Reader, HoFFile);
      Close(HoFFile);
      HoFData := Get_Tree(Reader);
      EntriesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(HoFData, "entry");
      Load_Hall_Of_Fame_Loop :
      for I in 0 .. Length(EntriesList) - 1 loop
         EntryNode := Item(EntriesList, I);
         Hall_Of_Fame_Array(I + 1).Name :=
           To_Unbounded_String(Get_Attribute(EntryNode, "name"));
         Hall_Of_Fame_Array(I + 1).Points :=
           Natural'Value(Get_Attribute(EntryNode, "points"));
         Hall_Of_Fame_Array(I + 1).Death_Reason :=
           To_Unbounded_String(Get_Attribute(EntryNode, "Death_Reason"));
      end loop Load_Hall_Of_Fame_Loop;
      Free(Reader);
   exception
      when Ada.Directories.Name_Error =>
         null;
   end Load_Hall_Of_Fame;

   procedure Update_Hall_Of_Fame
     (Player_Name, Death_Reason: Unbounded_String) is
      NewIndex: Natural range 0 .. 10 := 0;
      HoFFile: File_Type;
      HoF: DOM_Implementation;
      EntryNode, MainNode: DOM.Core.Element;
      RawValue: Unbounded_String;
   begin
      Find_New_Index_Loop :
      for I in Hall_Of_Fame_Array'Range loop
         if Hall_Of_Fame_Array(I).Points < GetGamePoints then
            NewIndex := I;
            exit Find_New_Index_Loop;
         end if;
      end loop Find_New_Index_Loop;
      if NewIndex = 0 then
         return;
      end if;
      Move_Hall_Of_Fame_Loop :
      for I in reverse NewIndex .. 9 loop
         Hall_Of_Fame_Array(I + 1) := Hall_Of_Fame_Array(I);
      end loop Move_Hall_Of_Fame_Loop;
      Hall_Of_Fame_Array(NewIndex) :=
        (Name => Player_Name, Points => GetGamePoints,
         Death_Reason => Death_Reason);
      HoFData := Create_Document(HoF);
      MainNode := Create_Element(HoFData, "halloffame");
      MainNode := Append_Child(HoFData, MainNode);
      Update_Hall_Of_Fame_Loop :
      for I in Hall_Of_Fame_Array'Range loop
         if Hall_Of_Fame_Array(I).Name = Null_Unbounded_String then
            exit Update_Hall_Of_Fame_Loop;
         end if;
         EntryNode := Create_Element(HoFData, "entry");
         EntryNode := Append_Child(MainNode, EntryNode);
         Set_Attribute
           (EntryNode, "name", To_String(Hall_Of_Fame_Array(I).Name));
         RawValue :=
           To_Unbounded_String(Integer'Image(Hall_Of_Fame_Array(I).Points));
         Set_Attribute
           (EntryNode, "points", To_String(Trim(RawValue, Ada.Strings.Left)));
         Set_Attribute
           (EntryNode, "Death_Reason",
            To_String(Hall_Of_Fame_Array(I).Death_Reason));
      end loop Update_Hall_Of_Fame_Loop;
      Create(HoFFile, Out_File, To_String(Save_Directory) & "halloffame.dat");
      Write(Stream => Stream(HoFFile), N => HoFData, Pretty_Print => True);
      Close(HoFFile);
   end Update_Hall_Of_Fame;

end HallOfFame;
