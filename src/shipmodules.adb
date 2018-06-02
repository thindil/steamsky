--    Copyright 2016-2018 Bartek thindil Jasicki
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

with Ada.Directories; use Ada.Directories;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with DOM.Readers; use DOM.Readers;
with Input_Sources.File; use Input_Sources.File;
with Game; use Game;
with Log; use Log;

package body ShipModules is

   procedure LoadShipModules is
      Files: Search_Type;
      FoundFile: Directory_Entry_Type;
      ModulesFile: File_Input;
      Reader: Tree_Reader;
      NodesList: Node_List;
      ModulesData: Document;
      TempRecord: BaseModule_Data;
   begin
      if Modules_List.Length > 0 then
         return;
      end if;
      if not Exists
          (To_String(DataDirectory) & "shipmodules" & Dir_Separator) then
         raise Modules_Directory_Not_Found;
      end if;
      Start_Search
        (Files,
         To_String(DataDirectory) & "shipmodules" & Dir_Separator,
         "*.dat");
      if not More_Entries(Files) then
         raise Modules_Files_Not_Found;
      end if;
      while More_Entries(Files) loop
         Get_Next_Entry(Files, FoundFile);
         TempRecord :=
           (Name => Null_Unbounded_String,
            MType => ENGINE,
            Weight => 0,
            Value => 0,
            MaxValue => 0,
            Durability => 0,
            RepairMaterial => Null_Unbounded_String,
            RepairSkill => 2,
            Price => 0,
            InstallTime => 60,
            Unique => False,
            Size => 0,
            Description => Null_Unbounded_String,
            Index => Null_Unbounded_String);
         LogMessage
           ("Loading ship modules file: " & Full_Name(FoundFile),
            Everything);
         Open(Full_Name(FoundFile), ModulesFile);
         Parse(Reader, ModulesFile);
         Close(ModulesFile);
         ModulesData := Get_Tree(Reader);
         NodesList :=
           DOM.Core.Documents.Get_Elements_By_Tag_Name(ModulesData, "module");
         for I in 0 .. Length(NodesList) - 1 loop
            TempRecord.Index :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "index"));
            TempRecord.Name :=
              To_Unbounded_String(Get_Attribute(Item(NodesList, I), "name"));
            TempRecord.MType :=
              ModuleType'Value(Get_Attribute(Item(NodesList, I), "type"));
            TempRecord.Weight :=
              Natural'Value(Get_Attribute(Item(NodesList, I), "weight"));
            TempRecord.Value :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "value"));
            TempRecord.MaxValue :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "maxvalue"));
            TempRecord.Durability :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "durability"));
            TempRecord.RepairMaterial :=
              To_Unbounded_String
                (Get_Attribute(Item(NodesList, I), "material"));
            for J in Skills_List.Iterate loop
               if Get_Attribute(Item(NodesList, I), "skill") =
                 To_String(Skills_List(J).Name) then
                  TempRecord.RepairSkill := SkillsData_Container.To_Index(J);
                  exit;
               end if;
            end loop;
            TempRecord.Price :=
              Integer'Value(Get_Attribute(Item(NodesList, I), "price"));
            TempRecord.InstallTime :=
              Positive'Value(Get_Attribute(Item(NodesList, I), "installtime"));
            if Get_Attribute(Item(NodesList, I), "unique") /= "" then
               TempRecord.Unique := True;
            end if;
            if Get_Attribute(Item(NodesList, I), "size") /= "" then
               TempRecord.Size :=
                 Integer'Value(Get_Attribute(Item(NodesList, I), "size"));
            end if;
            TempRecord.Description :=
              To_Unbounded_String(Node_Value(First_Child(Item(NodesList, I))));
            LogMessage
              ("Module added: " & To_String(TempRecord.Name),
               Everything);
            Modules_List.Append(New_Item => TempRecord);
            TempRecord :=
              (Name => Null_Unbounded_String,
               MType => ENGINE,
               Weight => 0,
               Value => 0,
               MaxValue => 0,
               Durability => 0,
               RepairMaterial => Null_Unbounded_String,
               RepairSkill => 2,
               Price => 0,
               InstallTime => 60,
               Unique => False,
               Size => 0,
               Description => Null_Unbounded_String,
               Index => Null_Unbounded_String);
         end loop;
         Free(Reader);
      end loop;
      End_Search(Files);
   end LoadShipModules;

   function FindProtoModule(Index: Unbounded_String) return Natural is
   begin
      for I in Modules_List.Iterate loop
         if Modules_List(I).Index = Index then
            return BaseModules_Container.To_Index(I);
         end if;
      end loop;
      return 0;
   end FindProtoModule;

end ShipModules;
