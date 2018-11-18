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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with DOM.Core; use DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Nodes; use DOM.Core.Nodes;
with DOM.Core.Elements; use DOM.Core.Elements;
with Game; use Game;
with Log; use Log;

package body ShipModules is

   procedure LoadShipModules(Reader: Tree_Reader) is
      NodesList: Node_List;
      ModulesData: Document;
      TempRecord: BaseModule_Data;
      Action: DataAction;
      ModuleNode: Node;
      ModuleIndex: Natural;
   begin
      TempRecord :=
        (Name => Null_Unbounded_String, MType => ENGINE, Weight => 0,
         Value => 0, MaxValue => 0, Durability => 0,
         RepairMaterial => Null_Unbounded_String, RepairSkill => 2, Price => 0,
         InstallTime => 60, Unique => False, Size => 0,
         Description => Null_Unbounded_String, Index => Null_Unbounded_String);
      ModulesData := Get_Tree(Reader);
      NodesList :=
        DOM.Core.Documents.Get_Elements_By_Tag_Name(ModulesData, "module");
      for I in 0 .. Length(NodesList) - 1 loop
         ModuleNode := Item(NodesList, I);
         TempRecord.Index :=
           To_Unbounded_String(Get_Attribute(ModuleNode, "index"));
         if Get_Attribute(ModuleNode, "action")'Length > 0 then
            Action := DataAction'Value(Get_Attribute(ModuleNode, "action"));
         else
            Action := ADD;
         end if;
         ModuleIndex := FindProtoModule(TempRecord.Index);
         if (Action = UPDATE or Action = REMOVE) then
            if ModuleIndex = 0 then
               raise Data_Loading_Error
                 with "Can't " & To_Lower(DataAction'Image(Action)) &
                 " ship module '" & To_String(TempRecord.Index) &
                 "', there no ship module with that index.";
            end if;
         elsif ModuleIndex > 0 then
            raise Data_Loading_Error
              with "Can't add ship module '" & To_String(TempRecord.Index) &
              "', there is one with that index.";
         end if;
         if Action /= REMOVE then
            if Action = UPDATE then
               TempRecord := Modules_List(ModuleIndex);
            end if;
            if Get_Attribute(ModuleNode, "name")'Length > 0 then
               TempRecord.Name :=
                 To_Unbounded_String(Get_Attribute(ModuleNode, "name"));
            end if;
            if Get_Attribute(ModuleNode, "type")'Length > 0 then
               TempRecord.MType :=
                 ModuleType'Value(Get_Attribute(ModuleNode, "type"));
            end if;
            if Get_Attribute(ModuleNode, "weight")'Length > 0 then
               TempRecord.Weight :=
                 Natural'Value(Get_Attribute(ModuleNode, "weight"));
            end if;
            if Get_Attribute(ModuleNode, "value")'Length > 0 then
               TempRecord.Value :=
                 Integer'Value(Get_Attribute(ModuleNode, "value"));
            end if;
            if Get_Attribute(ModuleNode, "maxvalue")'Length > 0 then
               TempRecord.MaxValue :=
                 Integer'Value(Get_Attribute(ModuleNode, "maxvalue"));
            end if;
            if Get_Attribute(ModuleNode, "durability")'Length > 0 then
               TempRecord.Durability :=
                 Integer'Value(Get_Attribute(ModuleNode, "durability"));
            end if;
            if Get_Attribute(ModuleNode, "material")'Length > 0 then
               TempRecord.RepairMaterial :=
                 To_Unbounded_String(Get_Attribute(ModuleNode, "material"));
            end if;
            if Get_Attribute(ModuleNode, "skill")'Length > 0 then
               TempRecord.RepairSkill :=
                 FindSkillIndex
                   (To_Unbounded_String(Get_Attribute(ModuleNode, "skill")));
            end if;
            if Get_Attribute(ModuleNode, "price")'Length > 0 then
               TempRecord.Price :=
                 Integer'Value(Get_Attribute(ModuleNode, "price"));
            end if;
            if Get_Attribute(ModuleNode, "installtime")'Length > 0 then
               TempRecord.InstallTime :=
                 Positive'Value(Get_Attribute(ModuleNode, "installtime"));
            end if;
            if Get_Attribute(ModuleNode, "unique") /= "" then
               TempRecord.Unique := True;
            end if;
            if Get_Attribute(ModuleNode, "size") /= "" then
               TempRecord.Size :=
                 Integer'Value(Get_Attribute(ModuleNode, "size"));
            end if;
            if Has_Child_Nodes(ModuleNode) then
               TempRecord.Description :=
                 To_Unbounded_String(Node_Value(First_Child(ModuleNode)));
            end if;
            if Action /= UPDATE then
               Modules_List.Append(New_Item => TempRecord);
               LogMessage
                 ("Module added: " & To_String(TempRecord.Name), Everything);
            else
               Modules_List(ModuleIndex) := TempRecord;
               LogMessage
                 ("Module updated: " & To_String(TempRecord.Name), Everything);
            end if;
         else
            Modules_List.Delete(Index => ModuleIndex);
            LogMessage
              ("Module removed: " & To_String(TempRecord.Index), Everything);
         end if;
         TempRecord :=
           (Name => Null_Unbounded_String, MType => ENGINE, Weight => 0,
            Value => 0, MaxValue => 0, Durability => 0,
            RepairMaterial => Null_Unbounded_String, RepairSkill => 2,
            Price => 0, InstallTime => 60, Unique => False, Size => 0,
            Description => Null_Unbounded_String,
            Index => Null_Unbounded_String);
      end loop;
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
