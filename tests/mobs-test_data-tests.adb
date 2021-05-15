--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mobs.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Items; use Items;

--  begin read only
--  end read only
package body Mobs.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only
--  begin read only
   function Wrap_Test_GenerateMob_520182_3c2c38 (MobIndex, FactionIndex: Unbounded_String)  return Member_Data
   is
   begin
      begin
         pragma Assert
           ((ProtoMobs_List.Contains(MobIndex) and Factions_List.Contains(FactionIndex)));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(mobs.ads:0):Test_GenearateMob test requirement violated");
      end;
      declare
         Test_GenerateMob_520182_3c2c38_Result : constant Member_Data := GNATtest_Generated.GNATtest_Standard.Mobs.GenerateMob (MobIndex, FactionIndex);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(mobs.ads:0:):Test_GenearateMob test commitment violated");
         end;
         return Test_GenerateMob_520182_3c2c38_Result;
      end;
   end Wrap_Test_GenerateMob_520182_3c2c38;
--  end read only

--  begin read only
   procedure Test_GenerateMob_test_genearatemob (Gnattest_T : in out Test);
   procedure Test_GenerateMob_520182_3c2c38 (Gnattest_T : in out Test) renames Test_GenerateMob_test_genearatemob;
--  id:2.2/5201826c898ff8db/GenerateMob/1/0/test_genearatemob/
   procedure Test_GenerateMob_test_genearatemob (Gnattest_T : in out Test) is
      function GenerateMob (MobIndex, FactionIndex: Unbounded_String) return Member_Data renames Wrap_Test_GenerateMob_520182_3c2c38;
--  end read only

      pragma Unreferenced(Gnattest_T);
      NewMob: Member_Data;

   begin

      NewMob :=
        GenerateMob(To_Unbounded_String("5"), To_Unbounded_String("POLEIS"));
      Assert(NewMob.Attributes(1)(1) = 2, "Failed to generate mob.");
      Assert
        (NewMob.OrderTime = 15,
         "Failed to set order time for the generated mob.");

--  begin read only
   end Test_GenerateMob_test_genearatemob;
--  end read only

--  begin read only
   function Wrap_Test_GetRandomItem_61c13c_8c2473 (ItemsIndexes: UnboundedString_Container.Vector; EquipIndex, HighestLevel, WeaponSkillLevel: Positive; FactionIndex: Unbounded_String)  return Unbounded_String
   is
   begin
      begin
         pragma Assert
           ((EquipIndex < 8 and HighestLevel < 101 and WeaponSkillLevel < 101 and Factions_List.Contains(FactionIndex)));
         null;
      exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "req_sloc(mobs.ads:0):Test_GetRandomItem test requirement violated");
      end;
      declare
         Test_GetRandomItem_61c13c_8c2473_Result : constant Unbounded_String := GNATtest_Generated.GNATtest_Standard.Mobs.GetRandomItem (ItemsIndexes, EquipIndex, HighestLevel, WeaponSkillLevel, FactionIndex);
      begin
         begin
            pragma Assert
              (True);
            null;
         exception
            when System.Assertions.Assert_Failure =>
               AUnit.Assertions.Assert
                 (False,
                  "ens_sloc(mobs.ads:0:):Test_GetRandomItem test commitment violated");
         end;
         return Test_GetRandomItem_61c13c_8c2473_Result;
      end;
   end Wrap_Test_GetRandomItem_61c13c_8c2473;
--  end read only

--  begin read only
   procedure Test_GetRandomItem_test_getrandomitem (Gnattest_T : in out Test);
   procedure Test_GetRandomItem_61c13c_8c2473 (Gnattest_T : in out Test) renames Test_GetRandomItem_test_getrandomitem;
--  id:2.2/61c13cdf12147be3/GetRandomItem/1/0/test_getrandomitem/
   procedure Test_GetRandomItem_test_getrandomitem (Gnattest_T : in out Test) is
      function GetRandomItem (ItemsIndexes: UnboundedString_Container.Vector; EquipIndex, HighestLevel, WeaponSkillLevel: Positive; FactionIndex: Unbounded_String) return Unbounded_String renames Wrap_Test_GetRandomItem_61c13c_8c2473;
--  end read only

      pragma Unreferenced(Gnattest_T);

   begin

      Assert
        (GetRandomItem
           (Weapons_List,
            1,
            20,
            20,
            To_Unbounded_String("POLEIS")) /=
         Null_Unbounded_String,
         "Failed to get random item for mob.");

--  begin read only
   end Test_GetRandomItem_test_getrandomitem;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Mobs.Test_Data.Tests;
