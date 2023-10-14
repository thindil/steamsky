--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with GNATtest_Generated;

package Crew.Mob_Record_Test_Data is

   type Mob_Record_Access is
     access all GNATtest_Generated.GNATtest_Standard.Crew.Mob_Record'Class;

--  begin read only
   type Test_Mob_Record is abstract new AUnit.Test_Fixtures.Test_Fixture
--  end read only
      with
   record
      Fixture: Mob_Record_Access;
   end record;

end Crew.Mob_Record_Test_Data;
