--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Crew.Mob_Record_Test_Data.Mob_Record_Tests;

with GNATtest_Generated;

package Bases.Recruit_Data_Test_Data is

--  begin read only
   type Test_Recruit_Data is new GNATtest_Generated.GNATtest_Standard.Crew
     .Mob_Record_Test_Data
     .Mob_Record_Tests
     .Test_Mob_Record
--  end read only
   with
   null record;

   procedure Set_Up(Gnattest_T: in out Test_Recruit_Data);
   procedure Tear_Down(Gnattest_T: in out Test_Recruit_Data);

end Bases.Recruit_Data_Test_Data;
