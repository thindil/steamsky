--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Mobs.ProtoMobRecord_Test_Data is

   --  Local_ProtoMobRecord : aliased GNATtest_Generated.GNATtest_Standard.Mobs.ProtoMobRecord;
   procedure Set_Up(Gnattest_T: in out Test_ProtoMobRecord) is
   begin
      GNATtest_Generated.GNATtest_Standard.Crew.Mob_Record_Test_Data
        .Mob_Record_Tests
        .Set_Up
        (GNATtest_Generated.GNATtest_Standard.Crew.Mob_Record_Test_Data
           .Mob_Record_Tests
           .Test_Mob_Record
           (Gnattest_T));
      null;
      --  Gnattest_T.Fixture := Local_ProtoMobRecord'Access;
   end Set_Up;

   procedure Tear_Down(Gnattest_T: in out Test_ProtoMobRecord) is
   begin
      GNATtest_Generated.GNATtest_Standard.Crew.Mob_Record_Test_Data
        .Mob_Record_Tests
        .Tear_Down
        (GNATtest_Generated.GNATtest_Standard.Crew.Mob_Record_Test_Data
           .Mob_Record_Tests
           .Test_Mob_Record
           (Gnattest_T));
   end Tear_Down;

end Mobs.ProtoMobRecord_Test_Data;
