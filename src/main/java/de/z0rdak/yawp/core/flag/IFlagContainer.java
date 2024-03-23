package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.handler.flags.FlagState;

public interface IFlagContainer {

   void put(IFlag flag);

   boolean contains(String flag);

   FlagState flagState(RegionFlag flag);

   void updateFlag(IFlag flag);

   void toggleFlag(String flag, boolean enable);
}
