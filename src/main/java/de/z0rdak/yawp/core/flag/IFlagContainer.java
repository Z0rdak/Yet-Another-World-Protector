package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.nbt.NbtCompound;

public interface IFlagContainer extends INbtSerializable<NbtCompound> {

    void put(IFlag flag);

    boolean contains(String flag);

    FlagState flagState(String flagName);

    void updateFlag(IFlag flag);

    void toggleFlag(String flag, boolean enable);
}
