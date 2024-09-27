package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.nbt.CompoundTag;

import java.util.List;

public interface IFlagContainer extends INbtSerializable<CompoundTag> {

    void put(IFlag flag);

    boolean contains(String flag);

    List<IFlag> getFlags(FlagState state);

    FlagState flagState(String flagName);

    void updateFlag(IFlag flag);

    void toggleFlag(String flag, boolean enable);
}
