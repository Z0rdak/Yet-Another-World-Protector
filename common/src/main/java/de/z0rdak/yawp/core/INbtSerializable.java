package de.z0rdak.yawp.core;

import net.minecraft.nbt.Tag;

public interface INbtSerializable<T extends Tag> {
    T serializeNBT();

    void deserializeNBT(T nbt);
}
