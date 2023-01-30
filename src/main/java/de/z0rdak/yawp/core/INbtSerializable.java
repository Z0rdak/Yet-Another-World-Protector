package de.z0rdak.yawp.core;

import net.minecraft.nbt.NbtElement;

public interface INbtSerializable<T extends NbtElement> {
    T serializeNBT();

    void deserializeNBT(T nbt);
}
