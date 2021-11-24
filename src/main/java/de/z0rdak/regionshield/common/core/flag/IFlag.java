package de.z0rdak.regionshield.common.core.flag;

import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;

public interface IFlag extends INBTSerializable<CompoundNBT> {

    String getFlagName();
    boolean isDefaultValue();
    void allow();
    void deny();
}
