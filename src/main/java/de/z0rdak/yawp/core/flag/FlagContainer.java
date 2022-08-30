package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundTag;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.HashMap;
import java.util.Set;

public class FlagContainer extends HashMap<String, IFlag> implements INBTSerializable<CompoundTag> {


    public FlagContainer(CompoundTag nbt){
        this();
        this.deserializeNBT(nbt);
    }

    public FlagContainer(){
        super();
    }

    public FlagContainer(IFlag flag) {
        this();
        this.put(flag.getFlagName(), flag);
    }

    public FlagContainer(Set<IFlag> flags) {
        this();
        flags.forEach( flag -> this.put(flag.getFlagName(), flag));
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        this.forEach((key, value) -> nbt.put(key, value.serializeNBT()));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        Set<String> flagKeys = nbt.getAllKeys();
        flagKeys.forEach( key -> {
            CompoundTag flag = nbt.getCompound(key);
            this.put(key, new ConditionFlag(flag));
        });
    }
}
