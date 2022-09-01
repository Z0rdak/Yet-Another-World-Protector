package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.HashMap;
import java.util.Set;

public class FlagContainer extends HashMap<String, IFlag> implements INBTSerializable<CompoundNBT> {


    public FlagContainer(CompoundNBT nbt){
        this();
        this.deserializeNBT(nbt);
    }

    public FlagContainer(){
        super();
    }

    public FlagContainer(IFlag flag) {
        this();
        this.put(flag);
    }

    public FlagContainer(Set<IFlag> flags) {
        this();
        flags.forEach(this::put);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        this.forEach((key, value) -> nbt.put(key, value.serializeNBT()));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        Set<String> flagKeys = nbt.getAllKeys();
        flagKeys.forEach( key -> {
            CompoundNBT flag = nbt.getCompound(key);
            this.put(key, new ConditionFlag(flag));
        });
    }

    public void put(IFlag flag){
        this.put(flag.getFlagIdentifier(), flag);
    }

    public boolean contains(IFlag flag){
        return this.containsKey(flag.getFlagIdentifier());
    }
}
