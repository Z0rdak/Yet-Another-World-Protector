package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundTag;

import java.util.HashSet;
import java.util.Set;

import static de.z0rdak.yawp.core.flag.FlagType.*;

public class ListFlag extends AbstractFlag {

    public Set<String> resourceKey;

    public ListFlag(String flagIdentifier, boolean isAllowed){
        super(flagIdentifier, LIST_FLAG, isAllowed);
        resourceKey = new HashSet<>(0);
    }

    public ListFlag(CompoundTag nbt){
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public boolean containsKey(String key){
        return this.resourceKey.contains(key);
    }

    public boolean allows(String key){
        return this.containsKey(key) && isInverted();
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        // TODO
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        // TODO
    }
}
