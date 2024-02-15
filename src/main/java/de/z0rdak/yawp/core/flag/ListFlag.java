package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;

import java.util.HashSet;
import java.util.Set;

import static de.z0rdak.yawp.core.flag.FlagType.LIST_FLAG;

public class ListFlag extends AbstractFlag {

    public Set<String> resourceKey;

    public ListFlag(String flagIdentifier, boolean isAllowed){
        super(flagIdentifier, LIST_FLAG, isAllowed);
        resourceKey = new HashSet<>(0);
    }

    public ListFlag(CompoundNBT nbt){
        super(nbt);
        this.deserializeNBT(nbt);
    }


    public boolean containsKey(String key){
        return this.resourceKey.contains(key);
    }

    public boolean allows(String key){
        return this.containsKey(key) && doesOverride();
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        throw new UnsupportedOperationException("Not implemented yet");
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        throw new UnsupportedOperationException("Not implemented yet");
    }

    @Override
    public boolean isAllowed(Object... args) {
        throw new UnsupportedOperationException("Not implemented yet");
    }
}
