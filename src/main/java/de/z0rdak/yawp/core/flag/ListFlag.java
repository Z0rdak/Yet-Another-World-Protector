package de.z0rdak.yawp.core.flag;

import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;

import java.util.HashSet;
import java.util.Set;

import static de.z0rdak.yawp.core.flag.FlagType.LIST_FLAG;

@Deprecated
public class ListFlag extends AbstractFlag {

    public Set<String> resourceKey;

    public ListFlag(String flagIdentifier, boolean isAllowed) {
        super(flagIdentifier, LIST_FLAG, isAllowed);
        resourceKey = new HashSet<>(0);
    }

    public ListFlag(HolderLookup.Provider provider, CompoundTag nbt) {
        super(provider, nbt);
        this.deserializeNBT(provider, nbt);
    }

    public boolean containsKey(String key){
        return this.resourceKey.contains(key);
    }

    public boolean allows(String key){
        return this.containsKey(key) && doesOverride();
    }

    @Override
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = super.serializeNBT(provider);
        throw new UnsupportedOperationException("Not implemented yet");
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
        throw new UnsupportedOperationException("Not implemented yet");
    }
}
