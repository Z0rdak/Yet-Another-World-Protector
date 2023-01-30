package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.NbtCompound;

import java.util.HashSet;
import java.util.Set;

import static de.z0rdak.yawp.core.flag.FlagType.LIST_FLAG;

// TODO: List of Strings which can represent everything
// List can be assinged to ListFlag, for reusability
// ListFlag only checks List against entity/item/or something
// ListFlag defines what entries are expected: ResourceLocation, Tags, etc...
public class ListFlag extends AbstractFlag {

    public Set<String> resourceKey;

    public ListFlag(String flagIdentifier, boolean isAllowed) {
        super(flagIdentifier, LIST_FLAG, isAllowed);
        resourceKey = new HashSet<>(0);
    }

    public ListFlag(NbtCompound nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public boolean containsKey(String key) {
        return this.resourceKey.contains(key);
    }

    public boolean allows(String key) {
        return this.containsKey(key) && isInverted();
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        // TODO
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        // TODO
    }

    @Override
    public boolean isAllowed(Object... args) {
        // should be... List<ResourceLocation> || List<String> ||...
        return false;
    }
}
