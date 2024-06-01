package de.z0rdak.yawp.core.flag;

import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import org.apache.commons.lang3.NotImplementedException;

import static de.z0rdak.yawp.core.flag.FlagType.INT_FLAG;

/**
 * Will be used for applying effects with a specific value and interval
 */
@Deprecated
public class IntFlag extends AbstractFlag {
    private int value;
    private int tickInterval;

    public IntFlag(String flag, int value, int tickInterval){
        super(flag, INT_FLAG, false);
        this.value = value;
        this.tickInterval = tickInterval;
    }

    public IntFlag(HolderLookup.Provider provider, CompoundTag nbt) {
        super(provider, nbt);
        this.deserializeNBT(provider, nbt);
    }

    public int getValue() {
        return value;
    }

    public int getTickInterval() {
        return tickInterval;
    }

    public void setValue(int value) {
        this.value = value;
    }

    public void setTickInterval(int tickInterval) {
        this.tickInterval = tickInterval;
    }

    @Override
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = super.serializeNBT(provider);
        throw new NotImplementedException("Missing serializeNBT implementation in IntFlag");
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
        throw new NotImplementedException("Missing deserializeNBT implementation in IntFlag");
    }
}
