package de.z0rdak.yawp.core.stick;

import de.z0rdak.yawp.core.INbtSerializable;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.nbt.CompoundTag;

import static de.z0rdak.yawp.constants.serialization.ItemNbtKeys.STICK_TYPE;

public abstract class AbstractStick implements INbtSerializable<CompoundTag> {

    private StickType stickType;

    public AbstractStick(StickType stickType) {
        this.stickType = stickType;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(STICK_TYPE, this.stickType.stickName);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.stickType = StickType.of(nbt.getString(STICK_TYPE));
    }

    public StickType getStickType() {
        return stickType;
    }

    public abstract void cycleMode();
}
