package de.z0rdak.yawp.core.stick;

import de.z0rdak.yawp.core.INbtSerializable;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.nbt.NbtCompound;

import static de.z0rdak.yawp.util.StickUtil.STICK_TYPE;

public abstract class AbstractStick implements INbtSerializable<NbtCompound> {

    private StickType stickType;

    public AbstractStick(StickType stickType) {
        this.stickType = stickType;
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = new NbtCompound();
        nbt.putString(STICK_TYPE, this.stickType.stickName);
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        this.stickType = StickType.of(nbt.getString(STICK_TYPE));
    }

    public StickType getStickType() {
        return stickType;
    }

    public abstract void cycleMode();
}
