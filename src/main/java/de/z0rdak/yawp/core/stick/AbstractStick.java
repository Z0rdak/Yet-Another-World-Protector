package de.z0rdak.yawp.core.stick;

import de.z0rdak.yawp.util.StickType;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;

import static de.z0rdak.yawp.util.StickUtil.STICK_TYPE;

public abstract class AbstractStick implements INBTSerializable<CompoundNBT> {

    private StickType stickType;

    public AbstractStick(StickType stickType) {
        this.stickType = stickType;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(STICK_TYPE, this.stickType.stickName);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.stickType = StickType.of(nbt.getString(STICK_TYPE));
    }

    public StickType getStickType() {
        return stickType;
    }

    public abstract void cycleMode();
}
