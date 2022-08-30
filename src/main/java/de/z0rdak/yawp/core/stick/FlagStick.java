package de.z0rdak.yawp.core.stick;

import de.z0rdak.yawp.util.StickAction;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.nbt.CompoundTag;
import net.minecraftforge.common.util.INBTSerializable;

import static de.z0rdak.yawp.util.StickAction.ADD;

public class FlagStick extends AbstractStick implements INBTSerializable<CompoundTag> {

    private String selectedFlag;
    private StickAction action;

    public FlagStick() {
        super(StickType.FLAG_STICK);
        this.action = ADD;
        this.selectedFlag = "";
    }

    public FlagStick(CompoundTag nbt){
        this();
        deserializeNBT(nbt);
    }

    public void cycleMode(){
        this.action = StickAction.values()[(this.action.ordinal() + 1) % StickAction.values().length];
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        nbt.putString("selected_flag", this.selectedFlag);
        nbt.putString("flag_action", this.action.toString());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.action = StickAction.valueOf(nbt.getString("flag_action"));
        this.selectedFlag = nbt.getString("selected_flag");
    }

}
