package de.z0rdak.yawp.core.stick;

import de.z0rdak.yawp.util.StickAction;
import de.z0rdak.yawp.util.StickType;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;

import static de.z0rdak.yawp.util.StickAction.ADD;

public class FlagStick extends AbstractStick implements INBTSerializable<CompoundNBT> {

    private String selectedFlag;
    private StickAction action;

    public FlagStick() {
        super(StickType.FLAG_STICK);
        this.action = ADD;
        this.selectedFlag = "";
    }

    public FlagStick(CompoundNBT nbt){
        this();
        deserializeNBT(nbt);
    }

    public void cycleMode(){
        this.action = StickAction.values()[(this.action.ordinal() + 1) % StickAction.values().length];
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.putString("selected_flag", this.selectedFlag);
        nbt.putString("flag_action", this.action.toString());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.action = StickAction.valueOf(nbt.getString("flag_action"));
        this.selectedFlag = nbt.getString("selected_flag");
    }

}