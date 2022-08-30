package de.z0rdak.yawp.core.stick;

import de.z0rdak.yawp.util.StickAction;
import de.z0rdak.yawp.util.StickType;
import de.z0rdak.yawp.util.constants.NBTConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraftforge.common.util.INBTSerializable;

public class RegionStick extends AbstractStick implements INBTSerializable<CompoundTag> {

    private String selectedRegion;
    private StickAction action;

    public RegionStick() {
        super(StickType.REGION_STICK);
        this.action = StickAction.ADD;
        this.selectedRegion = "";
    }

    public RegionStick(CompoundTag nbt){
        this();
        deserializeNBT(nbt);
    }

    public void cycleMode(){
        this.action = this.action == StickAction.ADD ? StickAction.REMOVE : StickAction.ADD;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        nbt.putString(NBTConstants.SELECTED_REGION, this.selectedRegion);
        nbt.putString(NBTConstants.FLAG_ACTION, this.action.toString());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.action = StickAction.valueOf(nbt.getString(NBTConstants.FLAG_ACTION));
        this.selectedRegion = nbt.getString(NBTConstants.SELECTED_REGION);
    }
}
