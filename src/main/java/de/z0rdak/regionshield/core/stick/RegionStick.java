package de.z0rdak.regionshield.core.stick;

import de.z0rdak.regionshield.util.StickAction;
import de.z0rdak.regionshield.util.StickType;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;

public class RegionStick extends AbstractStick implements INBTSerializable<CompoundNBT> {

    private String selectedRegion;
    private StickAction action;

    public RegionStick() {
        super(StickType.REGION_STICK);
        this.action = StickAction.ADD;
        // TODO: ?
        this.selectedRegion = "";
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.putString("selected_region", this.selectedRegion);
        nbt.putString("flag_action", this.action.toString());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.action = StickAction.valueOf(nbt.getString("flag_action"));
        this.selectedRegion = nbt.getString("selected_region");
    }
}
