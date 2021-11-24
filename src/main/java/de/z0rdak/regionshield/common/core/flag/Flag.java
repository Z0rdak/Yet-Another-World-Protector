package de.z0rdak.regionshield.common.core.flag;

import net.minecraft.nbt.CompoundNBT;

import static de.z0rdak.regionshield.common.util.constants.RegionNBT.FLAG;
import static de.z0rdak.regionshield.common.util.constants.RegionNBT.IS_DENIED;

public class Flag implements IFlag {

    private String flag;
    private boolean isDenied;
    // TODO: default value
    //

    public Flag(String flag){
        this.flag = flag;
        this.isDenied = true;
    }

    public Flag(String flag, boolean isDenied){
        this.flag = flag;
        this.isDenied = isDenied;
    }

    public Flag(CompoundNBT nbt){
        this.deserializeNBT(nbt);
    }

    @Override
    public String getFlagName() {
        return this.flag;
    }

    @Override
    public boolean isDefaultValue(){
        return this.isDenied;
    }

    @Override
    public void allow() {
        this.isDenied = false;
    }

    @Override
    public void deny() {
        this.isDenied = true;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(FLAG, this.flag);
        nbt.putBoolean(IS_DENIED, this.isDenied);
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.flag = nbt.getString(FLAG);
        this.isDenied =  nbt.getBoolean(IS_DENIED);
    }
}
