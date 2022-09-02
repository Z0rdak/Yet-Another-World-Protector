package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.HashMap;
import java.util.Set;

import static de.z0rdak.yawp.util.constants.RegionNBT.FLAG_TYPE;

public class FlagContainer extends HashMap<String, IFlag> implements INBTSerializable<CompoundNBT> {


    public FlagContainer(CompoundNBT nbt){
        this();
        this.deserializeNBT(nbt);
    }

    public FlagContainer(){
        super();
    }

    public FlagContainer(IFlag flag) {
        this();
        this.put(flag);
    }

    public FlagContainer(Set<IFlag> flags) {
        this();
        flags.forEach(this::put);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        this.forEach((key, value) -> nbt.put(key, value.serializeNBT()));
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        Set<String> flagKeys = nbt.getAllKeys();
        flagKeys.forEach( key -> {
            CompoundNBT flagNbt = nbt.getCompound(key);
            FlagType flagType = FlagType.of(flagNbt.getString(FLAG_TYPE));
            if (flagType != null) {
                switch (flagType) {
                    case BOOLEAN_FLAG:
                        this.put(key, new BooleanFlag(flagNbt));
                        break;
                    case LIST_FLAG:
                        this.put(key, new ListFlag(flagNbt));
                        break;
                    case INT_FLAG:
                        this.put(key, new IntFlag(flagNbt));
                        break;
                }
            } else {
                // TODO: Throw error to give error message in abstract region to be more precise with error
                YetAnotherWorldProtector.LOGGER.warn("Error reading entry for flag '" + key + "'.");
            }
        });
    }

    public void put(IFlag flag){
        this.put(flag.getFlagIdentifier(), flag);
    }

    public boolean contains(IFlag flag){
        return this.containsKey(flag.getFlagIdentifier());
    }
}
