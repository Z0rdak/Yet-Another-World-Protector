package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.neoforged.neoforge.common.util.INBTSerializable;
import org.jetbrains.annotations.UnknownNullability;

import java.util.HashMap;
import java.util.Set;

import static de.z0rdak.yawp.util.constants.RegionNBT.FLAG_TYPE;

/**
 * Represents a simple map holding the flag values to the corresponding flag names for a region and providing methods handling the flags. <br>
 * [Key] FlagName -> [Value] IFlag <br>
 * E.g. "break_blocks" -> BooleanFlag {"value": false, ...}
 */
public class FlagContainer extends HashMap<String, IFlag> implements INBTSerializable<CompoundTag> {


    public FlagContainer(CompoundTag nbt){
        this();
        this.deserializeNBT(provider, nbt);
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
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = new CompoundTag();
        this.forEach((flagName, iFlag) -> {
            if (RegionFlag.contains(flagName)) {
                nbt.put(flagName, iFlag.serializeNBT(provider));
            }
        });
        return nbt;
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        Set<String> flagKeys = nbt.getAllKeys();
        flagKeys.forEach( key -> {
            CompoundTag flagNbt = nbt.getCompound(key);
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

    public void put(IFlag flag) {
        this.put(flag.getFlagIdentifier(), flag);
    }

    public boolean contains(RegionFlag flag) {
        return this.containsKey(flag.name);
    }

    public boolean contains(String flag) {
        return this.containsKey(flag);
    }
}
