package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.nbt.NbtCompound;

import java.util.HashMap;
import java.util.Set;

import static de.z0rdak.yawp.util.constants.RegionNBT.FLAG_TYPE;

/**
 * Represents a simple map holding the flag values to the corresponding flag names for a region and providing methods handling the flags. <br>
 * [Key] FlagName -> [Value] IFlag <br>
 * E.g. "break_blocks" -> BooleanFlag {"value": false, ...}
 */
public class FlagContainer extends HashMap<String, IFlag> implements INbtSerializable<NbtCompound> {


    public FlagContainer(NbtCompound nbt) {
        this();
        this.deserializeNBT(nbt);
    }

    public FlagContainer() {
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
    public NbtCompound serializeNBT() {
        NbtCompound nbt = new NbtCompound();
        this.forEach((key, value) -> nbt.put(key, value.serializeNBT()));
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        Set<String> flagKeys = nbt.getKeys();
        flagKeys.forEach(key -> {
            NbtCompound flagNbt = nbt.getCompound(key);
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
