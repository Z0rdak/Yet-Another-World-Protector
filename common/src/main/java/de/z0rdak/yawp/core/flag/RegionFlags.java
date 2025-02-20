package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.constants.Constants;
import net.minecraft.nbt.CompoundTag;
import org.jetbrains.annotations.NotNull;

import java.util.*;

import static de.z0rdak.yawp.constants.serialization.RegionNbtKeys.FLAG_TYPE;

public class RegionFlags implements IFlagContainer {

    private final Map<String, IFlag> flags = new HashMap<>();

    public RegionFlags() {}

    public RegionFlags(CompoundTag nbt) {
        this.deserializeNBT(nbt);
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        flags.forEach((flagName, iFlag) -> {
            if (RegionFlag.contains(flagName)) {
                nbt.put(flagName, iFlag.serializeNBT());
            }
        });
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        flags.clear();
        for (String key : nbt.getAllKeys()) {
            if (RegionFlag.contains(key)) {
                CompoundTag flagNbt = nbt.getCompound(key);
                FlagType flagType = FlagType.of(flagNbt.getString(FLAG_TYPE));
                if (flagType != null) {
                    IFlag flag = switch (flagType) {
                        case BOOLEAN_FLAG -> new BooleanFlag(flagNbt);
                        case LIST_FLAG -> new ListFlag(flagNbt);
                        case INT_FLAG -> new IntFlag(flagNbt);
                    };
                    flags.put(key, flag);
                } else {
                    Constants.LOGGER.warn("Error reading entry for flag '{}'.", key);
                }
            }
        }
    }

    @Override
    public void put(IFlag flag) {
        flags.put(flag.getName(), flag);
    }

    @Override
    public IFlag get(String flagName) {
        return flags.get(flagName);
    }

    @Override
    public void remove(String flag) {
        flags.remove(flag);
    }

    @Override
    public boolean contains(String flag) {
        return flags.containsKey(flag);
    }

    @Override
    public void clear() {
        flags.clear();
    }

    @Override
    public boolean isEmpty() {
        return flags.isEmpty();
    }

    @Override
    public int size() {
        return flags.size();
    }

    @Override
    public Set<Map.Entry<String, IFlag>> flagEntries() {
        return Collections.unmodifiableSet(flags.entrySet());
    }

    @Override
    public List<IFlag> flags() {
        return List.copyOf(flags.values());
    }

    @Override
    public List<IFlag> flags(FlagState state) {
        return flags.values().stream().filter(flag -> flag.getState() == state).toList();
    }

    /**
     * Retrieves the {@link FlagState} of a specified flag within this region.
     * <p>
     * Since {@code RegionFlags} does not allow null values, a {@code null} flag is treated as 
     * {@link FlagState#UNDEFINED}. If the flag exists in the region, its state is returned.
     * Otherwise, the method returns {@code FlagState.UNDEFINED}.
     * </p>
     *
     * @param flagName the name of the flag to check
     * @return the {@link FlagState} of the specified flag, or {@link FlagState#UNDEFINED} if the flag is not present
     */
    @Override
    public FlagState flagState(String flagName) {
        if (this.contains(flagName)) {
            return flags.get(flagName).getState();
        } else
            return FlagState.UNDEFINED;
    }

    @Override
    public boolean isFlagDefined(@NotNull String flagName) {
        return flagState(flagName) != FlagState.UNDEFINED;
    }

    @Override
    public boolean isAllowedOrDenied(@NotNull String flagName) {
        FlagState flagState = flagState(flagName);
        return flagState == FlagState.ALLOWED || flagState == FlagState.DENIED;
    }

    public RegionFlags deepCopy() {
        return new RegionFlags(this.serializeNBT());
    }

    public Map<String, IFlag> getActiveFlags() {
        Map<String, IFlag> activeFlags = new HashMap<>();
        flags.forEach((k, v) -> {
            if (v.isActive()) {
                activeFlags.put(k, v);
            }
        });
        return activeFlags;
    }
}