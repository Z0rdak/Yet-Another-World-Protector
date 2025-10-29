package de.z0rdak.yawp.core.flag;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import org.jetbrains.annotations.NotNull;

import java.util.*;

public class RegionFlags implements IFlagContainer {
    public static Codec<RegionFlags> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                    Codec.unboundedMap(Codec.STRING, FlagValue.CODEC)
                            .optionalFieldOf("flags", new HashMap<>())
                            .forGetter(a -> a.flags)
            ).apply(instance, RegionFlags::new)
    );

    private final Map<String, IFlag> flags;

    public RegionFlags() {
        this.flags = new HashMap<>();
    }

    public RegionFlags(Map<String, IFlag> flags) {
        this.flags = new HashMap<>(flags);
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
    public boolean isSet(@NotNull String flagName) {
        FlagState flagState = flagState(flagName);
        return flagState == FlagState.ALLOWED || flagState == FlagState.DENIED;
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

    public Map<String, IFlag> getFlagMap() {
        return this.flags;
    }
}