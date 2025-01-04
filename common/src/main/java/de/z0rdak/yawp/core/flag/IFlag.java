package de.z0rdak.yawp.core.flag;

import de.z0rdak.yawp.core.INbtSerializable;
import net.minecraft.nbt.CompoundTag;

public interface IFlag extends INbtSerializable<CompoundTag>, Comparable<IFlag> {

    /**
     * Get the unique identifier for the flag. <br>
     * The valid flags are currently stored as an enum. <br>
     * Mod:Name -> ResourceLocation in the future.
     *
     * @return unique name for flag.
     * @see RegionFlag
     */
    String getName();

    /**
     * Returns the flag type of the flag.     *
     *
     * @return the flag type enum value of the flag.
     * @see FlagType
     */
    FlagType getType();

    /**
     * Returns whether the flag does override the same flag defined in child regions. <br>
     *
     * @return true if the flag overrides the same flag in child regions
     */
    boolean doesOverride();

    /**
     * Set the override state of the flag. <br>
     * When true, it overrides the same flag in child regions.
     *
     * @param doesOverride overrides the same flag in child regions if set to true
     */
    void setOverride(boolean doesOverride);

    /**
     * Returns whether the flag is active in the region. <br>
     * This means the flag state is either ALLOWED or DENIED. <br>
     * Disabled flags are not considered for flag checks.
     *
     * @return true if flag is active, false otherwise.
     */
    boolean isActive();

    FlagState getState();

    void setState(FlagState state);

    FlagMessage getFlagMsg();

    void setFlagMsg(FlagMessage msg);
}
