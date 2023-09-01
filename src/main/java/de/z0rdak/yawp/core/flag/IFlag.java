package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;

/**
 *
 * ListFlag
 * Blacklist or Whitelist
 *
 * Usable for placing/breaking blocks, item usage
 * Granular entity spawn control
 *
 * /rs flag add <region> <flag>
 * /rs flag add <region> block-blacklist <modid:block> [... <modid:block>]
 * Remove all blocks from blacklist and blacklist itself
 * /rs flag remove <region> block-blacklist
 * Removes all given blocks from blacklist
 * /rs flag remove <region> block-blacklist <modid:block> [... <modid:block>]
 * Removes all blocks from blacklist
 * /rs flag remove <region> block-blacklist clear
 *
 * NumberFlag
 * /rs flag add <region> max-level 30
 * /rs flag add <region>
 *
 * // Trigger
 * /rs trigger <region> on-leave clear-xp
 *
 */
public interface IFlag extends INBTSerializable<CompoundNBT>, Comparable<IFlag> {

    /**
     * Get the unique identifier for the flag. <br>
     * The valid flags are currently stored as an enum. <br>
     * Mod:Name -> ResourceLocation in future.
     * @see RegionFlag
     * @return unique name for flag.
     */
    String getFlagIdentifier();

    /**
     * Returns the flag type of the flag.
     * @see FlagType
     * @return the flag type enum value of the flag.
     */
    FlagType getFlagType();

    /**
     * Returns whether the flag is inverted for flag checks. <br>
     * If true the flag is treated as a whitelist flag (flag action is allowed), <br>
     * otherwise is treated like a blacklist (flag action is prohibited).
     * @return true if the flag acts as a whitelist flag, false otherwise.
     */
    boolean isInverted();

    /**
     * Set the inverted state of the flag.
     * @param inverted true sets flag to act like a whitelist flag, false sets flag to be a blacklist flag.
     */
    void setInverted(boolean inverted);

    /**
     * Returns whether the flag is active in the region. <br>
     * Disabled flags are not considered for flag checks.
     * @return true if flag is active, false otherwise.
     */
    boolean isActive();

    /**
     * Set the active state of the flag.
     * @param active true activates the flag, false disables the flag for checks.
     */
    void setIsActive(boolean active);

    FlagMessage getFlagMsg();

    void setFlagMsg(FlagMessage msg);

    /**
     * FIXME: should work, but hacky workaround
     * Returns whether the flag allows the flag action considering the optional arguments
     * @param args optional arguments to be used in flag check
     * @return whether the flag allows the flag action
     */
    boolean isAllowed(Object... args);
}
