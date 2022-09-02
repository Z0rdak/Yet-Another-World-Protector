package de.z0rdak.yawp.core.flag;

import net.minecraft.nbt.CompoundTag;
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
 *
 *
 *
 * NumberFlag
 * /rs flag add <region> max-level 30
 * /rs flag add <region>
 *
 *
 *
 *
 *
 *
 *
 * // Trigger
 * /rs trigger <region> on-leave clear-xp
 *
 */
public interface IFlag extends INBTSerializable<CompoundTag> {

    /**
     * Mod:Name -> ResourceLocation in future
     * @return unique name for flag
     */
    String getFlagIdentifier();

    FlagType getFlagType();

    boolean isInverted();

    void setInverted(boolean inverted);

    boolean isActive();

    void setIsActive(boolean active);

}
