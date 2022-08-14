package de.z0rdak.regionshield.core.flag;

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
public interface IFlag extends INBTSerializable<CompoundNBT> {

    /**
     * Mod:Name -> ResourceLocation in future
     * @return unique name for flag
     */
    String getFlagName();

    /**
     * Lang key for a short concise description of the flag.
     * @return lang key for a description
     */
    String getFlagDescription();

    String getFlagType();

    boolean isAllowed();

    void setAllowed(boolean allowed);

    boolean isActive();

    void setIsActive(boolean active);

}
