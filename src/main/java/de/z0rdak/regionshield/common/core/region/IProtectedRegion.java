package de.z0rdak.regionshield.common.core.region;

import de.z0rdak.regionshield.common.core.flag.IFlag;
import de.z0rdak.regionshield.common.core.flag.RegionFlag;
import de.z0rdak.regionshield.common.util.PlayerUtils;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/**
 * This interface represents a general protected region.
 * This region has most likely a set of flags to prevent certain action in
 * this region and a set of player, which are allowed to bypass these
 * restrictions.
 * Additionally a flag can determine whether actions are whitelisted
 * or blacklisted and a flag which determines whether the regions is
 * active or not.
 * <p>
 * Classes which implement this interface must also provide a way
 * to serialize the region data into a CompoundNBT.
 */
public interface IProtectedRegion extends INBTSerializable<CompoundNBT> {

    String getName();

    boolean addFlag(IFlag flag);

    boolean addFlag(String flag, boolean allowance);

    boolean removeFlag(String flag);

    boolean containsFlag(String flag);

    boolean containsFlag(RegionFlag flag);

    Set<String> getFlags();

    Collection<IFlag> getRegionFlags();

    boolean addPlayer(PlayerEntity player);

    boolean addPlayer(PlayerUtils.MCPlayerInfo playerInfo);

    boolean removePlayer(PlayerEntity player);

    boolean removePlayer(String playerName);

    boolean isActive();

    void setIsActive(boolean isActive);

    Map<UUID, String> getMembers();

    boolean permits(PlayerEntity player);

    boolean forbids(PlayerEntity player);

    boolean denies(String flag);

    boolean denies(RegionFlag flag);

}
