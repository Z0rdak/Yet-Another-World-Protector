package de.z0rdak.regionshield.core.region;

import de.z0rdak.regionshield.core.affiliation.PlayerContainer;
import de.z0rdak.regionshield.core.flag.IFlag;
import de.z0rdak.regionshield.core.flag.RegionFlag;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.Collection;
import java.util.UUID;

/**
 * This interface represents a general protected region.
 * This region has most likely a set of flags to prevent certain action in
 * this region and a set of player, which are allowed to bypass these
 * restrictions.
 * <p>
 * Additionally, a flag can determine whether actions are whitelisted
 * or blacklisted and a flag which determines whether the regions are
 * active or not.
 * <p>
 * Classes which implement this interface must also provide a way
 * to serialize the region data into a CompoundNBT.
 */
public interface IProtectedRegion extends INBTSerializable<CompoundNBT> {

    String getName();

    boolean addFlag(IFlag flag);

    boolean removeFlag(IFlag flag);

    boolean containsFlag(IFlag flag);

    boolean containsFlag(RegionFlag flag);

    Collection<IFlag> getFlags();

    boolean addMember(PlayerEntity player);

    boolean addOwner(PlayerEntity player);

    boolean removeMember(UUID uuid);

    boolean removeOwner(UUID uuid);

    boolean isActive();

    void setIsActive(boolean isActive);

    PlayerContainer getMembers();

    PlayerContainer getOwners();

    boolean permits(PlayerEntity player);
}
