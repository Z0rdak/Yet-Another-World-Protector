package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.scores.Team;
import net.minecraftforge.common.util.INBTSerializable;

import java.util.Collection;

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
 * to serialize the region data into a CompoundTag.
 */
public interface IProtectedRegion extends INBTSerializable<CompoundTag> {

    String getName();

    void addFlag(IFlag flag);

    void removeFlag(String flag);

    boolean containsFlag(IFlag flag);

    boolean containsFlag(RegionFlag flag);

    Collection<IFlag> getFlags();

    void addMember(Player player);

    void addMember(Team team);

    void addOwner(Player player);

    void addOwner(Team team);

    void removeMember(Player player);

    void removeOwner(Player player);

    void removeMember(Team team);

    void removeOwner(Team team);

    boolean isActive();

    void setIsActive(boolean isActive);

    PlayerContainer getMembers();

    PlayerContainer getOwners();

    boolean permits(Player player);
}
