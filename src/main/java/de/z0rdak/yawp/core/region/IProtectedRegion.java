package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.flag.FlagContainer;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.scoreboard.Team;
import net.minecraft.util.RegistryKey;
import net.minecraft.world.World;
import net.minecraftforge.common.util.INBTSerializable;

import javax.annotation.Nullable;
import java.util.Collection;
import java.util.Map;

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
 *  * TODO: Maybe move getDim to this interface and implement it in AbstractRegion
 */
public interface IProtectedRegion extends INBTSerializable<CompoundNBT> {

    String getName();

    RegistryKey<World> getDim();

    void addFlag(IFlag flag);

    void removeFlag(String flag);

    boolean containsFlag(String flag);

    boolean containsFlag(RegionFlag flag);

    Collection<IFlag> getFlags();

    FlagContainer getFlagContainer();

    IFlag getFlag(String flagName);

    void updateFlag(IFlag flag);

    void addMember(PlayerEntity player);

    void addMember(Team team);

    void addOwner(PlayerEntity player);

    void addOwner(Team team);

    void removeMember(PlayerEntity player);

    void removeOwner(PlayerEntity player);

    void removeMember(Team team);

    void removeOwner(Team team);

    boolean isActive();

    void setIsActive(boolean isActive);

    PlayerContainer getMembers();

    PlayerContainer getOwners();

    boolean permits(PlayerEntity player);

    @Nullable
    IProtectedRegion getParent();

    void setParent(IProtectedRegion parent);

    Map<String, IProtectedRegion> getChildren();

    void addChild(IProtectedRegion child);

    void removeChild(IProtectedRegion child);

    boolean hasChild(IProtectedRegion child);
}
