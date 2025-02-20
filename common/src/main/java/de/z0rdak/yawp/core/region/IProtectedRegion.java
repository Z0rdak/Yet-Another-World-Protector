package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.INbtSerializable;
import de.z0rdak.yawp.core.flag.RegionFlags;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.group.PlayerContainer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import org.jetbrains.annotations.Nullable;

import java.util.Map;
import java.util.Set;
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
public interface IProtectedRegion extends INbtSerializable<CompoundTag> {

    String getName();

    ResourceKey<Level> getDim();

    RegionType getRegionType();

    void addFlag(IFlag flag);

    void removeFlag(String flag);

    boolean containsFlag(String flag);

    boolean containsFlag(RegionFlag flag);

    void setFlags(RegionFlags flags);

    void setGroups(Map<String, PlayerContainer> groups);

    RegionFlags getFlags();

    @Nullable
    IFlag getFlag(String flagName);

    void addPlayer(Player player, String group);

    void addPlayer(UUID uuid, String playerName, String group);

    void addTeam(String teamName, String group);

    void removeTeam(String teamName, String group);

    void removePlayer(UUID playerUuid, String group);

    boolean hasTeam(String teamName, String group);

    boolean hasPlayer(UUID playerUuid, String group);

    PlayerContainer getGroup(String group);

    boolean permits(Player player);

    boolean isInGroup(Player player, String group);

    boolean isActive();

    void setIsActive(boolean isActive);

    boolean isMuted();

    void setIsMuted(boolean isMuted);

    IProtectedRegion getParent();

    String getParentName();

    Map<String, IProtectedRegion> getChildren();

    Set<String> getChildrenNames();

    boolean addChild(IProtectedRegion child);

    void removeChild(IProtectedRegion child);

    void clearChildren();

    void resetGroups();

    boolean hasChild(IProtectedRegion child);
}
