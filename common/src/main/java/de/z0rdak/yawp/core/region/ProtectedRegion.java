package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.api.Flag;
import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.core.flag.RegionFlags;
import de.z0rdak.yawp.core.flag.IFlag;

import de.z0rdak.yawp.core.group.PlayerContainer;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.Identifier;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import java.util.*;

/**
 * An abstract region represents the basic implementation of a IProtectedRegion.
 * This abstraction can be used for markable regions as well as regions without
 * an area (dimensions). <br>
 */
public abstract class ProtectedRegion implements IProtectedRegion {

    protected ResourceKey<Level> dimension;
    protected IProtectedRegion parent;
    protected UUID dataUUid;
    protected UUID parentId;
    private String name;
    private final RegionType regionType;
    private RegionFlags flags;
    private final Map<String, PlayerContainer> groups;
    private boolean isActive;
    private boolean isMuted;
    private final Map<String, IProtectedRegion> children;
    private final Set<UUID> childrenIds;

    protected ProtectedRegion(String name, UUID id, UUID parentId, ResourceKey<Level> dimension, RegionType type) {
        this.name = name;
        this.dataUUid = id;
        this.parentId = parentId;
        this.dimension = dimension;
        this.regionType = type;
        this.flags = new RegionFlags();
        this.groups = new HashMap<>();
        this.groups.put(Permissions.MEMBER, new PlayerContainer(Permissions.MEMBER));
        this.groups.put(Permissions.OWNER, new PlayerContainer(Permissions.OWNER));
        this.children = new HashMap<>();
        this.isActive = true;
        this.childrenIds = new HashSet<>();
    }

    protected ProtectedRegion(String name, UUID id, UUID parentId, ResourceKey<Level> dimension, RegionType regionType, Player owner) {
        this(name, id, parentId, dimension, regionType);
        if (owner != null) {
            this.groups.get(Permissions.OWNER).addPlayer(owner.getUUID(), owner.getScoreboardName());
        }
    }

    @Override
    public UUID getUuid() {
        return this.dataUUid;
    }

    @Override
    public UUID getParentId() {
        return parentId;
    }

    @Override
    public String getName() {
        return name;
    }

    protected void setName(String name) {
        this.name = name;
    }

    public void setGroups(Map<String, PlayerContainer> groups) {
        this.groups.putAll(groups);
    }

    public Map<String, PlayerContainer> getGroups() {
        return Collections.unmodifiableMap(groups);
    }

    @Override
    public ResourceKey<Level> getDim() {
        return dimension;
    }

    @Override
    public RegionType getRegionType() {
        return this.regionType;
    }

    @Override
    public void addFlag(IFlag flag) {
        this.flags.put(flag);
    }

    @Override
    public void removeFlag(String flag) {
        this.flags.remove(flag);
    }

    public boolean containsFlag(Flag flag) {
        return this.containsFlag(flag.name());
    }

    @Override
    public boolean containsFlag(String flag) {
        return this.flags.contains(flag);
    }

    public void setFlags(RegionFlags flags) {
        this.flags = flags;
    }

    @Override
    public RegionFlags getFlags() {
        return flags;
    }

    @Override
    public IFlag getFlag(String flagName) {
        if (this.flags.contains(flagName)) {
            return this.flags.get(flagName);
        } else {
            return null;
        }
    }

    @Override
    public boolean isActive() {
        return this.isActive;
    }

    @Override
    public void setIsActive(boolean isActive) {
        this.isActive = isActive;
    }

    @Override
    public boolean isMuted() {
        return this.isMuted;
    }

    @Override
    public void setIsMuted(boolean isMuted) {
        this.isMuted = isMuted;
    }

    @Override
    public void addPlayer(Player player, String group) {
        this.getGroup(group).addPlayer(player.getUUID(), player.getScoreboardName());
    }

    @Override
    public void addPlayer(UUID uuid, String playerName, String group) {
        this.getGroup(group).addPlayer(uuid, playerName);
    }

    public void resetGroups() {
        this.groups.clear();
        this.groups.put(Permissions.MEMBER, new PlayerContainer(Permissions.MEMBER));
        this.groups.put(Permissions.OWNER, new PlayerContainer(Permissions.OWNER));
    }

    @Override
    public void removePlayer(UUID playerUuid, String group) {
        if (group.equals("*")) {
            for (String g : this.groups.keySet()) {
                this.getGroup(g).removePlayer(playerUuid);
            }
            return;
        }
        this.getGroup(group).removePlayer(playerUuid);
    }

    @Override
    public boolean hasPlayer(UUID playerUuid, String group) {
        return this.getGroup(group).hasPlayer(playerUuid);
    }

    /**
     * Gets the container for the provided group. Creates a new one if none is existent.
     */
    @Override
    public PlayerContainer getGroup(String group) {
        if (!this.groups.containsKey(group)) {
            return this.groups.put(group, new PlayerContainer(group));
        }
        return this.groups.get(group);
    }

    /**
     * Checks if the player is defined in the regions player list OR whether the player is an operator.
     * Usually this check is needed when an event occurs, and it needs to be checked whether
     * the player has a specific permission to perform an action in the region.
     *
     * @param player to be checked
     * @return true if player is in region list or is an operator, false otherwise
     */
    @Override
    public boolean permits(Player player) {
        return isInGroup(player, Permissions.OWNER) || isInGroup(player, Permissions.MEMBER);
    }

    public boolean isInGroup(Player player, String group) {
        return this.groups.get(group).hasPlayer(player.getUUID());
    }

    @Override
    public void clearChildren() {
        this.children.clear();
        this.childrenIds.clear();
    }

    public void setChildrenIds(List<UUID> childrenNames) {
        this.childrenIds.clear();
        this.childrenIds.addAll(childrenNames);
    }

    @Override
    public Map<String, IProtectedRegion> getChildren() {
        return Collections.unmodifiableMap(this.children);
    }

    @Override
    public Set<UUID> getChildrenIds() {
        return this.childrenIds;
    }

    @Override
    public boolean hasChild(IProtectedRegion maybeChild) {
        return this.children.containsKey(maybeChild.getName());
    }

    @Override
    public boolean addChild(IProtectedRegion child) {
        if (child == null) {
            return false;
        }
        var name = child.getName();
        var uuid = child.getUuid();
        if (children.containsKey(name)) {
            return false;
        }
        children.put(name, child);
        childrenIds.add(uuid);
        return true;
    }

    @Override
    public void removeChild(IProtectedRegion child) {
        if (child == null) {
            return;
        }
        String name = child.getName();
        var uuid = child.getUuid();
        children.remove(name);
        childrenIds.remove(uuid);
    }

    @Override
    public void setParent(IProtectedRegion parent) {
        this.parent = parent;
        this.parentId = parent.getUuid();
    }

    public IProtectedRegion getParent() {
        return parent;
    }

}
