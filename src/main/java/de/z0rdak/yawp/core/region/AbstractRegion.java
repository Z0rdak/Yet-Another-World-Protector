package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.commands.CommandUtil;
import de.z0rdak.yawp.core.flag.FlagContainer;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.core.group.PlayerContainer;
import net.minecraft.core.Registry;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.scores.Team;

import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

/**
 * An abstract region represents the basic implementation of a IProtectedRegion.
 * This abstraction can be used for markable regions as well as regions without
 * an area (dimensions). <br>
 */
public abstract class AbstractRegion implements IProtectedRegion {
    protected String name;
    protected ResourceKey<Level> dimension;
    protected RegionType regionType;
    protected FlagContainer flags;
    protected Map<String, PlayerContainer> groups;
    protected boolean isActive;
    protected boolean isMuted;
    protected boolean inheritFlags;
    protected IProtectedRegion parent;
    protected String parentName;
    protected Map<String, IProtectedRegion> children;
    protected Set<String> childrenNames;

    protected AbstractRegion(CompoundTag nbt) {
        this.childrenNames = new HashSet<>(0);
        this.children = new HashMap<>(0);
        this.parentName = null;
        this.parent = null;
        this.flags = new FlagContainer();
        this.groups = new HashMap<>();
        this.groups.put(MEMBERS, new PlayerContainer());
        this.groups.put(OWNERS, new PlayerContainer());
        this.inheritFlags = true;
        this.deserializeNBT(nbt);
    }

    protected AbstractRegion(String name, ResourceKey<Level> dimension, RegionType type) {
        this.name = name;
        this.dimension = dimension;
        this.regionType = type;
        this.flags = new FlagContainer();
        this.groups = new HashMap<>();
        this.groups.put(MEMBERS, new PlayerContainer());
        this.groups.put(OWNERS, new PlayerContainer());
        this.children = new HashMap<>();
        this.isActive = true;
        this.inheritFlags = true;
        this.childrenNames = new HashSet<>();
    }

    protected AbstractRegion(String name, ResourceKey<Level> dimension, RegionType regionType, Player owner) {
        this(name, dimension, regionType);
        if (owner != null) {
            this.groups.get(OWNERS).addPlayer(owner.getUUID(), owner.getScoreboardName());
        }
    }

    @Override
    public String getParentName() {
        return parentName;
    }

    @Override
    public String getName() {
        return name;
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

    public boolean containsFlag(RegionFlag flag) {
        return this.flags.contains(flag.name);
    }

    @Override
    public boolean containsFlag(String flag) {
        return this.flags.contains(flag);
    }

    @Override
    public Collection<IFlag> getFlags() {
        return Collections.unmodifiableList(new ArrayList<>(this.flags.values()));
    }

    @Override
    public FlagContainer getFlagContainer() {
        return flags;
    }

    public boolean inheritsFlags() {
        return this.inheritFlags;
    }

    public void setInheritFlags(boolean inheritFlags) {
        this.inheritFlags = inheritFlags;
    }

    @Nullable
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


    @Override
    public void addTeam(String teamName, String group) {
        this.getGroup(group).addTeam(teamName);
    }

    @Override
    public void removeTeam(String teamName, String group) {
        this.getGroup(group).removeTeam(teamName);
    }

    public void resetGroups() {
        this.groups.clear();
        this.groups.put(MEMBERS, new PlayerContainer());
        this.groups.put(OWNERS, new PlayerContainer());
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
    public boolean hasTeam(String teamName, String group) {
        return this.getGroup(group).hasTeam(teamName);
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
            // FIXME: return null instead to signal non-existing group? or manage them properly
            return this.groups.put(group, new PlayerContainer());
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
     * TODO: Add argument for group to check
     */
    @Override
    public boolean permits(Player player) {
        return isInGroup(player, CommandUtil.OWNER) || isInGroup(player, CommandUtil.MEMBER);
    }

    public boolean isInGroup(Player player, String group) {
        return this.groups.get(group).hasPlayer(player.getUUID()) || (player.getTeam() != null && this.groups.get(group).hasTeam(player.getTeam().getName()));
    }

    /**
     * Will always be called by IMarkableRegion to remove child of type IMarkableRegion
     */
    @Override
    public void removeChild(IProtectedRegion child) {
        this.children.remove(child.getName());
        this.childrenNames.remove(child.getName());
    }

    @Override
    public void clearChildren() {
        this.children.clear();
        this.childrenNames.clear();
    }

    @Override
    public Map<String, IProtectedRegion> getChildren() {
        return Collections.unmodifiableMap(this.children);
    }

    @Override
    public Set<String> getChildrenNames() {
        return this.childrenNames;
    }

    @Override
    public boolean hasChild(IProtectedRegion maybeChild) {
        return this.children.containsKey(maybeChild.getName());
    }

    @Override
    public boolean addChild(IProtectedRegion child) {
        this.children.put(child.getName(), child);
        this.childrenNames.add(child.getName());
        ((AbstractRegion) child).setParent(this);
        return true;
    }

    protected boolean setParent(IProtectedRegion parent) {
        this.parent = parent;
        this.parentName = parent.getName();
        return true;
    }


    public IProtectedRegion getParent() {
        return parent;
    }

    @Override
    public CompoundTag serializeNBT() {
        CompoundTag nbt = new CompoundTag();
        nbt.putString(NAME, this.name);
        nbt.putString(DIM, this.dimension.location().toString());
        nbt.putString(REGION_TYPE, this.regionType.type);
        nbt.putBoolean(ACTIVE, this.isActive);
        nbt.putBoolean(MUTED, this.isMuted);
        nbt.putBoolean(INHERIT_FLAGS, this.inheritFlags);
        nbt.put(FLAGS, this.flags.serializeNBT());
        nbt.put(OWNERS, this.groups.get(OWNERS).serializeNBT());
        nbt.put(MEMBERS, this.groups.get(MEMBERS).serializeNBT());
        if (this.parent != null) {
            nbt.putString(PARENT, this.parent.getName());
        } else {
            nbt.putString(PARENT, "");
        }
        if (this.children != null) {
            ListTag childrenList = new ListTag();
            childrenList.addAll(this.children.keySet().stream().map(StringTag::valueOf).collect(Collectors.toSet()));
            nbt.put(CHILDREN, childrenList);
        } else {
            nbt.put(CHILDREN, new ListTag());
        }
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        this.name = nbt.getString(NAME);
        this.dimension = ResourceKey.create(Registry.DIMENSION_REGISTRY,
                new ResourceLocation(nbt.getString(DIM)));
        this.isActive = nbt.getBoolean(ACTIVE);
        this.isMuted = nbt.getBoolean(MUTED);
        this.regionType = RegionType.of(nbt.getString(REGION_TYPE));
        this.inheritFlags = nbt.getBoolean(INHERIT_FLAGS);
        this.flags = new FlagContainer(nbt.getCompound(FLAGS));
        this.groups = new HashMap<>();
        this.groups.put(OWNERS, new PlayerContainer(nbt.getCompound(OWNERS)));
        this.groups.put(MEMBERS, new PlayerContainer(nbt.getCompound(MEMBERS)));
        if (this.parent == null && nbt.contains(PARENT, Tag.TAG_STRING)) {
            String parentName = nbt.getString(PARENT);
            if (!parentName.isEmpty()) {
                this.parentName = nbt.getString(PARENT);
            } else {
                this.parentName = null;
            }
        }
        if (this.children != null && this.children.isEmpty()) {
            if (nbt.contains(CHILDREN, Tag.TAG_LIST)) {
                ListTag childrenNbt = nbt.getList(CHILDREN, Tag.TAG_STRING);
                if (childrenNbt.size() > 0) {
                    this.children = new HashMap<>(childrenNbt.size());
                    this.childrenNames = new HashSet<>(childrenNbt.size());
                    for (int i = 0; i < childrenNbt.size(); i++) {
                        this.childrenNames.add(childrenNbt.getString(i));
                    }
                }
            }
        }
    }
}
