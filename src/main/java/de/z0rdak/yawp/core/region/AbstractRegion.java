package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.flag.FlagContainer;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtElement;
import net.minecraft.nbt.NbtList;
import net.minecraft.nbt.NbtString;
import net.minecraft.registry.RegistryKey;
import net.minecraft.registry.RegistryKeys;
import net.minecraft.scoreboard.Team;
import net.minecraft.util.Identifier;
import net.minecraft.world.World;

import java.util.UUID;
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
    protected RegistryKey<World> dimension;
    protected RegionType regionType;
    protected FlagContainer flags;
    protected PlayerContainer owners;
    protected PlayerContainer members;
    protected boolean isActive;

    protected IProtectedRegion parent;
    protected String parentName;
    protected Map<String, IProtectedRegion> children;
    protected Set<String> childrenNames;

    protected AbstractRegion(NbtCompound nbt) {
        this.childrenNames = new HashSet<>(0);
        this.children = new HashMap<>(0);
        this.parentName = null;
        this.parent = null;
        this.flags = new FlagContainer();
        this.members = new PlayerContainer();
        this.owners = new PlayerContainer();
        this.deserializeNBT(nbt);
    }

    protected AbstractRegion(String name, RegionType type) {
        this.name = name;
        this.regionType = type;
        this.flags = new FlagContainer();
        this.members = new PlayerContainer();
        this.owners = new PlayerContainer();
        this.isActive = true;
        this.children = new HashMap<>();
    }


    // TODO: Check constructors with new parameter
    protected AbstractRegion(String name, RegistryKey<World> dimension, RegionType type) {
        this.name = name;
        this.dimension = dimension;
        this.regionType = type;
        this.flags = new FlagContainer();
        this.members = new PlayerContainer();
        this.owners = new PlayerContainer();
        this.children = new HashMap<>();
        this.isActive = true;
    }

    /**
     * Minimal constructor to create an abstract region by supplying a name, type and an owner.
     *
     * @param name  name of the region
     * @param owner region owner
     */
    protected AbstractRegion(String name, RegionType regionType, PlayerEntity owner) {
        this(name, regionType);
        if (owner != null) {
            this.owners.addPlayer(owner);
        }
    }

    protected AbstractRegion(String name, RegistryKey<World> dimension, RegionType regionType, PlayerEntity owner) {
        this(name, dimension, regionType);
        if (owner != null) {
            this.owners.addPlayer(owner);
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
    public RegistryKey<World> getDim() {
        return dimension;
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
        return this.flags.contains(flag);
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

    @Override
    public IFlag getFlag(String flagName) {
        if (this.flags.contains(flagName)) {
            return this.flags.get(flagName);
        } else {
            return null;
        }
    }

    public void updateFlag(IFlag flag) {
        this.flags.put(flag);
    }

    public void toggleFlag(String flag, boolean enable) {
        if (this.containsFlag(flag)) {
            this.flags.get(flag).setIsActive(enable);
        }
    }

    public void invertFlag(String flag) {
        if (this.containsFlag(flag)) {
            this.flags.get(flag).setInverted(this.flags.get(flag).isInverted());
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
    public void addMember(PlayerEntity player) {
        this.members.addPlayer(player);
    }

    @Override
    public void addMember(Team team) {
        this.members.addTeam(team);
    }


    @Override
    public void addOwner(PlayerEntity player) {
        this.owners.addPlayer(player);
    }

    @Override
    public void addOwner(Team team) {
        this.owners.addTeam(team);
    }

    @Override
    public void removeMember(PlayerEntity player) {
        this.members.removePlayer(player);

    }

    @Override
    public void removeMember(Team team) {
        this.members.removeTeam(team.getName());

    }

    @Override
    public void removeOwner(PlayerEntity player) {
        this.owners.removePlayer(player);
    }

    @Override
    public void removeOwner(Team team) {
        this.owners.removeTeam(team);
    }

    @Override
    public boolean hasOwner(String teamName) {
        return this.owners.containsTeam(teamName);
    }

    @Override
    public boolean hasOwner(UUID playerUuid) {
        return this.owners.containsPlayer(playerUuid);
    }

    @Override
    public boolean hasMember(String teamName) {
        return this.members.containsTeam(teamName);
    }

    @Override
    public boolean hasMember(UUID playerUuid) {
        return this.members.containsPlayer(playerUuid);
    }

    @Override
    public PlayerContainer getMembers() {
        return this.members;
    }

    @Override
    public PlayerContainer getOwners() {
        return owners;
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
    public boolean permits(PlayerEntity player) {
        boolean isOwner = this.owners.containsPlayer(player.getUuid())
                || (player.getScoreboardTeam() != null && this.owners.containsTeam(player.getScoreboardTeam().getName()));
        boolean isMember = this.members.containsPlayer(player.getUuid())
                || (player.getScoreboardTeam() != null && this.members.containsTeam(player.getScoreboardTeam().getName()));
        return isOwner || isMember;
    }

    /**
     * Will always be called by IMarkableRegion to remove child of type IMarkableRegion
     *
     * @param child
     */
    @Override
    public void removeChild(IProtectedRegion child) {
        this.children.remove(child.getName());
    }

    /**
     * TODO: Global region stuff
     * Most error handling and consistency checks are done beforehand by the ArgumentTypes for add and removing children.
     * Try to add a child region to this region. <br>
     * 1. The child already has a parent which is a local region  or <br>
     * 2. The child is the same regions as this or <br>
     * 3. The child is the parent of this region <br>
     * 4. The child area is not completely contained by the parent area. (done beforehand)
     * Also removes child from dimension region
     * FIXME: since this method does manage more than adding children, it should be renamed
     *
     * @param child child to add to this region.
     */
    @Override
    public void addChild(IProtectedRegion child) {
        IProtectedRegion childParent = child.getParent();
        if (childParent != null) {
            boolean hasDimRegionParent = childParent instanceof DimensionalRegion;
            boolean hasLocalRegionParent = childParent instanceof IMarkableRegion;
            if (hasLocalRegionParent) {
                if (!childParent.equals(this) && this instanceof IMarkableRegion) {
                    // TODO: Why not allow this, as long as the owner of both regions are the same?
                    throw new IllegalRegionStateException("Not allowed to \"steal\" child from other parent than a dimensional region");
                }
                if (this instanceof DimensionalRegion) {
                    childParent.removeChild(child);
                }
            }
            if (hasDimRegionParent) {
                childParent.removeChild(child);
            }
        }
        child.setParent(this);
        this.children.put(child.getName(), child);
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

    /**
     * FIXME: setParent should not be used directly. Use addChild instead
     * Contains common consistency checks for setting a parent region.
     * More specific checks and assignments need to be implemented in subclasses.
     *
     * @param parent the parent to set for this region.
     * @throws IllegalRegionStateException when consistency checks are failing.
     */
    public boolean setParent(IProtectedRegion parent) {
        if (parent instanceof DimensionalRegion) {
            this.parent = parent;
            return true;
        }
        if (!parent.getDim().getValue().equals(GlobalRegion.GLOBAL) || !(parent instanceof GlobalRegion)) {
            if (!parent.getDim().getValue().equals(this.dimension.getValue())) {
                throw new IllegalRegionStateException("Region '" + parent.getName() + "' is not in the same dimension!");
            }
        }
        if (parent.equals(this)) {
            throw new IllegalRegionStateException("Region '" + parent.getName() + "' can't be its own parent!");
        }
        if (children.containsKey(parent.getName())) {
            throw new IllegalRegionStateException("Parent '" + parent.getName() + "' is already set as child for region '" + this.getName() + "'!");
        }
        return parent.hasChild(this);
    }

    public IProtectedRegion getParent() {
        return parent;
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = new NbtCompound();
        nbt.putString(NAME, this.name);
        nbt.putString(DIM, dimension.getValue().toString());
        nbt.putString(REGION_TYPE, this.regionType.type);
        nbt.putBoolean(ACTIVE, this.isActive);
        nbt.put(FLAGS, this.flags.serializeNBT());
        nbt.put(OWNERS, this.owners.serializeNBT());
        nbt.put(MEMBERS, this.members.serializeNBT());
        if (this.parent != null) {
            nbt.putString(PARENT, this.parent.getName());
        } else {
            nbt.putString(PARENT, "");
        }
        if (this.children != null) {
            NbtList childrenList = new NbtList();
            childrenList.addAll(this.children.keySet().stream()
                    .map(NbtString::of)
                    .collect(Collectors.toSet()));
            nbt.put(CHILDREN, childrenList);
        } else {
            nbt.put(CHILDREN, new NbtList());
        }
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        this.name = nbt.getString(NAME);
        this.dimension = RegistryKey.of(RegistryKeys.WORLD, Identifier.of(nbt.getString(DIM)));
        this.isActive = nbt.getBoolean(ACTIVE);
        this.regionType = RegionType.of(nbt.getString(REGION_TYPE));
        this.flags = new FlagContainer(nbt.getCompound(FLAGS));
        this.owners = new PlayerContainer(nbt.getCompound(OWNERS));
        this.members = new PlayerContainer(nbt.getCompound(MEMBERS));
        if (this.parent == null) {
            // deserialize parent only if present and if this is no instance of GlobalRegion
            if (nbt.contains(PARENT, NbtElement.STRING_TYPE) && !(this instanceof GlobalRegion)) {
                String parentName = nbt.getString(PARENT);
                if (!parentName.equals("")) {
                    this.parentName = nbt.getString(PARENT);
                } else {
                    this.parentName = null;
                }
            }
        }
        if (this.children != null && this.children.isEmpty()) {
            if (nbt.contains(CHILDREN, NbtElement.LIST_TYPE)) {
                NbtList childrenNbt = nbt.getList(CHILDREN, NbtElement.STRING_TYPE);
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
