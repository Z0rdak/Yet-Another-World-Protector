package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.core.affiliation.PlayerContainer;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.FlagContainer;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import de.z0rdak.yawp.util.LocalRegions;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.scoreboard.Team;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.registry.Registry;
import net.minecraft.world.World;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.*;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

/**
 * A abstract region represents the basic implementation of a IProtectedRegion.
 * This abstraction can be used for markable regions as well as regions without
 * an area (dimensions). <br>
 * TODO: Recursive check for membership in parent regions
 */
public abstract class AbstractRegion implements IProtectedRegion {

    protected String name;
    protected RegistryKey<World> dimension;
    protected RegionType regionType;
    protected FlagContainer flags;
    protected PlayerContainer owners;
    protected PlayerContainer members;
    protected boolean isActive;

    @Nullable
    protected IProtectedRegion parent;
    protected Map<String, IProtectedRegion> children;

    protected AbstractRegion(CompoundNBT nbt) {
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
     * @param name name of the region
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
    public String getName() {
        return name;
    }

    @Override
    public RegistryKey<World> getDim() {
        return dimension;
    }

    @Override
    public void addFlag(IFlag flag){
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

    @Nullable
    @Override
    public IFlag getFlag(String flagName){
        if (this.flags.contains(flagName)) {
            return this.flags.get(flagName);
        } else {
            return null;
        }
    }

    public void updateFlag(IFlag flag) {
        this.flags.put(flag);
    }

    public void toggleFlag(String flag, boolean enable){
        if (this.containsFlag(flag)) {
            this.flags.get(flag).setIsActive(enable);
        }
    }

    public void invertFlag(String flag){
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
        boolean isOwner = this.owners.containsPlayer(player.getUUID())
                || (player.getTeam() != null && this.owners.containsTeam(player.getTeam().getName()));
        boolean isMember = this.members.containsPlayer(player.getUUID())
                || (player.getTeam() != null && this.members.containsTeam(player.getTeam().getName()));
        return isOwner || isMember;
    }

    /**
     * Will always be called by IMarkableRegion to remove child of type IMarkableRegion
     * @param child
     */
    @Override
    public void removeChild(IProtectedRegion child) {
        this.children.remove(child.getName());
    }

    /**
     * Try to add a child region to this region. <br>
     * Will throw an exception IllegalRegionStateException if: <br>
     * 1. The child already has a parent    or <br>
     * 2. The child is the same regions as this or <br>
     * 3. The child is the parent of this region <br>
     * 4. The child area is not completely contained by the parent area.
     * @param child child to add to this region.
     */
    @Override
    public void addChild(@Nonnull IProtectedRegion child) {
        if (child.getParent() != null) {
            throw new IllegalRegionStateException("");
        }
        if (child.equals(this)) {
            throw new IllegalRegionStateException("");
        }
        if (child.equals(this.parent)) {
            throw new IllegalRegionStateException("");
        }
        this.children.put(child.getName(), child);
        child.setParent(this);
    }

    @Override
    public Map<String, IProtectedRegion> getChildren() {
        return Collections.unmodifiableMap(this.children);
    }

    @Override
    public boolean hasChild(IProtectedRegion maybeChild){
        return this.children.containsKey(maybeChild.getName());
    }

    /**
     * FIXME: setParent should not be used directly. Use addChild instead
     * Contains common consistency checks for setting a parent region.
     * More specific checks and assignments need to be implemented in subclasses.
     * @param parent the parent to set for this region.
     * @throws IllegalRegionStateException when consistency checks are failing.
     */
    public boolean setParent(IProtectedRegion parent){
        if (parent instanceof DimensionalRegion) {
            this.parent = parent;
            return true;
        }
        if (!parent.getDim().location().equals(GlobalRegion.GLOBAL) || !(parent instanceof GlobalRegion)) {
            if (!parent.getDim().location().equals(this.dimension.location())) {
                throw new IllegalRegionStateException("Region '" + parent.getName() + "'is not in the same dimension!");
            }
        }
        if (parent.equals(this)) {
            throw new IllegalRegionStateException("Region '" + parent.getName() + "' can't be its own parent!");
        }
        if (children.containsKey(parent.getName())) {
            throw new IllegalRegionStateException("Parent '" + parent.getName() + "' is already set as child for region '" + this.getName() + "'!");
        }
        if (parent.hasChild(this)) {
            YetAnotherWorldProtector.LOGGER.debug("Already set parent for region");
            return true;
        }
        return false;
    }

    @Nullable
    public IProtectedRegion getParent() {
        return parent;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = new CompoundNBT();
        nbt.putString(NAME, this.name);
        nbt.putString(DIM, dimension.location().toString());
        nbt.putString(REGION_TYPE, this.regionType.type);
        nbt.putBoolean(ACTIVE, this.isActive);
        nbt.put(FLAGS, this.flags.serializeNBT());
        nbt.put(OWNERS, this.owners.serializeNBT());
        nbt.put(MEMBERS, this.members.serializeNBT());
        if (this.parent != null) {
            nbt.put(PARENT, this.parent.serializeNBT());
        } else {
            nbt.put(PARENT, new CompoundNBT());
        }
        if (this.children != null) {
            CompoundNBT childrenNbt = new CompoundNBT();
            this.children.forEach( (name, child) -> childrenNbt.put(name, child.serializeNBT()));
            nbt.put(CHILDREN, childrenNbt);
        } else {
            nbt.put(CHILDREN, new CompoundNBT());
        }
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        this.name = nbt.getString(NAME);
        this.dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY,
                new ResourceLocation(nbt.getString(DIM)));
        this.isActive = nbt.getBoolean(ACTIVE);
        this.regionType = RegionType.of(nbt.getString(REGION_TYPE));
        this.flags = new FlagContainer(nbt.getCompound(FLAGS));
        this.owners = new PlayerContainer(nbt.getCompound(OWNERS));
        this.members = new PlayerContainer(nbt.getCompound(MEMBERS));
        // deserialize parent only if present and if this is no instance of GlobalRegion
        if (nbt.contains(PARENT) && !(this instanceof GlobalRegion)) {
            this.deserializeParentRegion(nbt.getCompound(PARENT));
        } else {
            this.parent = null;
        }
        if (nbt.contains(CHILDREN)){
            CompoundNBT childrenNbt = nbt.getCompound(CHILDREN);
            if (childrenNbt.isEmpty()) {
                this.children = new HashMap<>();
            } else {
                this.children = new HashMap<>(childrenNbt.size());
                childrenNbt.getAllKeys().forEach(key -> this.children.put(key, this.deserializeLocalRegion(nbt.getCompound(key))));
            }
        } else {
            this.children = new HashMap<>(0);
        }
    }
   
    protected IProtectedRegion deserializeRegion(RegionType regionType, CompoundNBT regionNbt) {
        switch (regionType) {
            case GLOBAL:
                throw new UnsupportedOperationException("Global not supported yet");
            case DIMENSION:
                return new DimensionalRegion(regionNbt);
            case LOCAL:
                AreaType areaType = AreaType.of(regionNbt.getString(AREA_TYPE));
                if (areaType == null) {
                    YetAnotherWorldProtector.LOGGER.error("Unable to read parent region type for region '" + name + "' in dimension '" + this.dimension + "'!");
                    return null;
                } else {
                    return deserializeLocalRegion(areaType, regionNbt);
                }
            case TEMPLATE:
                throw new UnsupportedOperationException("Template not supported yet");
            default:
                throw new IllegalArgumentException("");
        }
    }

    private IMarkableRegion deserializeLocalRegion(AreaType areaType, CompoundNBT regionNbt) {
        switch (areaType) {
            case CUBOID:
                return new CuboidRegion(regionNbt);
            case CYLINDER:
                return new CylinderRegion(regionNbt);
            case SPHERE:
                return new SphereRegion(regionNbt);
            case POLYGON_3D:
                return new PolygonRegion(regionNbt);
            case PRISM:
                return new PrismRegion(regionNbt);
            default:
                throw new IllegalArgumentException("Unable to read area type.");
        }
    }
}
