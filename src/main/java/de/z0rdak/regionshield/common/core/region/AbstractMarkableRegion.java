package de.z0rdak.regionshield.common.core.region;

import de.z0rdak.regionshield.RegionShield;
import de.z0rdak.regionshield.common.core.area.AreaType;
import de.z0rdak.regionshield.common.core.area.IMarkableArea;
import de.z0rdak.regionshield.common.util.constants.RegionNBT;
import de.z0rdak.regionshield.server.data.RegionDataManager;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.registry.Registry;
import net.minecraft.world.World;

import java.beans.PropertyChangeSupport;
import java.util.*;
import java.util.stream.Collectors;

/**
 * The AbstractMarkableRegion represents a abstract implementation for a markable region.
 * This can be used to implement different types of regions which define their area in a different way.
 *
 * Constraint for regions with parents/childs that must be fulfilled at all times for the inheritance to work properly:
 * Considering region A as parent and region B as its child. As well as region C as child of region B.
 *
 *   A.Members c= B.Members c= C.Members
 *   C.Area c= B.Area c= A.Area ?
 *   C.Parent == B && C.Parent != A
 *   With other words: A region can only have one parent region
 *
 */
public abstract class AbstractMarkableRegion extends AbstractRegion implements IMarkableRegion {

    public final static int DEFAULT_PRIORITY = 1;
    private final PropertyChangeSupport pcs;
    protected String name;
    protected Map<UUID, String> owners;
    protected String parent;
    protected IMarkableRegion parentRegion;
    protected Set<String> childs;
    protected Map<String, IMarkableRegion> childRegions;
    protected int priority;
    protected RegistryKey<World> dimension;
    protected IMarkableArea area;
    protected boolean isMuted;
    protected AreaType areaType;
    protected BlockPos tpTarget;

    public AbstractMarkableRegion(String name, IMarkableArea area, RegistryKey<World> dimension) {
        this();
        this.name = name;
        this.dimension = dimension;
        this.area = area;
        this.areaType = area.getAreaType();
    }

    /***
     *
     * @param name
     * @param area
     * @param parent
     */
    public AbstractMarkableRegion(String name, IMarkableArea area, IMarkableRegion parent) {
        this();
        this.name = name;
        this.area = area;
        this.areaType = area.getAreaType();
        this.setParent(parent);
    }

    public void setParent(IMarkableRegion parent){
        this.assertInvariant();
        this.parent = parent.getName();
        this.dimension = parent.getDimension();
        this.parentRegion = parent;
        this.isMuted = parent.isMuted();
        this.priority = Math.max(parent.getPriority(), this.priority);
    }

    public void assertInvariant(){
        List<UUID> res = this.members.keySet()
                .stream()
                .filter( uuid -> !this.parentRegion.getMembers().containsKey(uuid))
                .collect(Collectors.toList());

        boolean memberAssertion = res.size() == 0;
        assert memberAssertion
                && this.dimension.equals(this.parentRegion.getDimension())
                && this.priority >= this.parentRegion.getPriority()
                && !this.childRegions.containsKey(this.parentRegion.getName());
        // TODO: Method to get list op area positions to check containment
        // && this.parentRegion.getArea().contains(this.area);
    }

    public AbstractMarkableRegion(String name, IMarkableArea area, RegistryKey<World> dimension, PlayerEntity owner) {
        this();
        this.name = name;
        this.dimension = dimension;
        this.area = area;
        this.areaType = area.getAreaType();
        this.owners.put(owner.getUUID(), owner.getDisplayName().getString());
    }

    public AbstractMarkableRegion(String name, IMarkableArea area, RegistryKey<World> dimension, List<PlayerEntity> owners) {
        this();
        this.name = name;
        this.dimension = dimension;
        this.area = area;
        this.areaType = area.getAreaType();
        owners.forEach(owner -> this.owners.put(owner.getUUID(), owner.getDisplayName().getString()));
    }

    protected AbstractMarkableRegion() {
        this.pcs = new PropertyChangeSupport(this);
        this.pcs.addPropertyChangeListener( e -> {
            RegionDataManager.save();
            RegionShield.LOGGER.debug("Property '" + e.getPropertyName() + "' changed: oldvalue=" + e.getOldValue() + ", newvalue=" + e.getNewValue());
        } );
        this.parentRegion = null;
        this.owners = new HashMap<>(0);
        this.parent = null;
        this.childRegions = new HashMap<>(0);
        this.childs = new HashSet<>(0);
        this.priority = DEFAULT_PRIORITY;
        this.isMuted = true;
    }

    public IMarkableArea getArea(){
        return this.area;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public int getPriority() {
        return priority;
    }

    @Override
    public RegistryKey<World> getDimension() {
        return dimension;
    }

    @Override
    public boolean isMuted() {
        return this.isMuted;
    }

    @Override
    public BlockPos getTpTarget() {
        return new BlockPos(this.tpTarget);
    }

    @Override
    public void setIsMuted(boolean isMuted) {
        this.pcs.firePropertyChange("isMuted", this.isMuted, isMuted);
        this.isMuted = isMuted;
    }

    @Override
    public void setPriority(int priority) {
        pcs.firePropertyChange("priority", this.priority, priority);
        this.priority = priority;
    }

    public void setArea(IMarkableArea area){
        pcs.firePropertyChange("area", getArea(), area);
        this.area = area;
    }

    @Override
    public void setTpTarget(BlockPos tpPos) {
        this.pcs.firePropertyChange("tpTarget", this.tpTarget, tpPos);
        this.tpTarget = tpPos;
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.putString(RegionNBT.NAME, name);
        nbt.putInt(RegionNBT.TP_X, this.tpTarget.getX());
        nbt.putInt(RegionNBT.TP_Y, this.tpTarget.getY());
        nbt.putInt(RegionNBT.TP_Z, this.tpTarget.getZ());
        nbt.putInt(RegionNBT.PRIORITY, priority);
        nbt.putString(RegionNBT.DIM, dimension.location().toString());
        nbt.putBoolean(RegionNBT.MUTED, isMuted);
        nbt.putString(RegionNBT.AREA_TYPE, this.areaType.toString());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.name = nbt.getString(RegionNBT.NAME);
        this.tpTarget = new BlockPos(nbt.getInt(RegionNBT.TP_X),
                nbt.getInt(RegionNBT.TP_Y),
                nbt.getInt(RegionNBT.TP_Z));
        this.priority = nbt.getInt(RegionNBT.PRIORITY);
        this.dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY,
                new ResourceLocation(nbt.getString(RegionNBT.DIM)));
        this.isMuted = nbt.getBoolean(RegionNBT.MUTED);
        this.areaType = AreaType.valueOf(nbt.getString(RegionNBT.AREA_TYPE));
    }
}
