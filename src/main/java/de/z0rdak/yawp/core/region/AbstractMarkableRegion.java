package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.util.constants.RegionNBT;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.NBTUtil;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.registry.Registry;
import net.minecraft.world.World;
import org.apache.commons.lang3.NotImplementedException;

import java.util.*;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

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

    protected int priority;
    protected boolean isMuted;
    protected RegistryKey<World> dimension;
    protected IMarkableArea area;
    protected AreaType areaType;
    protected BlockPos tpTarget;
    protected AbstractRegion parent;
    protected Map<String, IMarkableRegion> children;

    public AbstractMarkableRegion(String name, IMarkableArea area, PlayerEntity owner, RegistryKey<World> dimension, AbstractRegion parent) {
        super(name, RegionType.LOCAL, owner);
        this.dimension = dimension;
        this.area = area;
        this.areaType = area.getAreaType();
        this.priority = RegionConfig.DEFAULT_REGION_PRIORITY.get();
        if (parent != null) {
            this.setParent(parent);
        }
        this.parent = null;
    }

    public AbstractMarkableRegion(String name, IMarkableArea area, PlayerEntity owner, RegistryKey<World> dimension) {
        this(name, area, owner, dimension, null);
    }

    public AbstractMarkableRegion(String name, IMarkableArea area, BlockPos tpTarget, PlayerEntity owner, RegistryKey<World> dimension) {
        this(name, area, owner, dimension, null);
        this.tpTarget = tpTarget;
    }

    public AbstractMarkableRegion(CompoundNBT nbt){
        super(nbt);
        this.deserializeNBT(nbt);
    }

    public void setParent(AbstractRegion parent){
        if (parent instanceof DimensionalRegion) {
            DimensionalRegion dimensionalRegion = (DimensionalRegion) parent;
            dimensionalRegion.getName();
            this.parent = dimensionalRegion;
            this.dimension = dimensionalRegion.getDimensionKey();
            YetAnotherWorldProtector.LOGGER.info("Setting parent (dim) '" + parent.getName() + "' for region '" + this.getName() + "'");
            return;
        }
        if (parent instanceof AbstractMarkableRegion) {
            AbstractMarkableRegion markableRegion = (AbstractMarkableRegion) parent;
            this.dimension = markableRegion.getDim();
            this.parent = markableRegion;
            this.isMuted = markableRegion.isMuted();
            this.priority = Math.max(markableRegion.getPriority(), this.priority);
            YetAnotherWorldProtector.LOGGER.info("Setting parent '" + parent.getName() + "' for region '" + this.getName() + "'");
        }
    }

    public void removeParent() {
        this.parent = null;
        // TODO: Event?
        // TODO: Something else?
    }

    @Override
    public boolean contains(BlockPos position) {
        return this.area.contains(position);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.put(TP_POS, NBTUtil.writeBlockPos(this.tpTarget));
        nbt.putInt(PRIORITY, priority);
        nbt.putString(DIM, dimension.location().toString());
        nbt.putBoolean(MUTED, isMuted);
        nbt.putString(AREA_TYPE, this.areaType.areaType);
        nbt.put(AREA, this.area.serializeNBT());
        nbt.put(PARENT, this.parent.serializeNBT());
        CompoundNBT childrenNbt = new CompoundNBT();
        this.children.forEach( (name, child) -> {
            childrenNbt.put(name, child.serializeNBT());
        });
        nbt.put(CHILDREN, childrenNbt);
        return nbt;
    }

    private AbstractMarkableRegion deserializeLocalRegion(CompoundNBT childNbt){
        AreaType parentArea = AreaType.valueOf(childNbt.getString(AREA_TYPE));
        switch (parentArea) {
            case CUBOID:
                return new CuboidRegion(childNbt);
            case CYLINDER:
                return new CylinderRegion(childNbt);
            case SPHERE:
                return new SphereRegion(childNbt);
            case POLYGON_3D:
                return new PolygonRegion(childNbt);
            case PRISM:
                return new PrismRegion(childNbt);
            case UNKNOWN:
            default:
                YetAnotherWorldProtector.LOGGER.info("");
                return null;
        }
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.tpTarget = NBTUtil.readBlockPos(nbt.getCompound(TP_POS));
        this.priority = nbt.getInt(PRIORITY);
        this.dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY,
                new ResourceLocation(nbt.getString(DIM)));
        this.isMuted = nbt.getBoolean(MUTED);
        this.areaType = AreaType.valueOf(nbt.getString(AREA_TYPE));
        if (nbt.contains(PARENT)) {
           this.deserializeParentRegion(nbt.getCompound(PARENT));
        } else {
            this.parent = null;
        }
        if (nbt.contains(CHILDREN)){
            CompoundNBT childrenNbt = nbt.getCompound(CHILDREN);
            this.children = new HashMap<>(childrenNbt.size());
            childrenNbt.getAllKeys().forEach( key -> {
                this.children.put(key, this.deserializeLocalRegion(nbt.getCompound(key)));
            });
        } else {
            this.children = new HashMap<>(0);
        }
    }

    private void deserializeParentRegion(CompoundNBT parentNbt) {
        RegionType type = RegionType.of(parentNbt.getString(REGION_TYPE));
        if (type != null) {
            switch (type) {
                case GLOBAL:
                    // TODO: This is not (yet) intended
                    break;
                case DIMENSION:
                    this.parent = new DimensionalRegion(parentNbt);
                    break;
                case LOCAL:
                    this.parent = deserializeLocalRegion(parentNbt);
                    break;
            }
        } else {
            this.parent = null;
            // TODO:
            YetAnotherWorldProtector.LOGGER.info("Unable to load... ");
        }
    }

    @Override
    public IMarkableArea getArea() {
        return area;
    }

    public AbstractRegion getParent() {
        return parent;
    }

    public Map<String, IProtectedRegion> getChildRegions() {
        return Collections.unmodifiableMap(this.children);
    }

    public boolean hasChild(IMarkableRegion maybeChild){
        throw new NotImplementedException("");
    }

    public boolean isChildOf(AbstractRegion maybeParent){
        throw new NotImplementedException("");
    }

    @Override
    public int getPriority() {
        return priority;
    }

    @Override
    public boolean isMuted() {
        return isMuted;
    }

    @Override
    public RegistryKey<World> getDim() {
        return dimension;
    }

    public AreaType getAreaType() {
        return areaType;
    }

    @Override
    public BlockPos getTpTarget() {
        return tpTarget;
    }

    @Override
    public void setPriority(int priority) {
        this.priority = priority;
    }

    @Override
    public void setIsMuted(boolean isMuted) {
        this.isMuted = isMuted;
    }

    @Override
    public void setArea(IMarkableArea area) {
        this.area = area;
    }

    @Override
    public void setTpTarget(BlockPos tpTarget) {
        this.tpTarget = tpTarget;
    }
}
