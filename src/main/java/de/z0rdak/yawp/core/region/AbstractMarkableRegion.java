package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.flag.IFlag;
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

import java.io.Serializable;
import java.util.*;

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
public abstract class AbstractMarkableRegion extends AbstractRegion implements IMarkableRegion, Serializable {

    protected AbstractRegion parentRegion;
    private boolean inheritFromParent; // if (inheritFromParent && parent == null) inheritFromDimension()
    protected Map<String, IMarkableRegion> childRegions;
    protected int priority;
    protected boolean isMuted;
    protected RegistryKey<World> dimension;
    protected IMarkableArea area;
    protected AreaType areaType;
    protected BlockPos tpTarget;

    public AbstractMarkableRegion(String name, IMarkableArea area, PlayerEntity owner, RegistryKey<World> dimension) {
        super(name, owner);
        this.dimension = dimension;
        this.area = area;
        this.areaType = area.getAreaType();
        this.priority = RegionConfig.DEFAULT_REGION_PRIORITY.get();
        this.inheritFromParent = false;
    }

    // TODO: rework Constructor chain
    public AbstractMarkableRegion(String name, IMarkableArea area, BlockPos tpTarget, PlayerEntity owner, RegistryKey<World> dimension) {
        super(name, owner);
        this.tpTarget = tpTarget;
        this.dimension = dimension;
        this.area = area;
        this.areaType = area.getAreaType();
        this.priority = RegionConfig.DEFAULT_REGION_PRIORITY.get();
        this.inheritFromParent = false;
    }

    protected AbstractMarkableRegion() {
        super("");
        this.parentRegion = null;
        this.childRegions = new HashMap<>(0);
        this.priority = RegionConfig.DEFAULT_REGION_PRIORITY.get();
        this.isMuted = false;
        this.inheritFromParent = false;
    }

    protected AbstractMarkableRegion(String name) {
        super(name);
        this.parentRegion = null;
        this.childRegions = new HashMap<>(0);
        this.priority = RegionConfig.DEFAULT_REGION_PRIORITY.get();
        this.isMuted = false;
        this.inheritFromParent = false;
    }

    @Override
    public void addFlag(IFlag flag) {

    }

    public void setParent(AbstractRegion parent){
        if (parent instanceof DimensionalRegion) {
            DimensionalRegion dimensionalRegion = (DimensionalRegion) parent;
            dimensionalRegion.getName();
            this.parentRegion = dimensionalRegion;
            this.dimension = dimensionalRegion.getDimensionKey();
            return;
        }
        if (parent instanceof AbstractMarkableRegion) {
            AbstractMarkableRegion markableRegion = (AbstractMarkableRegion) parent;
            this.dimension = markableRegion.getDim();
            this.parentRegion = markableRegion;
            this.isMuted = markableRegion.isMuted();
            this.priority = Math.max(markableRegion.getPriority(), this.priority);
        }
    }

    public void removeParent() {
        throw new NotImplementedException("");
    }

    @Override
    public boolean contains(BlockPos position) {
        return this.area.contains(position);
    }

    @Override
    public CompoundNBT serializeNBT() {
        CompoundNBT nbt = super.serializeNBT();
        nbt.put(RegionNBT.TP_POS, NBTUtil.writeBlockPos(this.tpTarget));
        nbt.putInt(RegionNBT.PRIORITY, priority);
        nbt.putString(RegionNBT.DIM, dimension.location().toString());
        nbt.putBoolean(RegionNBT.MUTED, isMuted);
        nbt.putString(RegionNBT.AREA_TYPE, this.areaType.areaType);
        nbt.put(RegionNBT.AREA, this.area.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundNBT nbt) {
        super.deserializeNBT(nbt);
        this.tpTarget = NBTUtil.readBlockPos(nbt.getCompound(RegionNBT.TP_POS));
        this.priority = nbt.getInt(RegionNBT.PRIORITY);
        this.dimension = RegistryKey.create(Registry.DIMENSION_REGISTRY,
                new ResourceLocation(nbt.getString(RegionNBT.DIM)));
        this.isMuted = nbt.getBoolean(RegionNBT.MUTED);
        this.areaType = AreaType.valueOf(nbt.getString(RegionNBT.AREA_TYPE));
    }

    @Override
    public IMarkableArea getArea() {
        return area;
    }

    public AbstractRegion getParentRegion() {
        return parentRegion;
    }

    public boolean isInheritFromParent() {
        return inheritFromParent;
    }

    public Map<String, IMarkableRegion> getChildRegions() {
        return Collections.unmodifiableMap(childRegions);
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
