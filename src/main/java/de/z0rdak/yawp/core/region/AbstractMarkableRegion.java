package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.IMarkableArea;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtHelper;
import net.minecraft.registry.RegistryKey;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import java.util.HashMap;
import java.util.Objects;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

/**
 * The AbstractMarkableRegion represents an abstract implementation for a markable region.
 * This can be used to implement different types of regions which define their area in a different way.
 */
public abstract class AbstractMarkableRegion extends AbstractRegion implements IMarkableRegion {

    protected int priority;
    protected IMarkableArea area;
    protected AreaType areaType;
    protected BlockPos tpTarget;

    public AbstractMarkableRegion(String name, IMarkableArea area, PlayerEntity owner, RegistryKey<World> dimension, AbstractRegion parent) {
        super(name, dimension, RegionType.LOCAL, owner);
        this.area = area;
        this.areaType = area.getAreaType();
        this.priority = RegionConfig.getDefaultPriority();
        if (parent != null) {
            this.setParent(parent);
        }
    }

    public AbstractMarkableRegion(String name, IMarkableArea area, PlayerEntity owner, RegistryKey<World> dimension) {
        this(name, area, owner, dimension, null);
    }

    public AbstractMarkableRegion(String name, IMarkableArea area, BlockPos tpTarget, PlayerEntity owner, RegistryKey<World> dimension) {
        this(name, area, owner, dimension, null);
        this.tpTarget = tpTarget;
    }

    public AbstractMarkableRegion(NbtCompound nbt){
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    protected boolean setParent(IProtectedRegion parent) {
        if (this.parent == null) {
            boolean isParentLocalOrDim = parent.getRegionType() == RegionType.DIMENSION || parent.getRegionType() == RegionType.LOCAL;
            return isParentLocalOrDim ? super.setParent(parent) : false;
        } else {
            if (this.parent.getRegionType() == RegionType.LOCAL && parent.getRegionType() == RegionType.DIMENSION) {
                return super.setParent(parent);
            }
            if (this.parent.getRegionType() == RegionType.DIMENSION && parent.getRegionType() == RegionType.LOCAL) {
                return super.setParent(parent);
            }
        }
        return false;
    }

    @Override
    public boolean addChild(IProtectedRegion child) {
        if (child.getRegionType() == RegionType.LOCAL && child.getParent() == null) {
            return super.addChild(child);
        }
        if (child.getRegionType() == RegionType.LOCAL && child.getParent().getRegionType() == RegionType.DIMENSION) {
            return super.addChild(child);
        }
        return false;
    }

    @Override
    public boolean contains(BlockPos position) {
        return this.area.contains(position);
    }

    @Override
    public NbtCompound serializeNBT() {
        NbtCompound nbt = super.serializeNBT();
        nbt.put(TP_POS, NbtHelper.fromBlockPos(this.tpTarget));
        nbt.putInt(PRIORITY, priority);
        nbt.putBoolean(MUTED, this.isMuted());
        nbt.putString(AREA_TYPE, this.areaType.areaType);
        nbt.put(AREA, this.area.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(NbtCompound nbt) {
        super.deserializeNBT(nbt);
        this.tpTarget = NbtHelper.toBlockPos(nbt.getCompound(TP_POS));
        this.priority = nbt.getInt(PRIORITY);
        this.setIsMuted(nbt.getBoolean(MUTED));
        AreaType areaType = AreaType.of(nbt.getString(AREA_TYPE));
        if (areaType == null) {
            YetAnotherWorldProtector.LOGGER.error("Error loading region data for: '" + this.getName() + "' in dim '" + this.dimension.getValue() + "'");
            throw new IllegalArgumentException("Error loading region data for: '" + this.getName() + "' in dim '" + this.dimension.getValue() + "'");
        }
        this.areaType = areaType;
    }

    @Override
    public IMarkableArea getArea() {
        return area;
    }

    @Override
    public void rename(String newName) {
        this.setName(newName);
    }

    @Override
    public int getPriority() {
        return priority;
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
    public void setArea(IMarkableArea area) {
        this.area = area;
    }

    @Override
    public void setTpTarget(BlockPos tpTarget) {
        this.tpTarget = tpTarget;
    }
}
