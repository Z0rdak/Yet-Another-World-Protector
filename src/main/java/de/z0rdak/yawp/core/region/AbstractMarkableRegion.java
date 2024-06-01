package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.IMarkableArea;
import net.minecraft.core.BlockPos;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import java.util.Optional;

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

    public AbstractMarkableRegion(String name, IMarkableArea area, Player owner, ResourceKey<Level> dimension, AbstractRegion parent) {
        super(name, dimension, RegionType.LOCAL, owner);
        this.area = area;
        this.areaType = area.getAreaType();
        this.priority = RegionConfig.getDefaultPriority();
        if (parent != null) {
            this.setParent(parent);
        }
    }

    public AbstractMarkableRegion(String name, IMarkableArea area, Player owner, ResourceKey<Level> dimension) {
        this(name, area, owner, dimension, null);
    }

    public AbstractMarkableRegion(String name, IMarkableArea area, BlockPos tpTarget, Player owner, ResourceKey<Level> dimension) {
        this(name, area, owner, dimension, null);
        this.tpTarget = tpTarget;
    }

    public AbstractMarkableRegion(HolderLookup.Provider provider, CompoundTag nbt) {
        super(provider, nbt);
        this.deserializeNBT(provider, nbt);
    }

    @Override
    protected boolean setParent(IProtectedRegion parent) {
        if (this.parent == null && parent.getRegionType() == RegionType.DIMENSION) {
            return super.setParent(parent);
        }
        if (this.parent.getRegionType() == RegionType.LOCAL && parent.getRegionType() == RegionType.DIMENSION) {
            return super.setParent(parent);
        }
        if (this.parent.getRegionType() == RegionType.DIMENSION && parent.getRegionType() == RegionType.LOCAL) {
            return super.setParent(parent);
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
    public CompoundTag serializeNBT(HolderLookup.Provider provider) {
        CompoundTag nbt = super.serializeNBT(provider);
        nbt.put(TP_POS, NbtUtils.writeBlockPos(this.tpTarget));
        nbt.putInt(PRIORITY, priority);
        nbt.putBoolean(MUTED, this.isMuted());
        nbt.putString(AREA_TYPE, this.areaType.areaType);
        nbt.put(AREA, this.area.serializeNBT(provider));
        return nbt;
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
        Optional<BlockPos> blockPos = NbtUtils.readBlockPos(nbt, TP_POS);
        blockPos.ifPresent(pos -> this.tpTarget = pos);
        this.priority = nbt.getInt(PRIORITY);
        this.setIsMuted(nbt.getBoolean(MUTED));
        AreaType areaType = AreaType.of(nbt.getString(AREA_TYPE));
        if (areaType == null) {
            YetAnotherWorldProtector.LOGGER.error("Error loading region data for: '" + this.getName() + "' in dim '" + this.dimension.location() + "'");
            throw new IllegalArgumentException("Error loading region data for: '" + this.getName() + "' in dim '" + this.dimension.location() + "'");
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
