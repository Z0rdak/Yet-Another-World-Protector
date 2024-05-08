package de.z0rdak.yawp.core.region;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import java.util.HashMap;
import java.util.Objects;

import static de.z0rdak.yawp.util.constants.RegionNBT.*;

/**
 * The AbstractMarkableRegion represents an abstract implementation for a markable region.
 * This can be used to implement different types of regions which define their area in a different way.
 */
public abstract class AbstractMarkableRegion extends AbstractRegion implements IMarkableRegion {

    protected int priority;
    protected boolean isMuted;
    protected IMarkableArea area;
    protected AreaType areaType;
    protected BlockPos tpTarget;

    public AbstractMarkableRegion(String name, IMarkableArea area, Player owner, ResourceKey<Level> dimension, AbstractRegion parent) {
        super(name, dimension, RegionType.LOCAL, owner);
        this.area = area;
        this.areaType = area.getAreaType();
        this.priority = RegionConfig.DEFAULT_REGION_PRIORITY.get();
        this.children = new HashMap<>();
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

    public AbstractMarkableRegion(CompoundTag nbt){
        super(nbt);
        this.deserializeNBT(provider, nbt);
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
        nbt.putBoolean(MUTED, isMuted);
        nbt.putString(AREA_TYPE, this.areaType.areaType);
        nbt.put(AREA, this.area.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(HolderLookup.Provider provider, CompoundTag nbt) {
        super.deserializeNBT(provider, nbt);
        this.tpTarget = NbtUtils.readBlockPos(nbt.getCompound(TP_POS));
        this.priority = nbt.getInt(PRIORITY);
        this.isMuted = nbt.getBoolean(MUTED);
        AreaType areaType = AreaType.of(nbt.getString(AREA_TYPE));
        if (areaType == null) {
            YetAnotherWorldProtector.LOGGER.error("Error loading region data for: '" + this.name + "' in dim '" + this.dimension.location() + "'");
            throw new IllegalArgumentException("Error loading region data for: '" + this.name + "' in dim '" + this.dimension.location() + "'");
        }
        this.areaType = areaType;
    }

    @Override
    public IMarkableArea getArea() {
        return area;
    }

    /**
     * FIXME: refactor to polymorphic instance method
     *
     * @param outer
     * @param inner
     * @return
     */
    public static boolean fullyContains(IMarkableArea outer, IMarkableArea inner) {
        boolean haveSameAreaType = outer.getAreaType() == inner.getAreaType();
        if (Objects.requireNonNull(outer.getAreaType()) == AreaType.CUBOID) {
            CuboidArea outerCuboid = (CuboidArea) outer;
            if (haveSameAreaType) {
                // should always be the case in the first iteration where only cuboids are allowed
                return outerCuboid.contains((CuboidArea) inner);
            } else {
                throw new UnsupportedOperationException("Only cuboid areas are supported currently.");
            }
        }
        throw new UnsupportedOperationException("Only cuboid areas are supported currently.");
    }

    /**
     * A IMarkableRegion can have both a Dimensional Region or another IMarkableRegion as its direct parent. <br>
     * Depending on the type, different properties must be set.
     *
     * @param parent the parent to set for this region.
     */
    @Override
    public boolean setParent(IProtectedRegion parent) {
        if (super.setParent(parent)) {
            return true;
        }
        if (parent instanceof IMarkableRegion markableParentRegion) {
            if (fullyContains(((IMarkableRegion) parent).getArea(), this.area)) {
                this.isMuted = markableParentRegion.isMuted();
                this.parent = markableParentRegion;
                YetAnotherWorldProtector.LOGGER.debug("Setting parent '" + parent.getName() + "' for region '" + this.getName() + "'");
                return true;
            } else {
                return false;
            }
        }
        return false;
    }

    @Override
    public int getPriority() {
        return priority;
    }

    @Override
    public boolean isMuted() {
        return isMuted;
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
