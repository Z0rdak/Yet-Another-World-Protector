package de.z0rdak.yawp.core.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.*;
import de.z0rdak.yawp.core.flag.Flag;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlags;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.platform.Services;
import de.z0rdak.yawp.util.NbtCompatHelper;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static de.z0rdak.yawp.constants.serialization.RegionNbtKeys.*;

/**
 * The MarkedRegion represents an abstract implementation for a markable region.
 * This can be used to implement different types of regions which define their area in a different way.
 */
public abstract class MarkedRegion extends ProtectedRegion implements IMarkableRegion {

    public static final Codec<IMarkableRegion> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                            Codec.STRING.fieldOf("name")
                                    .forGetter(IMarkableRegion::getName),
                            ResourceKey.codec(Registries.DIMENSION)
                                    .fieldOf("dimension")
                                    .forGetter(IMarkableRegion::getDim),
                            Codec.STRING.fieldOf("parentName")
                                    .forGetter(IMarkableRegion::getParentName),
                            Codec.STRING.fieldOf("type")
                                    .forGetter(r -> r.getRegionType().type),
                            Codec.unboundedMap(Codec.STRING, Flag.CODEC)
                                    .fieldOf("flags")
                                    .forGetter(r -> r.getFlags().getFlagMap()),
                            Codec.BOOL.fieldOf("isActive")
                                    .forGetter(IMarkableRegion::isActive),
                            Codec.BOOL.fieldOf("isMuted")
                                    .forGetter(IMarkableRegion::isMuted),
                            Codec.INT.fieldOf("priority")
                                    .forGetter(IMarkableRegion::getPriority),
                            Codec.STRING.fieldOf("areaType")
                                    .forGetter(r -> r.getAreaType().areaType),
                            MarkedAreaType.MARKED_AREA_CODEC.fieldOf("area")
                                    .forGetter(IMarkableRegion::getArea),
                            BlockPos.CODEC.fieldOf("tpTarget")
                                    .forGetter(IMarkableRegion::getTpTarget),
                            Codec.unboundedMap(Codec.STRING, PlayerContainer.CODEC).fieldOf("groups")
                                    .forGetter(IMarkableRegion::getGroups),
                            Codec.list(Codec.STRING).fieldOf("childrenNames")
                                    .forGetter(r -> new ArrayList<>(r.getChildrenNames()))
                    )
                    .apply(instance, (name, dim, parentName, regionType, flags, isActive, isMuted,
                                      priority, areaType, area, blockPos, groups, childrenNames) -> {
                        var areaT = AreaType.of(areaType);
                        switch (areaT) {
                            case CUBOID -> {
                                return new CuboidRegion(name, dim, parentName, flags, isActive, isMuted, priority, area, blockPos, groups, childrenNames);
                            }
                            case SPHERE -> {
                                return new SphereRegion(name, dim, parentName, flags, isActive, isMuted, priority, area, blockPos, groups, childrenNames);
                            }
                            default -> throw new IllegalStateException("Unexpected value: " + areaT);
                        }
                    })
    );


    protected int priority;
    protected IMarkableArea area;
    protected AreaType areaType;
    protected BlockPos tpTarget;

    protected MarkedRegion(String name, ResourceKey<Level> dim, String parentName,
                           Map<String, IFlag> flags, boolean isActive, boolean isMuted,
                           int priority, String areaType, IMarkableArea area, BlockPos blockPos,
                           Map<String, PlayerContainer> groups, List<String> childrenNames) {
        super(name, dim, RegionType.LOCAL, null);
        this.setArea(area);
        this.setPriority(priority);
        this.areaType = AreaType.of(areaType);
        this.parentName = parentName;
        this.setFlags(new RegionFlags(flags));
        this.setIsActive(isActive);
        this.setIsMuted(isMuted);
        this.setTpTarget(blockPos);
        this.setGroups(groups);
        this.setChildrenNames(childrenNames);
    }


    public MarkedRegion(String name, IMarkableArea area, Player owner, ResourceKey<Level> dimension, ProtectedRegion parent) {
        super(name, dimension, RegionType.LOCAL, owner);
        this.area = area;
        this.areaType = area.getAreaType();
        this.priority = Services.REGION_CONFIG.getDefaultPriority();
        if (parent != null) {
            this.setParent(parent);
        }
    }

    public MarkedRegion(String name, IMarkableArea area, Player owner, ResourceKey<Level> dimension) {
        this(name, area, owner, dimension, null);
    }

    public MarkedRegion(String name, IMarkableArea area, BlockPos tpTarget, Player owner, ResourceKey<Level> dimension) {
        this(name, area, owner, dimension, null);
        this.tpTarget = tpTarget;
    }

    public MarkedRegion(CompoundTag nbt) {
        super(nbt);
        this.deserializeNBT(nbt);
    }

    @Override
    protected boolean setParent(IProtectedRegion parent) {
        if (this.parent == null) {
            boolean isParentLocalOrDim = parent.getRegionType() == RegionType.DIMENSION || parent.getRegionType() == RegionType.LOCAL;
            return isParentLocalOrDim && super.setParent(parent);
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
    public CompoundTag serializeNBT() {
        CompoundTag nbt = super.serializeNBT();
        nbt.put(TP_POS, NbtCompatHelper.asInts(this.tpTarget));
        nbt.putInt(PRIORITY, priority);
        nbt.putString(AREA_TYPE, this.areaType.areaType);
        nbt.put(AREA, this.area.serializeNBT());
        return nbt;
    }

    @Override
    public void deserializeNBT(CompoundTag nbt) {
        super.deserializeNBT(nbt);
        this.tpTarget = NbtCompatHelper.asBlockPos(nbt, TP_POS).orElseThrow();
        this.priority = nbt.getInt(PRIORITY).orElse(Services.REGION_CONFIG.getDefaultPriority());
        AreaType areaType = AreaType.of(nbt.getString(AREA_TYPE).orElseThrow());
        if (areaType == null) {
            Constants.LOGGER.error("Error loading region data for: '{}' in dim '{}'", this.getName(), this.dimension.location());
            throw new IllegalArgumentException("Error loading region data for: '" + this.getName() + "' in dim '" + this.dimension.location() + "'");
        }
        this.areaType = areaType;
    }

    @Override
    public IMarkableArea getArea() {
        return area;
    }

    @Override
    public void setArea(IMarkableArea area) {
        this.area = area;
    }

    @Override
    public void rename(String newName) {
        this.setName(newName);
    }

    @Override
    public int getPriority() {
        return priority;
    }

    @Override
    public void setPriority(int priority) {
        this.priority = priority;
    }

    @Override
    public AreaType getAreaType() {
        return areaType;
    }

    @Override
    public BlockPos getTpTarget() {
        return tpTarget;
    }

    @Override
    public void setTpTarget(BlockPos tpTarget) {
        this.tpTarget = tpTarget;
    }
}
