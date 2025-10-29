package de.z0rdak.yawp.core.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.Lifecycle;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.core.area.*;
import de.z0rdak.yawp.core.flag.FlagValue;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlags;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import java.util.*;

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
                            Codec.unboundedMap(Codec.STRING, FlagValue.CODEC)
                                    .optionalFieldOf("flags", Lifecycle.stable(), new HashMap<>(), Lifecycle.stable())
                                    .forGetter(r -> r.getFlags().getFlagMap()),
                            Codec.BOOL.optionalFieldOf("isActive", true)
                                    .forGetter(IMarkableRegion::isActive),
                            Codec.BOOL.optionalFieldOf("isMuted", false)
                                    .forGetter(IMarkableRegion::isMuted),
                            Codec.INT.fieldOf("priority")
                                    .forGetter(IMarkableRegion::getPriority),
                            Codec.STRING.fieldOf("areaType")
                                    .forGetter(r -> MarkedAreaTypes.areaIdentifier(r.getAreaType()).toString()),
                            MarkedAreaTypes.MARKED_AREA_CODEC.fieldOf("area")
                                    .forGetter(IMarkableRegion::getArea),
                            Codec.unboundedMap(Codec.STRING, TeleportAnchor.CODEC)
                                    .optionalFieldOf("tpAnchors", Lifecycle.stable(), new HashMap<>(), Lifecycle.stable())
                                    .forGetter(r -> r.getTpAnchors().getTpAnchors()),
                            Codec.unboundedMap(Codec.STRING, PlayerContainer.CODEC)
                                    .optionalFieldOf("groups", Lifecycle.stable(), new HashMap<>(), Lifecycle.stable())
                                    .forGetter(IMarkableRegion::getGroups),
                            Codec.list(Codec.STRING)
                                    .optionalFieldOf("childrenNames", Lifecycle.stable(), new ArrayList<>(), Lifecycle.stable())
                                    .forGetter(r -> new ArrayList<>(r.getChildrenNames()))
                    )
                    .apply(instance, (name, dim, parentName, regionType, flags, isActive, isMuted,
                                      priority, areaType, area, anchors, groups, childrenNames) -> {
                        String lowerCase = areaType.toLowerCase(Locale.ROOT);
                        var areaT = AreaType.of(ResourceLocation.parse(lowerCase).getPath());
                        switch (areaT) {
                            case CUBOID -> {
                                return new CuboidRegion(name, dim, parentName, flags, isActive, isMuted, priority, area, new RegionAnchors(anchors), groups, childrenNames);
                            }
                            case SPHERE -> {
                                return new SphereRegion(name, dim, parentName, flags, isActive, isMuted, priority, area, new RegionAnchors(anchors), groups, childrenNames);
                            }
                            default -> throw new IllegalStateException("Unexpected value: " + areaT);
                        }
                    })
    );


    protected int priority;
    protected IMarkableArea area;
    protected AreaType areaType;
    protected RegionAnchors anchors;

    protected MarkedRegion(String name, ResourceKey<Level> dim, String parentName,
                           Map<String, IFlag> flags, boolean isActive, boolean isMuted,
                           int priority, String areaType, IMarkableArea area, RegionAnchors anchors,
                           Map<String, PlayerContainer> groups, List<String> childrenNames) {
        super(name, dim, RegionType.LOCAL, null);
        this.setArea(area);
        this.setPriority(priority);
        this.areaType = AreaType.of(areaType);
        this.parentName = parentName;
        this.anchors = anchors;
        this.setFlags(new RegionFlags(flags));
        this.setIsActive(isActive);
        this.setIsMuted(isMuted);
        this.setGroups(groups);
        this.setChildrenNames(childrenNames);
    }


    public MarkedRegion(String name, IMarkableArea area, Player owner, ResourceKey<Level> dimension, ProtectedRegion parent) {
        super(name, dimension, RegionType.LOCAL, owner);
        this.area = area;
        this.areaType = area.getAreaType();
        this.priority = Services.REGION_CONFIG.getDefaultPriority();
        this.anchors = new RegionAnchors();
        if (parent != null) {
            this.setParent(parent);
        }
    }

    public MarkedRegion(String name, IMarkableArea area, Player owner, ResourceKey<Level> dimension) {
        this(name, area, owner, dimension, null);
    }

    public MarkedRegion(String name, IMarkableArea area, RegionAnchors anchors, Player owner, ResourceKey<Level> dimension) {
        this(name, area, owner, dimension, null);
        this.anchors = anchors;
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
    public IMarkableArea getArea() {
        return area;
    }

    @Override
    public void setArea(IMarkableArea area) {
        this.area = area;
        this.areaType = area.getAreaType();
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
    public RegionAnchors getTpAnchors() {
        return anchors;
    }
}
