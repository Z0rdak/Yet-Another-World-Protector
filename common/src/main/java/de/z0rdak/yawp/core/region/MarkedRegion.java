package de.z0rdak.yawp.core.region;

import com.mojang.serialization.Codec;
import com.mojang.serialization.Lifecycle;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import de.z0rdak.yawp.core.area.*;
import de.z0rdak.yawp.core.area.anchors.RegionAnchors;
import de.z0rdak.yawp.core.area.anchors.TeleportAnchor;
import de.z0rdak.yawp.core.flag.FlagValue;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.flag.RegionFlags;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.core.UUIDUtil;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;

import java.util.*;

/**
 * The MarkedRegion represents an abstract implementation for a markable region.
 * This can be used to implement different types of regions which define their area in a different way.
 */
public class MarkedRegion extends ProtectedRegion implements IMarkableRegion {

    public static final Codec<IMarkableRegion> CODEC = RecordCodecBuilder.create(
            instance -> instance.group(
                            Codec.STRING.fieldOf("name")
                                    .forGetter(IMarkableRegion::getName),
                            ResourceKey.codec(Registries.DIMENSION)
                                    .fieldOf("dim")
                                    .forGetter(IMarkableRegion::getDim),
                            UUIDUtil.STRING_CODEC.fieldOf("id")
                                    .forGetter(IMarkableRegion::getUuid),
                            // could validate dim + name against id here
                            UUIDUtil.STRING_CODEC.fieldOf("parentId")
                                    .forGetter(IMarkableRegion::getParentId),
                            Codec.unboundedMap(Codec.STRING, FlagValue.CODEC)
                                    .fieldOf("flags")
                                    .forGetter(r -> r.getFlags().getFlagMap()),
                            Codec.BOOL.fieldOf("active")
                                    .forGetter(IMarkableRegion::isActive),
                            Codec.BOOL.fieldOf("muted")
                                    .forGetter(IMarkableRegion::isMuted),
                            Codec.INT.fieldOf("priority")
                                    .forGetter(IMarkableRegion::getPriority),
                            MarkedAreaTypes.MARKED_AREA_CODEC.fieldOf("area")
                                    .forGetter(IMarkableRegion::getArea),
                            Codec.unboundedMap(Codec.STRING, TeleportAnchor.CODEC)
                                    .fieldOf("anchors")
                                    .forGetter(r -> r.getTpAnchors().getTpAnchors()),
                            Codec.unboundedMap(Codec.STRING, PlayerContainer.CODEC)
                                    .fieldOf("groups")
                                    .forGetter(IMarkableRegion::getGroups),
                            Codec.list(UUIDUtil.STRING_CODEC)
                                    .fieldOf("childrenIds")
                                    .forGetter(r -> new ArrayList<>(r.getChildrenIds()))
                    )
                    .apply(instance, (name, dim, id, parentId, flags, isActive, isMuted,
                                      priority, area, anchors, groups, childrenIds) ->
                            new MarkedRegion(name, dim, id, parentId, flags, isActive, isMuted, priority, area, new RegionAnchors(anchors), groups, childrenIds))
    );

    protected int priority;
    protected IMarkableArea area;
    protected RegionAnchors anchors;

    protected MarkedRegion(String name, ResourceKey<Level> dim, UUID id, UUID parentId,
                           Map<String, IFlag> flags, boolean isActive, boolean isMuted,
                           int priority, IMarkableArea area, RegionAnchors anchors,
                           Map<String, PlayerContainer> groups, List<UUID> childrenIds) {
        super(name, id, parentId, dim, RegionType.LOCAL, null);
        this.setArea(area);
        this.setPriority(priority);
        this.anchors = anchors;
        this.setFlags(new RegionFlags(flags));
        this.setIsActive(isActive);
        this.setIsMuted(isMuted);
        this.setGroups(groups);
        this.setChildrenIds(childrenIds);
    }


    public MarkedRegion(String name, UUID id, UUID parentId, IMarkableArea area, ResourceKey<Level> dim) {
        super(name, id, parentId, dim, RegionType.LOCAL);
        this.area = area;
        this.priority = Services.REGION_CONFIG.getDefaultPriority();
        this.anchors = new RegionAnchors();
    }

    public MarkedRegion(String name, UUID id, UUID parentId, IMarkableArea area, RegionAnchors anchors, ResourceKey<Level> dimension) {
        this(name, id, parentId, area, dimension);
        this.anchors = anchors;
    }

    @Override
    public Identifier getId() {
        return Identifier.parse(this.dimension.identifier() + "/" + this.getName());
    }

    @Override
    public boolean contains(BlockPos position) {
        return this.area.contains(position);
    }

    @Override
    public boolean hasLocalParent() {
        return this.getParent().getRegionType() == RegionType.LOCAL;
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
        return area.getAreaType();
    }

    @Override
    public RegionAnchors getTpAnchors() {
        return anchors;
    }
}
