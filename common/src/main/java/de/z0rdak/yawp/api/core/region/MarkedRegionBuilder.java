package de.z0rdak.yawp.api.core.region;

import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.area.CuboidArea;
import de.z0rdak.yawp.core.area.IMarkableArea;
import de.z0rdak.yawp.core.area.SphereArea;
import de.z0rdak.yawp.core.area.anchors.RegionAnchors;
import de.z0rdak.yawp.core.area.anchors.TeleportAnchor;
import de.z0rdak.yawp.core.flag.RegionFlags;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.core.region.MarkedRegion;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;

import java.util.*;

public class MarkedRegionBuilder {

    protected final Map<String, PlayerContainer> groups;
    protected String name;
    protected int priority;
    protected ResourceKey<Level> dim;
    protected boolean active;
    private IMarkableArea area;
    protected boolean muted;
    protected RegionFlags flags;
    protected AreaType areaType;

    protected MarkedRegionBuilder(final String name) {
        this();
        this.name = name;
    }

    protected MarkedRegionBuilder() {
        this.flags = new RegionFlags();
        this.groups = new HashMap<>();
        this.active = true;
        this.muted = false;
        this.priority = 0;
        Permissions.GROUP_LIST.forEach(group -> this.groups.put(group, new PlayerContainer(group)));
    }

    public MarkedRegionBuilder withName(String name) {
        this.name = name;
        return this;
    }

    public MarkedRegionBuilder withPriority(int priority) {
        this.priority = priority;
        return this;
    }

    public MarkedRegionBuilder withDefaultPriority() {
        return this.withPriority(Services.REGION_CONFIG.getDefaultPriority());
    }

    public MarkedRegionBuilder inDim(ResourceKey<Level> dim) {
        this.dim = dim;
        return this;
    }

    public MarkedRegionBuilder inDim(Level level) {
        this.dim = level.dimension();
        return this;
    }

    public MarkedRegionBuilder active(boolean active) {
        this.active = active;
        return this;
    }

    public MarkedRegionBuilder off() {
        return this.active(false);
    }

    public MarkedRegionBuilder on() {
        return this.active(true);
    }

    public MarkedRegionBuilder mute(boolean muted) {
        this.muted = muted;
        return this;
    }

    public MarkedRegionBuilder mute() {
        return this.mute(true);
    }

    public MarkedRegionBuilder addGroup(String groupName) {
        this.groups.put(groupName, new PlayerContainer(groupName));
        return this;
    }

    public MarkedRegionBuilder addGroup(PlayerContainer group) {
        this.groups.put(group.getGroupName(), group);
        return this;
    }

    public MarkedRegionBuilder addPlayer(String groupName, Player player) {
        if (!this.groups.containsKey(groupName))
            this.groups.put(groupName, new PlayerContainer(groupName));
        this.groups.get(groupName).addPlayer(player.getUUID(), player.getScoreboardName());
        return this;
    }

    public MarkedRegionBuilder withFlags(RegionFlags flags) {
        this.flags = flags;
        return this;
    }

    public MarkedRegionBuilder withFlags(List<IFlag> flags) {
        if (this.flags == null) this.flags = new RegionFlags();
        flags.forEach(flag -> this.flags.put(flag));
        return this;
    }

    public MarkedRegionBuilder addFlag(IFlag flag) {
        if (this.flags == null) this.flags = new RegionFlags();
        this.flags.put(flag);
        return this;
    }

    public MarkedRegionBuilder cuboid(BlockPos first, BlockPos second) {
        this.area = new CuboidArea(first, second);
        return this;
    }

    public MarkedRegionBuilder sphere(BlockPos center, int radius) {
        this.area = new SphereArea(center, radius);
        return this;
    }

    public MarkedRegionBuilder area(IMarkableArea area) {
        this.area = area;
        return this;
    }

    // TODO provide either parent local or set parentId to level uuid

    public MarkedRegion build() {
        Objects.requireNonNull(name);
        Objects.requireNonNull(dim);
        Objects.requireNonNull(area);
        return new MarkedRegion(name, UUID.randomUUID(), null, area, new RegionAnchors(), dim);
    }
}
