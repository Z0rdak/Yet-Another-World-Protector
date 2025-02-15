package de.z0rdak.yawp.api.core.region;

import de.z0rdak.yawp.api.permission.Permissions;
import de.z0rdak.yawp.core.area.AreaType;
import de.z0rdak.yawp.core.flag.RegionFlags;
import de.z0rdak.yawp.core.flag.IFlag;
import de.z0rdak.yawp.core.group.PlayerContainer;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.scores.Team;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class LocalRegionBuilder<T extends IMarkableRegion> {

    protected final Map<String, PlayerContainer> groups;
    protected String name;
    protected int priority;
    protected ResourceKey<Level> dim;
    protected boolean active;
    protected boolean muted;
    protected RegionFlags flags;
    protected AreaType areaType;

    protected LocalRegionBuilder(final String name) {
        this();
        this.name = name;
    }

    protected LocalRegionBuilder() {
        this.flags = new RegionFlags();
        this.groups = new HashMap<>();
        this.active = true;
        this.muted = false;
        this.priority = 0;
        Permissions.GROUP_LIST.forEach(group -> this.groups.put(group, new PlayerContainer(group)));
    }

    public LocalRegionBuilder<T> withName(String name) {
        this.name = name;
        return this;
    }

    public LocalRegionBuilder<T> withPriority(int priority) {
        this.priority = priority;
        return this;
    }

    public LocalRegionBuilder<T> withDefaultPriority() {
        return this.withPriority(Services.REGION_CONFIG.getDefaultPriority());
    }

    public LocalRegionBuilder<T> inDim(ResourceKey<Level> dim) {
        this.dim = dim;
        return this;
    }

    public LocalRegionBuilder<T> inDim(Level level) {
        this.dim = level.dimension();
        return this;
    }

    public LocalRegionBuilder<T> active(boolean active) {
        this.active = active;
        return this;
    }

    public LocalRegionBuilder<T> off() {
        return this.active(false);
    }

    public LocalRegionBuilder<T> on() {
        return this.active(true);
    }

    public LocalRegionBuilder<T> mute(boolean muted) {
        this.muted = muted;
        return this;
    }

    public LocalRegionBuilder<T> mute() {
        return this.mute(true);
    }

    public LocalRegionBuilder<T> addGroup(String groupName) {
        this.groups.put(groupName, new PlayerContainer(groupName));
        return this;
    }

    public LocalRegionBuilder<T> addGroup(PlayerContainer group) {
        this.groups.put(group.getGroupName(), group);
        return this;
    }

    public LocalRegionBuilder<T> addTeam(String groupName, Team team) {
        if (!this.groups.containsKey(groupName))
            this.groups.put(groupName, new PlayerContainer(groupName));
        this.groups.get(groupName).addTeam(team.getName());
        return this;
    }

    public LocalRegionBuilder<T> addPlayer(String groupName, Player player) {
        if (!this.groups.containsKey(groupName))
            this.groups.put(groupName, new PlayerContainer(groupName));
        this.groups.get(groupName).addPlayer(player.getUUID(), player.getScoreboardName());
        return this;
    }

    public LocalRegionBuilder<T> withFlags(RegionFlags flags) {
        this.flags = flags;
        return this;
    }

    public LocalRegionBuilder<T> withFlags(List<IFlag> flags) {
        if (this.flags == null) this.flags = new RegionFlags();
        flags.forEach(flag -> this.flags.put(flag));
        return this;
    }

    public LocalRegionBuilder<T> addFlag(IFlag flag) {
        if (this.flags == null) this.flags = new RegionFlags();
        this.flags.put(flag);
        return this;
    }

    public abstract T build();
}
