package de.z0rdak.yawp.platform.event;

import de.z0rdak.yawp.api.events.flag.FlagCheckRequest;
import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.eventbus.api.Cancelable;
import net.minecraftforge.eventbus.api.Event;

import javax.annotation.Nullable;

/**
 * See {@code FlagCheckRequest}
 */
@Cancelable
public class ForgeFlagCheckRequest extends Event {
    private final BlockPos target;
    private final ResourceKey<Level> dimension;
    @Nullable
    private final Player player;
    private final RegionFlag regionFlag;
    private final String id;

    public ForgeFlagCheckRequest(BlockPos target, RegionFlag regionFlag, ResourceKey<Level> dimension, @Nullable Player player, String id) {
        this.player = player;
        this.target = target;
        this.dimension = dimension;
        this.regionFlag = regionFlag;
        this.id = id;
    }

    public ForgeFlagCheckRequest(FlagCheckRequest event) {
        this.player = event.getPlayer();
        this.target = event.getTarget();
        this.dimension = event.getDimension();
        this.regionFlag = event.getRegionFlag();
        this.id = event.getId();
    }

    public String getId() {
        return id;
    }

    public BlockPos getTarget() {
        return this.target;
    }

    public RegionFlag getRegionFlag() {
        return regionFlag;
    }

    public ResourceKey<Level> getDimension() {
        return dimension;
    }

    @Nullable
    public Player getPlayer() {
        return player;
    }
}
